/*
 * bltWinPrnt.c --
 *
 *	This module implements WIN32 printer access.
 *
 * Copyright 1998 by Bell Labs Innovations for Lucent Technologies.
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that the copyright notice and warranty
 * disclaimer appear in supporting documentation, and that the names
 * of Lucent Technologies any of their entities not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 *
 * Lucent Technologies disclaims all warranties with regard to this
 * software, including all implied warranties of merchantability and
 * fitness.  In no event shall Lucent Technologies be liable for any
 * special, indirect or consequential damages or any damages
 * whatsoever resulting from loss of use, data or profits, whether in
 * an action of contract, negligence or other tortuous action, arising
 * out of or in connection with the use or performance of this
 * software.
 * 
 */

#if _MSC_VER

#include <bltInt.h>
#include <X11/Xutil.h>
#include <X11/Xlib.h>
#undef Status
#include <winspool.h>
#include <windowsx.h>
#include <commdlg.h>

extern HINSTANCE TclWinGetTclInstance(void);

/*
set pid [blt::printer open {\\alprint\2a211}]
blt::printer getattr $pid varName
blt::printer setattr $pid varName
blt::printer write $pid file
blt::printer close $pid
blt::printer names
.graph print $pid
*/

typedef struct {
    int type;
    HDC hDC;
} PrintDrawable;

typedef struct PrinterDevice {
    HANDLE hPrinter;
    char *fileName;
    PRINTER_INFO_2 *printerInfoPtr;
    PrintDrawable drawable;
    Tcl_HashEntry *hashPtr;
} PrinterDevice;

typedef struct PrinterToken {
    DWORD token;
    char *string;
} PrinterToken;

static PrinterToken sizeTable[] = {
    /* Letter 8 1/2 x 11 in */
    DMPAPER_LETTER,		"Letter",
    /* Letter Small 8 1/2 x 11 in */
    DMPAPER_LETTERSMALL,	"Letter Small",	
    /* Tabloid 11 x 17 in */
    DMPAPER_TABLOID,		"Tabloid",	
    /* Ledger 17 x 11 in */
    DMPAPER_LEDGER,		"Ledger",
    /* Legal 8 1/2 x 14 in */
    DMPAPER_LEGAL,		"Legal",	
    /* Statement 5 1/2 x 8 1/2 in */
    DMPAPER_STATEMENT,		"Statement",	
    /* Executive 7 1/4 x 10 1/2 in */
    DMPAPER_EXECUTIVE,		"Executive",	
    /* A3 297 x 420 mm */
    DMPAPER_A3,			"A3",		
    /* A4 210 x 297 mm */
    DMPAPER_A4,			"A4",		
    /* A4 Small 210 x 297 mm */
    DMPAPER_A4SMALL,		"A4 Small",
    /* A5 148 x 210 mm */
    DMPAPER_A5,			"A5",		
    /* B4 (JIS) 250 x 354 */
    DMPAPER_B4,			"B4 (JIS)",		
    /* B5 (JIS) 182 x 257 mm */
    DMPAPER_B5,			"B5 (JIS)",		
    /* Folio 8 1/2 x 13 in */
    DMPAPER_FOLIO,		"Folio",	
    /* Quarto 215 x 275 mm */
    DMPAPER_QUARTO,		"Quarto",	
    /* 10x14 in */
    DMPAPER_10X14,		"10x14",	
    /* 11x17 in */
    DMPAPER_11X17,		"11x17",	
    /* Note 8 1/2 x 11 in */
    DMPAPER_NOTE,		"Note",
    /* Envelope #9 3 7/8 x 8 7/8 */
    DMPAPER_ENV_9,		"Envelope #9",  
    /* Envelope #10 4 1/8 x 9 1/2 */
    DMPAPER_ENV_10,		"Envelope #10", 
    /* Envelope #11 4 1/2 x 10 3/8 */
    DMPAPER_ENV_11,		"Envelope #11",
    /* Envelope #12 4 \276 x 11 */
    DMPAPER_ENV_12,		"Envelope #12", 
    /* Envelope #14 5 x 11 1/2 */
    DMPAPER_ENV_14,		"Envelope #14", 
    /* C size sheet */
    DMPAPER_CSHEET,		"C size sheet", 
    /* D size sheet */
    DMPAPER_DSHEET,		"D size sheet", 
    /* E size sheet */
    DMPAPER_ESHEET,		"E size sheet", 
    /* Envelope DL 110 x 220mm */
    DMPAPER_ENV_DL,		"Envelope DL",  
    /* Envelope C5 162 x 229 mm */
    DMPAPER_ENV_C5,		"Envelope C5",  
    /* Envelope C3  324 x 458 mm */
    DMPAPER_ENV_C3,		"Envelope C3",
    /* Envelope C4  229 x 324 mm */
    DMPAPER_ENV_C4,		"Envelope C4",  
    /* Envelope C6  114 x 162 mm */
    DMPAPER_ENV_C6,		"Envelope C6",  
    /* Envelope C65 114 x 229 mm */
    DMPAPER_ENV_C65,		"Envelope C65", 
    /* Envelope B4  250 x 353 mm */
    DMPAPER_ENV_B4,		"Envelope B4",  
    /* Envelope B5  176 x 250 mm */
    DMPAPER_ENV_B5,		"Envelope B5",
    /* Envelope B6  176 x 125 mm */
    DMPAPER_ENV_B6,		"Envelope B6",
    /* Envelope 110 x 230 mm */
    DMPAPER_ENV_ITALY,		"Envelope Italy",
    /* Env Monarch 3 7/8 x 7 1/2 in */
    DMPAPER_ENV_MONARCH,	"Envelope Monarch",
    /* 6 3/4 Envelope 3 5/8 x 6 1/2 in */
    DMPAPER_ENV_PERSONAL,	"6 3/4 Envelope",
    /* US Std Fanfold 14 7/8 x 11 in */
    DMPAPER_FANFOLD_US,		"US Std Fanfold",
    /* German Std Fanfold 8 1/2 x 12 in */
    DMPAPER_FANFOLD_STD_GERMAN,	"German Std Fanfold",
    /* German Legal Fanfold 8 1/2 x 13 in */
    DMPAPER_FANFOLD_LGL_GERMAN,	"German Legal Fanfold",
    /* B4 (ISO) 250 x 353 mm */
    DMPAPER_ISO_B4,             "ISOB4",
    /* Japanese Postcard 100 x 148 mm */
    DMPAPER_JAPANESE_POSTCARD,	"Postcard (JIS)",
    /* 9 x 11 in */
    DMPAPER_9X11,		"9x11",
    /* 10 x 11 in */
    DMPAPER_10X11,              "10x11",
    /* 15 x 11 in */
    DMPAPER_15X11,              "15x11",
    /* Envelope Invite 220 x 220 mm */
    DMPAPER_ENV_INVITE,		"Envelope Invite",
    /* Letter Extra 9 \275 x 12 in */
    DMPAPER_LETTER_EXTRA,	"Letter Extra",
    /* Legal Extra 9 \275 x 15 in */
    DMPAPER_LEGAL_EXTRA,	"Legal Extra",
    /* Tabloid Extra 11.69 x 18 in */
    DMPAPER_TABLOID_EXTRA,	"Tabloid Extra",
    /* A4 Extra 9.27 x 12.69 in */
    DMPAPER_A4_EXTRA,		"A4 Extra",
    /* Letter Transverse 8 \275 x 11 in */
    DMPAPER_LETTER_TRANSVERSE,	"Letter Transverse",
    /* A4 Transverse 210 x 297 mm */
    DMPAPER_A4_TRANSVERSE,	"A4 Transverse",
    /* Letter Extra Transverse 9\275 x 12 in */
    DMPAPER_LETTER_EXTRA_TRANSVERSE,"Letter Extra Transverse",
    /* SuperA/SuperA/A4 227 x 356 mm */
    DMPAPER_A_PLUS,		"Super A Plus",
    /* SuperB/SuperB/A3 305 x 487 mm */
    DMPAPER_B_PLUS,		"Super B Plus",
    /* Letter Plus 8.5 x 12.69 in */
    DMPAPER_LETTER_PLUS,	"Letter Plus",
    /* A4 Plus 210 x 330 mm */
    DMPAPER_A4_PLUS,		"A4 Plus",
    /* A5 Transverse 148 x 210 mm */
    DMPAPER_A5_TRANSVERSE,	"A5 Transverse",
    /* B5 (JIS) Transverse 182 x 257 mm */
    DMPAPER_B5_TRANSVERSE,	"B5 Transverse",
    /* A3 Extra 322 x 445 mm */
    DMPAPER_A3_EXTRA,		"A3 Extra",
    /* A5 Extra 174 x 235 mm */
    DMPAPER_A5_EXTRA,		"A5 Extra",
    /* B5 (ISO) Extra 201 x 276 mm */
    DMPAPER_B5_EXTRA,		"B5 Extra",
    /* A2 420 x 594 mm */
    DMPAPER_A2,			"A2",
    /* A3 Transverse 297 x 420 mm */
    DMPAPER_A3_TRANSVERSE,	"A3 Transverse",
    /* A3 Extra Transverse 322 x 445 mm   */
    DMPAPER_A3_EXTRA_TRANSVERSE,	"A3 Extra Transverse",
    0, NULL
} ;

static PrinterToken statusTable[] = {
    PRINTER_STATUS_BUSY,		"Busy",
    PRINTER_STATUS_DOOR_OPEN,		"Door Open",
    PRINTER_STATUS_ERROR,		"Error",
    PRINTER_STATUS_INITIALIZING,	"Initializing",
    PRINTER_STATUS_IO_ACTIVE,		"IO Active",
    PRINTER_STATUS_MANUAL_FEED,		"Manual Feed",
    PRINTER_STATUS_NOT_AVAILABLE,	"Not Available",
    PRINTER_STATUS_NO_TONER,		"No Toner",
    PRINTER_STATUS_OFFLINE,		"Offline",
    PRINTER_STATUS_OUTPUT_BIN_FULL,	"Bin Full",
    PRINTER_STATUS_OUT_OF_MEMORY,	"Out Of Memory",
    PRINTER_STATUS_PAGE_PUNT,		"Page Punt",
    PRINTER_STATUS_PAPER_JAM,		"Paper Jam",
    PRINTER_STATUS_PAPER_OUT,		"Paper Out",
    PRINTER_STATUS_PAPER_PROBLEM,	"Paper Problem",
    PRINTER_STATUS_PAUSED,		"Paused",
    PRINTER_STATUS_PENDING_DELETION,	"Pending Deletion",
    PRINTER_STATUS_POWER_SAVE,		"Power Save",
    PRINTER_STATUS_PRINTING,		"Printing",
    PRINTER_STATUS_PROCESSING,		"Processing",
    PRINTER_STATUS_SERVER_UNKNOWN,	"Server Unknown",
    PRINTER_STATUS_TONER_LOW,		"Toner Low",
    PRINTER_STATUS_USER_INTERVENTION,	"User Intervention",
    PRINTER_STATUS_WAITING,		"Waiting",
    PRINTER_STATUS_WARMING_UP,		"Warming Up",
    0, NULL
};

static PrinterToken attributeTable[] = {
    PRINTER_ATTRIBUTE_DEFAULT,			"Default",
    PRINTER_ATTRIBUTE_DIRECT,			"Direct",
    PRINTER_ATTRIBUTE_DO_COMPLETE_FIRST,	"Do Complete First",
    PRINTER_ATTRIBUTE_ENABLE_BIDI,		"Enable BIDI",
    PRINTER_ATTRIBUTE_ENABLE_DEVQ,		"Enable Devq",
    PRINTER_ATTRIBUTE_HIDDEN,			"Hidden",
    PRINTER_ATTRIBUTE_KEEPPRINTEDJOBS,		"Keep Printed Jobs",
    PRINTER_ATTRIBUTE_LOCAL,			"Local",
    PRINTER_ATTRIBUTE_NETWORK,			"Network",
    PRINTER_ATTRIBUTE_QUEUED,			"Queued",
    PRINTER_ATTRIBUTE_RAW_ONLY,			"Raw Only",
    PRINTER_ATTRIBUTE_SHARED,			"Shared",
    PRINTER_ATTRIBUTE_WORK_OFFLINE,		"Offline",
    0, NULL
};

static PrinterToken binTable[] = {
    DMBIN_UPPER,	"Upper",
    DMBIN_LOWER,	"Lower",
    DMBIN_MIDDLE,	"Middle",
    DMBIN_MANUAL,	"Manual",
    DMBIN_ENVELOPE,	"Envelope",
    DMBIN_ENVMANUAL,	"Envelope Manual",
    DMBIN_AUTO,		"Automatic",
    DMBIN_TRACTOR,	"Tractor",
    DMBIN_SMALLFMT,	"Small Format",
    DMBIN_LARGEFMT,	"Large Format",
    DMBIN_LARGECAPACITY,"Large Capacity",
    DMBIN_CASSETTE,	"Cassette",
    DMBIN_FORMSOURCE,	"Form Source",
    0, NULL
};

static PrinterToken orientationTable[] = {
    DMORIENT_PORTRAIT,	"Portrait",
    DMORIENT_LANDSCAPE,	"Landscape",
    0, NULL
};

static PrinterToken qualityTable[] = {
    DMRES_HIGH,		"High",
    DMRES_MEDIUM,	"Medium",
    DMRES_LOW,		"Low",
    DMRES_DRAFT,	"Draft",
    0, NULL
};

static PrinterToken colorTable[] = {
    DMCOLOR_COLOR,	"Color",
    DMCOLOR_MONOCHROME, "Monochrome",
    0, NULL
};

static PrinterToken duplexTable[] = {
    DMDUP_SIMPLEX,	"Simplex",
    DMDUP_HORIZONTAL,	"Horizontal",
    DMDUP_VERTICAL,	"Vertical",
    0, NULL
};

static PrinterToken ttOptionTable[] = {
    DMTT_BITMAP,	"Bitmap",
    DMTT_DOWNLOAD,	"Download",
    DMTT_SUBDEV,	"Substitute Device",
    DMTT_DOWNLOAD_OUTLINE, "Download Outline",
    0, NULL
};

static Tcl_HashTable printerTable;
static Tcl_HashTable idTable;
static int initialized = FALSE;
static int nextId = 0;

static int
GetOpenPrinter(
    Tcl_Interp *interp, 
    const char *id, 
    PrinterDevice **devicePtrPtr)
{
    Tcl_HashEntry *hPtr;

    hPtr = Tcl_FindHashEntry(&idTable, id);
    if (hPtr == NULL) {
	Tcl_AppendResult(interp, "can't find printer id \"", id, "\"", 
			 (char *)NULL);
	return TCL_ERROR;
    }
    *devicePtrPtr = (PrinterDevice *)Tcl_GetHashValue(hPtr);
    return TCL_OK;
}

static void
DestroyPrinter(PrinterDevice *devicePtr)
{
    ClosePrinter(devicePtr->hPrinter);
    if (devicePtr->drawable.hDC != NULL) {
	DeleteDC(devicePtr->drawable.hDC);
    }
    if (devicePtr->printerInfoPtr != NULL) {
	free((char *)devicePtr->printerInfoPtr);
    }
    if (devicePtr->hashPtr != NULL) {
	Tcl_DeleteHashEntry(devicePtr->hashPtr);
    }
    free((char *)devicePtr);
}

static char *
AttributesToString(
    DWORD attributes, 
    Tcl_DString *dStrPtr)
{
    register PrinterToken *p;

    Tcl_DStringInit(dStrPtr);
    for(p = attributeTable; p->string != NULL; p++) {
	if (attributes & p->token) {
	    Tcl_DStringAppendElement(dStrPtr, p->string);
	}
    }
    return Tcl_DStringValue(dStrPtr);
}

static char *
StatusToString(
    DWORD status, 
    Tcl_DString *dStrPtr)
{
    register PrinterToken *p;

    Tcl_DStringInit(dStrPtr);
    for(p = statusTable; p->string != NULL; p++) {
	if (status & p->token) {
	    Tcl_DStringAppendElement(dStrPtr, p->string);
	}
    }
    return Tcl_DStringValue(dStrPtr);
}

static char *
TokenToString(
    PrinterToken *table, 
    DWORD token)
{
    register PrinterToken *p;

    for(p = table; p->string != NULL; p++) {
	if (token == p->token) {
	    return p->string;
	}
    }
    return "???";
}

static DWORD
StringToToken(
    PrinterToken *table, 
    char *string)
{
    register PrinterToken *p;
    char c;

    for(p = table; p->string != NULL; p++) {
	if ((c == p->string[0]) && (strcmp(string, p->string) == 0)) {
	    return p->token;
	}
    }
    return 0;
}

static void
GetFormInfo(
    Tcl_Interp *interp,
    FORM_INFO_1 *infoArr,
    int numForms,
    char *varName)
{
    Tcl_DString dString;
    register int i;

    Tcl_DStringInit(&dString);
    for(i = 0; i < numForms; i++) {
	Tcl_DStringAppendElement(&dString, infoArr[i].pName);
    }
    Tcl_SetVar2(interp, varName, "EnumForms", Tcl_DStringValue(&dString),
       TCL_LEAVE_ERR_MSG);
    Tcl_DStringFree(&dString);
}

static void
GetPrinterAttributes(
    Tcl_Interp *interp,		/* Interpreter context. */
    PrinterDevice *devicePtr,
    char *varName)		/* Name of array variable to contain 
				 * printer device information. */
{
    char *string;
    Tcl_DString dString;
    PRINTER_INFO_2 *infoPtr = devicePtr->printerInfoPtr; 
    DEVMODE *modePtr;

    Tcl_DStringInit(&dString);
    Tcl_SetVar2(interp, varName, "ServerName", infoPtr->pServerName, 0);
    Tcl_SetVar2(interp, varName, "PrinterName", infoPtr->pPrinterName, 0);
    Tcl_SetVar2(interp, varName, "PortName", infoPtr->pPortName, 0);
    Tcl_SetVar2(interp, varName, "DriverName", infoPtr->pDriverName, 0);
    Tcl_SetVar2(interp, varName, "Comment", infoPtr->pComment, 0);
    Tcl_SetVar2(interp, varName, "Location", infoPtr->pLocation, 0);
    Tcl_SetVar2(interp, varName, "SepFile", infoPtr->pSepFile, 0);
    Tcl_SetVar2(interp, varName, "PrintProcessor", infoPtr->pPrintProcessor,0);
    Tcl_SetVar2(interp, varName, "Datatype", infoPtr->pDatatype, 0);
    Tcl_SetVar2(interp, varName, "Parameters", infoPtr->pParameters, 0);
    Tcl_SetVar2(interp, varName, "Attributes", 
		AttributesToString(infoPtr->Attributes, &dString), 0);
    Tcl_SetVar2(interp, varName, "Priority", Blt_Int(infoPtr->Priority), 0);
    Tcl_SetVar2(interp, varName, "DefaultPriority", 
		Blt_Int(infoPtr->DefaultPriority), 0);
    Tcl_SetVar2(interp, varName, "StartTime", Blt_Int(infoPtr->StartTime), 0);
    Tcl_SetVar2(interp, varName, "UntilTime", Blt_Int(infoPtr->UntilTime), 0);
    Tcl_SetVar2(interp, varName, "Status", 
		StatusToString(infoPtr->Status, &dString), 0);
    Tcl_SetVar2(interp, varName, "Jobs", Blt_Int(infoPtr->cJobs), 0);
    Tcl_SetVar2(interp, varName, "AveragePPM", Blt_Int(infoPtr->AveragePPM), 
		0);
    modePtr = infoPtr->pDevMode;
    if (modePtr->dmFields & DM_ORIENTATION) {
	Tcl_SetVar2(interp, varName, "Orientation", 
	    TokenToString(orientationTable, modePtr->dmOrientation), 0);
    }
    if (modePtr->dmFields & DM_PAPERSIZE) {
	Tcl_SetVar2(interp, varName, "PaperSize", 
	    TokenToString(sizeTable, modePtr->dmPaperSize), 0);
    }
    if (modePtr->dmFields & DM_PAPERWIDTH) {
	Tcl_SetVar2(interp, varName, "PaperWidth", 
	    Blt_Int(modePtr->dmPaperWidth), 0);
    }
    if (modePtr->dmFields & DM_PAPERLENGTH) {
	Tcl_SetVar2(interp, varName, "PaperLength", 
	    Blt_Int(modePtr->dmPaperLength), 0);
    }
    if (modePtr->dmFields & DM_SCALE) {
	Tcl_SetVar2(interp, varName, "Scale", Blt_Int(modePtr->dmScale), 0);
    }
    if (modePtr->dmFields & DM_COPIES) {
	Tcl_SetVar2(interp, varName, "Copies", Blt_Int(modePtr->dmCopies), 0);
    }
    if (modePtr->dmFields & DM_DEFAULTSOURCE) {
	Tcl_SetVar2(interp, varName, "DefaultSource", 
	    TokenToString(binTable, modePtr->dmDefaultSource), 0);
    }
    if (modePtr->dmFields & DM_PRINTQUALITY) {
	if (modePtr->dmPrintQuality < 0) {
	    string = TokenToString(qualityTable, modePtr->dmPrintQuality);
	} else {
	    string = Blt_Int(modePtr->dmPrintQuality);
	}
	Tcl_SetVar2(interp, varName, "PrintQuality", string, 0);
    }
    if (modePtr->dmFields & DM_COLOR) {
	Tcl_SetVar2(interp, varName, "Color", 
		    TokenToString(colorTable, modePtr->dmColor), 0);
    }
    if (modePtr->dmFields & DM_DUPLEX) {
	Tcl_SetVar2(interp, varName, "Duplex", 
	    TokenToString(duplexTable, modePtr->dmDuplex), 0);
    }
    if (modePtr->dmFields & DM_YRESOLUTION) {
	Tcl_SetVar2(interp, varName, "YResolution", 
	    Blt_Int(modePtr->dmYResolution), 0);
    }
    if (modePtr->dmFields & DM_TTOPTION) {
	Tcl_SetVar2(interp, varName, "TTOption", 
	    TokenToString(ttOptionTable, modePtr->dmTTOption), 0);
    }
    if (modePtr->dmFields & DM_COLLATE) {
	string = "???";
	if (modePtr->dmCollate == DMCOLLATE_TRUE) {
	    string = "true";
	} else if (modePtr->dmCollate == DMCOLLATE_FALSE) {
	    string = "false";
	} 
	Tcl_SetVar2(interp, varName, "Collate", string, 0);
    }
    if (modePtr->dmFields & DM_FORMNAME) {
	Tcl_SetVar2(interp, varName, "FormName", modePtr->dmFormName, 0);
    }
    Tcl_SetVar2(interp, varName, "OutputFile", modePtr->dmDeviceName, 0);
    Tcl_DStringFree(&dString);
}

static void
SetPrinterAttributes(
    Tcl_Interp *interp,
    PrinterDevice *devicePtr,
    char *varName)
{
    char *string;
    DEVMODE *modePtr;
    int value;

    modePtr = devicePtr->printerInfoPtr->pDevMode;
    modePtr->dmFields = 0;
    string = Tcl_GetVar2(interp, varName, "Orientation", 0);
    if (string != NULL) {
	value = StringToToken(orientationTable, string);
	if (value > 0) {
	    modePtr->dmFields |= DM_ORIENTATION;
	    modePtr->dmOrientation = value;
	}
    }
    string = Tcl_GetVar2(interp, varName, "PaperSize", 0);
    if (string != NULL){
	value = StringToToken(sizeTable, string);
	if (value > 0) {
	    modePtr->dmFields |= DM_PAPERSIZE;
	    modePtr->dmPaperSize = value;
	}
    }
    string = Tcl_GetVar2(interp, varName, "PaperWidth", 0);
    if (string != NULL) {
	if (Tcl_GetInt(interp, string, &value) == TCL_OK) {
	    modePtr->dmFields |= DM_PAPERWIDTH;
	    modePtr->dmPaperWidth = value;
	}
    }
    string = Tcl_GetVar2(interp, varName, "PaperLength", 0);
    if (string != NULL) {
	if (Tcl_GetInt(interp, string, &value) == TCL_OK) {
	    modePtr->dmFields |= DM_PAPERLENGTH;
	    modePtr->dmPaperLength = value;
	}
    }
    string = Tcl_GetVar2(interp, varName, "Scale", 0);
    if (string != NULL) {
	if (Tcl_GetInt(interp, string, &value) == TCL_OK) {
	    modePtr->dmFields |= DM_SCALE;
	    modePtr->dmScale = value;
	}
    }
    string = Tcl_GetVar2(interp, varName, "Copies", 0);
    if (string != NULL) {
	if (Tcl_GetInt(interp, string, &value) == TCL_OK) {
	    modePtr->dmFields |= DM_COPIES;
	    modePtr->dmCopies = value;
	}
    }
    string = Tcl_GetVar2(interp, varName, "DefaultSource", 0);
    if (string != NULL) {
	value = StringToToken(binTable, string);
	if (value > 0) {
	    modePtr->dmFields |= DM_DEFAULTSOURCE;
	    modePtr->dmDefaultSource = value;
	}
    }
    string = Tcl_GetVar2(interp, varName, "PrintQuality", 0);
    if (string != NULL) {
	value = StringToToken(qualityTable, string);
	if (value > 0) {
	    modePtr->dmFields |= DM_PRINTQUALITY;
	    modePtr->dmPrintQuality = value;
	}
    }
    string = Tcl_GetVar2(interp, varName, "Color", 0);
    if (string != NULL) {
	value = StringToToken(colorTable, string);
	if (value > 0) {
	    modePtr->dmFields |= DM_COLOR;
	    modePtr->dmColor = value;
	}
    }
    string = Tcl_GetVar2(interp, varName, "Duplex", 0);
    if (string != NULL) {
	value = StringToToken(duplexTable, string);
	if (value > 0) {
	    modePtr->dmFields |= DM_DUPLEX;
	    modePtr->dmDuplex = value;
	}
    }
    string = Tcl_GetVar2(interp, varName, "YResolution", 0);
    if (string != NULL) {
	if (Tcl_GetInt(interp, string, &value) == TCL_OK) {
	    modePtr->dmFields |= DM_YRESOLUTION;
	    modePtr->dmYResolution = value;
	}
    }
    string = Tcl_GetVar2(interp, varName, "TTOption", 0);
    if (string != NULL) {
	value = StringToToken(ttOptionTable, string);
	if (value > 0) {
	    modePtr->dmFields |= DM_TTOPTION;
	    modePtr->dmTTOption = value;
	}
    }
    string = Tcl_GetVar2(interp, varName, "Collate", 0);
    if (string != NULL) {
	if (Tcl_GetBoolean(interp, string, &value) == TCL_OK) {
	    modePtr->dmFields |= DM_COLLATE;
	    modePtr->dmCollate = value;
	}
    }
    string = Tcl_GetVar2(interp, varName, "OutputFile", 0);
    if (string != NULL) {
	if (devicePtr->fileName != NULL) {
	    free(devicePtr->fileName);
	}
	devicePtr->fileName = strdup(string);
    }
    string = Tcl_GetVar2(interp, varName, "Priority", 0);
    if (string != NULL) {
	if ((Tcl_GetInt(interp, string, &value) == TCL_OK) &&
	    (value >= NO_PRIORITY) && (value <= MAX_PRIORITY)) {
	    devicePtr->printerInfoPtr->Priority = value;
	}
    }
}

/*ARGSUSED*/
static int
CloseOp(
    ClientData clientData,	/* Not used. */
    Tcl_Interp *interp, 
    int argc, 
    char **argv)
{
    PrinterDevice *devicePtr;

    if (GetOpenPrinter(interp, argv[2], &devicePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (!ClosePrinter(devicePtr->hPrinter)) {
	Tcl_AppendResult(interp, "can't close printer \"", argv[2], "\": ",
		Blt_Win32Error(), (char *)NULL);
	return TCL_ERROR;
    }
    DestroyPrinter(devicePtr);
    return TCL_OK;
}

/*ARGSUSED*/
static int
EnumOp(
    ClientData clientData,	/* Not used. */
    Tcl_Interp *interp, 
    int argc, 
    char **argv)
{
    PrinterToken *p;
    char c;
    unsigned int length;

    c = argv[2][0];
    length = strlen(argv[2]);
    if ((c == 'p') && (strncmp(argv[2], "paper", length) == 0)) {
	p = sizeTable;
    } else if ((c == 'q') && (strncmp(argv[2], "quality", length) == 0)) {
	p = qualityTable;
    } else if ((c == 'b') && (strncmp(argv[2], "bin", length) == 0)) {
	p = binTable;
    } else if ((c == 'o') && (strncmp(argv[2], "orientation", length) == 0)) {
	p = orientationTable;
    } else if ((c == 'c') && (strncmp(argv[2], "color", length) == 0)) {
	p = colorTable;
    } else if ((c == 'd') && (strncmp(argv[2], "duplex", length) == 0)) {
	p = duplexTable;
    } else if ((c == 't') && (strncmp(argv[2], "ttoption", length) == 0)) {
	p = ttOptionTable;
    } else {
	Tcl_AppendResult(interp, "bad enumeration field \"", argv[2], "\"",
		 (char *)NULL);
	return TCL_ERROR;
    }
    for(/*empty*/; p->string != NULL; p++) {
	Tcl_AppendElement(interp, p->string);
    }
    return TCL_OK;
}

/*ARGSUSED*/
static int
GetattrOp(
    ClientData clientData,	/* Not used. */
    Tcl_Interp *interp, 
    int argc, 
    char **argv)
{
    PrinterDevice *devicePtr;

    if (GetOpenPrinter(interp, argv[2], &devicePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    GetPrinterAttributes(interp, devicePtr, argv[3]);
    return TCL_OK;
}


/*ARGSUSED*/
static int
OpenOp(
    ClientData clientData,	/* Not used. */
    Tcl_Interp *interp, 
    int argc, 
    char **argv)
{
    PrinterDevice *devicePtr;
    HANDLE hPrinter;
    Tcl_HashEntry *hPtr;
    int isNew;
    char string[200];
    unsigned char *buffer;
    int numBytes;
    
    hPtr = Tcl_CreateHashEntry(&printerTable, argv[2], &isNew);
    if (!isNew) {
	Tcl_AppendResult(interp, "printer \"", argv[2], "\" is already open",
		 (char *)NULL);
	return TCL_ERROR;
    }
    if (!OpenPrinter(argv[2], &hPrinter, NULL)) {
	Tcl_AppendResult(interp, "can't open printer \"", argv[2], "\": ",
		Blt_Win32Error(), (char *)NULL);
	return TCL_ERROR;
    }

    /* Call the first time to determine the amount of memory needed. */
    GetPrinter(hPrinter, 2, NULL, 0, &numBytes);
    if (GetLastError() != ERROR_INSUFFICIENT_BUFFER) {
	Tcl_AppendResult(interp, "can't get attributes size for \"", 
		argv[2], "\": ", Blt_Win32Error(), (char *)NULL);
	return TCL_ERROR;
    }

    /* Allocate a buffer to contain all printer information. */
    buffer = (unsigned char *)calloc(1, numBytes);
    assert(buffer);

    if (!GetPrinter(hPrinter, 2, buffer, numBytes, &numBytes)) {
	Tcl_AppendResult(interp, "can't get printer attributes for \"", 
		argv[2], "\": ", Blt_Win32Error(), (char *)NULL);
	return TCL_ERROR;
    }
    devicePtr = (PrinterDevice *)calloc(1, sizeof(PrinterDevice));
    devicePtr->printerInfoPtr = (PRINTER_INFO_2 *)buffer;
    devicePtr->hPrinter = hPrinter;
    do {
	sprintf(string, "printer%d", nextId++);
	hPtr = Tcl_CreateHashEntry(&idTable, string, &isNew);
    } while (!isNew);
    devicePtr->hashPtr = hPtr;
    Tcl_SetHashValue(hPtr, (ClientData)devicePtr);
    Tcl_SetResult(interp, string, TCL_VOLATILE);
    return TCL_OK;
}

/*ARGSUSED*/
static int
NamesOp(
    ClientData clientData,	/* Not used. */
    Tcl_Interp *interp, 
    int argc, 
    char **argv)
{
    register int i;
    int numPrinters, numBytes;
    int elemSize, level;
    unsigned char *buffer;
    int result;
    char *p;

    if (Blt_GetPlatformId() == VER_PLATFORM_WIN32_NT) {
	level = 4;
	elemSize = sizeof(PRINTER_INFO_4);
    } else {
	level = 5;
	elemSize = sizeof(PRINTER_INFO_5);
    }
    EnumPrinters(
        PRINTER_ENUM_NAME,      /* Flags */ 
	NULL,			/* Printer name */
	level,			/* Information level: 1, 2, 4, or 5 */
	NULL,			/* Array of returned information */
	0,			/* Size of array */
	&numBytes,		/* Size needed for array */
	&numPrinters);		/* Number of structures returned */

    if (GetLastError() != ERROR_INSUFFICIENT_BUFFER) {
	Tcl_AppendResult(interp, "can't enumerate printers: ", 
		Blt_Win32Error(), (char *)NULL);
	return TCL_ERROR;
    }

    buffer = (unsigned char *)malloc(numBytes);
    assert(buffer);

    result = EnumPrinters(
        PRINTER_ENUM_NAME,      /* Flags */ 
	NULL,			/* Printer name */
	level,			/* Information level: 1, 2, 4, or 5 */
	buffer,			/* Array of returned information */
	numBytes,		/* Size of array */
	&numBytes,		/* Size needed for array */
	&numPrinters);		/* Number of structures returned */

    if (!result) {
	Tcl_AppendResult(interp, "can't enumerate printers: ", 
		Blt_Win32Error(), (char *)NULL);
	return TCL_ERROR;
    }
    p = buffer;
    for(i = 0; i < numPrinters; i++) {
	if ((argc == 2) || (Tcl_StringMatch(p, argv[2]))) {
	    Tcl_AppendElement(interp, *(char **)p);
	}
	p += elemSize;
    }
    free((char *)buffer);
    return TCL_OK;
}

/*ARGSUSED*/
static int
SetattrOp(
    ClientData clientData,	/* Not used. */
    Tcl_Interp *interp, 
    int argc, 
    char **argv)
{
    PrinterDevice *devicePtr;

    if (GetOpenPrinter(interp, argv[2], &devicePtr) != TCL_OK) {
	return TCL_ERROR;
    }

    /* Convert Tcl array to PRINTER_INFO_2 structure. */ 
    SetPrinterAttributes(interp, devicePtr, argv[3]);

    /* Reset the printer. */
    if (!SetPrinter(devicePtr->hPrinter, 2, 
	(unsigned char *)devicePtr->printerInfoPtr, 0)) {
	char *printer = devicePtr->printerInfoPtr->pPrinterName;

	Tcl_AppendResult(interp, "can't set attributes for \"", printer, 
		"\": ", Blt_Win32Error(), (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}
	
/*ARGSUSED*/
static int
WriteOp(
    ClientData clientData,	/* Not used. */
    Tcl_Interp *interp, 
    int argc, 
    char **argv)
{
    PrinterDevice *devicePtr;
    DWORD bytesLeft, numBytes;
    DOC_INFO_1 docInfo;
    DWORD jobId;
    char *title, *printer;
    register char *data;
    static int nextJob = 0;
    char string[200];

    if (GetOpenPrinter(interp, argv[2], &devicePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    bytesLeft = strlen(argv[3]);
    if (argc == 5) {
	title = argv[3], data = argv[4];
    } else {
	sprintf(string, "Print Job #%d", nextJob++);
	title = string, data = argv[3];
    }
    memset(&docInfo, 0, sizeof(DOC_INFO_1));
    docInfo.pDocName = title;
    if (devicePtr->fileName != NULL) {
	docInfo.pOutputFile = devicePtr->fileName;
    } else {
	docInfo.pOutputFile = NULL;
    }
    docInfo.pDatatype = "RAW";

    printer = devicePtr->printerInfoPtr->pPrinterName;

    /* Start new document */
    jobId = StartDocPrinter(devicePtr->hPrinter, 1, (unsigned char *)&docInfo);
    if (jobId == 0) {
	Tcl_AppendResult(interp, "error starting document on \"", printer, 
		"\": ", Blt_Win32Error(), (char *)NULL);
	return TCL_ERROR;
    }
    /* Start new page */
    if (!StartPagePrinter(devicePtr->hPrinter)) {
	Tcl_AppendResult(interp, "error starting page on \"", printer, "\": ", 
		Blt_Win32Error(), (char *)NULL);
	return TCL_ERROR;
    }
    do {
	if (!WritePrinter(devicePtr->hPrinter, data, bytesLeft, &numBytes)) {
	    Tcl_AppendResult(interp, "can't write data to \"", printer, "\": ",
		Blt_Win32Error(), (char *)NULL);
	    EndDocPrinter(devicePtr->hPrinter);
	    return TCL_ERROR;
	}
	data += numBytes;
	bytesLeft -= numBytes;
    } while (bytesLeft > 0);
    /* End last page */
    if (!EndPagePrinter(devicePtr->hPrinter)) {
	Tcl_AppendResult(interp, "error ending page on \"", printer, "\": ", 
		Blt_Win32Error(), (char *)NULL);
	return TCL_ERROR;
    }
    /* End document */
    if (!EndDocPrinter(devicePtr->hPrinter)) {
	Tcl_AppendResult(interp, "error ending document on \"", printer, 
		"\": ", Blt_Win32Error(), (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

static Blt_OpSpec printerOps[] =
{
    {"close", 1, (Blt_Operation)CloseOp, 3, 3, "id", },
    {"enum", 1, (Blt_Operation)EnumOp, 3, 3, "attribute", },
    {"getattrs", 1, (Blt_Operation)GetattrOp, 4, 4, "id varName",},
    {"names", 1, (Blt_Operation)NamesOp, 2, 3, "?pattern?", },
    {"open", 1, (Blt_Operation)OpenOp, 3, 3, "printerName", },
    {"setattrs", 1, (Blt_Operation)SetattrOp, 4, 4, "id varName",},
    {"write", 1, (Blt_Operation)WriteOp, 4, 5, "id ?title? string", },
};

static int numPrinterOps = sizeof(printerOps) / sizeof(Blt_OpSpec);

/* ARGSUSED */
static int
PrinterCmd(
    ClientData clientData,	/* Not used. */
    Tcl_Interp *interp, 
    int argc, 
    char **argv)
{
    Blt_Operation proc;
    int result;

    proc = Blt_GetOperation(interp, numPrinterOps, printerOps, BLT_OPER_ARG1,
	argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    result = (*proc) (clientData, interp, argc, argv);
    return (result);
}

static void
PrinterCmdDeleted(
    ClientData clientData)	/* Not used. */
{
    if (initialized) {
	Tcl_HashEntry *hPtr;
	Tcl_HashSearch cursor;
	PrinterDevice *devicePtr;

	for (hPtr = Tcl_FirstHashEntry(&idTable, &cursor); hPtr != NULL;
	    hPtr = Tcl_NextHashEntry(&cursor)) {
	    devicePtr = (PrinterDevice *)Tcl_GetHashValue(hPtr);
	    devicePtr->hashPtr = NULL;
	    DestroyPrinter(devicePtr);
	}
	Tcl_DeleteHashTable(&printerTable);
	Tcl_DeleteHashTable(&idTable);
	initialized = FALSE;
    }
}

int
Blt_PrinterInit(Tcl_Interp *interp)
{
    static Blt_CmdSpec cmdSpec = {"printer", PrinterCmd, PrinterCmdDeleted };

    if (!initialized) {
	Tcl_InitHashTable(&idTable, TCL_STRING_KEYS);
	Tcl_InitHashTable(&printerTable, TCL_STRING_KEYS);
	nextId = 0;
	initialized = TRUE;
    }
    if (Blt_InitCmd(interp, "blt", &cmdSpec) == NULL) {
	return TCL_ERROR;
    }
    return TCL_OK;
}


/* Public routines */
int
Blt_GetOpenPrinter(
    Tcl_Interp *interp, 
    const char *id, 
    Drawable *drawablePtr)
{
    PrinterDevice *devicePtr;

    if (GetOpenPrinter(interp, id, &devicePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (devicePtr->drawable.hDC == NULL) {
	PRINTER_INFO_2 *infoPtr = devicePtr->printerInfoPtr;
	char *printer;

	devicePtr->drawable.hDC = CreateDC(
	    infoPtr->pDriverName, 
	    infoPtr->pDevMode->dmDeviceName,
	    NULL,
	    infoPtr->pDevMode);
	printer = devicePtr->printerInfoPtr->pPrinterName;
	if (devicePtr->drawable.hDC == NULL) {
	    Tcl_AppendResult(interp, "can't allocate printer DC for \"", 
		printer, "\": ", Blt_Win32Error(), (char *)NULL);
	    return TCL_ERROR;
	}
        devicePtr->drawable.type = TWD_WINDC;
    }
    *drawablePtr = (Drawable)&(devicePtr->drawable);
    return TCL_OK;
}

int
Blt_StartPrintJob(
    Tcl_Interp *interp,
    const char *id)
{
    PrinterDevice *devicePtr;
    DOCINFO docInfo;
    char *printer;
    int jobId;

    if (GetOpenPrinter(interp, id, &devicePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    memset((char *)&docInfo, 0, sizeof(DOCINFO));
    docInfo.cbSize = sizeof(DOCINFO);
    docInfo.lpszDocName = NULL;
    docInfo.lpszOutput = devicePtr->fileName;
    jobId = StartDoc(devicePtr->drawable.hDC, &docInfo);
    printer = devicePtr->printerInfoPtr->pPrinterName;
    if (jobId == 0) {
	Tcl_AppendResult(interp, "error starting document on \"", printer,
		"\": ", Blt_Win32Error(), (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

int
Blt_EndPrintJob(
    Tcl_Interp *interp,
    const char *id)
{
    PrinterDevice *devicePtr;

    if (GetOpenPrinter(interp, id, &devicePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    EndPage(devicePtr->drawable.hDC);
    EndDoc(devicePtr->drawable.hDC);
    return TCL_OK;
}

#endif
