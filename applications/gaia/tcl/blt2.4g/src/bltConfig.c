/*
 * bltConfig.c --
 *
 *	This module implements custom configuration options for the BLT
 *	toolkit.
 *
 * Copyright 1991-1998 Lucent Technologies, Inc.
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
 */

#include "bltInt.h"
#include <X11/Xutil.h>

static int StringToFill _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *string, char *widgRec,
	int flags));
static char *FillToString _ANSI_ARGS_((ClientData, Tk_Window, char *, int,
	Tcl_FreeProc **));

Tk_CustomOption bltFillOption =
{
    StringToFill, FillToString, (ClientData)0
};

static int StringToPad _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *string, char *widgRec,
	int offset));
static char *PadToString _ANSI_ARGS_((ClientData clientData, Tk_Window tkwin,
	char *widgRec, int offset, Tcl_FreeProc **freeProcPtr));

Tk_CustomOption bltPadOption =
{
    StringToPad, PadToString, (ClientData)0
};

static int StringToLength _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
	Tk_Window tkwin, char *string, char *widgRec, int flags));
static char *LengthToString _ANSI_ARGS_((ClientData, Tk_Window, char *, int,
	Tcl_FreeProc **));

Tk_CustomOption bltLengthOption =
{
    StringToLength, LengthToString, (ClientData)0
};


static int StringToDashes _ANSI_ARGS_((ClientData, Tcl_Interp *, Tk_Window,
	char *, char *, int));
static char *DashesToString _ANSI_ARGS_((ClientData, Tk_Window, char *, int,
	Tcl_FreeProc **));

Tk_CustomOption bltDashesOption =
{
    StringToDashes, DashesToString, (ClientData)0
};

static int StringToShadow _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
	Tk_Window tkwin, char *string, char *widgRec, int offset));
static char *ShadowToString _ANSI_ARGS_((ClientData clientData, Tk_Window tkwin,
	char *widgRec, int offset, Tcl_FreeProc **freeProcPtr));

Tk_CustomOption bltShadowOption =
{
    StringToShadow, ShadowToString, (ClientData)0
};

static int StringToUid _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *string, char *widgRec,
	int flags));
static char *UidToString _ANSI_ARGS_((ClientData, Tk_Window, char *, int,
	Tcl_FreeProc **));

Tk_CustomOption bltUidOption =
{
    StringToUid, UidToString, (ClientData)0
};

static int StringToState _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *string, char *widgRec,
	int flags));
static char *StateToString _ANSI_ARGS_((ClientData, Tk_Window, char *, int,
	Tcl_FreeProc **));

Tk_CustomOption bltStateOption =
{
    StringToState, StateToString, (ClientData)0
};

static int StringToList _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
	Tk_Window tkwin, char *string, char *widgRec, int flags));
static char *ListToString _ANSI_ARGS_((ClientData, Tk_Window, char *, int,
	Tcl_FreeProc **));

Tk_CustomOption bltListOption =
{
    StringToList, ListToString, (ClientData)0
};

/*
 *----------------------------------------------------------------------
 *
 * Blt_NameOfFill --
 *
 *	Converts the integer representing the fill style into a string.
 *
 *----------------------------------------------------------------------
 */
char *
Blt_NameOfFill(fill)
    Fill fill;
{
    switch (fill) {
    case FILL_X:
	return "x";
    case FILL_Y:
	return "y";
    case FILL_NONE:
	return "none";
    case FILL_BOTH:
	return "both";
    default:
	return "unknown value";
    }
}

/*
 *----------------------------------------------------------------------
 *
 * StringToFill --
 *
 *	Converts the fill style string into its numeric representation.
 *
 *	Valid style strings are:
 *
 *	  "none"   Use neither plane.
 * 	  "x"	   X-coordinate plane.
 *	  "y"	   Y-coordinate plane.
 *	  "both"   Use both coordinate planes.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToFill(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* Fill style string */
    char *widgRec;		/* Cubicle structure record */
    int offset;			/* Offset of style in record */
{
    Fill *fillPtr = (Fill *)(widgRec + offset);
    unsigned int length;
    char c;

    c = string[0];
    length = strlen(string);
    if ((c == 'n') && (strncmp(string, "none", length) == 0)) {
	*fillPtr = FILL_NONE;
    } else if ((c == 'x') && (strncmp(string, "x", length) == 0)) {
	*fillPtr = FILL_X;
    } else if ((c == 'y') && (strncmp(string, "y", length) == 0)) {
	*fillPtr = FILL_Y;
    } else if ((c == 'b') && (strncmp(string, "both", length) == 0)) {
	*fillPtr = FILL_BOTH;
    } else {
	Tcl_AppendResult(interp, "bad argument \"", string,
	    "\": should be \"none\", \"x\", \"y\", or \"both\"", (char *)NULL);
	return TCL_ERROR;
    }
    return (TCL_OK);
}

/*
 *----------------------------------------------------------------------
 *
 * FillToString --
 *
 *	Returns the fill style string based upon the fill flags.
 *
 * Results:
 *	The fill style string is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
FillToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Widget structure record */
    int offset;			/* Offset of fill in widget record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    Fill fill = *(Fill *)(widgRec + offset);

    return (Blt_NameOfFill(fill));
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_StringToFlag --
 *
 *	Converts the fill style string into its numeric representation.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
int
Blt_StringToFlag(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* Bit mask to be tested in status word */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* Fill style string */
    char *widgRec;		/* Cubicle structure record */
    int offset;			/* Offset of style in record */
{
    unsigned int mask = (unsigned int)clientData;	/* Bit to be tested */
    int *flagPtr = (int *)(widgRec + offset);
    int bool;

    if (Tcl_GetBoolean(interp, string, &bool) != TCL_OK) {
	return TCL_ERROR;
    }
    if (bool) {
	*flagPtr |= mask;
    } else {
	*flagPtr &= ~mask;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_FlagToString --
 *
 *	Returns the fill style string based upon the fill flags.
 *
 * Results:
 *	The fill style string is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
char *
Blt_FlagToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* Bit mask to be test in status word */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Widget structure record */
    int offset;			/* Offset of fill in widget record */
    Tcl_FreeProc **freeProcPtr;	/* Unused */
{
    unsigned int mask = (unsigned int)clientData;	/* Bit to be tested */
    unsigned int bool = *(unsigned int *)(widgRec + offset);

    return (bool & mask) ? "1" : "0";
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_GetLength --
 *
 *	Like Tk_GetPixels, but doesn't allow negative pixel values.
 *
 * Results:
 *	A standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
int
Blt_GetLength(interp, tkwin, string, valuePtr)
    Tcl_Interp *interp;
    Tk_Window tkwin;
    char *string;
    int *valuePtr;
{
    int length;

    if (Tk_GetPixels(interp, tkwin, string, &length) != TCL_OK) {
	return TCL_ERROR;
    }
    if (length < 0) {
	Tcl_AppendResult(interp, "can't have negative screen distance \"",
	    string, "\"", (char *)NULL);
	return TCL_ERROR;
    }
    *valuePtr = length;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * StringToLength --
 *
 *	Like TK_CONFIG_PIXELS, but adds an extra check for negative
 *	values.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToLength(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* Window */
    char *string;		/* Pixel value string */
    char *widgRec;		/* Widget record */
    int offset;			/* Offset of pixel size in record */
{
    int *lengthPtr = (int *)(widgRec + offset);

    return (Blt_GetLength(interp, tkwin, string, lengthPtr));
}

/*
 *----------------------------------------------------------------------
 *
 * LengthToString --
 *
 *	Returns the string representing the positive pixel size.
 *
 * Results:
 *	The pixel size string is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
LengthToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Widget structure record */
    int offset;			/* Offset in widget record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    int length = *(int *)(widgRec + offset);
    char *result;
    char string[200];

    sprintf(string, "%d", length);
    result = strdup(string);
    assert(result);
    *freeProcPtr = (Tcl_FreeProc *)free;
    return (result);
}

/*
 *----------------------------------------------------------------------
 *
 * StringToPad --
 *
 *	Convert a string into two pad values.  The string may be in one of
 *	the following forms:
 *
 *	    n    - n is a non-negative integer. This sets both
 *		   pad values to n.
 *	  {n m}  - both n and m are non-negative integers. side1
 *		   is set to n, side2 is set to m.
 *
 * Results:
 *	If the string is successfully converted, TCL_OK is returned.
 *	Otherwise, TCL_ERROR is returned and an error message is left in
 *	interp->result.
 *
 * Side Effects:
 *	The padding structure passed is updated with the new values.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToPad(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* Window */
    char *string;		/* Pixel value string */
    char *widgRec;		/* Widget record */
    int offset;			/* Offset of pad in widget */
{
    Pad *padPtr = (Pad *)(widgRec + offset);
    int numElem;
    int pad, result;
    char **padArr;

    if (Tcl_SplitList(interp, string, &numElem, &padArr) != TCL_OK) {
	return TCL_ERROR;
    }
    result = TCL_ERROR;
    if ((numElem < 1) || (numElem > 2)) {
	Tcl_AppendResult(interp, "wrong # elements in padding list",
	    (char *)NULL);
	goto error;
    }
    if (Blt_GetLength(interp, tkwin, padArr[0], &pad) != TCL_OK) {
	goto error;
    }
    padPtr->side1 = pad;
    if ((numElem > 1) &&
	(Blt_GetLength(interp, tkwin, padArr[1], &pad) != TCL_OK)) {
	goto error;
    }
    padPtr->side2 = pad;
    result = TCL_OK;

  error:
    free((char *)padArr);
    return (result);
}

/*
 *----------------------------------------------------------------------
 *
 * PadToString --
 *
 *	Converts the two pad values into a Tcl list.  Each pad has two
 *	pixel values.  For vertical pads, they represent the top and bottom
 *	margins.  For horizontal pads, they're the left and right margins.
 *	All pad values are non-negative integers.
 *
 * Results:
 *	The padding list is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
PadToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Structure record */
    int offset;			/* Offset of pad in record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    Pad *padPtr = (Pad *)(widgRec + offset);
    char *result;
    char string[200];

    sprintf(string, "%d %d", padPtr->side1, padPtr->side2);
    result = strdup(string);
    if (result == NULL) {
	return "out of memory";
    }
    *freeProcPtr = (Tcl_FreeProc *)free;
    return (result);
}

/*
 *----------------------------------------------------------------------
 *
 * StringToShadow --
 *
 *	Convert a string into two pad values.  The string may be in one of
 *	the following forms:
 *
 *	    n    - n is a non-negative integer. This sets both
 *		   pad values to n.
 *	  {n m}  - both n and m are non-negative integers. side1
 *		   is set to n, side2 is set to m.
 *
 * Results:
 *	If the string is successfully converted, TCL_OK is returned.
 *	Otherwise, TCL_ERROR is returned and an error message is left in
 *	interp->result.
 *
 * Side Effects:
 *	The padding structure passed is updated with the new values.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToShadow(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* Window */
    char *string;		/* Pixel value string */
    char *widgRec;		/* Widget record */
    int offset;			/* Offset of pad in widget */
{
    Shadow *shadowPtr = (Shadow *) (widgRec + offset);
    XColor *colorPtr;
    int dropOffset;

    colorPtr = NULL;
    dropOffset = 0;
    if ((string != NULL) && (string[0] != '\0')) {
	int numElem;
	char **elemArr;

	if (Tcl_SplitList(interp, string, &numElem, &elemArr) != TCL_OK) {
	    return TCL_ERROR;
	}
	if ((numElem < 1) || (numElem > 2)) {
	    Tcl_AppendResult(interp, "wrong # elements in drop shadow value",
		(char *)NULL);
	    free((char *)elemArr);
	    return TCL_ERROR;
	}
	colorPtr = Tk_GetColor(interp, tkwin, Tk_GetUid(elemArr[0]));
	if (colorPtr == NULL) {
	    free((char *)elemArr);
	    return TCL_ERROR;
	}
	dropOffset = 1;
	if (numElem == 2) {
	    if (Blt_GetLength(interp, tkwin, elemArr[1], &dropOffset)
		!= TCL_OK) {
		Tk_FreeColor(colorPtr);
		free((char *)elemArr);
		return TCL_ERROR;
	    }
	}
	free((char *)elemArr);
    }
    if (shadowPtr->color != NULL) {
	Tk_FreeColor(shadowPtr->color);
    }
    shadowPtr->color = colorPtr;
    shadowPtr->offset = dropOffset;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ShadowToString --
 *
 *	Converts the two pad values into a Tcl list.  Each pad has two
 *	pixel values.  For vertical pads, they represent the top and bottom
 *	margins.  For horizontal pads, they're the left and right margins.
 *	All pad values are non-negative integers.
 *
 * Results:
 *	The padding list is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
ShadowToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Structure record */
    int offset;			/* Offset of pad in record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    Shadow *shadowPtr = (Shadow *) (widgRec + offset);
    char *result;

    result = "";
    if (shadowPtr->color != NULL) {
	char string[200];

	sprintf(string, "%s %d", Tk_NameOfColor(shadowPtr->color),
	    shadowPtr->offset);
	result = strdup(string);
	*freeProcPtr = (Tcl_FreeProc *)free;
    }
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * GetDashes --
 *
 *	Converts a Tcl list of dash values into a dash list ready for
 *	use with XSetDashes.
 *
 * 	A valid list dash values can have zero through 11 elements
 *	(PostScript limit).  Values must be between 1 and 255. Although
 *	a list of 0 (like the empty string) means no dashes.
 *
 * Results:
 *	A standard Tcl result. If the list represented a valid dash
 *	list TCL_OK is returned and *dashesPtr* will contain the
 *	valid dash list. Otherwise, TCL_ERROR is returned and
 *	interp->result will contain an error message.
 *
 *
 *----------------------------------------------------------------------
 */
static int
GetDashes(interp, string, dashesPtr)
    Tcl_Interp *interp;
    char *string;
    Dashes *dashesPtr;
{
    if ((string == NULL) || (*string == '\0')) {
	dashesPtr->numValues = 0;
    } else if (strcmp(string, "dash") == 0) {	/* 5 2 */
	dashesPtr->numValues = 2;
	dashesPtr->valueArr[0] = 5;
	dashesPtr->valueArr[1] = 2;
    } else if (strcmp(string, "dot") == 0) {	/* 1 */
	dashesPtr->numValues = 1;
	dashesPtr->valueArr[0] = 1;
    } else if (strcmp(string, "dashdot") == 0) {	/* 2 4 2 */
	dashesPtr->numValues = 3;
	dashesPtr->valueArr[0] = 2;
	dashesPtr->valueArr[1] = 4;
	dashesPtr->valueArr[2] = 2;
    } else if (strcmp(string, "dashdotdot") == 0) {	/* 2 4 2 2 */
	dashesPtr->numValues = 4;
	dashesPtr->valueArr[0] = 2;
	dashesPtr->valueArr[1] = 4;
	dashesPtr->valueArr[2] = 2;
	dashesPtr->valueArr[3] = 2;
    } else {
	int numValues;
	char **strArr;
	long int value;
	register int i;

	if (Tcl_SplitList(interp, string, &numValues, &strArr) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (numValues > 11) {	/* This is the postscript limit */
	    Tcl_AppendResult(interp, "too many values for dash list in \"", string, "\"",
		(char *)NULL);
	    free((char *)strArr);
	    return TCL_ERROR;
	}
	for (i = 0; i < numValues; i++) {
	    if (Tcl_ExprLong(interp, strArr[i], &value) != TCL_OK) {
		free((char *)strArr);
		return TCL_ERROR;
	    }
	    /*
	     * Backward compatibility:
	     * Allow list of 0 to turn off dashes
	     */
	    if ((value == 0) && (numValues == 1)) {
		break;
	    }
	    if ((value < 1) || (value > 255)) {
		Tcl_AppendResult(interp, "dash value \"", strArr[i],
		    "\" is out of range", (char *)NULL);
		free((char *)strArr);
		return TCL_ERROR;
	    }
	    dashesPtr->valueArr[i] = (unsigned char)value;
	}
	dashesPtr->numValues = i;
	free((char *)strArr);
    }
    /* Make sure the array ends with a NUL byte  */
    dashesPtr->valueArr[dashesPtr->numValues] = '\0';
    return TCL_OK;

}

/*
 *----------------------------------------------------------------------
 *
 * StringToDashes --
 *
 *	Convert the list of dash values into a dashes array.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *	The Dashes structure is updated with the new dash list.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToDashes(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* New dash value list */
    char *widgRec;		/* Widget record */
    int offset;			/* offset to Dashes structure */
{
    Dashes *dashesPtr = (Dashes *)(widgRec + offset);

    return (GetDashes(interp, string, dashesPtr));
}

/*
 *----------------------------------------------------------------------
 *
 * DashesToString --
 *
 *	Convert the dashes array into a list of values.
 *
 * Results:
 *	The string representing the dashes returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
DashesToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Widget record */
    int offset;			/* offset of Dashes in record */
    Tcl_FreeProc **freeProcPtr;	/* Memory deallocation scheme to use */
{
    Dashes *dashesPtr = (Dashes *)(widgRec + offset);
    Tcl_DString dStr;
    register int i;
    char *result;
    char string[200];

    if (dashesPtr->numValues == 0) {
	return "";
    }
    Tcl_DStringInit(&dStr);
    for (i = 0; i < dashesPtr->numValues; i++) {
	sprintf(string, "%d", (int)dashesPtr->valueArr[i]);
	Tcl_DStringAppendElement(&dStr, string);
    }
    result = Tcl_DStringValue(&dStr);
    if (result == dStr.staticSpace) {
	result = strdup(result);
    }
    *freeProcPtr = (Tcl_FreeProc *)free;
    return (result);
}

/*
 *----------------------------------------------------------------------
 *
 * StringToUid --
 *
 *	Converts the string to a BLT Uid. Blt Uid's are hashed, reference
 *	counted strings.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToUid(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* Fill style string */
    char *widgRec;		/* Cubicle structure record */
    int offset;			/* Offset of style in record */
{
    Tk_Uid *uidPtr = (Tk_Uid *)(widgRec + offset);
    Tk_Uid newId;

    newId = NULL;
    if ((string != NULL) && (*string != '\0')) {
	newId = Blt_GetUid(string);
    }
    if (*uidPtr != NULL) {
	Blt_FreeUid(*uidPtr);
    }
    *uidPtr = newId;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * UidToString --
 *
 *	Returns the fill style string based upon the fill flags.
 *
 * Results:
 *	The fill style string is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
UidToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Widget structure record */
    int offset;			/* Offset of fill in widget record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    Tk_Uid uid = *(Tk_Uid *)(widgRec + offset);

    return (uid == NULL) ? "" : uid;
}

/*
 *----------------------------------------------------------------------
 *
 * StringToState --
 *
 *	Converts the string to a state value. Valid states are
 *	disabled, normal.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToState(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* String representation of option value */
    char *widgRec;		/* Widget structure record */
    int offset;			/* Offset of field in record */
{
    int *statePtr = (int *)(widgRec + offset);

    if (strcmp(string, "normal") == 0) {
	*statePtr = STATE_NORMAL;
    } else if (strcmp(string, "disabled") == 0) {
	*statePtr = STATE_DISABLED;
    } else if (strcmp(string, "active") == 0) {
	*statePtr = STATE_ACTIVE;
    } else {
	Tcl_AppendResult(interp, "bad state \"", string,
	    "\": should be normal, active, or disabled", (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * StateToString --
 *
 *	Returns the string representation of the state configuration field
 *
 * Results:
 *	The string is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
StateToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Widget structure record */
    int offset;			/* Offset of fill in widget record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    int state = *(int *)(widgRec + offset);

    switch (state) {
    case STATE_ACTIVE:
	return "active";
    case STATE_DISABLED:
	return "disabled";
    case STATE_NORMAL:
	return "normal";
    default:
	return "???";
    }
}

/*
 *----------------------------------------------------------------------
 *
 * StringToList --
 *
 *	Converts the string to a list.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToList(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* String representation of option value */
    char *widgRec;		/* Widget structure record */
    int offset;			/* Offset of field in record */
{
    char ***listPtr = (char ***)(widgRec + offset);
    char **elemArr;
    int numElem;

    if (*listPtr != NULL) {
	free((char *)*listPtr);
	*listPtr = NULL;
    }
    if ((string == NULL) || (*string == '\0')) {
	return TCL_OK;
    }
    if (Tcl_SplitList(interp, string, &numElem, &elemArr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (numElem > 0) {
	*listPtr = elemArr;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ListToString --
 *
 *	Returns the string representation of the state configuration field
 *
 * Results:
 *	The string is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
ListToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* Not used. */
    char *widgRec;		/* Widget structure record. */
    int offset;			/* Offset of fill in widget record. */
    Tcl_FreeProc **freeProcPtr;	/* Not used. */
{
    char **list = *(char ***)(widgRec + offset);
    register char **p;
    char *result;
    Tcl_DString dString;

    if (list == NULL) {
	return "";
    }
    Tcl_DStringInit(&dString);
    for (p = list; *p != NULL; p++) {
	Tcl_DStringAppendElement(&dString, *p);
    }
    result = Tcl_DStringValue(&dString);
    if (result == dString.staticSpace) {
	result = strdup(result);
    }
    Tcl_DStringFree(&dString);
    *freeProcPtr = (Tcl_FreeProc *)free;
    return result;
}
