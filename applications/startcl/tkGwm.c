/*
 * tkGwm.c --
 *
 *	This module implements "Gwm" widgets.  (See SUN/130)
 *
 * Copyright (c) 1994  Daresbury & Rutherford Appleton Laboratories
 * Copyright (c) 2009  Science and Technology Facilities Council
 * All rights reserved.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#if defined(HAVE_MALLOC_H)
#  include <malloc.h>
#endif

#include "tcl.h"
#include "tk.h"

#include <X11/Xlib.h>
#include <X11/Xatom.h>

#include "gwm.h"
#include "gwm_sys.h"
#include "gwm_err.h"
#include "tkGwm.h"
#include "tkGwm_sys.h"

/*
 * Information used for argv parsing for widget commands.
 */

static Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_COLOR, "-background", "background", "Background",
        "black", Tk_Offset(Gwm, bg), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
        "white", Tk_Offset(Gwm, fg), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_COLOR, "-ovcolour", "ovColour", "OvColour",
        "white", Tk_Offset(Gwm, ovcolour), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_INT, "-xoffset", "xOffset", "XOffset",
        "0", Tk_Offset(Gwm, xoffset), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_INT, "-yoffset", "yOffset", "YOffset",
        "0", Tk_Offset(Gwm, yoffset), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_INT, "-xovoffset", "xOvOffset", "XOvOffset",
        "0", Tk_Offset(Gwm, xovoffset), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_INT, "-yovoffset", "yOvOffset", "YOvOffset",
        "0", Tk_Offset(Gwm, yovoffset), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_BOOLEAN, "-crosshair", "crosshair", "Crosshair",
        "0", Tk_Offset(Gwm, crosshair), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_COLOR, "-crosscolour", "crossColour", "CrossColour",
        "white", Tk_Offset(Gwm, crosscolour), 0, (Tk_CustomOption *)NULL},
/* Don't change the order of the above - GwmConfigure depends on it */
    {TK_CONFIG_SYNONYM, "-bg", "background", (char *) NULL,
        (char *) NULL, 0, 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_INT, "-colours", "colours", "Colours",
        "0", Tk_Offset(Gwm, cols), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_ACTIVE_CURSOR, "-cursor", "cursor", "Cursor",
        "", Tk_Offset(Gwm, cursor), TK_CONFIG_NULL_OK, (Tk_CustomOption *)NULL},
    {TK_CONFIG_SYNONYM, "-fg", "foreground", (char *) NULL,
        (char *) NULL, 0, 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_SYNONYM, "-gwmname", "name", (char *) NULL,
        (char *) NULL, 0, 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_INT, "-jpegquality", "jpegQuality", "JpegQuality",
        "75", Tk_Offset(Gwm, quality), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_BOOLEAN, "-jpegprogressive", "jpegProgressive",
        "JpegProgressive",
        "0", Tk_Offset(Gwm, progressive), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_PIXELS, "-height", "height", "Height",
        "512", Tk_Offset(Gwm, height), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_INT, "-mincolours", "minColours", "MinColours",
        "2", Tk_Offset(Gwm, mincols), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_STRING, "-name", "name", "Name",
        "xwindows", Tk_Offset(Gwm, name), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_BOOLEAN, "-overlay", "overlay", "Overlay",
        "0", Tk_Offset(Gwm, overlay), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_MM, "-paperwidth", "paperWidth", "PaperWidth",
        "180m", Tk_Offset(Gwm, paperwidth), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_MM, "-paperheight", "paperHeight", "PaperHeight",
        "250m", Tk_Offset(Gwm, paperheight), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_COLOR, "-printbackground", "printBackground", "PrintBackground",
        "white", Tk_Offset(Gwm, printbg), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_SYNONYM, "-printbg", "printBackground", (char *) NULL,
        (char *) NULL, 0, 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_STRING, "-printformat", "printFormat", "PrintFormat",
        "postscript", Tk_Offset(Gwm, printformat), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_STRING, "-takefocus", "takeFocus", "TakeFocus",
	(char *) NULL, Tk_Offset(Gwm, takeFocus), TK_CONFIG_NULL_OK,
	(Tk_CustomOption *)NULL},
    {TK_CONFIG_PIXELS, "-width", "width", "Width",
        "768", Tk_Offset(Gwm, width), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_STRING, "-printvariable", "printVariable", "PrintVariable",
        "gwm_printvar", Tk_Offset(Gwm, printvar), 0, (Tk_CustomOption *)NULL},
    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
	(char *) NULL, 0, 0, (Tk_CustomOption *)NULL}
};

/*
   Static Tk_ItemType structure for creating the gwm canvas item type.
*/
static char gwmName[] = "gwm";
static Tk_ItemType itemType;

static Tk_CustomOption tagsOption;

Tk_ConfigSpec itemSpecs[] = {
    {TK_CONFIG_COLOR, "-background", "background", "Background",
        "black", Tk_Offset(GwmItem, bg), 0, &tagsOption},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
        "white", Tk_Offset(GwmItem, fg), 0, &tagsOption},
    {TK_CONFIG_STRING, "-command", "command", "Command",
        (char *)NULL, Tk_Offset(GwmItem, command), 0, &tagsOption},
#if 0
    {TK_CONFIG_COLOR, "-ovcolour", "ovColour", "OvColour",
        "white", Tk_Offset(GwmItem, ovcolour), 0, &tagsOption},
    {TK_CONFIG_INT, "-xovoffset", "xOvOffset", "XOvOffset",
        "0", Tk_Offset(GwmItem, xovoffset), 0, &tagsOption},
    {TK_CONFIG_INT, "-yovoffset", "yOvOffset", "YOvOffset",
        "0", Tk_Offset(GwmItem, yovoffset), 0, &tagsOption},
#endif
/* Don't change the order of the above - tkGwmItemConfigure depends on it */
    {TK_CONFIG_PIXELS, "-height", "height", "Height",
        "512", Tk_Offset(GwmItem, height), 0, &tagsOption},
    {TK_CONFIG_SYNONYM, "-bg", "background", (char *) NULL,
        (char *) NULL, 0, 0, &tagsOption},
    {TK_CONFIG_INT, "-colours", "colours", "Colours",
        "0", Tk_Offset(GwmItem, cols), 0, &tagsOption},
    {TK_CONFIG_SYNONYM, "-fg", "foreground", (char *) NULL,
        (char *) NULL, 0, 0, &tagsOption},
    {TK_CONFIG_SYNONYM, "-gwmname", "name", (char *) NULL,
        (char *) NULL, 0, 0, &tagsOption},
    {TK_CONFIG_INT, "-mincolours", "minColours", "MinColours",
        "2", Tk_Offset(GwmItem, mincols), 0, &tagsOption},
    {TK_CONFIG_STRING, "-name", "name", "Name",
        "xwindows", Tk_Offset(GwmItem, name), 0, &tagsOption},
#if 0
    {TK_CONFIG_BOOLEAN, "-overlay", "overlay", "Overlay",
        "0", Tk_Offset(GwmItem, overlay), 0, &tagsOption},
#endif
    {TK_CONFIG_PIXELS, "-width", "width", "Width",
        "768", Tk_Offset(GwmItem, width), 0, &tagsOption},
    {TK_CONFIG_CUSTOM, "-tags", (char *) NULL, (char *) NULL,
        (char *) NULL, 0, TK_CONFIG_NULL_OK, &tagsOption},
    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
	(char *) NULL, 0, 0, &tagsOption}
};

static int GwmCmd _ANSI_ARGS_((ClientData, Tcl_Interp*, int, char**));
static int  GwmConfigure _ANSI_ARGS_((Tcl_Interp*, Gwm*, int, char**, int));
static void GwmDestroy _ANSI_ARGS_((char*));
static void GwmDisplay _ANSI_ARGS_((ClientData));
static void GwmEventProc _ANSI_ARGS_((ClientData, XEvent*));
static int GwmWidgetCmd _ANSI_ARGS_((ClientData, Tcl_Interp*, int, char**));
static void DrawCursor(Gwm*, int, int);
static void Clear(Gwm*);
static void OvClear(Gwm*);
static int SendKeypress(Tcl_Interp*, Gwm*, int, int, char*);


int Tkgwm_Init
(
Tcl_Interp *interp
)
{
/*
 * Enable stubs interface
*/
   if ( Tcl_InitStubs( interp, "8.0", 0 ) == NULL ) return TCL_ERROR;
   if ( Tk_InitStubs( interp, "8.0", 0 ) == NULL ) return TCL_ERROR;

/*
 * Create the gwm command for creating gwm widgets.
*/
   Tcl_CreateCommand(interp, "gwm",
                     (Tcl_CmdProc *)GwmCmd,
                     (ClientData)Tk_MainWindow(interp),
                     (void (*)()) NULL);

/*
 * Initialize the tagsOptions structure.
*/
   tagsOption.parseProc = Tk_CanvasTagsParseProc;
   tagsOption.printProc = Tk_CanvasTagsPrintProc;
   tagsOption.clientData = (ClientData) NULL;

/*
 * Create the gwm canvas item type.
*/
   itemType.name = gwmName;
   itemType.itemSize = (int) sizeof(GwmItem);
   itemType.createProc = (Tk_ItemCreateProc *)tkgwmItemCreate;
   itemType.configSpecs = itemSpecs;
   itemType.configProc = (Tk_ItemConfigureProc *) tkgwmItemConfigure;
   itemType.coordProc = (Tk_ItemCoordProc *)tkgwmItemCoord;
   itemType.deleteProc = tkgwmItemDelete;
   itemType.displayProc = tkgwmItemDisplay;
   itemType.alwaysRedraw = 0;
   itemType.pointProc = tkgwmItemPoint;
   itemType.areaProc = tkgwmItemArea;
   itemType.postscriptProc = tkgwmItemPostScript;
   itemType.translateProc = tkgwmItemTranslate;
   itemType.scaleProc = tkgwmItemScale;
   itemType.indexProc = NULL;
   itemType.icursorProc = NULL;
   itemType.selectionProc = NULL;
   itemType.insertProc = NULL;
   itemType.dCharsProc = NULL;

   Tk_CreateItemType(&itemType);

   return TCL_OK;
}



/*
 *--------------------------------------------------------------
 *
 * Gwm --
 *
 *	This procedure is invoked to process the "gwm" Tcl
 *	command.  It creates a new "gwm" widget.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	A new widget is created and configured.
 *
 *--------------------------------------------------------------
 */

#define BUFLEN 64

static int
GwmCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window associated with
				 * interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    Tk_Window winMain = (Tk_Window) clientData;
    Gwm *gwmPtr;
    Tk_Window tkwin;
    XGCValues gcvalues;
    int status;
    char msgbuf[BUFLEN];
    XWindowAttributes winatt;
    XVisualInfo vinfo_template;
    int nitems;


    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args:  should be \"",
		argv[0], " pathName ?options?\"", (char *) NULL);
	return TCL_ERROR;
    }

    tkwin = Tk_CreateWindowFromPath(interp, winMain, argv[1], (char *) NULL);
    if (tkwin == NULL) {
	return TCL_ERROR;
    }

    /*
     * Allocate and initialize the widget record.
     */

    gwmPtr = (Gwm *) ckalloc(sizeof(Gwm));
    gwmPtr->tkwin = tkwin;
    gwmPtr->display = Tk_Display(tkwin);
    gwmPtr->interp = interp;
    gwmPtr->bg = NULL;
    gwmPtr->fg = NULL;
    gwmPtr->ovcolour = NULL;
    gwmPtr->name = NULL;
    gwmPtr->printformat = NULL;
    gwmPtr->printbg = NULL;
    gwmPtr->printvar = NULL;
    gwmPtr->printing = 0;
    gwmPtr->cursor = None;
    gwmPtr->crosscolour = NULL;
    gwmPtr->crossx = 0;
    gwmPtr->crossy = 0;
    gwmPtr->takeFocus = NULL;

    Tk_SetClass(gwmPtr->tkwin, "Gwm");

    if (GwmConfigure(interp, gwmPtr, argc-2, argv+2, 0) != TCL_OK) {
	Tk_DestroyWindow(gwmPtr->tkwin);
	return TCL_ERROR;
    }

/*
 *  Make the window exist as the window id is needed in order to turn it
 *  into a GWM window.
 */
    Tk_MakeWindowExist(gwmPtr->tkwin);

/*
 *  Make the window into a gwm window
 */
    status = GWM_MakeIntoWindow(gwmPtr->display,
	Tk_WindowId(gwmPtr->tkwin), gwmPtr->name,
	gwmPtr->width, gwmPtr->height, gwmPtr->cols, gwmPtr->mincols,
	gwmPtr->fg->pixel, gwmPtr->bg->pixel, gwmPtr->overlay,
	gwmPtr->ovcolour->pixel);

/*
 *  Fill in the window info structure and create a gc for copying the
 *  pixmap to the window
 */
    if (status == GWM_SUCCESS) {
	status = GWM_GetWinInfo(gwmPtr->display,
		Tk_WindowId(gwmPtr->tkwin), &(gwmPtr->info));
	gwmPtr->info->gc = XCreateGC( gwmPtr->display,
		Tk_WindowId(gwmPtr->tkwin), 0, &gcvalues);
	gwmPtr->cols = gwmPtr->info->ctsize;

/*
 *  Set the scroll offsets in the info structure
 */
	gwmPtr->info->x_offset = gwmPtr->xoffset;
	gwmPtr->info->y_offset = gwmPtr->yoffset;
	gwmPtr->info->x_ov_offset = gwmPtr->xovoffset;
	gwmPtr->info->y_ov_offset = gwmPtr->yovoffset;

/*
 *  Set the offset properties
 */
	status = GWM_SetScroll( gwmPtr->display, Tk_WindowId(gwmPtr->tkwin),
		gwmPtr->xoffset, gwmPtr->yoffset);
	status = GWM_SetOvScroll( gwmPtr->display, Tk_WindowId(gwmPtr->tkwin),
		gwmPtr->xovoffset, gwmPtr->yovoffset);

/*
 *  Fill out the visual info structure.
 */
        XGetWindowAttributes( gwmPtr->display, Tk_WindowId(gwmPtr->tkwin),
                &winatt);
        vinfo_template.visualid = XVisualIDFromVisual( winatt.visual );
        gwmPtr->vinfo = XGetVisualInfo( gwmPtr->display, VisualIDMask,
                &vinfo_template, &nitems);


    } else {
	GWM_ErrorMessage(status, msgbuf, BUFLEN);
	Tcl_AppendResult(interp, msgbuf, (char *) NULL);
	Tk_DestroyWindow(gwmPtr->tkwin);
	return TCL_ERROR;
    }

/*
 *  Set up tk event handling for the widget
 */
    Tk_CreateEventHandler(gwmPtr->tkwin, StructureNotifyMask |
	    ExposureMask | PropertyChangeMask,
	    GwmEventProc, (ClientData) gwmPtr);

/*
 *  Create the command for this widget.
 */
    gwmPtr->widgetCmd = Tcl_CreateCommand(interp, Tk_PathName(gwmPtr->tkwin),
                                          (Tcl_CmdProc *)GwmWidgetCmd,
                                          (ClientData) gwmPtr,
                                          (void (*)()) NULL);

    Tcl_SetResult(interp, Tk_PathName(gwmPtr->tkwin), TCL_VOLATILE);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * GwmWidgetCmd --
 *
 *	This procedure is invoked to process the Tcl command
 *	that corresponds to a widget managed by this module.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */

static int
GwmWidgetCmd(clientData, interp, argc, argv)
    ClientData clientData;		/* Information about gwm widget. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    Gwm *gwmPtr = (Gwm *) clientData;
    int result = TCL_OK;
    int length;
    char c;
    int ctentry;
    XColor color;
    int i;
    int cx, cy;
    int x, y;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " option ?arg arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }
    Tcl_Preserve((ClientData) gwmPtr);
    c = argv[1][0];
    length = strlen(argv[1]);

/*
 * "cget" command
 */
    if ((c == 'c') && (strncmp(argv[1], "cget", length) == 0)
            && (length >= 2)) {
        if (argc != 3) {
            Tcl_AppendResult(interp, "wrong # args: should be \"",
                    argv[0], " cget option\"",
                    (char *) NULL);
            goto error;
        }
        result = Tk_ConfigureValue(interp, gwmPtr->tkwin, configSpecs,
                (char *) gwmPtr, argv[2], 0);

/*
 * "configure" command
 */
    } else if ((c == 'c') && (strncmp(argv[1], "configure", length) == 0)) {
	if (argc == 2) {
	    result = Tk_ConfigureInfo(interp, gwmPtr->tkwin, configSpecs,
		    (char *) gwmPtr, (char *) NULL, 0);
	} else if (argc == 3) {
	    result = Tk_ConfigureInfo(interp, gwmPtr->tkwin, configSpecs,
		    (char *) gwmPtr, argv[2], 0);
	} else {
	    result = GwmConfigure(interp, gwmPtr, argc-2, argv+2,
		    TK_CONFIG_ARGV_ONLY);
	}

/*
 * "clear" command
 */
    } else if ((c == 'c') && (strncmp(argv[1], "clear", length) == 0)) {
	if (argc > 2 ) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " clear\"", (char *) NULL);
	    goto error;
	}
	Clear(gwmPtr);

/*
 * "get" command
 */
    } else if ((c == 'g') && (strncmp(argv[1], "get", length) == 0)) {
	if (argc != 4 ) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " get option arg\"", (char *) NULL);
	    goto error;
	}
    	c = argv[2][0];
    	length = strlen(argv[2]);

/*
 *  "get colour"
 */
    	if ((c == 'c') && (strncmp(argv[2], "colour", length) == 0)) {
	    if (Tcl_GetInt(interp, argv[3], &ctentry) != TCL_OK) {
		Tcl_AppendResult(interp,
			"invalid colour table entry specification \"", argv[3],
			"\"", (char *) NULL);
		goto error;
	    }
	    if ( ctentry < -1 || ctentry >= (int)gwmPtr->info->ctsize ) {
		Tcl_AppendResult(interp, "colour table entry \"",  argv[3],
			"\" out of range", (char *) NULL);
		goto error;
	    }
	    if ( ctentry >= 0 ) {
	    	color.pixel = gwmPtr->info->ctable[ctentry];
	    } else {
		if (gwmPtr->overlay) {
		    color.pixel = gwmPtr->info->ctable[0] | ~gwmPtr->info->mask;
		} else {
		    Tcl_AppendResult(interp, "widget \"", argv[0],
			"\" does not have an overlay", (char *) NULL);
	    	    goto error;
		}
	    }
	    XQueryColor( gwmPtr->display, gwmPtr->info->cmap, &color);
	    Tcl_AppendResult(interp, Tk_NameOfColor(&color), (char *) NULL);

/*
 *  Unrecognised get option
 */
	} else {
	    Tcl_AppendResult(interp, "bad option \"", argv[2],
		    "\":  must be get colour", (char *) NULL);
	    goto error;
	}

/*
 * "input" command
 */
    } else if ((c == 'i') && (strncmp(argv[1], "input", length) == 0)) {
        if (argc != 5 ) {
            Tcl_AppendResult(interp, "wrong # args: should be \"",
                    argv[0], " keypress x y key\"", (char *) NULL);
            goto error;
        }
        if (Tcl_GetInt(interp, argv[2], &x) != TCL_OK) {
            Tcl_AppendResult(interp, "invalid x postion\"", argv[2],
                "\"", (char *) NULL);
            goto error;
        }
        if (Tcl_GetInt(interp, argv[3], &y) != TCL_OK) {
            Tcl_AppendResult(interp, "invalid y postion\"", argv[2],
                "\"", (char *) NULL);
            goto error;
        }
        if ( SendKeypress(interp, gwmPtr, x, y, argv[4]) != TCL_OK)
            goto error;

/*
 * "ovclear" command
 */
    } else if ((c == 'o') && (strncmp(argv[1], "ovclear", length) == 0)) {
	if (argc > 2 ) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " ovclear\"", (char *) NULL);
	    goto error;
	}
	OvClear(gwmPtr);

/*
 * "print" command
 */
    } else if ((c == 'p') && (strncmp(argv[1], "print", length) == 0)) {
	if (argc != 3 ) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " print arg\"", (char *) NULL);
	    goto error;
	}
	if (tkgwmStartPrint(interp, gwmPtr, argv[2]) != TCL_OK) goto error;

    /*
     * Return without freeing the widget data structure. This will be done
     * when printing is complete.
     */
	return result;
/*
 * "set" command
 */
    } else if ((c == 's') && (strncmp(argv[1], "set", length) == 0)) {
	if (argc != 5 ) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " set option arg arg\"", (char *) NULL);
	    goto error;
	}
    	c = argv[2][0];
    	length = strlen(argv[2]);

/*
 *  "set colour"
 */
    	if ((c == 'c') && (strncmp(argv[2], "colour", length) == 0)) {
	    if (Tcl_GetInt(interp, argv[3], &ctentry) != TCL_OK) {
		Tcl_AppendResult(interp,
			"invalid colour table entry specification \"", argv[3],
			"\"", (char *) NULL);
		goto error;
	    }
	    if ( gwmPtr->info->visclass != PseudoColor &&
		gwmPtr->info->visclass != DirectColor &&
		gwmPtr->info->visclass != GrayScale ) {
		Tcl_AppendResult(interp, "colour table is read only",
			(char *) NULL);
		goto error;
	    }
	    if ( ctentry < -1 || ctentry >= (int)gwmPtr->info->ctsize ) {
		Tcl_AppendResult(interp, "colour table entry \"",  argv[3],
			"\" out of range", (char *) NULL);
		goto error;
	    }
	    if ( !XParseColor( gwmPtr->display, gwmPtr->info->cmap,
		    argv[4], &color) ) {
		Tcl_AppendResult(interp, "invalid colour \"",  argv[4], "\"",
			(char *) NULL);
		goto error;
	    }
	    color.pixel = gwmPtr->info->ctable[ctentry];
	    if (ctentry >= 0) {
	    	color.flags =  DoRed | DoGreen | DoBlue;
	    	XStoreColor(gwmPtr->display, gwmPtr->info->cmap,
		    &color);
	    } else {
		if (gwmPtr->overlay) {
	    	    for ( i = 0; i < gwmPtr->info->ctsize; i++ ) {
		    	color.pixel = (gwmPtr->info->ctable)[i] |
			    ~gwmPtr->info->mask;
		        XStoreColor( gwmPtr->display, gwmPtr->info->cmap,
			    &color);
		    }
		} else {
		    Tcl_AppendResult(interp, "widget \"", argv[0],
			"\" does not have an overlay", (char *) NULL);
		    goto error;
	    	}
	    }
	} else if ((c == 'c') && (strncmp(argv[2], "crosshair", length) == 0)) {
/*
 *  "set crosshair"
 */
	    if (Tk_GetPixels(interp, gwmPtr->tkwin, argv[3], &cx) != TCL_OK) {
		Tcl_AppendResult(interp,
			" invalid crosshair position specification \"", argv[3],
			"\"", (char *) NULL);
		goto error;
	    }
	    if (Tk_GetPixels(interp, gwmPtr->tkwin, argv[4], &cy) != TCL_OK) {
		Tcl_AppendResult(interp,
			" invalid crosshair position specification \"", argv[4],
			"\"", (char *) NULL);
		goto error;
	    }
	    if (gwmPtr->crosshair) {
		DrawCursor (gwmPtr, cx, cy);
	    }
	    gwmPtr->crossx = cx;
	    gwmPtr->crossy = cy;
/*
 *  Unrecognised set option
 */
	} else {
	    Tcl_AppendResult(interp, "bad option \"", argv[2],
		    "\":  must be set colour or set crosshair", (char *) NULL);
	    goto error;
	}

/*
 * Command not recognised
 */
    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1],
		"\":  must be configure, clear, get, ovclear, print or set",
		(char *) NULL);
	goto error;
    }
    Tcl_Release((ClientData) gwmPtr);
    XFlush( gwmPtr->display);
    return result;

    error:
    Tcl_Release((ClientData) gwmPtr);
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * GwmConfigure --
 *
 *	This procedure is called to process an argv/argc list in
 *	conjunction with the Tk option database to configure (or
 *	reconfigure) a gwm widget.
 *
 * Results:
 *	The return value is a standard Tcl result.  If TCL_ERROR is
 *	returned, then interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information, such as colors, border width,
 *	etc. get set for gwPtr;  old resources get freed,
 *	if there were any.
 *
 *----------------------------------------------------------------------
 */

static int
GwmConfigure(interp, gwmPtr, argc, argv, flags)
    Tcl_Interp *interp;			/* Used for error reporting. */
    Gwm *gwmPtr;			/* Information about widget. */
    int argc;				/* Number of valid entries in argv. */
    char **argv;			/* Arguments. */
    int flags;				/* Flags to pass to
					 * Tk_ConfigureWidget. */
{
    Atom atom;
    Tk_ConfigSpec *p;
    XColor color;
    XEvent event;
    const char *colname;
    int i;
    int status;

    if (Tk_ConfigureWidget(interp, gwmPtr->tkwin, configSpecs,
                           argc, (const char **)argv,
                           (char *) gwmPtr, flags) != TCL_OK) {
	return TCL_ERROR;
    }

    /* Set config flags (TK_CONFIG_OPTION_SPECIFIED not supported in Tk8.5) */
    for ( p = configSpecs; p->type != TK_CONFIG_END; p++ ) {

        /*  Default is cleared. */
        p->specFlags &= ~TK_CONFIG_OPTION_SPECIFIED;

        for ( i = 0; i < argc; i +=2 ) {
            if ( strcmp( argv[i], p->argvName ) == 0 ) {
                /*  Option in list, so set. */
                p->specFlags |= TK_CONFIG_OPTION_SPECIFIED;
                break;
            }
        }
    }

    if (Tk_WindowId(gwmPtr->tkwin)) {

	if (configSpecs[1].specFlags & TK_CONFIG_OPTION_SPECIFIED) {
	    color.red = gwmPtr->fg->red;
	    color.green = gwmPtr->fg->green;
	    color.blue = gwmPtr->fg->blue;
	    color.pixel = gwmPtr->info->ctable[1];
	    color.flags =  DoRed | DoGreen | DoBlue;

	    colname = Tk_NameOfColor(&color);
	    atom = XInternAtom( gwmPtr->display, "GWM_foreground", False );
	    XChangeProperty( gwmPtr->display, gwmPtr->info->win_id, atom,
        	XA_STRING, 8, PropModeReplace,
		(unsigned char*)colname, strlen(colname) );

	    if (gwmPtr->info->visclass == PseudoColor ||
		gwmPtr->info->visclass == DirectColor ||
		gwmPtr->info->visclass == GrayScale ) {
		XStoreColor(gwmPtr->display, gwmPtr->info->cmap, &color);
	    }
	}

	if (configSpecs[0].specFlags & TK_CONFIG_OPTION_SPECIFIED) {
	    color.red = gwmPtr->bg->red;
	    color.green = gwmPtr->bg->green;
	    color.blue = gwmPtr->bg->blue;
	    color.pixel = gwmPtr->info->ctable[0];
	    color.flags =  DoRed | DoGreen | DoBlue;

	    colname = Tk_NameOfColor(&color);
	    atom = XInternAtom( gwmPtr->display, "GWM_background", False );
	    XChangeProperty( gwmPtr->display, gwmPtr->info->win_id, atom,
        	XA_STRING, 8, PropModeReplace, (unsigned char*)colname,
		strlen(colname) );

	    if (gwmPtr->info->visclass == PseudoColor ||
		gwmPtr->info->visclass == DirectColor ||
		gwmPtr->info->visclass == GrayScale ) {
		XStoreColor(gwmPtr->display, gwmPtr->info->cmap, &color);
	    }
	}
	if (gwmPtr->info->visclass == PseudoColor ||
	    gwmPtr->info->visclass == DirectColor ||
	    gwmPtr->info->visclass == GrayScale ) {
	    if (gwmPtr->overlay) {
		if (configSpecs[2].specFlags & TK_CONFIG_OPTION_SPECIFIED) {
        	    color.red = gwmPtr->ovcolour->red;
       		    color.green = gwmPtr->ovcolour->green;
        	    color.blue = gwmPtr->ovcolour->blue;
		    color.flags =  DoRed | DoGreen | DoBlue;
		    for ( i = 0; i < gwmPtr->info->ctsize; i++ ) {
        		color.pixel = gwmPtr->info->ctable[i] |
			    ~gwmPtr->info->mask;
        		XStoreColor( gwmPtr->display, gwmPtr->info->cmap,
			    &color);
		    }
		}
	    }
	}

    /*
     *  Set the offset properties if any of them have changed
     */
	if (configSpecs[3].specFlags & TK_CONFIG_OPTION_SPECIFIED ||
		configSpecs[4].specFlags & TK_CONFIG_OPTION_SPECIFIED ||
		configSpecs[5].specFlags & TK_CONFIG_OPTION_SPECIFIED ||
		configSpecs[6].specFlags & TK_CONFIG_OPTION_SPECIFIED ) {
	    status = GWM_SetScroll( gwmPtr->display, Tk_WindowId(gwmPtr->tkwin),
		    gwmPtr->xoffset, gwmPtr->yoffset);
	    status = GWM_SetOvScroll( gwmPtr->display,
		    Tk_WindowId(gwmPtr->tkwin), gwmPtr->xovoffset,
		    gwmPtr->yovoffset);

	     gwmPtr->info->x_offset = gwmPtr->xoffset;
	     gwmPtr->info->y_offset = gwmPtr->yoffset;
	     gwmPtr->info->x_ov_offset = gwmPtr->xovoffset;
	     gwmPtr->info->y_ov_offset = gwmPtr->yovoffset;

	/*
	 *  Build a dummy event structure an refresh the window
	 */
	    event.type = Expose;
	    event.xexpose.x = 0;
	    event.xexpose.y = 0;
	    event.xexpose.width = gwmPtr->width;
	    event.xexpose.height = gwmPtr->height;
	    status = GWM_ProcessEvent( gwmPtr->info, &event);

	/*
	 * Redraw the crosshair.
	 */
	    if (gwmPtr->crosshair) {
		DrawCursor( gwmPtr, gwmPtr->crossx, gwmPtr->crossy);
	    }
	}

    /*
     * Redraw the crosshair if its state has changed.
     */
	if (configSpecs[7].specFlags & TK_CONFIG_OPTION_SPECIFIED ||
		configSpecs[8].specFlags & TK_CONFIG_OPTION_SPECIFIED ) {
	    if (gwmPtr->crosshair) {
		DrawCursor( gwmPtr, gwmPtr->crossx, gwmPtr->crossy);
	    } else {

		event.type = Expose;
		event.xexpose.x = 0;
		event.xexpose.y = gwmPtr->crossy;
		event.xexpose.width = gwmPtr->info->pix_width;
		event.xexpose.height = 1;
		status = GWM_ProcessEvent( gwmPtr->info, &event);

		event.type = Expose;
		event.xexpose.x = gwmPtr->crossx;
		event.xexpose.y = 0;
		event.xexpose.width = 1;
		event.xexpose.height = gwmPtr->info->pix_height;
		status = GWM_ProcessEvent( gwmPtr->info, &event);
	    }
	}
    }
    /*
     * Register the desired geometry for the window.  Then arrange for
     * the window to be redisplayed.
     */

    Tk_GeometryRequest(gwmPtr->tkwin, gwmPtr->width, gwmPtr->height);
    Tcl_DoWhenIdle(GwmDisplay, (ClientData) gwmPtr);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * GwmEventProc --
 *
 *	This procedure is invoked by the Tk dispatcher for various
 *	events on gwm widgets.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	When the window gets deleted, internal structures get
 *	cleaned up.  When it gets exposed, it is redisplayed.
 *
 *--------------------------------------------------------------
 */

static void
GwmEventProc(clientData, eventPtr)
    ClientData clientData;	/* Information about window. */
    XEvent *eventPtr;		/* Information about event. */
{
    int status;
    Window root;
    int x, y;
    unsigned int bw, depth;
    Gwm *gwmPtr = (Gwm *) clientData;

    if (eventPtr->type == MapNotify) {
	XGetGeometry( gwmPtr->display, Tk_WindowId(gwmPtr->tkwin),
	    &root, &x, &y, &(gwmPtr->info->win_width),
	    &(gwmPtr->info->win_height), &bw, &depth);
    }

    status = GWM_ProcessEvent( gwmPtr->info, eventPtr);

    if (eventPtr->type == Expose) {
	if (gwmPtr->crosshair) DrawCursor( gwmPtr, gwmPtr->crossx,
		gwmPtr->crossy);
    }

    if (eventPtr->type == DestroyNotify) {
	if (gwmPtr->tkwin != NULL)
	{
	    gwmPtr->tkwin = NULL;
	    Tcl_DeleteCommand(gwmPtr->interp,
		Tcl_GetCommandName(gwmPtr->interp, gwmPtr->widgetCmd));
	    Tcl_EventuallyFree((ClientData) gwmPtr, GwmDestroy);
	}
    }
}

/*
 *--------------------------------------------------------------
 *
 * GwmDisplay --
 *
 *	This procedure redraws the contents of a gwm window.
 *	It is invoked as a do-when-idle handler, so it only runs
 *	when there's nothing else for the application to do.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Information appears on the screen.
 *
 *--------------------------------------------------------------
 */

static void
GwmDisplay(clientData)
    ClientData clientData;	/* Information about window. */
{
#if 0
    Gwm *gwmPtr = (Gwm *) clientData;
    Tk_Window tkwin = gwmPtr->tkwin;

    if (!Tk_IsMapped(tkwin)) {
	return;
    }
#endif
}
/*
 *----------------------------------------------------------------------
 *
 * GwmDestroy --
 *
 *	This procedure is invoked by Tcl_EventuallyFree or Tcl_Release
 *	to clean up the internal structure of a gwm at a safe time
 *	(when no-one is using it anymore).
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Everything associated with the gwm is freed up.
 *
 *----------------------------------------------------------------------
 */

static void
GwmDestroy(clientData)
    char* clientData;	/* Info about square widget. */
{
    Gwm *gwmPtr = (Gwm *) clientData;

/*
 * Free the colour table entries allocated by gwm
 */
    if (gwmPtr->info->visclass == PseudoColor ||
	gwmPtr->info->visclass == DirectColor ||
	gwmPtr->info->visclass == GrayScale ) {
    	XFreeColors( gwmPtr->display, gwmPtr->info->cmap,
	    gwmPtr->info->ctable, gwmPtr->info->ctsize, 0);
    }

/*
 * Free the GC.
 */
    Tk_FreeXId(gwmPtr->display,  XGContextFromGC(gwmPtr->info->gc));
    XFreeGC(gwmPtr->display, gwmPtr->info->gc);

/*
 * Release the pixmap id (the pixmap itself is deleted by the DestroyNotify
 * event handler.
 */
    Tk_FreeXId(gwmPtr->display,  gwmPtr->info->pix_id);

/*
 * Free the visual info structure.
 */
    XFree( gwmPtr->vinfo );

/*
 * Deallocate the info structure
 */
    free(gwmPtr->info);

/*
 *  Free options
 */
    Tk_FreeOptions(configSpecs, (char *) gwmPtr, gwmPtr->display, 0);
    ckfree((char *) gwmPtr);
}
/*
 *----------------------------------------------------------------------
 *
 * Clear --
 *
 *	This procedure clears the gwm window. It is called by the
 *      widget clear command
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The Gwm window is cleared
 *
 *----------------------------------------------------------------------
 */

static void Clear(Gwm *gwmPtr)
{
    GC gc;                                        /* graphics context        */
    XGCValues gcval;                              /* graphics context values */


/*
**  Create a graphics context with the foreground set to the background
**  colour of the window (entry 0 in the colour table array) and the plane
**  mask set to protect the overlay plane.
*/
    gcval.foreground = gwmPtr->info->ctable[0];
    gcval.plane_mask = gwmPtr->info->mask;
    gc = Tk_GetGC( gwmPtr->tkwin, GCForeground | GCPlaneMask, &gcval);

/*
**  Erase the contents of the pixmap
*/
    XFillRectangle( gwmPtr->display, gwmPtr->info->pix_id, gc, 0, 0,
	gwmPtr->info->pix_width, gwmPtr->info->pix_height);

/*
**  Erase the area of the window occupied by the pixmap
*/
    XFillRectangle( gwmPtr->display, gwmPtr->info->win_id, gc,
	gwmPtr->info->x_offset, gwmPtr->info->y_offset,
	gwmPtr->info->pix_width, gwmPtr->info->pix_height);

/*
**  Delete the graphics context
*/
    Tk_FreeGC( gwmPtr->display, gc );

    if (gwmPtr->crosshair) DrawCursor( gwmPtr, gwmPtr->crossx,
	gwmPtr->crossy);
}

/*
 *----------------------------------------------------------------------
 *
 * OvClear --
 *
 *	This procedure clears the gwm window overlay plane. It is
 *      called by the widget ovclear command
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The Gwm overlay is cleared
 *
 *----------------------------------------------------------------------
 */

static void OvClear(Gwm *gwmPtr)
{
    GC gc;                                        /* graphics context        */
    XGCValues gcval;                              /* graphics context values */

/*
**  Create a graphics context with the foreground set to 0 and the plane
**  mask set to protect all planes except the overlay plane.
*/
    gcval.foreground = 0;
    gcval.plane_mask = ~gwmPtr->info->mask;
    gc = Tk_GetGC( gwmPtr->tkwin, GCForeground | GCPlaneMask, &gcval);

/*
**  Erase the contents of the pixmap
*/
    XFillRectangle( gwmPtr->display, gwmPtr->info->pix_id, gc, 0, 0,
	gwmPtr->info->pix_width, gwmPtr->info->pix_height);

/*
**  Erase the area of the window occupied by the pixmap
*/

    XFillRectangle( gwmPtr->display, gwmPtr->info->win_id, gc,
	gwmPtr->info->x_ov_offset, gwmPtr->info->y_ov_offset,
	gwmPtr->info->pix_width, gwmPtr->info->pix_height);

/*
**  Delete the graphics context
*/
    Tk_FreeGC( gwmPtr->display, gc );

    if (gwmPtr->crosshair) DrawCursor( gwmPtr, gwmPtr->crossx,
	gwmPtr->crossy);
}

static void DrawCursor(Gwm *gwmPtr, int cx, int cy)
{
    GC gc;
    XGCValues gcvalues;
    XEvent event;
    int status;

    gcvalues.foreground = gwmPtr->crosscolour->pixel;
    gc = Tk_GetGC( gwmPtr->tkwin, GCForeground, &gcvalues);

/*
 *  Erase old crosshair with pixmap contents
 */
    event.type = Expose;
    event.xexpose.x = 0;
    event.xexpose.y = gwmPtr->crossy;
    event.xexpose.width = gwmPtr->info->pix_width;
    event.xexpose.height = 1;
    status = GWM_ProcessEvent( gwmPtr->info, &event);

    event.type = Expose;
    event.xexpose.x = gwmPtr->crossx;
    event.xexpose.y = 0;
    event.xexpose.width = 1;
    event.xexpose.height = gwmPtr->info->pix_height;
    status = GWM_ProcessEvent( gwmPtr->info, &event);

/*
 * Draw the cross hair at the new position
 */
    XDrawLine( gwmPtr->display, Tk_WindowId(gwmPtr->tkwin), gc,
	0, cy, gwmPtr->info->pix_width, cy);
    XDrawLine( gwmPtr->display, Tk_WindowId(gwmPtr->tkwin), gc,
	cx, 0, cx, gwmPtr->info->pix_height);

    Tk_FreeGC( gwmPtr->display, gc);
}

static int SendKeypress(Tcl_Interp* interp, Gwm* gwmPtr, int x, int y,
    char *arg)
{
    XEvent event;
    Status status;
    Window inwin;
    Window root, parent;
    Window* children;
    unsigned int nchildren;
    int i;
    XWindowAttributes attrib;

/*
 * Find the window id of the input only child window of the gwm window
 */
    status = XQueryTree( gwmPtr->display, gwmPtr->info->win_id, &root,
        &parent, &children, &nchildren);

    if (status == 0) {
        Tcl_AppendResult(interp, "error from XQueryTree\n", (char *) NULL);
        return TCL_ERROR;
    }

/*
 * Find the input only window
 */
    for ( i = 0; i < nchildren; i++) {
        XGetWindowAttributes( gwmPtr->display, children[i], &attrib);
        if ( attrib.class == InputOnly ) {
            inwin = children[i];
            break;
        }
    }
    XFree( children );
    if ( i == nchildren ) return TCL_OK;  /* no input windows */

    event.type = KeyPress;
    event.xany.display = gwmPtr->display;
    event.xany.window = inwin;
    event.xkey.x = x;
    event.xkey.y = y;
    event.xkey.state = 0;
    event.xkey.keycode = XKeysymToKeycode( gwmPtr->display,
        XStringToKeysym(arg));

    status = XSendEvent( gwmPtr->display, inwin, False, KeyPressMask,
        &event);
    if (status == 0) {
        Tcl_AppendResult(interp, "error from XSendEvent\n", (char *) NULL);
        return TCL_ERROR;
    }

    return TCL_OK;
}
