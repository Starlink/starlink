/*
 * bltContainer.c --
 *
 *	This module implements a container widget for the BLT toolkit.
 *
 * Copyright 1998 Lucent Technologies, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that the copyright notice and warranty
 * disclaimer appear in supporting documentation, and that the names
 * of Lucent Technologies or any of their entities not be used in
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
 *	Container widget created by George A. Howlett
 */

#include "bltInt.h"

#ifndef NO_CONTAINER

#include "bltList.h"
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#ifndef WIN32
#include <X11/Xproto.h>
#endif
#include <ctype.h>

#define CONTAINER_REDRAW		(1<<1)
#define CONTAINER_MAPPED		(1<<2)
#define CONTAINER_FOCUS			(1<<4)

#define DEF_CONTAINER_BG_MONO		STD_MONO_NORMAL_BG
#define DEF_CONTAINER_BG_COLOR		STD_COLOR_NORMAL_BG
#define DEF_CONTAINER_BORDER_WIDTH	STD_BORDERWIDTH
#define DEF_CONTAINER_COMMAND		(char *)NULL
#define DEF_CONTAINER_CURSOR		(char *)NULL
#define DEF_CONTAINER_HEIGHT		"0"
#define DEF_CONTAINER_HIGHLIGHT_BG_COLOR	STD_COLOR_NORMAL_BG
#define DEF_CONTAINER_HIGHLIGHT_BG_MONO	STD_MONO_NORMAL_BG
#define DEF_CONTAINER_HIGHLIGHT_COLOR	RGB_COLOR_BLACK
#define DEF_CONTAINER_HIGHLIGHT_WIDTH	"2"
#define DEF_CONTAINER_RELIEF		"sunken"
#define DEF_CONTAINER_TAKE_FOCUS	"0"
#define DEF_CONTAINER_WIDTH		"0"
#define DEF_CONTAINER_WINDOW		(char *)NULL

#if (TK_MAJOR_VERSION < 8)
#define TK_REPARENTED			0x2000
#endif

typedef struct Container {
    Tk_Window tkwin;		/* Window that embodies the widget.
                                 * NULL means that the window has been
                                 * destroyed but the data structures
                                 * haven't yet been cleaned up.*/

    Display *display;		/* Display containing widget; needed,
                                 * among other things, to release
                                 * resources after tkwin has already
                                 * gone away. */

    Tcl_Interp *interp;		/* Interpreter associated with widget. */

    Tcl_Command cmdToken;	/* Token for widget's command. */

    unsigned int flags;		/* For bit-field definitions, see above. */

    int inset;			/* Total width of borders; traversal
				 * highlight and 3-D border. Indicates the
				 * offset from outside edges to leave room
				 * for borders. */

    Tk_Cursor cursor;		/* X Cursor */

    Tk_3DBorder border;		/* 3D border surrounding the embedded
				 * window. */
    int borderWidth;		/* Width of 3D border. */
    int relief;			/* 3D border relief. */

    /*
     * Focus highlight ring
     */
    int highlightWidth;		/* Width in pixels of highlight to draw
				 * around widget when it has the focus.
				 * <= 0 means don't draw a highlight. */
    XColor *highlightBgColor;	/* Color for drawing traversal highlight
				 * area when highlight is off. */
    XColor *highlightColor;	/* Color for drawing traversal highlight. */

    GC highlightGC;		/* GC for focus highlight. */

    char *takeFocus;		/* Says whether to select this widget during
				 * tab traveral operations.  This value isn't
				 * used in C code, but for the widget's Tcl
				 * bindings. */

    int reqWidth, reqHeight;	/* Requested dimensions of the container
				 * window. */

    Window windowId;		/* X window Id of embedded window contained
				 * by the widget.  If None, no window is
				 * embedded. */
    XSizeHints hints;
    int x, y, width, height;
} Container;

static int StringToXId _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *string, char *widgRec,
	int offset));
static char *XIdToString _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtrPtr));

static Tk_CustomOption xIdOption =
{
    StringToXId, XIdToString, (ClientData)0,
};

extern Tk_CustomOption bltLengthOption;

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_CONTAINER_BG_MONO, Tk_Offset(Container, border),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_CONTAINER_BG_COLOR, Tk_Offset(Container, border),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_SYNONYM, "-bd", "borderWidth", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_SYNONYM, "-bg", "background", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_ACTIVE_CURSOR, "-cursor", "cursor", "Cursor",
	DEF_CONTAINER_CURSOR, Tk_Offset(Container, cursor), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-borderwidth", "borderWidth", "BorderWidth",
	DEF_CONTAINER_BORDER_WIDTH, Tk_Offset(Container, borderWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-height", "height", "Height",
	DEF_CONTAINER_HEIGHT, Tk_Offset(Container, reqHeight),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_COLOR, "-highlightbackground", "highlightBackground",
	"HighlightBackground",
	DEF_CONTAINER_HIGHLIGHT_BG_COLOR, Tk_Offset(Container, highlightBgColor),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-highlightbackground", "highlightBackground",
	"HighlightBackground",
	DEF_CONTAINER_HIGHLIGHT_BG_MONO, Tk_Offset(Container, highlightBgColor),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_COLOR, "-highlightcolor", "highlightColor", "HighlightColor",
	DEF_CONTAINER_HIGHLIGHT_COLOR, Tk_Offset(Container, highlightColor), 0},
    {TK_CONFIG_PIXELS, "-highlightthickness", "highlightThickness",
	"HighlightThickness",
	DEF_CONTAINER_HIGHLIGHT_WIDTH, Tk_Offset(Container, highlightWidth),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_RELIEF, "-relief", "relief", "Relief",
	DEF_CONTAINER_RELIEF, Tk_Offset(Container, relief), 0},
    {TK_CONFIG_STRING, "-takefocus", "takeFocus", "TakeFocus",
	DEF_CONTAINER_TAKE_FOCUS, Tk_Offset(Container, takeFocus),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-width", "width", "Width",
	DEF_CONTAINER_WIDTH, Tk_Offset(Container, reqWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-window", "window", "Window",
	DEF_CONTAINER_WINDOW, Tk_Offset(Container, windowId),
	TK_CONFIG_DONT_SET_DEFAULT, &xIdOption},
    {TK_CONFIG_END, (char *)NULL, (char *)NULL, (char *)NULL,
	(char *)NULL, 0, 0}
};

/* Forward Declarations */
static void DestroyContainer _ANSI_ARGS_((DestroyData dataPtr));
static void ContainerEventProc _ANSI_ARGS_((ClientData clientdata,
	XEvent *eventPtr));
static void DisplayContainer _ANSI_ARGS_((ClientData clientData));
static void ContainerInstDeletedCmd _ANSI_ARGS_((ClientData clientdata));
static int ContainerInstCmd _ANSI_ARGS_((ClientData clientdata,
	Tcl_Interp *interp, int argc, char **argv));
static void EventuallyRedraw _ANSI_ARGS_((Container * contPtr));

/* ARGSUSED */
static int
XGeometryErrorProc(clientData, errEventPtr)
    ClientData clientData;
    XErrorEvent *errEventPtr;
{
    int *errorPtr = (int *)clientData;

    *errorPtr = TCL_ERROR;
    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * StringToXId --
 *
 *	Converts a string into an X window Id.
 *
 * Results:
 *	If the string is successfully converted, TCL_OK is returned.
 *	Otherwise, TCL_ERROR is returned and an error message is left
 *	in interpreter's result field.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToXId(clientData, interp, parent, string, widgRec, offset)
    ClientData clientData;	/* Not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window parent;		/* Parent window */
    char *string;		/* String representation. */
    char *widgRec;		/* Widget record */
    int offset;			/* Offset to field in structure */
{
    Container *contPtr = (Container *) widgRec;
    Window *winPtr = (Window *) (widgRec + offset);
    Window windowId;
    long mask;


    if (string[0] == '.') {
	Tk_Window tkwin;

	tkwin = Tk_NameToWindow(interp, string, Tk_MainWindow(interp));
	if (tkwin == NULL) {
	    return TCL_ERROR;
	}
	windowId = Blt_WindowId(tkwin);
    } else if ((string[0] == '0') && (string[1] == 'x')) {
	int newId;

	if (Tcl_GetInt(interp, string, &newId) != TCL_OK) {
	    return TCL_ERROR;
	}
	windowId = newId;
    } else if ((string == NULL) || (string[0] == '\0')) {
	windowId = None;
    } else {
	Tcl_AppendResult(interp, "bad window id \"", string, "\"",
	    (char *)NULL);
	return TCL_ERROR;
    }

    if (windowId != None) {
	int x, y, width, height, borderWidth, depth;
	Window root;
	Tk_ErrorHandler handler;
	int result;
	int any = -1;

	handler = Tk_CreateErrorHandler(contPtr->display, any, X_GetGeometry,
	    any, XGeometryErrorProc, (ClientData)&result);
	result = XGetGeometry(contPtr->display, windowId, &root, &x, &y,
	    (unsigned int *)&width, (unsigned int *)&height,
	    (unsigned int *)&borderWidth, (unsigned int *)&depth);
	Tk_DeleteErrorHandler(handler);
	XSync(contPtr->display, False);
	if (result == 0) {
	    Tcl_AppendResult(interp, "can't get window geometry for \"",
		string, "\"", (char *)NULL);
	    return TCL_ERROR;
	}
	XSetWindowBorderWidth(contPtr->display, windowId, 0);

	result = XGetWMNormalHints(contPtr->display, windowId,
	    &(contPtr->hints), &mask);
	if (result == 0) {
	    Tcl_AppendResult(interp, "can't get window hints for \"",
		string, "\"", (char *)NULL);
	    return TCL_ERROR;
	}
	if (contPtr->hints.width > 0) {
	    width = contPtr->hints.width;
	}
	if (contPtr->hints.height > 0) {
	    width = contPtr->hints.height;
	}
	/* Set the requested width and height on behalf of the
	 * embedded window. */
	contPtr->x = x;
	contPtr->y = y;
	contPtr->width = width;
	contPtr->height = height;

	if (contPtr->reqWidth > 0) {
	    width = contPtr->reqWidth;
	} else {
	    width += 2 * contPtr->inset;
	}
	if (contPtr->reqHeight > 0) {
	    height = contPtr->reqHeight;
	} else {
	    height += 2 * contPtr->inset;
	}
	if ((Tk_ReqWidth(contPtr->tkwin) != width) ||
	    (Tk_ReqHeight(contPtr->tkwin) != height)) {
	    Tk_GeometryRequest(contPtr->tkwin, width, height);
	}
    }
    *winPtr = windowId;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * XIdToString --
 *
 *	Converts the Tk window back to its string representation (i.e.
 *	its name).
 *
 * Results:
 *	The name of the window is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
XIdToString(clientData, parent, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* Not used */
    Tk_Window parent;		/* Not used */
    char *widgRec;		/* Widget record */
    int offset;			/* Offset of field in record */
    Tcl_FreeProc **freeProcPtr;	/* Memory deallocation scheme to use */
{
    Container *contPtr = (Container *) widgRec;
    Window windowId = *(Window *) (widgRec + offset);
    Tk_Window tkwin, mainWindow;
    static char string[20];

    if (windowId == None) {
	return "";
    }
    mainWindow = Tk_MainWindow(contPtr->interp);
    tkwin = Tk_IdToWindow(Tk_Display(mainWindow), windowId);
    if (tkwin != NULL) {
	return Tk_PathName(tkwin);
    }
    sprintf(string, "0x%x", (unsigned int)windowId);
    return string;
}

/*
 *----------------------------------------------------------------------
 *
 * EventuallyRedraw --
 *
 *	Queues a request to redraw the widget at the next idle point.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Information gets redisplayed.  Right now we don't do selective
 *	redisplays:  the whole window will be redrawn.
 *
 *----------------------------------------------------------------------
 */
static void
EventuallyRedraw(contPtr)
    Container *contPtr;
{
    if ((contPtr->tkwin != NULL) && !(contPtr->flags & CONTAINER_REDRAW)) {
	contPtr->flags |= CONTAINER_REDRAW;
	Tk_DoWhenIdle(DisplayContainer, (ClientData)contPtr);
    }
}

/*
 * --------------------------------------------------------------
 *
 * EmbeddedEventProc --
 *
 * 	This procedure is invoked by the Tk dispatcher for various
 * 	events on the encapsulated window.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	When the window gets deleted, internal structures get
 *	cleaned up.  When it gets resized or exposed, it's redisplayed.
 *
 * --------------------------------------------------------------
 */
static int
EmbeddedEventProc(clientData, eventPtr)
    ClientData clientData;	/* Information about the tab window. */
    XEvent *eventPtr;		/* Information about event. */
{
    Container *contPtr = (Container *) clientData;

    if ((contPtr->windowId == None) ||
	(eventPtr->xany.window != contPtr->windowId)) {
	return 0;
    }
    if ((eventPtr->type == DestroyNotify) ||
	(eventPtr->type == ReparentNotify)) {
	contPtr->windowId = None;
	EventuallyRedraw(contPtr);
	Tk_DeleteGenericHandler(EmbeddedEventProc, clientData);
    }
    return 1;
}

/*
 * --------------------------------------------------------------
 *
 * ContainerEventProc --
 *
 * 	This procedure is invoked by the Tk dispatcher for various
 * 	events on container widgets.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	When the window gets deleted, internal structures get
 *	cleaned up.  When it gets exposed, it is redisplayed.
 *
 * --------------------------------------------------------------
 */
static void
ContainerEventProc(clientData, eventPtr)
    ClientData clientData;	/* Information about window. */
    XEvent *eventPtr;		/* Information about event. */
{
    Container *contPtr = (Container *) clientData;

    switch (eventPtr->type) {
    case Expose:
	if (eventPtr->xexpose.count == 0) {
	    EventuallyRedraw(contPtr);
	}
	break;

    case FocusIn:
    case FocusOut:
	if (eventPtr->xfocus.detail != NotifyInferior) {
	    if (eventPtr->type == FocusIn) {
		contPtr->flags |= CONTAINER_FOCUS;
	    } else {
		contPtr->flags &= ~CONTAINER_FOCUS;
	    }
	    EventuallyRedraw(contPtr);
	}
	break;

    case ConfigureNotify:
	EventuallyRedraw(contPtr);
	break;

    case DestroyNotify:
	if (contPtr->tkwin != NULL) {
	    char *cmdName;

	    cmdName = Tcl_GetCommandName(contPtr->interp, contPtr->cmdToken);
#ifdef ITCL_NAMESPACES
	    Itk_SetWidgetCommand(contPtr->tkwin, (Tcl_Command) NULL);
#endif /* ITCL_NAMESPACES */
	    Tcl_DeleteCommand(contPtr->interp, cmdName);
	    contPtr->tkwin = NULL;
	}
	if (contPtr->flags & CONTAINER_REDRAW) {
	    Tk_CancelIdleCall(DisplayContainer, (ClientData)contPtr);
	}
	Tk_EventuallyFree((ClientData)contPtr, DestroyContainer);
	break;
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * DestroyContainer --
 *
 * 	This procedure is invoked by Tk_EventuallyFree or Tk_Release
 * 	to clean up the internal structure of the widget at a safe
 * 	time (when no-one is using it anymore).
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Everything associated with the widget is freed up.
 *
 * ----------------------------------------------------------------------
 */
static void
DestroyContainer(dataPtr)
    DestroyData dataPtr;	/* Pointer to the widget record. */
{
    Container *contPtr = (Container *) dataPtr;

    if (contPtr->highlightGC != NULL) {
	Tk_FreeGC(contPtr->display, contPtr->highlightGC);
    }
    if (contPtr->windowId != None) {
	Tk_DeleteGenericHandler(EmbeddedEventProc, (ClientData)contPtr);
    }
    Tk_FreeOptions(configSpecs, (char *)contPtr, contPtr->display, 0);
    free((char *)contPtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * ConfigureContainer --
 *
 * 	This procedure is called to process an argv/argc list, plus
 *	the Tk option database, in order to configure (or reconfigure)
 *	the widget.
 *
 * Results:
 *	The return value is a standard Tcl result.  If TCL_ERROR is
 * 	returned, then interp->result contains an error message.
 *
 * Side Effects:
 *	Configuration information, such as text string, colors, font,
 *	etc. get set for contPtr; old resources get freed, if there
 *	were any.  The widget is redisplayed.
 *
 * ----------------------------------------------------------------------
 */
static int
ConfigureContainer(interp, contPtr, argc, argv, flags)
    Tcl_Interp *interp;		/* Interpreter to report errors. */
    Container *contPtr;		/* Information about widget; may or
			         * may not already have values for
			         * some fields. */
    int argc;
    char **argv;
    int flags;
{
    XGCValues gcValues;
    unsigned long gcMask;
    GC newGC;
    Window old;

    old = contPtr->windowId;
    if (Tk_ConfigureWidget(interp, contPtr->tkwin, configSpecs, argc, argv,
	    (char *)contPtr, flags) != TCL_OK) {
	return TCL_ERROR;
    }
    if ((contPtr->reqHeight > 0) && (contPtr->reqWidth > 0)) {
	Tk_GeometryRequest(contPtr->tkwin, contPtr->reqWidth,
	    contPtr->reqHeight);
    }
    contPtr->inset = contPtr->borderWidth + contPtr->highlightWidth;
    if (Blt_ConfigModified(configSpecs, "-window", (char *)NULL)) {

	if (old != None) {
	    XUnmapWindow(contPtr->display, old);
	}
	contPtr->flags &= ~CONTAINER_MAPPED;
	if (contPtr->windowId != None) {
	    if (Tk_WindowId(contPtr->tkwin) == None) {
		Tk_MakeWindowExist(contPtr->tkwin);
	    }
	    if (Blt_ReparentWindow(contPtr->display, contPtr->windowId,
		    Tk_WindowId(contPtr->tkwin), contPtr->inset,
		    contPtr->inset) != TCL_OK) {
		Tcl_AppendResult(interp, "can't reparent window", (char *)NULL);
		return TCL_ERROR;
	    }
	    XSelectInput(contPtr->display, contPtr->windowId,
		StructureNotifyMask);
	    Tk_CreateGenericHandler(EmbeddedEventProc, (ClientData)contPtr);
	}
    }
    /*
     * GC for focus highlight.
     */
    gcMask = GCForeground;
    gcValues.foreground = contPtr->highlightColor->pixel;
    newGC = Tk_GetGC(contPtr->tkwin, gcMask, &gcValues);
    if (contPtr->highlightGC != NULL) {
	Tk_FreeGC(contPtr->display, contPtr->highlightGC);
    }
    contPtr->highlightGC = newGC;

    EventuallyRedraw(contPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ContainerInstDeletedCmd --
 *
 *	This procedure can be called if the window was destroyed
 *	(tkwin will be NULL) and the command was deleted
 *	automatically.  In this case, we need to do nothing.
 *
 *	Otherwise this routine was called because the command was
 *	deleted.  Then we need to clean-up and destroy the widget.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	The widget is destroyed.
 *
 *----------------------------------------------------------------------
 */
static void
ContainerInstDeletedCmd(clientData)
    ClientData clientData;	/* Pointer to widget record for widget. */
{
    Container *contPtr = (Container *) clientData;

    if (contPtr->tkwin != NULL) {
	Tk_Window tkwin;

	tkwin = contPtr->tkwin;
	contPtr->tkwin = NULL;
	Tk_DestroyWindow(tkwin);
#ifdef ITCL_NAMESPACES
	Itk_SetWidgetCommand(tkwin, (Tcl_Command) NULL);
#endif /* ITCL_NAMESPACES */
    }
}

/*
 * ------------------------------------------------------------------------
 *
 * ContainerCmd --
 *
 * 	This procedure is invoked to process the Tcl command that
 * 	corresponds to a widget managed by this module. See the user
 * 	documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side Effects:
 *	See the user documentation.
 *
 * -----------------------------------------------------------------------
 */
/* ARGSUSED */
static int
ContainerCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window associated with interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    Container *contPtr;
    Tk_Window tkwin;
    unsigned int mask;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " pathName ?option value?...\"", (char *)NULL);
	return TCL_ERROR;
    }
    tkwin = (Tk_Window)clientData;
    tkwin = Tk_CreateWindowFromPath(interp, tkwin, argv[1], (char *)NULL);
    if (tkwin == NULL) {
	return TCL_ERROR;
    }
    contPtr = (Container *) calloc(1, sizeof(Container));
    assert(contPtr);
    Tk_SetClass(tkwin, "Container");
    contPtr->tkwin = tkwin;
    contPtr->display = Tk_Display(tkwin);
    contPtr->interp = interp;
    contPtr->flags = 0;
    contPtr->borderWidth = contPtr->highlightWidth = 2;
    contPtr->relief = TK_RELIEF_SUNKEN;

    if (ConfigureContainer(interp, contPtr, argc - 2, argv + 2, 0) != TCL_OK) {
	Tk_DestroyWindow(contPtr->tkwin);
	return TCL_ERROR;
    }
    mask = (StructureNotifyMask | ExposureMask | FocusChangeMask);
    Tk_CreateEventHandler(tkwin, mask, ContainerEventProc, (ClientData)contPtr);
    contPtr->cmdToken = Tcl_CreateCommand(interp, argv[1], ContainerInstCmd,
	(ClientData)contPtr, ContainerInstDeletedCmd);
#ifdef ITCL_NAMESPACES
    Itk_SetWidgetCommand(contPtr->tkwin, contPtr->cmdToken);
#endif
    Tk_MakeWindowExist(tkwin);
    Tcl_SetResult(interp, Tk_PathName(contPtr->tkwin), TCL_STATIC);
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * DisplayContainer --
 *
 * 	This procedure is invoked to display the widget.
 *
 * Results:
 *	None.
 *
 * Side effects:
 * 	The widget is redisplayed.
 *
 * ----------------------------------------------------------------------
 */
static void
DisplayContainer(clientData)
    ClientData clientData;	/* Information about widget. */
{
    Container *contPtr = (Container *) clientData;
    Drawable drawable;
    int width, height;

    contPtr->flags &= ~CONTAINER_REDRAW;
    if (contPtr->tkwin == NULL) {
	return;			/* Window has been destroyed. */
    }
    if (!Tk_IsMapped(contPtr->tkwin)) {
	return;
    }
    drawable = Tk_WindowId(contPtr->tkwin);
    if (contPtr->windowId != None) {
	width = Tk_Width(contPtr->tkwin) - (2 * contPtr->inset);
	height = Tk_Height(contPtr->tkwin) - (2 * contPtr->inset);
	if ((contPtr->x != contPtr->inset) ||
	    (contPtr->y != contPtr->inset) ||
	    (contPtr->width != width) ||
	    (contPtr->height != height)) {
	    XMoveResizeWindow(contPtr->display, contPtr->windowId,
		contPtr->inset, contPtr->inset, width, height);
	    contPtr->width = width, contPtr->height = height;
	    contPtr->x = contPtr->y = contPtr->inset;
	}
	if (!(contPtr->flags & CONTAINER_MAPPED)) {
	    XMapWindow(contPtr->display, contPtr->windowId);
	    contPtr->flags |= CONTAINER_MAPPED;
	}
	if (contPtr->borderWidth > 0) {
	    Tk_Draw3DRectangle(contPtr->tkwin, drawable, contPtr->border,
		contPtr->highlightWidth, contPtr->highlightWidth,
		Tk_Width(contPtr->tkwin) - 2 * contPtr->highlightWidth,
		Tk_Height(contPtr->tkwin) - 2 * contPtr->highlightWidth,
		contPtr->borderWidth, contPtr->relief);
	}
    } else {
	Tk_Fill3DRectangle(contPtr->tkwin, drawable, contPtr->border,
	    contPtr->highlightWidth, contPtr->highlightWidth,
	    Tk_Width(contPtr->tkwin) - 2 * contPtr->highlightWidth,
	    Tk_Height(contPtr->tkwin) - 2 * contPtr->highlightWidth,
	    contPtr->borderWidth, contPtr->relief);
    }

    /* Draw focus highlight ring. */
    if (contPtr->highlightWidth > 0) {
	XColor *color;
	GC gc;

	color = (contPtr->flags & CONTAINER_FOCUS)
	    ? contPtr->highlightColor : contPtr->highlightBgColor;
	gc = Tk_GCForColor(color, drawable);
	Tk_DrawFocusHighlight(contPtr->tkwin, gc, contPtr->highlightWidth,
	    drawable);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * CgetOp --
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
CgetOp(contPtr, interp, argc, argv)
    Container *contPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    return Tk_ConfigureValue(interp, contPtr->tkwin, configSpecs,
	(char *)contPtr, argv[2], 0);
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureOp --
 *
 * 	This procedure is called to process an argv/argc list, plus
 *	the Tk option database, in order to configure (or reconfigure)
 *	the widget.
 *
 * Results:
 *	A standard Tcl result.  If TCL_ERROR is returned, then
 *	interp->result contains an error message.
 *
 * Side Effects:
 *	Configuration information, such as text string, colors, font,
 *	etc. get set for contPtr; old resources get freed, if there
 *	were any.  The widget is redisplayed.
 *
 *----------------------------------------------------------------------
 */
static int
ConfigureOp(contPtr, interp, argc, argv)
    Container *contPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    if (argc == 2) {
	return Tk_ConfigureInfo(interp, contPtr->tkwin, configSpecs,
	    (char *)contPtr, (char *)NULL, 0);
    } else if (argc == 3) {
	return Tk_ConfigureInfo(interp, contPtr->tkwin, configSpecs,
	    (char *)contPtr, argv[2], 0);
    }
    if (ConfigureContainer(interp, contPtr, argc - 2, argv + 2,
	    TK_CONFIG_ARGV_ONLY) != TCL_OK) {
	return TCL_ERROR;
    }
    EventuallyRedraw(contPtr);
    return TCL_OK;
}

/*
 * --------------------------------------------------------------
 *
 * ContainerCmd --
 *
 * 	This procedure is invoked to process the "container" command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 * --------------------------------------------------------------
 */
static Blt_OpSpec operSpecs[] =
{
    {"cget", 2, (Blt_Operation)CgetOp, 3, 3, "option",},
    {"configure", 2, (Blt_Operation)ConfigureOp, 2, 0, "?option value?...",},
};

static int numSpecs = sizeof(operSpecs) / sizeof(Blt_OpSpec);

static int
ContainerInstCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Information about the widget. */
    Tcl_Interp *interp;		/* Interpreter to report errors back to. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Vector of argument strings. */
{
    Blt_Operation proc;
    Container *contPtr = (Container *) clientData;
    int result;

    proc = Blt_GetOperation(interp, numSpecs, operSpecs,
	BLT_OPER_ARG1, argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    Tk_Preserve((ClientData)contPtr);
    result = (*proc) (contPtr, interp, argc, argv);
    Tk_Release((ClientData)contPtr);
    return result;
}

int
Blt_ContainerInit(interp)
    Tcl_Interp *interp;
{
    static Blt_CmdSpec cmdSpec =
    {
	"container", ContainerCmd,
    };
    if (Blt_InitCmd(interp, "blt", &cmdSpec) == NULL) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

#endif /* NO_CONTAINER */
