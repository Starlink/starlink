/*
 *  bltDnd.c --
 *
 *	This module implements a drag-and-drop mechanism for the Tk
 *	Toolkit.  Allows widgets to be registered as drag&drop sources
 *	and targets for handling "drag-and-drop" operations between
 *	Tcl/Tk applications.
 *
 * Copyright 1993-1998 Lucent Technologies, Inc.
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
 * The "drag&drop" command was created by Michael J. McLennan.
 */
#include "bltInt.h"

#ifndef NO_DRAGDROP

#include <X11/Xatom.h>

#if HAVE_NAMESPACES
static char dragDropCmd[] = "blt::drag&drop";
#else
static char dragDropCmd[] = "drag&drop";
#endif

static char className[] = "DragDropToken";	/* CLASS NAME of token window */
static char propName[] = "DragDropInfo";	/* Property name */

static Tcl_HashTable sourceTable;
static Tcl_HashTable targetTable;
static char *errorCmd;
static int numActive;
static int locX, locY;
static int initialized = FALSE;

#ifdef WIN32
typedef HWND WindowToken;
typedef char *GlobalAtom;
#else
typedef Window WindowToken;
typedef Atom GlobalAtom;
#endif

static GlobalAtom dndAtom;

/* Error Proc used to report drag&drop background errors */
#define DEF_ERROR_PROC              "bgerror"

/*
 *  Maximum size property that can be read at one time:
 */
#define MAX_PROP_SIZE 1000

/*
 *  CONFIG PARAMETERS
 */

#define DEF_DND_BUTTON_BG_COLOR		RGB_COLOR_YELLOW
#define DEF_DND_BUTTON_BG_MONO		STD_MONO_NORMAL_BG
#define DEF_DND_BUTTON_NUMBER		"3"
#define DEF_DND_PACKAGE_COMMAND		(char *)NULL
#define DEF_DND_REJECT_BG_COLOR		STD_COLOR_NORMAL_BG
#define DEF_DND_REJECT_BG_MONO		RGB_COLOR_WHITE
#define DEF_DND_REJECT_FG_COLOR		RGB_COLOR_RED
#define DEF_DND_REJECT_FG_MONO		RGB_COLOR_BLACK
#define DEF_DND_REJECT_STIPPLE_COLOR	(char *)NULL
#define DEF_DND_REJECT_STIPPLE_MONO	RGB_COLOR_GREY50
#define DEF_DND_SELF_TARGET		"no"
#define DEF_DND_SEND			"all"
#define DEF_DND_SITE_COMMAND		(char *)NULL
#define DEF_DND_TOKEN_ACTIVE_BG_COLOR	STD_COLOR_ACTIVE_BG
#define DEF_DND_TOKEN_ACTIVE_BG_MONO	STD_MONO_ACTIVE_BG
#define DEF_DND_TOKEN_ACTIVE_BORDERWIDTH	"3"
#define DEF_DND_TOKEN_ACTIVE_RELIEF	"sunken"
#define DEF_DND_TOKEN_ANCHOR		"se"
#define DEF_DND_TOKEN_BG_COLOR		STD_COLOR_NORMAL_BG
#define DEF_DND_TOKEN_BG_MONO		STD_MONO_NORMAL_BG
#define DEF_DND_TOKEN_BORDERWIDTH	"3"
#define DEF_DND_TOKEN_CURSOR		"top_left_arrow"
#define DEF_DND_TOKEN_OUTLINE_COLOR	RGB_COLOR_BLACK
#define DEF_DND_TOKEN_OUTLINE_MONO	RGB_COLOR_BLACK
#define DEF_DND_TOKEN_RELIEF		"raised"

/*
 *  DRAG&DROP ROOT WINDOW HIERARCHY (cached during "drag" operations)
 */
typedef struct TreeNode {
    WindowToken windowToken;	/* Token for window */

    int initialized;		/* non-zero => rest of info is valid */

    struct TreeNode *parentPtr;	/* Node containing this as a child */

    int x0, y0, x1, y1;		/* Extents (upper-left and lower-right
				 * corners) of the window. */

    char *dndProp;		/* drag&drop property info */

    Tk_Uid interpName;		/* interp name within dndProp */

    char *dataType;		/* Data type of currently matching some
				 * source with this target. */

    char *targetName;

    Blt_List *dataTypes;	/* List of handlers parsed from the drag&drop
				 * property.  The ordering is important since
				 * the first matching data type is selected.
				 * If NULL, no handlers are defined. */

    Blt_List *children;		/* List of this window's children. If NULL,
				 * there are no children. */
} TreeNode;

/*
 *  DRAG&DROP REGISTRATION DATA
 */
typedef struct Token {
    /*
     * This is a goof in the Tk API.  It assumes that only a "toplevel"
     * widget will become a TopLevel window (i.e. a window whose parent
     * is the root window).  Under Win32, Tk tries to use the widget
     * record associated with the TopLevel as a Tk frame widget, to
     * read its menu name.  What this means is that any widget that's
     * going to be a TopLevel, must also look like a frame. Therefore
     * we've copied the frame widget structure fields into the token.
     */
    Tk_Window tkwin;		/* Window that embodies the frame.  NULL
				 * means that the window has been destroyed
				 * but the data structures haven't yet been
				 * cleaned up. */
    Display *display;		/* Display containing widget.  Used, among
				 * other things, so that resources can be
				 * freed even after tkwin has gone away. */
    Tcl_Interp *interp;		/* Interpreter associated with widget.  Used
				 * to delete widget command. */
    Tcl_Command widgetCmd;	/* Token for frame's widget command. */
    char *className;		/* Class name for widget (from configuration
				 * option).  Malloc-ed. */
    int mask;			/* Either FRAME or TOPLEVEL;  used to select
				 * which configuration options are valid for
				 * widget. */
    char *screenName;		/* Screen on which widget is created.  Non-null
				 * only for top-levels.  Malloc-ed, may be
				 * NULL. */
    char *visualName;		/* Textual description of visual for window,
				 * from -visual option.  Malloc-ed, may be
				 * NULL. */
    char *colormapName;		/* Textual description of colormap for window,
				 * from -colormap option.  Malloc-ed, may be
				 * NULL. */
    char *menuName;		/* Textual description of menu to use for
				 * menubar. Malloc-ed, may be NULL. */
    Colormap colormap;		/* If not None, identifies a colormap
				 * allocated for this window, which must be
				 * freed when the window is deleted. */
    Tk_3DBorder border;		/* Structure used to draw 3-D border and
				 * background.  NULL means no background
				 * or border. */
    int borderWidth;		/* Width of 3-D border (if any). */
    int relief;			/* 3-d effect: TK_RELIEF_RAISED etc. */
    int highlightWidth;		/* Width in pixels of highlight to draw
				 * around widget when it has the focus.
				 * 0 means don't draw a highlight. */
    XColor *highlightBgColorPtr;
    /* Color for drawing traversal highlight
				 * area when highlight is off. */
    XColor *highlightColorPtr;	/* Color for drawing traversal highlight. */
    int width;			/* Width to request for window.  <= 0 means
				 * don't request any size. */
    int height;			/* Height to request for window.  <= 0 means
				 * don't request any size. */
    Tk_Cursor cursor;		/* Current cursor for window, or None. */
    char *takeFocus;		/* Value of -takefocus option;  not used in
				 * the C code, but used by keyboard traversal
				 * scripts.  Malloc'ed, but may be NULL. */
    int isContainer;		/* 1 means this window is a container, 0 means
				 * that it isn't. */
    char *useThis;		/* If the window is embedded, this points to
				 * the name of the window in which it is
				 * embedded (malloc'ed).  For non-embedded
				 * windows this is NULL. */
    int flags;			/* Various flags;  see below for
				 * definitions. */

    /* Token specific fields */

    int lastX, lastY;		/* last position of token window */
    int overTarget;		/* non-zero => over target window */
    Tk_TimerToken timer;	/* token for routine to hide tokenwin */
    GC rejectFgGC;		/* GC used to draw rejection fg: (\) */
    GC rejectBgGC;		/* GC used to draw rejection bg: (\) */

    /* User-configurable fields */

    Tk_Anchor anchor;		/* Position of token win relative to mouse */
    Tk_3DBorder outline;	/* Outline border around token window */
    Tk_3DBorder normalBorder;	/* Border/background for token window */
    Tk_3DBorder activeBorder;	/* Border/background for token window */
    int activeRelief;
    int activeBorderWidth;	/* Border width in pixels */
    XColor *rejectFg;		/* Color used to draw rejection fg: (\) */
    XColor *rejectBg;		/* Color used to draw rejection bg: (\) */
    Pixmap rejectStipple;	/* Stipple used to draw rejection: (\) */

} Token;

typedef struct {
    Tcl_Interp *interp;
    Tk_Window tkwin;		/* drag&drop source window */
    Display *display;		/* drag&drop source window display */
    Blt_List *listPtr;

    int button;			/* button used to invoke drag for sources */

    Token token;		/* Information about drag&drop token window */

    int pkgCmdInProgress;	/* non-zero => executing pkgCmd */
    char *pkgCmd;		/* cmd executed on start of drag op */
    char *pkgCmdResult;		/* result returned by recent pkgCmd */
    char *sitecmd;		/* cmd executed to update token win */

    TreeNode *rootPtr;		/* window info (used during "drag") */

    int selfTarget;		/* non-zero => source can drop onto itself */
    Tk_Cursor cursor;		/* cursor restored after dragging */

    char **sendTypes;		/* list of data handler names or "all" */

    Tcl_HashEntry *hashPtr;
} Source;

/*
 *  PERCENT SUBSTITUTIONS
 */
typedef struct SubstDescriptors {
    char letter;		/* character like 'x' in "%x" */
    char *value;		/* value to be substituted in place of "%x" */
} SubstDescriptors;

typedef struct {
    Tcl_Interp *interp;
    Tk_Window tkwin;		/* drag&drop target window */
    Display *display;		/* drag&drop target window display */
    Blt_List *listPtr;
    Tcl_HashEntry *hashPtr;
} Target;

/*
 * Each "drag&drop" widget window is tagged with a "DragDropInfo"
 * property in XA_STRING format.  This property identifies the
 * window as a "drag&drop" widget, and contains the following:
 *
 *     "<interp-name>]<drag&drop-path>]<handler-list>"
 *
 * The <drag&drop-path> is the window path name of the drag&drop
 * widget, <interp-name> is the name of the interpreter controlling
 * the widget (useful for the "send" command), and <handler-list>
 * is the list of handler types recognized by the widget.
 *
 * When the user invokes the "drag" operation, a snapshot of the
 * entire window hierarchy is made, and windows carrying a
 * "DragDropInfo" property are identified.  As the token window is
 * dragged around, * this snapshot can be queried to determine when
 * the token is over a valid target window.  When the token is
 * dropped over a valid site, the drop information is sent to the
 * application via the usual "send" command.  If communication fails,
 * the drag&drop facility automatically posts a rejection symbol on
 * the token window.
 */
extern Tk_CustomOption bltListOption;

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_INT, "-button", "buttonBinding", "ButtonBinding",
	DEF_DND_BUTTON_NUMBER, Tk_Offset(Source, button), 0},
    {TK_CONFIG_STRING, "-packagecmd", "packageCommand", "Command",
	DEF_DND_PACKAGE_COMMAND, Tk_Offset(Source, pkgCmd), TK_CONFIG_NULL_OK},
    {TK_CONFIG_COLOR, "-rejectbg", "rejectBackground", "Background",
	DEF_DND_REJECT_BG_COLOR, Tk_Offset(Source, token.rejectBg),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-rejectbg", "rejectBackground", "Background",
	DEF_DND_REJECT_BG_MONO, Tk_Offset(Source, token.rejectBg),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_COLOR, "-rejectfg", "rejectForeground", "Foreground",
	DEF_DND_REJECT_FG_COLOR, Tk_Offset(Source, token.rejectFg),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-rejectfg", "rejectForeground", "Foreground",
	DEF_DND_REJECT_BG_COLOR, Tk_Offset(Source, token.rejectFg),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_BITMAP, "-rejectstipple", "rejectStipple", "Stipple",
	DEF_DND_REJECT_STIPPLE_COLOR, Tk_Offset(Source, token.rejectStipple),
	TK_CONFIG_COLOR_ONLY | TK_CONFIG_NULL_OK},
    {TK_CONFIG_BITMAP, "-rejectstipple", "rejectStipple", "Stipple",
	DEF_DND_REJECT_STIPPLE_MONO, Tk_Offset(Source, token.rejectStipple),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_BOOLEAN, "-selftarget", "selfTarget", "SelfTarget",
	DEF_DND_SELF_TARGET, Tk_Offset(Source, selfTarget), 0},
    {TK_CONFIG_CUSTOM, "-send", "send", "Send",
	DEF_DND_SEND, Tk_Offset(Source, sendTypes), TK_CONFIG_NULL_OK,
	&bltListOption},
    {TK_CONFIG_STRING, "-sitecmd", "siteCommand", "Command",
	DEF_DND_SITE_COMMAND, Tk_Offset(Source, sitecmd), TK_CONFIG_NULL_OK},
    {TK_CONFIG_ANCHOR, "-tokenanchor", "tokenAnchor", "Anchor",
	DEF_DND_TOKEN_ANCHOR, Tk_Offset(Source, token.anchor), 0},
    {TK_CONFIG_BORDER, "-tokenactivebackground",
	"tokenActiveBackground", "ActiveBackground",
	DEF_DND_TOKEN_ACTIVE_BG_COLOR, Tk_Offset(Source, token.activeBorder),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_BORDER, "-tokenactivebackground",
	"tokenActiveBackground", "ActiveBackground",
	DEF_DND_TOKEN_ACTIVE_BG_MONO, Tk_Offset(Source, token.activeBorder),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_BORDER, "-tokenbg", "tokenBackground", "Background",
	DEF_DND_TOKEN_BG_COLOR, Tk_Offset(Source, token.normalBorder),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_BORDER, "-tokenbg", "tokenBackground", "Background",
	DEF_DND_TOKEN_BG_MONO, Tk_Offset(Source, token.normalBorder),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_BORDER, "-tokenoutline", "tokenOutline", "Outline",
	DEF_DND_TOKEN_OUTLINE_COLOR, Tk_Offset(Source, token.outline),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_BORDER, "-tokenoutline", "tokenOutline", "Outline",
	DEF_DND_TOKEN_OUTLINE_MONO, Tk_Offset(Source, token.outline),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_PIXELS, "-tokenborderwidth", "tokenBorderWidth", "BorderWidth",
	DEF_DND_TOKEN_BORDERWIDTH, Tk_Offset(Source, token.borderWidth), 0},
    {TK_CONFIG_CURSOR, "-tokencursor", "tokenCursor", "Cursor",
	DEF_DND_TOKEN_CURSOR, Tk_Offset(Source, token.cursor), TK_CONFIG_NULL_OK},
    {TK_CONFIG_END, (char *)NULL, (char *)NULL, (char *)NULL, (char *)NULL, 0, 0},
};

static Tk_ConfigSpec tokenConfigSpecs[] =
{
    {TK_CONFIG_BORDER, "-activebackground", "activeBackground",
	"ActiveBackground", DEF_DND_TOKEN_ACTIVE_BG_COLOR,
	Tk_Offset(Token, activeBorder), TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_BORDER, "-activebackground", "activeBackground",
	"ActiveBackground",
	DEF_DND_TOKEN_ACTIVE_BG_MONO, Tk_Offset(Token, activeBorder),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_RELIEF, "-activerelief", "activeRelief", "activeRelief",
	DEF_DND_TOKEN_ACTIVE_RELIEF, Tk_Offset(Token, activeRelief), 0},
    {TK_CONFIG_ANCHOR, "-anchor", "anchor", "Anchor",
	DEF_DND_TOKEN_ANCHOR, Tk_Offset(Token, anchor), 0},
    {TK_CONFIG_PIXELS, "-activeborderwidth", "activeBorderWidth",
	"ActiveBorderWidth",
	DEF_DND_TOKEN_ACTIVE_BORDERWIDTH, Tk_Offset(Token, activeBorderWidth), 0},
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_DND_TOKEN_BG_COLOR, Tk_Offset(Token, normalBorder),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_DND_TOKEN_BG_MONO, Tk_Offset(Token, normalBorder),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_PIXELS, "-borderwidth", "borderWidth", "BorderWidth",
	DEF_DND_TOKEN_BORDERWIDTH, Tk_Offset(Token, borderWidth), 0},
    {TK_CONFIG_CURSOR, "-cursor", "cursor", "Cursor",
	DEF_DND_TOKEN_CURSOR, Tk_Offset(Token, cursor), TK_CONFIG_NULL_OK},
    {TK_CONFIG_BORDER, "-outline", "outline", "Outline",
	DEF_DND_TOKEN_OUTLINE_COLOR, Tk_Offset(Token, outline),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_BORDER, "-outline", "outline", "Outline",
	DEF_DND_TOKEN_OUTLINE_MONO, Tk_Offset(Token, outline), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_COLOR, "-rejectbg", "rejectBackground", "Background",
	DEF_DND_REJECT_BG_COLOR, Tk_Offset(Token, rejectBg), TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-rejectbg", "rejectBackground", "Background",
	DEF_DND_REJECT_BG_MONO, Tk_Offset(Token, rejectBg), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_COLOR, "-rejectfg", "rejectForeground", "Foreground",
	DEF_DND_REJECT_FG_COLOR, Tk_Offset(Token, rejectFg), TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-rejectfg", "rejectForeground", "Foreground",
	DEF_DND_REJECT_BG_COLOR, Tk_Offset(Token, rejectFg), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_BITMAP, "-rejectstipple", "rejectStipple", "Stipple",
	DEF_DND_REJECT_STIPPLE_COLOR, Tk_Offset(Token, rejectStipple),
	TK_CONFIG_COLOR_ONLY | TK_CONFIG_NULL_OK},
    {TK_CONFIG_BITMAP, "-rejectstipple", "rejectStipple", "Stipple",
	DEF_DND_REJECT_STIPPLE_MONO, Tk_Offset(Token, rejectStipple),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_RELIEF, "-relief", "relief", "Relief",
	DEF_DND_TOKEN_RELIEF, Tk_Offset(Token, relief), 0},
    {TK_CONFIG_END, (char *)NULL, (char *)NULL, (char *)NULL, (char *)NULL, 0, 0},
};


/*
 *  FORWARD DECLARATIONS
 */
static void DeleteDndCmd _ANSI_ARGS_((ClientData clientData));
static int DndCmd _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
	int argc, char **argv));

static void TokenEventProc _ANSI_ARGS_((ClientData clientData, XEvent *eventPtr));
static void MoveToken _ANSI_ARGS_((Source * srcPtr, Token *tokenPtr));
static void UpdateToken _ANSI_ARGS_((ClientData clientData));
static void HideToken _ANSI_ARGS_((Token *tokenPtr));
static void RejectToken _ANSI_ARGS_((Token *tokenPtr));

static Source *FindSource _ANSI_ARGS_((char *pathname));
static Source *CreateSource _ANSI_ARGS_((Tcl_Interp *interp, char *pathname,
	int *newEntry));
static void DestroySource _ANSI_ARGS_((Source * srcPtr));
static void SourceEventProc _ANSI_ARGS_((ClientData clientData,
	XEvent *eventPtr));
static int ConfigureSource _ANSI_ARGS_((Tcl_Interp *interp, Source * srcPtr,
	int argc, char **argv, int flags));
static int ConfigureToken _ANSI_ARGS_((Tcl_Interp *interp, Source * srcPtr,
	int argc, char **argv));

static Target *CreateTarget _ANSI_ARGS_((Tk_Window tkwin, int *newEntry));
static void DestroyTarget _ANSI_ARGS_((Target * targetPtr));
static void TargetEventProc _ANSI_ARGS_((ClientData clientData,
	XEvent *eventPtr));
static TreeNode *GetTargetWindow _ANSI_ARGS_((Source * srcPtr, int x, int y));
static void AddTargetProperty _ANSI_ARGS_((Tcl_Interp *interp,
	Target * targetPtr));

static void DndSend _ANSI_ARGS_((Source * srcPtr));

static TreeNode *InitRoot _ANSI_ARGS_((Display *display));
static void DestroyTree _ANSI_ARGS_((TreeNode * wr));
static void QueryTree _ANSI_ARGS_((Display *display, TreeNode * treePtr));


static char *ExpandPercents _ANSI_ARGS_((char *str, SubstDescriptors * subs,
	int nsubs, Tcl_DString * dStrPtr));


#ifdef	WIN32

#if _MSC_VER
#include <tchar.h> 
#endif

typedef struct PropertyInfo {
    char *prefix;
    int prefixSize;
    char *propReturn;
} PropertyInfo;


static BOOL CALLBACK
GetEnumWindowsProc(HWND hWnd, LPARAM clientData)
{
    Blt_List *listPtr = (Blt_List *)clientData;

    Blt_ListAppend(listPtr, (char *)hWnd, (ClientData)0);
    return TRUE;
}

static WindowToken
GetWindowToken(tkwin)
    Tk_Window tkwin;
{
    return Tk_GetHWND(Tk_WindowId(tkwin));
}

/*
 * ------------------------------------------------------------------------
 *
 *  GetWindowStackOrder --
 *
 *	Returns a list of the child windows according to their stacking
 *	order.  The window handles are ordered from top to bottom.
 *
 * ------------------------------------------------------------------------
 */
static Blt_List *
GetWindowStackOrder(display, parent)
    Display *display;
    HWND parent;
{
    Blt_List *listPtr;
    HWND child;

    listPtr = Blt_CreateList(TCL_ONE_WORD_KEYS);
    for (child = GetTopWindow(parent); child != NULL;
	child = GetNextWindow(child, GW_HWNDNEXT)) {
	Blt_ListAppend(listPtr, (char *)child, (ClientData)0);
    }
    return listPtr;
}

/*
 * ------------------------------------------------------------------------
 *
 *  GetEnumPropsExProc --
 *
 * ------------------------------------------------------------------------
 */
static BOOL CALLBACK
GetEnumPropsExProc(HWND hwnd, LPCTSTR atom, HANDLE hData, DWORD clientData)
{
    PropertyInfo *infoPtr = (PropertyInfo *) clientData;

    if (strncmp(infoPtr->prefix, atom, infoPtr->prefixSize) == 0) {
	assert(infoPtr->propReturn == NULL);
	infoPtr->propReturn = (char *)atom;
	return FALSE;
    }
    return TRUE;
}

/*
 * ------------------------------------------------------------------------
 *
 *  GetPropData --
 *
 *	This is a bad Windows hack to pass property information between
 *	applications.  (Ab)Normally the property data (one-word value) is
 *	stored in the data handle.  But the data content is available only
 *	within the application.  The pointer value is meaningless outside
 *	of the current application address space.  Not really useful at all.
 *
 *	So the trick here is to encode the property name with all the
 *	necessary information and to loop through all the properties
 *	of a window, looking for one that starts with our property name
 *	prefix.  The downside is that the property name is limited to
 *	255 bytes.  But that should be enough.  It's also slower since
 *	we examine each property until we find ours.
 *
 *	We'll plug in the OLE stuff later.
 *
 * ------------------------------------------------------------------------
 */

static char *
GetPropData(hWnd, atom)
    HWND hWnd;
    char *atom;
{
    PropertyInfo propInfo;
    if (hWnd == NULL) {
	return NULL;
    }
    propInfo.prefix = atom;
    propInfo.prefixSize = strlen(atom);
    propInfo.propReturn = NULL;
    EnumPropsEx(hWnd, GetEnumPropsExProc, (DWORD) & propInfo);
    return propInfo.propReturn;
}


static char *
FindWindowProperty(display, hWnd, atom)
    Display *display;
    HWND hWnd;
    char *atom;
{
    char *data;

    data = GetPropData(hWnd, atom);
    if (data != NULL) {
	return strdup(data);
    }
    return NULL;
}

static void
ChangeWindowProperty(tkwin, atom, data)
    Tk_Window tkwin;
    char *atom;
    char *data;
{
    HWND hWnd;
    char *oldData;

    hWnd = Tk_GetHWND(Tk_WindowId(tkwin));
    if (hWnd == NULL) {
	return;
    }
    oldData = GetPropData(hWnd, atom);
    if (oldData != NULL) {
	RemoveProp(hWnd, oldData);
    }
    SetProp(hWnd, data, (HANDLE) 0);
}

static void
RemoveWindowProperty(tkwin, atom)
    Tk_Window tkwin;
    char *atom;
{
    HWND hWnd;
    char *oldData;

    hWnd = Tk_GetHWND(Tk_WindowId(tkwin));
    if (hWnd == NULL) {
	return;
    }
    oldData = GetPropData(hWnd, propName);
    if (oldData != NULL) {
	RemoveProp(hWnd, oldData);
    }
}

#else

static WindowToken
GetWindowToken(tkwin)
    Tk_Window tkwin;
{
    return Tk_WindowId(tkwin);
}

/*
 * ------------------------------------------------------------------------
 *
 *  GetWindowStackOrder --
 *
 *	Returns a list of the child windows according to their stacking
 *	order.  The window ids are ordered from top to bottom.
 *
 * ------------------------------------------------------------------------
 */
static Blt_List *
GetWindowStackOrder(display, window)
    Display *display;
    Window window;
{
    Blt_List *listPtr;
    Window *childArr;
    unsigned int numChildren;
    Window dummy;

    listPtr = NULL;
    if ((XQueryTree(display, window, &dummy, &dummy, &childArr,
		&numChildren)) && (numChildren > 0)) {
	register int i;

	listPtr = Blt_CreateList(TCL_ONE_WORD_KEYS);
	for (i = 0; i < numChildren; i++) {
	    /*
	     *  XQuery returns windows in bottom to top order.
	     *  We only care about the top window.
	     */
	    Blt_ListPrepend(listPtr, (char *)childArr[i], (ClientData)0);
	}
	if (childArr != NULL) {
	    XFree((char *)childArr);	/* done with list of kids */
	}
    }
    return listPtr;
}

char *
FindWindowProperty(display, window, atom)
    Display *display;
    Window window;
    Atom atom;
{
    char *dndProp;
    int result, actualFormat;
    Atom actualType;
    unsigned long numItems, bytesAfter;

    if (window == None) {
	return NULL;
    }
    dndProp = NULL;
    result = XGetWindowProperty(display, window, atom, 0, MAX_PROP_SIZE, False,
	XA_STRING, &actualType, &actualFormat, &numItems, &bytesAfter,
	(unsigned char **)&dndProp);
    if ((result != Success) || (actualFormat != 8) ||
	(actualType != XA_STRING)) {
	if (dndProp != NULL) {
	    XFree((char *)dndProp);
	    dndProp = NULL;
	}
    }
    return dndProp;
}

#endif /* WIN32 */

/*
 * ------------------------------------------------------------------------
 *
 *  ActivateToken --
 *
 * ------------------------------------------------------------------------
 */
static void
ActivateToken(tokenPtr, active)
    Token *tokenPtr;
    int active;
{
    int relief;
    Tk_3DBorder *borderPtr;
    int borderWidth;

    Tk_Fill3DRectangle(tokenPtr->tkwin, Tk_WindowId(tokenPtr->tkwin),
	tokenPtr->outline, 0, 0, Tk_Width(tokenPtr->tkwin),
	Tk_Height(tokenPtr->tkwin), 0, TK_RELIEF_FLAT);
    if (active) {
	relief = tokenPtr->activeRelief;
	borderPtr = &(tokenPtr->activeBorder);
	borderWidth = tokenPtr->activeBorderWidth;
    } else {
	relief = tokenPtr->relief;
	borderPtr = &(tokenPtr->normalBorder);
	borderWidth = tokenPtr->borderWidth;
    }
    Tk_Fill3DRectangle(tokenPtr->tkwin, Tk_WindowId(tokenPtr->tkwin),
	*borderPtr, 2, 2, Tk_Width(tokenPtr->tkwin) - 4,
	Tk_Height(tokenPtr->tkwin) - 4, borderWidth, relief);
}

/*
 * ------------------------------------------------------------------------
 *
 *  TokenEventProc --
 *
 *	Invoked by the Tk dispatcher to handle widget events.
 *	Manages redraws for the drag&drop token window.
 *
 * ------------------------------------------------------------------------
 */
static void
TokenEventProc(clientData, eventPtr)
    ClientData clientData;	/* data associated with widget */
    XEvent *eventPtr;		/* information about event */
{
    Token *tokenPtr = (Token *)clientData;

    if ((eventPtr->type == Expose) && (eventPtr->xexpose.count == 0)) {
	if (tokenPtr->tkwin != NULL) {
	    ActivateToken(tokenPtr, tokenPtr->overTarget);
	}
    } else if (eventPtr->type == DestroyNotify) {
	tokenPtr->tkwin = NULL;
    }
}

/*
 * ------------------------------------------------------------------------
 *
 *  HideToken --
 *
 *	Unmaps the drag&drop token.  Invoked directly at the end of a
 *	successful communication, or after a delay if the communication
 *	fails (allowing the user to see a graphical picture of failure).
 *
 * ------------------------------------------------------------------------
 */
static void
HideToken(tokenPtr)
    Token *tokenPtr;
{
    if (tokenPtr->tkwin != NULL) {
	Tk_UnmapWindow(tokenPtr->tkwin);
    }
    tokenPtr->timer = NULL;
}

/*
 * ------------------------------------------------------------------------
 *
 *  RaiseToken --
 *
 * ------------------------------------------------------------------------
 */
static void
RaiseToken(srcPtr, tokenPtr)
    Source *srcPtr;
    Token *tokenPtr;
{
    Blt_MapTopLevelWindow(tokenPtr->tkwin);
    Blt_RaiseTopLevelWindow(tokenPtr->tkwin);
}

/*
 * ------------------------------------------------------------------------
 *
 *  MoveToken --
 *
 *	Invoked during "drag" operations to move a token window to its
 *	current "drag" coordinate.
 *
 * ------------------------------------------------------------------------
 */
static void
MoveToken(srcPtr, tokenPtr)
    Source *srcPtr;		/* drag&drop source window data */
    Token *tokenPtr;
{
    int x, y;
    int maxWidth, maxHeight;
    int vx, vy, vw, vh;

    /*
     *  Adjust current location for virtual root windows.
     */
    Tk_GetVRootGeometry(srcPtr->tkwin, &vx, &vy, &vw, &vh);
    x = tokenPtr->lastX + vx;
    y = tokenPtr->lastY + vy;

    maxWidth = WidthOfScreen(Tk_Screen(srcPtr->tkwin)) -
	Tk_Width(tokenPtr->tkwin);
    maxHeight = HeightOfScreen(Tk_Screen(srcPtr->tkwin)) -
	Tk_Height(tokenPtr->tkwin);
    Blt_TranslateAnchor(x, y, Tk_Width(tokenPtr->tkwin),
	Tk_Height(tokenPtr->tkwin), tokenPtr->anchor, &x, &y);
    if (x > maxWidth) {
	x = maxWidth;
    } else if (x < 0) {
	x = 0;
    }
    if (y > maxHeight) {
	y = maxHeight;
    } else if (y < 0) {
	y = 0;
    }
    if ((x != Tk_X(tokenPtr->tkwin)) || (y != Tk_Y(tokenPtr->tkwin))) {
	Tk_MoveToplevelWindow(tokenPtr->tkwin, x, y);
    }
    RaiseToken(srcPtr, tokenPtr);
}

static Tk_Cursor
GetWidgetCursor(interp, tkwin)
    Tk_Window tkwin;
    Tcl_Interp *interp;
{
    char *cursorName;
    Tk_Cursor cursor;

    cursor = None;
    if (Tcl_VarEval(interp, Tk_PathName(tkwin), " cget -cursor",
	    (char *)NULL) != TCL_OK) {
	return None;
    }
    cursorName = Tcl_GetStringResult(interp);
    if ((cursorName != NULL) && (cursorName[0] != '\0')) {
	cursor = Tk_GetCursor(interp, tkwin, Tk_GetUid(cursorName));
    }
    Tcl_ResetResult(interp);
    return cursor;
}

/*
 * ------------------------------------------------------------------------
 *
 *  UpdateToken --
 *
 *	Invoked when the event loop is idle to determine whether or not
 *	the current drag&drop token position is over another drag&drop
 *	target.
 *
 * ------------------------------------------------------------------------
 */
static void
UpdateToken(clientData)
    ClientData clientData;	/* widget data */
{
    register Source *srcPtr = (Source *) clientData;
    Token *tokenPtr = &(srcPtr->token);
    int status, result;
    SubstDescriptors subst[2];

    status = FALSE;
    if (GetTargetWindow(srcPtr, tokenPtr->lastX, tokenPtr->lastY) != NULL) {
	status = TRUE;
    }
    if (tokenPtr->overTarget ^ status) {
	ActivateToken(tokenPtr, status);
	/*
         *  If the source has a site command, then invoke it to
         *  modify the appearance of the token window.  Pass any
         *  errors onto the drag&drop error handler.
         */
	if (srcPtr->sitecmd) {
	    char buffer[200];
	    Tcl_DString cmdStr;

	    sprintf(buffer, "%d", status);
	    subst[0].letter = 's';
	    subst[0].value = buffer;
	    subst[1].letter = 't';
	    subst[1].value = Tk_PathName(tokenPtr->tkwin);

	    Tcl_DStringInit(&cmdStr);
	    result = Tcl_Eval(srcPtr->interp,
		ExpandPercents(srcPtr->sitecmd, subst, 2, &cmdStr));
	    Tcl_DStringFree(&cmdStr);

	    if ((result != TCL_OK) && (errorCmd != NULL) && (*errorCmd)) {

		(void)Tcl_VarEval(srcPtr->interp, errorCmd, " {",
		    Tcl_GetStringResult(srcPtr->interp), "}",
		    (char *)NULL);
	    }
	}
    }
    tokenPtr->overTarget = status;
}

/*
 * ------------------------------------------------------------------------
 *
 *  RejectToken --
 *
 *	Draws a rejection mark on the current drag&drop token, and arranges
 *	for the token to be unmapped after a small delay.
 *
 * ------------------------------------------------------------------------
 */
static void
RejectToken(tokenPtr)
    Token *tokenPtr;
{
    int divisor = 6;		/* controls size of rejection symbol */
    int w, h, lineWidth, x, y, margin;

    margin = 2 * tokenPtr->borderWidth;
    w = Tk_Width(tokenPtr->tkwin) - 2 * margin;
    h = Tk_Height(tokenPtr->tkwin) - 2 * margin;
    lineWidth = (w < h) ? w / divisor : h / divisor;
    lineWidth = (lineWidth < 1) ? 1 : lineWidth;

    w = h = lineWidth * (divisor - 1);
    x = (Tk_Width(tokenPtr->tkwin) - w) / 2;
    y = (Tk_Height(tokenPtr->tkwin) - h) / 2;

    /*
     *  Draw the rejection symbol background (\) on the token window...
     */
    XSetLineAttributes(Tk_Display(tokenPtr->tkwin), tokenPtr->rejectBgGC,
	lineWidth + 4, LineSolid, CapButt, JoinBevel);

    XDrawArc(Tk_Display(tokenPtr->tkwin), Tk_WindowId(tokenPtr->tkwin),
	tokenPtr->rejectBgGC, x, y, w, h, 0, 23040);

    XDrawLine(Tk_Display(tokenPtr->tkwin), Tk_WindowId(tokenPtr->tkwin),
	tokenPtr->rejectBgGC, x + lineWidth, y + lineWidth, x + w - lineWidth,
	y + h - lineWidth);

    /*
     *  Draw the rejection symbol foreground (\) on the token window...
     */
    XSetLineAttributes(Tk_Display(tokenPtr->tkwin), tokenPtr->rejectFgGC,
	lineWidth, LineSolid, CapButt, JoinBevel);

    XDrawArc(Tk_Display(tokenPtr->tkwin), Tk_WindowId(tokenPtr->tkwin),
	tokenPtr->rejectFgGC, x, y, w, h, 0, 23040);

    XDrawLine(Tk_Display(tokenPtr->tkwin), Tk_WindowId(tokenPtr->tkwin),
	tokenPtr->rejectFgGC, x + lineWidth, y + lineWidth, x + w - lineWidth,
	y + h - lineWidth);

    /*
     *  Arrange for token window to disappear eventually.
     */
    tokenPtr->timer = Tk_CreateTimerHandler(1000, (Tcl_TimerProc *) HideToken,
	(ClientData)tokenPtr);
}

/*
 * ------------------------------------------------------------------------
 *
 *  ConfigureToken --
 *
 * ------------------------------------------------------------------------
 */
static int
ConfigureToken(interp, srcPtr, argc, argv)
    Tcl_Interp *interp;
    Source *srcPtr;
    int argc;
    char **argv;
{
    Token *tokenPtr;

    tokenPtr = &(srcPtr->token);
    if (Tk_ConfigureWidget(interp, srcPtr->tkwin, tokenConfigSpecs, argc, argv,
	    (char *)tokenPtr, TK_CONFIG_ARGV_ONLY) != TCL_OK) {
	return TCL_ERROR;
    }
    return ConfigureSource(interp, srcPtr, 0, (char **)NULL,
	TK_CONFIG_ARGV_ONLY);
}

/*
 * ------------------------------------------------------------------------
 *
 *  CreateToken --
 *
 * ------------------------------------------------------------------------
 */
static int
CreateToken(interp, srcPtr)
    Tcl_Interp *interp;
    Source *srcPtr;
{
    XSetWindowAttributes atts;
    Tk_Window tkwin;
    char tokenName[200];
    static int nextTokenId = 0;
    Token *tokenPtr = &(srcPtr->token);

    sprintf(tokenName, "dd-token%d", ++nextTokenId);
    tkwin = Tk_CreateWindow(interp, srcPtr->tkwin, tokenName, "");
    if (tkwin == NULL) {
	Tcl_AppendResult(interp, "can't token window \"", tokenName, "\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    Tk_SetClass(tkwin, className);
    Tk_CreateEventHandler(tkwin, ExposureMask | StructureNotifyMask,
	TokenEventProc, (ClientData)tokenPtr);

    atts.override_redirect = True;
    atts.save_under = True;
    Tk_ChangeWindowAttributes(tkwin, CWOverrideRedirect | CWSaveUnder, &atts);
    Tk_SetInternalBorder(tkwin, tokenPtr->borderWidth + 2);
    tokenPtr->tkwin = tkwin;
#ifdef WIN32
    {
	Tk_FakeWin *winPtr = (Tk_FakeWin *) tkwin;
	winPtr->dummy18 = (ClientData)tokenPtr;
    }
#endif /* WIN32 */
    Tk_MakeWindowExist(tkwin);
    if (srcPtr->button > 0) {
	Tcl_DString dString;
	int result;

	Tcl_DStringInit(&dString);
	Blt_DStringAppendElements(&dString, "blt::InitDrag&DropBindings",
	    Tk_PathName(srcPtr->tkwin), Blt_Int(srcPtr->button),
	    dragDropCmd, (char *)NULL);
	result = Tcl_Eval(interp, Tcl_DStringValue(&dString));
	Tcl_DStringFree(&dString);
	if (result != TCL_OK) {
	    Tk_DestroyWindow(tokenPtr->tkwin);
	    return TCL_ERROR;
	}
    }
    /*
     *  Arrange for the window to unregister itself when it
     *  is destroyed.
     */
    Tk_CreateEventHandler(srcPtr->tkwin, StructureNotifyMask, SourceEventProc,
	(ClientData)srcPtr);
    return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *
 *  CreateSource --
 *
 *	Looks for a Source record in the hash table for drag&drop source
 *	widgets.  Creates a new record if the widget name is not already
 *	registered.  Returns a pointer to the desired record.
 *
 * ------------------------------------------------------------------------
 */
static Source *
CreateSource(interp, pathName, newPtr)
    Tcl_Interp *interp;
    char *pathName;		/* widget pathname for desired record */
    int *newPtr;		/* returns non-zero => new record created */
{
    Tcl_HashEntry *hPtr;

    hPtr = Tcl_CreateHashEntry(&sourceTable, pathName, newPtr);
    if (*newPtr) {
	Source *srcPtr;
	Tk_Window tkwin;

	tkwin = Tk_NameToWindow(interp, pathName, Tk_MainWindow(interp));
	if (tkwin == NULL) {
	    Tcl_AppendResult(interp, "window does not exist: ", pathName,
		(char *)NULL);
	    return NULL;
	}
	srcPtr = (Source *) calloc(1, sizeof(Source));
	assert(srcPtr);
	srcPtr->tkwin = tkwin;
	srcPtr->display = Tk_Display(tkwin);
	srcPtr->interp = interp;
	srcPtr->token.anchor = TK_ANCHOR_SE;	/* Used to be "center". */
	srcPtr->token.relief = TK_RELIEF_RAISED;
	srcPtr->token.activeRelief = TK_RELIEF_SUNKEN;
	srcPtr->token.borderWidth = srcPtr->token.activeBorderWidth = 3;
	srcPtr->hashPtr = hPtr;
	if (ConfigureSource(interp, srcPtr, 0, (char **)NULL, 0) != TCL_OK) {
	    DestroySource(srcPtr);
	    return NULL;
	}
	Tcl_SetHashValue(hPtr, (ClientData)srcPtr);
    }
    return (Source *) Tcl_GetHashValue(hPtr);
}

/*
 * ------------------------------------------------------------------------
 *
 *  DestroySource --
 *
 *	Looks for a Source record in the hash table for drag&drop source
 *	widgets.  Destroys the record if found.
 *
 * ------------------------------------------------------------------------
 */
static void
DestroySource(srcPtr)
    Source *srcPtr;
{
    Tk_CancelIdleCall(UpdateToken, (ClientData)srcPtr);
    if (srcPtr->token.timer) {
	Tk_DeleteTimerHandler(srcPtr->token.timer);
    }
    Tk_FreeOptions(configSpecs, (char *)srcPtr, srcPtr->display, 0);

    if (srcPtr->token.rejectFgGC != NULL) {
	Tk_FreeGC(srcPtr->display, srcPtr->token.rejectFgGC);
    }
    if (srcPtr->token.rejectBgGC != NULL) {
	Tk_FreeGC(srcPtr->display, srcPtr->token.rejectBgGC);
    }
    if (srcPtr->pkgCmdResult) {
	free(srcPtr->pkgCmdResult);
    }
    if (srcPtr->rootPtr != NULL) {
	DestroyTree(srcPtr->rootPtr);
    }
    if (srcPtr->cursor != None) {
	Tk_FreeCursor(srcPtr->display, srcPtr->cursor);
    }
    if (srcPtr->token.cursor != None) {
	Tk_FreeCursor(srcPtr->display, srcPtr->token.cursor);
    }
    free((char *)srcPtr->sendTypes);
    if (srcPtr->listPtr != NULL) {
	Blt_ListItem item;
	char *cmd;

	for (item = Blt_ListFirstItem(srcPtr->listPtr); item != NULL;
	    item = Blt_ListNextItem(item)) {
	    cmd = (char *)Blt_ListGetValue(item);
	    if (cmd != NULL) {
		free(cmd);
	    }
	}
	Blt_ListDestroy(srcPtr->listPtr);
    }
    if (srcPtr->hashPtr != NULL) {
	Tcl_DeleteHashEntry(srcPtr->hashPtr);
    }
    free((char *)srcPtr);
}

/*
 * ------------------------------------------------------------------------
 *
 *  FindSource --
 *
 *	Looks for a Source record in the hash table for drag&drop source
 *	widgets.  Returns a pointer to the desired record.
 *
 * ------------------------------------------------------------------------
 */
static Source *
FindSource(pathname)
    char *pathname;		/* widget pathname for desired record */
{
    Tcl_HashEntry *hPtr;

    hPtr = Tcl_FindHashEntry(&sourceTable, pathname);
    if (hPtr == NULL) {
	return NULL;
    }
    return (Source *)Tcl_GetHashValue(hPtr);
}

/*
 * ------------------------------------------------------------------------
 *
 *  ConfigureSource --
 *
 *	Called to process an (argc,argv) list to configure (or
 *	reconfigure) a drag&drop source widget.
 *
 * ------------------------------------------------------------------------
 */
static int
ConfigureSource(interp, srcPtr, argc, argv, flags)
    Tcl_Interp *interp;		/* current interpreter */
    register Source *srcPtr;	/* drag&drop source widget record */
    int argc;			/* number of arguments */
    char **argv;		/* argument strings */
    int flags;			/* flags controlling interpretation */
{
    unsigned long gcMask;
    XGCValues gcValues;
    GC newGC;

    /*
     *  Handle the bulk of the options...
     */
    if (Tk_ConfigureWidget(interp, srcPtr->tkwin, configSpecs, argc, argv,
	    (char *)srcPtr, flags) != TCL_OK) {
	return TCL_ERROR;
    }
    /*
     *  Check the button binding for valid range (0 or 1-5)
     */
    if ((srcPtr->button < 0) || (srcPtr->button > 5)) {
	Tcl_SetResult(interp, "button number must be 1-5, or 0 for no bindings",
	    TCL_STATIC);
	return TCL_ERROR;
    }
    /*
     *  Set up the rejection foreground GC for the token window...
     */
    gcValues.foreground = srcPtr->token.rejectFg->pixel;
    gcValues.subwindow_mode = IncludeInferiors;
    gcValues.graphics_exposures = False;
    gcMask = GCForeground | GCSubwindowMode | GCGraphicsExposures;

    if (srcPtr->token.rejectStipple != None) {
	gcValues.stipple = srcPtr->token.rejectStipple;
	gcValues.fill_style = FillStippled;
	gcMask |= GCForeground | GCStipple | GCFillStyle;
    }
    newGC = Tk_GetGC(srcPtr->tkwin, gcMask, &gcValues);

    if (srcPtr->token.rejectFgGC != NULL) {
	Tk_FreeGC(srcPtr->display, srcPtr->token.rejectFgGC);
    }
    srcPtr->token.rejectFgGC = newGC;

    /*
     *  Set up the rejection background GC for the token window...
     */
    gcValues.foreground = srcPtr->token.rejectBg->pixel;
    gcValues.subwindow_mode = IncludeInferiors;
    gcValues.graphics_exposures = False;
    gcMask = GCForeground | GCSubwindowMode | GCGraphicsExposures;

    newGC = Tk_GetGC(srcPtr->tkwin, gcMask, &gcValues);

    if (srcPtr->token.rejectBgGC != NULL) {
	Tk_FreeGC(srcPtr->display, srcPtr->token.rejectBgGC);
    }
    srcPtr->token.rejectBgGC = newGC;

    /*
     *  Reset the border width in case it has changed...
     */
    if (srcPtr->token.tkwin) {
	Tk_SetInternalBorder(srcPtr->token.tkwin,
	    srcPtr->token.borderWidth + 2);
    }
    return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *
 *  SourceEventProc --
 *
 *	Invoked by Tk_HandleEvent whenever a DestroyNotify event is received
 *	on a registered drag&drop source widget.
 *
 * ------------------------------------------------------------------------
 */
static void
SourceEventProc(clientData, eventPtr)
    ClientData clientData;	/* drag&drop registration list */
    XEvent *eventPtr;		/* event description */
{
    Source *srcPtr = (Source *) clientData;

    if (eventPtr->type == DestroyNotify) {
	DestroySource(srcPtr);
    }
}

/*
 * ------------------------------------------------------------------------
 *
 *  CreateTarget --
 *
 *	Looks for a Target record in the hash table for drag&drop
 *	target widgets.  Creates a new record if the widget name is
 *	not already registered.  Returns a pointer to the desired
 *	record.
 *
 * ------------------------------------------------------------------------
 */
static Target *
CreateTarget(tkwin, newPtr)
    Tk_Window tkwin;		/* widget pathname for desired record */
    int *newPtr;		/* returns non-zero => new record created */
{
    Target *targetPtr;
    Tcl_HashEntry *hPtr;

    hPtr = Tcl_CreateHashEntry(&targetTable, (char *)tkwin, newPtr);
    if (*newPtr) {
	/*
         *  Initialize a data structure for the widget...
         */
	targetPtr = (Target *) calloc(1, sizeof(Target));
	targetPtr->tkwin = tkwin;
	targetPtr->display = Tk_Display(tkwin);
	targetPtr->hashPtr = hPtr;
	Tcl_SetHashValue(hPtr, (ClientData)targetPtr);
    }
    return (Target *) Tcl_GetHashValue(hPtr);
}

/*
 * ------------------------------------------------------------------------
 *
 *  DestroyTarget --
 *
 * ------------------------------------------------------------------------
 */
static void
DestroyTarget(targetPtr)
    Target *targetPtr;
{
    register Blt_ListItem item;
    char *cmd;

    for (item = Blt_ListFirstItem(targetPtr->listPtr); item != NULL;
	item = Blt_ListNextItem(item)) {
	cmd = (char *)Blt_ListGetValue(item);
	if (cmd != NULL) {
	    free((char *)cmd);
	}
    }
    Blt_ListDestroy(targetPtr->listPtr);
    if (targetPtr->hashPtr != NULL) {
	Tcl_DeleteHashEntry(targetPtr->hashPtr);
    }
    free((char *)targetPtr);
}

/*
 * ------------------------------------------------------------------------
 *
 *  TargetEventProc --
 *
 *  Invoked by Tk_HandleEvent whenever a DestroyNotify event is received
 *  on a registered drag&drop target widget.
 * ------------------------------------------------------------------------
 */
static void
TargetEventProc(clientData, eventPtr)
    ClientData clientData;	/* drag&drop registration list */
    XEvent *eventPtr;		/* event description */
{
    Target *targetPtr = (Target *) clientData;

    if (eventPtr->type == DestroyNotify) {
#ifdef	WIN32
	/*
	 * Under Win32 you must removed properties before the window
	 * can be destroyed.
	 */
	RemoveWindowProperty(targetPtr->tkwin, dndAtom);
#endif
	DestroyTarget(targetPtr);
    }
}

/*
 * ------------------------------------------------------------------------
 *
 *  DndSend --
 *
 *	Invoked after a drop operation to send data to the drop
 *	application.
 *
 * ------------------------------------------------------------------------
 */
static void
DndSend(srcPtr)
    register Source *srcPtr;	/* drag&drop source record */
{
    int status;
    TreeNode *treePtr;
    Tcl_DString buffer;
    SubstDescriptors subst[3];
    Tcl_DString dString;

    treePtr = GetTargetWindow(srcPtr, srcPtr->token.lastX, srcPtr->token.lastY);
    if (treePtr == NULL) {
	return;
    }
    Tcl_DStringInit(&buffer);
    Tcl_DStringAppend(&buffer, srcPtr->pkgCmdResult, -1);

    /*
     *  See if current position is over drop point.
     */
    Tcl_DStringInit(&dString);
    Blt_DStringAppendElements(&dString, "send", treePtr->interpName,
	dragDropCmd, "location", (char *)NULL);
    Tcl_DStringAppendElement(&dString, Blt_Int(srcPtr->token.lastX));
    Tcl_DStringAppendElement(&dString, Blt_Int(srcPtr->token.lastY));
    status = Tcl_Eval(srcPtr->interp, Tcl_DStringValue(&dString));
    Tcl_DStringFree(&dString);
    if (status != TCL_OK) {
	goto reject;
    }
    if (treePtr->dataType != NULL) {
	Blt_ListItem item;


	item = Blt_ListFind(srcPtr->listPtr, treePtr->dataType);
	if (item != NULL) {
	    char *cmd;

	    cmd = (char *)Blt_ListGetValue(item);
	    if (cmd != NULL) {
		subst[0].letter = 'i';
		subst[0].value = treePtr->interpName;
		subst[1].letter = 'w';
		subst[1].value = treePtr->targetName;
		subst[2].letter = 'v';
		subst[2].value = srcPtr->pkgCmdResult;

		Tcl_DStringInit(&dString);
		status = Tcl_Eval(srcPtr->interp,
		    ExpandPercents(cmd, subst, 3, &dString));
		Tcl_DStringFree(&dString);

		Tcl_DStringSetLength(&buffer, 0);
		Tcl_DStringAppend(&buffer,
		    Tcl_GetStringResult(srcPtr->interp), -1);
	    }
	}
    } else {
	Tcl_AppendResult(srcPtr->interp, "target \"",
	    treePtr->targetName,
	    "\" does not recognize handlers for source \"",
	    Tk_PathName(srcPtr->tkwin), "\"", (char *)NULL);
	goto reject;
    }
    /*
     *  Part 2:	If the "send" of data was successful, now tell target
     *		application to handle the data.
     */
    Tcl_DStringInit(&dString);
    Blt_DStringAppendElements(&dString, "send", treePtr->interpName,
	dragDropCmd, "target", treePtr->targetName, "handle",
	treePtr->dataType, Tcl_DStringValue(&buffer), (char *)NULL);
    status = Tcl_Eval(srcPtr->interp, Tcl_DStringValue(&dString));
    Tcl_DStringFree(&dString);
    if (status != TCL_OK) {
	goto reject;
    }
    HideToken(&(srcPtr->token));
    Tcl_DStringFree(&buffer);
    return;
  reject:
    /*
     * Give failure information to user.  If an error occurred and an
     * error proc is defined, then use it to handle the error.
     */
    RejectToken(&(srcPtr->token));
    Tcl_DStringFree(&buffer);
    if (errorCmd != NULL) {
	Tcl_VarEval(srcPtr->interp, errorCmd, " {",
	    Tcl_GetStringResult(srcPtr->interp), "}", (char *)NULL);
    }
}

/*
 * ------------------------------------------------------------------------
 *
 *  InitRoot --
 *
 *	Invoked at the start of a "drag" operation to capture the
 *	positions of all windows on the current root.  Queries the
 *	entire window hierarchy and determines the placement of each
 *	window.  Queries "DragDropInfo" property info where
 *	appropriate.  This information is used during the drag
 *	operation to determine when the drag&drop token is over a
 *	valid drag&drop target.
 *
 *  Results:
 *	Returns the record for the root window, which contains records
 *	for all other windows as children.
 *
 * ------------------------------------------------------------------------
 */
static TreeNode *
InitRoot(display)
    Display *display;		/* drag&drop source window */
{
    TreeNode *treePtr;

    treePtr = (TreeNode *) calloc(1, sizeof(TreeNode));
    assert(treePtr);
#ifdef WIN32
    treePtr->windowToken = GetDesktopWindow();
#else
    treePtr->windowToken = DefaultRootWindow(display);
#endif
    QueryTree(display, treePtr);
    return treePtr;
}

/*
 * ------------------------------------------------------------------------
 *
 *  FindTopWindow --
 *
 *	Searches for the topmost descendant window in the given tree
 *	at the X-Y coordinates.
 *
 *  Results:
 *	Returns a pointer to the node representing the window containing
 *	the point.  If one can't be found, NULL is returned.
 *
 * ------------------------------------------------------------------------
 */
static TreeNode *
FindTopWindow(display, rootPtr, x, y)
    Display *display;
    TreeNode *rootPtr;
    int x, y;
{
    register Blt_ListItem item;
    register TreeNode *treePtr;

    if (!rootPtr->initialized) {
	QueryTree(display, rootPtr);
    }
    if ((x < rootPtr->x0) || (x > rootPtr->x1) ||
	(y < rootPtr->y0) || (y > rootPtr->y1)) {
	return NULL;		/* Point is not over window  */
    }
    treePtr = rootPtr;

    /*
     * The window list is ordered top to bottom, so stop when we find
     * the first child that contains the X-Y coordinate. It will be
     * the topmost window in that hierarchy.  If none exists, then we
     * already have the topmost window.
     */
  descend:
    for (item = Blt_ListFirstItem(rootPtr->children); item != NULL;
	item = Blt_ListNextItem(item)) {
	rootPtr = (TreeNode *) Blt_ListGetValue(item);
	if (!rootPtr->initialized) {
	    QueryTree(display, rootPtr);
	}
	if ((x >= rootPtr->x0) && (x <= rootPtr->x1) &&
	    (y >= rootPtr->y0) && (y <= rootPtr->y1)) {
	    /*
	     * Remember the last window containing the pointer and
	     * descend into its window hierarchy. We'll look for a
	     * child that also contains the pointer.
	     */
	    treePtr = rootPtr;
	    goto descend;
	}
    }
    return treePtr;
}

/*
 * ------------------------------------------------------------------------
 *
 *  GetTargetWindow --
 *
 *      Checks to see if a compatible drag&drop target exists at the
 *      given position.  A target is "compatible" if it is a drag&drop
 *      window, and if it has a handler that is compatible with the
 *      current source window.
 *
 *  Results:
 *	Returns a pointer to a structure describing the target, or NULL
 *	if no compatible target is found.
 *
 * ------------------------------------------------------------------------ 
 */
static TreeNode *
GetTargetWindow(srcPtr, x, y)
    Source *srcPtr;		/* drag&drop source window */
    int x, y;			/* current drag&drop location
				 * (in virtual coords) */
{
    int virtX, virtY;
    int dummy;
    TreeNode *treePtr;
    char **elemArr;
    int numElems;
    register int i;
    char *dndProp;
    Blt_List *listPtr;
    int result;

    /*
     * If no window info has been been gathered yet for this target,
     * then abort this call.  This probably means that the token is
     * moved before it has been properly built.
     */
    if (srcPtr->rootPtr == NULL) {
	return NULL;
    }
    /*
     *  Adjust current location for virtual root windows.
     */
    Tk_GetVRootGeometry(srcPtr->tkwin, &virtX, &virtY, &dummy, &dummy);
    x += virtX;
    y += virtY;

    if (srcPtr->sendTypes == NULL) {
	return NULL;		/* Send is turned off. */
    }
    treePtr = FindTopWindow(srcPtr->display, srcPtr->rootPtr, x, y);
    if (treePtr == NULL) {
	return NULL;		/* Not over a window. */
    }
    if ((!srcPtr->selfTarget) &&
	(GetWindowToken(srcPtr->tkwin) == treePtr->windowToken)) {
	return NULL;		/* If the self-target flag isn't set,
				 *  don't allow the source window to
				 *  drop onto itself.  */
    }
    /* See if this window has a "DragDropInfo" property. */
    dndProp = FindWindowProperty(srcPtr->display, treePtr->windowToken, dndAtom);
    if (dndProp == NULL) {
	return NULL;		/* No property on window. */
    }
    result = Tcl_SplitList(srcPtr->interp, dndProp, &numElems, &elemArr);
    XFree((char *)dndProp);
    if (result != TCL_OK) {
	return NULL;		/* Malformed property list. */
    }
    listPtr = NULL;
    Blt_ListDestroy(treePtr->dataTypes);
    if (numElems > 2) {
	treePtr->interpName = Blt_GetUid(elemArr[1]);
	treePtr->targetName = Blt_GetUid(elemArr[2]);
	if (numElems > 3) {
	    listPtr = Blt_CreateList(TCL_STRING_KEYS);
	    for (i = 3; i < numElems; i++) {
		Blt_ListAppend(listPtr, elemArr[i], (ClientData)0);
	    }
	}
    }
    free((char *)elemArr);
    treePtr->dataTypes = listPtr;

    /*
     * See if the candidate target has a handler that matches one of
     * the data types our source can deliver.
     */
    if (treePtr->dataTypes != NULL) {
	Blt_ListItem srcItem, targetItem;
	char *dataType;

	srcItem = targetItem = NULL;
	dataType = NULL;
	if (strcmp(srcPtr->sendTypes[0], "all") == 0) {
	    for (targetItem = Blt_ListFirstItem(treePtr->dataTypes);
		targetItem != NULL; targetItem = Blt_ListNextItem(targetItem)) {
		dataType = Blt_ListGetKey(targetItem);
		srcItem = Blt_ListFind(srcPtr->listPtr, dataType);
		if (srcItem != NULL) {
		    break;
		}
	    }
	} else {
	    register char **typePtr;

	    for (typePtr = srcPtr->sendTypes; *typePtr != NULL; typePtr++) {
		dataType = *typePtr;
		srcItem = Blt_ListFind(srcPtr->listPtr, dataType);
		if (srcItem == NULL) {
		    continue;
		}
		targetItem = Blt_ListFind(srcPtr->listPtr, dataType);
		if (targetItem == NULL) {
		    continue;
		}
		break;
	    }
	}
	if ((srcItem != NULL) && (targetItem != NULL)) {
	    treePtr->dataType = dataType;
	    return treePtr;
	}
    }
    return NULL;		/* No matching data type. */
}

/*
 * ------------------------------------------------------------------------
 *
 *  ReleaseTreeInfo --
 *
 * ------------------------------------------------------------------------
 */
static void
ReleaseTreeInfo(treePtr)
    TreeNode *treePtr;		/* Window to be freed */
{
    TreeNode *childPtr;
    Blt_ListItem item;

    /* Release properties and handlers. */
    if (treePtr->dndProp != NULL) {
	XFree((char *)treePtr->dndProp);
    }
    treePtr->dndProp = NULL;
    if (treePtr->interpName != NULL) {
	Blt_FreeUid(treePtr->interpName);
    }
    treePtr->interpName = NULL;
    if (treePtr->targetName != NULL) {
	Blt_FreeUid(treePtr->targetName);
    }
    treePtr->targetName = NULL;
    Blt_ListDestroy(treePtr->dataTypes);
    treePtr->dataTypes = NULL;

    /* Throw away left over old slots. */
    for (item = Blt_ListFirstItem(treePtr->children); item != NULL;
	item = Blt_ListNextItem(item)) {
	childPtr = (TreeNode *) Blt_ListGetValue(item);
	DestroyTree(childPtr);
    }
    Blt_ListDestroy(treePtr->children);
    treePtr->children = NULL;
}

/*
 * ------------------------------------------------------------------------
 *
 *  DestroyTree --
 *
 * ------------------------------------------------------------------------
 */
static void
DestroyTree(treePtr)
    TreeNode *treePtr;		/* window rep to be freed */
{
    ReleaseTreeInfo(treePtr);
    free((char *)treePtr);
}

/*
 * ------------------------------------------------------------------------
 *
 *  QueryTree --
 *
 *	Invoked during "drag" operations to dig a little deeper into
 *	the root window hierarchy and cache the resulting
 *	information.  If a point coordinate lies within an
 *	uninitialized TreeNode, this routine is called to query
 *	window coordinates and drag&drop info.  If this particular
 *	window has any children, more uninitialized TreeNode
 *	structures are allocated.  Further queries will cause these
 *	structures to be initialized in turn.
 *
 * ------------------------------------------------------------------------
 */
static void
QueryTree(display, treePtr)
    Display *display;
    TreeNode *treePtr;		/* window rep to be initialized */
{
    TreeNode *childPtr;
    XWindowAttributes winInfo;
    register Blt_ListItem item;
    Blt_List *listPtr;

    if (treePtr->initialized) {
	return;
    }
#ifdef notdef
    /* Release properties and handlers. */
    ReleaseTreeInfo(treePtr);
#endif

    /*
     *  Query for the window coordinates.
     */
    listPtr = NULL;
    item = NULL;
    if (XGetWindowAttributes(display, (Window)treePtr->windowToken, &winInfo) &&
	(winInfo.map_state == IsViewable)) {
	register Blt_ListItem newItem;

	treePtr->x0 = winInfo.x;
	treePtr->y0 = winInfo.y;
	treePtr->x1 = winInfo.x + winInfo.width;
	treePtr->y1 = winInfo.y + winInfo.height;

#ifndef WIN32
	if (treePtr->parentPtr != NULL) {	/* offset by parent coords */
	    treePtr->x0 += treePtr->parentPtr->x0;
	    treePtr->y0 += treePtr->parentPtr->y0;
	    treePtr->x1 += treePtr->parentPtr->x0;
	    treePtr->y1 += treePtr->parentPtr->y0;
	}
#endif
	treePtr->dndProp = NULL;
	/*
	 * First get the list of child windows in order of how they are
	 * stacked. The topmost window should be first in the list.
	 * ListPtr will be NULL if there are no children.
	 */
	listPtr = GetWindowStackOrder(display, treePtr->windowToken);

	/* Add and initialize extra slots if needed. */
	for (newItem = Blt_ListFirstItem(listPtr); newItem != NULL;
	    newItem = Blt_ListNextItem(newItem)) {
	    childPtr = (TreeNode *) calloc(1, sizeof(TreeNode));
	    assert(childPtr);
	    childPtr->initialized = FALSE;
	    childPtr->windowToken = (WindowToken) Blt_ListGetKey(newItem);
	    childPtr->parentPtr = treePtr;
	    Blt_ListSetValue(newItem, (ClientData)childPtr);
	}
    } else {
	/* If it's not viewable don't bother doing anything else. */
	treePtr->x0 = treePtr->y0 = -1;
	treePtr->x1 = treePtr->y1 = -1;
    }
    treePtr->children = listPtr;
    treePtr->initialized = TRUE;
}

/*
 * ------------------------------------------------------------------------
 *
 *  AddTargetProperty --
 *
 *	Attaches a drag&drop property to the given target window.
 *	This property allows us to recognize the window later as a
 *	valid target. It also stores important information including
 *	the interpreter managing the target and the pathname of the
 *	target window.  Usually this routine is called when the target
 *	is first registered or first exposed (so that the X-window
 *	really exists).
 *
 * ------------------------------------------------------------------------
 */
static void
AddTargetProperty(interp, targetPtr)
    Tcl_Interp *interp;
    Target *targetPtr;		/* drag&drop target window data */
{
    Tk_Window mainWindow;
    char *dndProp;
    Tcl_DString dString;
    register Blt_ListItem item;
    unsigned int numBytes;

    if (targetPtr->tkwin == NULL) {
	return;
    }
    mainWindow = Tk_MainWindow(interp);
    Tcl_DStringInit(&dString);
    Tcl_DStringAppendElement(&dString, propName);
    Tcl_DStringAppendElement(&dString, Tk_Name(mainWindow));
    Tcl_DStringAppendElement(&dString, Tk_PathName(targetPtr->tkwin));

    /*
     * Each window's dnd property contains
     *
     *	1. name of the application (ie. the interpreter's name).
     *	2. Tk path name of the target window?
     *   3. optionally a list of all the data types that can be handled.
     */
    for (item = Blt_ListFirstItem(targetPtr->listPtr); item != NULL;
	item = Blt_ListNextItem(item)) {
	Tcl_DStringAppendElement(&dString, Blt_ListGetKey(item));
    }
    dndProp = Tcl_DStringValue(&dString);
    numBytes = strlen(dndProp) + 1;
    Tk_MakeWindowExist(targetPtr->tkwin);
#ifdef WIN32
    ChangeWindowProperty(targetPtr->tkwin, dndAtom, dndProp);
#else
    XChangeProperty(targetPtr->display, Tk_WindowId(targetPtr->tkwin), dndAtom,
	XA_STRING, 8, PropModeReplace, (unsigned char *)dndProp, numBytes);
#endif
    Tcl_DStringFree(&dString);
}

/*
 * ------------------------------------------------------------------------
 *
 *  ExpandPercents --
 *
 *	Expands all percent substitutions found in the input "str"
 *	that match specifications in the "subs" list.  Any percent
 *	field that is not found in the "subs" list is left alone.
 *	Returns a string that remains valid until the next call to
 *	this routine.
 *
 * ------------------------------------------------------------------------
 */
static char *
ExpandPercents(string, subArr, numSubs, dStrPtr)
    char *string;		/* Incoming command string */
    SubstDescriptors *subArr;	/* Array of known substitutions */
    int numSubs;		/* Number of elements in subs array */
    Tcl_DString *dStrPtr;
{
    register char *chunk, *p;
    char letter;
    char percentSign;
    int i;

    /*
     *  Scan through the copy of the input string, look for
     *  the next '%' character, and try to make the substitution.
     *  Continue doing this to the end of the string.
     */
    chunk = p = string;
    while ((p = strchr(p, '%')) != NULL) {

	/* Copy up to the percent sign.  Repair the string afterwards */
	percentSign = *p;
	*p = '\0';
	Tcl_DStringAppend(dStrPtr, chunk, -1);
	*p = percentSign;

	/* Search for a matching substition rule */
	letter = *(p + 1);
	for (i = 0; i < numSubs; i++) {
	    if (subArr[i].letter == letter) {
		break;
	    }
	}
	if (i < numSubs) {
	    /* Make the substitution */
	    Tcl_DStringAppend(dStrPtr, subArr[i].value, -1);
	} else {
	    /* Copy in the %letter verbatim */
	    char noSubst[3] = "%?";

	    noSubst[1] = letter;
	    Tcl_DStringAppend(dStrPtr, noSubst, -1);
	}
	p += 2;			/* Skip % + letter */
	if (letter == '\0') {
	    p += 1;		/* Premature % substitution (end of string) */
	}
	chunk = p;
    }
    /* Pick up last chunk if a substition wasn't the last thing in the string */
    if (*chunk != '\0') {
	Tcl_DStringAppend(dStrPtr, chunk, -1);
    }
    return Tcl_DStringValue(dStrPtr);
}



static int
DragOp(interp, argc, argv)
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int x, y;
    Token *tokenPtr;
    int status;
    Source *srcPtr;
    SubstDescriptors subst[2];
    char *result;

    /*
     *  HANDLE:  drag&drop drag <path> <x> <y>
     */
    if (argc < 5) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " drag pathname x y\"", (char *)NULL);
	return TCL_ERROR;
    }
    srcPtr = FindSource(argv[2]);
    if (srcPtr == NULL) {
	Tcl_AppendResult(interp, "bad drag&drop source \"", argv[2], "\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    if ((Tcl_GetInt(interp, argv[3], &x) != TCL_OK) ||
	(Tcl_GetInt(interp, argv[4], &y) != TCL_OK)) {
	return TCL_ERROR;
    }
    tokenPtr = &(srcPtr->token);
    locX = x;			/* save drag&drop location */
    locY = y;
    tokenPtr->lastX = x;
    tokenPtr->lastY = y;

    /*
     *  If HideToken() is pending, then do it now!
     */
    if (tokenPtr->timer != 0) {
	Tk_DeleteTimerHandler(tokenPtr->timer);
	HideToken(tokenPtr);
    }
    /*
     *  If pkgCmd is in progress, then ignore subsequent calls
     *  until it completes.  Only perform drag if pkgCmd
     *  completed successfully and token window is mapped.
     */
    if ((!Tk_IsMapped(tokenPtr->tkwin)) && (!srcPtr->pkgCmdInProgress)) {
	Tcl_DString cmdStr;

	/*
	 *  No list of send handlers?  Then source is disabled.
	 *  Abort drag quietly.
	 */
	if (srcPtr->sendTypes == NULL) {
	    return TCL_OK;
	}
	/*
	 *  No token command?  Then cannot build token.
	 *  Signal error.
	 */
	if (srcPtr->pkgCmd == NULL) {
	    Tcl_AppendResult(interp, "missing -packagecmd: ", argv[2],
		(char *)NULL);
	    return TCL_ERROR;
	}
	/*
	 *  Execute token command to initialize token window.
	 */
	srcPtr->pkgCmdInProgress = TRUE;
	subst[0].letter = 'W';
	subst[0].value = Tk_PathName(srcPtr->tkwin);
	subst[1].letter = 't';
	subst[1].value = Tk_PathName(tokenPtr->tkwin);

	Tcl_DStringInit(&cmdStr);
	status = Tcl_Eval(srcPtr->interp,
	    ExpandPercents(srcPtr->pkgCmd, subst, 2, &cmdStr));
	Tcl_DStringFree(&cmdStr);

	srcPtr->pkgCmdInProgress = FALSE;

	result = Tcl_GetStringResult(interp);
	/*
	 *  Null string from the package command?
	 *  Then quietly abort the drag&drop operation.
	 */
	if (result[0] == '\0') {
	    return TCL_OK;
	}
	/*
	 *  Save result of token command for send command.
	 */
	if (srcPtr->pkgCmdResult != NULL) {
	    free((char *)srcPtr->pkgCmdResult);
	}
	srcPtr->pkgCmdResult = strdup(result);
	if (status != TCL_OK) {
	    /*
	     * Token building failed.  If an error handler is defined,
	     * then signal the error.  Otherwise, abort quietly.
	     */
	    if ((errorCmd != NULL) && (errorCmd[0] != '\0')) {
		return Tcl_VarEval(interp, errorCmd, " {", result, "}",
		    (char *)NULL);
	    }
	    return TCL_OK;
	}
	/*
	 *  Install token cursor.
	 */
	if (tokenPtr->cursor != None) {
	    Tk_Cursor cursor;

	    /* Save the old cursor */
	    cursor = GetWidgetCursor(srcPtr->interp, srcPtr->tkwin);
	    if (srcPtr->cursor != None) {
		Tk_FreeCursor(srcPtr->display, srcPtr->cursor);
	    }
	    srcPtr->cursor = cursor;
	    /* Temporarily install the token cursor */
	    Tk_DefineCursor(srcPtr->tkwin, tokenPtr->cursor);
	}
	/*
	 *  Get ready to drag token window...
	 *  1) Cache info for all windows on root
	 *  2) Map token window to begin drag operation
	 */
	if (srcPtr->rootPtr != NULL) {
	    DestroyTree(srcPtr->rootPtr);
	}
	srcPtr->rootPtr = InitRoot(srcPtr->display);

	numActive++;		/* one more drag&drop window active */

	if (Tk_WindowId(tokenPtr->tkwin) == None) {
	    Tk_MakeWindowExist(tokenPtr->tkwin);
	}
	if (!Tk_IsMapped(tokenPtr->tkwin)) {
	    Tk_MapWindow(tokenPtr->tkwin);
	}
	RaiseToken(srcPtr, tokenPtr);
    }
    /*
     *  Arrange to update status of token window...
     */
    Tk_CancelIdleCall(UpdateToken, (ClientData)srcPtr);
    Tk_DoWhenIdle(UpdateToken, (ClientData)srcPtr);

    /*
     *  Move the token window to the current drag point...
     */
    MoveToken(srcPtr, tokenPtr);
    return TCL_OK;
}

/*
 *  HANDLE:  drag&drop drop <path> <x> <y>
 */
static int
DropOp(interp, argc, argv)
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Source *srcPtr;
    int x, y;

    if (argc < 5) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " drop pathname x y\"", (char *)NULL);
	return TCL_ERROR;
    }
    srcPtr = FindSource(argv[2]);
    if (srcPtr == NULL) {
	Tcl_AppendResult(interp, "bad drag&drop source \"", argv[2], "\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    if ((Tcl_GetInt(interp, argv[3], &x) != TCL_OK) ||
	(Tcl_GetInt(interp, argv[4], &y) != TCL_OK)) {
	return TCL_ERROR;
    }
    locX = x;			/* save drag&drop location */
    locY = y;
    srcPtr->token.lastX = x;
    srcPtr->token.lastY = y;

    /*
     *  Put the cursor back to its usual state.
     */
    if (srcPtr->cursor == None) {
	Tk_UndefineCursor(srcPtr->tkwin);
    } else {
	Tk_DefineCursor(srcPtr->tkwin, srcPtr->cursor);
    }
    Tk_CancelIdleCall(UpdateToken, (ClientData)srcPtr);

    /*
     *  Make sure that token window was not dropped before it
     *  was either mapped or packed with info.
     */
    if ((Tk_IsMapped(srcPtr->token.tkwin)) && (!srcPtr->pkgCmdInProgress)) {
	UpdateToken((ClientData)srcPtr);

	if (srcPtr->sendTypes != NULL) {
	    if (srcPtr->token.overTarget) {
		DndSend(srcPtr);
	    } else {
		HideToken(&(srcPtr->token));
	    }
	}
	numActive--;		/* one fewer active token window */
    }
    return TCL_OK;
}

/*
 *  HANDLE:  drag&drop errors ?<proc>?
 */
static int
ErrorsOp(interp, argc, argv)
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    if (argc == 3) {
	if (errorCmd) {
	    free((char *)errorCmd);
	}
	errorCmd = strdup(argv[2]);
    } else if (argc != 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
	    argv[0], " errors ?proc?\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    Tcl_SetResult(interp, errorCmd, TCL_VOLATILE);
    return TCL_OK;
}

/*
 *  HANDLE:  drag&drop active
 */
static int
ActiveOp(interp, argc, argv)
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    if (argc != 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " active\"", (char *)NULL);
	return TCL_ERROR;
    }
    Tcl_SetResult(interp, (numActive > 0) ? "1" : "0", TCL_STATIC);
    return TCL_OK;
}

/*
 *  HANDLE:  drag&drop location ?<x> <y>?
 */
static int
LocationOp(interp, argc, argv)
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    if ((argc != 2) && (argc != 4)) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " location ?x y?\"", (char *)NULL);
	return TCL_ERROR;
    }
    if (argc == 4) {
	int x, y;

	if ((Tcl_GetInt(interp, argv[2], &x) != TCL_OK) ||
	    (Tcl_GetInt(interp, argv[3], &y) != TCL_OK)) {
	    return TCL_ERROR;
	}
	locX = x;
	locY = y;
    }
    Tcl_AppendElement(interp, Blt_Int(locX));
    Tcl_AppendElement(interp, Blt_Int(locY));
    return TCL_OK;
}

/*
 *  HANDLE:  drag&drop token <pathName>
 */
static int
TokenOp(interp, argc, argv)
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Source *srcPtr;

    srcPtr = FindSource(argv[2]);
    if (srcPtr == NULL) {
	Tcl_AppendResult(interp, "window \"", argv[2],
	    "\" has not been initialized as a drag&drop source", (char *)NULL);
	return TCL_ERROR;
    }
    if (argc > 3) {
	if (ConfigureToken(interp, srcPtr, argc - 3, argv + 3) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    Tcl_SetResult(interp, Tk_PathName(srcPtr->token.tkwin), TCL_STATIC);
    return TCL_OK;
}

static int
HandlerOpOp(srcPtr, interp, argc, argv)
    Source *srcPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Blt_ListItem item;
    char *cmd;

    /*
     *  HANDLE:  drag&drop source <pathName> handler \
     *             ?<data>? ?<scmd>...?
     */
    if (srcPtr->listPtr == NULL) {
	srcPtr->listPtr = Blt_CreateList(TCL_STRING_KEYS);
    }
    if (argc == 4) {
	/* Show source handler data types */
	for (item = Blt_ListFirstItem(srcPtr->listPtr);
	    item != NULL; item = Blt_ListNextItem(item)) {
	    Tcl_AppendElement(interp, Blt_ListGetKey(item));
	}
	return TCL_OK;
    }
    item = Blt_ListFind(srcPtr->listPtr, argv[4]);
    if (item == NULL) {
	if (strstr(argv[4], " ")) {
	    Tcl_AppendResult(interp, "bad source handler name \"",
		argv[4], "\": should not contain spaces",
		(char *)NULL);
	    return TCL_ERROR;
	}
	if (item == NULL) {
	    item = Blt_ListAppend(srcPtr->listPtr, argv[4], (ClientData)NULL);
	}
    }
    /*
     *  HANDLE:  drag&drop source <pathName> handler <data>
     *
     *    Create the new <data> type if it doesn't already
     *    exist, and return the code associated with it.
     */
    if (argc == 5) {
	cmd = (char *)Blt_ListGetValue(item);
	if (cmd == NULL) {
	    cmd = "";
	}
	Tcl_SetResult(interp, cmd, TCL_STATIC);
	return TCL_OK;
    }
    /*
     *  HANDLE:  drag&drop source <pathName> handler \
     *               <data> <cmd> ?<arg>...?
     *
     *    Create the new <data> type and set its command
     */
    cmd = Tcl_Concat(argc - 5, argv + 5);
    Blt_ListSetValue(item, (ClientData)cmd);
    return TCL_OK;
}

/*
 *  HANDLE:  drag&drop source
 *           drag&drop source <pathName> ?options...?
 *           drag&drop source <pathName> handler ?<data>? ?<scmd> <arg>...?
 */
static int
SourceOp(interp, argc, argv)
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Source *srcPtr;
    int isNew;
    Token *tokenPtr;

    if (argc == 2) {
	Tcl_HashSearch cursor;
	Tcl_HashEntry *hPtr;
	char *name;

	for (hPtr = Tcl_FirstHashEntry(&sourceTable, &cursor);
	    hPtr != NULL; hPtr = Tcl_NextHashEntry(&cursor)) {
	    name = Tcl_GetHashKey(&sourceTable, hPtr);
	    Tcl_AppendElement(interp, name);
	}
	return TCL_OK;
    }
    /*
     *  Find or create source info...
     */
    srcPtr = CreateSource(interp, argv[2], &isNew);
    if (srcPtr == NULL) {
	return TCL_ERROR;
    }
    tokenPtr = &(srcPtr->token);
    if (argc > 3) {
	char c;
	int length;
	int status;

	/*
	 *  HANDLE:  drag&drop source <pathName> ?options...?
	 */
	c = argv[3][0];
	length = strlen(argv[3]);

	if (c == '-') {
	    if (argc == 3) {
		status = Tk_ConfigureInfo(interp, tokenPtr->tkwin, configSpecs,
		    (char *)srcPtr, (char *)NULL, 0);
	    } else if (argc == 4) {
		status = Tk_ConfigureInfo(interp, tokenPtr->tkwin, configSpecs,
		    (char *)srcPtr, argv[3], 0);
	    } else {
		status = ConfigureSource(interp, srcPtr, argc - 3, argv + 3,
		    TK_CONFIG_ARGV_ONLY);
	    }
	    if (status != TCL_OK) {
		return TCL_ERROR;
	    }
	} else if ((c == 'h') && strncmp(argv[3], "handler", length) == 0) {
	    return HandlerOpOp(srcPtr, interp, argc, argv);
	} else {
	    Tcl_AppendResult(interp, "bad operation \"", argv[3],
		"\": must be \"handler\" or a configuration option",
		(char *)NULL);
	    return TCL_ERROR;
	}
    }
    if (isNew) {
	/*
	 *  Create the window for the drag&drop token...
	 */
	if (CreateToken(interp, srcPtr) != TCL_OK) {
	    DestroySource(srcPtr);
	    return TCL_ERROR;
	}
    }
    return TCL_OK;
}

/*
 *  HANDLE:  drag&drop target ?<pathName>? ?handling info...?
 */
static int
TargetOp(interp, argc, argv)
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    SubstDescriptors subst[2];
    Tk_Window tkwin;
    Target *targetPtr;
    int isNew;

    if (argc == 2) {
	Tcl_HashSearch cursor;
	Tcl_HashEntry *hPtr;

	for (hPtr = Tcl_FirstHashEntry(&targetTable, &cursor);
	    hPtr != NULL; hPtr = Tcl_NextHashEntry(&cursor)) {
	    tkwin = (Tk_Window)Tcl_GetHashKey(&targetTable, hPtr);
	    Tcl_AppendElement(interp, Tk_PathName(tkwin));
	}
	return TCL_OK;
    }
    tkwin = Tk_NameToWindow(interp, argv[2], Tk_MainWindow(interp));
    if (tkwin == NULL) {
	Tcl_AppendResult(interp, "window does not exist: ", argv[2],
	    (char *)NULL);
	return TCL_ERROR;
    }
    targetPtr = CreateTarget(tkwin, &isNew);

    /*
     *  If this is a new target, attach a property to identify
     *  window as "drag&drop" target, and arrange for the window
     *  to un-register itself when it is destroyed.
     */
    if (isNew) {
	Tk_MakeWindowExist(targetPtr->tkwin);
	AddTargetProperty(interp, targetPtr);

	/*
	 *  Arrange for the window to unregister itself when it
	 *  is destroyed.
	 */
	Tk_CreateEventHandler(targetPtr->tkwin, StructureNotifyMask,
	    TargetEventProc, (ClientData)targetPtr);
    }
    if ((argc >= 4) && (strcmp(argv[3], "handler") == 0)) {
	/*
	 *  HANDLE:  drag&drop target <pathName> handler
	 *           drag&drop target <pathName> handler ?<data> <cmd> <arg>...?
	 */
	if (targetPtr->listPtr == NULL) {
	    targetPtr->listPtr = Blt_CreateList(TCL_STRING_KEYS);
	}
	if (argc == 4) {
	    Blt_ListItem item;

	    for (item = Blt_ListFirstItem(targetPtr->listPtr); item != NULL;
		item = Blt_ListNextItem(item)) {
		Tcl_AppendElement(interp, Blt_ListGetKey(item));
	    }
	    return TCL_OK;
	} else if (argc >= 6) {
	    Blt_ListItem item;
	    char *cmd;

	    /*
	     *  Process handler definition
	     */
	    if (strstr(argv[4], " ")) {
		Tcl_AppendResult(interp,
		    "bad source handler name \"",
		    argv[4], "\": should not contain spaces",
		    (char *)NULL);
		return TCL_ERROR;
	    }
	    item = Blt_ListFind(targetPtr->listPtr, argv[4]);
	    cmd = Tcl_Concat(argc - 5, argv + 5);
	    if (item == NULL) {
		item = Blt_ListAppend(targetPtr->listPtr, argv[4],
		    (ClientData)cmd);
	    } else {
		char *oldCmd;

		oldCmd = (char *)Blt_ListGetValue(item);
		if (oldCmd != NULL) {
		    free(oldCmd);
		}
		Blt_ListSetValue(item, (ClientData)cmd);
	    }
	    /*
	     * Update the target property on the window.
	     */
	    AddTargetProperty(interp, targetPtr);
	    return TCL_OK;
	}
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " ", argv[1], " ", argv[2], " ", argv[3],
	    " data command ?arg arg...?", (char *)NULL);
	return TCL_ERROR;
    } else if ((argc >= 4) && (strcmp(argv[3], "handle") == 0)) {
	/*
	 *  HANDLE:  drag&drop target <pathName> handle <data> ?<value>?
	 */
	Blt_ListItem item;
	Tcl_DString cmdStr;
	int result;
	char *cmd;

	if (argc < 5 || argc > 6) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " ", argv[1], " ", argv[2], " handle data ?value?",
		(char *)NULL);
	    return TCL_ERROR;
	}
	item = Blt_ListFind(targetPtr->listPtr, argv[4]);
	if (item == NULL) {
	    Tcl_AppendResult(interp, "target cannot handle datatype: ",
		argv[4], (char *)NULL);
	    return TCL_ERROR;	/* no handler found */
	}
	cmd = (char *)Blt_ListGetValue(item);
	if (cmd != NULL) {
	    subst[0].letter = 'W';
	    subst[0].value = Tk_PathName(targetPtr->tkwin);
	    subst[1].letter = 'v';
	    if (argc > 5) {
		subst[1].value = argv[5];
	    } else {
		subst[1].value = "";
	    }
	    Tcl_DStringInit(&cmdStr);
	    result = Tcl_Eval(interp,
		ExpandPercents(cmd, subst, 2, &cmdStr));
	    Tcl_DStringFree(&cmdStr);
	    return result;
	}
	return TCL_OK;
    }
    Tcl_AppendResult(interp, "usage: ", argv[0], " target ", argv[2],
	" handler ?data command arg arg...?\n   or: ",
	argv[0], " target ", argv[2], " handle <data>",
	(char *)NULL);
    return TCL_ERROR;
}

/*
 * ------------------------------------------------------------------------
 *
 *  DndCmd --
 *
 *  Invoked by TCL whenever the user issues a drag&drop command.
 *  Handles the following syntax:
 *
 *    drag&drop source
 *    drag&drop source <pathName> ?options...?
 *    drag&drop source <pathName> handler ?<dataType>? ?<cmd> <arg>...?
 *
 *    drag&drop target
 *    drag&drop target <pathName> handler ?<dataType> <cmd> <arg>...?
 *    drag&drop target <pathName> handle <dataType> ?<value>?
 *
 *    drag&drop token <pathName>
 *    drag&drop drag <pathName> <x> <y>
 *    drag&drop drop <pathName> <x> <y>
 *
 *    drag&drop errors ?<proc>?
 *    drag&drop active
 *    drag&drop location ?<x> <y>?
 *
 * ------------------------------------------------------------------------
 */
static int
DndCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Registration information */
    Tcl_Interp *interp;		/* current interpreter */
    int argc;			/* number of arguments */
    char **argv;		/* argument strings */
{
    int length;
    char c;

    if (!initialized) {
	static char initCmd[] =
	{"\n\
set file [file join $blt_library bltDnd.tcl]\n\
if { [file readable $file] } {\n\
    source $file\n\
}\n"
	};
	if (Tcl_GlobalEval(interp, initCmd) != TCL_OK) {
	    Tcl_AddErrorInfo(interp,
		"\n    (while loading bindings for blt::drag&drop)");
	    return TCL_ERROR;
	}
	Tcl_InitHashTable(&sourceTable, TCL_STRING_KEYS);
	Tcl_InitHashTable(&targetTable, TCL_ONE_WORD_KEYS);
	errorCmd = strdup(DEF_ERROR_PROC);
	numActive = 0;
	locX = locY = 0;
	initialized = TRUE;
#ifdef WIN32
	dndAtom = propName;
#else
	dndAtom = XInternAtom(Tk_Display(Tk_MainWindow(interp)), propName, False);
#endif
    }
    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " oper ?args?\"", (char *)NULL);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);

    if ((c == 's') && strncmp(argv[1], "source", length) == 0) {
	return SourceOp(interp, argc, argv);
    } else if ((c == 't') && (length >= 2) &&
	(strncmp(argv[1], "target", length) == 0)) {
	return TargetOp(interp, argc, argv);
    } else if ((c == 't') && (length >= 2) &&
	(strncmp(argv[1], "token", length) == 0)) {
	return TokenOp(interp, argc, argv);
    } else if ((c == 'd') && strncmp(argv[1], "drag", length) == 0) {
	return DragOp(interp, argc, argv);
    } else if ((c == 'd') && strncmp(argv[1], "drop", length) == 0) {
	return DropOp(interp, argc, argv);
    } else if ((c == 'e') && strncmp(argv[1], "errors", length) == 0) {
	return ErrorsOp(interp, argc, argv);
    } else if ((c == 'a') && strncmp(argv[1], "active", length) == 0) {
	return ActiveOp(interp, argc, argv);
    } else if ((c == 'l') && strncmp(argv[1], "location", length) == 0) {
	return LocationOp(interp, argc, argv);
    }
    /*
     *  Report improper command arguments
     */
    Tcl_AppendResult(interp, "bad operation \"", argv[1],
	"\": must be active, drag, drop, errors, location, ",
	"source, target or token",
	(char *)NULL);
    return TCL_ERROR;
}

/*
 * ------------------------------------------------------------------------
 *  DeleteDndCmd --
 *
 *	Invoked when the drag&drop command is removed from an
 *	interpreter to free up allocated memory.
 *
 * ------------------------------------------------------------------------
 */
/*ARGSUSED*/
static void
DeleteDndCmd(clientData)
    ClientData clientData;	/* Not used.*/
{
    Tcl_HashSearch cursor;
    Tcl_HashEntry *hPtr;
    Source *srcPtr;
    Target *targetPtr;

    for (hPtr = Tcl_FirstHashEntry(&sourceTable, &cursor); hPtr != NULL;
	hPtr = Tcl_NextHashEntry(&cursor)) {
	srcPtr = (Source *) Tcl_GetHashValue(hPtr);
	srcPtr->hashPtr = NULL;
	DestroySource(srcPtr);
    }
    Tcl_DeleteHashTable(&sourceTable);
    for (hPtr = Tcl_FirstHashEntry(&targetTable, &cursor); hPtr != NULL;
	hPtr = Tcl_NextHashEntry(&cursor)) {
	targetPtr = (Target *) Tcl_GetHashValue(hPtr);
	targetPtr->hashPtr = NULL;
	DestroyTarget(targetPtr);
    }
    Tcl_DeleteHashTable(&targetTable);
    if (errorCmd != NULL) {
	free(errorCmd);
    }
}

/*
 * ------------------------------------------------------------------------
 *  Blt_DndInit --
 *
 *	Adds the drag&drop command to the given interpreter.  Should
 *	be invoked to properly install the command whenever a new
 *	interpreter is created.
 *
 * ------------------------------------------------------------------------
 */
int
Blt_DndInit(interp)
    Tcl_Interp *interp;		/* interpreter to be updated */
{
    static Blt_CmdSpec cmdSpec =
    {
	"drag&drop", DndCmd, DeleteDndCmd,
    };
    if (Blt_InitCmd(interp, "blt", &cmdSpec) == NULL) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

#endif /* NO_DRAGDROP */
