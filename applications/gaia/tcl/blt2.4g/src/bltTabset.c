/*
 * bltTabset.c --
 *
 *	This module implements a tabset widget for the BLT toolkit.
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
 *	Tabset widget created by George A. Howlett (gah@bell-labs.com)
 *
 */

#include "bltInt.h"

#ifndef NO_TABSET

#include "bltList.h"
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <ctype.h>

#if (TK_MAJOR_VERSION < 8)
#define TK_REPARENTED           0x2000
#endif

/*
 * The macro below is used to modify a "char" value (e.g. by casting
 * it to an unsigned character) so that it can be used safely with
 * macros such as isspace.
 */
#define CLAMP(val,low,hi)	\
	(((val) < (low)) ? (low) : ((val) > (hi)) ? (hi) : (val))

#define GAP			2
#define SELECT_PAD		5
#define OUTER_PAD		1
#define LABEL_PAD		1
#define LABEL_PADX		2
#define LABEL_PADY		2
#define IMAGE_PAD		2
#define CORNER_OFFSET		3

#define POSITION_END		(-1)
#define WORLD(s, sx)		((sx) - (s)->inset + (s)->scrollOffset)

#define SCREEN(s, wx)		((wx) - (s)->scrollOffset + (s)->inset)

#define TABWIDTH(s, t)		\
  ((s)->side & SIDE_VERTICAL) ? (t)->height : (t)->width)
#define TABHEIGHT(s, t)		\
  ((s)->side & SIDE_VERTICAL) ? (t)->height : (t)->width)

#define VPORTWIDTH(s)		 \
  (((s)->side & SIDE_HORIZONTAL) ? (Tk_Width((s)->tkwin) - 2 * (s)->inset) : \
   (Tk_Height((s)->tkwin) - 2 * (s)->inset))

#define VPORTHEIGHT(s)		 \
  (((s)->side & SIDE_VERTICAL) ? (Tk_Width((s)->tkwin) - 2 * (s)->inset) : \
   (Tk_Height((s)->tkwin) - 2 * (s)->inset))

#define GETATTR(t,attr)		\
   (((t)->attr != NULL) ? (t)->attr : (t)->setPtr->defAttrs.attr)

/*
 * ----------------------------------------------------------------------------
 *
 *  Internal widget flags:
 *
 *	TABSET_LAYOUT		The layout of the widget needs to be
 *				recomputed.
 *
 *	TABSET_REDRAW		A redraw request is pending for the widget.
 *
 *	TABSET_SCROLL		A scroll request is pending.
 *
 *	TABSET_FOCUS		The widget is receiving keyboard events.
 *				Draw the focus highlight border around the
 *				widget.
 *
 *	TABSET_MULTIPLE_TIER	Tabset is using multiple tiers.
 *
 *	TABSET_STATIC		Tabset does not scroll.
 *
 * ---------------------------------------------------------------------------
 */
#define TABSET_LAYOUT		(1<<0)
#define TABSET_REDRAW		(1<<1)
#define TABSET_SCROLL		(1<<2)
#define TABSET_FOCUS		(1<<4)

#define TABSET_STATIC		(1<<8)
#define TABSET_MULTIPLE_TIER	(1<<9)

#define SIDE_TOP		(1<<0)
#define SIDE_RIGHT		(1<<1)
#define SIDE_LEFT		(1<<2)
#define SIDE_BOTTOM		(1<<3)

#define SIDE_VERTICAL	(SIDE_LEFT | SIDE_RIGHT)
#define SIDE_HORIZONTAL (SIDE_TOP | SIDE_BOTTOM)

#define DEF_TABSET_ACTIVE_BG_COLOR	RGB_COLOR_GREY90
#define DEF_TABSET_ACTIVE_BG_MONO	STD_MONO_ACTIVE_BG
#define DEF_TABSET_ACTIVE_FG_COLOR	STD_COLOR_ACTIVE_FG
#define DEF_TABSET_ACTIVE_FG_MONO	STD_MONO_ACTIVE_FG
#define DEF_TABSET_BG_MONO		STD_MONO_NORMAL_BG
#define DEF_TABSET_BG_COLOR		STD_COLOR_NORMAL_BG
#define DEF_TABSET_BORDER_WIDTH		STD_BORDERWIDTH
#define DEF_TABSET_COMMAND		(char *)NULL
#define DEF_TABSET_CURSOR		(char *)NULL
#define DEF_TABSET_DASHES		"dash"
#define DEF_TABSET_FG_COLOR		STD_COLOR_NORMAL_FG
#define DEF_TABSET_FG_MONO		STD_MONO_NORMAL_FG
#define DEF_TABSET_FONT			STD_FONT
#define DEF_TABSET_GAP			"3"
#define DEF_TABSET_HEIGHT		"0"
#define DEF_TABSET_HIGHLIGHT_BG_COLOR	STD_COLOR_NORMAL_BG
#define DEF_TABSET_HIGHLIGHT_BG_MONO	STD_MONO_NORMAL_BG
#define DEF_TABSET_HIGHLIGHT_COLOR	RGB_COLOR_BLACK
#define DEF_TABSET_HIGHLIGHT_WIDTH	"2"
#define DEF_TABSET_NORMAL_BG_COLOR 	STD_COLOR_NORMAL_BG
#define DEF_TABSET_NORMAL_FG_MONO	STD_MONO_ACTIVE_FG
#define DEF_TABSET_RELIEF		"sunken"
#define DEF_TABSET_ROTATE		"0.0"
#define DEF_TABSET_SCROLL_INCREMENT 	"0"
#define DEF_TABSET_SELECT_BG_COLOR 	RGB_COLOR_GREY82
#define DEF_TABSET_SELECT_BG_MONO  	STD_MONO_SELECT_BG
#define DEF_TABSET_SELECT_BORDER_WIDTH 	"1"
#define DEF_TABSET_SELECT_CMD		(char *)NULL
#define DEF_TABSET_SELECT_FG_COLOR 	STD_COLOR_SELECT_FG
#define DEF_TABSET_SELECT_FG_MONO  	STD_MONO_SELECT_FG
#define DEF_TABSET_SELECT_MODE		"multiple"
#define DEF_TABSET_SELECT_RELIEF	"raised"
#define DEF_TABSET_SELECT_PAD		"5"
#define DEF_TABSET_SHADOW_COLOR		RGB_COLOR_BLACK
#define DEF_TABSET_SIDE			"top"
#define DEF_TABSET_TAB_BG_COLOR		RGB_COLOR_GREY75
#define DEF_TABSET_TAB_BG_MONO		STD_MONO_SELECT_BG
#define DEF_TABSET_TAB_RELIEF		"raised"
#define DEF_TABSET_TAKE_FOCUS		"1"
#define DEF_TABSET_TEXT_COLOR		STD_COLOR_NORMAL_FG
#define DEF_TABSET_TEXT_MONO		STD_MONO_NORMAL_FG
#define DEF_TABSET_TEXT_SIDE		"left"
#define DEF_TABSET_TIERS		"0"
#define DEF_TABSET_TILE			(char *)NULL
#define DEF_TABSET_WIDTH		"0"
#define DEF_TABSET_TAB_WIDTH		"no"
#define DEF_TABSET_PAGE_WIDTH		"0"
#define DEF_TABSET_PAGE_HEIGHT		"0"

#define DEF_TAB_ACTIVE_BG		(char *)NULL
#define DEF_TAB_ACTIVE_FG		(char *)NULL
#define DEF_TAB_ANCHOR			"center"
#define DEF_TAB_BG			(char *)NULL
#define DEF_TAB_COMMAND			(char *)NULL
#define DEF_TAB_DATA			(char *)NULL
#define DEF_TAB_FG			(char *)NULL
#define DEF_TAB_FILL			"none"
#define DEF_TAB_FONT			(char *)NULL
#define DEF_TAB_HEIGHT			"0"
#define DEF_TAB_IMAGE			(char *)NULL
#define DEF_TAB_PAD			"0"
#define DEF_TAB_SELECT_BG		(char *)NULL
#define DEF_TAB_SELECT_BORDER_WIDTH 	"1"
#define DEF_TAB_SELECT_CMD		(char *)NULL
#define DEF_TAB_SELECT_FG	 	(char *)NULL
#define DEF_TAB_SHADOW			(char *)NULL
#define DEF_TAB_STATE			"normal"
#define DEF_TAB_STIPPLE			"BLT"
#define DEF_TAB_BIND_TAGS		"all"
#define DEF_TAB_TEXT			(char *)NULL
#define DEF_TAB_VISUAL			(char *)NULL
#define DEF_TAB_WIDTH			"0"
#define DEF_TAB_WINDOW			(char *)NULL

typedef struct Tabset Tabset;

static void SlaveGeometryProc _ANSI_ARGS_((ClientData, Tk_Window));
static void SlaveCustodyProc _ANSI_ARGS_((ClientData, Tk_Window));

static Tk_GeomMgr tabMgrInfo =
{
    "tabset",			/* Name of geometry manager used by winfo */
    SlaveGeometryProc,		/* Procedure to for new geometry requests */
    SlaveCustodyProc,		/* Procedure when window is taken away */
};

extern Tk_CustomOption bltDashesOption;
extern Tk_CustomOption bltFillOption;
extern Tk_CustomOption bltLengthOption;
extern Tk_CustomOption bltListOption;
extern Tk_CustomOption bltPadOption;
extern Tk_CustomOption bltShadowOption;
extern Tk_CustomOption bltStateOption;
extern Tk_CustomOption bltTileOption;
extern Tk_CustomOption bltUidOption;

static int StringToImage _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *string, char *widgRec,
	int offset));
static char *ImageToString _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtrPtr));

static int StringToWindow _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *string, char *widgRec,
	int offset));
static char *WindowToString _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtrPtr));

static int StringToSide _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *string, char *widgRec,
	int offset));
static char *SideToString _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtrPtr));

/*
 * Contains a pointer to the widget that's currently being configured.
 * This is used in the custom configuration parse routine for images.
 */
static Tabset *tabSet;

static Tk_CustomOption imageOption =
{
    StringToImage, ImageToString, (ClientData)&tabSet,
};

static Tk_CustomOption sideOption =
{
    StringToSide, SideToString, (ClientData)0,
};

static Tk_CustomOption windowOption =
{
    StringToWindow, WindowToString, (ClientData)0,
};

/*
 * TabImage --
 *
 *	When multiple instances of an image are displayed in the
 *	same widget, this can be inefficient in terms of both memory
 *	and time.  We only need one instance of each image, regardless
 *	of number of times we use it.  And searching/deleting instances
 *	can be very slow as the list gets large.
 *
 *	The workaround, employed below, is to maintain a hash table of
 *	images that maintains a reference count for each image.
 */

typedef struct TabImage {
    int refCount;		/* Reference counter for this image. */
    Tk_Image tkImage;		/* The Tk image being cached. */
    int width, height;		/* Dimensions of the cached image. */
    Tcl_HashEntry *hashPtr;	/* Hash table pointer to the image. */

}       *TabImage;

#define ImageHeight(image)	((image)->height)
#define ImageWidth(image)	((image)->width)
#define ImageData(image)	((image)->tkImage)

#define TAB_VISIBLE	(1<<0)
#define TAB_REDRAW	(1<<2)

typedef struct {
    Tk_Uid nameId;		/* Identifier for tab entry */
    int index;			/* Index of the tab in the list */
    int state;			/* State of the tab: Disabled, active, or
				 * normal. */
    unsigned int flags;

    int tier;			/* Index of tier [1..numTiers] containing
				 * this tab. */

    int worldX, worldY;		/* Position of the tab in world coordinates. */
    int worldWidth, worldHeight;/* Dimensions of the tab, corrected for
				 * orientation (-side).  It includes the
				 * border, padding, label, etc. */
    int scrnX, scrnY;
    short int scrnWidth, scrnHeight;	/*  */

    Tabset *setPtr;		/* Tabset that includes this
				 * tab. Needed for callbacks can pass
				 * only a tab pointer.  */
    Tk_Uid tags;

    /*
     * Tab label:
     */
    Tk_Uid text;		/* String displayed as the tab's label. */
    TabImage image;		/* Image displayed as the label. */

    short int textWidth, textHeight;
    short int labelWidth, labelHeight;
    Pad iPadX, iPadY;		/* Internal padding around the text */

    Tk_Font font;

    /*
     * Normal:
     */
    XColor *textColor;		/* Text color */
    Tk_3DBorder border;		/* Background color and border for tab.*/

    /*
     * Selected: Tab is currently selected.
     */
    XColor *selColor;		/* Selected text color */
    Tk_3DBorder selBorder;	/* 3D border of selected folder. */

    /*
     * Active: Mouse passes over the tab.
     */
    Tk_3DBorder activeBorder;	/* Active background color. */
    XColor *activeFgColor;	/* Active text color */

    Shadow shadow;
    Pixmap stipple;		/* Stipple for outline of embedded window
				 * when torn off. */
    /*
     * Embedded Window:
     */
    Tk_Window tkwin;		/* Window to be mapped when the tab is
				 * selected.  If NULL, don't make
				 * space for the page. */

    int reqWidth, reqHeight;	/* If non-zero, overrides the
				 * requested dimensions of the
				 * embedded window. */

    Tk_Window container;	/* The window containing the embedded
				 * window.  Does not necessarily have
				 * to be the parent. */

    Tk_Anchor anchor;		/* Anchor: indicates how the embedded
				 * window is positioned within the
				 * extra space on the page. */

    Pad padX, padY;		/* Padding around embedded window */

    Fill fill;			/* Indicates how the window should
				 * fill the page. */

    /*
     * Auxillary information:
     */
    Tk_Uid command;		/* Command (malloc-ed) invoked when the tab
				 * is selected */
    Tk_Uid data;		/* This value isn't used in C code.
				 * It may be used by clients in Tcl bindings
				 * to associate extra data (other than the
				 * label or name) with the tab. */

    Blt_ListItem item;		/* Pointer to where the tab resides in the
				 * list of tabs. */
    GC textGC;
    GC backGC;

    Blt_Tile bgTile;
    GC tileGC;

} Tab;

static Tk_ConfigSpec tabConfigSpecs[] =
{
    {TK_CONFIG_BORDER, "-activebackground", "activeBackground",
	"ActiveBackground",
	DEF_TAB_ACTIVE_BG, Tk_Offset(Tab, activeBorder), TK_CONFIG_NULL_OK},
    {TK_CONFIG_COLOR, "-activeforeground", "activeForeground",
	"ActiveForeground",
	DEF_TAB_ACTIVE_FG, Tk_Offset(Tab, activeFgColor), TK_CONFIG_NULL_OK},
    {TK_CONFIG_ANCHOR, "-anchor", "anchor", "Anchor",
	DEF_TAB_ANCHOR, Tk_Offset(Tab, anchor), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_TAB_BG, Tk_Offset(Tab, border), TK_CONFIG_NULL_OK},
    {TK_CONFIG_SYNONYM, "-bg", "background", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_CUSTOM, "-bindtags", "bindTags", "BindTags",
	DEF_TAB_BIND_TAGS, Tk_Offset(Tab, tags),
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_CUSTOM, "-command", "command", "Command",
	DEF_TAB_COMMAND, Tk_Offset(Tab, command),
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_CUSTOM, "-data", "data", "data",
	DEF_TAB_DATA, Tk_Offset(Tab, data),
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_SYNONYM, "-fg", "foreground", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_CUSTOM, "-fill", "fill", "Fill",
	DEF_TAB_FILL, Tk_Offset(Tab, fill),
	TK_CONFIG_DONT_SET_DEFAULT, &bltFillOption},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_TAB_FG, Tk_Offset(Tab, textColor), TK_CONFIG_NULL_OK},
    {TK_CONFIG_FONT, "-font", "font", "Font",
	DEF_TAB_FONT, Tk_Offset(Tab, font), 0},
    {TK_CONFIG_CUSTOM, "-image", "image", "image",
	DEF_TAB_IMAGE, Tk_Offset(Tab, image),
	TK_CONFIG_NULL_OK, &imageOption},
    {TK_CONFIG_CUSTOM, "-ipadx", "iPadX", "PadX",
	DEF_TAB_PAD, Tk_Offset(Tab, iPadX),
	TK_CONFIG_DONT_SET_DEFAULT, &bltPadOption},
    {TK_CONFIG_CUSTOM, "-ipady", "iPadY", "PadY",
	DEF_TAB_PAD, Tk_Offset(Tab, iPadY),
	TK_CONFIG_DONT_SET_DEFAULT, &bltPadOption},
    {TK_CONFIG_CUSTOM, "-padx", "padX", "PadX",
	DEF_TAB_PAD, Tk_Offset(Tab, padX), 0, &bltPadOption},
    {TK_CONFIG_CUSTOM, "-pady", "padY", "PadY",
	DEF_TAB_PAD, Tk_Offset(Tab, padY), 0, &bltPadOption},
    {TK_CONFIG_BORDER, "-selectbackground", "selectBackground", "Background",
	DEF_TAB_SELECT_BG, Tk_Offset(Tab, selBorder), 0},
    {TK_CONFIG_CUSTOM, "-shadow", "shadow", "Shadow",
	DEF_TAB_SHADOW, Tk_Offset(Tab, shadow),
	TK_CONFIG_NULL_OK, &bltShadowOption},
    {TK_CONFIG_CUSTOM, "-state", "state", "State",
	DEF_TAB_STATE, Tk_Offset(Tab, state), 0, &bltStateOption},
    {TK_CONFIG_BITMAP, "-stipple", "stipple", "Stipple",
	DEF_TAB_STIPPLE, Tk_Offset(Tab, stipple), 0},
    {TK_CONFIG_CUSTOM, "-text", "Text", "Text",
	DEF_TAB_TEXT, Tk_Offset(Tab, text),
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_CUSTOM, "-window", "window", "Window",
	DEF_TAB_WINDOW, Tk_Offset(Tab, tkwin),
	TK_CONFIG_NULL_OK, &windowOption},
    {TK_CONFIG_CUSTOM, "-windowheight", "windowHeight", "WindowHeight",
	DEF_TAB_HEIGHT, Tk_Offset(Tab, reqHeight),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-windowwidth", "windowWidth", "WindowWidth",
	DEF_TAB_WIDTH, Tk_Offset(Tab, reqWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_END, (char *)NULL, (char *)NULL, (char *)NULL,
	(char *)NULL, 0, 0}
};

/*
 * TabAttributes --
 */
typedef struct TabAttributes {
    Tk_Window tkwin;		/* Default window to map pages. */

    int reqWidth, reqHeight;	/* Requested tab size. */
    int constWidth;
    int borderWidth;		/* Width of 3D border around the tab's
				 * label. */
    int pad;			/* Extra padding of a tab entry */

    XColor *activeFgColor;	/* Active foreground. */
    Tk_3DBorder activeBorder;	/* Active background. */
    XColor *selColor;		/* Selected foreground. */
    Tk_Font font;
    XColor *textColor;

    Tk_3DBorder border;		/* Normal background. */
    Tk_3DBorder selBorder;	/* Selected background. */

    Dashes dashes;
    GC normalGC, activeGC;
    int relief;
    char *command;
    double rotate;
    int textSide;

} TabAttributes;

struct Tabset {
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

    unsigned int flags;		/* For bitfield definitions, see below */

    int inset;			/* Total width of all borders, including
				 * traversal highlight and 3-D border.
				 * Indicates how much interior stuff must
				 * be offset from outside edges to leave
				 * room for borders. */
    int pageInset;		/* Total width of 3-D folder border + corner,
				 * Indicates how much interior stuff must
				 * be offset from outside edges of folder.*/

    int yPad;			/* Extra offset for selected tab. Only
				 * for single tiers. */

    int pageTop;		/* Offset from top of tabset to the
				 * start of the page. */

    Tk_Cursor cursor;		/* X Cursor */

    Tk_3DBorder border;		/* 3D border surrounding the window. */
    int borderWidth;		/* Width of 3D border. */
    int relief;			/* 3D border relief. */

    XColor *shadowColor;	/* Shadow color around folder. */
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


    int side;			/* How tabset is oriented: either SIDE_LEFT,
				 * SIDE_RIGHT, SIDE_TOP, or SIDE_BOTTOM. */

    int gap;
    int tabWidth, tabHeight;
    int selectPad;		/* Padding around label of the selected tab. */

    TabAttributes defAttrs;	/* Global attribute information specific to
				 * tabs. */
    GC copyGC;

    Blt_Tile bgTile;

    GC tileGC;			/* GC for tiling the window. The foreground
				 * attribute is the default text color. */

    int reqWidth, reqHeight;	/* Requested dimensions of the tabset
				 * window. */
    int pageWidth, pageHeight;
    int reqPageWidth, reqPageHeight;	/* Dimenions of page. */

    int lastX, lastY;
    /*
     * Scrolling information:
     */
    int worldWidth;
    int scrollOffset;		/* Offset of viewport in world coordinates. */
    char *scrollCmdPrefix;	/* Command strings to control scrollbar.*/

    int scrollUnits;		/* Smallest unit of scrolling for tabs. */

    /*
     * Scanning information:
     */
    int scanAnchor;		/* Scan anchor in screen coordinates. */
    int scanOffset;		/* Offset of the start of the scan in world
				 * coordinates.*/


    int corner;			/* Number of pixels to offset next point
				 * when drawing corners of the folder. */
    int reqTiers;		/* Requested number of tiers. Zero means to
				 * dynamically scroll if there are too many
				 * tabs to be display on a single tier. */
    int numTiers;		/* Actual number of tiers. */

    Tcl_HashTable imageTable;

    Tab *selectPtr;		/* The currently selected tab.
				 * (i.e. its page is displayed). */

    Tab *activePtr;		/* Tab last located under the pointer.
				 * It is displayed with its active
				 * foreground/background colors.  */

    Tab *focusPtr;		/* Tab currently receiving focus. */

    Tab *startPtr;		/* The first tab on the first tier. */

    Blt_List tabList;		/* List of tab entries. Used to
				 * arrange placement of tabs. */

    Tcl_HashTable tabTable;	/* Hash table of tab entries. Used for
				 * lookups of tabs by name. */

    int numVisible;		/* Number of tabs that are currently visible
				 * in the view port. */

    BindTable bindTable;	/* Tab binding information */
};

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_BORDER, "-activebackground", "activeBackground",
	"activeBackground",
	DEF_TABSET_ACTIVE_BG_COLOR, Tk_Offset(Tabset, defAttrs.activeBorder),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_BORDER, "-activebackground", "activeBackground",
	"activeBackground",
	DEF_TABSET_ACTIVE_BG_MONO, Tk_Offset(Tabset, defAttrs.activeBorder),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_COLOR, "-activeforeground", "activeForeground",
	"activeForeground",
	DEF_TABSET_ACTIVE_FG_COLOR, Tk_Offset(Tabset, defAttrs.activeFgColor),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-activeforeground", "activeForeground",
	"activeForeground",
	DEF_TABSET_ACTIVE_FG_MONO, Tk_Offset(Tabset, defAttrs.activeFgColor),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_TABSET_BG_MONO, Tk_Offset(Tabset, border), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_TABSET_BG_COLOR, Tk_Offset(Tabset, border), TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_SYNONYM, "-bd", "borderWidth", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_SYNONYM, "-bg", "background", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_ACTIVE_CURSOR, "-cursor", "cursor", "Cursor",
	DEF_TABSET_CURSOR, Tk_Offset(Tabset, cursor), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-borderwidth", "borderWidth", "BorderWidth",
	DEF_TABSET_BORDER_WIDTH, Tk_Offset(Tabset, borderWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-dashes", "dashes", "Dashes",
	DEF_TABSET_DASHES, Tk_Offset(Tabset, defAttrs.dashes),
	TK_CONFIG_NULL_OK, &bltDashesOption},
    {TK_CONFIG_SYNONYM, "-fg", "foreground", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_FONT, "-font", "font", "Font",
	DEF_TABSET_FONT, Tk_Offset(Tabset, defAttrs.font), 0},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_TABSET_TEXT_COLOR, Tk_Offset(Tabset, defAttrs.textColor),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_TABSET_TEXT_MONO, Tk_Offset(Tabset, defAttrs.textColor),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_CUSTOM, "-gap", "gap", "Gap",
	DEF_TABSET_GAP, Tk_Offset(Tabset, gap),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-height", "height", "Height",
	DEF_TABSET_HEIGHT, Tk_Offset(Tabset, reqHeight),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_COLOR, "-highlightbackground", "highlightBackground",
	"HighlightBackground",
	DEF_TABSET_HIGHLIGHT_BG_COLOR, Tk_Offset(Tabset, highlightBgColor),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-highlightbackground", "highlightBackground",
	"HighlightBackground",
	DEF_TABSET_HIGHLIGHT_BG_MONO, Tk_Offset(Tabset, highlightBgColor),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_COLOR, "-highlightcolor", "highlightColor", "HighlightColor",
	DEF_TABSET_HIGHLIGHT_COLOR, Tk_Offset(Tabset, highlightColor), 0},
    {TK_CONFIG_PIXELS, "-highlightthickness", "highlightThickness",
	"HighlightThickness",
	DEF_TABSET_HIGHLIGHT_WIDTH, Tk_Offset(Tabset, highlightWidth),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-pageheight", "pageHeight", "PageHeight",
	DEF_TABSET_PAGE_HEIGHT, Tk_Offset(Tabset, reqPageHeight),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-pagewidth", "pageWidth", "PageWidth",
	DEF_TABSET_PAGE_WIDTH, Tk_Offset(Tabset, reqPageWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_RELIEF, "-relief", "relief", "Relief",
	DEF_TABSET_RELIEF, Tk_Offset(Tabset, relief), 0},
    {TK_CONFIG_DOUBLE, "-rotate", "rotate", "Rotate",
	DEF_TABSET_ROTATE, Tk_Offset(Tabset, defAttrs.rotate),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BOOLEAN, "-samewidth", "sameWidth", "SameWidth",
	DEF_TABSET_TAB_WIDTH, Tk_Offset(Tabset, defAttrs.constWidth),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_STRING, "-scrollcommand", "scrollCommand", "ScrollCommand",
	(char *)NULL, Tk_Offset(Tabset, scrollCmdPrefix), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-scrollincrement", "scrollIncrement", "ScrollIncrement",
	DEF_TABSET_SCROLL_INCREMENT, Tk_Offset(Tabset, scrollUnits),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_BORDER, "-selectbackground", "selectBackground", "Background",
	DEF_TABSET_SELECT_BG_MONO, Tk_Offset(Tabset, defAttrs.selBorder),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_BORDER, "-selectbackground", "selectBackground", "Background",
	DEF_TABSET_SELECT_BG_COLOR, Tk_Offset(Tabset, defAttrs.selBorder),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_STRING, "-selectcommand", "selectCommand", "SelectCommand",
	DEF_TABSET_SELECT_CMD, Tk_Offset(Tabset, defAttrs.command),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_COLOR, "-selectforeground", "selectForeground", "Foreground",
	DEF_TABSET_SELECT_FG_MONO, Tk_Offset(Tabset, defAttrs.selColor),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_COLOR, "-selectforeground", "selectForeground", "Foreground",
	DEF_TABSET_SELECT_FG_COLOR, Tk_Offset(Tabset, defAttrs.selColor),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_CUSTOM, "-selectpad", "selectPad", "SelectPad",
	DEF_TABSET_SELECT_PAD, Tk_Offset(Tabset, selectPad),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_COLOR, "-shadowcolor", "shadowColor", "ShadowColor",
	DEF_TABSET_SHADOW_COLOR, Tk_Offset(Tabset, shadowColor), 0},
    {TK_CONFIG_CUSTOM, "-side", "side", "side",
	DEF_TABSET_SIDE, Tk_Offset(Tabset, side),
	TK_CONFIG_DONT_SET_DEFAULT, &sideOption},
    {TK_CONFIG_BORDER, "-tabbackground", "tabBackground", "Background",
	DEF_TABSET_TAB_BG_MONO, Tk_Offset(Tabset, defAttrs.border),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_BORDER, "-tabbackground", "tabBackground", "Background",
	DEF_TABSET_TAB_BG_COLOR, Tk_Offset(Tabset, defAttrs.border),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_CUSTOM, "-tabborderwidth", "tabBorderWidth", "BorderWidth",
	DEF_TABSET_BORDER_WIDTH, Tk_Offset(Tabset, defAttrs.borderWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_RELIEF, "-tabrelief", "tabRelief", "TabRelief",
	DEF_TABSET_TAB_RELIEF, Tk_Offset(Tabset, defAttrs.relief), 0},
    {TK_CONFIG_STRING, "-takefocus", "takeFocus", "TakeFocus",
	DEF_TABSET_TAKE_FOCUS, Tk_Offset(Tabset, takeFocus), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-textside", "textSide", "TextSide",
	DEF_TABSET_TEXT_SIDE, Tk_Offset(Tabset, defAttrs.textSide),
	TK_CONFIG_DONT_SET_DEFAULT, &sideOption},
    {TK_CONFIG_CUSTOM, "-tiers", "tiers", "Tiers",
	DEF_TABSET_TIERS, Tk_Offset(Tabset, reqTiers),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-tile", "tile", "Tile",
	(char *)NULL, Tk_Offset(Tabset, bgTile), TK_CONFIG_NULL_OK,
	&bltTileOption},
    {TK_CONFIG_CUSTOM, "-width", "width", "Width",
	DEF_TABSET_WIDTH, Tk_Offset(Tabset, reqWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_END, (char *)NULL, (char *)NULL, (char *)NULL,
	(char *)NULL, 0, 0}
};

/* Forward Declarations */
static void DestroyTabset _ANSI_ARGS_((DestroyData dataPtr));
static void DestroyTearoff _ANSI_ARGS_((DestroyData dataPtr));
static void SlaveEventProc _ANSI_ARGS_((ClientData clientdata,
	XEvent *eventPtr));
static void TearoffEventProc _ANSI_ARGS_((ClientData clientdata,
	XEvent *eventPtr));
static void TabsetEventProc _ANSI_ARGS_((ClientData clientdata,
	XEvent *eventPtr));
static void DrawLabel _ANSI_ARGS_((Tabset * setPtr, Tab * tabPtr,
	Drawable drawable));
static void DisplayFolder _ANSI_ARGS_((Tabset * setPtr, Tab * tabPtr,
	Drawable drawable));
static void DisplayTabset _ANSI_ARGS_((ClientData clientData));
static void DisplayTearoff _ANSI_ARGS_((ClientData clientData));
static void TabsetInstDeletedCmd _ANSI_ARGS_((ClientData clientdata));
static int TabsetInstCmd _ANSI_ARGS_((ClientData clientdata,
	Tcl_Interp *interp, int argc, char **argv));
static void GetWindowRegion _ANSI_ARGS_((Tab * tabPtr, Tk_Window parent,
	int tearOff, XRectangle *rectPtr));
static void ArrangeWindow _ANSI_ARGS_((Tk_Window tkwin, XRectangle *rectPtr,
	int force));
static void EventuallyRedraw _ANSI_ARGS_((Tabset * setPtr));
static void EventuallyRedrawTearoff _ANSI_ARGS_((Tab * tabPtr));
static void ComputeLayout _ANSI_ARGS_((Tabset * setPtr));
static void DrawOuterBorders _ANSI_ARGS_((Tabset * setPtr, Drawable drawable));

#ifdef __STDC__
static Tk_ImageChangedProc ImageChangedProc;
static Blt_TileChangedProc TileChangedProc;
static BindTagProc GetTags;
static BindPickProc PickTab;
static Tcl_IdleProc AdoptWindow;
static Tcl_CmdProc TabsetCmd;
#endif /* __STDC__ */

/*
 *----------------------------------------------------------------------
 *
 * WorldToScreen --
 *
 *	Converts world coordinates to screen coordinates. Note that
 *	the world view is always tabs up.
 *
 * Results:
 *	The screen coordinates are returned via *xScrnPtr and *yScrnPtr.
 *
 *----------------------------------------------------------------------
 */
static void
WorldToScreen(setPtr, x, y, xScrnPtr, yScrnPtr)
    Tabset *setPtr;
    int x, y;
    int *xScrnPtr, *yScrnPtr;
{
    int sx, sy;

    sx = sy = 0;		/* Suppress compiler warning. */
    /* Translate X-Y to screen coordinates */
    x += (setPtr->inset - setPtr->scrollOffset);
    y += setPtr->yPad + setPtr->inset;
    switch (setPtr->side) {
    case SIDE_TOP:
	sx = x, sy = y;		/* Do nothing */
	break;
    case SIDE_RIGHT:
	sx = Tk_Width(setPtr->tkwin) - y - 1;
	sy = x;
	break;
    case SIDE_LEFT:
	sx = y, sy = x;		/* Flip coordinates */
	break;
    case SIDE_BOTTOM:
	sx = x;
	sy = Tk_Height(setPtr->tkwin) - y - 1;
	break;
    }
    *xScrnPtr = sx;
    *yScrnPtr = sy;
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
EventuallyRedraw(setPtr)
    Tabset *setPtr;
{
    if ((setPtr->tkwin != NULL) && !(setPtr->flags & TABSET_REDRAW)) {
	setPtr->flags |= TABSET_REDRAW;
	Tk_DoWhenIdle(DisplayTabset, (ClientData)setPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * EventuallyRedrawTearoff --
 *
 *	Queues a request to redraw the tearoff at the next idle point.
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
EventuallyRedrawTearoff(tabPtr)
    Tab *tabPtr;
{
    if ((tabPtr->tkwin != NULL) && !(tabPtr->flags & TAB_REDRAW)) {
	tabPtr->flags |= TAB_REDRAW;
	Tk_DoWhenIdle(DisplayTearoff, (ClientData)tabPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ImageChangedProc
 *
 *	This routine is called whenever an image displayed in a tab
 *	changes.  In this case, we assume that everything will change
 *	and queue a request to re-layout and redraw the entire tabset.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
/* ARGSUSED */
static void
ImageChangedProc(clientData, x, y, width, height, imageWidth, imageHeight)
    ClientData clientData;
    int x, y, width, height;	/* Not used */
    int imageWidth, imageHeight;/* Not used */
{
    Tabset *setPtr = (Tabset *) clientData;

    setPtr->flags |= (TABSET_LAYOUT | TABSET_SCROLL);
    EventuallyRedraw(setPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * GetImage --
 *
 *	This is a wrapper procedure for Tk_GetImage. The problem is
 *	that if the same image is used repeatedly in the same widget,
 *	the separate instances are saved in a linked list.  This makes
 *	it especially slow to destroy the widget.  As a workaround,
 *	this routine hashes the image and maintains a reference count
 *	for it.
 *
 * Results:
 *	Returns a pointer to the new image.
 *
 *----------------------------------------------------------------------
 */
static TabImage
GetImage(setPtr, interp, tkwin, name)
    Tabset *setPtr;
    Tcl_Interp *interp;
    Tk_Window tkwin;
    char *name;
{
    struct TabImage *imagePtr;
    int isNew;
    Tcl_HashEntry *hPtr;

    hPtr = Tcl_CreateHashEntry(&(setPtr->imageTable), (char *)name, &isNew);
    if (isNew) {
	Tk_Image tkImage;
	int width, height;

	tkImage = Tk_GetImage(interp, tkwin, name, ImageChangedProc,
	    (ClientData)setPtr);
	if (tkImage == NULL) {
	    Tcl_DeleteHashEntry(hPtr);
	    return NULL;
	}
	Tk_SizeOfImage(tkImage, &width, &height);
	imagePtr = (struct TabImage *)malloc(sizeof(struct TabImage));
	imagePtr->tkImage = tkImage;
	imagePtr->hashPtr = hPtr;
	imagePtr->refCount = 1;
	imagePtr->width = width;
	imagePtr->height = height;
	Tcl_SetHashValue(hPtr, (ClientData)imagePtr);
    } else {
	imagePtr = (struct TabImage *)Tcl_GetHashValue(hPtr);
	imagePtr->refCount++;
    }
    return imagePtr;
}

/*
 *----------------------------------------------------------------------
 *
 * FreeImage --
 *
 *	Releases the image if it's not being used anymore by this
 *	widget.  Note there may be several uses of the same image
 *	by many tabs.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	The reference count is decremented and the image is freed
 *	is it's not being used anymore.
 *
 *----------------------------------------------------------------------
 */
static void
FreeImage(imagePtr)
    struct TabImage *imagePtr;
{
    imagePtr->refCount--;
    if (imagePtr->refCount == 0) {
	Tcl_DeleteHashEntry(imagePtr->hashPtr);
	Tk_FreeImage(imagePtr->tkImage);
	free((char *)imagePtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * StringToImage --
 *
 *	Converts an image name into a Tk image token.
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
StringToImage(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* Contains a pointer to the tabset containing
				 * this image. */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* Window associated with the tabset. */
    char *string;		/* String representation */
    char *widgRec;		/* Widget record */
    int offset;			/* Offset to field in structure */
{
    Tabset *setPtr = *(Tabset **) clientData;
    TabImage *imagePtr = (TabImage *) (widgRec + offset);
    TabImage image;

    image = NULL;
    if ((string != NULL) && (*string != '\0')) {
	image = GetImage(setPtr, interp, tkwin, string);
	if (image == NULL) {
	    return TCL_ERROR;
	}
    }
    if (*imagePtr != NULL) {
	FreeImage(*imagePtr);
    }
    *imagePtr = image;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ImageToString --
 *
 *	Converts the Tk image back to its string representation (i.e.
 *	its name).
 *
 * Results:
 *	The name of the image is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
ImageToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* Pointer to tabset containing image. */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Widget record */
    int offset;			/* Offset of field in record */
    Tcl_FreeProc **freeProcPtr;	/* Memory deallocation scheme to use */
{
    Tabset *setPtr = *(Tabset **) clientData;
    TabImage *imagePtr = (TabImage *) (widgRec + offset);

    if (*imagePtr == NULL) {
	return "";
    }
    return Tcl_GetHashKey(&(setPtr->imageTable), (*imagePtr)->hashPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * StringToWindow --
 *
 *	Converts a window name into Tk window.
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
StringToWindow(clientData, interp, parent, string, widgRec, offset)
    ClientData clientData;	/* Not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window parent;		/* Parent window */
    char *string;		/* String representation. */
    char *widgRec;		/* Widget record */
    int offset;			/* Offset to field in structure */
{
    Tab *tabPtr = (Tab *) widgRec;
    Tk_Window *tkwinPtr = (Tk_Window *)(widgRec + offset);
    Tk_Window old, tkwin;
    Tabset *setPtr;

    old = *tkwinPtr;
    tkwin = NULL;
    setPtr = tabPtr->setPtr;
    if ((string != NULL) && (*string != '\0')) {
	tkwin = Tk_NameToWindow(interp, string, parent);
	if (tkwin == NULL) {
	    return TCL_ERROR;
	}
	if (tkwin == old) {
	    return TCL_OK;
	}
	/*
	 * Allow only windows that are children of the tabset window to be
	 * embedded into the page.  This way we can make assumptions about
	 * the window based upon its parent; either it's the tabset window
	 * or it has been torn off.
	 */
	parent = Tk_Parent(tkwin);
	if (parent != setPtr->tkwin) {
	    Tcl_AppendResult(interp, "can't manage \"", Tk_PathName(tkwin),
		"\" in tabset \"", Tk_PathName(setPtr->tkwin), "\"",
		(char *)NULL);
	    return TCL_ERROR;
	}
	Tk_ManageGeometry(tkwin, &tabMgrInfo, (ClientData)tabPtr);
	Tk_CreateEventHandler(tkwin, StructureNotifyMask, SlaveEventProc,
	    (ClientData)tabPtr);

	/*
	 * We need to make the window to exist immediately.  If the
	 * window is torn off (placed into another container window),
	 * the timing between the container and the its new child
	 * (this window) gets tricky.  This should work for Tk 4.2.
	 */
	Tk_MakeWindowExist(tkwin);
    }
    if (old != NULL) {
	if (tabPtr->container != NULL) {
	    Tk_EventuallyFree((ClientData)tabPtr, DestroyTearoff);
	}
	Tk_DeleteEventHandler(old, StructureNotifyMask, SlaveEventProc,
	    (ClientData)tabPtr);
	Tk_ManageGeometry(old, (Tk_GeomMgr *) NULL, (ClientData)tabPtr);
	Tk_UnmapWindow(old);
    }
    *tkwinPtr = tkwin;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * WindowToString --
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
WindowToString(clientData, parent, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* Not used */
    Tk_Window parent;		/* Not used */
    char *widgRec;		/* Widget record */
    int offset;			/* Offset of field in record */
    Tcl_FreeProc **freeProcPtr;	/* Memory deallocation scheme to use */
{
    Tk_Window tkwin = *(Tk_Window *)(widgRec + offset);

    if (tkwin == NULL) {
	return "";
    }
    return Tk_PathName(tkwin);
}

/*
 *----------------------------------------------------------------------
 *
 * StringToSide --
 *
 *	Converts "left", "right", "top", "bottom", into a numeric token
 *	designating the side of the tabset which to display tabs.
 *
 * Results:
 *	If the string is successfully converted, TCL_OK is returned.
 *	Otherwise, TCL_ERROR is returned and an error message is left
 *	in interpreter's result field.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED */
static int
StringToSide(clientData, interp, parent, string, widgRec, offset)
    ClientData clientData;	/* Not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window parent;		/* Parent window */
    char *string;		/* Option value string */
    char *widgRec;		/* Widget record */
    int offset;			/* offset to field in structure */
{
    int *sidePtr = (int *)(widgRec + offset);
    char c;
    unsigned int length;

    c = string[0];
    length = strlen(string);
    if ((c == 'l') && (strncmp(string, "left", length) == 0)) {
	*sidePtr = SIDE_LEFT;
    } else if ((c == 'r') && (strncmp(string, "right", length) == 0)) {
	*sidePtr = SIDE_RIGHT;
    } else if ((c == 't') && (strncmp(string, "top", length) == 0)) {
	*sidePtr = SIDE_TOP;
    } else if ((c == 'b') && (strncmp(string, "bottom", length) == 0)) {
	*sidePtr = SIDE_BOTTOM;
    } else {
	Tcl_AppendResult(interp, "bad side \"", string,
	    "\": should be left, right, top, or bottom", (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * SideToString --
 *
 *	Converts the window into its string representation (its name).
 *
 * Results:
 *	The name of the window is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
SideToString(clientData, parent, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window parent;		/* not used */
    char *widgRec;		/* Widget record */
    int offset;			/* offset of windows array in record */
    Tcl_FreeProc **freeProcPtr;	/* Memory deallocation scheme to use */
{
    int side = *(int *)(widgRec + offset);

    switch (side) {
    case SIDE_LEFT:
	return "left";
    case SIDE_RIGHT:
	return "right";
    case SIDE_BOTTOM:
	return "bottom";
    case SIDE_TOP:
	return "top";
    }
    return "unknown side value";
}


static int
WorldY(tabPtr)
    Tab *tabPtr;
{
    int tier;

    tier = tabPtr->setPtr->numTiers - tabPtr->tier;
    return tier * tabPtr->setPtr->tabHeight;
}

/*
 * ----------------------------------------------------------------------
 *
 * RenumberTiers --
 *
 *	In multi-tier mode, we need to find the start of the tier
 *	containing the newly selected tab.
 *
 *	Tiers are draw from the last tier to the first, so that
 *	the the lower-tiered tabs will partially cover the bottoms
 *	of tab directly above it.  This simplifies the drawing of
 *	tabs because we don't worry how tabs are clipped by their
 *	neighbors.
 *
 *	In addition, tabs are re-marked with the correct tier number.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Renumbering the tab's tier will change the vertical placement
 *	of the tab (i.e. shift tiers).
 *
 * ----------------------------------------------------------------------
 */
static void
RenumberTiers(setPtr, tabPtr)
    Tabset *setPtr;
    Tab *tabPtr;
{
    int tier;
    Tab *prevPtr;
    register Blt_ListItem item;

    setPtr->focusPtr = setPtr->selectPtr = tabPtr;
    Blt_SetFocusItem(setPtr->bindTable, setPtr->focusPtr);

    tier = tabPtr->tier;
    for (item = Blt_ListPrevItem(tabPtr->item); item != NULL; /*empty*/ ) {
	prevPtr = (Tab *) Blt_ListGetValue(item);
	if ((prevPtr == NULL) || (prevPtr->tier != tier)) {
	    break;
	}
	tabPtr = prevPtr;
	item = Blt_ListPrevItem(item);
    }
    setPtr->startPtr = tabPtr;
    for (item = Blt_ListFirstItem(&(setPtr->tabList)); item != NULL;
	item = Blt_ListNextItem(item)) {
	tabPtr = (Tab *) Blt_ListGetValue(item);
	tabPtr->tier = (tabPtr->tier - tier + 1);
	if (tabPtr->tier < 1) {
	    tabPtr->tier += setPtr->numTiers;
	}
	tabPtr->worldY = WorldY(tabPtr);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * FindTab --
 *
 *	Searches for a tab based upon its name.
 *
 * Results:
 *	A standard Tcl result.  Note that no error message is generated
 *	the tab can't be found.
 *
 * Side Effects:
 *	If the tab is found, *tabPtrPtr will contain the pointer to the
 *	tab structure.
 *
 * ----------------------------------------------------------------------
 */
static int
FindTab(setPtr, name, tabPtrPtr)
    Tabset *setPtr;
    char *name;
    Tab **tabPtrPtr;
{
    Tk_Uid nameId;
    Tcl_HashEntry *hPtr;

    *tabPtrPtr = NULL;
    nameId = Blt_FindUid(name);
    if (nameId == NULL) {
	return TCL_ERROR;	/* If a Tk_Uid doesn't already exist for that
				 * string, then the string can't possibly
				 * represent a tab. */
    }
    hPtr = Tcl_FindHashEntry(&(setPtr->tabTable), nameId);
    if (hPtr == NULL) {
	return TCL_ERROR;
    }
    *tabPtrPtr = (Tab *) Tcl_GetHashValue(hPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * PickTab --
 *
 *	Searches the tab located within the given screen X-Y coordinates
 *	in the viewport.  Note that tabs overlap slightly, so that its
 *	important to search from the innermost tier out.
 *
 * Results:
 *	Returns the pointer to the tab.  If the pointer isn't contained
 *	by any tab, NULL is returned.
 *
 *----------------------------------------------------------------------
 */
static ClientData
PickTab(clientData, x, y)
    ClientData clientData;
    int x, y;			/* Screen coordinates to test. */
{
    Tabset *setPtr = (Tabset *) clientData;	/* Tabset widget record. */
    Tab *tabPtr;
    Blt_ListItem item;

    for (item = Blt_ListFirstItem(&(setPtr->tabList)); item != NULL;
	item = Blt_ListNextItem(item)) {
	tabPtr = (Tab *) Blt_ListGetValue(item);
	if (!(tabPtr->flags & TAB_VISIBLE)) {
	    continue;
	}
	if ((x >= tabPtr->scrnX) && (y >= tabPtr->scrnY) &&
	    (x < (tabPtr->scrnX + tabPtr->scrnWidth)) &&
	    (y < (tabPtr->scrnY + tabPtr->scrnHeight))) {
	    return (ClientData) tabPtr;
	}
    }
    return NULL;
}

static Tab *
TabLeft(setPtr, tabPtr)
    Tabset *setPtr;
    Tab *tabPtr;
{
    Blt_ListItem item;

    item = Blt_ListPrevItem(tabPtr->item);
    if (item != NULL) {
	Tab *newPtr;

	newPtr = (Tab *) Blt_ListGetValue(item);
	/* Move only if the next tab is on another tier. */
	if (newPtr->tier == tabPtr->tier) {
	    tabPtr = newPtr;
	}
    }
    return tabPtr;
}

static Tab *
TabRight(setPtr, tabPtr)
    Tabset *setPtr;
    Tab *tabPtr;
{
    Blt_ListItem item;

    item = Blt_ListNextItem(tabPtr->item);
    if (item != NULL) {
	Tab *newPtr;

	newPtr = (Tab *) Blt_ListGetValue(item);
	/* Move only if the next tab is on another tier. */
	if (newPtr->tier == tabPtr->tier) {
	    tabPtr = newPtr;
	}
    }
    return tabPtr;
}

static Tab *
TabUp(setPtr)
    Tabset *setPtr;
{
    int x, y;
    int worldX, worldY;
    Tab *tabPtr;

    tabPtr = setPtr->focusPtr;
    worldX = tabPtr->worldX + (tabPtr->worldWidth / 2);
    worldY = tabPtr->worldY - (setPtr->tabHeight / 2);
    WorldToScreen(setPtr, worldX, worldY, &x, &y);

    tabPtr = (Tab *) PickTab((ClientData)setPtr, x, y);
    if (tabPtr == NULL) {
	/*
	 * We might have inadvertly picked the gap between two tabs,
	 * so if the first pick fails, try again a little to the left.
	 */
	WorldToScreen(setPtr, worldX + setPtr->gap, worldY, &x, &y);
	tabPtr = (Tab *) PickTab((ClientData)setPtr, x, y);
    }
    if ((tabPtr == NULL) &&
	(setPtr->focusPtr->tier < (setPtr->numTiers - 1))) {
	WorldToScreen(setPtr, worldX, worldY - setPtr->tabHeight, &x, &y);
	tabPtr = (Tab *) PickTab((ClientData)setPtr, x, y);
    }
    if (tabPtr == NULL) {
	tabPtr = setPtr->focusPtr;
    }
    return tabPtr;
}

static Tab *
TabDown(setPtr)
    Tabset *setPtr;
{
    int x, y;
    int worldX, worldY;
    Tab *tabPtr;

    tabPtr = setPtr->focusPtr;
    worldX = tabPtr->worldX + (tabPtr->worldWidth / 2);
    worldY = tabPtr->worldY + (3 * setPtr->tabHeight) / 2;
    WorldToScreen(setPtr, worldX, worldY, &x, &y);
    tabPtr = (Tab *) PickTab((ClientData)setPtr, x, y);
    if (tabPtr == NULL) {
	/*
	 * We might have inadvertly picked the gap between two tabs,
	 * so if the first pick fails, try again a little to the left.
	 */
	WorldToScreen(setPtr, worldX - setPtr->gap, worldY, &x, &y);
	tabPtr = (Tab *) PickTab((ClientData)setPtr, x, y);
    }
    if ((tabPtr == NULL) && (setPtr->focusPtr->tier > 2)) {
	WorldToScreen(setPtr, worldX, worldY + setPtr->tabHeight, &x, &y);
	tabPtr = (Tab *) PickTab((ClientData)setPtr, x, y);
    }
    if (tabPtr == NULL) {
	tabPtr = setPtr->focusPtr;
    }
    return tabPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * GetIndex --
 *
 *	Converts a string representing a tab index into a tab pointer.
 *	The index may be in one of the following forms:
 *
 *	 number		Tab at position in the list of tabs.
 *	 name		Name of the tab.
 *	 @x,y		Tab closest to the specified X-Y screen coordinates.
 *	 "tabActive"	Tab mouse is located over.
 *	 "tabFocus"	Tab is the widget's focus.
 *	 "tabSelect"	Currently selected tab.
 *	 "tabRight"	Next tab from the focus tab.
 *	 "tabLeft"	Previous tab from the focus tab.
 *	 "tabUp"	Next tab from the focus tab.
 *	 "tabDown"	Previous tab from the focus tab.
 *	 "tabEnd"	Last tab in list.
 *
 * Results:
 *	If the string is successfully converted, TCL_OK is returned.
 *	The pointer to the node is returned via tabPtrPtr.
 *	Otherwise, TCL_ERROR is returned and an error message is left
 *	in interpreter's result field.
 *
 *----------------------------------------------------------------------
 */
static int
GetIndex(setPtr, string, tabPtrPtr)
    Tabset *setPtr;
    char *string;
    Tab **tabPtrPtr;
{
    Tab *tabPtr;
    Blt_ListItem item;
    int position;
    char c;

    c = string[0];
    item = NULL;
    tabPtr = NULL;
    if (setPtr->focusPtr == NULL) {
	setPtr->focusPtr = setPtr->selectPtr;
	Blt_SetFocusItem(setPtr->bindTable, setPtr->focusPtr);
    }
    if ((isdigit(UCHAR(c))) &&
	(Tcl_GetInt(setPtr->interp, string, &position) == TCL_OK)) {
	item = Blt_ListFindNthItem(&(setPtr->tabList), position, 1);
	if (item != NULL) {
	    tabPtr = (Tab *) Blt_ListGetValue(item);
	}
    } else if ((c == 't') && (string[1] == 'a') && (string[2] == 'b')) {
	unsigned int length;
	char *suffix;

	suffix = string + 3;
	c = suffix[0];
	length = strlen(suffix);

	/*
	 * "tabActive"	Tab mouse is located over.
	 * "tabFocus"	Tab is the widget's focus.
	 * "tabSelect"	Currently selected tab.
	 * "tabRight"	Next tab from the focus tab.
	 * "tabLeft"	Previous tab from the focus tab.
	 * "tabUp"	Next tab from the focus tab.
	 * "tabDown"	Previous tab from the focus tab.
	 * "tabEnd"	Last tab in list.
	 */
	if ((c == 'A') && (length > 0) &&
	    (strncmp(suffix, "Active", length) == 0)) {
	    tabPtr = setPtr->activePtr;
	} else if ((c == 'S') && (length > 0) &&
	    (strncmp(suffix, "Select", length) == 0)) {
	    tabPtr = (Tab *) Blt_GetCurrentItem(setPtr->bindTable);
	} else if ((c == 'F') && (length > 0) &&
	    (strncmp(suffix, "Focus", length) == 0)) {
	    tabPtr = setPtr->focusPtr;
	} else if ((c == 'U') && (length > 0) &&
	    (strncmp(suffix, "Up", length) == 0)) {
	    switch (setPtr->side) {
	    case SIDE_LEFT:
	    case SIDE_RIGHT:
		tabPtr = TabLeft(setPtr, setPtr->focusPtr);
		break;

	    case SIDE_BOTTOM:
		tabPtr = TabDown(setPtr);
		break;

	    case SIDE_TOP:
		tabPtr = TabUp(setPtr);
		break;
	    }
	} else if ((c == 'D') && (length > 0) &&
	    (strncmp(suffix, "Down", length) == 0)) {
	    switch (setPtr->side) {
	    case SIDE_LEFT:
	    case SIDE_RIGHT:
		tabPtr = TabRight(setPtr, setPtr->focusPtr);
		break;

	    case SIDE_BOTTOM:
		tabPtr = TabUp(setPtr);
		break;

	    case SIDE_TOP:
		tabPtr = TabDown(setPtr);
		break;
	    }
	} else if ((c == 'L') && (length > 0) &&
	    (strncmp(suffix, "Left", length) == 0)) {
	    switch (setPtr->side) {
	    case SIDE_LEFT:
		tabPtr = TabUp(setPtr);
		break;

	    case SIDE_RIGHT:
		tabPtr = TabDown(setPtr);
		break;

	    case SIDE_BOTTOM:
	    case SIDE_TOP:
		tabPtr = TabLeft(setPtr, setPtr->focusPtr);
		break;
	    }
	} else if ((c == 'R') && (length > 0) &&
	    (strncmp(suffix, "Right", length) == 0)) {
	    switch (setPtr->side) {
	    case SIDE_LEFT:
		tabPtr = TabDown(setPtr);
		break;

	    case SIDE_RIGHT:
		tabPtr = TabUp(setPtr);
		break;

	    case SIDE_BOTTOM:
	    case SIDE_TOP:
		tabPtr = TabRight(setPtr, setPtr->focusPtr);
		break;
	    }
	} else if ((c == 'E') && (length > 0) &&
	    (strncmp(suffix, "End", length) == 0)) {
	    item = Blt_ListLastItem(&(setPtr->tabList));
	    tabPtr = (Tab *) Blt_ListGetValue(item);

	} else {
	    goto checkName;
	}
    } else if (c == '@') {
	int x, y;

	if (Blt_GetXYPosition(setPtr->interp, setPtr->tkwin, string,
		&x, &y) != TCL_OK) {
	    return TCL_ERROR;
	}
	tabPtr = (Tab *) PickTab((ClientData)setPtr, x, y);
    } else {
      checkName:
	FindTab(setPtr, string, &tabPtr);
    }
    *tabPtrPtr = tabPtr;
    Tcl_ResetResult(setPtr->interp);
    return TCL_OK;
}

static int
TabIndex(setPtr, string, tabPtrPtr)
    Tabset *setPtr;
    char *string;
    Tab **tabPtrPtr;
{
    if (GetIndex(setPtr, string, tabPtrPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (*tabPtrPtr == NULL) {
	Tcl_AppendResult(setPtr->interp, "can't find tab \"", string,
	    "\" in \"", Tk_PathName(setPtr->tkwin), "\"", (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}


static Tab *
NextOrLastTab(tabPtr)
    Tab *tabPtr;
{
    Blt_ListItem item;

    item = Blt_ListNextItem(tabPtr->item);
    if (item == NULL) {
	item = Blt_ListPrevItem(tabPtr->item);
    }
    if (item == NULL) {
	return NULL;
    }
    return (Tab *) Blt_ListGetValue(item);
}

/*
 * --------------------------------------------------------------
 *
 * SlaveEventProc --
 *
 * 	This procedure is invoked by the Tk dispatcher for various
 * 	events on embedded widgets contained in the tabset.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	When an embedded window gets deleted, internal structures get
 *	cleaned up.  When it gets resized, the tabset is redisplayed.
 *
 * --------------------------------------------------------------
 */
static void
SlaveEventProc(clientData, eventPtr)
    ClientData clientData;	/* Information about the tab window. */
    XEvent *eventPtr;		/* Information about event. */
{
    Tab *tabPtr = (Tab *) clientData;

    if ((tabPtr == NULL) || (tabPtr->tkwin == NULL)) {
	return;
    }
    switch (eventPtr->type) {
    case ConfigureNotify:
	/*
	 * If the window's requested size changes, redraw the window.
	 * But only if it's currently the selected page.
	 */
	if ((tabPtr->container == NULL) && (Tk_IsMapped(tabPtr->tkwin)) &&
	    (tabPtr->setPtr->selectPtr == tabPtr)) {
	    EventuallyRedraw(tabPtr->setPtr);
	}
	break;

    case DestroyNotify:
	/*
	 * Mark the tab as deleted by dereferencing the Tk window
	 * pointer. Redraw the window only if the tab is currently
	 * visible.
	 */
	if ((Tk_IsMapped(tabPtr->tkwin)) &&
	    (tabPtr->setPtr->selectPtr == tabPtr)) {
	    EventuallyRedraw(tabPtr->setPtr);
	}
	Tk_DeleteEventHandler(tabPtr->tkwin, StructureNotifyMask,
	    SlaveEventProc, (ClientData)tabPtr);
	tabPtr->tkwin = NULL;
	break;

    }
}

/*
 * ----------------------------------------------------------------------
 *
 * SlaveCustodyProc --
 *
 *	This procedure is invoked when a tab window has been
 *	stolen by another geometry manager.  The information and
 *	memory associated with the tab window is released.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Arranges for the widget formerly associated with the tab
 *	window to have its layout re-computed and arranged at the
 *	next idle point.
 *
 * ---------------------------------------------------------------------
 */
 /* ARGSUSED */
static void
SlaveCustodyProc(clientData, tkwin)
    ClientData clientData;	/* Information about the former tab window. */
    Tk_Window tkwin;		/* Not used. */
{
    Tab *tabPtr = (Tab *) clientData;
    Tabset *setPtr;

    if ((tabPtr == NULL) || (tabPtr->tkwin == NULL)) {
	return;
    }
    setPtr = tabPtr->setPtr;
    if (tabPtr->container != NULL) {
	Tk_EventuallyFree((ClientData)tabPtr, DestroyTearoff);
    }
    /*
     * Mark the tab as deleted by dereferencing the Tk window
     * pointer. Redraw the window only if the tab is currently
     * visible.
     */
    if (tabPtr->tkwin != NULL) {
	if (Tk_IsMapped(tabPtr->tkwin) && (setPtr->selectPtr == tabPtr)) {
	    EventuallyRedraw(setPtr);
	}
	Tk_DeleteEventHandler(tabPtr->tkwin, StructureNotifyMask,
	    SlaveEventProc, (ClientData)tabPtr);
	tabPtr->tkwin = NULL;
    }
}

/*
 * -------------------------------------------------------------------------
 *
 * SlaveGeometryProc --
 *
 *	This procedure is invoked by Tk_GeometryRequest for tab
 *	windows managed by the widget.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Arranges for tkwin, and all its managed siblings, to be
 *	repacked and drawn at the next idle point.
 *
 * ------------------------------------------------------------------------
 */
 /* ARGSUSED */
static void
SlaveGeometryProc(clientData, tkwin)
    ClientData clientData;	/* Information about window that got new
			         * preferred geometry.  */
    Tk_Window tkwin;		/* Other Tk-related information about the
			         * window. */
{
    Tab *tabPtr = (Tab *) clientData;

    if ((tabPtr == NULL) || (tabPtr->tkwin == NULL)) {
	fprintf(stderr, "%s: line %d \"tkwin is null\"", __FILE__, __LINE__);
	return;
    }
    EventuallyRedraw(tabPtr->setPtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * DestroyTab --
 *
 * ----------------------------------------------------------------------
 */
static void
DestroyTab(setPtr, tabPtr)
    Tabset *setPtr;
    Tab *tabPtr;
{
    Tcl_HashEntry *hPtr;

    if (tabPtr->flags & TAB_REDRAW) {
	Tk_CancelIdleCall(DisplayTearoff, (ClientData)tabPtr);
    }
    if (tabPtr->container != NULL) {
	Tk_DestroyWindow(tabPtr->container);
    }
    if (tabPtr->tkwin != NULL) {
	if (Tk_IsMapped(tabPtr->tkwin)) {
	    Tk_UnmapWindow(tabPtr->tkwin);
	}
    }
    if (tabPtr == setPtr->activePtr) {
	setPtr->activePtr = NULL;
    }
    if (tabPtr == setPtr->selectPtr) {
	setPtr->selectPtr = NextOrLastTab(tabPtr);
    }
    if (tabPtr == setPtr->focusPtr) {
	setPtr->focusPtr = setPtr->selectPtr;
	Blt_SetFocusItem(setPtr->bindTable, setPtr->focusPtr);
    }
    if (tabPtr == setPtr->startPtr) {
	setPtr->startPtr = NULL;
    }
    Tk_FreeOptions(tabConfigSpecs, (char *)tabPtr, setPtr->display, 0);
    if (tabPtr->text != NULL) {
	Blt_FreeUid(tabPtr->text);
    }
    hPtr = Tcl_FindHashEntry(&(setPtr->tabTable), tabPtr->nameId);
    assert(hPtr);
    Tcl_DeleteHashEntry(hPtr);

    if (tabPtr->image != NULL) {
	FreeImage(tabPtr->image);
    }
    if (tabPtr->nameId != NULL) {
	Blt_FreeUid(tabPtr->nameId);
    }
    if (tabPtr->textGC != NULL) {
	Tk_FreeGC(setPtr->display, tabPtr->textGC);
    }
    if (tabPtr->backGC != NULL) {
	Tk_FreeGC(setPtr->display, tabPtr->backGC);
    }
    if (tabPtr->tileGC != NULL) {
	Tk_FreeGC(setPtr->display, tabPtr->tileGC);
    }
    if (tabPtr->command != NULL) {
	Blt_FreeUid(tabPtr->command);
    }
    if (tabPtr->item != NULL) {
	Blt_ListDeleteItem(tabPtr->item);
    }
    if (tabPtr->tags != NULL) {
	Blt_FreeUid(tabPtr->tags);
    }
    Blt_DeleteAllBindings(setPtr->bindTable, (ClientData)tabPtr);
    free((char *)tabPtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * CreateTab --
 *
 *	Creates a new tab structure.  A tab contains information about
 *	the state of the tab and its embedded window.
 *
 * Results:
 *	Returns a pointer to the new tab structure.
 *
 * ----------------------------------------------------------------------
 */
static Tab *
CreateTab(setPtr, name)
    Tabset *setPtr;
    char *name;
{
    Tab *tabPtr;
    Tcl_HashEntry *hPtr;
    int isNew;

    tabPtr = (Tab *) calloc(1, sizeof(Tab));
    assert(tabPtr);
    tabPtr->setPtr = setPtr;
    tabPtr->nameId = Blt_GetUid(name);
    tabPtr->text = Blt_GetUid(name);
    tabPtr->fill = FILL_NONE;
    tabPtr->anchor = TK_ANCHOR_CENTER;
    tabPtr->container = NULL;
    tabPtr->state = STATE_NORMAL;
    tabPtr->index = -1;
    hPtr = Tcl_CreateHashEntry(&(setPtr->tabTable), tabPtr->nameId, &isNew);
    Tcl_SetHashValue(hPtr, (ClientData)tabPtr);
    return tabPtr;
}

/*
 *--------------------------------------------------------------
 *
 * GetPosition --
 *
 *	Convert a string representing a position in the list of children
 *	to its numeric value.  A position index can be in one of
 *	the following forms.
 *
 * 	  number		- number of the item in the tab list, indexed
 *			  from zero.
 *	  "end"		- last position in the tab list.
 *
 * Results:
 *	A standard Tcl result.  If "string" is a valid index, then
 *	*indexPtr is filled with the corresponding numeric index.
 *	Otherwise an error message is left in interp->result.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */
static int
GetPosition(interp, string, indexPtr)
    Tcl_Interp *interp;		/* Interpreter to report results back to. */
    char *string;		/* Numerical index into the tab list or "end"
				 * to refer to last element. */
    int *indexPtr;		/* Where to store converted relief. */
{
    if ((string[0] == 'e') && (strcmp(string, "end") == 0)) {
	*indexPtr = POSITION_END;	/* Indicates last position in tab list. */
    } else {
	int position;

	if (Tcl_GetInt(interp, string, &position) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (position < 0) {
	    Tcl_AppendResult(interp, "bad position \"", string, "\"",
		(char *)NULL);
	    return TCL_ERROR;
	}
	*indexPtr = position;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * TileChangedProc
 *
 *	Stub for image change notifications.  Since we immediately draw
 *	the image into a pixmap, we don't really care about image changes.
 *
 *	It would be better if Tk checked for NULL proc pointers.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
TileChangedProc(clientData, tile)
    ClientData clientData;
    Blt_Tile tile;
{
    Tabset *setPtr = (Tabset *) clientData;

    if (setPtr->tkwin != NULL) {
	GC newGC;
	Pixmap pixmap;

	newGC = NULL;
	pixmap = Blt_PixmapOfTile(tile);
	if (pixmap != None) {
	    unsigned long gcMask;
	    XGCValues gcValues;

	    gcMask = (GCTile | GCFillStyle);
	    gcValues.fill_style = FillTiled;
	    gcValues.tile = pixmap;
	    newGC = Tk_GetGC(setPtr->tkwin, gcMask, &gcValues);
	}
	if (setPtr->tileGC != NULL) {
	    Tk_FreeGC(setPtr->display, setPtr->tileGC);
	}
	setPtr->tileGC = newGC;
	EventuallyRedraw(setPtr);
    }
}

static int
ConfigureTab(setPtr, tabPtr)
    Tabset *setPtr;
    Tab *tabPtr;
{
    GC newGC;
    XGCValues gcValues;
    unsigned long gcMask;
    int labelWidth, labelHeight;
    Tk_Font font;
    Tk_3DBorder border;

    font = GETATTR(tabPtr, font);
    labelWidth = labelHeight = 0;
    if (tabPtr->text != NULL) {
	TextAttributes attr;

	Blt_InitTextAttributes(&attr);
	attr.font = font;
	attr.shadow.offset = tabPtr->shadow.offset;
	Blt_GetTextExtents(&attr, tabPtr->text, &labelWidth, &labelHeight);
	Blt_GetBoundingBox(labelWidth, labelHeight, setPtr->defAttrs.rotate,
	    &labelWidth, &labelHeight, (XPoint *)NULL);
    }
    tabPtr->textWidth = (short int)labelWidth;
    tabPtr->textHeight = (short int)labelHeight;
    if (tabPtr->image != NULL) {
	if (setPtr->defAttrs.textSide & SIDE_VERTICAL) {
	    labelWidth += ImageWidth(tabPtr->image);
	    labelHeight = MAX(labelHeight, ImageHeight(tabPtr->image));
	} else {
	    labelHeight += ImageHeight(tabPtr->image);
	    labelWidth = MAX(labelWidth, ImageWidth(tabPtr->image));
	}
    }
    labelWidth += PADDING(tabPtr->iPadX);
    labelHeight += PADDING(tabPtr->iPadY);

    tabPtr->labelWidth = labelWidth;
    tabPtr->labelHeight = labelHeight;

    /* Set the size of the tab from the label or image dimensions. */

    if (setPtr->side & SIDE_VERTICAL) {
	tabPtr->worldWidth = labelHeight;
	tabPtr->worldHeight = labelWidth;
    } else {
	tabPtr->worldWidth = labelWidth;
	tabPtr->worldHeight = labelHeight;
    }
    tabPtr->worldWidth += 2 * (setPtr->pageInset + LABEL_PADX);
    tabPtr->worldHeight += 2 * (setPtr->pageInset + LABEL_PADY);

    newGC = NULL;
    if (tabPtr->text != NULL) {
	XColor *colorPtr;

	gcMask = GCForeground | GCFont;
	colorPtr = GETATTR(tabPtr, textColor);
	gcValues.foreground = colorPtr->pixel;
	gcValues.font = Tk_FontId(font);
	newGC = Tk_GetGC(setPtr->tkwin, gcMask, &gcValues);
    }
    if (tabPtr->textGC != NULL) {
	Tk_FreeGC(setPtr->display, tabPtr->textGC);
    }
    tabPtr->textGC = newGC;

    gcMask = GCForeground | GCStipple | GCFillStyle;
    gcValues.fill_style = FillStippled;
    border = GETATTR(tabPtr, border);
    gcValues.foreground = Tk_3DBorderColor(border)->pixel;
    gcValues.stipple = tabPtr->stipple;
    newGC = Tk_GetGC(setPtr->tkwin, gcMask, &gcValues);
    if (tabPtr->backGC != NULL) {
	Tk_FreeGC(setPtr->display, tabPtr->backGC);
    }
    tabPtr->backGC = newGC;
    /*
     * GC for tiled background.
     */
    newGC = NULL;
    if (tabPtr->bgTile != NULL) {
	Pixmap pixmap;

	Blt_SetTileChangedProc(tabPtr->bgTile, TileChangedProc,
	    (ClientData)setPtr);
	pixmap = Blt_PixmapOfTile(tabPtr->bgTile);
	if (pixmap != None) {
	    gcMask = (GCTile | GCFillStyle);
	    gcValues.fill_style = FillTiled;
	    gcValues.tile = pixmap;
	    newGC = Tk_GetGC(setPtr->tkwin, gcMask, &gcValues);
	}
    }
    if (tabPtr->tileGC != NULL) {
	Tk_FreeGC(setPtr->display, tabPtr->tileGC);
    }
    tabPtr->tileGC = newGC;
    if (tabPtr->flags & TAB_VISIBLE) {
	EventuallyRedraw(setPtr);
    }
    return TCL_OK;
}

/*
 * --------------------------------------------------------------
 *
 * TearoffEventProc --
 *
 * 	This procedure is invoked by the Tk dispatcher for various
 * 	events on the tearoff widget.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	When the tearoff gets deleted, internal structures get
 *	cleaned up.  When it gets resized or exposed, it's redisplayed.
 *
 * --------------------------------------------------------------
 */
static void
TearoffEventProc(clientData, eventPtr)
    ClientData clientData;	/* Information about the tab window. */
    XEvent *eventPtr;		/* Information about event. */
{
    Tab *tabPtr = (Tab *) clientData;

    if ((tabPtr == NULL) || (tabPtr->tkwin == NULL) ||
	(tabPtr->container == NULL)) {
	return;
    }
    switch (eventPtr->type) {
    case Expose:
	if (eventPtr->xexpose.count == 0) {
	    EventuallyRedrawTearoff(tabPtr);
	}
	break;

    case ConfigureNotify:
	EventuallyRedrawTearoff(tabPtr);
	break;

    case DestroyNotify:
	if (tabPtr->flags & TAB_REDRAW) {
	    tabPtr->flags &= ~TAB_REDRAW;
	    Tk_CancelIdleCall(DisplayTearoff, clientData);
	}
	Tk_DestroyWindow(tabPtr->container);
	tabPtr->container = NULL;
	break;

    }
}

/*
 * ----------------------------------------------------------------------------
 *
 * GetReqWidth --
 *
 *	Returns the width requested by the embedded tab window and
 *	any requested padding around it. This represents the requested
 *	width of the page.
 *
 * Results:
 *	Returns the requested width of the page.
 *
 * ----------------------------------------------------------------------------
 */
static int
GetReqWidth(tabPtr)
    Tab *tabPtr;
{
    int width;

    if (tabPtr->reqWidth > 0) {
	width = tabPtr->reqWidth;
    } else {
	width = Tk_ReqWidth(tabPtr->tkwin);
    }
    width += PADDING(tabPtr->padX) +
	2 * Tk_Changes(tabPtr->tkwin)->border_width;
    return width;
}

/*
 * ----------------------------------------------------------------------------
 *
 * GetReqHeight --
 *
 *	Returns the height requested by the window and padding around
 *	the window. This represents the requested height of the page.
 *
 * Results:
 *	Returns the requested height of the page.
 *
 * ----------------------------------------------------------------------------
 */
static int
GetReqHeight(tabPtr)
    Tab *tabPtr;
{
    int height;

    if (tabPtr->reqHeight > 0) {
	height = tabPtr->reqHeight;
    } else {
	height = Tk_ReqHeight(tabPtr->tkwin);
    }
    height += PADDING(tabPtr->padY) +
	2 * Tk_Changes(tabPtr->tkwin)->border_width;
    return height;
}

/*
 * ----------------------------------------------------------------------------
 *
 * TranslateAnchor --
 *
 * 	Translate the coordinates of a given bounding box based upon the
 * 	anchor specified.  The anchor indicates where the given xy position
 * 	is in relation to the bounding box.
 *
 *  		nw --- n --- ne
 *  		|            |     x,y ---+
 *  		w   center   e      |     |
 *  		|            |      +-----+
 *  		sw --- s --- se
 *
 * Results:
 *	The translated coordinates of the bounding box are returned.
 *
 * ----------------------------------------------------------------------------
 */
static void
TranslateAnchor(dx, dy, anchor, xPtr, yPtr)
    int dx, dy;			/* Difference between outer and inner regions
				 */
    Tk_Anchor anchor;		/* Direction of the anchor */
    int *xPtr, *yPtr;
{
    int x, y;

    x = y = 0;
    switch (anchor) {
    case TK_ANCHOR_NW:		/* Upper left corner */
	break;
    case TK_ANCHOR_W:		/* Left center */
	y = (dy / 2);
	break;
    case TK_ANCHOR_SW:		/* Lower left corner */
	y = dy;
	break;
    case TK_ANCHOR_N:		/* Top center */
	x = (dx / 2);
	break;
    case TK_ANCHOR_CENTER:	/* Centered */
	x = (dx / 2);
	y = (dy / 2);
	break;
    case TK_ANCHOR_S:		/* Bottom center */
	x = (dx / 2);
	y = dy;
	break;
    case TK_ANCHOR_NE:		/* Upper right corner */
	x = dx;
	break;
    case TK_ANCHOR_E:		/* Right center */
	x = dx;
	y = (dy / 2);
	break;
    case TK_ANCHOR_SE:		/* Lower right corner */
	x = dx;
	y = dy;
	break;
    }
    *xPtr = (*xPtr) + x;
    *yPtr = (*yPtr) + y;
}


static void
GetWindowRegion(tabPtr, parent, tearoff, rectPtr)
    Tab *tabPtr;
    Tk_Window parent;
    int tearoff;
    XRectangle *rectPtr;
{
    int pad;
    Tabset *setPtr;
    int cavityWidth, cavityHeight;
    int winWidth, winHeight;
    int dx, dy;
    int x, y;

    setPtr = tabPtr->setPtr;
    pad = setPtr->inset + setPtr->pageInset;
    if (!tearoff) {
	switch (setPtr->side) {
	case SIDE_RIGHT:
	case SIDE_BOTTOM:
	    x = (short)(setPtr->inset + setPtr->pageInset);
	    y = (short)(setPtr->inset + setPtr->pageInset);
	    break;

	case SIDE_LEFT:
	    x = (short)setPtr->pageTop;
	    y = (short)(setPtr->inset + setPtr->pageInset);
	    break;

	case SIDE_TOP:
	    x = (short)(setPtr->inset + setPtr->pageInset);
	    y = (short)setPtr->pageTop;
	    break;
	}
	if (setPtr->side & SIDE_VERTICAL) {
	    cavityWidth = Tk_Width(setPtr->tkwin) - (setPtr->pageTop + pad);
	    cavityHeight = Tk_Height(setPtr->tkwin) - (2 * pad);
	} else {
	    cavityWidth = Tk_Width(setPtr->tkwin) - (2 * pad);
	    cavityHeight = Tk_Height(setPtr->tkwin) - (setPtr->pageTop + pad);
	}
    } else {
	x = setPtr->inset + setPtr->pageInset;
#define TEAR_OFF_TAB_SIZE	5
	y = setPtr->inset + setPtr->pageInset + setPtr->yPad +
	    OUTER_PAD + TEAR_OFF_TAB_SIZE;
	cavityWidth = Tk_Width(parent) - (2 * pad);
	cavityHeight = Tk_Height(parent) - (y + pad);
    }
    winWidth = GetReqWidth(tabPtr);
    winHeight = GetReqHeight(tabPtr);

    /*
     * Always fill the window if its inside the tearoff.
     */
    if ((parent != setPtr->tkwin) || (cavityWidth < winWidth) ||
	(tabPtr->fill & FILL_X)) {
	winWidth = cavityWidth;
    }
    if ((parent != setPtr->tkwin) || (cavityHeight < winHeight) ||
	(tabPtr->fill & FILL_Y)) {
	winHeight = cavityHeight;
    }
    dx = (cavityWidth - winWidth);
    dy = (cavityHeight - winHeight);
    if ((dx > 0) || (dy > 0)) {
	TranslateAnchor(dx, dy, tabPtr->anchor, &x, &y);
    }
    rectPtr->x = x + tabPtr->padLeft;
    rectPtr->y = y + tabPtr->padTop;
    rectPtr->width = winWidth - PADDING(tabPtr->padX);
    rectPtr->height = winHeight - PADDING(tabPtr->padY);
}

static void
ArrangeWindow(tkwin, regionPtr, force)
    Tk_Window tkwin;
    XRectangle *regionPtr;
    int force;
{
    if ((force) ||
	(regionPtr->x != Tk_X(tkwin)) ||
	(regionPtr->y != Tk_Y(tkwin)) ||
	(regionPtr->width != Tk_Width(tkwin)) ||
	(regionPtr->height != Tk_Height(tkwin))) {
	Tk_MoveResizeWindow(tkwin, regionPtr->x, regionPtr->y, regionPtr->width,
	    regionPtr->height);
    }
    if (!Tk_IsMapped(tkwin)) {
	Tk_MapWindow(tkwin);
    }
}

/*ARGSUSED*/
static void
GetTags(table, object, tagArr, numTagsPtr)
    BindTable table;
    ClientData object;
    ClientData tagArr[];
    int *numTagsPtr;
{
    Tab *tabPtr = (Tab *) object;
    Tabset *setPtr;
    int numTags;

    setPtr = (Tabset *) table->clientData;
    tagArr[0] = (ClientData)Tk_GetUid(tabPtr->nameId);
    numTags = 1;
    if (tabPtr->tags != NULL) {
	int numNames;
	char **nameArr;
	register char **p;

	if (Tcl_SplitList(tabPtr->setPtr->interp, tabPtr->tags, &numNames,
		&nameArr) == TCL_OK) {
	    for (p = nameArr; (*p != NULL) && (numTags < 10); p++, numTags++) {
		tagArr[numTags] = (ClientData)Tk_GetUid((ClientData)*p);
	    }
	    free((char *)nameArr);
	}
    }
    *numTagsPtr = numTags;
}

/*
 * --------------------------------------------------------------
 *
 * TabsetEventProc --
 *
 * 	This procedure is invoked by the Tk dispatcher for various
 * 	events on tabset widgets.
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
TabsetEventProc(clientData, eventPtr)
    ClientData clientData;	/* Information about window. */
    XEvent *eventPtr;		/* Information about event. */
{
    Tabset *setPtr = (Tabset *) clientData;

    switch (eventPtr->type) {
    case Expose:
	if (eventPtr->xexpose.count == 0) {
	    EventuallyRedraw(setPtr);
	}
	break;

    case ConfigureNotify:
	setPtr->flags |= (TABSET_LAYOUT | TABSET_SCROLL);
	EventuallyRedraw(setPtr);
	break;

    case FocusIn:
    case FocusOut:
	if (eventPtr->xfocus.detail != NotifyInferior) {
	    if (eventPtr->type == FocusIn) {
		setPtr->flags |= TABSET_FOCUS;
	    } else {
		setPtr->flags &= ~TABSET_FOCUS;
	    }
	    EventuallyRedraw(setPtr);
	}
	break;

    case DestroyNotify:
	if (setPtr->tkwin != NULL) {
	    char *cmdName;

	    cmdName = Tcl_GetCommandName(setPtr->interp,
		setPtr->cmdToken);
#ifdef ITCL_NAMESPACES
	    Itk_SetWidgetCommand(setPtr->tkwin, (Tcl_Command) NULL);
#endif /* ITCL_NAMESPACES */
	    Tcl_DeleteCommand(setPtr->interp, cmdName);
	    setPtr->tkwin = NULL;
	}
	if (setPtr->flags & TABSET_REDRAW) {
	    Tk_CancelIdleCall(DisplayTabset, (ClientData)setPtr);
	}
	Tk_EventuallyFree((ClientData)setPtr, DestroyTabset);
	break;

    }
}

/*
 * ----------------------------------------------------------------------
 *
 * DestroyTabset --
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
DestroyTabset(dataPtr)
    DestroyData dataPtr;	/* Pointer to the widget record. */
{
    Tabset *setPtr = (Tabset *) dataPtr;
    Tab *tabPtr;
    register Blt_ListItem item, nextItem;

    if (setPtr->tileGC != NULL) {
	Tk_FreeGC(setPtr->display, setPtr->tileGC);
    }
    if (setPtr->highlightGC != NULL) {
	Tk_FreeGC(setPtr->display, setPtr->highlightGC);
    }
    if (setPtr->bgTile != NULL) {
	Blt_FreeTile(setPtr->bgTile);
    }
    if (setPtr->defAttrs.activeGC != NULL) {
	Blt_FreePrivateGC(setPtr->display, setPtr->defAttrs.activeGC);
    }
    for (item = Blt_ListFirstItem(&(setPtr->tabList)); item != NULL;
	/*empty*/ ) {
	nextItem = Blt_ListNextItem(item);
	tabPtr = (Tab *) Blt_ListGetValue(item);
	DestroyTab(setPtr, tabPtr);
	item = nextItem;
    }
    Blt_DestroyBindingTable(setPtr->bindTable);
    Tcl_DeleteHashTable(&(setPtr->tabTable));
    Tk_FreeOptions(configSpecs, (char *)setPtr, setPtr->display, 0);
    free((char *)setPtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * CreateTabset --
 *
 * ----------------------------------------------------------------------
 */
static Tabset *
CreateTabset(interp, tkwin)
    Tcl_Interp *interp;
    Tk_Window tkwin;
{
    Tabset *setPtr;

    setPtr = (Tabset *) calloc(1, sizeof(Tabset));
    assert(setPtr);

    Tk_SetClass(tkwin, "Tabset");
    setPtr->tkwin = tkwin;
    setPtr->display = Tk_Display(tkwin);
    setPtr->interp = interp;

    setPtr->flags |= (TABSET_LAYOUT | TABSET_SCROLL);
    setPtr->side = SIDE_TOP;
    setPtr->borderWidth = setPtr->highlightWidth = 2;
    setPtr->selectPad = SELECT_PAD;
    setPtr->relief = TK_RELIEF_SUNKEN;
    setPtr->defAttrs.relief = TK_RELIEF_RAISED;
    setPtr->defAttrs.borderWidth = 2;
    setPtr->defAttrs.textSide = SIDE_LEFT;
    setPtr->scrollUnits = 2;
    setPtr->corner = CORNER_OFFSET;
    setPtr->gap = GAP;
    setPtr->bindTable = Blt_CreateBindingTable(interp, tkwin, (ClientData)setPtr,
	PickTab, GetTags);
    Blt_InitList(&(setPtr->tabList), TCL_ONE_WORD_KEYS);
    Tcl_InitHashTable(&(setPtr->tabTable), TCL_ONE_WORD_KEYS);
    Tcl_InitHashTable(&(setPtr->imageTable), TCL_STRING_KEYS);
    return setPtr;
}

/*
 * ----------------------------------------------------------------------
 *
 * ConfigureTabset --
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
 *	etc. get set for setPtr; old resources get freed, if there
 *	were any.  The widget is redisplayed.
 *
 * ----------------------------------------------------------------------
 */
static int
ConfigureTabset(interp, setPtr, argc, argv, flags)
    Tcl_Interp *interp;		/* Interpreter to report errors. */
    Tabset *setPtr;		/* Information about widget; may or
			         * may not already have values for
			         * some fields. */
    int argc;
    char **argv;
    int flags;
{
    XGCValues gcValues;
    unsigned long gcMask;
    GC newGC;

    tabSet = setPtr;
    if (Tk_ConfigureWidget(interp, setPtr->tkwin, configSpecs, argc, argv,
	    (char *)setPtr, flags) != TCL_OK) {
	return TCL_ERROR;
    }
    if (Blt_ConfigModified(configSpecs, "-width", "-height", "-side",
	    (char *)NULL)) {
	setPtr->flags |= (TABSET_LAYOUT | TABSET_SCROLL);
    }
    if ((setPtr->reqHeight > 0) && (setPtr->reqWidth > 0)) {
	Tk_GeometryRequest(setPtr->tkwin, setPtr->reqWidth, setPtr->reqHeight);
    }
    /*
     * GC for focus highlight.
     */
    gcMask = GCForeground;
    gcValues.foreground = setPtr->highlightColor->pixel;
    newGC = Tk_GetGC(setPtr->tkwin, gcMask, &gcValues);
    if (setPtr->highlightGC != NULL) {
	Tk_FreeGC(setPtr->display, setPtr->highlightGC);
    }
    setPtr->highlightGC = newGC;

    /*
     * GC for tiled background.
     */
    newGC = NULL;
    if (setPtr->bgTile != NULL) {
	Pixmap pixmap;

	Blt_SetTileChangedProc(setPtr->bgTile, TileChangedProc,
	    (ClientData)setPtr);
	pixmap = Blt_PixmapOfTile(setPtr->bgTile);
	if (pixmap != None) {
	    gcMask = (GCTile | GCFillStyle);
	    gcValues.fill_style = FillTiled;
	    gcValues.tile = pixmap;
	    newGC = Tk_GetGC(setPtr->tkwin, gcMask, &gcValues);
	}
    }
    if (setPtr->tileGC != NULL) {
	Tk_FreeGC(setPtr->display, setPtr->tileGC);
    }
    setPtr->tileGC = newGC;

    /*
     * GC for active line.
     */
    gcMask = GCForeground | GCLineWidth | GCLineStyle | GCCapStyle;
    gcValues.foreground = setPtr->defAttrs.activeFgColor->pixel;
    gcValues.line_width = 0;
    gcValues.cap_style = CapProjecting;
    gcValues.line_style = (setPtr->defAttrs.dashes.numValues > 0)
	? LineOnOffDash : LineSolid;

    newGC = Blt_GetPrivateGC(setPtr->tkwin, gcMask, &gcValues);
    if (setPtr->defAttrs.dashes.numValues > 0) {
	setPtr->defAttrs.dashes.offset = 2;
	Blt_SetDashes(setPtr->display, newGC, &(setPtr->defAttrs.dashes));
    }
    if (setPtr->defAttrs.activeGC != NULL) {
	Blt_FreePrivateGC(setPtr->display, setPtr->defAttrs.activeGC);
    }
    setPtr->defAttrs.activeGC = newGC;

    setPtr->defAttrs.rotate = FMOD(setPtr->defAttrs.rotate, 360.0);
    if (setPtr->defAttrs.rotate < 0.0) {
	setPtr->defAttrs.rotate += 360.0;
    }
    setPtr->inset = setPtr->highlightWidth + setPtr->borderWidth + OUTER_PAD;
    if (Blt_ConfigModified(configSpecs, "-font", "-*foreground", "-rotate",
	    "-*background", "-side", (char *)NULL)) {
	register Blt_ListItem item;
	Tab *tabPtr;

	for (item = Blt_ListFirstItem(&(setPtr->tabList)); item != NULL;
	    item = Blt_ListNextItem(item)) {
	    tabPtr = (Tab *) Blt_ListGetValue(item);
	    ConfigureTab(setPtr, tabPtr);
	}
	setPtr->flags |= (TABSET_LAYOUT | TABSET_SCROLL);
    }
    if (setPtr->scrollUnits < 1) {
	setPtr->scrollUnits = 1;
    }
    setPtr->pageInset = setPtr->defAttrs.borderWidth + setPtr->corner;
    EventuallyRedraw(setPtr);
    return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *
 * TabsetCmd --
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
TabsetCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window associated with interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    Tabset *setPtr;
    Tk_Window tkwin;
    static int initialized = FALSE;
    unsigned int mask;

    if (!initialized) {
	static char initCmd[] =
	{"\n\
set file [file join $blt_library bltTabset.tcl]\n\
if { [file readable $file] } {\n\
    source $file\n\
} \n"
	};
	if (Tcl_GlobalEval(interp, initCmd) != TCL_OK) {
	    Tcl_AddErrorInfo(interp,
		"\n    (while loading bindings for blt::tabset)");
	    return TCL_ERROR;
	}
	initialized = TRUE;
    }
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
    setPtr = CreateTabset(interp, tkwin);
    if (ConfigureTabset(interp, setPtr, argc - 2, argv + 2, 0) != TCL_OK) {
	Tk_DestroyWindow(setPtr->tkwin);
	return TCL_ERROR;
    }
    mask = (ExposureMask | StructureNotifyMask | FocusChangeMask);
    Tk_CreateEventHandler(tkwin, mask, TabsetEventProc, (ClientData)setPtr);
    setPtr->cmdToken = Tcl_CreateCommand(interp, argv[1], TabsetInstCmd,
	(ClientData)setPtr, TabsetInstDeletedCmd);
#ifdef ITCL_NAMESPACES
    Itk_SetWidgetCommand(setPtr->tkwin, setPtr->cmdToken);
#endif
    /*
     * Try to invoke a procedure to initialize various bindings on tabs.
     * This can fail if the initialization file wasn't sourced above.
     * We silently ignore any errors.
     */
    Tcl_VarEval(interp, "blt::InitTabBindings ", argv[1], (char *)NULL);
    Tcl_SetResult(interp, Tk_PathName(setPtr->tkwin), TCL_STATIC);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * TabsetInstDeletedCmd --
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
TabsetInstDeletedCmd(clientData)
    ClientData clientData;	/* Pointer to widget record for widget. */
{
    Tabset *setPtr = (Tabset *) clientData;

    if (setPtr->tkwin != NULL) {
	Tk_Window tkwin;

	tkwin = setPtr->tkwin;
	setPtr->tkwin = NULL;
	Tk_DestroyWindow(tkwin);
#ifdef ITCL_NAMESPACES
	Itk_SetWidgetCommand(tkwin, (Tcl_Command) NULL);
#endif /* ITCL_NAMESPACES */
    }
}

/*
 * --------------------------------------------------------------
 *
 * Tabset operations
 *
 * --------------------------------------------------------------
 */
/*
 *----------------------------------------------------------------------
 *
 * ActivateOp --
 *
 *	Selects the tab to appear active.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ActivateOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;
    int argc;			/* Not used. */
    char **argv;
{
    Tab *tabPtr, *oldPtr, *selPtr;
    Drawable drawable;

    if (argv[2][0] == '\0') {
	tabPtr = NULL;
    } else if (GetIndex(setPtr, argv[2], &tabPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if ((tabPtr != NULL) && (tabPtr->state == STATE_DISABLED)) {
	tabPtr = NULL;
    }
    oldPtr = setPtr->activePtr;


    setPtr->activePtr = tabPtr;
    drawable = Tk_WindowId(setPtr->tkwin);
    if (tabPtr != oldPtr) {
	int redraw;

	selPtr = setPtr->selectPtr;
	redraw = FALSE;
	if (oldPtr != NULL) {
	    if ((oldPtr == TabLeft(setPtr, setPtr->selectPtr)) ||
		(oldPtr == TabRight(setPtr, setPtr->selectPtr))) {
		redraw = TRUE;
	    }
	    if ((oldPtr->tier == 2) &&
		(oldPtr->worldX + oldPtr->worldWidth) >= (selPtr->worldX) &&
		(oldPtr->worldX < (selPtr->worldX + selPtr->worldWidth))) {
		redraw = TRUE;
	    } else {
		DrawLabel(setPtr, oldPtr, drawable);
	    }
	}
	if ((tabPtr != NULL) && (!redraw)) {
	    if ((tabPtr == TabLeft(setPtr, setPtr->selectPtr)) ||
		(tabPtr == TabRight(setPtr, setPtr->selectPtr))) {
		redraw = TRUE;
	    }
	    if ((tabPtr->tier == 2) &&
		(tabPtr->worldX + tabPtr->worldWidth) >= (selPtr->worldX) &&
		(tabPtr->worldX < (selPtr->worldX + selPtr->worldWidth))) {
		redraw = TRUE;
	    } else {
		DrawLabel(setPtr, tabPtr, drawable);
	    }
	}
	DrawOuterBorders(setPtr, drawable);
	if (redraw) {
	    EventuallyRedraw(setPtr);
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * BindOp --
 *
 *	  .t bind index sequence command
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
BindOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    return Blt_ConfigureBindings(interp, setPtr->bindTable, Tk_GetUid(argv[2]),
	argc - 3, argv + 3);
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
CgetOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    tabSet = setPtr;
    return Tk_ConfigureValue(interp, setPtr->tkwin, configSpecs,
	(char *)setPtr, argv[2], 0);
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
 *	etc. get set for setPtr; old resources get freed, if there
 *	were any.  The widget is redisplayed.
 *
 *----------------------------------------------------------------------
 */
static int
ConfigureOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    tabSet = setPtr;
    if (argc == 2) {
	return Tk_ConfigureInfo(interp, setPtr->tkwin, configSpecs,
	    (char *)setPtr, (char *)NULL, 0);
    } else if (argc == 3) {
	return Tk_ConfigureInfo(interp, setPtr->tkwin, configSpecs,
	    (char *)setPtr, argv[2], 0);
    }
    if (ConfigureTabset(interp, setPtr, argc - 2, argv + 2,
	    TK_CONFIG_ARGV_ONLY) != TCL_OK) {
	return TCL_ERROR;
    }
    EventuallyRedraw(setPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * DeleteOp --
 *
 *	Deletes tab from the set. Deletes either a range of
 *	tabs or a single node.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
DeleteOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    Tab *firstPtr, *lastPtr;

    lastPtr = NULL;
    if (TabIndex(setPtr, argv[2], &firstPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if ((argc == 4) && (TabIndex(setPtr, argv[3], &lastPtr) != TCL_OK)) {
	return TCL_ERROR;
    }
    if (lastPtr == NULL) {
	DestroyTab(setPtr, firstPtr);
    } else {
	Tab *tabPtr;
	register Blt_ListItem item, nextItem;

	tabPtr = NULL;		/* Suppress compiler warning. */

	/* Make sure that the first tab is before the last. */
	for (item = firstPtr->item; item != NULL;
	    item = Blt_ListNextItem(item)) {
	    tabPtr = (Tab *) Blt_ListGetValue(item);
	    if (tabPtr == lastPtr) {
		break;
	    }
	}
	if (tabPtr != lastPtr) {
	    return TCL_OK;
	}
	item = firstPtr->item;
	while (item != NULL) {
	    nextItem = Blt_ListNextItem(item);
	    tabPtr = (Tab *) Blt_ListGetValue(item);
	    DestroyTab(setPtr, tabPtr);
	    item = nextItem;
	    if (tabPtr == lastPtr) {
		break;
	    }
	}
    }
    setPtr->flags |= (TABSET_LAYOUT | TABSET_SCROLL);
    EventuallyRedraw(setPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * FocusOp --
 *
 *	Selects the tab to get focus.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
FocusOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;
    int argc;			/* Not used. */
    char **argv;
{
    Tab *tabPtr;

    if (GetIndex(setPtr, argv[2], &tabPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if ((tabPtr != NULL) && (tabPtr->state == STATE_DISABLED)) {
	tabPtr = NULL;
    }
    if (tabPtr != NULL) {
	setPtr->focusPtr = tabPtr;
	Blt_SetFocusItem(setPtr->bindTable, setPtr->focusPtr);
	EventuallyRedraw(setPtr);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * IndexOp --
 *
 *	Converts a string representing a tab index.
 *
 * Results:
 *	A standard Tcl result.  Interp->result will contain the
 *	identifier of each index found. If an index could not be found,
 *	then the serial identifier will be the empty string.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
IndexOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    Tab *tabPtr;

    if (GetIndex(setPtr, argv[2], &tabPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (tabPtr == NULL) {
	Tcl_SetResult(interp, "", TCL_STATIC);
    } else {
	Tcl_SetResult(interp, Blt_Int(tabPtr->index), TCL_VOLATILE);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * GetOp --
 *
 *	Converts a tab index into the tab identifier.
 *
 * Results:
 *	A standard Tcl result.  Interp->result will contain the
 *	identifier of each index found. If an index could not be found,
 *	then the serial identifier will be the empty string.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
GetOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    Tab *tabPtr;

    if (GetIndex(setPtr, argv[2], &tabPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (tabPtr == NULL) {
	Tcl_SetResult(interp, "", TCL_STATIC);
    } else {
	Tcl_SetResult(interp, tabPtr->nameId, TCL_VOLATILE);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * InsertOp --
 *
 *	Add new entries into a tab set.
 *
 *	.t insert end label option-value label option-value...
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
InsertOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    Tab *tabPtr;
    int position;
    register int i;
    char **options;
    Blt_ListItem item, beforeItem;
    Blt_List *listPtr;
    int start, count;

    if (GetPosition(setPtr->interp, argv[2], &position) != TCL_OK) {
	return TCL_ERROR;
    }
    listPtr = &(setPtr->tabList);
    if ((position == POSITION_END) ||
	(position >= Blt_ListGetLength(listPtr))) {
	beforeItem = NULL;
    } else {
	beforeItem = Blt_ListFindNthItem(listPtr, position, 1);
    }
    tabSet = setPtr;
    setPtr->flags |= (TABSET_LAYOUT | TABSET_SCROLL);
    EventuallyRedraw(setPtr);
    for (i = 3; i < argc; /*empty*/ ) {
	if (FindTab(setPtr, argv[i], &tabPtr) == TCL_OK) {
	    Tcl_AppendResult(setPtr->interp, "tab \"", argv[i],
		"\" already exists in \"", Tk_PathName(setPtr->tkwin), "\"",
		(char *)NULL);
	    return TCL_ERROR;
	}
	tabPtr = CreateTab(setPtr, argv[i]);
	if (tabPtr == NULL) {
	    return TCL_ERROR;
	}
	/*
	 * Count the option-value pairs that follow.  Count until we
	 * spot one that doesn't look like a configuration option (i.e.
	 * doesn't start with a minus "-").
	 */
	i++;
	start = i;
	for ( /*empty*/ ; i < argc; i += 2) {
	    if (argv[i][0] != '-') {
		break;
	    }
	}
	count = i - start;
	options = argv + start;
	if (Blt_ConfigureWidgetComponent(interp, setPtr->tkwin, tabPtr->nameId,
		"Tab", tabConfigSpecs, count, options, (char *)tabPtr, 0) != TCL_OK) {
	    DestroyTab(setPtr, tabPtr);
	    return TCL_ERROR;
	}
	if (ConfigureTab(setPtr, tabPtr) != TCL_OK) {
	    DestroyTab(setPtr, tabPtr);
	    return TCL_ERROR;
	}
	item = Blt_ListNewItem(listPtr, tabPtr->nameId);
	if (beforeItem == NULL) {
	    Blt_ListAppendItem(listPtr, item);
	} else {
	    Blt_ListLinkBefore(listPtr, item, beforeItem);
	}
	tabPtr->index = Blt_ListGetLength(&(setPtr->tabList));
	tabPtr->item = item;
	Blt_ListSetValue(item, (ClientData)tabPtr);
    }
    return TCL_OK;
}

/*
 * Preprocess the command string for percent substitution.
 */
static void
PercentSubst(setPtr, tabPtr, command, cmdStrPtr)
    Tabset *setPtr;
    Tab *tabPtr;
    char *command;
    Tcl_DString *cmdStrPtr;
{
    register char *last, *p;

    /*
     * Get the full path name of the node, in case we need to
     * substitute for it.
     */
    Tcl_DStringInit(cmdStrPtr);
    for (last = p = command; *p != '\0'; p++) {
	if (*p == '%') {
	    char *string;
	    char buf[3];

	    if (p > last) {
		*p = '\0';
		Tcl_DStringAppend(cmdStrPtr, last, -1);
		*p = '%';
	    }
	    switch (*(p + 1)) {
	    case '%':		/* Percent sign */
		string = "%";
		break;
	    case 'W':		/* Widget name */
		string = Tk_PathName(setPtr->tkwin);
		break;
	    case 'i':		/* Tab Index */
		string = Blt_Int(tabPtr->index);
		break;
	    case 'n':		/* Tab name */
		string = tabPtr->nameId;
		break;
	    default:
		if (*(p + 1) == '\0') {
		    p--;
		}
		buf[0] = *p, buf[1] = *(p + 1), buf[2] = '\0';
		string = buf;
		break;
	    }
	    Tcl_DStringAppend(cmdStrPtr, string, -1);
	    p++;
	    last = p + 1;
	}
    }
    if (p > last) {
	*p = '\0';
	Tcl_DStringAppend(cmdStrPtr, last, -1);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * InvokeOp --
 *
 * 	This procedure is called to invoke a selection command.
 *
 *	  .h invoke index
 *
 * Results:
 *	A standard Tcl result.  If TCL_ERROR is returned, then
 *	interp->result contains an error message.
 *
 * Side Effects:
 *	Configuration information, such as text string, colors, font,
 * 	etc. get set;  old resources get freed, if there were any.
 * 	The widget is redisplayed if needed.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
InvokeOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;		/* Not used. */
    int argc;
    char **argv;
{
    Tab *tabPtr;
    char *command;

    if (GetIndex(setPtr, argv[2], &tabPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if ((tabPtr == NULL) || (tabPtr->state == STATE_DISABLED)) {
	return TCL_OK;
    }
    command = GETATTR(tabPtr, command);
    if (command != NULL) {
	Tcl_DString cmdString;
	int result;

	PercentSubst(setPtr, tabPtr, command, &cmdString);
	result = Tcl_GlobalEval(setPtr->interp, Tcl_DStringValue(&cmdString));
	Tcl_DStringFree(&cmdString);
	if (result != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    if ((setPtr->selectPtr != NULL) && (setPtr->selectPtr != tabPtr) &&
	(setPtr->selectPtr->tkwin != NULL)) {
	if (setPtr->selectPtr->container == NULL) {
	    if (Tk_IsMapped(setPtr->selectPtr->tkwin)) {
		Tk_UnmapWindow(setPtr->selectPtr->tkwin);
	    }
	} else {
	    /* Redraw now unselected container. */
	    EventuallyRedrawTearoff(setPtr->selectPtr);
	}
    }
    setPtr->focusPtr = setPtr->selectPtr = tabPtr;
    Blt_SetFocusItem(setPtr->bindTable, setPtr->focusPtr);
    if ((setPtr->numTiers > 1) && (tabPtr->tier != setPtr->startPtr->tier)) {
	RenumberTiers(setPtr, tabPtr);
	Blt_PickCurrentItem(setPtr->bindTable);
    }
    setPtr->flags |= (TABSET_SCROLL);
    if (tabPtr->container != NULL) {
	EventuallyRedrawTearoff(tabPtr);
    }
    EventuallyRedraw(setPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * MoveOp --
 *
 *	Moves a tab to a new location.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
MoveOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    Tab *tabPtr, *linkPtr;
    int before;

    if (GetIndex(setPtr, argv[2], &tabPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if ((tabPtr == NULL) || (tabPtr->state == STATE_DISABLED)) {
	return TCL_OK;
    }
    if ((argv[3][0] == 'b') && (strcmp(argv[3], "before") == 0)) {
	before = 1;
    } else if ((argv[3][0] == 'a') && (strcmp(argv[3], "after") == 0)) {
	before = 0;
    } else {
	Tcl_AppendResult(interp, "bad key word \"", argv[3],
	    "\": should be \"after\" or \"before\"", (char *)NULL);
	return TCL_ERROR;
    }
    if (TabIndex(setPtr, argv[4], &linkPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (tabPtr == linkPtr) {
	return TCL_OK;
    }
    Blt_ListUnlinkItem(tabPtr->item);
    if (before) {
	Blt_ListLinkBefore(&(setPtr->tabList), tabPtr->item, linkPtr->item);
    } else {
	Blt_ListLinkAfter(&(setPtr->tabList), tabPtr->item, linkPtr->item);
    }
    setPtr->flags |= (TABSET_LAYOUT | TABSET_SCROLL);
    EventuallyRedraw(setPtr);
    return TCL_OK;
}

/*ARGSUSED*/
static int
NearestOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;
    int argc;			/* Not used. */
    char **argv;
{
    int x, y;			/* Screen coordinates of the test point. */
    Tab *tabPtr;

    if ((Tk_GetPixels(interp, setPtr->tkwin, argv[2], &x) != TCL_OK) ||
	(Tk_GetPixels(interp, setPtr->tkwin, argv[3], &y) != TCL_OK)) {
	return TCL_ERROR;
    }
    if (setPtr->numVisible > 0) {
	tabPtr = (Tab *) PickTab((ClientData)setPtr, x, y);
	if (tabPtr != NULL) {
	    Tcl_SetResult(interp, tabPtr->nameId, TCL_VOLATILE);
	}
    }
    return TCL_OK;
}


static int
ViewOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int width;

    width = VPORTWIDTH(setPtr);
    if (argc == 2) {
	double fract;

	/*
	 * Note that we are bounding the fractions between 0.0 and 1.0
	 * to support the "canvas"-style of scrolling.
	 */
	fract = (double)setPtr->scrollOffset / setPtr->worldWidth;
	Tcl_AppendElement(interp, Blt_Double(interp, CLAMP(fract, 0.0, 1.0)));
	fract = (double)(setPtr->scrollOffset + width) / setPtr->worldWidth;
	Tcl_AppendElement(interp, Blt_Double(interp, CLAMP(fract, 0.0, 1.0)));
	return TCL_OK;
    }
    if (Blt_GetScrollInfo(interp, argc, argv, &(setPtr->scrollOffset),
	    setPtr->worldWidth, width, setPtr->scrollUnits, SCROLL_MODE_CANVAS)
	!= TCL_OK) {
	return TCL_ERROR;
    }
    setPtr->flags |= TABSET_SCROLL;
    EventuallyRedraw(setPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * CgetOpOp --
 *
 *	  .h tab cget index option
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
CgetOpOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    Tab *tabPtr;

    if (TabIndex(setPtr, argv[3], &tabPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    tabSet = setPtr;
    return Tk_ConfigureValue(interp, setPtr->tkwin, tabConfigSpecs,
	(char *)tabPtr, argv[4], 0);
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureOpOp --
 *
 * 	This procedure is called to process a list of configuration
 *	options database, in order to reconfigure the options for
 *	one or more tabs in the widget.
 *
 *	  .h tab configure index ?index...? ?option value?...
 *
 * Results:
 *	A standard Tcl result.  If TCL_ERROR is returned, then
 *	interp->result contains an error message.
 *
 * Side Effects:
 *	Configuration information, such as text string, colors, font,
 * 	etc. get set;  old resources get freed, if there were any.
 * 	The widget is redisplayed if needed.
 *
 *----------------------------------------------------------------------
 */
static int
ConfigureOpOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int numTabs, numOpts, result;
    char **options;
    register int i;
    Tab *tabPtr;


    /* Figure out where the option value pairs begin */
    argc -= 3;
    argv += 3;
    for (i = 0; i < argc; i++) {
	if (argv[i][0] == '-') {
	    break;
	}
	if (TabIndex(setPtr, argv[i], &tabPtr) != TCL_OK) {
	    return TCL_ERROR;	/* Can't find node. */
	}
    }
    numTabs = i;		/* Number of tab indices specified */
    numOpts = argc - i;		/* Number of options specified */
    options = argv + i;		/* Start of options in argv  */

    for (i = 0; i < numTabs; i++) {
	TabIndex(setPtr, argv[i], &tabPtr);
	if (argc == 1) {
	    return Tk_ConfigureInfo(interp, setPtr->tkwin, tabConfigSpecs,
		(char *)tabPtr, (char *)NULL, 0);
	} else if (argc == 2) {
	    return Tk_ConfigureInfo(interp, setPtr->tkwin, tabConfigSpecs,
		(char *)tabPtr, argv[2], 0);
	}
	tabSet = setPtr;
	Tcl_Preserve((ClientData)tabPtr);
	result = Tk_ConfigureWidget(interp, setPtr->tkwin, tabConfigSpecs,
	    numOpts, options, (char *)tabPtr, TK_CONFIG_ARGV_ONLY);
	if (result == TCL_OK) {
	    result = ConfigureTab(setPtr, tabPtr);
	}
	Tcl_Release((ClientData)tabPtr);
	if (result == TCL_ERROR) {
	    return TCL_ERROR;
	}
	if (tabPtr->flags & TAB_VISIBLE) {
	    setPtr->flags |= (TABSET_LAYOUT | TABSET_SCROLL);
	    EventuallyRedraw(setPtr);
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * NamesOpOp --
 *
 *	  .h tab names pattern
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
NamesOpOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;
    int argc;			/* Not used */
    char **argv;		/* Not used */
{
    Tab *tabPtr;
    Blt_ListItem item;

    if (argc == 3) {
	for (item = Blt_ListFirstItem(&(setPtr->tabList)); item != NULL;
	    item = Blt_ListNextItem(item)) {
	    tabPtr = (Tab *) Blt_ListGetValue(item);
	    Tcl_AppendElement(interp, tabPtr->nameId);
	}
    } else {
	register int i;

	for (item = Blt_ListFirstItem(&(setPtr->tabList)); item != NULL;
	    item = Blt_ListNextItem(item)) {
	    tabPtr = (Tab *) Blt_ListGetValue(item);
	    for (i = 3; i < argc; i++) {
		if (Tcl_StringMatch(tabPtr->nameId, argv[i])) {
		    Tcl_AppendElement(interp, tabPtr->nameId);
		    break;
		}
	    }
	}
    }
    return TCL_OK;
}

static void
AdoptWindow(clientData)
    ClientData clientData;
{
    Tab *tabPtr = (Tab *) clientData;
    Tk_FakeWin *fakePtr;
    int x, y, inset;
    int result;

    inset = tabPtr->setPtr->borderWidth + OUTER_PAD;
    x = inset + tabPtr->setPtr->gap + tabPtr->setPtr->corner;
    y = inset;
    result = Blt_ReparentWindow(tabPtr->setPtr->display,
	Tk_WindowId(tabPtr->tkwin), Tk_WindowId(tabPtr->container), x, y);
    if (result == TCL_OK) {
	fakePtr = (Tk_FakeWin *) tabPtr->tkwin;
	fakePtr->flags |= TK_REPARENTED;
    }
    Tk_MapWindow(tabPtr->tkwin);
}

static void
DestroyTearoff(dataPtr)
    DestroyData dataPtr;
{
    Tab *tabPtr = (Tab *) dataPtr;

    if (tabPtr->container != NULL) {
	Tabset *setPtr;
	Tk_Window tkwin;
	setPtr = tabPtr->setPtr;

	tkwin = tabPtr->container;
	if (tabPtr->flags & TAB_REDRAW) {
	    Tk_CancelIdleCall(DisplayTearoff, (ClientData)tabPtr);
	}
	Tk_DeleteEventHandler(tkwin, StructureNotifyMask, TearoffEventProc,
	    (ClientData)tabPtr);
	if (tabPtr->tkwin != NULL) {
	    Tk_FakeWin *fakePtr;
	    XRectangle region;

	    GetWindowRegion(tabPtr, setPtr->tkwin, FALSE, &region);
	    Blt_ReparentWindow(setPtr->display, Tk_WindowId(tabPtr->tkwin),
		Tk_WindowId(setPtr->tkwin), region.x, region.y);
	    fakePtr = (Tk_FakeWin *) tabPtr->tkwin;
	    fakePtr->flags &= ~TK_REPARENTED;

	    if (tabPtr == setPtr->selectPtr) {
		ArrangeWindow(tabPtr->tkwin, &region, TRUE);
	    } else {
		Tk_UnmapWindow(tabPtr->tkwin);
	    }
	}
	Tk_DestroyWindow(tkwin);
	tabPtr->container = NULL;
    }
}

static int
CreateTearoff(setPtr, name, tabPtr)
    Tabset *setPtr;
    char *name;
    Tab *tabPtr;
{
    Tk_Window tkwin;
    int width, height;

    tkwin = Tk_CreateWindowFromPath(setPtr->interp, setPtr->tkwin, name,
	(char *)NULL);
    if (tkwin == NULL) {
	return TCL_ERROR;
    }
    tabPtr->container = tkwin;
    if (Tk_WindowId(tkwin) == None) {
	Tk_MakeWindowExist(tkwin);
    }
    Tk_SetClass(tkwin, "Tearoff");
    Tk_CreateEventHandler(tkwin, (ExposureMask | StructureNotifyMask),
	TearoffEventProc, (ClientData)tabPtr);
    if (Tk_WindowId(tabPtr->tkwin) == None) {
	Tk_MakeWindowExist(tabPtr->tkwin);
    }
    width = Tk_Width(tabPtr->tkwin);
    if (width < 2) {
	width = (tabPtr->reqWidth > 0)
	    ? tabPtr->reqWidth : Tk_ReqWidth(tabPtr->tkwin);
    }
    width += PADDING(tabPtr->padX) + 2 * Tk_Changes(tabPtr->tkwin)->border_width;
    width += 2 * (setPtr->borderWidth + OUTER_PAD);
    height = Tk_Height(tabPtr->tkwin);
    if (height < 2) {
	height = (tabPtr->reqHeight > 0)
	    ? tabPtr->reqHeight : Tk_ReqHeight(tabPtr->tkwin);
    }
    height += PADDING(tabPtr->padY) +
	2 * Tk_Changes(tabPtr->tkwin)->border_width;
    height += 2 * setPtr->inset + TEAR_OFF_TAB_SIZE + 2 * OUTER_PAD;
    Tk_GeometryRequest(tkwin, width, height);
    Tk_UnmapWindow(tabPtr->tkwin);
    Tk_MoveWindow(tabPtr->tkwin, 0, 0);
    Tcl_SetResult(setPtr->interp, Tk_PathName(tkwin), TCL_STATIC);
#ifdef WIN32
    AdoptWindow((ClientData)tabPtr);
#else
    Tk_DoWhenIdle(AdoptWindow, (ClientData)tabPtr);
#endif
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * TearOffOpOp --
 *
 *	  .h tab tearoff index ?title?
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
TearoffOpOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    Tab *tabPtr;
    int result;
    Tk_Window tkwin;

    result = TCL_OK;

    if (GetIndex(setPtr, argv[3], &tabPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if ((tabPtr->tkwin == NULL) || (tabPtr->state == STATE_DISABLED)) {
	return TCL_OK;		/* No-op */
    }
    if (argc == 4) {
	Tk_Window parent;

	parent = (tabPtr->container == NULL) ? setPtr->tkwin : tabPtr->container;
	Tcl_SetResult(setPtr->interp, Tk_PathName(parent), TCL_STATIC);
	return TCL_OK;
    }
    Tk_Preserve((ClientData)tabPtr);
    result = TCL_OK;

    tkwin = Tk_NameToWindow(interp, argv[4], setPtr->tkwin);
    Tcl_ResetResult(interp);

    if (tabPtr->container != NULL) {
	Tk_EventuallyFree((ClientData)tabPtr, DestroyTearoff);
    }
    if ((tkwin != setPtr->tkwin) && (tabPtr->container == NULL)) {
	result = CreateTearoff(setPtr, argv[4], tabPtr);
    }
    Tk_Release((ClientData)tabPtr);
    EventuallyRedraw(setPtr);
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * TabOp --
 *
 *	This procedure handles tab operations.
 *
 * Results:
 *	A standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
static Blt_OpSpec tabOperSpecs[] =
{
    {"cget", 2, (Blt_Operation)CgetOpOp, 5, 5, "index option",},
    {"configure", 2, (Blt_Operation)ConfigureOpOp, 4, 0,
	"index ?option value?...",},
    {"names", 1, (Blt_Operation)NamesOpOp, 3, 0, "?pattern...?",},
    {"tearoff", 1, (Blt_Operation)TearoffOpOp, 4, 5, "index ?parent?",},
};

static int numTabSpecs = sizeof(tabOperSpecs) / sizeof(Blt_OpSpec);

static int
TabOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Blt_Operation proc;
    int result;

    proc = Blt_GetOperation(interp, numTabSpecs, tabOperSpecs,
	BLT_OPER_ARG2, argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    result = (*proc) (setPtr, interp, argc, argv);
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * ScanOp --
 *
 *	Implements the quick scan.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ScanOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    int x, y;
    char c;
    unsigned int length;
    int oper;

#define SCAN_MARK	1
#define SCAN_DRAGTO	2
    c = argv[2][0];
    length = strlen(argv[2]);
    if ((c == 'm') && (strncmp(argv[2], "mark", length) == 0)) {
	oper = SCAN_MARK;
    } else if ((c == 'd') && (strncmp(argv[2], "dragto", length) == 0)) {
	oper = SCAN_DRAGTO;
    } else {
	Tcl_AppendResult(interp, "bad scan operation \"", argv[2],
	    "\": should be either \"mark\" or \"dragto\"", (char *)NULL);
	return TCL_ERROR;
    }
    if ((Tk_GetPixels(interp, setPtr->tkwin, argv[3], &x) != TCL_OK) ||
	(Tk_GetPixels(interp, setPtr->tkwin, argv[4], &y) != TCL_OK)) {
	return TCL_ERROR;
    }
    if (oper == SCAN_MARK) {
	if (setPtr->side & SIDE_VERTICAL) {
	    setPtr->scanAnchor = y;
	} else {
	    setPtr->scanAnchor = x;
	}
	setPtr->scanOffset = setPtr->scrollOffset;
    } else {
	int offset, delta;

	if (setPtr->side & SIDE_VERTICAL) {
	    delta = setPtr->scanAnchor - y;
	} else {
	    delta = setPtr->scanAnchor - x;
	}
	offset = setPtr->scanOffset + (10 * delta);
	offset = Blt_AdjustViewport(offset, setPtr->worldWidth,
	    VPORTWIDTH(setPtr), setPtr->scrollUnits, SCROLL_MODE_CANVAS);
	setPtr->scrollOffset = offset;
	setPtr->flags |= TABSET_SCROLL;
	EventuallyRedraw(setPtr);
    }
    return TCL_OK;
}

/*ARGSUSED*/
static int
SeeOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;		/* Not used. */
    int argc;
    char **argv;
{
    Tab *tabPtr;

    if (GetIndex(setPtr, argv[2], &tabPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (tabPtr != NULL) {
	int left, right, width;

	width = VPORTWIDTH(setPtr);
	left = setPtr->scrollOffset + setPtr->selectPad;
	right = setPtr->scrollOffset + width - setPtr->selectPad;

	/* If the tab is partially obscured, scroll so that it's
	 * entirely in view. */
	if (tabPtr->worldX < left) {
	    setPtr->scrollOffset = tabPtr->worldX - setPtr->selectPad;
	} else if ((tabPtr->worldX + tabPtr->worldWidth) >= right) {
	    setPtr->scrollOffset = tabPtr->worldX + tabPtr->worldWidth -
		width + setPtr->selectPad;
	}
	setPtr->flags |= TABSET_SCROLL;
	EventuallyRedraw(setPtr);
    }
    return TCL_OK;
}

/*ARGSUSED*/
static int
SizeOp(setPtr, interp, argc, argv)
    Tabset *setPtr;
    Tcl_Interp *interp;
    int argc;			/* Not used. */
    char **argv;		/* Not used. */
{
    Tcl_SetResult(interp, Blt_Int(Blt_ListGetLength(&(setPtr->tabList))),
	TCL_VOLATILE);
    return TCL_OK;
}



static void
WidenTabs(setPtr, startPtr, numTabs, extraSpace)
    Tabset *setPtr;
    Tab *startPtr;
    int numTabs;
    int extraSpace;
{
    register Tab *tabPtr;
    register int i;
    int adjustSize;
    register Blt_ListItem item;
    int x;

    x = startPtr->tier;
    while (extraSpace > 0) {
	adjustSize = extraSpace / numTabs;
	if (adjustSize == 0) {
	    adjustSize = 1;
	}
	item = startPtr->item;
	for (i = 0; (item != NULL) && (i < numTabs) && (extraSpace > 0); i++) {
	    tabPtr = (Tab *) Blt_ListGetValue(item);
	    extraSpace -= adjustSize;
	    tabPtr->worldWidth += adjustSize;
	    assert(x == tabPtr->tier);
	    item = Blt_ListNextItem(item);
	}
    }
    x = setPtr->selectPad;
    item = startPtr->item;
    for (i = 0; (i < numTabs) && (item != NULL); i++) {
	tabPtr = (Tab *) Blt_ListGetValue(item);
	tabPtr->worldX = x;
	x += tabPtr->worldWidth + setPtr->gap;
	item = Blt_ListNextItem(item);
    }
}

static int
CountTabs(setPtr)
    Tabset *setPtr;
{
    int numTabs;
    int width, height;
    register Blt_ListItem item;
    register Tab *tabPtr;
    register int pageWidth, pageHeight;
    register int maxWidth, maxHeight;

    pageWidth = pageHeight = 0;
    maxWidth = maxHeight = 0;
    numTabs = 0;

    /*
     * Determine how much space is needed for each tab.  This will
     * also give us the maximum tab height.
     */
    for (item = Blt_ListFirstItem(&(setPtr->tabList)); item != NULL;
	item = Blt_ListNextItem(item)) {
	tabPtr = (Tab *) Blt_ListGetValue(item);

	if (setPtr->side & SIDE_VERTICAL) {
	    tabPtr->worldWidth = tabPtr->labelHeight;
	    tabPtr->worldHeight = tabPtr->labelWidth;
	} else {
	    tabPtr->worldWidth = tabPtr->labelWidth;
	    tabPtr->worldHeight = tabPtr->labelHeight;
	}
	tabPtr->worldWidth += 2 * (setPtr->pageInset + LABEL_PADX);
	tabPtr->worldHeight += 2 * (setPtr->pageInset + LABEL_PADY);

	/* Reset visibility flag and order of tabs. */

	tabPtr->flags &= ~TAB_VISIBLE;
	tabPtr->index = numTabs++;

	/*
	 * Find the maximum page dimensions based of the requested
	 * size of the embedded window for each tab.
	 */
	if (tabPtr->tkwin != NULL) {
	    width = GetReqWidth(tabPtr);
	    if (pageWidth < width) {
		pageWidth = width;
	    }
	    height = GetReqHeight(tabPtr);
	    if (pageHeight < height) {
		pageHeight = height;
	    }
	}
	/* Track the maximum tab dimensions. */

	if (maxWidth < tabPtr->worldWidth) {
	    maxWidth = tabPtr->worldWidth;
	}
	if (maxHeight < tabPtr->worldHeight) {
	    maxHeight = tabPtr->worldHeight;
	}
    }
    setPtr->tabWidth = maxWidth;
    setPtr->tabHeight = maxHeight;

    /*
     * If constant-width tabs are selected, reset the sizes for each
     * tab to the maximum size.
     */
    if (setPtr->defAttrs.constWidth) {
	for (item = Blt_ListFirstItem(&(setPtr->tabList)); item != NULL;
	    item = Blt_ListNextItem(item)) {
	    tabPtr = (Tab *) Blt_ListGetValue(item);
	    tabPtr->worldWidth = setPtr->tabWidth;
	}
    }
    /*
     * Let the user override any page dimension.
     */
    setPtr->pageWidth = pageWidth;
    setPtr->pageHeight = pageHeight;
    if (setPtr->reqPageWidth > 0) {
	setPtr->pageWidth = setPtr->reqPageWidth;
    }
    if (setPtr->reqPageHeight > 0) {
	setPtr->pageHeight = setPtr->reqPageHeight;
    }
    return numTabs;
}


static void
AdjustTabSizes(setPtr, numTabs)
    Tabset *setPtr;
    int numTabs;
{
    int tabsPerTier, expand;
    int total, count, extra;
    Tab *startPtr, *nextPtr;
    register Blt_ListItem item;
    register Tab *tabPtr;
    int x, maxWidth;

    tabsPerTier = (numTabs + (setPtr->numTiers - 1)) / setPtr->numTiers;
    x = setPtr->selectPad;
    maxWidth = 0;
    if (setPtr->defAttrs.constWidth) {
	register int i;

	item = Blt_ListFirstItem(&(setPtr->tabList));
	count = 1;
	while (item != NULL) {
	    for (i = 0; i < tabsPerTier; i++) {
		tabPtr = (Tab *) Blt_ListGetValue(item);
		tabPtr->tier = count;
		tabPtr->worldX = x;
		x += tabPtr->worldWidth + setPtr->gap;
		item = Blt_ListNextItem(item);
		if (x > maxWidth) {
		    maxWidth = x;
		}
		if (item == NULL) {
		    goto done;
		}
	    }
	    count++;
	    x = setPtr->selectPad;
	}
    }
  done:
    /* Add to tab widths to fill out row. */
    expand = (((numTabs % tabsPerTier) == 0) || (!setPtr->defAttrs.constWidth));
    if (!expand) {
	return;
    }
    startPtr = NULL;
    count = total = 0;
    for (item = Blt_ListFirstItem(&(setPtr->tabList)); item != NULL; /*empty*/ ) {
	tabPtr = (Tab *) Blt_ListGetValue(item);
	if (startPtr == NULL) {
	    startPtr = tabPtr;
	}
	count++;
	total += tabPtr->worldWidth + setPtr->gap;
	item = Blt_ListNextItem(item);
	if (item != NULL) {
	    nextPtr = (Tab *) Blt_ListGetValue(item);
	    if (tabPtr->tier == nextPtr->tier) {
		continue;
	    }
	}
	extra = setPtr->worldWidth - total;
	assert(count > 0);
	if (extra > 0) {
	    WidenTabs(setPtr, startPtr, count, extra);
	}
	count = total = 0;
	startPtr = NULL;
    }
}

/*
 *
 * tabWidth = textWidth + gap + (2 * (pad + outerBW));
 *
 * tabHeight = textHeight + 2 * (pad + outerBW) + topMargin;
 *
 */
static void
ComputeLayout(setPtr)
    Tabset *setPtr;
{
    int width, height;
    Blt_ListItem item;
    Tab *tabPtr;
    int x, extra;
    int numTiers, numTabs;

    setPtr->numTiers = 0;
    setPtr->pageTop = 0;
    setPtr->worldWidth = 1;
    setPtr->yPad = 0;

    numTiers = 0;
    numTabs = CountTabs(setPtr);
    if (numTabs == 0) {
	return;
    }
    /* Reset the pointers to the selected and starting tab. */
    if (setPtr->selectPtr == NULL) {
	item = Blt_ListFirstItem(&(setPtr->tabList));
	if (item != NULL) {
	    setPtr->selectPtr = (Tab *) Blt_ListGetValue(item);
	}
    }
    if (setPtr->startPtr == NULL) {
	setPtr->startPtr = setPtr->selectPtr;
    }
    if (setPtr->focusPtr == NULL) {
	setPtr->focusPtr = setPtr->selectPtr;
	Blt_SetFocusItem(setPtr->bindTable, setPtr->focusPtr);
    }
    width = Tk_Width(setPtr->tkwin) - (2 * setPtr->inset) -
	setPtr->selectPad - setPtr->corner;
    height = Tk_Height(setPtr->tkwin) - 2 *
	(setPtr->corner + setPtr->selectPad);

    if (setPtr->side & SIDE_VERTICAL) {
	int temp;

	temp = width, width = height, height = temp;
    }
    setPtr->flags |= TABSET_STATIC;
    if (setPtr->reqTiers > 1) {
	int total, maxWidth;

	/* Static multiple tier mode. */

	/* Sum tab widths and determine the number of tiers needed. */
	numTiers = 1;
	total = x = setPtr->selectPad;
	for (item = Blt_ListFirstItem(&(setPtr->tabList)); item != NULL;
	    item = Blt_ListNextItem(item)) {
	    tabPtr = (Tab *) Blt_ListGetValue(item);
	    if ((x + tabPtr->worldWidth) > width) {
		numTiers++;
		x = setPtr->selectPad;
	    }
	    tabPtr->worldX = x;
	    tabPtr->tier = numTiers;
	    extra = tabPtr->worldWidth + setPtr->gap;
	    total += extra, x += extra;
	}
	maxWidth = width;

	if (numTiers > setPtr->reqTiers) {
	    /*
	     * The tabs do not fit into the requested number of tiers.
             * Go into scrolling mode.
	     */
	    width = ((total + setPtr->tabWidth) / setPtr->reqTiers);
	    x = setPtr->selectPad;
	    numTiers = 1;
	    for (item = Blt_ListFirstItem(&(setPtr->tabList)); item != NULL;
		item = Blt_ListNextItem(item)) {
		tabPtr = (Tab *) Blt_ListGetValue(item);
		tabPtr->tier = numTiers;
		/*
		 * Keep adding tabs to a tier until we overfill it.
		 */
		tabPtr->worldX = x;
		x += tabPtr->worldWidth + setPtr->gap;
		if (x > width) {
		    numTiers++;
		    if (x > maxWidth) {
			maxWidth = x;
		    }
		    x = setPtr->selectPad;
		}
	    }
	    setPtr->flags &= ~TABSET_STATIC;
	}
	setPtr->worldWidth = maxWidth;
	setPtr->numTiers = numTiers;

	if (numTiers > 1) {
	    AdjustTabSizes(setPtr, numTabs);
	}
	if (!(setPtr->flags & TABSET_STATIC)) {
	    setPtr->worldWidth += (setPtr->selectPad + setPtr->corner);
	} else {
	    setPtr->worldWidth = VPORTWIDTH(setPtr);
	}
	if (setPtr->selectPtr != NULL) {
	    RenumberTiers(setPtr, setPtr->selectPtr);
	}
    } else {
	/*
	 * Scrollable single tier mode.
	 */
	numTiers = 1;
	x = setPtr->selectPad;
	for (item = Blt_ListFirstItem(&(setPtr->tabList)); item != NULL;
	    item = Blt_ListNextItem(item)) {
	    tabPtr = (Tab *) Blt_ListGetValue(item);
	    tabPtr->tier = numTiers;
	    tabPtr->worldX = x;
	    tabPtr->worldY = 0;
	    x += tabPtr->worldWidth + setPtr->gap;
	}
	setPtr->worldWidth = x + setPtr->corner + setPtr->gap +
	    setPtr->selectPad;
	setPtr->flags &= ~TABSET_STATIC;
	setPtr->yPad = setPtr->selectPad;
    }
    setPtr->numTiers = numTiers;
    setPtr->pageTop = (setPtr->numTiers * setPtr->tabHeight) +
	setPtr->inset + setPtr->pageInset + setPtr->yPad;

    if (setPtr->side & SIDE_VERTICAL) {
	for (item = Blt_ListFirstItem(&(setPtr->tabList)); item != NULL;
	    item = Blt_ListNextItem(item)) {
	    tabPtr = (Tab *) Blt_ListGetValue(item);
	    tabPtr->scrnWidth = (short int)setPtr->tabHeight;
	    tabPtr->scrnHeight = (short int)tabPtr->worldWidth;
	}
    } else {
	for (item = Blt_ListFirstItem(&(setPtr->tabList)); item != NULL;
	    item = Blt_ListNextItem(item)) {
	    tabPtr = (Tab *) Blt_ListGetValue(item);
	    tabPtr->scrnWidth = (short int)tabPtr->worldWidth;
	    tabPtr->scrnHeight = (short int)setPtr->tabHeight;
	}
    }
}

static void
ComputeVisibleTabs(setPtr)
    Tabset *setPtr;
{
    int numVisibleTabs;
    register Tab *tabPtr;
    register Blt_ListItem item;

    setPtr->numVisible = 0;
    if (Blt_ListGetLength(&(setPtr->tabList)) == 0) {
	return;
    }
    numVisibleTabs = 0;
    if (setPtr->flags & TABSET_STATIC) {

	/* Static multiple tier mode. */

	for (item = Blt_ListFirstItem(&(setPtr->tabList)); item != NULL;
	    item = Blt_ListNextItem(item)) {
	    tabPtr = (Tab *) Blt_ListGetValue(item);
	    tabPtr->flags |= TAB_VISIBLE;
	    numVisibleTabs++;
	}
    } else {
	int width, offset;
	/*
	 * Scrollable (single or multiple) tier mode.
	 */
	offset = setPtr->scrollOffset - (OUTER_PAD + setPtr->selectPad);
	width = VPORTWIDTH(setPtr) + setPtr->scrollOffset + 2 * OUTER_PAD;
	for (item = Blt_ListFirstItem(&(setPtr->tabList)); item != NULL;
	    item = Blt_ListNextItem(item)) {
	    tabPtr = (Tab *) Blt_ListGetValue(item);
	    if ((tabPtr->worldX >= width) ||
		((tabPtr->worldX + tabPtr->worldWidth) < offset)) {
		tabPtr->flags &= ~TAB_VISIBLE;
	    } else {
		tabPtr->flags |= TAB_VISIBLE;
		numVisibleTabs++;
	    }
	}
    }
    for (item = Blt_ListFirstItem(&(setPtr->tabList)); item != NULL;
	item = Blt_ListNextItem(item)) {
	tabPtr = (Tab *) Blt_ListGetValue(item);
	tabPtr->scrnX = tabPtr->scrnY = -1000;
	if (tabPtr->flags & TAB_VISIBLE) {
	    WorldToScreen(setPtr, tabPtr->worldX, tabPtr->worldY,
		&(tabPtr->scrnX), &(tabPtr->scrnY));
	    switch (setPtr->side) {
	    case SIDE_RIGHT:
		tabPtr->scrnX -= setPtr->tabHeight;
		break;

	    case SIDE_BOTTOM:
		tabPtr->scrnY -= setPtr->tabHeight;
		break;
	    }
	}
    }
    setPtr->numVisible = numVisibleTabs;
    Blt_PickCurrentItem(setPtr->bindTable);
}


static void
Draw3DFolder(setPtr, tabPtr, drawable, side, pointArr, numPoints)
    Tabset *setPtr;
    Tab *tabPtr;
    Drawable drawable;
    int side;
    XPoint pointArr[];
    int numPoints;
{
    GC gc;
    int relief, borderWidth;
    Tk_3DBorder border;

    if (tabPtr == setPtr->selectPtr) {
	border = GETATTR(tabPtr, selBorder);
    } else {
	border = tabPtr->border;
	if (border == NULL) {
	    border = setPtr->defAttrs.border;
	}
    }
    relief = setPtr->defAttrs.relief;
    if ((side == SIDE_RIGHT) || (side == SIDE_TOP)) {
	borderWidth = -setPtr->defAttrs.borderWidth;
	if (relief == TK_RELIEF_SUNKEN) {
	    relief = TK_RELIEF_RAISED;
	} else if (relief == TK_RELIEF_RAISED) {
	    relief = TK_RELIEF_SUNKEN;
	}
    } else {
	borderWidth = setPtr->defAttrs.borderWidth;
    }
    /* Draw the outline of the folder. */
    gc = Tk_GCForColor(setPtr->shadowColor, drawable);
    XDrawLines(setPtr->display, drawable, gc, pointArr, numPoints,
	CoordModeOrigin);
    /* And the folder itself. */
    if (tabPtr->tileGC != NULL) {
	XFillPolygon(setPtr->display, drawable, tabPtr->tileGC, pointArr,
	    numPoints, Convex, CoordModeOrigin);
	Tk_Draw3DPolygon(setPtr->tkwin, drawable, border, pointArr, numPoints,
	    borderWidth, relief);
    } else {
	Tk_Fill3DPolygon(setPtr->tkwin, drawable, border, pointArr, numPoints,
	    borderWidth, relief);
    }
}

/*
 *   x,y
 *    |1|2|3|   4    |3|2|1|
 *
 *   1. tab border width
 *   2. corner offset
 *   3. label pad
 *   4. label width
 *
 *
 */
static void
DrawLabel(setPtr, tabPtr, drawable)
    Tabset *setPtr;
    Tab *tabPtr;
    Drawable drawable;
{
    int x, y, width, height;
    int dx, dy;
    int textX, textY, imgX, imgY;
    int imgWidth, imgHeight;
    int labelWidth, labelHeight;
    int active, selected;
    XColor *fgColor, *bgColor;
    Tk_3DBorder border;
    XRectangle clipRect;
    int tabInset;

    if (!(tabPtr->flags & TAB_VISIBLE)) {
	return;
    }
    active = (setPtr->activePtr == tabPtr);
    selected = (setPtr->selectPtr == tabPtr);

    fgColor = GETATTR(tabPtr, textColor);
    border = GETATTR(tabPtr, border);
    if (selected) {
	border = GETATTR(tabPtr, selBorder);
    }
    bgColor = Tk_3DBorderColor(border);
    if (active) {
	Tk_3DBorder activeBorder;

	activeBorder = GETATTR(tabPtr, activeBorder);
	bgColor = Tk_3DBorderColor(activeBorder);
    }
    tabInset = setPtr->pageInset;


    x = tabPtr->scrnX + tabInset;
    y = tabPtr->scrnY + tabInset;

    width = tabPtr->scrnWidth - 2 * tabInset;
    height = tabPtr->scrnHeight - 2 * tabInset;
    clipRect.x = x - 1, clipRect.y = y - 1;
    clipRect.width = width + 1, clipRect.height = height + 1;

    {
	GC gc;

	/*
	 * Draw the correct background color.
	 */
	gc = Tk_GCForColor(bgColor, drawable);
	XFillRectangles(setPtr->display, drawable, gc, &clipRect, 1);
	if ((setPtr->flags & TABSET_FOCUS) && (setPtr->focusPtr == tabPtr)) {
	    XDrawRectangles(setPtr->display, drawable,
		setPtr->defAttrs.activeGC, &clipRect, 1);
	}
    }
    if (tabPtr->image != NULL) {
	imgWidth = ImageWidth(tabPtr->image);
	imgHeight = ImageHeight(tabPtr->image);
    } else {
	imgWidth = imgHeight = 0;
    }
    x = tabPtr->scrnX;
    y = tabPtr->scrnY;
    labelWidth = tabPtr->labelWidth + PADDING(tabPtr->iPadX);
    labelHeight = tabPtr->labelHeight + PADDING(tabPtr->iPadY);
    dx = (tabPtr->scrnWidth - labelWidth) / 2;
    dy = (tabPtr->scrnHeight - labelHeight) / 2;

    textX = textY = imgX = imgY = 0;	/* Suppress compiler warning. */
    switch (setPtr->defAttrs.textSide) {
    case SIDE_LEFT:
	textX = x + dx + tabPtr->iPadX.side1;
	textY = y + (tabPtr->scrnHeight - tabPtr->textHeight) / 2;
	imgX = textX + tabPtr->textWidth + IMAGE_PAD;
	imgY = y + (tabPtr->scrnHeight - imgHeight) / 2;
	break;
    case SIDE_RIGHT:
	imgX = x + dx + tabPtr->iPadX.side1;
	imgY = y + (tabPtr->scrnHeight - imgHeight) / 2;
	textX = imgX + imgWidth + IMAGE_PAD;
	textY = y + (tabPtr->scrnHeight - tabPtr->textHeight) / 2;
	break;
    case SIDE_BOTTOM:
	imgY = y + dy + tabPtr->iPadY.side1;
	imgX = x + (tabPtr->scrnWidth - imgWidth) / 2;
	textY = imgY + imgHeight + IMAGE_PAD;
	textX = x + (tabPtr->scrnWidth - tabPtr->textWidth) / 2;
	break;
    case SIDE_TOP:
	textX = x + (tabPtr->scrnWidth - tabPtr->textWidth) / 2;
	textY = y + dy + tabPtr->iPadY.side1;
	imgX = x + (tabPtr->scrnWidth - imgWidth) / 2;
	imgY = textY + tabPtr->textHeight + IMAGE_PAD;
	break;
    }
    if (tabPtr->image != NULL) {
	Tk_RedrawImage(ImageData(tabPtr->image), 0, 0,
	    ImageWidth(tabPtr->image), ImageHeight(tabPtr->image), drawable,
	    imgX, imgY);
    }
    if (tabPtr->text != NULL) {
	TextAttributes attr;
	XColor *activeColor;

	activeColor = fgColor;
	if (selected) {
	    activeColor = GETATTR(tabPtr, selColor);
	} else if (active) {
	    activeColor = GETATTR(tabPtr, activeFgColor);
	}
	Blt_SetTextDrawAttributes(&attr, GETATTR(tabPtr, font), tabPtr->textGC,
	    fgColor, activeColor, tabPtr->shadow.color,
	    setPtr->defAttrs.rotate, TK_ANCHOR_NW, TK_JUSTIFY_LEFT,
	    0, tabPtr->shadow.offset);
	attr.state = tabPtr->state;
	attr.border = border;
	if (selected || active) {
	    attr.state |= STATE_ACTIVE;
	}
	Blt_DrawText(setPtr->tkwin, drawable, tabPtr->text, &attr, textX, textY);
    }
}

#define BottomLeft(px, py) \
	pointPtr->x = (px) + setPtr->corner, pointPtr->y = (py), pointPtr++, \
	pointPtr->x = (px), pointPtr->y = (py) - setPtr->corner, pointPtr++

#define TopLeft(px, py) \
	pointPtr->x = (px), pointPtr->y = (py) + setPtr->corner, pointPtr++, \
	pointPtr->x = (px) + setPtr->corner, pointPtr->y = (py), pointPtr++

#define TopRight(px, py) \
	pointPtr->x = (px) - setPtr->corner, pointPtr->y = (py), pointPtr++, \
	pointPtr->x = (px), pointPtr->y = (py) + setPtr->corner, pointPtr++

#define BottomRight(px, py) \
	pointPtr->x = (px), pointPtr->y = (py) - setPtr->corner, pointPtr++, \
	pointPtr->x = (px) - setPtr->corner, pointPtr->y = (py), pointPtr++

/*
 * From the left edge:
 *
 *   |a|b|c|d|e| f |d|e|g|h| i |h|g|e|d|f|    j    |e|d|c|b|a|
 *
 *	a. highlight ring
 *	b. tabset 3D border
 *	c. outer gap
 *      d. page border
 *	e. page corner
 *	f. gap + select pad
 *	g. label pad x (worldX)
 *	h. internal pad x
 *	i. label width
 *	j. rest of page width
 *
 *  worldX, worldY
 *          |
 *          |
 *          * 4+ . . +5
 *          3+         +6
 *           .         .
 *           .         .
 *   1+. . .2+         +7 . . . .+8
 * 0+                              +9
 *  .                              .
 *  .                              .
 *13+                              +10
 *  12+-------------------------+11
 *
 */
static void
DisplayFolder(setPtr, tabPtr, drawable)
    Tabset *setPtr;
    Tab *tabPtr;
    Drawable drawable;
{
    XPoint pointArr[15];
    XPoint *pointPtr;
    int width, height;
    int xMin, yMax, xMax, yMin, yBot;
    int x, y;
    register int i;
    int numPoints;

    width = VPORTWIDTH(setPtr);
    height = VPORTHEIGHT(setPtr);

    x = tabPtr->worldX;
    y = tabPtr->worldY;

    pointPtr = pointArr;

    /* Remember these are all world coordinates. */
    xMin = setPtr->scrollOffset;
    xMax = xMin + width;
    yBot = setPtr->pageTop - (setPtr->inset + setPtr->yPad);
    yMin = yBot - setPtr->pageInset;

    if (setPtr->pageHeight == 0) {
	yMax = yBot + 2 * setPtr->corner;
    } else {
	yMax = height - setPtr->yPad - 1;
    }
    if (tabPtr != setPtr->selectPtr) {
	/*
	 * Case 1: Unselected tab
	 *
	 * * 3+ . . +4
	 * 2+         +5
	 *  .         .
	 * 1+         +6
	 *   0+ . . +7
	 *
	 */
	BottomLeft(x, yBot);
	TopLeft(x, y);
	TopRight(x + tabPtr->worldWidth, y);
	BottomRight(x + tabPtr->worldWidth, yBot);
	numPoints = 9;

    } else if (!(tabPtr->flags & TAB_VISIBLE)) {
	/*
	 * Case 2: Selected tab not visible in viewport.  Draw folder only.
	 *
	 * * 3+ . . +4
	 * 2+         +5
	 *  .         .
	 * 1+         +6
	 *   0+------+7
	 *
	 */
	TopLeft(xMin, yMin);
	TopRight(xMax, yMin);
	BottomRight(xMax, yMax);
	BottomLeft(xMin, yMax);
	numPoints = 9;

    } else {
	int flags;
	int tabWidth;

	numPoints = 14;
	x -= setPtr->selectPad;
	y -= setPtr->selectPad;
	tabWidth = tabPtr->worldWidth + 2 * setPtr->selectPad;

#define CLIP_NONE	0
#define CLIP_LEFT	(1<<0)
#define CLIP_RIGHT	(1<<1)
	flags = 0;
	if (x < xMin) {
	    flags |= CLIP_LEFT;
	}
	if ((x + tabWidth) > xMax) {
	    flags |= CLIP_RIGHT;
	}
	switch (flags) {
	case CLIP_NONE:
	    /*
	     *  worldX, worldY
	     *          |
	     *          * 4+ . . +5
	     *          3+         +6
	     *           .         .
	     *           .         .
	     *   1+. . .2+         +7 . . . .+8
	     * 0+                              +9
	     *  .                              .
	     *  .                              .
	     *13+                              +10
	     *  12+-------------------------+11
	     */
	    if (x < (xMin + setPtr->corner)) {
		pointPtr->x = xMin, pointPtr->y = yMin, pointPtr++;
	    } else {
		TopLeft(xMin, yMin);
	    }
	    pointPtr->x = x, pointPtr->y = yMin, pointPtr++;
	    TopLeft(x, y);
	    TopRight(x + tabWidth, y);
	    pointPtr->x = x + tabWidth, pointPtr->y = yMin, pointPtr++;
	    if ((x + tabWidth) > (xMax - setPtr->corner)) {
		pointPtr->x = xMax;
		pointPtr->y = yMin + setPtr->corner, pointPtr++;
	    } else {
		TopRight(xMax, yMin);
	    }
	    BottomRight(xMax, yMax);
	    BottomLeft(xMin, yMax);
	    break;

	case CLIP_LEFT:
	    /*
	     *  worldX, worldY
	     *          |
	     *          * 4+ . . +5
	     *          3+         +6
	     *           .         .
	     *           .         .
	     *          2+         +7 . . . .+8
	     *            1+ . . . +0          +9
	     *                     .           .
	     *                     .           .
	     *                   13+           +10
	     *                     12+--------+11
	     */
	    pointPtr->x = xMin, pointPtr->y = yBot, pointPtr++;
	    BottomLeft(x, yBot);
	    TopLeft(x, y);
	    TopRight(x + tabWidth, y);
	    pointPtr->x = x + tabWidth, pointPtr->y = yMin, pointPtr++;
	    TopRight(xMax, yMin);
	    BottomRight(xMax, yMax);
	    BottomLeft(xMin, yMax);
	    break;

	case CLIP_RIGHT:
	    /*
	     *              worldX, worldY
	     *                     |
	     *                     * 9+ . . +10
	     *                     8+         +11
	     *                      .         .
	     *                      .         .
	     *           6+ . . . .7+         +12
	     *         5+          0+ . . . +13
	     *          .           .
	     *          .           .
	     *         4+           +1
	     *           3+-------+2
	     */
	    pointPtr->x = xMax, pointPtr->y = yBot, pointPtr++;
	    BottomRight(xMax, yMax);
	    BottomLeft(xMin, yMax);
	    TopLeft(xMin, yMin);
	    pointPtr->x = x, pointPtr->y = yMin, pointPtr++;
	    TopLeft(x, y);
	    TopRight(x + tabWidth, y);
	    BottomRight(x + tabWidth, yBot);
	    break;

	case (CLIP_LEFT | CLIP_RIGHT):
	    /*
	     *  worldX, worldY
	     *     |
	     *     * 4+ . . . . . . . . +5
	     *     3+                     +6
	     *      .                     .
	     *      .                     .
	     *     1+                     +7
	     *       2+ 0+          +9 .+8
	     *           .          .
	     *           .          .
	     *         13+          +10
	     *          12+-------+11
	     */
	    pointPtr->x = xMin, pointPtr->y = yBot, pointPtr++;
	    BottomLeft(x, yBot);
	    TopLeft(x, y);
	    TopRight(x + tabWidth, y);
	    BottomRight(x + tabWidth, yBot);
	    pointPtr->x = xMax, pointPtr->y = yBot, pointPtr++;
	    BottomRight(xMax, yMax);
	    BottomLeft(xMin, yMax);
	    break;
	}
    }
    pointPtr->x = pointArr[0].x;
    pointPtr->y = pointArr[0].y;
    for (i = 0; i < numPoints; i++) {
	WorldToScreen(setPtr, pointArr[i].x, pointArr[i].y, &x, &y);
	pointArr[i].x = x;
	pointArr[i].y = y;
    }
    Draw3DFolder(setPtr, tabPtr, drawable, setPtr->side, pointArr, numPoints);
    DrawLabel(setPtr, tabPtr, drawable);
    if (tabPtr->container != NULL) {
	XRectangle region;

	/* Draw rectangle covering the spot representing the window  */
	GetWindowRegion(tabPtr, setPtr->tkwin, FALSE, &region);
	XFillRectangles(setPtr->display, drawable, tabPtr->backGC,
	    &region, 1);
    }
}

static void
DrawOuterBorders(setPtr, drawable)
    Tabset *setPtr;
    Drawable drawable;
{
    /*
     * Draw 3D border just inside of the focus highlight ring.  We
     * draw the border even if the relief is flat so that any tabs
     * that hang over the edge will be clipped.
     */
    if (setPtr->borderWidth > 0) {
	Tk_Draw3DRectangle(setPtr->tkwin, drawable, setPtr->border,
	    setPtr->highlightWidth, setPtr->highlightWidth,
	    Tk_Width(setPtr->tkwin) - 2 * setPtr->highlightWidth,
	    Tk_Height(setPtr->tkwin) - 2 * setPtr->highlightWidth,
	    setPtr->borderWidth, setPtr->relief);
    }
    /* Draw focus highlight ring. */
    if (setPtr->highlightWidth > 0) {
	XColor *color;
	GC gc;

	color = (setPtr->flags & TABSET_FOCUS)
	    ? setPtr->highlightColor : setPtr->highlightBgColor;
	gc = Tk_GCForColor(color, drawable);
	Tk_DrawFocusHighlight(setPtr->tkwin, gc, setPtr->highlightWidth,
	    drawable);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * DisplayTabset --
 *
 * 	This procedure is invoked to display the widget.
 *
 *      Recomputes the layout of the widget if necessary. This is
 *	necessary if the world coordinate system has changed.
 *	Sets the vertical and horizontal scrollbars.  This is done
 *	here since the window width and height are needed for the
 *	scrollbar calculations.
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
DisplayTabset(clientData)
    ClientData clientData;	/* Information about widget. */
{
    Tabset *setPtr = (Tabset *) clientData;
    Pixmap drawable;
    int width, height;

    setPtr->flags &= ~TABSET_REDRAW;
    if (setPtr->tkwin == NULL) {
	return;			/* Window has been destroyed. */
    }
    if (setPtr->flags & TABSET_LAYOUT) {
	ComputeLayout(setPtr);
	setPtr->flags &= ~TABSET_LAYOUT;
    }
    if ((setPtr->reqHeight == 0) || (setPtr->reqWidth == 0)) {
	width = height = 0;
	if (setPtr->side & SIDE_VERTICAL) {
	    height = setPtr->worldWidth;
	} else {
	    width = setPtr->worldWidth;
	}
	if (setPtr->reqWidth > 0) {
	    width = setPtr->reqWidth;
	} else if (setPtr->pageWidth > 0) {
	    width = setPtr->pageWidth;
	}
	if (setPtr->reqHeight > 0) {
	    height = setPtr->reqHeight;
	} else if (setPtr->pageHeight > 0) {
	    height = setPtr->pageHeight;
	}
	if (setPtr->side & SIDE_VERTICAL) {
	    width += setPtr->pageTop + setPtr->inset + setPtr->pageInset;
	    height += 2 * (setPtr->inset + setPtr->pageInset);
	} else {
	    height += setPtr->pageTop + setPtr->inset + setPtr->pageInset;
	    width += 2 * (setPtr->inset + setPtr->pageInset);
	}
	if ((Tk_ReqWidth(setPtr->tkwin) != width) ||
	    (Tk_ReqHeight(setPtr->tkwin) != height)) {
	    Tk_GeometryRequest(setPtr->tkwin, width, height);
	}
    }
    if (setPtr->flags & TABSET_SCROLL) {
	width = VPORTWIDTH(setPtr);
	setPtr->scrollOffset = Blt_AdjustViewport(setPtr->scrollOffset,
	    setPtr->worldWidth, width, setPtr->scrollUnits,
	    SCROLL_MODE_CANVAS);
	if (setPtr->scrollCmdPrefix != NULL) {
	    Blt_UpdateScrollbar(setPtr->interp, setPtr->scrollCmdPrefix,
		(double)setPtr->scrollOffset / setPtr->worldWidth,
		(double)(setPtr->scrollOffset + width) / setPtr->worldWidth);
	}
	ComputeVisibleTabs(setPtr);
	setPtr->flags &= ~TABSET_SCROLL;
    }
    if (!Tk_IsMapped(setPtr->tkwin)) {
	return;
    }
    height = Tk_Height(setPtr->tkwin);
    drawable = Tk_GetPixmap(setPtr->display, Tk_WindowId(setPtr->tkwin),
	Tk_Width(setPtr->tkwin), Tk_Height(setPtr->tkwin),
	Tk_Depth(setPtr->tkwin));

    /*
     * Clear the background either by tiling a pixmap or filling with
     * a solid color. Tiling takes precedence.
     */
    if (setPtr->bgTile != NULL) {
	Blt_SetTileOrigin(setPtr->tkwin, setPtr->tileGC, 0, 0);
	XFillRectangle(setPtr->display, drawable, setPtr->tileGC, 0, 0,
	    Tk_Width(setPtr->tkwin), height);
    } else {
	Tk_Fill3DRectangle(setPtr->tkwin, drawable, setPtr->border, 0, 0,
	    Tk_Width(setPtr->tkwin), height, 0, TK_RELIEF_FLAT);
    }

    if (setPtr->numVisible > 0) {
	register int i;
	register Tab *tabPtr;
	register Blt_ListItem item;

	item = setPtr->startPtr->item;
	for (i = 0; i < Blt_ListGetLength(&(setPtr->tabList)); i++) {
	    item = Blt_ListPrevItem(item);
	    if (item == NULL) {
		item = Blt_ListLastItem(&(setPtr->tabList));
	    }
	    tabPtr = (Tab *) Blt_ListGetValue(item);
	    if ((tabPtr != setPtr->selectPtr) &&
		(tabPtr->flags & TAB_VISIBLE)) {
		DisplayFolder(setPtr, tabPtr, drawable);
	    }
	}
	DisplayFolder(setPtr, setPtr->selectPtr, drawable);
	if ((setPtr->selectPtr->tkwin != NULL) &&
	    (setPtr->selectPtr->container == NULL)) {
	    XRectangle region;

	    GetWindowRegion(setPtr->selectPtr, setPtr->tkwin, FALSE, &region);
	    ArrangeWindow(setPtr->selectPtr->tkwin, &region, 0);
	}
    }
    DrawOuterBorders(setPtr, drawable);
    XCopyArea(setPtr->display, drawable, Tk_WindowId(setPtr->tkwin),
	setPtr->highlightGC, 0, 0, Tk_Width(setPtr->tkwin), height, 0, 0);
    Tk_FreePixmap(setPtr->display, drawable);
}

/*
 * From the left edge:
 *
 *   |a|b|c|d|e| f |d|e|g|h| i |h|g|e|d|f|    j    |e|d|c|b|a|
 *
 *	a. highlight ring
 *	b. tabset 3D border
 *	c. outer gap
 *      d. page border
 *	e. page corner
 *	f. gap + select pad
 *	g. label pad x (worldX)
 *	h. internal pad x
 *	i. label width
 *	j. rest of page width
 *
 *  worldX, worldY
 *          |
 *          |
 *          * 4+ . . +5
 *          3+         +6
 *           .         .
 *           .         .
 *   1+. . .2+         +7 . . . .+8
 * 0+                              +9
 *  .                              .
 *  .                              .
 *13+                              +10
 *  12+-------------------------+11
 *
 */
static void
DisplayTearoff(clientData)
    ClientData clientData;
{
    Tabset *setPtr;
    Tab *tabPtr;
    Drawable drawable;
    XPoint pointArr[15];
    XPoint *pointPtr;
    int width, height;
    int xMin, yMax, xMax, yMin;
    int x, y;
    int numPoints;
    Tk_Window tkwin;

    tabPtr = (Tab *) clientData;
    if (tabPtr == NULL) {
	return;
    }
    tabPtr->flags &= ~TAB_REDRAW;
    setPtr = tabPtr->setPtr;
    if (setPtr->tkwin == NULL) {
	return;
    }
    tkwin = tabPtr->container;
    drawable = Tk_WindowId(tkwin);

    /*
     * Clear the background either by tiling a pixmap or filling with
     * a solid color. Tiling takes precedence.
     */
    if (setPtr->bgTile != NULL) {
	Blt_SetTileOrigin(tkwin, setPtr->tileGC, 0, 0);
	XFillRectangle(setPtr->display, drawable, setPtr->tileGC, 0, 0,
	    Tk_Width(tkwin), Tk_Height(tkwin));
    } else {
	Tk_Fill3DRectangle(setPtr->tkwin, drawable, setPtr->border, 0, 0,
	    Tk_Width(tkwin), Tk_Height(tkwin), 0, TK_RELIEF_FLAT);
    }

    width = Tk_Width(tkwin) - 2 * setPtr->inset;
    height = Tk_Height(tkwin) - 2 * setPtr->inset;
    x = setPtr->inset + setPtr->gap + setPtr->corner;
    y = setPtr->inset;

    pointPtr = pointArr;

    xMin = setPtr->inset;
    xMax = setPtr->inset + width;
    yMin = setPtr->inset + setPtr->corner + setPtr->selectPad;
    yMax = setPtr->inset + height;

    numPoints = 14;

    /*
     *  worldX, worldY
     *          |
     *          * 4+ . . +5
     *          3+         +6
     *           .         .
     *           .         .
     *   1+. . .2+         +7 . . . .+8
     * 0+                              +9
     *  .                              .
     *  .                              .
     *13+                              +10
     *  12+-------------------------+11
     */
    TopLeft(xMin, yMin);
    pointPtr->x = x, pointPtr->y = yMin, pointPtr++;
    TopLeft(x, y);
    TopRight(x + tabPtr->worldWidth, y);
    pointPtr->x = x + tabPtr->worldWidth, pointPtr->y = yMin, pointPtr++;
    TopRight(xMax, yMin);
    BottomRight(xMax, yMax);
    BottomLeft(xMin, yMax);
    pointPtr->x = pointArr[0].x;
    pointPtr->y = pointArr[0].y;
    Draw3DFolder(setPtr, tabPtr, drawable, SIDE_TOP, pointArr, numPoints);

    {
	Tk_Window parent;
	XRectangle region;

	parent = (tabPtr->container == NULL)
	    ? setPtr->tkwin : tabPtr->container;
	GetWindowRegion(tabPtr, parent, TRUE, &region);
	ArrangeWindow(tabPtr->tkwin, &region, TRUE);
    }
    /* Draw 3D border. */
    if ((setPtr->borderWidth > 0) && (setPtr->relief != TK_RELIEF_FLAT)) {
	Tk_Draw3DRectangle(tkwin, drawable, setPtr->border, 0, 0,
	    Tk_Width(tkwin), Tk_Height(tkwin), setPtr->borderWidth,
	    setPtr->relief);
    }
}

/*
 * --------------------------------------------------------------
 *
 * TabsetCmd --
 *
 * 	This procedure is invoked to process the "tabset" command.
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
    {"activate", 1, (Blt_Operation)ActivateOp, 3, 3, "tab",},
    {"bind", 1, (Blt_Operation)BindOp, 3, 5, "tab ?sequence command?",},
    {"cget", 2, (Blt_Operation)CgetOp, 3, 3, "option",},
    {"configure", 2, (Blt_Operation)ConfigureOp, 2, 0, "?option value?...",},
    {"delete", 1, (Blt_Operation)DeleteOp, 2, 0, "firstTab ?lastTab?",},
    {"focus", 1, (Blt_Operation)FocusOp, 3, 3, "tab",},
    {"get", 1, (Blt_Operation)GetOp, 3, 3, "tab",},
    {"index", 3, (Blt_Operation)IndexOp, 3, 5, "string",},
    {"insert", 3, (Blt_Operation)InsertOp, 3, 0,
	"position label ?label...? ?option value?",},
    {"invoke", 3, (Blt_Operation)InvokeOp, 3, 3, "tab",},
    {"move", 1, (Blt_Operation)MoveOp, 5, 5, "tab after|before tab",},
    {"nearest", 1, (Blt_Operation)NearestOp, 4, 4, "x y",},
    {"scan", 2, (Blt_Operation)ScanOp, 5, 5, "dragto|mark x y",},
    {"see", 2, (Blt_Operation)SeeOp, 3, 3, "tab",},
    {"size", 2, (Blt_Operation)SizeOp, 2, 2, "",},
    {"tab", 1, (Blt_Operation)TabOp, 2, 0, "oper args",},
    {"view", 1, (Blt_Operation)ViewOp, 2, 5,
	"?moveto fract? ?scroll number what?",},
};

static int numSpecs = sizeof(operSpecs) / sizeof(Blt_OpSpec);

static int
TabsetInstCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Information about the widget. */
    Tcl_Interp *interp;		/* Interpreter to report errors back to. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Vector of argument strings. */
{
    Blt_Operation proc;
    Tabset *setPtr = (Tabset *) clientData;
    int result;

    proc = Blt_GetOperation(interp, numSpecs, operSpecs,
	BLT_OPER_ARG1, argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    Tk_Preserve((ClientData)setPtr);
    result = (*proc) (setPtr, interp, argc, argv);
    Tk_Release((ClientData)setPtr);
    return result;
}

int
Blt_TabsetInit(interp)
    Tcl_Interp *interp;
{
    static Blt_CmdSpec cmdSpec =
    {
	"tabset", TabsetCmd,
    };

    if (Blt_InitCmd(interp, "blt", &cmdSpec) == NULL) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

#endif /* NO_TABSET */
