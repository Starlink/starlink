
/*
 * bltHier.c --
 *
 *	This module implements an hierarchy widget for the BLT toolkit.
 *
 * Copyright -1998 Lucent Technologies, Inc.
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
 * The "hierbox" widget was created by George A. Howlett (gah@bell-labs.com)
 */

/*
 * TODO:
 *
 *   1. "move" or "rename" operation to move or rename nodes.
 *
 *	.h move 20 into 0 at 2
 *	.h move 20 after 0
 *	.h move 20 before 0
 *	.h move 20 0
 *
 *	Can't move ancestor to descendant.
 *	Can't move before root.
 *
 * BUGS:
 *   1.  "see" operation should not scroll automatically.
 *   2.
 */

#include "bltInt.h"

#ifndef NO_HIERBOX

#include "bltList.h"
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <ctype.h>

/*
 * The macro below is used to modify a "char" value (e.g. by casting
 * it to an unsigned character) so that it can be used safely with
 * macros such as isspace.
 */
#define UCHAR(c) ((unsigned char) (c))

#define APPLY_BEFORE		(1<<0)
#define APPLY_OPEN_ONLY		(1<<1)
#define APPLY_RECURSE		(1<<2)

#define BUTTON_IPAD		1
#define BUTTON_SIZE		9
#define ICON_PADX		2
#define ICON_PADY		1
#define LABEL_PADX		3
#define LABEL_PADY		1

#define CLAMP(val,low,hi)	\
	(((val) < (low)) ? (low) : ((val) > (hi)) ? (hi) : (val))
#define ODD(x)			((x) | 0x01)
#define POSITION_END		(INT_MAX)
#define TOGGLE(x, mask)		\
	(((x) & (mask)) ? ((x) & ~(mask)) : ((x) | (mask)))

#define VPORTWIDTH(h)		(Tk_Width((h)->tkwin) - 2 * (h)->inset)
#define VPORTHEIGHT(h)		(Tk_Height((h)->tkwin) - 2 * (h)->inset)

#define WORLDX(h, sx)		((sx) - (h)->inset + (h)->xOffset)
#define WORLDY(h, sy)		((sy) - (h)->inset + (h)->yOffset)

#define SCREENX(h, wx)		((wx) - (h)->xOffset + (h)->inset)
#define SCREENY(h, wy)		((wy) - (h)->yOffset + (h)->inset)

#define LEVELWIDTH(d)		(hierPtr->levelInfo[(d)].width)
#define LEVELX(d)		(hierPtr->levelInfo[(d)].x)

/*
 * ----------------------------------------------------------------------------
 *
 *  Internal hierarchy widget flags:
 *
 *	HIER_LAYOUT		The layout of the hierarchy needs to be
 *				recomputed.
 *
 *	HIER_REDRAW		A redraw request is pending for the widget.
 *
 *	HIER_XSCROLL		X-scroll request is pending.
 *	HIER_YSCROLL		Y-scroll request is pending.
 *	HIER_SCROLL		Both X-scroll and  Y-scroll requests are
 *				pending.
 *
 *	HIER_FOCUS		The widget is receiving keyboard events.
 *				Draw the focus highlight border around the
 *				widget.
 *
 *	HIER_DIRTY		The hierarchy has changed, possibly invalidating
 *				locations and pointers to entries.  This widget
 *				need to recompute its layout.
 *
 *	HIER_BORDERS		The borders of the widget (highlight ring and
 *				3-D border) need to be redrawn.
 *
*
 *  Selection related flags:
 *
 *	SELECT_EXPORT		Export the selection to X.
 *
 *	SELECT_PENDING		A selection command idle task is pending.
 *
 *	SELECT_CLEAR		Entry's selection flag is to be cleared.
 *
 *	SELECT_SET		Entry's selection flag is to be set.
 *
 *	SELECT_TOGGLE		Entry's selection flag is to be toggled.
 *
 *	SELECT_MASK		Mask of selection set/clear/toggle flags.
 *
 * ---------------------------------------------------------------------------
 */
#define HIER_LAYOUT		(1<<0)
#define HIER_REDRAW		(1<<1)
#define HIER_XSCROLL		(1<<2)
#define HIER_YSCROLL		(1<<3)
#define HIER_SCROLL		(HIER_XSCROLL | HIER_YSCROLL)
#define HIER_FOCUS		(1<<4)
#define HIER_DIRTY		(1<<5)
#define HIER_BORDERS		(1<<6)

#define SELECT_PENDING		(1<<15)
#define SELECT_EXPORT		(1<<16)
#define SELECT_CLEAR		(1<<17)
#define SELECT_SET		(1<<18)
#define SELECT_TOGGLE		(SELECT_SET | SELECT_CLEAR)
#define SELECT_MASK		(SELECT_SET | SELECT_CLEAR)

/*
 * -------------------------------------------------------------------------
 *
 *  Internal entry flags:
 *
 *	ENTRY_BUTTON		Indicates that a button needs to be
 *				drawn for this entry.
 *
 *	ENTRY_SELECTED		Indicates that the entry has been
 *				selected and should be drawn with a
 *				selected background.
 *
 *	ENTRY_OPEN		Indicates that the entry is open and
 *				its subentries should also be displayed.
 *
 *	ENTRY_MAPPED		Indicates that the entry is mapped (i.e.
 *				can be viewed by opening or scrolling.
 *
 *	BUTTON_AUTO
 *	BUTTON_SHOW
 *	BUTTON_MASK
 *
 * -------------------------------------------------------------------------
 */
#define ENTRY_BUTTON	(1<<0)
#define ENTRY_SELECTED	(1<<1)
#define ENTRY_OPEN	(1<<2)
#define ENTRY_MAPPED	(1<<3)

#define BUTTON_AUTO	(1<<8)
#define BUTTON_SHOW	(1<<9)
#define BUTTON_MASK	(BUTTON_AUTO | BUTTON_SHOW)

#define DEF_HIER_ACTIVE_BG_COLOR	RGB_COLOR_GREY90
#define DEF_HIER_ACTIVE_BG_MONO		STD_MONO_ACTIVE_BG
#define DEF_HIER_ACTIVE_FG_COLOR	STD_COLOR_ACTIVE_FG
#define DEF_HIER_ACTIVE_FG_MONO		STD_MONO_ACTIVE_FG
#define DEF_HIER_ACTIVE_RELIEF		"flat"
#define DEF_HIER_BACKGROUND		STD_MONO_NORMAL_BG
#define DEF_HIER_BORDER_WIDTH		STD_BORDERWIDTH
#define DEF_HIER_MAKE_PATH		"no"
#define DEF_HIER_CLOSE_IMAGE		(char *)NULL
#define DEF_HIER_COMMAND		(char *)NULL

#define DEF_HIER_CURSOR			(char *)NULL
#define DEF_HIER_DASHES			"dot"
#define DEF_HIER_ACTIVE_DASHES		"dash"
#define DEF_HIER_EXPORT_SELECTION	"no"
#define DEF_HIER_FG_COLOR		STD_COLOR_NORMAL_FG
#define DEF_HIER_FG_MONO		STD_MONO_NORMAL_FG
#define DEF_HIER_FONT			STD_FONT
#define DEF_HIER_BUTTONS		(char *)NULL
#define DEF_HIER_BUTTON_ACTIVE_BG_COLOR RGB_COLOR_GREY70
#define DEF_HIER_BUTTON_ACTIVE_BG_MONO  STD_MONO_ACTIVE_BG
#define DEF_HIER_BUTTON_ACTIVE_FG_COLOR	STD_COLOR_ACTIVE_FG
#define DEF_HIER_BUTTON_ACTIVE_FG_MONO	STD_MONO_ACTIVE_FG
#define DEF_HIER_BUTTON_BORDER_WIDTH	"1"
#define DEF_HIER_BUTTON_NORMAL_BG_COLOR RGB_COLOR_GREY85
#define DEF_HIER_BUTTON_NORMAL_BG_MONO  STD_MONO_NORMAL_BG
#define DEF_HIER_BUTTON_NORMAL_FG_COLOR STD_COLOR_NORMAL_FG
#define DEF_HIER_BUTTON_NORMAL_FG_MONO  STD_MONO_NORMAL_FG
#define DEF_HIER_HEIGHT			"400"
#define DEF_HIER_HIDE_ROOT		"no"
#define DEF_HIER_HIGHLIGHT_BG_COLOR	STD_COLOR_NORMAL_BG
#define DEF_HIER_HIGHLIGHT_BG_MONO	STD_MONO_NORMAL_BG
#define DEF_HIER_HIGHLIGHT_COLOR	RGB_COLOR_BLACK
#define DEF_HIER_HIGHLIGHT_WIDTH	"2"
#define DEF_HIER_ALLOW_DUPLICATES	"yes"
#define DEF_HIER_LINE_COLOR		RGB_COLOR_BLACK
#define DEF_HIER_LINE_MONO		STD_MONO_NORMAL_FG
#define DEF_HIER_LINE_SPACING		"0"
#define DEF_HIER_LINE_WIDTH		"1"
#define DEF_HIER_NORMAL_BG_COLOR 	STD_COLOR_NORMAL_BG
#define DEF_HIER_NORMAL_FG_MONO		STD_MONO_ACTIVE_FG
#define DEF_HIER_RELIEF			"sunken"
#define DEF_HIER_SCROLL_INCREMENT 	"0"
#define DEF_HIER_SCROLL_MODE		"listbox"
#define DEF_HIER_SELECT_BG_COLOR 	RGB_COLOR_LIGHTSKYBLUE1
#define DEF_HIER_SELECT_BG_MONO  	STD_MONO_SELECT_BG
#define DEF_HIER_SELECT_BORDER_WIDTH 	"1"
#define DEF_HIER_SELECT_FG_COLOR 	STD_COLOR_SELECT_FG
#define DEF_HIER_SELECT_FG_MONO  	STD_MONO_SELECT_FG
#define DEF_HIER_SELECT_CMD		(char *)NULL
#define DEF_HIER_SELECT_MODE		"multiple"
#define DEF_HIER_SEPARATOR		(char *)NULL
#define DEF_HIER_SHOW_ROOT		"yes"
#define DEF_HIER_TRIMLEFT		""
#define DEF_HIER_TAKE_FOCUS		"1"
#define DEF_HIER_TEXT_COLOR		STD_COLOR_NORMAL_FG
#define DEF_HIER_TEXT_MONO		STD_MONO_NORMAL_FG
#define DEF_HIER_TILE			(char *)NULL
#define DEF_HIER_SCROLL_TILE		"yes"
#define DEF_HIER_WIDTH			"200"
#define DEF_HIER_OPEN_RELIEF		"sunken"
#define DEF_HIER_CLOSE_RELIEF		"raised"

#define DEF_ENTRY_BG_COLOR		(char *)NULL
#define DEF_ENTRY_BG_MONO		(char *)NULL
#define DEF_ENTRY_BIND_TAGS		"all"
#define DEF_ENTRY_COMMAND		(char *)NULL
#define DEF_ENTRY_DATA			(char *)NULL
#define DEF_ENTRY_FG_COLOR		(char *)NULL
#define DEF_ENTRY_FG_MONO		(char *)NULL
#define DEF_ENTRY_FONT			(char *)NULL
#define DEF_ENTRY_BUTTON		"auto"
#define DEF_ENTRY_ICONS			(char *)NULL
#define DEF_ENTRY_IMAGES		(char *)NULL
#define DEF_ENTRY_LABEL			(char *)NULL
#define DEF_ENTRY_SHADOW_COLOR		(char *)NULL
#define DEF_ENTRY_SHADOW_MONO		(char *)NULL
#define DEF_ENTRY_TEXT			(char *)NULL

typedef struct Hierbox Hierbox;
typedef struct Entry Entry;
typedef struct Tree Tree;

typedef int (CompareProc) _ANSI_ARGS_((Tcl_Interp *interp, char *name,
	char *pattern));
typedef int (ApplyProc) _ANSI_ARGS_((Hierbox * hierPtr, Tree * treePtr));
typedef Tree *(IterProc) _ANSI_ARGS_((Tree * treePtr, unsigned int mask));

extern Tk_CustomOption bltDashesOption;
extern Tk_CustomOption bltLengthOption;
extern Tk_CustomOption bltShadowOption;
extern Tk_CustomOption bltTileOption;
extern Tk_CustomOption bltUidOption;

static int StringToButton _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *string, char *widgRec,
	int offset));
static char *ButtonToString _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtrPtr));

static int StringToImages _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *string, char *widgRec,
	int offset));
static char *ImagesToString _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtrPtr));

static int StringToScrollMode _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *string, char *widgRec,
	int offset));
static char *ScrollModeToString _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtrPtr));

/*
 * Contains a pointer to the widget that's currently being configured.
 * This is used in the custom configuration parse routine for images.
 */
static Hierbox *hierBox;

static Tk_CustomOption imagesOption =
{
    StringToImages, ImagesToString, (ClientData)&hierBox,
};

static Tk_CustomOption buttonOption =
{
    StringToButton, ButtonToString, (ClientData)0,
};

static Tk_CustomOption scrollModeOption =
{
    StringToScrollMode, ScrollModeToString, (ClientData)0,
};

/*
 * HierImage --
 *
 *	Hash table of images. Maintains a reference count for each
 *	image.  Needed to workaround slow insertion/deletion of
 *	instances of a Tk image.
 *
 *	It's likely that the hierarchy widget will use many instances
 *	of the same image (for example the open/close icons). Unlike
 *	most Tk resources (such as colors or fonts) these instances
 *	aren't cached.
 *
 *	Since image instances can be displayed in different windows,
 *	with each window possibly having different color palettes, Tk
 *	stores instances internally in a linked list.  This adds lots
 *	of overhead, especially when the instances are used in the
 *	same widget (and hence use the same palette).  First, we never
 *	need more than one instance of an image, regardless of the
 *	number of places we use it.  Worse, searching/deleting
 *	instances in the linked list is very slow as more instances
 *	are added.
 */
typedef struct HierImage {
    int refCount;		/* Reference counter for this image. */
    Tk_Image tkImage;		/* The Tk image being cached. */
    short int width, height;	/* Dimensions of the cached image. */
    Tcl_HashEntry *hashPtr;	/* Hash table pointer to the image. */
}        *HierImage;

#define ImageHeight(image)		((image)->height)
#define ImageWidth(image)		((image)->width)
#define ImageData(image)		((image)->tkImage)

/*
 * Tree --
 *
 *	Structure representing a general order tree.
 *
 */
struct Tree {
    Tk_Uid nameId;		/* String identifying the node within this
				 * hierarchy.  Does not necessarily have to
				 * be unique. */

    Entry *entryPtr;		/* Points to the entry structure at this
				 * node. */

    struct Tree *parentPtr;	/* Pointer to the parent node.
				 * If NULL, this is the root node. */

    Blt_List *nodeList;		/* Pointer to list of child nodes. The list
				 * isn't allocated until the child nodes are
				 * to be inserted. */

    Blt_ListItem item;		/* List item containing this node in parent.
				 * This is used to remove the node from
				 * its parent when destroying the node. */

    short int level;		/* The level of this node in the tree. */
};

/*
 * Entry --
 *
 *	Contains data-specific information how to represent the data
 *	of a node of the hierarchy.
 *
 */
struct Entry {
    int worldX, worldY;		/* X-Y position in world coordinates
				 * where the entry is positioned. */

    short int width, height;	/* Dimensions of the entry. */

    int lineHeight;		/* Length of the vertical line segment. */

    unsigned int flags;		/* Flags for this entry. For the
				 * definitions of the various bit
				 * fields see below. */

    Tk_Uid data;		/* This value isn't used in C code.
				 * It may be used in Tcl bindings to
				 * associate extra data (other than
				 * the label, image, or text) with the
				 * entry. */

    Tk_Uid tags;		/* List of binding tags for this entry. */

    Tcl_HashEntry *hashPtr;

    Hierbox *hierPtr;

    Tk_Uid openCmd, closeCmd;	/* Tcl commands to invoke when entries
				 * are opened or closed. They override
				 * those specified globally. */
    /*
     * Button information:
     */
    short int buttonX, buttonY;	/* X-Y coordinate offsets from to
				 * upper left corner of the entry to
				 * the upper-left corner of the
				 * button.  Used to pick the
				 * button. */

    HierImage *icons;		/* Tk images displayed for the entry.
				 * The first image is the icon
				 * displayed to the left of the
				 * entry's label. The second is icon
				 * displayed when entry is
				 * "active". */

    GC iconGC;			/* GC for drawing default bitmaps. */

    short int iconWidth, iconHeight;
    /* Maximum dimensions for icons and
				 * buttons for this entry. This is
				 * used to align the button, icon, and
				 * text. */

    short int levelX;		/* X-coordinate offset of data image
				 * or text for children nodes. */
    /*
     * Label information:
     */
    Tk_Uid label;		/* Text displayed right of the icon. */

    short int labelWidth;	/* Width of label. */

    Tk_Font labelFont;		/* Font of label. Overrides global font
				 * specification. */

    XColor *labelColor;		/* Color of label. Overrides global
				 * foreground color specification. */

    GC labelGC;

    Shadow labelShadow;

    /*
     * Data (text or image) information:
     */
    Tk_Uid text;

    Tk_Font textFont;

    XColor *textColor;

    Shadow textShadow;

    GC textGC;

    HierImage *images;

};


static Tk_ConfigSpec entryConfigSpecs[] =
{
    {TK_CONFIG_CUSTOM, "-bindtags", "bindTags", "BindTags",
	DEF_ENTRY_BIND_TAGS, Tk_Offset(Entry, tags),
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_CUSTOM, "-closecommand", "entryCloseCommand",
	"EntryCloseCommand",
	DEF_ENTRY_COMMAND, Tk_Offset(Entry, closeCmd),
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_CUSTOM, "-data", "data", "data",
	DEF_ENTRY_DATA, Tk_Offset(Entry, data),
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_CUSTOM, "-button", "button", "Button",
	DEF_ENTRY_BUTTON, Tk_Offset(Entry, flags),
	TK_CONFIG_DONT_SET_DEFAULT, &buttonOption},
    {TK_CONFIG_CUSTOM, "-icons", "icons", "Icons",
	DEF_ENTRY_ICONS, Tk_Offset(Entry, icons),
	TK_CONFIG_NULL_OK, &imagesOption},
    {TK_CONFIG_CUSTOM, "-images", "images", "Images",
	DEF_ENTRY_IMAGES, Tk_Offset(Entry, images),
	TK_CONFIG_NULL_OK, &imagesOption},
    {TK_CONFIG_CUSTOM, "-label", "label", "Label",
	DEF_ENTRY_LABEL, Tk_Offset(Entry, label),
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_COLOR, "-labelcolor", "labelColor", "LabelColor",
	DEF_ENTRY_FG_COLOR, Tk_Offset(Entry, labelColor),
	TK_CONFIG_NULL_OK | TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-labelcolor", "labelColor", "LabelColor",
	DEF_ENTRY_FG_MONO, Tk_Offset(Entry, labelColor),
	TK_CONFIG_NULL_OK | TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_FONT, "-labelfont", "labelFont", "LabelFont",
	DEF_ENTRY_FONT, Tk_Offset(Entry, labelFont), 0},
    {TK_CONFIG_CUSTOM, "-labelshadow", "labelShadow", "LabelShadow",
	DEF_ENTRY_SHADOW_COLOR, Tk_Offset(Entry, labelShadow),
	TK_CONFIG_NULL_OK | TK_CONFIG_COLOR_ONLY, &bltShadowOption},
    {TK_CONFIG_CUSTOM, "-labelshadow", "labelShadow", "LabelShadow",
	DEF_ENTRY_SHADOW_MONO, Tk_Offset(Entry, labelShadow),
	TK_CONFIG_NULL_OK | TK_CONFIG_MONO_ONLY, &bltShadowOption},
    {TK_CONFIG_CUSTOM, "-opencommand", "entryOpenCommand", "EntryOpenCommand",
	DEF_ENTRY_COMMAND, Tk_Offset(Entry, openCmd),
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_CUSTOM, "-text", "text", "Text",
	DEF_ENTRY_LABEL, Tk_Offset(Entry, text),
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_COLOR, "-textcolor", "textColor", "TextColor",
	DEF_ENTRY_FG_COLOR, Tk_Offset(Entry, textColor),
	TK_CONFIG_NULL_OK | TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_FONT, "-textfont", "textFont", "TextFont",
	DEF_ENTRY_FONT, Tk_Offset(Entry, textFont), 0},
    {TK_CONFIG_CUSTOM, "-textshadow", "textShadow", "Shadow",
	DEF_ENTRY_SHADOW_COLOR, Tk_Offset(Entry, textShadow),
	TK_CONFIG_NULL_OK | TK_CONFIG_COLOR_ONLY, &bltShadowOption},
    {TK_CONFIG_CUSTOM, "-textShadow", "textShadow", "Shadow",
	DEF_ENTRY_SHADOW_MONO, Tk_Offset(Entry, textShadow),
	TK_CONFIG_NULL_OK | TK_CONFIG_MONO_ONLY, &bltShadowOption},
    {TK_CONFIG_END, (char *)NULL, (char *)NULL, (char *)NULL,
	(char *)NULL, 0, 0}
};

/*
 * Button --
 *
 *	A button is the open/close indicator at the far left of the
 *	entry.  It is displayed as a plus or minus in a solid
 *	colored box with optionally an border. It has both "active"
 *	and "normal" colors.
 */
typedef struct Button {
    XColor *fgColor;		/* Foreground color. */
    Tk_3DBorder border;		/* Background color. */
    XColor *activeFgColor;	/* Active foreground color. */
    Tk_3DBorder activeBorder;	/* Active background color. */
    GC normalGC, activeGC;
    Dashes dashes;
    int borderWidth;
} Button;

typedef struct LevelInfo {
    int x;
    int width;
} LevelInfo;

/*
 * Hierbox --
 *
 *	Represents the hierarchy widget which may contain one or more
 *	entries.
 *
 *	World coordinates refer to the X-Y coordinates of the entry
 *	position in the virtual world.  Coordinate 0,0 is the top of the
 *	hierarchy box and the bottom is the bottom of the last entry.
 *
 *	Note that world coordinates are integers, not signed short
 *	integers like X11 screen coordinates.  World coordinates are
 *	translated to screen coordinates by subtracting the offset
 *	from the upper-left corner of the view port.  The view port is
 *	the Tk window, the Hierbox's xOffset and yOffset fields are
 *	the world coordinates of the view port.  Scrolling the
 *	viewport is therefore just changing the xOffset and/or yOffset
 *	fields and redrawing.
 *
 */
struct Hierbox {
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

    int flags;			/* For bitfield definitions, see below */

    int allowDuplicates;	/* Allow duplicate entries. */

    int autoCreate;		/* Automatically create path entries
				 * as needed. */
    int exportSelection;	/* Export the selection to X. */

    int hideRoot;		/* Don't display the root entry. */

    int scrollTile;		/* Adjust the tile along with viewport
				 * offsets as the widget is
				 * scrolled. */

    int inset;			/* Total width of all borders,
				 * including traversal highlight and
				 * 3-D border.  Indicates how much
				 * interior stuff must be offset from
				 * outside edges to leave room for
				 * borders. */

    Tk_3DBorder border;		/* 3D border surrounding the window
				 * (viewport). */
    int borderWidth;		/* Width of 3D border. */
    int relief;			/* 3D border relief. */


    int highlightWidth;		/* Width in pixels of highlight to draw
				 * around widget when it has the focus.
				 * <= 0 means don't draw a highlight. */
    XColor *highlightBgColor;	/* Color for drawing traversal highlight
				 * area when highlight is off. */
    XColor *highlightColor;	/* Color for drawing traversal highlight. */

    Blt_Tile bgTile;		/* Tiled background */

    char *separator;		/* Pathname separators */
    char *trimLeft;		/* Leading characters to trim from
				 * pathnames */

    /*
     * Entries are connected by horizontal and vertical lines. They
     * may be drawn dashed or solid.
     */
    int lineWidth;		/* Width of lines connecting entries */
    int dashes;			/* Dash on-off value. */
    XColor *lineColor;		/* Color of connecting lines. */

    /*
     * Button Information:
     *
     * The button is the open/close indicator at the far left of the
     * entry.  It is usually displayed as a plus or minus in a solid
     * colored box with optionally an border. It has both "active"
     * and "normal" colors.
     */
    int buttonWidth, buttonHeight;
    Button button;

    HierImage *buttons;
    int openRelief, closeRelief;

    /*
     * Selection Information:
     *
     * The selection is the rectangle that contains a selected entry.
     * There may be many selected entries.  It is displayed as a
     * solid colored box with optionally a 3D border.
     */
    Tk_3DBorder selBorder;	/* Background color of an highlighted entry.*/

    int selRelief;		/* Relief of selected items. Currently
				 * is always raised. */

    int selBorderWidth;		/* Border width of a selected entry.*/

    XColor *selFgColor;		/* Text color of an highlighted entry. */

    Tree *selAnchorPtr;		/* Fixed end of selection (i.e. node
				 * at which selection was started.) */
    Tree *selMarkPtr;

    char *selectMode;		/* Selection style.  This value isn't
				 * used in C code (it's just a place
				 * holder), but for the widget's Tcl
				 * bindings. */

    char *selectCmd;		/* Tcl script that's invoked whenever
				 * the selection changes. */

    int leader;			/* Number of pixels padding between entries */

    Tk_Cursor cursor;		/* X Cursor */

    int reqWidth, reqHeight;	/* Requested dimensions of the widget's
				 * window. */

    GC tileGC;			/* GC for tiling the window. The
				 * foreground attribute is the default
				 * text color. */

    GC highlightGC;		/* GC for focus highlight. */

    GC lineGC;			/* GC for drawing dotted line between
				 * entries. */

    XColor *activeFgColor;
    Tk_3DBorder activeBorder;
    int activeRelief;
    GC activeGC;		/* Graphics context for the active label */

    Tree *activePtr;
    Tree *focusPtr;		/* Pointer to node that's currently active. */
    Tree *activeButtonPtr;	/* Pointer to last active button */

    /* Number of pixels to move for 1 scroll unit. */
    int reqScrollX, reqScrollY;
    int xScrollUnits, yScrollUnits;

    /* Command strings to control horizontal and vertical scrollbars. */
    char *xScrollCmdPrefix, *yScrollCmdPrefix;

    int scrollMode;		/* Mode of scrolling: either
				 * SCROLL_MODE_LISTBOX or
				 * SCROLL_MODE_CANVAS. */

    /*
     * Total size of all "open" entries. This represents the range of
     * world coordinates.
     */
    int worldWidth, worldHeight;
    int xOffset, yOffset;	/* Translation between view port and
				 * world origin. */
    int minHeight;		/* Minimum entry height. Used to
				 * to compute what the y-scroll unit
				 * should be. */
    LevelInfo *levelInfo;
    /*
     * Scanning information:
     */
    int scanAnchorX, scanAnchorY;
    /* Scan anchor in screen coordinates. */
    int scanX, scanY;		/* X-Y world coordinate where the scan
				 * started. */


    Tcl_HashTable nodeTable;	/* Table of node identifiers */
    Tcl_HashTable imageTable;	/* Table of Tk images */
    Tree *rootPtr;		/* Root of hierarchy */
    int depth;			/* Maximum depth of the hierarchy. */

    Tree **visibleArr;		/* Array of visible entries */
    int numVisible;		/* Number of entries in the above array */

    int nextSerial;

    char *openCmd, *closeCmd;	/* Tcl commands to invoke when entries
				 * are opened or closed. */

    Tk_Font font;		/* Global font for labels. The may be
				 * overridden by a -font option for an
				 * individual entry. */

    XColor *textColor;		/* Global text color for labels. This
				 * may be overridden by the -color
				 * option for an individual entry. */
    char *takeFocus;
    char *sortCmd;		/* Tcl command to invoke to sort entries */

    ClientData clientData;
    BindTable bindTable;	/* Table of binding information of entries. */
    BindTable buttonBindTable;	/* Table of binding information for buttons. */
};

static Tk_ConfigSpec buttonConfigSpecs[] =
{
    {TK_CONFIG_BORDER, "-activebackground", "activeBackground",
	"Background", DEF_HIER_BUTTON_ACTIVE_BG_MONO,
	Tk_Offset(Hierbox, button.activeBorder), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_BORDER, "-activebackground", "activeBackground",
	"Background", DEF_HIER_BUTTON_ACTIVE_BG_COLOR,
	Tk_Offset(Hierbox, button.activeBorder), TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-activeforeground", "activeForeground", "Foreground",
	DEF_HIER_BUTTON_ACTIVE_FG_COLOR,
	Tk_Offset(Hierbox, button.activeFgColor), TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-activeforeground", "activeForeground", "Foreground",
	DEF_HIER_BUTTON_ACTIVE_FG_MONO,
	Tk_Offset(Hierbox, button.activeFgColor), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_HIER_BUTTON_NORMAL_BG_COLOR, Tk_Offset(Hierbox, button.border),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_HIER_BUTTON_NORMAL_BG_MONO, Tk_Offset(Hierbox, button.border),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_SYNONYM, "-bd", "borderWidth", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_SYNONYM, "-bg", "background", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_ACTIVE_CURSOR, "-cursor", "cursor", "Cursor",
	DEF_HIER_CURSOR, Tk_Offset(Hierbox, cursor), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-borderwidth", "borderWidth", "BorderWidth",
	DEF_HIER_BORDER_WIDTH, Tk_Offset(Hierbox, button.borderWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-borderwidth", "borderWidth", "BorderWidth",
	DEF_HIER_BUTTON_BORDER_WIDTH, Tk_Offset(Hierbox, button.borderWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_SYNONYM, "-fg", "foreground", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_HIER_BUTTON_NORMAL_FG_COLOR, Tk_Offset(Hierbox, button.fgColor),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_HIER_BUTTON_NORMAL_FG_MONO, Tk_Offset(Hierbox, button.fgColor),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_CUSTOM, "-images", "images", "Images",
	DEF_HIER_BUTTONS, Tk_Offset(Hierbox, buttons),
	TK_CONFIG_NULL_OK, &imagesOption},
    {TK_CONFIG_END, (char *)NULL, (char *)NULL, (char *)NULL,
	(char *)NULL, 0, 0}
};

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_BORDER, "-activebackground", "activeBackground",
	"activeBackground",
	DEF_HIER_ACTIVE_BG_COLOR, Tk_Offset(Hierbox, activeBorder),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_BORDER, "-activebackground", "activeBackground",
	"activeBackground",
	DEF_HIER_ACTIVE_BG_MONO, Tk_Offset(Hierbox, activeBorder),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_CUSTOM, "-activedashes", "activeDashes", "ActiveDashes",
	DEF_HIER_ACTIVE_DASHES, Tk_Offset(Hierbox, button.dashes),
	TK_CONFIG_NULL_OK, &bltDashesOption},
    {TK_CONFIG_COLOR, "-activeforeground", "activeForeground",
	"activeForeground",
	DEF_HIER_ACTIVE_FG_COLOR, Tk_Offset(Hierbox, activeFgColor),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-activeforeground", "activeForeground",
	"activeForeground",
	DEF_HIER_ACTIVE_FG_MONO, Tk_Offset(Hierbox, activeFgColor),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_RELIEF, "-activerelief", "activeRelief", "Relief",
	DEF_HIER_ACTIVE_RELIEF, Tk_Offset(Hierbox, activeRelief), 0},
    {TK_CONFIG_BOOLEAN, "-allowduplicates", "allowDuplicates",
	"AllowDuplicates",
	DEF_HIER_ALLOW_DUPLICATES, Tk_Offset(Hierbox, allowDuplicates),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BOOLEAN, "-autocreate", "autoCreate", "AutoCreate",
	DEF_HIER_MAKE_PATH, Tk_Offset(Hierbox, autoCreate),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_HIER_BACKGROUND, Tk_Offset(Hierbox, border), 0},
    {TK_CONFIG_SYNONYM, "-bd", "borderWidth", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_SYNONYM, "-bg", "background", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_ACTIVE_CURSOR, "-cursor", "cursor", "Cursor",
	DEF_HIER_CURSOR, Tk_Offset(Hierbox, cursor), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-borderwidth", "borderWidth", "BorderWidth",
	DEF_HIER_BORDER_WIDTH, Tk_Offset(Hierbox, borderWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_STRING, "-closecommand", "closeCommand", "CloseCommand",
	DEF_HIER_COMMAND, Tk_Offset(Hierbox, closeCmd), TK_CONFIG_NULL_OK},
    {TK_CONFIG_RELIEF, "-closerelief", "closeRelief", "Relief",
	DEF_HIER_CLOSE_RELIEF, Tk_Offset(Hierbox, closeRelief), 0},
    {TK_CONFIG_CUSTOM, "-dashes", "dashes", "Dashes",
	DEF_HIER_DASHES, Tk_Offset(Hierbox, dashes),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_BOOLEAN, "-exportselection", "exportSelection",
	"ExportSelection",
	DEF_HIER_EXPORT_SELECTION, Tk_Offset(Hierbox, exportSelection),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_SYNONYM, "-fg", "foreground", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_FONT, "-font", "font", "Font",
	DEF_HIER_FONT, Tk_Offset(Hierbox, font), 0},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_HIER_TEXT_COLOR, Tk_Offset(Hierbox, textColor),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_HIER_TEXT_MONO, Tk_Offset(Hierbox, textColor), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_CUSTOM, "-height", "height", "Height",
	DEF_HIER_HEIGHT, Tk_Offset(Hierbox, reqHeight),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_BOOLEAN, "-hideroot", "hideRoot", "HideRoot",
	DEF_HIER_HIDE_ROOT, Tk_Offset(Hierbox, hideRoot),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_COLOR, "-highlightbackground", "highlightBackground",
	"HighlightBackground",
	DEF_HIER_HIGHLIGHT_BG_COLOR, Tk_Offset(Hierbox, highlightBgColor),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-highlightbackground", "highlightBackground",
	"HighlightBackground",
	DEF_HIER_HIGHLIGHT_BG_MONO, Tk_Offset(Hierbox, highlightBgColor),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_COLOR, "-highlightcolor", "highlightColor", "HighlightColor",
	DEF_HIER_HIGHLIGHT_COLOR, Tk_Offset(Hierbox, highlightColor), 0},
    {TK_CONFIG_PIXELS, "-highlightthickness", "highlightThickness",
	"HighlightThickness",
	DEF_HIER_HIGHLIGHT_WIDTH, Tk_Offset(Hierbox, highlightWidth),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_COLOR, "-linecolor", "lineColor", "LineColor",
	DEF_HIER_LINE_COLOR, Tk_Offset(Hierbox, lineColor),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-linecolor", "lineColor", "LineColor",
	DEF_HIER_LINE_MONO, Tk_Offset(Hierbox, lineColor),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_CUSTOM, "-linespacing", "lineSpacing", "LineSpacing",
	DEF_HIER_LINE_SPACING, Tk_Offset(Hierbox, leader),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-linewidth", "lineWidth", "LineWidth",
	DEF_HIER_LINE_WIDTH, Tk_Offset(Hierbox, lineWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_STRING, "-opencommand", "openCommand", "OpenCommand",
	DEF_HIER_COMMAND, Tk_Offset(Hierbox, openCmd), TK_CONFIG_NULL_OK},
    {TK_CONFIG_RELIEF, "-openrelief", "openRelief", "Relief",
	DEF_HIER_OPEN_RELIEF, Tk_Offset(Hierbox, openRelief), 0},
    {TK_CONFIG_RELIEF, "-relief", "relief", "Relief",
	DEF_HIER_RELIEF, Tk_Offset(Hierbox, relief), 0},
    {TK_CONFIG_CUSTOM, "-scrollmode", "scrollMode", "ScrollMode",
	DEF_HIER_SCROLL_MODE, Tk_Offset(Hierbox, scrollMode),
	0, &scrollModeOption},
    {TK_CONFIG_BOOLEAN, "-scrolltile", "scrollTile", "ScrollTile",
	DEF_HIER_SCROLL_TILE, Tk_Offset(Hierbox, scrollTile),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BORDER, "-selectbackground", "selectBackground", "Background",
	DEF_HIER_SELECT_BG_MONO, Tk_Offset(Hierbox, selBorder),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_BORDER, "-selectbackground", "selectBackground", "Background",
	DEF_HIER_SELECT_BG_COLOR, Tk_Offset(Hierbox, selBorder),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_CUSTOM, "-selectborderwidth", "selectBorderWidth", "BorderWidth",
	DEF_HIER_SELECT_BORDER_WIDTH, Tk_Offset(Hierbox, selBorderWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_STRING, "-selectcommand", "selectCommand", "SelectCommand",
	DEF_HIER_SELECT_CMD, Tk_Offset(Hierbox, selectCmd),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_COLOR, "-selectforeground", "selectForeground", "Foreground",
	DEF_HIER_SELECT_FG_MONO, Tk_Offset(Hierbox, selFgColor),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_COLOR, "-selectforeground", "selectForeground", "Foreground",
	DEF_HIER_SELECT_FG_COLOR, Tk_Offset(Hierbox, selFgColor),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_STRING, "-selectmode", "selectMode", "SelectMode",
	DEF_HIER_SELECT_MODE, Tk_Offset(Hierbox, selectMode),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_STRING, "-separator", "separator", "Separator",
	DEF_HIER_SEPARATOR, Tk_Offset(Hierbox, separator), TK_CONFIG_NULL_OK},
    {TK_CONFIG_STRING, "-takefocus", "takeFocus", "TakeFocus",
	DEF_HIER_TAKE_FOCUS, Tk_Offset(Hierbox, takeFocus), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-tile", "tile", "Tile",
	(char *)NULL, Tk_Offset(Hierbox, bgTile), TK_CONFIG_NULL_OK,
	&bltTileOption},
    {TK_CONFIG_STRING, "-trimleft", "trimLeft", "Trim",
	DEF_HIER_TRIMLEFT, Tk_Offset(Hierbox, trimLeft), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-width", "width", "Width",
	DEF_HIER_WIDTH, Tk_Offset(Hierbox, reqWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_STRING, "-xscrollcommand", "xScrollCommand", "ScrollCommand",
	(char *)NULL, Tk_Offset(Hierbox, xScrollCmdPrefix), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-xscrollincrement", "xScrollIncrement",
	"ScrollIncrement",
	DEF_HIER_SCROLL_INCREMENT, Tk_Offset(Hierbox, reqScrollX),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_STRING, "-yscrollcommand", "yScrollCommand", "ScrollCommand",
	(char *)NULL, Tk_Offset(Hierbox, yScrollCmdPrefix), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-yscrollincrement", "yScrollIncrement",
	"ScrollIncrement",
	DEF_HIER_SCROLL_INCREMENT, Tk_Offset(Hierbox, reqScrollY),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_END, (char *)NULL, (char *)NULL, (char *)NULL,
	(char *)NULL, 0, 0}
};

typedef struct LayoutInfo {
    int x, y;			/* Tracks the current world
				 * coordinates as we traverse through
				 * the tree. After a full-tree
				 * traversal, the y-coordinate will be
				 * the height of the virtual
				 * hierarchy. */
    int maxWidth;		/* Maximum entry width. This is the
				 * width of the virtual hierarchy. */
    int labelOffset;
    int minHeight;		/* Minimum entry height. Used to
				 * to compute what the y-scroll unit
				 * should be. */
    int maxIconWidth;
    int level, depth;
} LayoutInfo;

#define DEF_ICON_WIDTH 16
#define DEF_ICON_HEIGHT 16
static unsigned char folderBits[] =
{
    0x00, 0x00, 0x7c, 0x00, 0x82, 0x00, 0x81, 0x3f, 0x41, 0x40, 0x3f, 0xc0,
    0x01, 0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01, 0xc0,
    0x01, 0xc0, 0xff, 0xff, 0xfe, 0xff, 0x00, 0x00};

static unsigned char folderMaskBits[] =
{
    0x00, 0x00, 0x7c, 0x00, 0xfe, 0x00, 0xff, 0x3f, 0xff, 0x7f, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xfe, 0xff, 0x00, 0x00};

static Pixmap iconBitmap, iconMask;	/* Default icon bitmaps */
static XColor *iconColor;

/* Forward Declarations */
static void DestroyHierbox _ANSI_ARGS_((DestroyData dataPtr));
static void HierboxEventProc _ANSI_ARGS_((ClientData clientdata,
	XEvent *eventPtr));
static void DrawEntryButton _ANSI_ARGS_((Hierbox * hierPtr, Tree * treePtr,
	Drawable drawable));
static void DisplayHierbox _ANSI_ARGS_((ClientData clientData));
static void HierboxInstDeletedCmd _ANSI_ARGS_((ClientData clientdata));
static int HierboxInstCmd _ANSI_ARGS_((ClientData clientdata,
	Tcl_Interp *interp, int argc, char **argv));
static void EventuallyRedraw _ANSI_ARGS_((Hierbox * hierPtr));
static void SelectCmdProc _ANSI_ARGS_((ClientData clientData));
static void EventuallyInvokeSelectCmd _ANSI_ARGS_((Hierbox * hierPtr));
static int ComputeVisibleEntries _ANSI_ARGS_((Hierbox * hierPtr));
static int ConfigureEntry _ANSI_ARGS_((Hierbox * hierPtr, Entry * entryPtr,
	int argc, char **argv, int flags));

#ifdef __STDC__
static CompareProc ExactCompare, GlobCompare, RegexpCompare;
static ApplyProc SelectNode, GetSelectedLabels, CloseNode, SizeOfNode, IsSelectedNode, MapAncestors,
          FixUnmappedSelections, UnmapNode, SortNode, OpenNode;
static IterProc NextNode, LastNode;
static Tk_ImageChangedProc ImageChangedProc;
static Blt_ListCompareProc CompareNodesByTclCmd, CompareNodesByName;
static BindPickProc PickButton, PickEntry;
static BindTagProc GetTags;
static Tk_SelectionProc SelectionProc;
static Tk_LostSelProc LostSelection;
static Tcl_CmdProc HierboxCmd;
static Blt_TileChangedProc TileChangedProc;
#endif /* __STDC__ */


/*
 *----------------------------------------------------------------------
 *
 * StringToScrollMode --
 *
 *	Convert the string reprsenting a scroll mode, to its numeric
 *	form.
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
StringToScrollMode(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* Not used. */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* New legend position string */
    char *widgRec;		/* Widget record */
    int offset;			/* offset to XPoint structure */
{
    int *modePtr = (int *)(widgRec + offset);

    if ((string[0] == 'l') && (strcmp(string, "listbox") == 0)) {
	*modePtr = SCROLL_MODE_LISTBOX;
    } else if ((string[0] == 'c') && (strcmp(string, "canvas") == 0)) {
	*modePtr = SCROLL_MODE_CANVAS;
    } else {
	Tcl_AppendResult(interp, "bad scroll mode \"", string,
	    "\": should be \"listbox\" or \"canvas\"", (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ScrollModeToString --
 *
 * Results:
 *	The string representation of the button boolean is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
ScrollModeToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Widget record */
    int offset;			/* offset of flags field in record */
    Tcl_FreeProc **freeProcPtr;	/* Memory deallocation scheme to use */
{
    int mode = *(int *)(widgRec + offset);

    switch (mode) {
    case SCROLL_MODE_LISTBOX:
	return "listbox";
    case SCROLL_MODE_CANVAS:
	return "canvas";
    default:
	return "unknown scroll mode";
    }
}

/*
 *----------------------------------------------------------------------
 *
 * StringToButton --
 *
 *	Convert a string to one of three values.
 *		0 - false, no, off
 *		1 - true, yes, on
 *		2 - auto
 * Results:
 *	If the string is successfully converted, TCL_OK is returned.
 *	Otherwise, TCL_ERROR is returned and an error message is left in
 *	interpreter's result field.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToButton(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* Not used. */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* New legend position string */
    char *widgRec;		/* Widget record */
    int offset;			/* offset to XPoint structure */
{
    int *flags = (int *)(widgRec + offset);

    *flags &= ~BUTTON_MASK;
    if ((string[0] == 'a') && (strcmp(string, "auto") == 0)) {
	*flags |= BUTTON_AUTO;
    } else {
	int value;

	if (Tcl_GetBoolean(interp, string, &value) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (value) {
	    *flags |= BUTTON_SHOW;
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ButtonToString --
 *
 * Results:
 *	The string representation of the button boolean is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
ButtonToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Widget record */
    int offset;			/* offset of flags field in record */
    Tcl_FreeProc **freeProcPtr;	/* Memory deallocation scheme to use */
{
    unsigned int flags = *(int *)(widgRec + offset);

    switch (flags & BUTTON_MASK) {
    case 0:
	return "0";
    case BUTTON_SHOW:
	return "1";
    case BUTTON_AUTO:
	return "auto";
    default:
	return "unknown button value";
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ImageChangedProc
 *
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
    Hierbox *hierPtr = (Hierbox *) clientData;

    hierPtr->flags |= (HIER_LAYOUT | HIER_SCROLL);
    EventuallyRedraw(hierPtr);
}

static HierImage
GetImage(hierPtr, interp, tkwin, name)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    Tk_Window tkwin;
    char *name;
{
    struct HierImage *imagePtr;
    int isNew;
    Tcl_HashEntry *hPtr;

    hPtr = Tcl_CreateHashEntry(&(hierPtr->imageTable), (char *)name, &isNew);
    if (isNew) {
	Tk_Image tkImage;
	int width, height;

	tkImage = Tk_GetImage(interp, tkwin, name, ImageChangedProc,
	    (ClientData)hierPtr);
	if (tkImage == NULL) {
	    Tcl_DeleteHashEntry(hPtr);
	    return NULL;
	}
	Tk_SizeOfImage(tkImage, &width, &height);
	imagePtr = (struct HierImage *)malloc(sizeof(struct HierImage));
	imagePtr->tkImage = tkImage;
	imagePtr->hashPtr = hPtr;
	imagePtr->refCount = 1;
	imagePtr->width = width;
	imagePtr->height = height;
	Tcl_SetHashValue(hPtr, (ClientData)imagePtr);
    } else {
	imagePtr = (struct HierImage *)Tcl_GetHashValue(hPtr);
	imagePtr->refCount++;
    }
    return imagePtr;
}

static void
FreeImage(imagePtr)
    struct HierImage *imagePtr;
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
 * StringToImages --
 *
 *	Convert a list of image names into Tk images.
 *
 * Results:
 *	If the string is successfully converted, TCL_OK is returned.
 *	Otherwise, TCL_ERROR is returned and an error message is left in
 *	interpreter's result field.
 *
 *----------------------------------------------------------------------
 */
static int
StringToImages(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* New legend position string */
    char *widgRec;		/* Widget record */
    int offset;			/* offset to field in structure */
{
    Hierbox *hierPtr = *(Hierbox **) clientData;
    HierImage **imagePtrPtr = (HierImage **) (widgRec + offset);
    HierImage *imageArr;
    int result;

    result = TCL_OK;
    imageArr = NULL;
    if ((string != NULL) && (*string != '\0')) {
	int numNames;
	char **nameArr;

	if (Tcl_SplitList(interp, string, &numNames, &nameArr) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (numNames > 0) {
	    register int i;

	    imageArr = (HierImage *)
		malloc(sizeof(HierImage *) * (numNames + 1));
	    assert(imageArr);
	    for (i = 0; i < numNames; i++) {
		imageArr[i] = GetImage(hierPtr, interp, tkwin, nameArr[i]);
		if (imageArr[i] == NULL) {
		    result = TCL_ERROR;
		    break;
		}
	    }
	    free((char *)nameArr);
	    imageArr[numNames] = NULL;
	}
    }
    if (*imagePtrPtr != NULL) {
	register HierImage *imagePtr;

	for (imagePtr = *imagePtrPtr; *imagePtr != NULL; imagePtr++) {
	    FreeImage(*imagePtr);
	}
	free((char *)*imagePtrPtr);
    }
    *imagePtrPtr = imageArr;
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * ImagesToString --
 *
 *	Converts the image into its string representation (its name).
 *
 * Results:
 *	The name of the image is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
ImagesToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Widget record */
    int offset;			/* offset of images array in record */
    Tcl_FreeProc **freeProcPtr;	/* Memory deallocation scheme to use */
{
    HierImage **imagePtrPtr = (HierImage **) (widgRec + offset);
    Tcl_DString dString;
    char *result;

    Tcl_DStringInit(&dString);
    if (*imagePtrPtr != NULL) {
	Hierbox *hierPtr = *(Hierbox **) clientData;
	register HierImage *imagePtr;

	for (imagePtr = *imagePtrPtr; *imagePtr != NULL; imagePtr++) {
	    Tcl_DStringAppendElement(&dString,
		Tcl_GetHashKey(&(hierPtr->imageTable), (*imagePtr)->hashPtr));
	}
    }
    result = strdup(Tcl_DStringValue(&dString));
    Tcl_DStringFree(&dString);
    *freeProcPtr = (Tcl_FreeProc *)free;
    return result;
}

static int
ApplyToTree(hierPtr, rootPtr, proc, flags)
    Hierbox *hierPtr;
    Tree *rootPtr;
    ApplyProc *proc;
    unsigned int flags;
{
    if (flags & APPLY_BEFORE) {
	if ((*proc) (hierPtr, rootPtr) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    if (flags & APPLY_RECURSE) {
	if (!(flags & APPLY_OPEN_ONLY) ||
	    (rootPtr->entryPtr->flags & ENTRY_OPEN)) {
	    register Blt_ListItem item, nextItem;
	    Tree *treePtr;

	    nextItem = NULL;
	    for (item = Blt_ListFirstItem(rootPtr->nodeList); item != NULL;
		item = nextItem) {
		nextItem = Blt_ListNextItem(item);
		treePtr = (Tree *) Blt_ListGetValue(item);
		if (ApplyToTree(hierPtr, treePtr, proc, flags) != TCL_OK) {
		    return TCL_ERROR;
		}
	    }
	}
    }
    if ((flags & APPLY_BEFORE) == 0) {
	if ((*proc) (hierPtr, rootPtr) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    return TCL_OK;
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
 * 	  "number	- number of the item in the hierarchy, indexed
 *			  from zero.
 *	  "end"		- last position in the hierarchy.
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
    char *string;		/* Numerical index into textPtr's element
				 * list, or "end" to refer to last element. */
    int *indexPtr;		/* Where to store converted relief. */
{
    if ((string[0] == 'e') && (strcmp(string, "end") == 0)) {
	*indexPtr = POSITION_END;	/* Indicates last position in hierarchy. */
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

static void
ConfigureButtons(hierPtr)
    Hierbox *hierPtr;
{
    unsigned long gcMask;
    XGCValues gcValues;
    GC newGC;

    gcMask = GCForeground;
    gcValues.foreground = hierPtr->button.fgColor->pixel;
    newGC = Tk_GetGC(hierPtr->tkwin, gcMask, &gcValues);
    if (hierPtr->button.normalGC != NULL) {
	Tk_FreeGC(hierPtr->display, hierPtr->button.normalGC);
    }
    newGC = Tk_GetGC(hierPtr->tkwin, gcMask, &gcValues);
    hierPtr->button.normalGC = newGC;

    gcMask = GCForeground;
    gcValues.foreground = hierPtr->button.activeFgColor->pixel;
    newGC = Tk_GetGC(hierPtr->tkwin, gcMask, &gcValues);
    if (hierPtr->button.activeGC != NULL) {
	Tk_FreeGC(hierPtr->display, hierPtr->button.activeGC);
    }
    hierPtr->button.activeGC = newGC;

    hierPtr->buttonWidth = hierPtr->buttonHeight = ODD(BUTTON_SIZE);
    if (hierPtr->buttons != NULL) {
	register int i;

	for (i = 0; i < 2; i++) {
	    if (hierPtr->buttons[i] == NULL) {
		break;
	    }
	    if (hierPtr->buttonWidth < ImageWidth(hierPtr->buttons[i])) {
		hierPtr->buttonWidth = ImageWidth(hierPtr->buttons[i]);
	    }
	    if (hierPtr->buttonHeight < ImageHeight(hierPtr->buttons[i])) {
		hierPtr->buttonHeight = ImageHeight(hierPtr->buttons[i]);
	    }
	}
    }
    hierPtr->buttonWidth += 2 * hierPtr->button.borderWidth;
    hierPtr->buttonHeight += 2 * hierPtr->button.borderWidth;
}

static void
DestroyEntry(entryPtr)
    Entry *entryPtr;
{
    Hierbox *hierPtr = entryPtr->hierPtr;
    register HierImage *imagePtr;

    Tk_FreeOptions(entryConfigSpecs, (char *)entryPtr, hierPtr->display, 0);
    if (entryPtr->labelGC != NULL) {
	Tk_FreeGC(hierPtr->display, entryPtr->labelGC);
    }
    if (entryPtr->textGC != NULL) {
	Tk_FreeGC(hierPtr->display, entryPtr->textGC);
    }
    if (entryPtr->hashPtr != NULL) {
	Tcl_DeleteHashEntry(entryPtr->hashPtr);
    }
    if (entryPtr->textShadow.color != NULL) {
	Tk_FreeColor(entryPtr->textShadow.color);
    }
    if (entryPtr->labelShadow.color != NULL) {
	Tk_FreeColor(entryPtr->labelShadow.color);
    }
    if (entryPtr->iconGC != NULL) {
	Tk_FreeGC(hierPtr->display, entryPtr->iconGC);
    }
    if (entryPtr->openCmd != NULL) {
	Blt_FreeUid(entryPtr->openCmd);
    }
    if (entryPtr->closeCmd != NULL) {
	Blt_FreeUid(entryPtr->closeCmd);
    }
    if (entryPtr->data != NULL) {
	Blt_FreeUid(entryPtr->data);
    }
    if (entryPtr->label != NULL) {
	Blt_FreeUid(entryPtr->label);
    }
    if (entryPtr->text != NULL) {
	Blt_FreeUid(entryPtr->text);
    }
    if (entryPtr->tags != NULL) {
	Blt_FreeUid(entryPtr->tags);
    }
    if (entryPtr->icons != NULL) {
	for (imagePtr = entryPtr->icons; *imagePtr != NULL; imagePtr++) {
	    FreeImage(*imagePtr);
	}
	free((char *)entryPtr->icons);
    }
    if (entryPtr->images != NULL) {
	for (imagePtr = entryPtr->images; *imagePtr != NULL; imagePtr++) {
	    FreeImage(*imagePtr);
	}
	free((char *)entryPtr->images);
    }
    Blt_DeleteAllBindings(hierPtr->bindTable, (ClientData)entryPtr);
    Blt_DeleteAllBindings(hierPtr->buttonBindTable, (ClientData)entryPtr);
    free((char *)entryPtr);
}

/*ARGSUSED*/
static Tree *
LastNode(treePtr, mask)
    register Tree *treePtr;
    unsigned int mask;
{
    register Blt_ListItem item;

    if (treePtr->parentPtr == NULL) {
	return NULL;		/* The root is the first node. */
    }
    item = Blt_ListPrevItem(treePtr->item);
    if (item == NULL) {
	/* There are no siblings previous to this one, so pick the parent. */
	return treePtr->parentPtr;
    }
    /*
     * Traverse down the right-most thread, in order to select the
     * next entry.  Stop if we find a "closed" entry or reach a leaf.
     */
    treePtr = (Tree *) Blt_ListGetValue(item);
    while ((treePtr->entryPtr->flags & mask) == mask) {
	item = Blt_ListLastItem(treePtr->nodeList);
	if (item == NULL) {
	    break;		/* Found a leaf. */
	}
	treePtr = (Tree *) Blt_ListGetValue(item);
    }
    return treePtr;
}


static Tree *
NextNode(treePtr, mask)
    Tree *treePtr;
    unsigned int mask;
{
    register Blt_ListItem item;

    if ((treePtr->entryPtr->flags & mask) == mask) {
	/* Pick the first sub-node. */
	item = Blt_ListFirstItem(treePtr->nodeList);
	if (item != NULL) {
	    return (Tree *) Blt_ListGetValue(item);
	}
    }
    /*
     * Back up until we can find a level where we can pick a "next" entry.
     * For the last entry we'll thread our way back to the root.
     */
    while (treePtr->parentPtr != NULL) {
	item = Blt_ListNextItem(treePtr->item);
	if (item != NULL) {
	    return (Tree *) Blt_ListGetValue(item);
	}
	treePtr = treePtr->parentPtr;
    }
    return NULL;		/* At root, no next node. */
}

/*ARGSUSED*/
static Tree *
EndNode(treePtr, mask)
    Tree *treePtr;
    unsigned int mask;
{
    register Blt_ListItem item;

    item = Blt_ListLastItem(treePtr->nodeList);
    while (item != NULL) {
	treePtr = (Tree *) Blt_ListGetValue(item);
	if ((treePtr->entryPtr->flags & mask) != mask) {
	    break;
	}
	item = Blt_ListLastItem(treePtr->nodeList);
    }
    return treePtr;
}

static void
ExposeAncestors(treePtr)
    register Tree *treePtr;
{
    treePtr = treePtr->parentPtr;
    while (treePtr != NULL) {
	treePtr->entryPtr->flags |= (ENTRY_OPEN | ENTRY_MAPPED);
	treePtr = treePtr->parentPtr;
    }
}

static int
IsBefore(t1Ptr, t2Ptr)
    register Tree *t1Ptr, *t2Ptr;
{
    int depth;
    register int i;
    register Blt_ListItem item;
    Tree *treePtr;

    depth = MIN(t1Ptr->level, t2Ptr->level);

    if (depth == 0) {		/* One of the nodes is root. */
	if (t1Ptr->parentPtr == NULL) {
	    return 1;
	}
	return 0;
    }
    /*
     * Traverse back from the deepest node, until the both nodes are at the
     * same depth.  Check if the ancestor node found is the other node.
     */
    for (i = t1Ptr->level; i > depth; i--) {
	t1Ptr = t1Ptr->parentPtr;
    }
    if (t1Ptr == t2Ptr) {
	return 0;
    }
    for (i = t2Ptr->level; i > depth; i--) {
	t2Ptr = t2Ptr->parentPtr;
    }
    if (t2Ptr == t1Ptr) {
	return 1;
    }
    /*
     * First find the mutual ancestor of both nodes.  Look at each
     * preceding ancestor level-by-level for both nodes.  Eventually
     * we'll find a node that's the parent of both ancestors.  Then
     * find the first ancestor in the parent's list of subnodes.
     */
    for (i = depth; i > 0; i--) {
	if (t1Ptr->parentPtr == t2Ptr->parentPtr) {
	    break;
	}
	t1Ptr = t1Ptr->parentPtr;
	t2Ptr = t2Ptr->parentPtr;
    }
    for (item = Blt_ListFirstItem(t1Ptr->parentPtr->nodeList); item != NULL;
	item = Blt_ListNextItem(item)) {
	treePtr = (Tree *) Blt_ListGetValue(item);
	if (treePtr == t1Ptr) {
	    return 1;
	} else if (treePtr == t2Ptr) {
	    return 0;
	}
    }
    assert(item != NULL);
    return 0;
}

static int
IsAncestor(rootPtr, treePtr)
    Tree *rootPtr, *treePtr;
{
    if (treePtr != NULL) {
	treePtr = treePtr->parentPtr;
	while (treePtr != NULL) {
	    if (treePtr == rootPtr) {
		return 1;
	    }
	    treePtr = treePtr->parentPtr;
	}
    }
    return 0;
}

static int
IsHidden(treePtr)
    register Tree *treePtr;
{
    if (treePtr != NULL) {
	unsigned int mask;

	if (!(treePtr->entryPtr->flags & ENTRY_MAPPED)) {
	    return TRUE;
	}
	treePtr = treePtr->parentPtr;
	mask = (ENTRY_OPEN | ENTRY_MAPPED);
	while (treePtr != NULL) {
	    if ((treePtr->entryPtr->flags & mask) != mask) {
		return TRUE;
	    }
	    treePtr = treePtr->parentPtr;
	}
    }
    return FALSE;
}

static int
IsSelected(hierPtr, entryPtr)
    Hierbox *hierPtr;
    Entry *entryPtr;
{
    if (entryPtr->flags & ENTRY_SELECTED) {
	return TRUE;
    }
    if ((hierPtr->selAnchorPtr != NULL) && (hierPtr->selMarkPtr != NULL)) {
	Entry *e1Ptr, *e2Ptr;
	int inside;

	e1Ptr = (Entry *) hierPtr->selAnchorPtr->entryPtr;
	e2Ptr = (Entry *) hierPtr->selMarkPtr->entryPtr;
	inside = FALSE;
	if (e1Ptr->worldY < e2Ptr->worldY) {
	    if (((entryPtr->worldY + entryPtr->height) > e1Ptr->worldY) &&
		(entryPtr->worldY < (e2Ptr->worldY + e2Ptr->height))) {
		inside = TRUE;
	    }
	} else {
	    if (((entryPtr->worldY + entryPtr->height) > e2Ptr->worldY) &&
		(entryPtr->worldY < (e1Ptr->worldY + e1Ptr->height))) {
		inside = TRUE;
	    }
	}
	if (inside) {
	    switch (hierPtr->flags & SELECT_MASK) {
	    case SELECT_SET:
		return TRUE;
	    case SELECT_CLEAR:
		return FALSE;
	    case SELECT_TOGGLE:
		return ((entryPtr->flags & ENTRY_SELECTED) == 0);
	    }
	}
    }
    return FALSE;
}

static void
GetFullPath(treePtr, separator, pathStrPtr)
    Tree *treePtr;
    char *separator;
    Tcl_DString *pathStrPtr;
{
    char **compArr;
    register int i;
    int level;

    level = treePtr->level;
    compArr = (char **)malloc((level + 2) * sizeof(char *));
    assert(compArr);

    for (i = level; i >= 0; i--) {
	compArr[i] = treePtr->nameId;
	treePtr = treePtr->parentPtr;
    }
    Tcl_DStringInit(pathStrPtr);
    for (i = 0; i < level; i++) {
	if (separator == NULL) {
	    Tcl_DStringAppendElement(pathStrPtr, compArr[i]);
	} else {
	    Tcl_DStringAppend(pathStrPtr, compArr[i], -1);
	    Tcl_DStringAppend(pathStrPtr, separator, -1);
	}
    }
    if (separator == NULL) {
	Tcl_DStringAppendElement(pathStrPtr, compArr[i]);
    } else {
	Tcl_DStringAppend(pathStrPtr, compArr[i], -1);
    }
    free((char *)compArr);
}

static void
InsertNode(parentPtr, position, nodePtr)
    Tree *parentPtr;		/* Parent node where the new node will
				 * be added. */
    int position;		/* Position in the parent of the new node. */
    Tree *nodePtr;		/* The node to be inserted. */
{
    Blt_ListItem item;

    /*
     * Create lists to contain subnodes as needed.  We don't want to
     * unnecessarily allocate storage for leaves (of which there may
     * be many).
     */
    if (parentPtr->nodeList == NULL) {
	parentPtr->nodeList = Blt_CreateList(TCL_ONE_WORD_KEYS);
    }
    item = Blt_ListNewItem(parentPtr->nodeList, (char *)nodePtr->entryPtr);
    if (position >= Blt_ListGetLength(parentPtr->nodeList)) {
	Blt_ListAppendItem(parentPtr->nodeList, item);
    } else {
	Blt_ListItem beforeItem;

	beforeItem = Blt_ListFindNthItem(parentPtr->nodeList, position, 1);
	Blt_ListLinkBefore(parentPtr->nodeList, item, beforeItem);
    }
    nodePtr->level = parentPtr->level + 1;
    nodePtr->parentPtr = parentPtr;
    nodePtr->item = item;
    Blt_ListSetValue(item, (ClientData)nodePtr);
}

static int
DestroyNode(hierPtr, treePtr)
    Hierbox *hierPtr;
    Tree *treePtr;
{
    /*
     * Indicate that the screen layout of the hierarchy may have changed
     * because the node was deleted.  We don't want to access the
     * hierPtr->visibleArr array if one of the nodes is bogus.
     */
    hierPtr->flags |= HIER_DIRTY;
    if (treePtr == hierPtr->activePtr) {
	hierPtr->activePtr = treePtr->parentPtr;
    }
    if (treePtr == hierPtr->focusPtr) {
	hierPtr->focusPtr = treePtr->parentPtr;
	Blt_SetFocusItem(hierPtr->bindTable, hierPtr->focusPtr);
    }
    if (treePtr->entryPtr != NULL) {
	DestroyEntry(treePtr->entryPtr);
    }
    if (treePtr->item != NULL) {
	Blt_ListDeleteItem(treePtr->item);	/* Remove from parent's list */
    }
    if (treePtr->nameId != NULL) {
	Blt_FreeUid(treePtr->nameId);
    }
    if (treePtr->nodeList != NULL) {
	Blt_ListDestroy(treePtr->nodeList);
    }
    free((char *)treePtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * CreateNode --
 *
 *	Creates and inserts a new node into the given tree at the
 *	specified position.
 *
 * Results:
 *	Returns a pointer to the newly created node.  If an error
 *	occurred, such as the entry could not be configured, NULL
 *	is returned.
 *
 *----------------------------------------------------------------------
 */
static Tree *
CreateNode(hierPtr, parentPtr, position, name)
    Hierbox *hierPtr;		/* Hierarchy widget record */
    Tree *parentPtr;		/* Pointer to parent node. */
    int position;		/* Position in node list to insert node. */
    char *name;			/* Name identifier for the new node. */
{
    Entry *entryPtr;
    Tree *treePtr;
    Tcl_HashEntry *hPtr;
    int isNew;
    int serial;

    /* Create the entry structure */
    entryPtr = (Entry *) calloc(1, sizeof(Entry));
    assert(entryPtr);
    entryPtr->flags |= (BUTTON_AUTO | ENTRY_MAPPED);

    if (name != NULL) {
	entryPtr->label = Blt_GetUid(name);
    }
    if (ConfigureEntry(hierPtr, entryPtr, 0, (char **)NULL, 0) != TCL_OK) {
	DestroyEntry(entryPtr);
	return NULL;
    }
    /* Create the container structure too */
    treePtr = (Tree *) calloc(1, sizeof(Tree));
    assert(treePtr);
    if (name == NULL) {
	name = "";
    }
    treePtr->nameId = Blt_GetUid(name);
    treePtr->entryPtr = entryPtr;

    /* Generate a unique node serial number. */
    do {
	serial = hierPtr->nextSerial++;
	hPtr = Tcl_CreateHashEntry(&(hierPtr->nodeTable), (char *)serial,
	    &isNew);
    } while (!isNew);
    Tcl_SetHashValue(hPtr, (ClientData)treePtr);
    entryPtr->hashPtr = hPtr;
    entryPtr->hierPtr = hierPtr;

    if (parentPtr != NULL) {
	InsertNode(parentPtr, position, treePtr);
    }
    return treePtr;
}

/*
 *----------------------------------------------------------------------
 *
 * FindComponent --
 *
 *	Searches for the given child node.  The node is designated
 *	by its node identifier string.
 *
 * Results:
 *	If found, returns a node pointer. Otherwise NULL.
 *
 *----------------------------------------------------------------------
 */
static Tree *
FindComponent(parentPtr, name)
    Tree *parentPtr;
    char *name;
{
    Tk_Uid nameId;

    nameId = Blt_FindUid(name);
    if (nameId != NULL) {
	register Tree *treePtr;
	register Blt_ListItem item;

	/* The component identifier must already exist if the node exists. */
	for (item = Blt_ListFirstItem(parentPtr->nodeList); item != NULL;
	    item = Blt_ListNextItem(item)) {
	    treePtr = (Tree *) Blt_ListGetValue(item);
	    if (nameId == treePtr->nameId) {
		return treePtr;
	    }
	}
    }
    return NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * SkipSeparators --
 *
 *	Moves the character pointer past one of more separators.
 *
 * Results:
 *	Returns the updates character pointer.
 *
 *----------------------------------------------------------------------
 */
static char *
SkipSeparators(path, separator, length)
    char *path, *separator;
    int length;
{
    while ((*path == separator[0]) && (strncmp(path, separator, length) == 0)) {
	path += length;
    }
    return path;
}

/*
 *----------------------------------------------------------------------
 *
 * SplitPath --
 *
 *	Returns the trailing component of the given path.  Trailing
 *	separators are ignored.
 *
 * Results:
 *	Returns the string of the tail component.
 *
 *----------------------------------------------------------------------
 */
static int
SplitPath(hierPtr, path, levelPtr, compPtrPtr)
    Hierbox *hierPtr;
    char *path;
    int *levelPtr;
    char ***compPtrPtr;
{
    int skipLen, pathLen;
    char *sep;
    int level, listSize;
    char **components;
    register char *p;

    skipLen = strlen(hierPtr->separator);
    path = SkipSeparators(path, hierPtr->separator, skipLen);
    pathLen = strlen(path);

    level = pathLen / skipLen;
    listSize = (level + 1) * sizeof(char *);
    components = (char **)malloc(listSize + (pathLen + 1));
    assert(components);
    p = (char *)components + listSize;
    strcpy(p, path);

    sep = strstr(p, hierPtr->separator);
    level = 0;
    while ((*p != '\0') && (sep != NULL)) {
	*sep = '\0';
	components[level++] = p;
	p = SkipSeparators(sep + skipLen, hierPtr->separator, skipLen);
	sep = strstr(p, hierPtr->separator);
    }
    if (*p != '\0') {
	components[level++] = p;
    }
    components[level] = NULL;
    *levelPtr = level;
    *compPtrPtr = components;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * FindPath --
 *
 *	Finds the node designated by the given path.  Each path
 *	component is searched for as the tree is traversed.
 *
 *	A leading character string is trimmed off the path if it
 *	matches the one designated (see the -trimleft option).
 *
 *	If no separator is designated (see the -separator
 *	configuration option), the path is considered a Tcl list.
 *	Otherwise the each component of the path is separated by a
 *	character string.  Leading and trailing separators are
 *	ignored.  Multiple separators are treated as one.
 *
 * Results:
 *	Returns the pointer to the designated node.  If any component
 *	can't be found, NULL is returned.
 *
 *----------------------------------------------------------------------
 */
static Tree *
FindPath(hierPtr, rootPtr, path)
    Hierbox *hierPtr;
    Tree *rootPtr;
    char *path;
{
    register char *p;
    int skip;
    char *sep;
    Tree *treePtr;
    char save;

    /* Trim off characters that we don't want */
    if (hierPtr->trimLeft != NULL) {
	register char *s;

	/* Trim off leading character string if one exists. */
	for (p = path, s = hierPtr->trimLeft; *s != '\0'; s++, p++) {
	    if (*p != *s) {
		break;
	    }
	}
	if (*s == '\0') {
	    path = p;
	}
    }
    if (*path == '\0') {
	return rootPtr;
    }
    if (hierPtr->separator == NULL) {
	char **compArr;
	int numComp;
	register int i;

	/*
	 * No separator, so this must be a Tcl list of components.
	 */
	if (Tcl_SplitList(hierPtr->interp, path, &numComp, &compArr)
	    != TCL_OK) {
	    return NULL;
	}
	for (i = 0; i < numComp; i++) {
	    treePtr = FindComponent(rootPtr, compArr[i]);
	    if (treePtr == NULL) {
		free((char *)compArr);
		return NULL;
	    }
	    rootPtr = treePtr;
	}
	free((char *)compArr);
	return rootPtr;
    }
    skip = strlen(hierPtr->separator);
    path = SkipSeparators(path, hierPtr->separator, skip);
    sep = strstr(path, hierPtr->separator);
    p = path;
    treePtr = rootPtr;
    while ((*p != '\0') && (sep != NULL)) {
	save = *sep, *sep = '\0';
	treePtr = FindComponent(treePtr, p);
	*sep = save;
	if (treePtr == NULL) {
	    return NULL;	/* Bad component name. */
	}
	p = SkipSeparators(sep + skip, hierPtr->separator, skip);
	sep = strstr(p, hierPtr->separator);
    }
    if (*p != '\0') {
	treePtr = FindComponent(treePtr, p);
	if (treePtr == NULL) {
	    return NULL;
	}
    }
    return treePtr;
}

/*
 *----------------------------------------------------------------------
 *
 * NearestNode --
 *
 *	Finds the entry closest to the given screen X-Y coordinates
 *	in the viewport.
 *
 * Results:
 *	Returns the pointer to the closest node.  If no node is
 *	visible (nodes may be hidden), NULL is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static Tree *
NearestNode(hierPtr, x, y, selectOne)
    Hierbox *hierPtr;
    int x, y;
    int selectOne;
{
    register Tree *lastPtr, **treePtrPtr;
    register Entry *entryPtr;

    /*
     * We implicitly can pick only visible entries.  So make sure that
     * the tree exists.
     */
    if (hierPtr->numVisible == 0) {
	return NULL;
    }
    /*
     * Since the entry positions were previously computed in world
     * coordinates, convert Y-coordinate from screen to world
     * coordinates too.
     */
    y = WORLDY(hierPtr, y);
    treePtrPtr = hierPtr->visibleArr;
    lastPtr = *treePtrPtr;
    for ( /* empty */ ; *treePtrPtr != NULL; treePtrPtr++) {
	entryPtr = (*treePtrPtr)->entryPtr;
	/*
	 * If the start of the next entry starts beyond the point,
	 * use the last entry.
	 */
	if (entryPtr->worldY > y) {
	    return (selectOne) ? lastPtr : NULL;
	}
	if (y < (entryPtr->worldY + entryPtr->height)) {
	    return *treePtrPtr;	/* Found it. */
	}
	lastPtr = *treePtrPtr;
    }
    return (selectOne) ? lastPtr : NULL;
}

static Tree *
GetNodeByIndex(interp, hierPtr, string)
    Tcl_Interp *interp;
    Hierbox *hierPtr;
    char *string;
{
    if (isdigit(UCHAR(string[0]))) {
	int serial;

	if (Tcl_GetInt(interp, string, &serial) == TCL_OK) {
	    Tcl_HashEntry *hPtr;

	    hPtr = Tcl_FindHashEntry(&(hierPtr->nodeTable), (char *)serial);
	    if (hPtr != NULL) {
		return (Tree *) Tcl_GetHashValue(hPtr);
	    }
	}
	Tcl_ResetResult(interp);
    }
    return NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * NodeToString --
 *
 *	Converts a node pointer in its string representation.  The
 *	string is the node's identifier number.
 *
 * Results:
 *	The string representation of the node is returned.  Note that
 *	the string is stored statically, so that callers must save the
 *	string before the next call to this routine overwrites the
 *	static array again.
 *
 *----------------------------------------------------------------------
 */
static char *
NodeToString(hierPtr, nodePtr)
    Hierbox *hierPtr;
    Tree *nodePtr;
{
    static char string[200];
    int serial;

    /* Node table keys are integers.  Convert them to strings. */
    serial = (int)Tcl_GetHashKey(&(hierPtr->nodeTable),
	nodePtr->entryPtr->hashPtr);
    sprintf(string, "%d", serial);

    return string;
}

/*
 *----------------------------------------------------------------------
 *
 * GetNode --
 *
 *	Converts a string into node pointer.  The string may be in one
 *	of the following forms:
 *
 *	    @x,y	   - Closest node to the specified X-Y screen
 *			     coordinates.
 *	    "active"	   - Currently active node.
 *	    "current"	   - Currently picked node in primary bindtable.
 *	    "secondary"	   - Currently picked node in bindtable.
 *	    "next"	   - Next open node from the currently active
 *			     node.
 *	    "previous"     - Previous open node from the currently active node.
 *	    "nextsibling"  - Next sibling of the current node.
 *	    "prevsibling"  - Previous sibling of the current node.
 *	    "first"	   - Root node.
 *	    "last"	   - Last open node in the entire hierarchy.
 *	    "parent"	   - Parent of the current node.
 *	    "root"	   - Root node.
 *	    path		- Absolute path to a node.
 *
 * Results:
 *	If the string is successfully converted, TCL_OK is returned.
 *	The pointer to the node is returned via treePtrPtr.
 *	Otherwise, TCL_ERROR is returned and an error message is left
 *	in interpreter's result field.
 *
 *----------------------------------------------------------------------
 */

static int
GetNode(hierPtr, string, treePtrPtr)
    Hierbox *hierPtr;
    char *string;
    Tree **treePtrPtr;
{
    Tree *nodePtr;
    int length;
    char c;
    Tree *fromPtr;

    c = string[0];
    length = strlen(string);
    fromPtr = *treePtrPtr;
    nodePtr = NULL;
    if (isdigit(UCHAR(string[0]))) {
	nodePtr = GetNodeByIndex(hierPtr->interp, hierPtr, string);
    } else if ((c == 'c') && (strncmp(string, "current", length) == 0)) {
	if (!(hierPtr->flags & HIER_DIRTY)) {
	    nodePtr = (Tree *) Blt_GetCurrentItem(hierPtr->bindTable);
	    if (nodePtr == NULL) {
		nodePtr = (Tree *) Blt_GetCurrentItem(hierPtr->buttonBindTable);
	    }
	}
    } else if ((c == 'a') && (strncmp(string, "anchor", length) == 0)) {
	if (hierPtr->selAnchorPtr == NULL) {
	    Tcl_AppendResult(hierPtr->interp, "anchor not set in \"",
		Tk_PathName(hierPtr->tkwin), "\"", (char *)NULL);
	    return TCL_ERROR;
	}
	nodePtr = hierPtr->selAnchorPtr;
    } else if ((c == 'f') && (strncmp(string, "focus", length) == 0)) {
	nodePtr = hierPtr->focusPtr;
    } else if ((c == 'd') && (strncmp(string, "down", length) == 0)) {
	nodePtr = NextNode(fromPtr, ENTRY_OPEN | ENTRY_MAPPED);
	if (nodePtr == NULL) {
	    nodePtr = fromPtr;
	}
    } else if ((c == 'e') && (strncmp(string, "end", length) == 0)) {
	nodePtr = EndNode(hierPtr->rootPtr, ENTRY_OPEN | ENTRY_MAPPED);
    } else if ((c == 'p') && (length > 1) &&
	(strncmp(string, "prev", length) == 0)) {
	nodePtr = LastNode(fromPtr, ENTRY_OPEN | ENTRY_MAPPED);
	if (nodePtr == NULL) {
	    nodePtr = EndNode(hierPtr->rootPtr, ENTRY_OPEN | ENTRY_MAPPED);
	}
    } else if ((c == 'n') && (strncmp(string, "next", length) == 0)) {
	nodePtr = NextNode(fromPtr, ENTRY_OPEN | ENTRY_MAPPED);
	if (nodePtr == NULL) {
	    nodePtr = hierPtr->rootPtr;
	}
    } else if ((c == 'p') && (length > 1) &&
	(strncmp(string, "parent", length) == 0)) {
	nodePtr = fromPtr;
	if (nodePtr->parentPtr != NULL) {
	    nodePtr = nodePtr->parentPtr;
	}
    } else if ((c == 'r') && (strncmp(string, "root", length) == 0)) {
	nodePtr = hierPtr->rootPtr;
    } else if ((c == 'n') && (strncmp(string, "nextsibling", length) == 0)) {
	if (fromPtr->item != NULL) {
	    Blt_ListItem item;

	    item = Blt_ListNextItem(fromPtr->item);
	    if (item != NULL) {
		nodePtr = (Tree *) Blt_ListGetValue(item);
	    }
	}
    } else if ((c == 'p') && (length > 1) &&
	(strncmp(string, "prevsibling", length) == 0)) {
	if (fromPtr->item != NULL) {
	    Blt_ListItem item;

	    item = Blt_ListPrevItem(fromPtr->item);
	    if (item != NULL) {
		nodePtr = (Tree *) Blt_ListGetValue(item);
	    }
	}
    } else if ((c == 'u') && (strncmp(string, "up", length) == 0)) {
	nodePtr = LastNode(fromPtr, ENTRY_OPEN | ENTRY_MAPPED);
	if (nodePtr == NULL) {
	    nodePtr = fromPtr;
	}
    } else if ((c == 'v') && (length > 5) &&
	(strncmp(string, "view.top", length) == 0)) {
	if (hierPtr->numVisible > 0) {
	    nodePtr = hierPtr->visibleArr[0];
	}
    } else if ((c == 'v') && (length > 5) &&
	(strncmp(string, "view.bottom", length) == 0)) {
	if (hierPtr->numVisible > 0) {
	    nodePtr = hierPtr->visibleArr[hierPtr->numVisible - 1];
	}
    } else if (c == '@') {
	int x, y;

	if (Blt_GetXYPosition(hierPtr->interp, hierPtr->tkwin, string, &x, &y)
	    != TCL_OK) {
	    return TCL_ERROR;
	}
	nodePtr = NearestNode(hierPtr, x, y, TRUE);
    } else {
	nodePtr = FindPath(hierPtr, hierPtr->rootPtr, string);
	if (nodePtr == NULL) {
	    Tcl_ResetResult(hierPtr->interp);
	    Tcl_AppendResult(hierPtr->interp, "can't find node entry \"", string,
		"\" in \"", Tk_PathName(hierPtr->tkwin), "\"", (char *)NULL);
	    return TCL_ERROR;
	}
    }
    if ((nodePtr == hierPtr->rootPtr) && (hierPtr->hideRoot)) {
	nodePtr = NextNode(nodePtr, ENTRY_OPEN | ENTRY_MAPPED);
    }
    *treePtrPtr = nodePtr;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * StringToNode --
 *
 *	Like GetNode but also finds nodes by serial number.
 *	If the string starts with a digit, it's converted into a
 *	number and then looked-up in a hash table.  This means that
 *	serial identifiers take precedence over node names with
 *	the contain only numbers.
 *
 * Results:
 *	If the string is successfully converted, TCL_OK is returned.
 *	The pointer to the node is returned via treePtrPtr.
 *	Otherwise, TCL_ERROR is returned and an error message is left
 *	in interpreter's result field.
 *
 *----------------------------------------------------------------------
 */

static int
StringToNode(hierPtr, string, treePtrPtr)
    Hierbox *hierPtr;
    char *string;
    Tree **treePtrPtr;
{
    *treePtrPtr = hierPtr->focusPtr;
    if (GetNode(hierPtr, string, treePtrPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (*treePtrPtr == NULL) {
	Tcl_ResetResult(hierPtr->interp);
	Tcl_AppendResult(hierPtr->interp, "can't find node entry \"", string,
	    "\" in \"", Tk_PathName(hierPtr->tkwin), "\"", (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 * Preprocess the command string for percent substitution.
 */
static void
PercentSubst(hierPtr, treePtr, command, cmdStrPtr)
    Hierbox *hierPtr;
    Tree *treePtr;
    char *command;
    Tcl_DString *cmdStrPtr;
{
    Tcl_DString pathString;
    register char *last, *p;

    /*
     * Get the full path name of the node, in case we need to
     * substitute for it.
     */
    GetFullPath(treePtr, hierPtr->separator, &pathString);
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
		string = Tk_PathName(hierPtr->tkwin);
		break;
	    case 'P':		/* Full pathname */
		string = Tcl_DStringValue(&pathString);
		break;
	    case 'p':		/* Name of the node */
		string = treePtr->nameId;
		break;
	    case 'n':		/* Node identifier */
		string = NodeToString(hierPtr, treePtr);
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
    Tcl_DStringFree(&pathString);
}

/*
 *----------------------------------------------------------------------
 *
 * CloseNode --
 *
 *	Closes the node.  If a close Tcl command is specified (see the
 *	"-close" option), it's invoked after the node is closed.
 *
 * Results:
 *	If an error occurred when executing the Tcl proc, TCL_ERROR is
 *	returned.  Otherwise TCL_OK is returned.
 *
 *----------------------------------------------------------------------
 */
static int
CloseNode(hierPtr, treePtr)
    Hierbox *hierPtr;
    Tree *treePtr;
{
    Entry *entryPtr = treePtr->entryPtr;
    char *command;

    /*
     * Invoke the entry's "close" command, if there is one. Otherwise
     * try the hierbox's global "close" command.
     */
    command = (entryPtr->closeCmd != NULL) ? entryPtr->closeCmd :
	hierPtr->closeCmd;
    if ((entryPtr->flags & ENTRY_OPEN) && (command != NULL)) {
	Tcl_DString cmdString;
	int result;

	PercentSubst(hierPtr, treePtr, command, &cmdString);
	result = Tcl_GlobalEval(hierPtr->interp, Tcl_DStringValue(&cmdString));
	Tcl_DStringFree(&cmdString);
	if (result != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    /*
     * Mark the entry closed.
     */
    entryPtr->flags &= ~ENTRY_OPEN;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * OpenNode --
 *
 *	Opens the node.  If an open Tcl command is specified (see the
 *	"-open" option), it's invoked after the node is opened.
 *
 * Results:
 *	If an error occurred when executing the Tcl proc, TCL_ERROR is
 *	returned.  Otherwise TCL_OK is returned.
 *
 *----------------------------------------------------------------------
 */
static int
OpenNode(hierPtr, treePtr)
    Hierbox *hierPtr;
    Tree *treePtr;
{
    Entry *entryPtr = treePtr->entryPtr;
    char *command;

    /*
     * Invoke the entry's "open" command, if there is one. Otherwise
     * try the hierbox's global "open" command.
     */
    command = (entryPtr->openCmd != NULL)
	? entryPtr->openCmd : hierPtr->openCmd;
    if (!(entryPtr->flags & ENTRY_OPEN) && (command != NULL)) {
	Tcl_DString cmdString;
	int result;

	PercentSubst(hierPtr, treePtr, command, &cmdString);
	result = Tcl_GlobalEval(hierPtr->interp, Tcl_DStringValue(&cmdString));
	Tcl_DStringFree(&cmdString);
	if (result != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    /* Mark the entry open */
    entryPtr->flags |= ENTRY_OPEN;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * SelectNode --
 *
 *	Sets the selection flag for a node.  The selection flag is
 *	set/cleared/toggled based upon the flag set in the hierarchy
 *	widget.
 *
 * Results:
 *	Always returns TCL_OK.
 *
 *----------------------------------------------------------------------
 */
static int
SelectNode(hierPtr, treePtr)
    Hierbox *hierPtr;
    Tree *treePtr;
{
    Entry *entryPtr = treePtr->entryPtr;

    switch (hierPtr->flags & SELECT_MASK) {
    case SELECT_CLEAR:
	entryPtr->flags &= ~ENTRY_SELECTED;
	break;

    case SELECT_SET:
	entryPtr->flags |= ENTRY_SELECTED;
	break;

    case SELECT_TOGGLE:
	entryPtr->flags = TOGGLE(entryPtr->flags, ENTRY_SELECTED);
	break;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * SelectRange --
 *
 *	Sets the selection flag for a range of nodes.  The range is
 *	determined by two pointers which designate the first/last
 *	nodes of the range.
 *
 * Results:
 *	Always returns TCL_OK.
 *
 *----------------------------------------------------------------------
 */
static int
SelectRange(hierPtr, firstPtr, lastPtr)
    Hierbox *hierPtr;
    Tree *firstPtr, *lastPtr;
{
    register Tree *treePtr;

    /* Swap first and last pointers if the range is reversed. */
    if (IsBefore(lastPtr, firstPtr)) {
	treePtr = lastPtr;
	lastPtr = firstPtr;
	firstPtr = treePtr;
    }
    for (treePtr = firstPtr; treePtr != NULL;
	treePtr = NextNode(treePtr, ENTRY_OPEN | ENTRY_MAPPED)) {
	SelectNode(hierPtr, treePtr);
	if (treePtr == lastPtr) {
	    break;
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * IsSelectedNode --
 *
 *	Adds the name of the node the interpreter result if it is
 *	currently selected.
 *
 * Results:
 *	Always returns TCL_OK.
 *
 *----------------------------------------------------------------------
 */
static int
IsSelectedNode(hierPtr, treePtr)
    Hierbox *hierPtr;
    Tree *treePtr;
{
    if (treePtr->entryPtr->flags & ENTRY_SELECTED) {
	Tcl_AppendElement(hierPtr->interp, NodeToString(hierPtr, treePtr));
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * GetSelectedLabels --
 *
 *	This routine is invoked when the selection is exported.  Each
 *	selected entry is appended line-by-line to a dynamic string
 *	passed via the clientData field of the hierarchy widget.
 *
 * Results:
 *	Always returns TCL_OK.
 *
 *----------------------------------------------------------------------
 */
static int
GetSelectedLabels(hierPtr, treePtr)
    Hierbox *hierPtr;
    Tree *treePtr;
{
    Entry *entryPtr = treePtr->entryPtr;

    if (entryPtr->flags & ENTRY_SELECTED) {
	char *label;

	label = (entryPtr->label == NULL) ? "" : entryPtr->label;
	Tcl_DStringAppend((Tcl_DString *) hierPtr->clientData, label, -1);
	Tcl_DStringAppend((Tcl_DString *) hierPtr->clientData, "\n", -1);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ClearSubnodes --
 *
 *	Recursively clears the selection flags of the given node and
 *	and each subnode.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	If a selection command is specified (see the "-selectcommand"
 *	option), the Tcl proc is invoked at the next idle point.
 *
 *----------------------------------------------------------------------
 */
static void
ClearSubnodes(hierPtr, treePtr)
    Hierbox *hierPtr;
    Tree *treePtr;
{
    register Blt_ListItem item;
    unsigned int flags;

    flags = (hierPtr->flags & SELECT_MASK);
    hierPtr->flags &= ~SELECT_MASK;
    hierPtr->flags |= SELECT_CLEAR;
    for (item = Blt_ListFirstItem(treePtr->nodeList); item != NULL;
	item = Blt_ListNextItem(item)) {
	treePtr = (Tree *) Blt_ListGetValue(item);
	ApplyToTree(hierPtr, treePtr, SelectNode,
	    APPLY_RECURSE | APPLY_OPEN_ONLY | APPLY_BEFORE);
    }
    hierPtr->flags &= ~SELECT_MASK;
    hierPtr->flags |= flags;
    if (hierPtr->selectCmd != NULL) {
	EventuallyInvokeSelectCmd(hierPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * SizeOfNode --
 *
 *	Returns the number of children at the given node. The sum
 *	is passed via the clientData field of the hierarchy widget.
 *
 * Results:
 *	Always TCL_OK.
 *
 *----------------------------------------------------------------------
 */
static int
SizeOfNode(hierPtr, treePtr)
    Hierbox *hierPtr;
    Tree *treePtr;
{
    int *sumPtr = (int *)&(hierPtr->clientData);

    *sumPtr += Blt_ListGetLength(treePtr->nodeList);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * CompareNodesByName --
 *
 *	Comparison routine (used by qsort) to sort a list of subnodes.
 *	A simple string comparison is performed on each node name.
 *
 * Results:
 *	1 is the first is greater, -1 is the second is greater, 0
 *	if equal.
 *
 *----------------------------------------------------------------------
 */
static int
CompareNodesByName(i1Ptr, i2Ptr)
    Blt_ListItem *i1Ptr, *i2Ptr;
{
    Tree *t1Ptr, *t2Ptr;

    t1Ptr = (Tree *) Blt_ListGetValue(*i1Ptr);
    t2Ptr = (Tree *) Blt_ListGetValue(*i2Ptr);
    return strcmp(t1Ptr->nameId, t2Ptr->nameId);
}

/*
 *----------------------------------------------------------------------
 *
 * CompareNodesByTclCmd --
 *
 *	Comparison routine (used by qsort) to sort a list of subnodes.
 *	A specified Tcl proc is invoked to compare the nodes.
 *
 * Results:
 *	1 is the first is greater, -1 is the second is greater, 0
 *	if equal.
 *
 *----------------------------------------------------------------------
 */
static int
CompareNodesByTclCmd(i1Ptr, i2Ptr)
    Blt_ListItem *i1Ptr, *i2Ptr;
{
    int result;
    Tree *t1Ptr, *t2Ptr;
    Hierbox *hierPtr = hierBox;
    Tcl_Interp *interp = hierPtr->interp;

    t1Ptr = (Tree *) Blt_ListGetValue(*i1Ptr);
    t2Ptr = (Tree *) Blt_ListGetValue(*i2Ptr);
    result = 0;			/* Hopefully this will be Ok even if the
				 * Tcl command fails to return the correct
				 * result. */
    if ((Tcl_VarEval(interp, hierPtr->sortCmd, " ",
		Tk_PathName(hierPtr->tkwin), " ", NodeToString(hierPtr, t1Ptr), " ",
		NodeToString(hierPtr, t2Ptr)) != TCL_OK) ||
	(Tcl_GetInt(interp, Tcl_GetStringResult(interp), &result) != TCL_OK)) {
	Tk_BackgroundError(interp);
    }
    Tcl_ResetResult(interp);
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * SortNode --
 *
 *	Sorts the subnodes at a given node.
 *
 * Results:
 *	Always returns TCL_OK.
 *
 *----------------------------------------------------------------------
 */
static int
SortNode(hierPtr, treePtr)
    Hierbox *hierPtr;
    Tree *treePtr;
{
    if (treePtr->nodeList != NULL) {
	if (hierPtr->sortCmd != NULL) {
	    hierBox = hierPtr;
	    Blt_ListSort(treePtr->nodeList, CompareNodesByTclCmd);
	} else {
	    Blt_ListSort(treePtr->nodeList, CompareNodesByName);
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * UnmapNode --
 *
 *	Unmaps the given node.  The node will not be drawn.  Ignore
 *	unmapping of the root node (it makes no sense).
 *
 * Results:
 *	Always returns TCL_OK.
 *
 *----------------------------------------------------------------------
 */
static int
UnmapNode(hierPtr, treePtr)
    Hierbox *hierPtr;
    Tree *treePtr;
{
    if (treePtr != hierPtr->rootPtr) {
	treePtr->entryPtr->flags &= ~ENTRY_MAPPED;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * MapAncestors --
 *
 *	If a node in mapped, then all its ancestors must be mapped also.
 *	This routine traverses upwards and maps each unmapped ancestor.
 *	It's assumed that for any mapped ancestor, all it's ancestors
 *	will already be mapped too.
 *
 * Results:
 *	Always returns TCL_OK.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
MapAncestors(hierPtr, treePtr)
    Hierbox *hierPtr;		/* Not used. */
    Tree *treePtr;
{
    /*
     * Make sure that all the ancestors of this node are mapped too.
     */
    treePtr = treePtr->parentPtr;
    while (treePtr != NULL) {
	if (treePtr->entryPtr->flags & ENTRY_MAPPED) {
	    break;		/* Assume ancestors are also mapped. */
	}
	treePtr->entryPtr->flags |= ENTRY_MAPPED;
	treePtr = treePtr->parentPtr;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * MapNode --
 *
 *	Maps the given node.  Only mapped nodes are drawn.
 *
 * Results:
 *	Always returns TCL_OK.
 *
 *----------------------------------------------------------------------
 */
static int
MapNode(hierPtr, treePtr)
    Hierbox *hierPtr;
    Tree *treePtr;
{
    treePtr->entryPtr->flags |= ENTRY_MAPPED;
    MapAncestors(hierPtr, treePtr);
    return TCL_OK;
}

static int
FixUnmappedSelections(hierPtr, treePtr)
    Hierbox *hierPtr;
    Tree *treePtr;
{
    if (!(treePtr->entryPtr->flags & ENTRY_MAPPED)) {
	treePtr->entryPtr->flags &= ~ENTRY_SELECTED;
	if (IsAncestor(treePtr, hierPtr->focusPtr)) {
	    hierPtr->focusPtr = treePtr->parentPtr;
	    if (hierPtr->focusPtr == NULL) {
		hierPtr->focusPtr = hierPtr->rootPtr;
	    }
	    Blt_SetFocusItem(hierPtr->bindTable, hierPtr->focusPtr);
	}
	if (IsAncestor(treePtr, hierPtr->selAnchorPtr)) {
	    hierPtr->selMarkPtr = hierPtr->selAnchorPtr = NULL;
	}
	ClearSubnodes(hierPtr, treePtr);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyTree --
 *
 *	Recursively deletes the given node and all its subnodes.
 *
 * Results:
 *	If successful, returns TCL_OK.  Otherwise TCL_ERROR is
 *	returned.
 *
 *----------------------------------------------------------------------
 */
static int
DestroyTree(hierPtr, treePtr)
    Hierbox *hierPtr;
    Tree *treePtr;
{
    return ApplyToTree(hierPtr, treePtr, DestroyNode, APPLY_RECURSE);
}


static void
GetTags(table, object, tagArr, numTagsPtr)
    BindTable table;
    ClientData object;
    ClientData tagArr[];
    int *numTagsPtr;
{
    Hierbox *hierPtr;
    Entry *entryPtr;
    Tree *treePtr;
    int numTags;

    *numTagsPtr = 0;
    hierPtr = (Hierbox *) Blt_GetBindingData(table);
    if (hierPtr->flags & HIER_DIRTY) {
	return;
    }
    tagArr[0] = object;		/* Don't want to use the entry name and
				 * inadvertantly tag more than one entry. */
    numTags = 1;
    treePtr = (Tree *) object;
    entryPtr = treePtr->entryPtr;

    if (entryPtr->tags != NULL) {
	int numNames;
	char **nameArr;
	register char **p;

	if (Tcl_SplitList(hierPtr->interp, entryPtr->tags, &numNames,
		&nameArr) == TCL_OK) {
	    for (p = nameArr; (*p != NULL) && (numTags < 10); p++, numTags++) {
		tagArr[numTags] = (ClientData)Tk_GetUid((ClientData)*p);
	    }
	    free((char *)nameArr);
	}
    }
    *numTagsPtr = numTags;
}

/*ARGSUSED*/
static ClientData
PickButton(clientData, x, y)
    ClientData clientData;
    int x, y;			/* Screen coordinates of the test point. */
{
    Hierbox *hierPtr = (Hierbox *) clientData;
    register Entry *entryPtr;
    register Tree *treePtr;
    int buttonX, buttonY;

    if ((hierPtr->flags & HIER_DIRTY) || (hierPtr->numVisible == 0)) {
	return (ClientData) 0;
    }
    treePtr = NearestNode(hierPtr, x, y, FALSE);
    if (treePtr == NULL) {
	return (ClientData) 0;
    }
    entryPtr = treePtr->entryPtr;
    if (!(entryPtr->flags & ENTRY_BUTTON)) {
	return (ClientData) 0;
    }
    x = WORLDX(hierPtr, x);
    y = WORLDY(hierPtr, y);
    buttonX = entryPtr->worldX + entryPtr->buttonX;
    buttonY = entryPtr->worldY + entryPtr->buttonY;
    if ((x >= buttonX) && (x < (buttonX + hierPtr->buttonWidth)) &&
	(y >= buttonY) && (y < (buttonY + hierPtr->buttonHeight))) {
	return (ClientData) treePtr;
    }
    return (ClientData) 0;
}

/*ARGSUSED*/
static ClientData
PickEntry(clientData, x, y)
    ClientData clientData;
    int x, y;			/* Screen coordinates of the test point. */
{
    Hierbox *hierPtr = (Hierbox *) clientData;
    register Entry *entryPtr;
    register Tree *treePtr;
    int labelX;

    if ((hierPtr->flags & HIER_DIRTY) || (hierPtr->numVisible == 0)) {
	return (ClientData) 0;
    }
    treePtr = NearestNode(hierPtr, x, y, FALSE);
    if (treePtr == NULL) {
	return (ClientData) 0;
    }
    x = WORLDX(hierPtr, x);
    y = WORLDY(hierPtr, y);
    entryPtr = treePtr->entryPtr;
    labelX = entryPtr->worldX + LEVELWIDTH(treePtr->level);
    if (x >= labelX) {
	return (ClientData) treePtr;
    }
    return (ClientData) 0;
}

static int
ConfigureEntry(hierPtr, entryPtr, argc, argv, flags)
    Hierbox *hierPtr;
    Entry *entryPtr;
    int argc;
    char **argv;
    int flags;
{
    GC newGC;
    XGCValues gcValues;
    unsigned long gcMask;
    int entryWidth, entryHeight;
    int width, height;
    Tk_FontMetrics fontMetrics;
    Tk_Font font;

    hierBox = hierPtr;
    if (Tk_ConfigureWidget(hierPtr->interp, hierPtr->tkwin, entryConfigSpecs,
	    argc, argv, (char *)entryPtr, flags) != TCL_OK) {
	return TCL_ERROR;
    }
    entryPtr->iconWidth = entryPtr->iconHeight = 0;
    if (entryPtr->icons != NULL) {
	register int i;

	for (i = 0; i < 2; i++) {
	    if (entryPtr->icons[i] == NULL) {
		break;
	    }
	    if (entryPtr->iconWidth < ImageWidth(entryPtr->icons[i])) {
		entryPtr->iconWidth = ImageWidth(entryPtr->icons[i]);
	    }
	    if (entryPtr->iconHeight < ImageHeight(entryPtr->icons[i])) {
		entryPtr->iconHeight = ImageHeight(entryPtr->icons[i]);
	    }
	}
    }
    newGC = NULL;
    if ((entryPtr->icons == NULL) || (entryPtr->icons[0] == NULL)) {
	gcMask = GCClipMask | GCBackground;
	gcValues.clip_mask = iconMask;
	gcValues.background = iconColor->pixel;
	newGC = Tk_GetGC(hierPtr->tkwin, gcMask, &gcValues);
	entryPtr->iconWidth = DEF_ICON_WIDTH;
	entryPtr->iconHeight = DEF_ICON_HEIGHT;
    }
    entryPtr->iconWidth += 2 * ICON_PADX;
    entryPtr->iconHeight += 2 * ICON_PADY;
    if (entryPtr->iconGC != NULL) {
	Tk_FreeGC(hierPtr->display, entryPtr->iconGC);
    }
    entryPtr->iconGC = newGC;

    entryHeight = MAX(entryPtr->iconHeight, hierPtr->buttonHeight);
    entryWidth = 0;

    gcMask = GCForeground | GCFont;
    gcValues.foreground = hierPtr->textColor->pixel;
    if (entryPtr->labelColor != NULL) {
	gcValues.foreground = entryPtr->labelColor->pixel;
    }
    font = hierPtr->font;
    if (entryPtr->labelFont != NULL) {
	font = entryPtr->labelFont;
    }
    gcValues.font = Tk_FontId(font);
    newGC = Tk_GetGC(hierPtr->tkwin, gcMask, &gcValues);
    if (entryPtr->labelGC != NULL) {
	Tk_FreeGC(hierPtr->display, entryPtr->labelGC);
    }
    entryPtr->labelGC = newGC;

    if (entryPtr->label != NULL) {
	TextAttributes attr;

	Blt_InitTextAttributes(&attr);
	attr.shadow.offset = entryPtr->labelShadow.offset;
	attr.font = font;
	Blt_GetTextExtents(&attr, entryPtr->label, &width, &height);
    } else {
	Tk_GetFontMetrics(font, &fontMetrics);
	width = height = fontMetrics.linespace;
    }
    width += 2 * (LABEL_PADX + hierPtr->selBorderWidth);
    height += 2 * (LABEL_PADY + hierPtr->selBorderWidth);
    entryWidth += width;
    if (entryHeight < height) {
	entryHeight = height;
    }
    entryPtr->labelWidth = width;
    width = height = 0;
    if (entryPtr->images != NULL) {
	register HierImage *imagePtr;

	for (imagePtr = entryPtr->images; *imagePtr != NULL; imagePtr++) {
	    width += ImageWidth(*imagePtr);
	    if (height < ImageHeight(*imagePtr)) {
		height = ImageHeight(*imagePtr);
	    }
	}
    } else if (entryPtr->text != NULL) {
	TextAttributes attr;

	gcMask = GCForeground | GCFont;
	gcValues.foreground = (entryPtr->textColor == NULL)
	    ? hierPtr->textColor->pixel : entryPtr->textColor->pixel;
	font = (entryPtr->textFont == NULL)
	    ? hierPtr->font : entryPtr->textFont;
	gcValues.font = Tk_FontId(font);
	newGC = Tk_GetGC(hierPtr->tkwin, gcMask, &gcValues);
	if (entryPtr->textGC != NULL) {
	    Tk_FreeGC(hierPtr->display, entryPtr->textGC);
	}
	entryPtr->textGC = newGC;

	Blt_InitTextAttributes(&attr);
	attr.font = font;
	attr.shadow.offset = entryPtr->textShadow.offset;
	Blt_GetTextExtents(&attr, entryPtr->text, &width, &height);
	width += 2 * LABEL_PADX;
	height += 2 * LABEL_PADY;
    }
    entryWidth += width;
    if (entryHeight < height) {
	entryHeight = height;
    }
    entryPtr->width = entryWidth + 4;
    entryPtr->height = entryHeight + hierPtr->leader;

    /*
     * Force the height of the entry to an even number. This is to
     * make the dots or the vertical line segments coincide with the
     * start of the horizontal lines.
     */
    if (entryPtr->height & 0x01) {
	entryPtr->height++;
    }
    hierPtr->flags |= HIER_LAYOUT;
    EventuallyRedraw(hierPtr);
    return TCL_OK;
}

/*
 * Hierbox Procedures
 */
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
EventuallyRedraw(hierPtr)
    Hierbox *hierPtr;
{
    if ((hierPtr->tkwin != NULL) && !(hierPtr->flags & HIER_REDRAW)) {
	hierPtr->flags |= HIER_REDRAW;
	Tk_DoWhenIdle(DisplayHierbox, (ClientData)hierPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * EventuallyInvokeSelectCmd --
 *
 *      Queues a request to execute the -selectcommand code associated
 *      with the widget at the next idle point.  Invoked whenever the
 *      selection changes.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      Tcl code gets executed for some application-specific task.
 *
 *----------------------------------------------------------------------
 */
static void
EventuallyInvokeSelectCmd(hierPtr)
    Hierbox *hierPtr;
{
    if (!(hierPtr->flags & SELECT_PENDING)) {
	hierPtr->flags |= SELECT_PENDING;
	Tk_DoWhenIdle(SelectCmdProc, (ClientData)hierPtr);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * CreateHierbox --
 *
 * ----------------------------------------------------------------------
 */
static Hierbox *
CreateHierbox(interp, tkwin)
    Tcl_Interp *interp;
    Tk_Window tkwin;
{
    Hierbox *hierPtr;

    hierPtr = (Hierbox *) calloc(1, sizeof(Hierbox));
    assert(hierPtr);

    Tk_SetClass(tkwin, "Hierbox");
    hierPtr->tkwin = tkwin;
    hierPtr->display = Tk_Display(tkwin);
    hierPtr->interp = interp;

    hierPtr->leader = 0;
    hierPtr->dashes = 1;
    hierPtr->highlightWidth = 2;
    hierPtr->selBorderWidth = 1;
    hierPtr->borderWidth = 2;
    hierPtr->relief = TK_RELIEF_SUNKEN;
    hierPtr->activeRelief = TK_RELIEF_FLAT;
    hierPtr->selRelief = TK_RELIEF_RAISED;
    hierPtr->closeRelief = TK_RELIEF_RAISED;
    hierPtr->openRelief = TK_RELIEF_SUNKEN;
    hierPtr->reqWidth = 200;
    hierPtr->reqHeight = 400;
    hierPtr->lineWidth = 1;
    hierPtr->button.borderWidth = 1;
    Tcl_InitHashTable(&(hierPtr->nodeTable), TCL_ONE_WORD_KEYS);
    Tcl_InitHashTable(&(hierPtr->imageTable), TCL_STRING_KEYS);
    hierPtr->bindTable = Blt_CreateBindingTable(interp, tkwin,
	(ClientData)hierPtr, PickEntry, GetTags);
    hierPtr->buttonBindTable = Blt_CreateBindingTable(interp, tkwin,
	(ClientData)hierPtr, PickButton, GetTags);
    return hierPtr;
}

/*
 * ----------------------------------------------------------------------
 *
 * DestroyHierbox --
 *
 * 	This procedure is invoked by Tk_EventuallyFree or Tk_Release
 *	to clean up the internal structure of a Hierbox at a safe time
 *	(when no-one is using it anymore).
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Everything associated with the widget is freed up.
 *
 * ----------------------------------------------------------------------
 */
static void
DestroyHierbox(dataPtr)
    DestroyData dataPtr;	/* Pointer to the widget record. */
{
    Hierbox *hierPtr = (Hierbox *) dataPtr;

    if (hierPtr->tkwin != NULL) {
	Tk_DeleteSelHandler(hierPtr->tkwin, XA_PRIMARY, XA_STRING);
    }
    if (hierPtr->tileGC != NULL) {
	Tk_FreeGC(hierPtr->display, hierPtr->tileGC);
    }
    if (hierPtr->highlightGC != NULL) {
	Tk_FreeGC(hierPtr->display, hierPtr->highlightGC);
    }
    if (hierPtr->lineGC != NULL) {
	Tk_FreeGC(hierPtr->display, hierPtr->lineGC);
    }
    if (hierPtr->activeGC != NULL) {
	Blt_FreePrivateGC(hierPtr->display, hierPtr->activeGC);
    }
    if (hierPtr->bgTile != NULL) {
	Blt_FreeTile(hierPtr->bgTile);
    }
    if (hierPtr->visibleArr != NULL) {
	free((char *)hierPtr->visibleArr);
    }
    if (hierPtr->levelInfo != NULL) {
	free((char *)hierPtr->levelInfo);
    }
    if (hierPtr->buttons != NULL) {
	register HierImage *imagePtr;

	for (imagePtr = hierPtr->buttons; *imagePtr != NULL; imagePtr++) {
	    FreeImage(*imagePtr);
	}
	free((char *)hierPtr->buttons);
    }
    if (hierPtr->button.activeGC != NULL) {
	Tk_FreeGC(hierPtr->display, hierPtr->button.activeGC);
    }
    if (hierPtr->button.normalGC != NULL) {
	Tk_FreeGC(hierPtr->display, hierPtr->button.normalGC);
    }
    DestroyTree(hierPtr, hierPtr->rootPtr);
    Tk_FreeOptions(configSpecs, (char *)hierPtr, hierPtr->display, 0);
    Tcl_DeleteHashTable(&(hierPtr->nodeTable));

    Blt_DestroyBindingTable(hierPtr->bindTable);
    Blt_DestroyBindingTable(hierPtr->buttonBindTable);
    free((char *)hierPtr);
}

/*
 * --------------------------------------------------------------
 *
 * HierboxEventProc --
 *
 * 	This procedure is invoked by the Tk dispatcher for various
 * 	events on hierarchy widgets.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	When the window gets deleted, internal structures get
 *	cleaned up.  When it gets exposed, it is redisplayed.
 *
 * --------------------------------------------------------------
 */
static void
HierboxEventProc(clientData, eventPtr)
    ClientData clientData;	/* Information about window. */
    XEvent *eventPtr;		/* Information about event. */
{
    Hierbox *hierPtr = (Hierbox *) clientData;

    if (eventPtr->type == Expose) {
	if (eventPtr->xexpose.count == 0) {
	    EventuallyRedraw(hierPtr);
	}
    } else if (eventPtr->type == ConfigureNotify) {
	hierPtr->flags |= (HIER_LAYOUT | HIER_SCROLL);
	EventuallyRedraw(hierPtr);
    } else if ((eventPtr->type == FocusIn) || (eventPtr->type == FocusOut)) {
	if (eventPtr->xfocus.detail != NotifyInferior) {
	    if (eventPtr->type == FocusIn) {
		hierPtr->flags |= HIER_FOCUS;
	    } else {
		hierPtr->flags &= ~HIER_FOCUS;
	    }
	    EventuallyRedraw(hierPtr);
	}
    } else if (eventPtr->type == DestroyNotify) {
	if (hierPtr->tkwin != NULL) {
	    char *cmdName;

	    cmdName = Tcl_GetCommandName(hierPtr->interp, hierPtr->cmdToken);
#ifdef ITCL_NAMESPACES
	    Itk_SetWidgetCommand(hierPtr->tkwin, (Tcl_Command) NULL);
#endif /* ITCL_NAMESPACES */
	    Tcl_DeleteCommand(hierPtr->interp, cmdName);
	    hierPtr->tkwin = NULL;
	}
	if (hierPtr->flags & HIER_REDRAW) {
	    Tk_CancelIdleCall(DisplayHierbox, (ClientData)hierPtr);
	}
	if (hierPtr->flags & SELECT_PENDING) {
	    Tk_CancelIdleCall(SelectCmdProc, (ClientData)hierPtr);
	}
	Tcl_EventuallyFree((ClientData)hierPtr, DestroyHierbox);
    }
}

/* Selection Procedures */
/*
 *----------------------------------------------------------------------
 *
 * SelectionProc --
 *
 *	This procedure is called back by Tk when the selection is
 *	requested by someone.  It returns part or all of the selection
 *	in a buffer provided by the caller.
 *
 * Results:
 *	The return value is the number of non-NULL bytes stored at
 *	buffer.  Buffer is filled (or partially filled) with a
 *	NULL-terminated string containing part or all of the
 *	selection, as given by offset and maxBytes.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static int
SelectionProc(clientData, offset, buffer, maxBytes)
    ClientData clientData;	/* Information about the widget. */
    int offset;			/* Offset within selection of first
				 * character to be returned. */
    char *buffer;		/* Location in which to place
				 * selection. */
    int maxBytes;		/* Maximum number of bytes to place
				 * at buffer, not including terminating
				 * NULL character. */
{
    Hierbox *hierPtr = (Hierbox *) clientData;
    int size;
    Tcl_DString dString;

    if (!hierPtr->exportSelection) {
	return -1;
    }
    /*
     * Retrieve the names of the selected entries.
     */
    Tcl_DStringInit(&dString);
    hierPtr->clientData = (ClientData)&dString;
    ApplyToTree(hierPtr, hierPtr->rootPtr, GetSelectedLabels,
	APPLY_RECURSE | APPLY_BEFORE | APPLY_OPEN_ONLY);
    size = Tcl_DStringLength(&dString) - offset;
    strncpy(buffer, Tcl_DStringValue(&dString) + offset, maxBytes);
    Tcl_DStringFree(&dString);
    buffer[maxBytes] = '\0';
    return (size > maxBytes) ? maxBytes : size;
}

/*
 *----------------------------------------------------------------------
 *
 * LostSelection --
 *
 *	This procedure is called back by Tk when the selection is grabbed
 *	away.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The existing selection is unhighlighted, and the window is
 *	marked as not containing a selection.
 *
 *----------------------------------------------------------------------
 */
static void
LostSelection(clientData)
    ClientData clientData;	/* Information about the widget. */
{
    Hierbox *hierPtr = (Hierbox *) clientData;

    if ((hierPtr->selAnchorPtr != NULL) && (hierPtr->exportSelection)) {
	hierPtr->selMarkPtr = hierPtr->selAnchorPtr = NULL;

	hierPtr->flags &= ~SELECT_MASK;
	hierPtr->flags |= SELECT_CLEAR;
	ApplyToTree(hierPtr, hierPtr->rootPtr, SelectNode,
	    APPLY_RECURSE | APPLY_OPEN_ONLY | APPLY_BEFORE);
	hierPtr->flags &= ~SELECT_MASK;
	EventuallyRedraw(hierPtr);
	if (hierPtr->selectCmd != NULL) {
	    EventuallyInvokeSelectCmd(hierPtr);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * HierboxInstDeletedCmd --
 *
 *	This procedure is invoked when a widget command is deleted.  If
 *	the widget isn't already in the process of being destroyed,
 *	this command destroys it.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The widget is destroyed.
 *
 *----------------------------------------------------------------------
 */
static void
HierboxInstDeletedCmd(clientData)
    ClientData clientData;	/* Pointer to widget record for widget. */
{
    Hierbox *hierPtr = (Hierbox *) clientData;

    /*
     * This procedure could be invoked either because the window was
     * destroyed and the command was then deleted (in which case tkwin
     * is NULL) or because the command was deleted, and then this
     * procedure destroys the widget.
     */
    if (hierPtr->tkwin != NULL) {
	Tk_Window tkwin;

	tkwin = hierPtr->tkwin;
	hierPtr->tkwin = NULL;
	Tk_DestroyWindow(tkwin);
#ifdef ITCL_NAMESPACES
	Itk_SetWidgetCommand(tkwin, (Tcl_Command) NULL);
#endif /* ITCL_NAMESPACES */
    }
}

/*
 *----------------------------------------------------------------------
 *
 * TileChangedProc
 *
 *	Stub for image change notifications.  Since we immediately draw
 *	the image into a pixmap, we don't care about image changes.
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
    Hierbox *hierPtr = (Hierbox *) clientData;

    if (hierPtr->tkwin != NULL) {
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
	    newGC = Tk_GetGC(hierPtr->tkwin, gcMask, &gcValues);
	}
	if (hierPtr->tileGC != NULL) {
	    Tk_FreeGC(hierPtr->display, hierPtr->tileGC);
	}
	hierPtr->tileGC = newGC;
	EventuallyRedraw(hierPtr);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * ConfigureHierbox --
 *
 * 	This procedure is called to process an argv/argc list, plus
 *	the Tk option database, in order to configure (or reconfigure)
 *	the widget.
 *
 * Results:
 *	The return value is a standard Tcl result.  If TCL_ERROR is
 * 	returned, then interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information, such as text string, colors, font,
 *	etc. get set for hierPtr; old resources get freed, if there
 *	were any.  The widget is redisplayed.
 *
 * ----------------------------------------------------------------------
 */
static int
ConfigureHierbox(interp, hierPtr, argc, argv, flags)
    Tcl_Interp *interp;		/* Interpreter to report errors back to. */
    Hierbox *hierPtr;		/* Information about widget; may or may not
			         * already have values for some fields. */
    int argc;
    char **argv;
    int flags;
{
    XGCValues gcValues;
    unsigned long gcMask;
    GC newGC;

    hierBox = hierPtr;
    if (Tk_ConfigureWidget(interp, hierPtr->tkwin, configSpecs, argc, argv,
	    (char *)hierPtr, flags) != TCL_OK) {
	return TCL_ERROR;
    }
    if (Blt_ConfigModified(configSpecs, "-font", "-linespacing", "-width",
	    "-height", "-hideroot", (char *)NULL)) {
	/*
	 * These options change the layout of the box.
	 */
	hierPtr->flags |= (HIER_LAYOUT | HIER_SCROLL);	/* Mark for update */
    }
    if ((hierPtr->reqHeight != Tk_ReqHeight(hierPtr->tkwin)) ||
	(hierPtr->reqWidth != Tk_ReqWidth(hierPtr->tkwin))) {
	Tk_GeometryRequest(hierPtr->tkwin, hierPtr->reqWidth,
	    hierPtr->reqHeight);
    }
    gcMask = (GCForeground | GCLineWidth);
    gcValues.foreground = hierPtr->lineColor->pixel;
    gcValues.line_width = hierPtr->lineWidth;
    if (hierPtr->dashes > 0) {
	gcMask |= (GCLineStyle | GCDashList);
	gcValues.line_style = LineOnOffDash;
	gcValues.dashes = hierPtr->dashes;
    }
    newGC = Tk_GetGC(hierPtr->tkwin, gcMask, &gcValues);
    if (hierPtr->lineGC != NULL) {
	Tk_FreeGC(hierPtr->display, hierPtr->lineGC);
    }
    hierPtr->lineGC = newGC;

    /*
     * GC for focus highlight.
     */
    gcValues.foreground = hierPtr->highlightColor->pixel;
    newGC = Tk_GetGC(hierPtr->tkwin, gcMask, &gcValues);
    if (hierPtr->highlightGC != NULL) {
	Tk_FreeGC(hierPtr->display, hierPtr->highlightGC);
    }
    hierPtr->highlightGC = newGC;

    /*
     * GC for active label. Dashed outline.
     */
    gcMask = GCForeground | GCLineStyle;
    gcValues.foreground = hierPtr->activeFgColor->pixel;
    gcValues.line_style = (hierPtr->button.dashes.numValues > 0)
	? LineOnOffDash : LineSolid;
    newGC = Blt_GetPrivateGC(hierPtr->tkwin, gcMask, &gcValues);
    if (hierPtr->button.dashes.numValues > 0) {
	hierPtr->button.dashes.offset = 2;
	Blt_SetDashes(hierPtr->display, newGC, &(hierPtr->button.dashes));
    }
    if (hierPtr->activeGC != NULL) {
	Blt_FreePrivateGC(hierPtr->display, hierPtr->activeGC);
    }
    hierPtr->activeGC = newGC;

    /*
     * GC for tiled background.
     */
    newGC = NULL;
    if (hierPtr->bgTile != NULL) {
	Pixmap pixmap;

	Blt_SetTileChangedProc(hierPtr->bgTile, TileChangedProc,
	    (ClientData)hierPtr);
	pixmap = Blt_PixmapOfTile(hierPtr->bgTile);
	if (pixmap != None) {
	    gcMask = (GCTile | GCFillStyle);
	    gcValues.fill_style = FillTiled;
	    gcValues.tile = pixmap;
	    newGC = Tk_GetGC(hierPtr->tkwin, gcMask, &gcValues);
	}
    }
    if (hierPtr->tileGC != NULL) {
	Tk_FreeGC(hierPtr->display, hierPtr->tileGC);
    }
    hierPtr->tileGC = newGC;

    ConfigureButtons(hierPtr);

    hierPtr->inset = hierPtr->highlightWidth + hierPtr->borderWidth + 2;
    EventuallyRedraw(hierPtr);
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * ResetCoordinates --
 *
 *	Determines the maximum height of all visible entries.
 *
 * Results:
 *	Returns 1 if beyond the last visible entry, 0 otherwise.
 *
 * Side effects:
 *	The array of visible nodes is filled.
 *
 * ----------------------------------------------------------------------
 */
static void
ResetCoordinates(hierPtr, treePtr, infoPtr)
    Hierbox *hierPtr;
    Tree *treePtr;
    LayoutInfo *infoPtr;
{
    Entry *entryPtr = treePtr->entryPtr;
    int width;

    /*
     * If the entry is hidden, then do nothing.
     * Otherwise, include it in the layout.
     */
    entryPtr->worldY = infoPtr->y;
    if (!(entryPtr->flags & ENTRY_MAPPED)) {
	return;
    }
    treePtr->level = infoPtr->level;
    if (infoPtr->depth < infoPtr->level) {
	infoPtr->depth = infoPtr->level;
    }
    if ((entryPtr->flags & BUTTON_SHOW) || ((entryPtr->flags & BUTTON_AUTO) &&
	    (Blt_ListGetLength(treePtr->nodeList) > 0))) {
	entryPtr->flags |= ENTRY_BUTTON;
    } else {
	entryPtr->flags &= ~ENTRY_BUTTON;
    }
    if (entryPtr->height < infoPtr->minHeight) {
	infoPtr->minHeight = entryPtr->height;
    }
    /*
     * Note: The maximum entry width below does not take into account
     *       the space for the icon (level offset).  This has to be
     *       deferred because it's dependent upon the maximum icon
     *       size.
     */
    width = infoPtr->x + entryPtr->width;
    if (width > infoPtr->maxWidth) {
	infoPtr->maxWidth = width;
    }
    if (infoPtr->maxIconWidth < entryPtr->iconWidth) {
	infoPtr->maxIconWidth = entryPtr->iconWidth;
    }
    entryPtr->lineHeight = -(infoPtr->y);
    infoPtr->y += entryPtr->height;
    if (entryPtr->flags & ENTRY_OPEN) {
	register Blt_ListItem item;
	int labelOffset;
	Tree *bottomPtr;

	infoPtr->level++;
	labelOffset = infoPtr->labelOffset;
	infoPtr->labelOffset = 0;
	bottomPtr = treePtr;
	for (item = Blt_ListFirstItem(treePtr->nodeList); item != NULL;
	    item = Blt_ListNextItem(item)) {
	    treePtr = (Tree *) Blt_ListGetValue(item);
	    if (treePtr->entryPtr->flags & ENTRY_MAPPED) {
		ResetCoordinates(hierPtr, treePtr, infoPtr);
		bottomPtr = treePtr;
	    }
	}
	infoPtr->level--;
	entryPtr->lineHeight += bottomPtr->entryPtr->worldY;
	entryPtr->levelX = infoPtr->labelOffset;
	infoPtr->labelOffset = labelOffset;
    }
    if (infoPtr->labelOffset < entryPtr->labelWidth) {
	infoPtr->labelOffset = entryPtr->labelWidth;
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * ComputeWidths --
 *
 *	Determines the maximum height of all visible entries.
 *
 * Results:
 *	Returns 1 if beyond the last visible entry, 0 otherwise.
 *
 * Side effects:
 *	The array of visible nodes is filled.
 *
 * ----------------------------------------------------------------------
 */
static void
ComputeWidths(hierPtr, treePtr)
    Hierbox *hierPtr;
    Tree *treePtr;
{
    Entry *entryPtr = treePtr->entryPtr;

    if (!(entryPtr->flags & ENTRY_MAPPED)) {
	return;
    }
    if (entryPtr->iconWidth > LEVELWIDTH(treePtr->level + 1)) {
	LEVELWIDTH(treePtr->level + 1) = entryPtr->iconWidth;
    }
    if (entryPtr->flags & ENTRY_OPEN) {
	register Blt_ListItem item;

	for (item = Blt_ListFirstItem(treePtr->nodeList); item != NULL;
	    item = Blt_ListNextItem(item)) {
	    treePtr = (Tree *) Blt_ListGetValue(item);
	    if (treePtr->entryPtr->flags & ENTRY_MAPPED) {
		ComputeWidths(hierPtr, treePtr);
	    }
	}
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * ComputeLayout --
 *
 *	Recompute the layout when entries are opened/closed,
 *	inserted/deleted, or when text attributes change (such as
 *	font, linespacing).
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The world coordinates are set for all the opened entries.
 *
 * ----------------------------------------------------------------------
 */
static void
ComputeLayout(hierPtr)
    Hierbox *hierPtr;
{
    LayoutInfo info;

    info.level = info.depth = 0;
    info.x = info.y = 0;
    info.maxWidth = hierPtr->buttonWidth;
    info.maxIconWidth = hierPtr->buttonWidth;
    info.minHeight = INT_MAX;
    info.labelOffset = 0;

    if (hierPtr->hideRoot) {
	info.y = -(hierPtr->rootPtr->entryPtr->height);
    }
    ResetCoordinates(hierPtr, hierPtr->rootPtr, &info);

    hierPtr->xScrollUnits = info.maxIconWidth;
    hierPtr->minHeight = hierPtr->yScrollUnits = info.minHeight;
    if (hierPtr->reqScrollX > 0) {
	hierPtr->xScrollUnits = hierPtr->reqScrollX;
    }
    if (hierPtr->reqScrollY > 0) {
	hierPtr->yScrollUnits = hierPtr->reqScrollY;
    }
    hierPtr->depth = info.depth + 1;
    hierPtr->worldWidth = info.maxWidth;
    if (hierPtr->worldWidth < 1) {
	hierPtr->worldWidth = 1;
    }
    hierPtr->worldHeight = info.y;
    if (hierPtr->worldHeight < 1) {
	hierPtr->worldHeight = 1;
    }
    if (hierPtr->yScrollUnits < 1) {
	hierPtr->yScrollUnits = 1;
    }
    if (hierPtr->xScrollUnits < 1) {
	hierPtr->xScrollUnits = 1;
    }
    if (hierPtr->levelInfo != NULL) {
	free((char *)hierPtr->levelInfo);
    }
    hierPtr->levelInfo = (LevelInfo *)
	calloc(hierPtr->depth + 2, sizeof(LevelInfo));
    assert(hierPtr->levelInfo);
    ComputeWidths(hierPtr, hierPtr->rootPtr);
    {
	int sum, width;
	register int i;

	sum = 0;
	for (i = 0; i <= hierPtr->depth; i++) {
	    width = hierPtr->levelInfo[i].width;
	    width |= 0x01;
	    hierPtr->levelInfo[i].width = width;
	    sum += width;
	    hierPtr->levelInfo[i + 1].x = sum;
	}
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * ComputeVisibleEntries --
 *
 *	The entries visible in the viewport (the widget's window) are
 *	inserted into the array of visible nodes.
 *
 * Results:
 *	Returns 1 if beyond the last visible entry, 0 otherwise.
 *
 * Side effects:
 *	The array of visible nodes is filled.
 *
 * ----------------------------------------------------------------------
 */
static int
ComputeVisibleEntries(hierPtr)
    Hierbox *hierPtr;
{
    Entry *entryPtr;
    int height;
    register Blt_ListItem item;
    register Tree *treePtr;
    int x, maxX;
    int numSlots;

    hierPtr->xOffset = Blt_AdjustViewport(hierPtr->xOffset, hierPtr->worldWidth,
	VPORTWIDTH(hierPtr), hierPtr->xScrollUnits, hierPtr->scrollMode);
    hierPtr->yOffset = Blt_AdjustViewport(hierPtr->yOffset,
	hierPtr->worldHeight, VPORTHEIGHT(hierPtr), hierPtr->yScrollUnits,
	hierPtr->scrollMode);

    height = VPORTHEIGHT(hierPtr);

    /* Allocate worst case number of slots for entry array. */
    numSlots = (height / hierPtr->minHeight) + 3;
    if ((numSlots != hierPtr->numVisible) && (hierPtr->visibleArr != NULL)) {
	free((char *)hierPtr->visibleArr);
    }
    hierPtr->visibleArr = (Tree **) malloc(numSlots * sizeof(Tree *));
    assert(hierPtr->visibleArr);
    hierPtr->numVisible = 0;

    /* Find the node where the view port starts. */
    treePtr = hierPtr->rootPtr;
    entryPtr = treePtr->entryPtr;
    while ((entryPtr->worldY + entryPtr->height) <= hierPtr->yOffset) {
	for (item = Blt_ListLastItem(treePtr->nodeList); item != NULL;
	    item = Blt_ListPrevItem(item)) {
	    treePtr = (Tree *) Blt_ListGetValue(item);
	    if (IsHidden(treePtr)) {
		continue;	/* Ignore hidden entries.  */
	    }
	    entryPtr = treePtr->entryPtr;
	    if (entryPtr->worldY <= hierPtr->yOffset) {
		break;
	    }
	}

	/*
         * If we can't find the starting node, then the view must be
	 * scrolled down, but some nodes were deleted.  Reset the view
         * back to the top and try again.
         */
	if (item == NULL) {
	    if (hierPtr->yOffset == 0) {
		return TCL_OK;	/* All entries are hidden. */
	    }
	    hierPtr->yOffset = 0;
	    continue;
	}
    }

    height += hierPtr->yOffset;
    maxX = 0;
    while (treePtr != NULL) {
	if (!IsHidden(treePtr)) {
	    entryPtr = treePtr->entryPtr;
	    /*
	     * Compute and save the entry's X-coordinate now that we know
	     * what the maximum level offset for the entire Hierbox is.
	     */
	    entryPtr->worldX = LEVELX(treePtr->level);
	    x = entryPtr->worldX + LEVELWIDTH(treePtr->level) +
		LEVELWIDTH(treePtr->level + 1) + entryPtr->width;
	    if (x > maxX) {
		maxX = x;
	    }
	    if (entryPtr->worldY >= height) {
		break;
	    }
	    hierPtr->visibleArr[hierPtr->numVisible] = treePtr;
	    hierPtr->numVisible++;
	}
	treePtr = NextNode(treePtr, ENTRY_OPEN | ENTRY_MAPPED);
    }
    hierPtr->visibleArr[hierPtr->numVisible] = NULL;
    hierPtr->worldWidth = maxX;

    /*
     * -------------------------------------------------------------------
     *
     * Note:	It's assumed that the view port always starts at or
     *		over an entry.  Check that a change in the hierarchy
     *		(e.g. closing a node) hasn't left the viewport beyond
     *		the last entry.  If so, adjust the viewport to start
     *		on the last entry.
     *
     * -------------------------------------------------------------------
     */
    if (hierPtr->xOffset > (hierPtr->worldWidth - hierPtr->xScrollUnits)) {
	hierPtr->xOffset = hierPtr->worldWidth - hierPtr->xScrollUnits;
    }
    if (hierPtr->yOffset > (hierPtr->worldHeight - hierPtr->yScrollUnits)) {
	hierPtr->yOffset = hierPtr->worldHeight - hierPtr->yScrollUnits;
    }
    hierPtr->xOffset = Blt_AdjustViewport(hierPtr->xOffset, hierPtr->worldWidth,
	VPORTWIDTH(hierPtr), hierPtr->xScrollUnits, hierPtr->scrollMode);
    hierPtr->yOffset = Blt_AdjustViewport(hierPtr->yOffset,
	hierPtr->worldHeight, VPORTHEIGHT(hierPtr), hierPtr->yScrollUnits,
	hierPtr->scrollMode);
    Blt_PickCurrentItem(hierPtr->bindTable);
    Blt_PickCurrentItem(hierPtr->buttonBindTable);
    hierPtr->flags &= ~HIER_DIRTY;
    return TCL_OK;
}

/*
 * ---------------------------------------------------------------------------
 *
 * DrawVerticals --
 *
 * 	Draws vertical lines for the ancestor nodes.  While the entry
 *	of the ancestor may not be visible, its vertical line segment
 *	does extent into the viewport.  So walk back up the hierarchy
 *	drawing lines until we get to the root.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Vertical lines are drawn for the ancestor nodes.
 *
 * ---------------------------------------------------------------------------
 */
static void
DrawVerticals(hierPtr, treePtr, drawable)
    Hierbox *hierPtr;		/* Widget record containing the attribute
				 * information for buttons. */
    Tree *treePtr;		/* Entry to be drawn. */
    Drawable drawable;		/* Pixmap or window to draw into. */
{
    Entry *entryPtr;		/* Entry to be drawn. */
    int x1, y1, x2, y2;
    int height;
    int x, y;

    while (treePtr->parentPtr != NULL) {
	treePtr = treePtr->parentPtr;
	entryPtr = treePtr->entryPtr;

	/*
	 * World X-coordinates are computed only for entries that are in
	 * the current view port.  So for each of the off-screen ancestor
	 * nodes we must compute it here too.
	 */
	entryPtr->worldX = LEVELX(treePtr->level);
	x = SCREENX(hierPtr, entryPtr->worldX);
	y = SCREENY(hierPtr, entryPtr->worldY);
	height = MAX(entryPtr->iconHeight, hierPtr->buttonHeight);
	y += (height - hierPtr->buttonHeight) / 2;
	x1 = x2 = x + LEVELWIDTH(treePtr->level) +
	    LEVELWIDTH(treePtr->level + 1) / 2;
	y1 = y + hierPtr->buttonHeight / 2;
	y2 = y1 + entryPtr->lineHeight;

	if ((treePtr == hierPtr->rootPtr) && (hierPtr->hideRoot)) {
	    y1 += entryPtr->height;
	}
	/*
	 * Clip the line's Y-coordinates at the window border.
	 */
	if (y1 < 0) {
	    y1 = 0;
	}
	if (y2 > Tk_Height(hierPtr->tkwin)) {
	    y2 = Tk_Height(hierPtr->tkwin);
	}
	XDrawLine(hierPtr->display, drawable, hierPtr->lineGC, x1, y1, x2, y2);
    }
}

/*
 * ---------------------------------------------------------------------------
 *
 * DrawEntryButton --
 *
 * 	Draws a button for the given entry. The button is drawn
 * 	centered in the region immediately to the left of the origin
 * 	of the entry (computed in the layout routines). The height
 * 	and width of the button were previously calculated from the
 * 	average row height.
 *
 *		button height = entry height - (2 * some arbitrary padding).
 *		button width = button height.
 *
 *	The button may have a border.  The symbol (either a plus or
 *	minus) is slight smaller than the width or height minus the
 *	border.
 *
 *	    x,y origin of entry
 *
 *              +---+
 *              | + | icon label
 *              +---+
 *             closed
 *
 *           |----|----| horizontal offset
 *
 *              +---+
 *              | - | icon label
 *              +---+
 *              open
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	A button is drawn for the entry.
 *
 * ---------------------------------------------------------------------------
 */
static void
DrawEntryButton(hierPtr, treePtr, drawable)
    Hierbox *hierPtr;		/* Widget record containing the attribute
				 * information for buttons. */
    Tree *treePtr;		/* Node of entry. */
    Drawable drawable;		/* Pixmap or window to draw into. */
{
    Button *buttonPtr = &(hierPtr->button);
    Entry *entryPtr;
    int relief;
    Tk_3DBorder border;
    GC gc;
    int x, y;
    HierImage image;
    int width, height;

    entryPtr = treePtr->entryPtr;

    width = LEVELWIDTH(treePtr->level);
    height = MAX(entryPtr->iconHeight, hierPtr->buttonHeight);
    entryPtr->buttonX = (width - hierPtr->buttonWidth) / 2;
    entryPtr->buttonY = (height - hierPtr->buttonHeight) / 2;

    x = SCREENX(hierPtr, entryPtr->worldX) + entryPtr->buttonX;
    y = SCREENY(hierPtr, entryPtr->worldY) + entryPtr->buttonY;

    if (treePtr == hierPtr->activeButtonPtr) {
	border = buttonPtr->activeBorder;
	gc = buttonPtr->activeGC;
    } else {
	border = buttonPtr->border;
	gc = buttonPtr->normalGC;
    }
    if (treePtr->entryPtr->flags & ENTRY_OPEN) {
	relief = hierPtr->openRelief;
    } else {
	relief = hierPtr->closeRelief;
    }
    Tk_Fill3DRectangle(hierPtr->tkwin, drawable, border, x, y,
	hierPtr->buttonWidth, hierPtr->buttonHeight, buttonPtr->borderWidth,
	relief);
    if (relief == TK_RELIEF_FLAT) {
	/* Simulate TK_RELIEF_SOLID */
	XDrawRectangle(hierPtr->display, drawable, gc, x, y,
	    hierPtr->buttonWidth - 1, hierPtr->buttonHeight - 1);
    }
    x += buttonPtr->borderWidth;
    y += buttonPtr->borderWidth;
    width = hierPtr->buttonWidth - (2 * buttonPtr->borderWidth);
    height = hierPtr->buttonHeight - (2 * buttonPtr->borderWidth);

    image = NULL;
    if (hierPtr->buttons != NULL) {
	/* Open or close button image? */
	image = hierPtr->buttons[0];
	if ((treePtr->entryPtr->flags & ENTRY_OPEN) &&
	    (hierPtr->buttons[1] != NULL)) {
	    image = hierPtr->buttons[1];
	}
    }
    /* Image or rectangle? */
    if (image != NULL) {
	Tk_RedrawImage(ImageData(image), 0, 0, width, height, drawable, x, y);
    } else {
	XSegment segArr[2];
	int count;

	if (treePtr == hierPtr->activeButtonPtr) {
	    gc = hierPtr->button.activeGC;
	} else {
	    gc = hierPtr->button.normalGC;
	}
	count = 1;
	segArr[0].y1 = segArr[0].y2 = y + height / 2;
	segArr[0].x1 = x + BUTTON_IPAD;
#ifdef WIN32
	segArr[0].x2 = x + width - BUTTON_IPAD;
#else
	segArr[0].x2 = x + width - BUTTON_IPAD - 1;
#endif
	if (!(treePtr->entryPtr->flags & ENTRY_OPEN)) {
	    segArr[1].x1 = segArr[1].x2 = x + width / 2;
	    segArr[1].y1 = y + BUTTON_IPAD;
#ifdef WIN32
	    segArr[1].y2 = y + height - BUTTON_IPAD;
#else
	    segArr[1].y2 = y + height - BUTTON_IPAD - 1;
#endif
	    count++;
	}
	XDrawSegments(hierPtr->display, drawable, gc, segArr, count);
    }
}


static void
DrawEntryIcon(hierPtr, treePtr, x, y, entryHeight, drawable)
    Hierbox *hierPtr;		/* Widget record containing the attribute
				 * information for buttons. */
    Tree *treePtr;		/* Node of entry. */
    int x, y;
    int entryHeight;
    Drawable drawable;		/* Pixmap or window to draw into. */
{
    Entry *entryPtr = treePtr->entryPtr;
    HierImage image;

    image = NULL;
    if (entryPtr->icons != NULL) {	/* Open or normal icon image? */
	image = entryPtr->icons[0];
	if ((treePtr == hierPtr->focusPtr) && (entryPtr->icons[1] != NULL)) {
	    image = entryPtr->icons[1];
	}
    }
    if (image != NULL) {	/* Image or default icon bitmap? */
	x += (LEVELWIDTH(treePtr->level + 1) - ImageWidth(image)) / 2;
	y += (entryHeight - ImageHeight(image)) / 2;
	Tk_RedrawImage(ImageData(image), 0, 0, ImageWidth(image),
	    ImageHeight(image), drawable, x, y);
    } else {
	x += (LEVELWIDTH(treePtr->level + 1) - DEF_ICON_WIDTH) / 2;
	y += (entryHeight - DEF_ICON_HEIGHT) / 2;
	XSetClipOrigin(hierPtr->display, entryPtr->iconGC, x, y);
	XCopyPlane(hierPtr->display, iconBitmap, drawable, entryPtr->iconGC,
	    0, 0, DEF_ICON_WIDTH, DEF_ICON_HEIGHT, x, y, 1);
    }
}

static void
DrawEntryData(hierPtr, treePtr, x, y, entryHeight, drawable)
    Hierbox *hierPtr;		/* Widget record containing the attribute
				 * information for buttons. */
    Tree *treePtr;		/* Node of entry. */
    int x, y;
    int entryHeight;
    Drawable drawable;		/* Pixmap or window to draw into. */
{
    Entry *entryPtr = treePtr->entryPtr;
    /*
     * Auxillary data: text string or images.
     */
    if (entryPtr->images != NULL) {
	register HierImage *imagePtr;
	int imageY;

	for (imagePtr = entryPtr->images; *imagePtr != NULL; imagePtr++) {
	    imageY = y;
	    if (ImageHeight(*imagePtr) < entryHeight) {
		imageY += (entryHeight - ImageHeight(*imagePtr)) / 2;
	    }
	    Tk_RedrawImage(ImageData(*imagePtr), 0, 0, ImageWidth(*imagePtr),
		ImageHeight(*imagePtr), drawable, x, imageY);
	    x += ImageWidth(*imagePtr);
	}
    } else if (entryPtr->text != NULL) {
	TextAttributes attr;
	Tk_Font font;
	XColor *colorPtr;
	int width, height;

	font = hierPtr->font;
	if (entryPtr->textFont != NULL) {
	    font = entryPtr->textFont;
	}
	colorPtr = hierPtr->textColor;
	if (entryPtr->textColor != NULL) {
	    colorPtr = entryPtr->textColor;
	}
	y += hierPtr->selBorderWidth + LABEL_PADY;

	Blt_SetTextDrawAttributes(&attr, font, entryPtr->textGC, colorPtr,
	    hierPtr->selFgColor, entryPtr->textShadow.color, 0.0,
	    TK_ANCHOR_NW, TK_JUSTIFY_LEFT, 0, entryPtr->textShadow.offset);
	Blt_GetTextExtents(&attr, entryPtr->text, &width, &height);
	if (height < entryHeight) {
	    y += (entryHeight - height) / 2;
	}
	Blt_DrawText(hierPtr->tkwin, drawable, entryPtr->text, &attr, x, y);
    }
}


static void
DrawEntryLabel(hierPtr, treePtr, drawable)
    Hierbox *hierPtr;		/* Widget record containing the attribute
				 * information for buttons. */
    Tree *treePtr;		/* Node of entry. */
    Drawable drawable;		/* Pixmap or window to draw into. */
{
    Entry *entryPtr = treePtr->entryPtr;
    Tk_FontMetrics fontMetrics;
    TextAttributes attr;
    int width, height;
    Tk_Font font;
    int selected, hasFocus;
    int entryHeight;
    int x, y;
    int maxX, maxY;

    x = SCREENX(hierPtr, entryPtr->worldX) + LEVELWIDTH(treePtr->level) +
	LEVELWIDTH(treePtr->level + 1) + 4;
    y = SCREENY(hierPtr, entryPtr->worldY);
    entryHeight = MAX(entryPtr->iconHeight, hierPtr->buttonHeight);
    font = hierPtr->font;
    if (entryPtr->labelFont != NULL) {
	font = entryPtr->labelFont;
    }
    hasFocus = ((treePtr == hierPtr->focusPtr) && (hierPtr->flags & HIER_FOCUS));
    selected = IsSelected(hierPtr, entryPtr);
    Tk_GetFontMetrics(font, &fontMetrics);
    width = height = fontMetrics.linespace;
    if (entryPtr->label != NULL) {
	XColor *normalColor, *activeColor;

	normalColor = hierPtr->textColor;
	if (entryPtr->labelColor != NULL) {
	    normalColor = entryPtr->labelColor;
	}
	if (hasFocus) {
	    activeColor = hierPtr->activeFgColor;
	} else {
	    activeColor = hierPtr->selFgColor;
	}
	Blt_SetTextDrawAttributes(&attr, font, entryPtr->labelGC, normalColor,
	    activeColor, entryPtr->labelShadow.color, 0.0, TK_ANCHOR_NW,
	    TK_JUSTIFY_LEFT, 0, entryPtr->labelShadow.offset);
	Blt_GetTextExtents(&attr, entryPtr->label, &width, &height);
    }
    width += 2 * (LABEL_PADX + hierPtr->selBorderWidth);
    height += 2 * (LABEL_PADY + hierPtr->selBorderWidth);
    maxX = Tk_Width(hierPtr->tkwin) - hierPtr->inset;
    maxY = Tk_Height(hierPtr->tkwin) - hierPtr->inset;
    if (treePtr == hierPtr->activePtr) {
	Tk_Fill3DRectangle(hierPtr->tkwin, drawable, hierPtr->activeBorder,
	    x, y, maxX - x, entryPtr->height - 1, hierPtr->selBorderWidth,
	    hierPtr->activeRelief);
    } else {
	Tk_Fill3DRectangle(hierPtr->tkwin, drawable, hierPtr->border,
	    x, y, maxX - x, entryPtr->height - 1, 0, TK_RELIEF_FLAT);
    }
    if (height < entryHeight) {
	y += (entryHeight - height) / 2;
    }
    if (hasFocus) {
#ifdef WIN32
	XRectangle region;
	/*
	 * Use our XDrawRectangles routine that handles dashed lines,
	 * instead of Tk's XDrawRectangle, that doesn't.
	 */
	region.x = x - 1, region.y = y - 1;
	region.width = width - hierPtr->selBorderWidth + 2;
	region.height = height - hierPtr->selBorderWidth + 1;
	XDrawRectangles(hierPtr->display, drawable, hierPtr->activeGC, &region, 1);
#else
	XDrawRectangle(hierPtr->display, drawable, hierPtr->activeGC,
	    x - 1, y - 1, width - hierPtr->selBorderWidth + 2,
	    height - hierPtr->selBorderWidth + 1);
#endif
    }
    if (selected) {
	Tk_Fill3DRectangle(hierPtr->tkwin, drawable, hierPtr->selBorder,
	    x, y, width, height - 1, hierPtr->selBorderWidth,
	    (selected) ? hierPtr->selRelief : hierPtr->activeRelief);
    }
    if (entryPtr->label != NULL) {
	attr.state = (selected) ? STATE_ACTIVE : 0;
	Blt_DrawText(hierPtr->tkwin, drawable, entryPtr->label, &attr,
	    x + LABEL_PADX + hierPtr->selBorderWidth,
	    y + hierPtr->selBorderWidth + LABEL_PADY - 1);
    }
    /*
     * Auxillary data: text string or images.
     */
    if (treePtr->parentPtr != NULL) {
	x += treePtr->parentPtr->entryPtr->levelX + LABEL_PADX;
    } else {
	x += width + LABEL_PADX;
    }
    if ((y < 0) || (x > maxX) || ((y + entryPtr->height) > maxY)) {
	hierPtr->flags |= HIER_BORDERS;
    }
    DrawEntryData(hierPtr, treePtr, x, y, entryHeight, drawable);
}

/*
 * ---------------------------------------------------------------------------
 *
 * DrawEntry --
 *
 * 	Draws a button for the given entry.  Note that buttons should only
 *	be drawn if the entry has sub-entries to be opened or closed.  It's
 *	the responsibility of the calling routine to ensure this.
 *
 *	The button is drawn centered in the region immediately to the left
 *	of the origin of the entry (computed in the layout routines). The
 *	height and width of the button were previously calculated from the
 *	average row height.
 *
 *		button height = entry height - (2 * some arbitrary padding).
 *		button width = button height.
 *
 *	The button has a border.  The symbol (either a plus or minus) is
 *	slight smaller than the width or height minus the border.
 *
 *	    x,y origin of entry
 *
 *              +---+
 *              | + | icon label
 *              +---+
 *             closed
 *
 *           |----|----| horizontal offset
 *
 *              +---+
 *              | - | icon label
 *              +---+
 *              open
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	A button is drawn for the entry.
 *
 * ---------------------------------------------------------------------------
 */
static void
DrawEntry(hierPtr, treePtr, drawable)
    Hierbox *hierPtr;		/* Widget record containing the attribute
				 * information for buttons. */
    Tree *treePtr;		/* Node of entry to be drawn. */
    Drawable drawable;		/* Pixmap or window to draw into. */
{
    int x, y;
    int width, height;
    int buttonX, buttonY;
    int x1, y1, x2, y2;
    Entry *entryPtr;

    entryPtr = treePtr->entryPtr;
    x = SCREENX(hierPtr, entryPtr->worldX);
    y = SCREENY(hierPtr, entryPtr->worldY);

    width = LEVELWIDTH(treePtr->level);
    height = MAX(entryPtr->iconHeight, hierPtr->buttonHeight);

    entryPtr->buttonX = (width - hierPtr->buttonWidth) / 2;
    entryPtr->buttonY = (height - hierPtr->buttonHeight) / 2;

    buttonX = x + entryPtr->buttonX;
    buttonY = y + entryPtr->buttonY;

    x1 = x + (width / 2);
    y1 = y2 = buttonY + (hierPtr->buttonHeight / 2);
    x2 = x1 + (LEVELWIDTH(treePtr->level) + LEVELWIDTH(treePtr->level + 1)) / 2;

    if ((treePtr->parentPtr != NULL) && (hierPtr->lineWidth > 0)) {
	/*
	 * For every node except root, draw a horizontal line from
	 * the vertical bar to the middle of the icon.
	 */
	XDrawLine(hierPtr->display, drawable, hierPtr->lineGC, x1, y1, x2, y2);
    }
    if ((entryPtr->flags & ENTRY_OPEN) && (hierPtr->lineWidth > 0)) {
	/*
	 * Entry is open, draw vertical line.
	 */
	y2 = y1 + entryPtr->lineHeight;
	if (y2 > Tk_Height(hierPtr->tkwin)) {
	    y2 = Tk_Height(hierPtr->tkwin);	/* Clip line at window border. */
	}
	XDrawLine(hierPtr->display, drawable, hierPtr->lineGC, x2, y1, x2, y2);
    }
    if ((entryPtr->flags & ENTRY_BUTTON) && (treePtr->parentPtr != NULL)) {
	/*
	 * Except for root, draw a button for every entry that needs
	 * one.  The displayed button can be either a Tk image or a
	 * rectangle with plus or minus sign.
	 */
	DrawEntryButton(hierPtr, treePtr, drawable);
    }
    x += LEVELWIDTH(treePtr->level);
    DrawEntryIcon(hierPtr, treePtr, x, y, height, drawable);

    /*
     * Entry label.
     */
    x += LEVELWIDTH(treePtr->level + 1) + 4;
    DrawEntryLabel(hierPtr, treePtr, drawable);

}

static void
DrawOuterBorders(hierPtr, drawable)
    Hierbox *hierPtr;
    Drawable drawable;
{
    /* Draw 3D border just inside of the focus highlight ring. */
    if ((hierPtr->borderWidth > 0) && (hierPtr->relief != TK_RELIEF_FLAT)) {
	Tk_Draw3DRectangle(hierPtr->tkwin, drawable, hierPtr->border,
	    hierPtr->highlightWidth, hierPtr->highlightWidth,
	    Tk_Width(hierPtr->tkwin) - 2 * hierPtr->highlightWidth,
	    Tk_Height(hierPtr->tkwin) - 2 * hierPtr->highlightWidth,
	    hierPtr->borderWidth, hierPtr->relief);
    }
    /* Draw focus highlight ring. */
    if (hierPtr->highlightWidth > 0) {
	XColor *color;
	GC gc;

	color = (hierPtr->flags & HIER_FOCUS)
	    ? hierPtr->highlightColor : hierPtr->highlightBgColor;
	gc = Tk_GCForColor(color, drawable);
	Tk_DrawFocusHighlight(hierPtr->tkwin, gc, hierPtr->highlightWidth,
	    drawable);
    }
    hierPtr->flags &= ~HIER_BORDERS;
}

/*
 * ----------------------------------------------------------------------
 *
 * DisplayHierbox --
 *
 * 	This procedure is invoked to display the widget.
 *
 *      Recompute the layout of the text if necessary. This is
 *	necessary if the world coordinate system has changed.
 *	Specifically, the following may have occurred:
 *
 *	  1.  a text attribute has changed (font, linespacing, etc.).
 *	  2.  an entry's option changed, possibly resizing the entry.
 *
 *      This is deferred to the display routine since potentially
 *      many of these may occur.
 *
 *	Set the vertical and horizontal scrollbars.  This is done
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
DisplayHierbox(clientData)
    ClientData clientData;	/* Information about widget. */
{
    Hierbox *hierPtr = (Hierbox *) clientData;
    Pixmap drawable;

    hierPtr->flags &= ~HIER_REDRAW;
    if (hierPtr->tkwin == NULL) {
	return;			/* Window has been destroyed. */
    }
    if (hierPtr->flags & HIER_LAYOUT) {
	/*
	 * Recompute the layout when entries are opened/closed,
	 * inserted/deleted, or when text attributes change
	 * (such as font, linespacing).
	 */
	ComputeLayout(hierPtr);
	hierPtr->flags &= ~HIER_LAYOUT;
    }
    if (hierPtr->flags & HIER_SCROLL) {
	int width, height;

	/* Scrolling means that the view port has changed and that the
	 * visible entries need to be recomputed.  */
	ComputeVisibleEntries(hierPtr);

	width = VPORTWIDTH(hierPtr);
	height = VPORTHEIGHT(hierPtr);
	if (hierPtr->flags & HIER_XSCROLL) {
	    if (hierPtr->xScrollCmdPrefix != NULL) {
		Blt_UpdateScrollbar(hierPtr->interp, hierPtr->xScrollCmdPrefix,
		    (float)hierPtr->xOffset / hierPtr->worldWidth,
		    (float)(hierPtr->xOffset + width) / hierPtr->worldWidth);
	    }
	}
	if (hierPtr->flags & HIER_YSCROLL) {
	    if (hierPtr->yScrollCmdPrefix != NULL) {
		Blt_UpdateScrollbar(hierPtr->interp, hierPtr->yScrollCmdPrefix,
		    (float)hierPtr->yOffset / hierPtr->worldHeight,
		    (float)(hierPtr->yOffset + height) / hierPtr->worldHeight);
	    }
	}
	hierPtr->flags &= ~HIER_SCROLL;
    }
    if (!Tk_IsMapped(hierPtr->tkwin)) {
	return;
    }
    drawable = Tk_GetPixmap(hierPtr->display, Tk_WindowId(hierPtr->tkwin),
	Tk_Width(hierPtr->tkwin), Tk_Height(hierPtr->tkwin),
	Tk_Depth(hierPtr->tkwin));

    /*
     * Clear the background either by tiling a pixmap or filling with
     * a solid color. Tiling takes precedence.
     */
    if (hierPtr->bgTile != NULL) {
	if (hierPtr->scrollTile) {
	    XSetTSOrigin(hierPtr->display, hierPtr->tileGC, -hierPtr->xOffset,
		-hierPtr->yOffset);
	} else {
	    Blt_SetTileOrigin(hierPtr->tkwin, hierPtr->tileGC, 0, 0);
	}
	XFillRectangle(hierPtr->display, drawable, hierPtr->tileGC, 0, 0,
	    Tk_Width(hierPtr->tkwin), Tk_Height(hierPtr->tkwin));
    } else {
	Tk_Fill3DRectangle(hierPtr->tkwin, drawable, hierPtr->border, 0, 0,
	    Tk_Width(hierPtr->tkwin), Tk_Height(hierPtr->tkwin), 0,
	    TK_RELIEF_FLAT);
    }

    if (hierPtr->numVisible > 0) {
	register Tree **treePtrPtr;

	if (hierPtr->lineWidth > 0) {
	    DrawVerticals(hierPtr, hierPtr->visibleArr[0], drawable);
	}
	for (treePtrPtr = hierPtr->visibleArr; *treePtrPtr != NULL;
	    treePtrPtr++) {
	    DrawEntry(hierPtr, *treePtrPtr, drawable);
	}
    }
    DrawOuterBorders(hierPtr, drawable);
    /* Now copy the new view to the window. */
    XCopyArea(hierPtr->display, drawable, Tk_WindowId(hierPtr->tkwin),
	hierPtr->lineGC, 0, 0, Tk_Width(hierPtr->tkwin),
	Tk_Height(hierPtr->tkwin), 0, 0);
    Tk_FreePixmap(hierPtr->display, drawable);
}

/*
 *----------------------------------------------------------------------
 *
 * SelectCmdProc --
 *
 *      Invoked at the next idle point whenever the current
 *      selection changes.  Executes some application-specific code
 *      in the -selectcommand option.  This provides a way for
 *      applications to handle selection changes.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      Tcl code gets executed for some application-specific task.
 *
 *----------------------------------------------------------------------
 */
static void
SelectCmdProc(clientData)
    ClientData clientData;	/* Information about widget. */
{
    Hierbox *hierPtr = (Hierbox *) clientData;

    if (hierPtr->selectCmd != NULL) {
	hierPtr->flags &= ~SELECT_PENDING;
	if (Tcl_GlobalEval(hierPtr->interp, hierPtr->selectCmd) != TCL_OK) {
	    Tk_BackgroundError(hierPtr->interp);
	}
    }
}

/*
 * --------------------------------------------------------------
 *
 * HierboxCmd --
 *
 * 	This procedure is invoked to process the Tcl command that
 * 	corresponds to a widget managed by this module. See the user
 * 	documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 * --------------------------------------------------------------
 */
/* ARGSUSED */
static int
HierboxCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window associated with interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    Hierbox *hierPtr;
    Tk_Window tkwin;
    Tree *treePtr;
    static int initialized = 0;

    if (!initialized) {
	static char initCmd[] =
	{"\n\
set file [file join $blt_library bltHier.tcl]\n\
if { [file readable $file] } {\n\
    source $file\n\
}\n"
	};
	if (Tcl_GlobalEval(interp, initCmd) != TCL_OK) {
	    Tcl_AddErrorInfo(interp,
		"\n    (while loading bindings for blt::hierbox)");
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
    hierPtr = CreateHierbox(interp, tkwin);

    if (Blt_ConfigureWidgetComponent(interp, tkwin, "button", "Button",
	    buttonConfigSpecs, 0, (char **)NULL, (char *)hierPtr, 0) != TCL_OK) {
	Tk_DestroyWindow(tkwin);
	return TCL_ERROR;
    }
    if (ConfigureHierbox(interp, hierPtr, argc - 2, argv + 2, 0) != TCL_OK) {
	Tk_DestroyWindow(tkwin);
	return TCL_ERROR;
    }
    treePtr = CreateNode(hierPtr, (Tree *) NULL, POSITION_END,
	hierPtr->separator);
    if (treePtr == NULL) {
	return TCL_ERROR;
    }
    treePtr->entryPtr->flags = (ENTRY_OPEN | ENTRY_MAPPED);
    hierPtr->rootPtr = hierPtr->focusPtr = hierPtr->selAnchorPtr = treePtr;
    Blt_SetFocusItem(hierPtr->bindTable, hierPtr->focusPtr);

    Tk_CreateSelHandler(tkwin, XA_PRIMARY, XA_STRING, SelectionProc,
	(ClientData)hierPtr, XA_STRING);
    Tk_CreateEventHandler(tkwin, ExposureMask | StructureNotifyMask |
	FocusChangeMask, HierboxEventProc, (ClientData)hierPtr);

    hierPtr->cmdToken = Tcl_CreateCommand(interp, argv[1], HierboxInstCmd,
	(ClientData)hierPtr, HierboxInstDeletedCmd);
#ifdef ITCL_NAMESPACES
    Itk_SetWidgetCommand(hierPtr->tkwin, hierPtr->cmdToken);
#endif
    /*
     * Try to invoke a procedure to initialize various bindings on tabs.
     * This can fail if the initialization file wasn't sourced above.
     * We silently ignore any errors.
     */
    Tcl_VarEval(interp, "blt::HierboxInitBindings ", argv[1], (char *)NULL);
    Tcl_SetResult(interp, Tk_PathName(hierPtr->tkwin), TCL_STATIC);
    return TCL_OK;
}

/*
 * --------------------------------------------------------------
 *
 * Hierbox operations
 *
 * --------------------------------------------------------------
 */

/*ARGSUSED*/
static int
FocusOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    if (argc == 3) {
	Tree *treePtr;

	treePtr = hierPtr->focusPtr;
	if (GetNode(hierPtr, argv[2], &treePtr) != TCL_OK) {
	    return TCL_ERROR;
	}
	if ((treePtr != NULL) && (treePtr != hierPtr->focusPtr)) {
	    if (IsHidden(treePtr)) {
		/* Doesn't make sense to set focus to a node you can't see. */
		ExposeAncestors(treePtr);
	    }
	    hierPtr->flags |= (HIER_LAYOUT | HIER_SCROLL);
	    hierPtr->focusPtr = treePtr;
	}
	EventuallyRedraw(hierPtr);
    }
    Blt_SetFocusItem(hierPtr->bindTable, hierPtr->focusPtr);
    if (hierPtr->focusPtr != NULL) {
	Tcl_SetResult(interp, NodeToString(hierPtr, hierPtr->focusPtr),
	    TCL_VOLATILE);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * BboxOp --
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
BboxOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    Tree *treePtr;
    register int i;
    Entry *entryPtr;
    char string[200];
    int width, height, bottom;
    int xMin, yMin, xMax, yMax;
    int screen;

    if (hierPtr->flags & HIER_LAYOUT) {
	/*
	 * The layout is dirty.  Recompute it now, before we use the
	 * world dimensions.  But remember, the "bbox" operation isn't
	 * valid for hidden entries (since they're not visible, they
	 * don't have world coordinates).
	 */
	ComputeLayout(hierPtr);
	hierPtr->flags &= ~HIER_LAYOUT;
    }
    xMin = hierPtr->worldWidth;
    yMin = hierPtr->worldHeight;
    xMax = yMax = 0;

    screen = FALSE;
    if ((argv[2][0] == '-') && (strcmp(argv[2], "-screen") == 0)) {
	screen = TRUE;
	argc--, argv++;
    }
    for (i = 2; i < argc; i++) {
	if ((argv[i][0] == 'a') && (strcmp(argv[i], "all") == 0)) {
	    xMin = yMin = 0;
	    xMax = hierPtr->worldWidth;
	    yMax = hierPtr->worldHeight;
	    break;
	}
	treePtr = hierPtr->focusPtr;
	if (GetNode(hierPtr, argv[i], &treePtr) != TCL_OK) {
	    return TCL_ERROR;
	}
	if ((treePtr == NULL) || (IsHidden(treePtr))) {
	    continue;
	}
	entryPtr = treePtr->entryPtr;
	bottom = entryPtr->worldY + entryPtr->height;
	height = VPORTHEIGHT(hierPtr);
	if ((bottom <= hierPtr->yOffset) &&
	    (entryPtr->worldY >= (hierPtr->yOffset + height))) {
	    continue;
	}
	if (yMax < bottom) {
	    yMax = bottom;
	}
	if (yMin > entryPtr->worldY) {
	    yMin = entryPtr->worldY;
	}
	if (xMax <
	    (entryPtr->worldX + entryPtr->width + LEVELWIDTH(treePtr->level))) {
	    xMax = (entryPtr->worldX + entryPtr->width +
		LEVELWIDTH(treePtr->level));
	}
	if (xMin > entryPtr->worldX) {
	    xMin = entryPtr->worldX;
	}
    }

    if (screen) {
	width = VPORTWIDTH(hierPtr);
	height = VPORTHEIGHT(hierPtr);

	/*
	 * Do a min-max text for the intersection of the viewport and
	 * the computed bounding box.  If there is no intersection, return
	 * the empty string.
	 */
	if ((xMax < hierPtr->xOffset) || (yMax < hierPtr->yOffset) ||
	    (xMin >= (hierPtr->xOffset + width)) ||
	    (yMin >= (hierPtr->yOffset + height))) {
	    return TCL_OK;
	}
	/* Otherwise clip the coordinates at the view port boundaries. */
	if (xMin < hierPtr->xOffset) {
	    xMin = hierPtr->xOffset;
	} else if (xMax > (hierPtr->xOffset + width)) {
	    xMax = hierPtr->xOffset + width;
	}
	if (yMin < hierPtr->yOffset) {
	    yMin = hierPtr->yOffset;
	} else if (yMax > (hierPtr->yOffset + height)) {
	    yMax = hierPtr->yOffset + height;
	}
	xMin = SCREENX(hierPtr, xMin), yMin = SCREENY(hierPtr, yMin);
	xMax = SCREENX(hierPtr, xMax), yMax = SCREENY(hierPtr, yMax);
    }
    if ((xMin < xMax) && (yMin < yMax)) {
	sprintf(string, "%d %d %d %d", xMin, yMin, xMax - xMin, yMax - yMin);
	Tcl_SetResult(interp, string, TCL_VOLATILE);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ButtonActivateOp --
 *
 *	Selects the button to appear active.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ButtonActivateOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;			/* Not used. */
    char **argv;
{
    Tree *treePtr, *oldPtr;

    treePtr = hierPtr->focusPtr;
    if (argv[3][0] == '\0') {
	treePtr = NULL;
    } else if (GetNode(hierPtr, argv[3], &treePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    oldPtr = hierPtr->activeButtonPtr;
    hierPtr->activeButtonPtr = treePtr;
    if (treePtr != oldPtr) {
	if (hierPtr->flags & (HIER_REDRAW | HIER_DIRTY)) {
	    EventuallyRedraw(hierPtr);
	} else {
	    Drawable drawable;

	    drawable = Tk_WindowId(hierPtr->tkwin);
	    if (oldPtr != NULL) {
		DrawEntryButton(hierPtr, oldPtr, drawable);
	    }
	    if (treePtr != NULL) {
		DrawEntryButton(hierPtr, treePtr, drawable);
	    }
	    DrawOuterBorders(hierPtr, drawable);
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ButtonBindOp --
 *
 *	  .t bind tag sequence command
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ButtonBindOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    ClientData object;

    /*
     * Individual entries are selected by index only.  All other
     * strings are interpreted as a binding tag.  For example, if one
     * binds to "focus", it is assumed that this refers to a bind tag,
     * not the entry with focus.
     */
    object = (ClientData)GetNodeByIndex(interp, hierPtr, argv[3]);
    if (object == 0) {
	object = (ClientData)Tk_GetUid(argv[3]);
    }
    return Blt_ConfigureBindings(interp, hierPtr->buttonBindTable, object,
	argc - 4, argv + 4);
}

/*
 *----------------------------------------------------------------------
 *
 * ButCgetOpOp --
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ButtonCgetOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    return Tk_ConfigureValue(interp, hierPtr->tkwin, buttonConfigSpecs,
	(char *)hierPtr, argv[3], 0);
}

/*
 *----------------------------------------------------------------------
 *
 * ButtonConfigureOp --
 *
 * 	This procedure is called to process a list of configuration
 *	options database, in order to reconfigure the one of more
 *	entries in the widget.
 *
 *	  .h entryconfigure node node node node option value
 *
 * Results:
 *	A standard Tcl result.  If TCL_ERROR is returned, then
 *	interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information, such as text string, colors, font,
 *	etc. get set for hierPtr; old resources get freed, if there
 *	were any.  The hypertext is redisplayed.
 *
 *----------------------------------------------------------------------
 */
static int
ButtonConfigureOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    /* Figure out where the option value pairs begin */
    argc -= 3;
    argv += 3;

    if (argc == 0) {
	return Tk_ConfigureInfo(interp, hierPtr->tkwin, buttonConfigSpecs,
	    (char *)hierPtr, (char *)NULL, 0);
    } else if (argc == 1) {
	return Tk_ConfigureInfo(interp, hierPtr->tkwin, buttonConfigSpecs,
	    (char *)hierPtr, argv[0], 0);
    }
    if (Tk_ConfigureWidget(hierPtr->interp, hierPtr->tkwin, buttonConfigSpecs,
	    argc, argv, (char *)hierPtr, TK_CONFIG_ARGV_ONLY) != TCL_OK) {
	return TCL_ERROR;
    }
    ConfigureButtons(hierPtr);
    EventuallyRedraw(hierPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ButtonOp --
 *
 *	This procedure handles button operations.
 *
 * Results:
 *	A standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
static Blt_OpSpec buttonOperSpecs[] =
{
    {"activate", 1, (Blt_Operation)ButtonActivateOp, 4, 4, "node",},
    {"bind", 1, (Blt_Operation)ButtonBindOp, 4, 6,
	"tagName ?sequence command?",},
    {"cget", 2, (Blt_Operation)ButtonCgetOp, 4, 4, "option",},
    {"configure", 2, (Blt_Operation)ButtonConfigureOp, 3, 0,
	"?option value?...",},
};

static int numButtonSpecs = sizeof(buttonOperSpecs) / sizeof(Blt_OpSpec);

static int
ButtonOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Blt_Operation proc;
    int result;

    proc = Blt_GetOperation(interp, numButtonSpecs, buttonOperSpecs,
	BLT_OPER_ARG2, argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    result = (*proc) (hierPtr, interp, argc, argv);
    return result;
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
CgetOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    return Tk_ConfigureValue(interp, hierPtr->tkwin, configSpecs,
	(char *)hierPtr, argv[2], 0);
}

/*ARGSUSED*/
static int
CloseOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;		/* Not used. */
    int argc;
    char **argv;
{
    Tree *rootPtr;
    unsigned int flags;
    int length;
    register int i;

    flags = 0;
    length = strlen(argv[2]);
    if ((argv[2][0] == '-') && (length > 1) &&
	(strncmp(argv[2], "-recurse", length) == 0)) {
	argv++, argc--;
	flags |= APPLY_RECURSE;
    }
    for (i = 2; i < argc; i++) {
	rootPtr = hierPtr->focusPtr;
	if (GetNode(hierPtr, argv[i], &rootPtr) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (rootPtr == NULL) {
	    continue;
	}
	/*
	 * Clear any selected entries that may become hidden by
	 * closing the node.
	 */
	ClearSubnodes(hierPtr, rootPtr);

	/*
	 * -----------------------------------------------------------
	 *
	 *  Check if either the "focus" entry or selection anchor
	 *  is in this hierarchy.  Must move it or disable it before
	 *  we close the node.  Otherwise it may be deleted by a Tcl
	 *  "close" script, and we'll be left pointing to a bogus
	 *  memory location.
	 *
	 * -----------------------------------------------------------
	 */
	if (IsAncestor(rootPtr, hierPtr->focusPtr)) {
	    hierPtr->focusPtr = rootPtr;
	    Blt_SetFocusItem(hierPtr->bindTable, hierPtr->focusPtr);
	}
	if (IsAncestor(rootPtr, hierPtr->selAnchorPtr)) {
	    hierPtr->selMarkPtr = hierPtr->selAnchorPtr = NULL;
	}
	if (IsAncestor(rootPtr, hierPtr->activePtr)) {
	    hierPtr->activePtr = rootPtr;
	}
	if (ApplyToTree(hierPtr, rootPtr, CloseNode, flags) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    hierPtr->flags |= (HIER_LAYOUT | HIER_SCROLL);
    EventuallyRedraw(hierPtr);
    return TCL_OK;
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
 * Side effects:
 *	Configuration information, such as text string, colors, font,
 *	etc. get set for hierPtr; old resources get freed, if there
 *	were any.  The widget is redisplayed.
 *
 *----------------------------------------------------------------------
 */
static int
ConfigureOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    if (argc == 2) {
	return Tk_ConfigureInfo(interp, hierPtr->tkwin, configSpecs,
	    (char *)hierPtr, (char *)NULL, 0);
    } else if (argc == 3) {
	return Tk_ConfigureInfo(interp, hierPtr->tkwin, configSpecs,
	    (char *)hierPtr, argv[2], 0);
    }
    if (ConfigureHierbox(interp, hierPtr, argc - 2, argv + 2,
	    TK_CONFIG_ARGV_ONLY) != TCL_OK) {
	return TCL_ERROR;
    }
    EventuallyRedraw(hierPtr);
    return TCL_OK;
}

/*ARGSUSED*/
static int
CurselectionOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;		/* Not used. */
    int argc;			/* Not used. */
    char **argv;		/* Not used. */
{
    ApplyToTree(hierPtr, hierPtr->rootPtr, IsSelectedNode,
	APPLY_RECURSE | APPLY_OPEN_ONLY | APPLY_BEFORE);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ActivateOpOp --
 *
 *	Selects the tab to appear active.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ActivateOpOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;			/* Not used. */
    char **argv;
{
    Tree *treePtr, *oldPtr;

    treePtr = hierPtr->focusPtr;
    if (argv[3][0] == '\0') {
	treePtr = NULL;
    } else if (GetNode(hierPtr, argv[3], &treePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    oldPtr = hierPtr->activePtr;
    hierPtr->activePtr = treePtr;
    if (treePtr != oldPtr) {
	if (hierPtr->flags & (HIER_REDRAW | HIER_DIRTY)) {
	    EventuallyRedraw(hierPtr);
	} else {
	    Drawable drawable;

	    drawable = Tk_WindowId(hierPtr->tkwin);
	    if (oldPtr != NULL) {
		DrawEntryLabel(hierPtr, oldPtr, drawable);
	    }
	    if (treePtr != NULL) {
		DrawEntryLabel(hierPtr, treePtr, drawable);
	    }
	    if (hierPtr->flags & HIER_BORDERS) {
		DrawOuterBorders(hierPtr, drawable);
	    }
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
BindOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    ClientData item;

    /*
     * Individual entries are selected by index only.  All other strings
     * are interpreted as a binding tag.
     */
    item = (ClientData)GetNodeByIndex(interp, hierPtr, argv[2]);
    if (item == 0) {
	item = (ClientData)Tk_GetUid(argv[2]);
    }
    return Blt_ConfigureBindings(interp, hierPtr->bindTable, item, argc - 3,
	argv + 3);
}

/*
 *----------------------------------------------------------------------
 *
 * CgetOpOp --
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
CgetOpOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    Tree *treePtr;

    if (StringToNode(hierPtr, argv[3], &treePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    return Tk_ConfigureValue(interp, hierPtr->tkwin, entryConfigSpecs,
	(char *)treePtr->entryPtr, argv[4], 0);
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureOpOp --
 *
 * 	This procedure is called to process a list of configuration
 *	options database, in order to reconfigure the one of more
 *	entries in the widget.
 *
 *	  .h entryconfigure node node node node option value
 *
 * Results:
 *	A standard Tcl result.  If TCL_ERROR is returned, then
 *	interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information, such as text string, colors, font,
 *	etc. get set for hierPtr; old resources get freed, if there
 *	were any.  The hypertext is redisplayed.
 *
 *----------------------------------------------------------------------
 */
static int
ConfigureOpOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int numIds, numOpts;
    char **options;
    register int i;
    Tree *treePtr;


    /* Figure out where the option value pairs begin */
    argc -= 3;
    argv += 3;
    for (i = 0; i < argc; i++) {
	if (argv[i][0] == '-') {
	    break;
	}
	if (StringToNode(hierPtr, argv[i], &treePtr) != TCL_OK) {
	    return TCL_ERROR;	/* Can't find node. */
	}
    }
    numIds = i;			/* Number of element names specified */
    numOpts = argc - i;		/* Number of options specified */
    options = argv + i;		/* Start of options in argv  */

    for (i = 0; i < numIds; i++) {
	StringToNode(hierPtr, argv[i], &treePtr);
	if (argc == 1) {
	    return Tk_ConfigureInfo(interp, hierPtr->tkwin, entryConfigSpecs,
		(char *)treePtr->entryPtr, (char *)NULL, 0);
	} else if (argc == 2) {
	    return Tk_ConfigureInfo(interp, hierPtr->tkwin, entryConfigSpecs,
		(char *)treePtr->entryPtr, argv[2], 0);
	}
	if (ConfigureEntry(hierPtr, treePtr->entryPtr, numOpts, options,
		TK_CONFIG_ARGV_ONLY) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    hierPtr->flags |= (HIER_LAYOUT | HIER_SCROLL);
    EventuallyRedraw(hierPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * HiddenOpOp --
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
HiddenOpOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    Tree *treePtr;

    if (StringToNode(hierPtr, argv[3], &treePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    Tcl_SetResult(interp, IsHidden(treePtr) ? "1" : "0", TCL_STATIC);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * OpenOpOp --
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
OpenOpOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    Tree *treePtr;

    if (StringToNode(hierPtr, argv[3], &treePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    Tcl_SetResult(interp,
	(treePtr->entryPtr->flags & ENTRY_OPEN) ? "1" : "0", TCL_STATIC);
    return TCL_OK;
}

/*ARGSUSED*/
static int
ChildrenOpOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Tree *parentPtr, *nodePtr;

    if (StringToNode(hierPtr, argv[3], &parentPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (argc == 4) {
	register Blt_ListItem item;

	for (item = Blt_ListFirstItem(parentPtr->nodeList); item != NULL;
	    item = Blt_ListNextItem(item)) {
	    nodePtr = (Tree *) Blt_ListGetValue(item);
	    Tcl_AppendElement(interp, NodeToString(hierPtr, nodePtr));
	}
    } else if (argc == 6) {
	register Blt_ListItem firstItem, lastItem;
	int first, last;
	int numNodes;

	if ((GetPosition(interp, argv[4], &first) != TCL_OK) ||
	    (GetPosition(interp, argv[5], &last) != TCL_OK)) {
	    return TCL_ERROR;
	}
	numNodes = Blt_ListGetLength(parentPtr->nodeList);
	if (numNodes == 0) {
	    return TCL_OK;
	}
	if (last >= numNodes) {
	    last = numNodes - 1;
	}
	if (first >= numNodes) {
	    first = numNodes - 1;
	}
	firstItem = Blt_ListFindNthItem(parentPtr->nodeList, first, 1);
	lastItem = Blt_ListFindNthItem(parentPtr->nodeList, last, 1);
	if (first > last) {
	    for ( /*empty*/ ; lastItem != NULL;
		lastItem = Blt_ListPrevItem(lastItem)) {
		nodePtr = (Tree *) Blt_ListGetValue(lastItem);
		Tcl_AppendElement(interp, NodeToString(hierPtr, nodePtr));
		if (lastItem == firstItem) {
		    break;
		}
	    }
	} else {
	    for ( /*empty*/ ; firstItem != NULL;
		firstItem = Blt_ListNextItem(firstItem)) {
		nodePtr = (Tree *) Blt_ListGetValue(firstItem);
		Tcl_AppendElement(interp, NodeToString(hierPtr, nodePtr));
		if (firstItem == lastItem) {
		    break;
		}
	    }
	}
    } else {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0], " ",
	    argv[1], " ", argv[2], " index ?first last?", (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * SizeOpOp --
 *
 *	Counts the number of entries at this node.
 *
 * Results:
 *	A standard Tcl result.  If an error occurred TCL_ERROR is
 *	returned and interp->result will contain an error message.
 *	Otherwise, TCL_OK is returned and interp->result contains
 *	the number of entries.
 *
 *----------------------------------------------------------------------
 */
static int
SizeOpOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int length;
    Tree *rootPtr;
    int *sumPtr = (int *)&(hierPtr->clientData);
    unsigned int flags;

    flags = 0;
    length = strlen(argv[3]);
    if ((argv[3][0] == '-') && (length > 1) &&
	(strncmp(argv[3], "-recurse", length) == 0)) {
	argv++, argc--;
	flags |= APPLY_RECURSE;
    }
    if (argc == 3) {
	Tcl_AppendResult(interp, "missing node argument: should be \"",
	    argv[0], " entry open node\"", (char *)NULL);
	return TCL_ERROR;
    }
    if (StringToNode(hierPtr, argv[3], &rootPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    *sumPtr = 0;
    if (ApplyToTree(hierPtr, rootPtr, SizeOfNode, 0) != TCL_OK) {
	return TCL_ERROR;
    }
    Tcl_SetResult(interp, Blt_Int(*sumPtr), TCL_VOLATILE);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * EntryOp --
 *
 *	This procedure handles entry operations.
 *
 * Results:
 *	A standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
static Blt_OpSpec entryOperSpecs[] =
{
    {"activate", 1, (Blt_Operation)ActivateOpOp, 4, 4, "node",},
    {"cget", 2, (Blt_Operation)CgetOpOp, 5, 5, "node option",},
    {"children", 2, (Blt_Operation)ChildrenOpOp, 6, 6, "node first last",},
    {"configure", 2, (Blt_Operation)ConfigureOpOp, 4, 0,
	"node ?node...? ?option value?...",},
    {"hidden", 1, (Blt_Operation)HiddenOpOp, 4, 4, "node",},
    {"open", 1, (Blt_Operation)OpenOpOp, 4, 4, "node",},
    {"size", 1, (Blt_Operation)SizeOpOp, 4, 5, "?-recurse? node",},
};
static int numEntrySpecs = sizeof(entryOperSpecs) / sizeof(Blt_OpSpec);

static int
EntryOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Blt_Operation proc;
    int result;

    proc = Blt_GetOperation(interp, numEntrySpecs, entryOperSpecs,
	BLT_OPER_ARG2, argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    result = (*proc) (hierPtr, interp, argc, argv);
    return result;
}

/*ARGSUSED*/
static int
ExactCompare(interp, name, pattern)
    Tcl_Interp *interp;		/* Not used. */
    char *name;
    char *pattern;
{
    return (strcmp(name, pattern) == 0);
}

/*ARGSUSED*/
static int
GlobCompare(interp, name, pattern)
    Tcl_Interp *interp;		/* Not used. */
    char *name;
    char *pattern;
{
    return Tcl_StringMatch(name, pattern);
}

static int
RegexpCompare(interp, name, pattern)
    Tcl_Interp *interp;
    char *name;
    char *pattern;
{
    return Tcl_RegExpMatch(interp, name, pattern);
}

#ifdef notdef
static Blt_FlagSpec findSpecs[] =
{
    {"-count", 2, Tk_Offset(FindInfo, maxMatches), (char *)NULL,},
    {"-exact", 3, Tk_Offset(FindInfo, compareProc), (char *)ExactCompare,},
    {"-exec", 3, Tk_Offset(FindInfo, maxMatches), (char *)NULL,},
    {"-full", 2, Tk_Offset(FindInfo, fullPattern), (char *)NULL,},
    {"-glob", 2, Tk_Offset(FindInfo, compareProc), (char *)GlobCompare,},
    {"-name", 3, Tk_Offset(FindInfo, namePattern), (char *)NULL,},
    {"-nonmatching", 3, Tk_Offset(FindInfo, invertMatch), (char *)TRUE,},
    {"-regexp", 2, Tk_Offset(FindInfo, compareProc), (char *)RegexpCompare,},
};
#endif

/*
 *----------------------------------------------------------------------
 *
 * FindOp --
 *
 *	Find one or more nodes based upon the pattern provided.
 *
 * Results:
 *	A standard Tcl result.  The interpreter result will contain a
 *	list of the node serial identifiers.
 *
 *----------------------------------------------------------------------
 */
static int
FindOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    register Tree *treePtr;
    Tree *firstPtr, *lastPtr;
    int numMatches, maxMatches;
    char c;
    int length;
    CompareProc *compareProc;
    IterProc *nextProc;
    int invertMatch;		/* normal search mode (matching entries) */
    char *namePattern, *fullPattern;
    char *execCmd;
    register int i;
    int result;
    char *pattern, *option, *value;
    Tcl_DString dString, pathString;
    Blt_List configOptList;
    register Blt_ListItem item;


    invertMatch = FALSE;
    maxMatches = 0;
    execCmd = namePattern = fullPattern = NULL;
    compareProc = ExactCompare;
    nextProc = NextNode;
    Blt_InitList(&configOptList, TCL_STRING_KEYS);

    /*
     * Step 1:  Process flags for find operation.
     */
    for (i = 2; i < argc; i++) {
	if (argv[i][0] != '-') {
	    break;
	}
	option = argv[i] + 1;
	length = strlen(option);
	c = option[0];
	if ((c == 'e') && (length > 2) &&
	    (strncmp(option, "exact", length) == 0)) {
	    compareProc = ExactCompare;
	} else if ((c == 'g') && (strncmp(option, "glob", length) == 0)) {
	    compareProc = GlobCompare;
	} else if ((c == 'r') && (strncmp(option, "regexp", length) == 0)) {
	    compareProc = RegexpCompare;
	} else if ((c == 'n') && (length > 1) &&
	    (strncmp(option, "nonmatching", length) == 0)) {
	    invertMatch = TRUE;
	} else if ((c == 'n') && (length > 1) &&
	    (strncmp(option, "name", length) == 0)) {
	    if ((i + 1) == argc) {
		goto missingArg;
	    }
	    i++;
	    namePattern = argv[i];
	} else if ((c == 'f') && (strncmp(option, "full", length) == 0)) {
	    if ((i + 1) == argc) {
		goto missingArg;
	    }
	    i++;
	    fullPattern = argv[i];
	} else if ((c == 'e') && (length > 2) &&
	    (strncmp(option, "exec", length) == 0)) {
	    if ((i + 1) == argc) {
		goto missingArg;
	    }
	    i++;
	    execCmd = argv[i];
	} else if ((c == 'c') && (strncmp(option, "count", length) == 0)) {
	    if ((i + 1) == argc) {
		goto missingArg;
	    }
	    i++;
	    if (Tcl_GetInt(interp, argv[i], &maxMatches) != TCL_OK) {
		return TCL_ERROR;
	    }
	    if (maxMatches < 0) {
		Tcl_AppendResult(interp, "bad match count \"", argv[i],
		    "\": should be a positive number", (char *)NULL);
		Blt_ListReset(&configOptList);
		return TCL_ERROR;
	    }
	} else if ((option[0] == '-') && (option[1] == '\0')) {
	    break;
	} else {
	    /*
	     * Verify that the switch is actually an entry configuration
	     * option.
	     */
	    if (Tk_ConfigureValue(interp, hierPtr->tkwin, entryConfigSpecs,
		    (char *)hierPtr->rootPtr->entryPtr, argv[i], 0) != TCL_OK) {
		Tcl_ResetResult(interp);
		Tcl_AppendResult(interp, "bad find switch \"", argv[i], "\"",
		    (char *)NULL);
		Blt_ListReset(&configOptList);
		return TCL_ERROR;
	    }
	    if ((i + 1) == argc) {
		goto missingArg;
	    }
	    /* Save the option in the list of configuration options */
	    item = Blt_ListFind(&configOptList, argv[i]);
	    if (item == NULL) {
		item = Blt_ListNewItem(&configOptList, argv[i]);
		Blt_ListAppendItem(&configOptList, item);
	    }
	    Blt_ListSetValue(item, (ClientData)argv[i + 1]);
	    i++;
	}
    }

    if ((argc - i) > 2) {
	Blt_ListReset(&configOptList);
	Tcl_AppendResult(interp, "too many args", (char *)NULL);
	return TCL_ERROR;
    }
    /*
     * Step 2:  Find the range of the search.  Check the order of two
     *		nodes and arrange the search accordingly.
     *
     *	Note:	Be careful to treat "end" as the end of all nodes, instead
     *		of the end of visible nodes.  That way, we can search the
     *		entire tree, even if the last folder is closed.
     */
    firstPtr = hierPtr->rootPtr;/* Default to root node */
    lastPtr = EndNode(firstPtr, 0);

    if (i < argc) {
	if ((argv[i][0] == 'e') && (strcmp(argv[i], "end") == 0)) {
	    firstPtr = EndNode(hierPtr->rootPtr, 0);
	} else if (StringToNode(hierPtr, argv[i], &firstPtr) != TCL_OK) {
	    return TCL_ERROR;
	}
	i++;
    }
    if (i < argc) {
	if ((argv[i][0] == 'e') && (strcmp(argv[i], "end") == 0)) {
	    lastPtr = EndNode(hierPtr->rootPtr, 0);
	} else if (StringToNode(hierPtr, argv[i], &lastPtr) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    if (IsBefore(lastPtr, firstPtr)) {
	nextProc = LastNode;
    }
    numMatches = 0;

    /*
     * Step 3:	Search through the tree and look for nodes that match the
     *		current pattern specifications.  Save the name of each of
     *		the matching nodes.
     */
    Tcl_DStringInit(&dString);
    for (treePtr = firstPtr; treePtr != NULL;
	treePtr = (*nextProc) (treePtr, 0)) {
	if (namePattern != NULL) {
	    result = (*compareProc) (interp, treePtr->nameId, namePattern);
	    if (result == invertMatch) {
		goto nextNode;	/* Failed to match */
	    }
	}
	if (fullPattern != NULL) {
	    Tcl_DStringInit(&pathString);
	    GetFullPath(treePtr, hierPtr->separator, &pathString);
	    result = (*compareProc) (interp, Tcl_DStringValue(&pathString),
		fullPattern);
	    if (result == invertMatch) {
		goto nextNode;	/* Failed to match */
	    }
	}
	for (item = Blt_ListFirstItem(&configOptList); item != NULL;
	    item = Blt_ListNextItem(item)) {
	    option = Blt_ListGetKey(item);
	    Tcl_ResetResult(interp);
	    if (Tk_ConfigureValue(interp, hierPtr->tkwin, entryConfigSpecs,
		    (char *)treePtr->entryPtr, option, 0) != TCL_OK) {
		goto error;	/* This shouldn't happen. */
	    }
	    pattern = (char *)Blt_ListGetValue(item);
	    value = Tcl_GetStringResult(interp);
	    result = (*compareProc) (interp, value, pattern);
	    if (result == invertMatch) {
		goto nextNode;	/* Failed to match */
	    }
	}
	if (execCmd != NULL) {
	    Tcl_DString cmdString;

	    PercentSubst(hierPtr, treePtr, execCmd, &cmdString);
	    result = Tcl_GlobalEval(interp, Tcl_DStringValue(&cmdString));
	    Tcl_DStringFree(&cmdString);
	    if (result != TCL_OK) {
		goto error;
	    }
	}
	/* Finally, save the matching node name. */
	Tcl_DStringAppendElement(&dString, NodeToString(hierPtr, treePtr));
	numMatches++;
	if ((numMatches == maxMatches) && (maxMatches > 0)) {
	    break;
	}
      nextNode:
	if (treePtr == lastPtr) {
	    break;
	}
    }
    Tcl_ResetResult(interp);
    Blt_ListReset(&configOptList);
    Tcl_DStringResult(interp, &dString);
    return TCL_OK;

  missingArg:
    Tcl_AppendResult(interp, "missing argument for find option \"", argv[i],
	"\"", (char *)NULL);
  error:
    Tcl_DStringFree(&dString);
    Blt_ListReset(&configOptList);
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * GetOp --
 *
 *	Converts one or more node identifiers to its path component.
 *	The path may be either the single entry name or the full path
 *	of the entry.
 *
 * Results:
 *	A standard Tcl result.  The interpreter result will contain a
 *	list of the convert names.
 *
 *----------------------------------------------------------------------
 */
static int
GetOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int fullName;
    Tree *treePtr;
    Tcl_DString dString;
    Tcl_DString pathString;
    register int i;

    fullName = FALSE;
    if ((argc > 2) && (argv[2][0] == '-') && (strcmp(argv[2], "-full") == 0)) {
	fullName = TRUE;
	argv++, argc--;
    }
    Tcl_DStringInit(&dString);
    Tcl_DStringInit(&pathString);
    for (i = 2; i < argc; i++) {
	treePtr = hierPtr->focusPtr;
	if (GetNode(hierPtr, argv[i], &treePtr) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (treePtr == NULL) {
	    Tcl_DStringAppendElement(&dString, "");
	    continue;
	}
	if (fullName) {
	    GetFullPath(treePtr, hierPtr->separator, &pathString);
	    Tcl_DStringAppendElement(&dString, Tcl_DStringValue(&pathString));
	} else {
	    Tcl_DStringAppendElement(&dString, treePtr->nameId);
	}
    }
    Tcl_DStringFree(&pathString);
    Tcl_DStringResult(interp, &dString);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * SearchAndApplyToTree --
 *
 *	Searches through the current tree and applies a procedure
 *	to matching nodes.  The search specification is taken from
 *	the following command-line arguments:
 *
 *      ?-exact? ?-glob? ?-regexp? ?-nonmatching?
 *      ?-data string?
 *      ?-name string?
 *      ?-full string?
 *      ?--?
 *      ?index...?
 *
 * Results:
 *	A standard Tcl result.  If the result is valid, and if the
 *      nonmatchPtr is specified, it returns a boolean value
 *      indicating whether or not the search was inverted.  This
 *      is needed to fix things properly for the "hide nonmatching"
 *      case.
 *
 *----------------------------------------------------------------------
 */
static int
SearchAndApplyToTree(hierPtr, interp, argc, argv, proc, nonMatchPtr)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
    ApplyProc *proc;
    int *nonMatchPtr;		/* returns: inverted search indicator */
{
    CompareProc *compareProc;
    int invertMatch;		/* normal search mode (matching entries) */
    char *namePattern, *fullPattern;
    register int i;
    int length;
    int result;
    char *option, *pattern, *value;
    Tree *treePtr;
    char c;
    Blt_List configOptList;
    register Blt_ListItem item;

    Blt_InitList(&configOptList, TCL_STRING_KEYS);
    invertMatch = FALSE;
    namePattern = fullPattern = NULL;
    compareProc = ExactCompare;
    for (i = 2; i < argc; i++) {
	if (argv[i][0] != '-') {
	    break;
	}
	option = argv[i] + 1;
	length = strlen(option);
	c = option[0];
	if ((c == 'e') && (strncmp(option, "exact", length) == 0)) {
	    compareProc = ExactCompare;
	} else if ((c == 'g') && (strncmp(option, "glob", length) == 0)) {
	    compareProc = GlobCompare;
	} else if ((c == 'r') && (strncmp(option, "regexp", length) == 0)) {
	    compareProc = RegexpCompare;
	} else if ((c == 'n') && (length > 1) &&
	    (strncmp(option, "nonmatching", length) == 0)) {
	    invertMatch = TRUE;
	} else if ((c == 'f') && (strncmp(option, "full", length) == 0)) {
	    if ((i + 1) == argc) {
		goto missingArg;
	    }
	    i++;
	    fullPattern = argv[i];
	} else if ((c == 'n') && (length > 1) &&
	    (strncmp(option, "name", length) == 0)) {
	    if ((i + 1) == argc) {
		goto missingArg;
	    }
	    i++;
	    namePattern = argv[i];
	} else if ((option[0] == '-') && (option[1] == '\0')) {
	    break;
	} else {
	    /*
	     * Verify that the switch is actually an entry configuration option.
	     */
	    if (Tk_ConfigureValue(interp, hierPtr->tkwin, entryConfigSpecs,
		    (char *)hierPtr->rootPtr->entryPtr, argv[i], 0) != TCL_OK) {
		Tcl_ResetResult(interp);
		Tcl_AppendResult(interp, "bad switch \"", argv[i],
		    "\": must be -exact, -glob, -regexp, -name, -full, or -nonmatching",
		    (char *)NULL);
		return TCL_ERROR;
	    }
	    if ((i + 1) == argc) {
		goto missingArg;
	    }
	    /* Save the option in the list of configuration options */
	    item = Blt_ListFind(&configOptList, argv[i]);
	    if (item == NULL) {
		item = Blt_ListNewItem(&configOptList, argv[i]);
		Blt_ListAppendItem(&configOptList, item);
	    }
	    Blt_ListSetValue(item, (ClientData)argv[i + 1]);
	}
    }

    if ((namePattern != NULL) || (fullPattern != NULL) ||
	(Blt_ListGetLength(&configOptList) > 0)) {
	/*
	 * Search through the tree and look for nodes that match the
	 * current spec.  Apply the input procedure to each of the
	 * matching nodes.
	 */
	for (treePtr = hierPtr->rootPtr; treePtr != NULL;
	    treePtr = NextNode(treePtr, 0)) {

	    if (namePattern != NULL) {
		result = (*compareProc) (interp, treePtr->nameId, namePattern);
		if (result == invertMatch) {
		    continue;	/* Failed to match */
		}
	    }
	    if (fullPattern != NULL) {
		Tcl_DString pathString;

		Tcl_DStringInit(&pathString);
		GetFullPath(treePtr, hierPtr->separator, &pathString);
		result = (*compareProc) (interp, Tcl_DStringValue(&pathString),
		    fullPattern);
		Tcl_DStringFree(&pathString);
		if (result == invertMatch) {
		    continue;	/* Failed to match */
		}
	    }
	    for (item = Blt_ListFirstItem(&configOptList); item != NULL;
		item = Blt_ListNextItem(item)) {
		option = Blt_ListGetKey(item);
		Tcl_ResetResult(interp);
		if (Tk_ConfigureValue(interp, hierPtr->tkwin, entryConfigSpecs,
			(char *)treePtr->entryPtr, option, 0) != TCL_OK) {
		    return TCL_ERROR;	/* This shouldn't happen. */
		}
		pattern = (char *)Blt_ListGetValue(item);
		value = Tcl_GetStringResult(interp);
		result = (*compareProc) (interp, value, pattern);
		if (result == invertMatch) {
		    continue;	/* Failed to match */
		}
	    }
	    /* Finally, apply the procedure to the node */
	    (*proc) (hierPtr, treePtr);
	}
	Tcl_ResetResult(interp);
	Blt_ListReset(&configOptList);
    }
    /*
     * Apply the procedure to nodes that have been specified
     * individually.
     */
    for ( /*empty*/ ; i < argc; i++) {
	if ((argv[i][0] == 'a') && (strcmp(argv[i], "all") == 0)) {
	    return ApplyToTree(hierPtr, hierPtr->rootPtr, proc, APPLY_RECURSE);
	}
	if (StringToNode(hierPtr, argv[i], &treePtr) != TCL_OK) {
	    return TCL_ERROR;
	}
	if ((*proc) (hierPtr, treePtr) != TCL_OK) {
	    return TCL_ERROR;
	}
    }

    if (nonMatchPtr != NULL) {
	*nonMatchPtr = invertMatch;	/* return "inverted search" status */
    }
    return TCL_OK;

  missingArg:
    Blt_ListReset(&configOptList);
    Tcl_AppendResult(interp, "missing pattern for search option \"", argv[i],
	"\"", (char *)NULL);
    return TCL_ERROR;

}

/*
 *----------------------------------------------------------------------
 *
 * HideOp --
 *
 *	Hides one or more nodes.  Nodes can be specified by their
 *      index, or by matching a name or data value pattern.  By
 *      default, the patterns are matched exactly.  They can also
 *      be matched using glob-style and regular expression rules.
 *
 * Results:
 *	A standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
static int
HideOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int status, nonmatching;

    status = SearchAndApplyToTree(hierPtr, interp, argc, argv, UnmapNode,
	&nonmatching);

    if (status != TCL_OK) {
	return TCL_ERROR;
    }
    /*
     * If this was an inverted search, scan back through the
     * tree and make sure that the parents for all visible
     * nodes are also visible.  After all, if a node is supposed
     * to be visible, its parent can't be hidden.
     */
    if (nonmatching) {
	ApplyToTree(hierPtr, hierPtr->rootPtr, MapAncestors, APPLY_RECURSE);
    }
    /*
     * Make sure that selections are cleared from any hidden
     * nodes.  This wasn't done earlier--we had to delay it until
     * we fixed the visibility status for the parents.
     */
    ApplyToTree(hierPtr, hierPtr->rootPtr, FixUnmappedSelections,
	APPLY_RECURSE);

    hierPtr->flags |= (HIER_LAYOUT | HIER_SCROLL);
    EventuallyRedraw(hierPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ShowOp --
 *
 *	Mark one or more nodes to be exposed.  Nodes can be specified
 *	by their index, or by matching a name or data value pattern.  By
 *      default, the patterns are matched exactly.  They can also
 *      be matched using glob-style and regular expression rules.
 *
 * Results:
 *	A standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
static int
ShowOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    if (SearchAndApplyToTree(hierPtr, interp, argc, argv, MapNode,
	    (int *)NULL) != TCL_OK) {
	return TCL_ERROR;
    }
    hierPtr->flags |= (HIER_LAYOUT | HIER_SCROLL);
    EventuallyRedraw(hierPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * IndexOp --
 *
 *	Converts one of more words representing indices of the entries
 *	in the hierarchy widget to their respective serial identifiers.
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
IndexOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    Tree *nodePtr, *rootPtr;

    rootPtr = hierPtr->focusPtr;
    if ((argv[2][0] == '-') && (strcmp(argv[2], "-at") == 0)) {
	if (StringToNode(hierPtr, argv[3], &rootPtr) != TCL_OK) {
	    return TCL_ERROR;
	}
	argv += 2, argc -= 2;
    }
    if (argc > 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " index ?-at index? index\"", (char *)NULL);
	return TCL_ERROR;
    }
    nodePtr = rootPtr;
    if ((GetNode(hierPtr, argv[2], &nodePtr) == TCL_OK) && (nodePtr != NULL)) {
	Tcl_SetResult(interp, NodeToString(hierPtr, nodePtr), TCL_VOLATILE);
    } else {
	Tcl_SetResult(interp, "", TCL_STATIC);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * InsertOp --
 *
 *	Add new entries into a hierarchy.  If no node is specified,
 *	new entries will be added to the root of the hierarchy.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
InsertOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    Tree *rootPtr, *nodePtr, *parentPtr;
    int position;
    int level, count;
    register char *path;
    Tcl_DString dString;
    register int i, j;
    int numOpts;
    char **options;
    char **compArr;

    rootPtr = hierPtr->rootPtr;
    if ((argv[2][0] == '-') && (strcmp(argv[2], "-at") == 0)) {
	if (StringToNode(hierPtr, argv[3], &rootPtr) != TCL_OK) {
	    return TCL_ERROR;
	}
	argv += 2, argc -= 2;
    }
    if (GetPosition(hierPtr->interp, argv[2], &position) != TCL_OK) {
	return TCL_ERROR;
    }
    argc -= 3, argv += 3;

    /*
     * Count the pathnames that follow.  Count the arguments until we
     * spot one that looks like a configuration option (i.e. starts
     * with a minus ("-")).
     */
    for (count = 0; count < argc; count++) {
	if (argv[count][0] == '-') {
	    break;
	}
    }
    numOpts = argc - count;
    options = argv + count;

    Tcl_DStringInit(&dString);
    for (i = 0; i < count; i++) {
	path = argv[i];
	if (hierPtr->trimLeft != NULL) {
	    register char *p, *s;

	    /* Trim off leading character string if one exists. */
	    for (p = path, s = hierPtr->trimLeft; *s != '\0'; s++, p++) {
		if (*p != *s) {
		    break;
		}
	    }
	    if (*s == '\0') {
		path = p;
	    }
	}
	/*
	 * Split the path and find the parent node of the path.
	 */
	compArr = NULL;
	if (hierPtr->separator == NULL) {
	    if (Tcl_SplitList(interp, path, &level, &compArr) != TCL_OK) {
		goto error;
	    }
	} else {
	    if (SplitPath(hierPtr, path, &level, &compArr) != TCL_OK) {
		goto error;
	    }
	}
	if (level == 0) {
	    continue;		/* Root already exists. */
	}
	parentPtr = rootPtr;
	level--;
	for (j = 0; j < level; j++) {
	    nodePtr = FindComponent(parentPtr, compArr[j]);
	    if (nodePtr == NULL) {
		if (!hierPtr->autoCreate) {
		    Tcl_AppendResult(interp, "can't find path component \"",
			compArr[j], "\" in \"", path, "\"", (char *)NULL);
		    goto error;
		}
		nodePtr = CreateNode(hierPtr, parentPtr, POSITION_END,
		    compArr[j]);
	    }
	    parentPtr = nodePtr;
	}
	nodePtr = NULL;
	if (!hierPtr->allowDuplicates) {
	    nodePtr = FindComponent(parentPtr, compArr[level]);
	}
	if (nodePtr == NULL) {
	    nodePtr = CreateNode(hierPtr, parentPtr, position, compArr[level]);
	    if (nodePtr == NULL) {
		goto error;
	    }
	    if (ConfigureEntry(hierPtr, nodePtr->entryPtr, numOpts, options,
		    TK_CONFIG_ARGV_ONLY) != TCL_OK) {
		DestroyNode(hierPtr, nodePtr);
		goto error;
	    }
	    Tcl_DStringAppendElement(&dString, NodeToString(hierPtr, nodePtr));
	} else {
	    if (ConfigureEntry(hierPtr, nodePtr->entryPtr, numOpts, options,
		    0) != TCL_OK) {
		goto error;
	    }
	}
	free((char *)compArr);
    }
    hierPtr->flags |= (HIER_LAYOUT | HIER_SCROLL);
    EventuallyRedraw(hierPtr);
    Tcl_DStringResult(hierPtr->interp, &dString);
    return TCL_OK;
  error:
    if (compArr != NULL) {
	free((char *)compArr);
    }
    Tcl_DStringFree(&dString);
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * DeleteOp --
 *
 *	Deletes nodes from the hierarchy. Deletes either a range of
 *	entries from a hierarchy or a single node (except root).
 *	In all cases, nodes are removed recursively.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
DeleteOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    Tree *treePtr;
    register Blt_ListItem item;
    Blt_ListItem firstItem, lastItem, nextItem;

    if (StringToNode(hierPtr, argv[2], &treePtr) != TCL_OK) {
	return TCL_ERROR;	/* Node or path doesn't already exist */
    }
    firstItem = lastItem = NULL;
    switch (argc) {
    case 3:
	/*
	 * Delete a single hierarchy. If the node specified is root,
	 * delete only the children.
	 */
	if (treePtr != hierPtr->rootPtr) {
	    DestroyTree(hierPtr, treePtr);	/* Don't delete root */
	    goto done;
	}
	firstItem = Blt_ListFirstItem(treePtr->nodeList);
	lastItem = Blt_ListLastItem(treePtr->nodeList);
	break;

    case 4:
	/*
	 * Delete a single node from hierarchy specified by its
	 * numeric position.
	 */
	{
	    int position;

	    if (GetPosition(interp, argv[3], &position) != TCL_OK) {
		return TCL_ERROR;
	    }
	    if ((position != POSITION_END) &&
		(position >= Blt_ListGetLength(treePtr->nodeList))) {
		return TCL_OK;	/* Bad first index */
	    }
	    if (position == POSITION_END) {
		item = Blt_ListLastItem(treePtr->nodeList);
	    } else {
		item = Blt_ListFindNthItem(treePtr->nodeList, position, 1);
	    }
	    firstItem = lastItem = item;
	}
	break;

    case 5:
	/*
	 * Delete range of nodes in hierarchy specified by first/last
	 * positions.
	 */
	{
	    int first, last;
	    int numEntries;

	    if ((GetPosition(interp, argv[3], &first) != TCL_OK) ||
		(GetPosition(interp, argv[4], &last) != TCL_OK)) {
		return TCL_ERROR;
	    }
	    numEntries = Blt_ListGetLength(treePtr->nodeList);
	    if (numEntries == 0) {
		return TCL_OK;
	    }
	    if (first == POSITION_END) {
		first = numEntries - 1;
	    }
	    if (last >= numEntries) {
		last = numEntries - 1;
	    }
	    if (first > last) {
		Tcl_AppendResult(interp, "bad range: \"", argv[3],
		    " > ", argv[4], "\"", (char *)NULL);
		return TCL_ERROR;
	    }
	    firstItem = Blt_ListFindNthItem(treePtr->nodeList, first, 1);
	    lastItem = Blt_ListFindNthItem(treePtr->nodeList, last, 1);
	}
	break;
    }
    for (item = firstItem; item != NULL; item = nextItem) {
	nextItem = Blt_ListNextItem(item);
	DestroyTree(hierPtr, (Tree *) Blt_ListGetValue(item));
	if (item == lastItem) {
	    break;
	}
    }
  done:
    hierPtr->flags |= (HIER_LAYOUT | HIER_SCROLL);
    EventuallyRedraw(hierPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * MoveOp --
 *
 *	Move an entry into a new location in the hierarchy.
 *
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
MoveOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    Tree *srcPtr, *destPtr, *parentPtr;
    char c;
    int action;

#define MOVE_INTO	(1<<0)
#define MOVE_BEFORE	(1<<1)
#define MOVE_AFTER	(1<<2)
    if (StringToNode(hierPtr, argv[2], &srcPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    c = argv[3][0];
    action = MOVE_INTO;
    if ((c == 'i') && (strcmp(argv[3], "into") == 0)) {
	action = MOVE_INTO;
    } else if ((c == 'b') && (strcmp(argv[3], "before") == 0)) {
	action = MOVE_BEFORE;
    } else if ((c == 'a') && (strcmp(argv[3], "after") == 0)) {
	action = MOVE_AFTER;
    } else {
	Tcl_AppendResult(interp, "bad position \"", argv[3],
	    "\": should be into, before, or after", (char *)NULL);
	return TCL_ERROR;
    }
    if (StringToNode(hierPtr, argv[4], &destPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    /* Verify they aren't ancestors. */
    if (IsAncestor(srcPtr, destPtr)) {
	Tcl_AppendResult(interp, "can't move node: \"", argv[2],
	    "\" is an ancestor of \"", argv[4], "\"", (char *)NULL);
	return TCL_ERROR;
    }
    parentPtr = destPtr->parentPtr;
    if (parentPtr == NULL) {
	action = MOVE_INTO;
    }
    Blt_ListUnlinkItem(srcPtr->item);
    switch (action) {
    case MOVE_INTO:
	Blt_ListLinkBefore(destPtr->nodeList, srcPtr->item, (Blt_ListItem)NULL);
	parentPtr = destPtr;
	break;

    case MOVE_BEFORE:
	Blt_ListLinkBefore(parentPtr->nodeList, srcPtr->item, destPtr->item);
	break;

    case MOVE_AFTER:
	Blt_ListLinkAfter(parentPtr->nodeList, srcPtr->item, destPtr->item);
	break;
    }
    srcPtr->parentPtr = parentPtr;
    srcPtr->level = parentPtr->level + 1;
    hierPtr->flags |= (HIER_LAYOUT | HIER_SCROLL | HIER_DIRTY);
    EventuallyRedraw(hierPtr);
    return TCL_OK;
}

/*ARGSUSED*/
static int
NearestOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;			/* Not used. */
    char **argv;
{
    int x, y;			/* Screen coordinates of the test point. */
    register Entry *entryPtr;
    register Tree *treePtr;

    if ((Tk_GetPixels(interp, hierPtr->tkwin, argv[2], &x) != TCL_OK) ||
	(Tk_GetPixels(interp, hierPtr->tkwin, argv[3], &y) != TCL_OK)) {
	return TCL_ERROR;
    }
    if (hierPtr->numVisible == 0) {
	return TCL_OK;
    }
    treePtr = NearestNode(hierPtr, x, y, TRUE);
    if (treePtr == NULL) {
	return TCL_OK;
    }
    x = WORLDX(hierPtr, x);
    y = WORLDY(hierPtr, y);
    entryPtr = treePtr->entryPtr;
    if (argc > 4) {
	char *where;
	int labelX;

	where = "";
	if (entryPtr->flags & ENTRY_BUTTON) {
	    int buttonX, buttonY;

	    buttonX = entryPtr->worldX + entryPtr->buttonX;
	    buttonY = entryPtr->worldY + entryPtr->buttonY;
	    if ((x >= buttonX) && (x < (buttonX + hierPtr->buttonWidth)) &&
		(y >= buttonY) && (y < (buttonY + hierPtr->buttonHeight))) {
		where = "gadget";
	    }
	}
	labelX = entryPtr->worldX + LEVELWIDTH(treePtr->level);
	if ((x >= labelX) &&
	    (x < (labelX + LEVELWIDTH(treePtr->level + 1) + entryPtr->width))) {
	    where = "select";
	}
	if (Tcl_SetVar(interp, argv[4], where, TCL_LEAVE_ERR_MSG) == NULL) {
	    return TCL_ERROR;
	}
    }
    Tcl_SetResult(interp, NodeToString(hierPtr, treePtr), TCL_VOLATILE);
    return TCL_OK;
}

/*ARGSUSED*/
static int
OpenOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;		/* Not used. */
    int argc;
    char **argv;
{
    Tree *rootPtr;
    int length;
    unsigned int flags;
    register int i;

    flags = 0;
    length = strlen(argv[2]);
    if ((argv[2][0] == '-') && (length > 1) &&
	(strncmp(argv[2], "-recurse", length) == 0)) {
	argv++, argc--;
	flags |= APPLY_RECURSE;
    }
    for (i = 2; i < argc; i++) {
	rootPtr = hierPtr->focusPtr;
	if (GetNode(hierPtr, argv[i], &rootPtr) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (rootPtr == NULL) {
	    continue;
	}
	ExposeAncestors(rootPtr);	/* Also make sure that all the ancestors
				   * of this node are also not hidden. */
	if (ApplyToTree(hierPtr, rootPtr, OpenNode, flags) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    hierPtr->flags |= (HIER_LAYOUT | HIER_SCROLL);
    EventuallyRedraw(hierPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * RangeOp --
 *
 *	Returns the node identifiers in a given range.
 *
 *----------------------------------------------------------------------
 */
static int
RangeOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Tree *firstPtr, *lastPtr;
    register Tree *treePtr;
    unsigned int flags;
    int length;

    flags = 0;
    length = strlen(argv[2]);
    if ((argv[2][0] == '-') && (length > 1) &&
	(strncmp(argv[2], "-open", length) == 0)) {
	argv++, argc--;
	flags |= ENTRY_OPEN;
    }
    if (StringToNode(hierPtr, argv[2], &firstPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    lastPtr = EndNode(firstPtr, flags);
    if (argc > 3) {
	if (StringToNode(hierPtr, argv[3], &lastPtr) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    if (flags & ENTRY_OPEN) {
	if (IsHidden(firstPtr)) {
	    Tcl_AppendResult(interp, "first node \"", argv[2], "\" is hidden.",
		(char *)NULL);
	    return TCL_ERROR;
	}
	if (IsHidden(lastPtr)) {
	    Tcl_AppendResult(interp, "last node \"", argv[3], "\" is hidden.",
		(char *)NULL);
	    return TCL_ERROR;
	}
    }
    /*
     * The relative order of the first/last markers determines the
     * direction.
     */
    if (IsBefore(lastPtr, firstPtr)) {
	for (treePtr = lastPtr; treePtr != NULL;
	    treePtr = LastNode(treePtr, flags)) {
	    Tcl_AppendElement(interp, NodeToString(hierPtr, treePtr));
	    if (treePtr == firstPtr) {
		break;
	    }
	}
    } else {
	for (treePtr = firstPtr; treePtr != NULL;
	    treePtr = NextNode(treePtr, flags)) {
	    Tcl_AppendElement(interp, NodeToString(hierPtr, treePtr));
	    if (treePtr == lastPtr) {
		break;
	    }
	}
    }
    return TCL_OK;
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
ScanOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    int x, y;
    char c;
    unsigned int length;
    int oper;

#define SCAN_MARK		1
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
    if ((Tk_GetPixels(interp, hierPtr->tkwin, argv[3], &x) != TCL_OK) ||
	(Tk_GetPixels(interp, hierPtr->tkwin, argv[4], &y) != TCL_OK)) {
	return TCL_ERROR;
    }
    if (oper == SCAN_MARK) {
	hierPtr->scanAnchorX = x;
	hierPtr->scanAnchorY = y;
	hierPtr->scanX = hierPtr->xOffset;
	hierPtr->scanY = hierPtr->yOffset;
    } else {
	int worldX, worldY;
	int dx, dy;

	dx = hierPtr->scanAnchorX - x;
	dy = hierPtr->scanAnchorY - y;
	worldX = hierPtr->scanX + (10 * dx);
	worldY = hierPtr->scanY + (10 * dy);

	if (worldX < 0) {
	    worldX = 0;
	} else if (worldX >= hierPtr->worldWidth) {
	    worldX = hierPtr->worldWidth - hierPtr->xScrollUnits;
	}
	if (worldY < 0) {
	    worldY = 0;
	} else if (worldY >= hierPtr->worldHeight) {
	    worldY = hierPtr->worldHeight - hierPtr->yScrollUnits;
	}
	hierPtr->xOffset = worldX;
	hierPtr->yOffset = worldY;
	hierPtr->flags |= (HIER_LAYOUT | HIER_SCROLL);
	EventuallyRedraw(hierPtr);
    }
    return TCL_OK;
}

/*ARGSUSED*/
static int
SeeOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;		/* Not used. */
    int argc;
    char **argv;
{
    Entry *entryPtr;
    int width, height;
    int x, y;
    Tree *treePtr;
    Tk_Anchor anchor;
    int xMin, xMax, yMin, yMax;

    anchor = TK_ANCHOR_W;	/* Default anchor is West */
    if ((argv[2][0] == '-') && (strcmp(argv[2], "-anchor") == 0)) {
	if (argc == 3) {
	    Tcl_AppendResult(interp, "missing \"-anchor\" argument",
		(char *)NULL);
	    return TCL_ERROR;
	}
	if (Tk_GetAnchor(interp, argv[3], &anchor) != TCL_OK) {
	    return TCL_ERROR;
	}
	argc -= 2, argv += 2;
    }
    if (argc == 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    "see ?-anchor anchor? index\"", (char *)NULL);
	return TCL_ERROR;
    }
    treePtr = hierPtr->focusPtr;
    if (GetNode(hierPtr, argv[2], &treePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (treePtr == NULL) {
	return TCL_OK;
    }
    if (IsHidden(treePtr)) {
	ExposeAncestors(treePtr);
	hierPtr->flags |= (HIER_LAYOUT | HIER_SCROLL);
	/*
	 * If the entry wasn't previously exposed, its world coordinates
	 * aren't likely to be valid.  So re-compute the layout before
	 * we try to see the viewport to the entry's location.
	 */
	ComputeLayout(hierPtr);
    }
    entryPtr = treePtr->entryPtr;
    width = VPORTWIDTH(hierPtr);
    height = VPORTHEIGHT(hierPtr);

    /*
     * XVIEW:	If the entry is left or right of the current view, adjust
     *		the offset.  If the entry is nearby, adjust the view just
     *		a bit.  Otherwise, center the entry.
     */
    xMin = hierPtr->xOffset;
    xMax = hierPtr->xOffset + width;

    switch (anchor) {
    case TK_ANCHOR_W:
    case TK_ANCHOR_NW:
    case TK_ANCHOR_SW:
	x = 0;
	break;
    case TK_ANCHOR_E:
    case TK_ANCHOR_NE:
    case TK_ANCHOR_SE:
	x = entryPtr->worldX + entryPtr->width + LEVELWIDTH(treePtr->level) -
	    width;
	break;
    default:
	if (entryPtr->worldX < xMin) {
	    x = entryPtr->worldX;
	} else if ((entryPtr->worldX + entryPtr->width) > xMax) {
	    x = entryPtr->worldX + entryPtr->width - width;
	} else {
	    x = hierPtr->xOffset;
	}
	break;
    }
    /*
     * YVIEW:	If the entry is above or below the current view, adjust
     *		the offset.  If the entry is nearby, adjust the view just
     *		a bit.  Otherwise, center the entry.
     */
    yMin = hierPtr->yOffset;
    yMax = hierPtr->yOffset + height;

    switch (anchor) {
    case TK_ANCHOR_N:
    case TK_ANCHOR_NE:
    case TK_ANCHOR_NW:
	y = entryPtr->worldY;
	break;
    case TK_ANCHOR_S:
    case TK_ANCHOR_SE:
    case TK_ANCHOR_SW:
	y = entryPtr->worldY + entryPtr->height - height;
	break;
    default:
	if (entryPtr->worldY < yMin) {
	    y = entryPtr->worldY;
	} else if ((entryPtr->worldY + entryPtr->height) > yMax) {
	    y = entryPtr->worldY + entryPtr->height - height;
	} else {
	    y = hierPtr->yOffset;
	}
	break;
    }
    if ((y != hierPtr->yOffset) || (x != hierPtr->xOffset)) {
	hierPtr->xOffset = x;
	hierPtr->yOffset = y;
	hierPtr->flags |= (HIER_SCROLL | HIER_LAYOUT);
    }
    EventuallyRedraw(hierPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * AnchorOpOp --
 *
 *	Sets the selection anchor to the element given by a index.
 *	The selection anchor is the end of the selection that is fixed
 *	while dragging out a selection with the mouse.  The index
 *	"anchor" may be used to refer to the anchor element.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The selection changes.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
AnchorOpOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;		/* Not used. */
    int argc;			/* Not used. */
    char **argv;
{
    Tree *nodePtr;

    nodePtr = hierPtr->focusPtr;
    if (GetNode(hierPtr, argv[3], &nodePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    hierPtr->selAnchorPtr = nodePtr;
    hierPtr->selMarkPtr = NULL;	/* Clear the mark. */
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * CancelOpOp --
 *
 *	Cancels the temporary selection.  This is typically done when
 *	the mouse is released outside of the widget's window.
 *
 * Results:
 *	Always TCL_OK.
 *
 * Side effects:
 *	The appearance of the selection changes.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
CancelOpOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;		/* Not used. */
    int argc;			/* Not used. */
    char **argv;
{
    hierPtr->selMarkPtr = NULL;	/* Clear the mark. */
    EventuallyRedraw(hierPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * IncludesOpOp
 *
 *	Returns 1 if the element indicated by index is currently
 *	selected, 0 if it isn't.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The selection changes.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
IncludesOpOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;			/* Not used. */
    char **argv;
{
    Tree *treePtr;

    if (StringToNode(hierPtr, argv[3], &treePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    Tcl_SetResult(interp,
	(treePtr->entryPtr->flags & ENTRY_SELECTED) ? "1" : "0", TCL_STATIC);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * DragOpOp --
 *
 *	Sets the selection anchor to the element given by a index.
 *	The selection anchor is the end of the selection that is fixed
 *	while dragging out a selection with the mouse.  The index
 *	"anchor" may be used to refer to the anchor element.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The selection changes.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
DragOpOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;			/* Not used. */
    char **argv;
{
    Tree *nodePtr;
    char c;
    int length;

    nodePtr = hierPtr->focusPtr;
    if (GetNode(hierPtr, argv[3], &nodePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    c = argv[4][0];
    length = strlen(argv[4]);
    hierPtr->flags &= ~SELECT_MASK;
    if ((c == 'c') && (strncmp(argv[4], "clear", length) == 0)) {
	hierPtr->flags |= SELECT_CLEAR;
    } else if ((c == 's') && (strncmp(argv[4], "set", length) == 0)) {
	hierPtr->flags |= SELECT_SET;
    } else if ((c == 't') && (strncmp(argv[4], "toggle", length) == 0)) {
	hierPtr->flags |= SELECT_TOGGLE;
    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[4],
	    "\": should be \"clear\", \"set\", or \"toggle\"", (char *)NULL);
	return TCL_ERROR;
    }
    hierPtr->selMarkPtr = nodePtr;
    EventuallyRedraw(hierPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * SelectOpOp
 *
 *	Selects, deselects, or toggles all of the elements in the
 *	range between first and last, inclusive, without affecting the
 *	selection state of elements outside that range.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The selection changes.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
SelectOpOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;			/* Not used. */
    char **argv;
{
    Tree *firstPtr, *lastPtr;

    hierPtr->flags &= ~SELECT_MASK;
    switch (argv[2][0]) {
    case 's':
	hierPtr->flags |= SELECT_SET;
	break;
    case 'c':
	hierPtr->flags |= SELECT_CLEAR;
	break;
    case 't':
	hierPtr->flags |= SELECT_TOGGLE;
	break;
    }

    if (StringToNode(hierPtr, argv[3], &firstPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if ((IsHidden(firstPtr)) && !(hierPtr->flags & SELECT_CLEAR)) {
	Tcl_AppendResult(interp, "can't select hidden node \"", argv[3], "\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    lastPtr = firstPtr;
    if (argc > 4) {
	if (StringToNode(hierPtr, argv[4], &lastPtr) != TCL_OK) {
	    return TCL_ERROR;
	}
	if ((IsHidden(lastPtr)) && !(hierPtr->flags & SELECT_CLEAR)) {
	    Tcl_AppendResult(interp, "can't select hidden node \"", argv[4],
		"\"", (char *)NULL);
	    return TCL_ERROR;
	}
    }
    if (firstPtr == lastPtr) {
	SelectNode(hierPtr, firstPtr);
    } else {
	SelectRange(hierPtr, firstPtr, lastPtr);
    }
    hierPtr->flags &= ~SELECT_MASK;
    if (hierPtr->flags & SELECT_EXPORT) {
	Tk_OwnSelection(hierPtr->tkwin, XA_PRIMARY, LostSelection,
	    (ClientData)hierPtr);
    }
    EventuallyRedraw(hierPtr);
    if (hierPtr->selectCmd != NULL) {
	EventuallyInvokeSelectCmd(hierPtr);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * SelectionOp --
 *
 *	This procedure handles the individual options for text
 *	selections.  The selected text is designated by start and end
 *	indices into the text pool.  The selected segment has both a
 *	anchored and unanchored ends.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The selection changes.
 *
 *----------------------------------------------------------------------
 */
static Blt_OpSpec selectionOperSpecs[] =
{
    {"anchor", 1, (Blt_Operation)AnchorOpOp, 4, 4, "index",},
    {"cancel", 2, (Blt_Operation)CancelOpOp, 3, 3, "",},
    {"clear", 2, (Blt_Operation)SelectOpOp, 4, 5, "firstIndex ?lastIndex?",},
    {"dragto", 1, (Blt_Operation)DragOpOp, 5, 5, "index action",},
    {"includes", 2, (Blt_Operation)IncludesOpOp, 4, 4, "index",},
    {"set", 1, (Blt_Operation)SelectOpOp, 4, 5, "firstIndex ?lastIndex?",},
    {"toggle", 1, (Blt_Operation)SelectOpOp, 4, 5, "firstIndex ?lastIndex?",},
};
static int numSelectionSpecs = sizeof(selectionOperSpecs) / sizeof(Blt_OpSpec);

static int
SelectionOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Blt_Operation proc;
    int result;

    proc = Blt_GetOperation(interp, numSelectionSpecs, selectionOperSpecs,
	BLT_OPER_ARG2, argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    result = (*proc) (hierPtr, interp, argc, argv);
    return result;
}

/*ARGSUSED*/
static int
SortOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;		/* Not used. */
    int argc;
    char **argv;
{
    int length;
    Tree *rootPtr;
    register int i;
    unsigned int flags;

    flags = 0;
    hierPtr->sortCmd = NULL;
    for (i = 2; i < argc; i++) {
	if (argv[i][0] != '-') {
	    break;		/* Found start of indices */
	}
	length = strlen(argv[i]);
	if ((length > 1) && (strncmp(argv[i], "-recurse", length) == 0)) {
	    flags |= APPLY_RECURSE;
	} else if ((length > 1) && (strncmp(argv[i], "-command", length) == 0)) {
	    if ((i + 1) == argc) {
		Tcl_AppendResult(interp, "\"-command\" must be",
		    " followed by comparison command", (char *)NULL);
		return TCL_ERROR;
	    }
	    i++;
	    hierPtr->sortCmd = argv[i];
	} else if ((argv[i][1] == '-') && (argv[i][2] == '\0')) {
	    break;		/* Allow first index to start with a '-' */
	} else {
	    Tcl_AppendResult(interp, "bad switch \"", argv[i],
		"\": must be -command or -recurse", (char *)NULL);
	    return TCL_ERROR;
	}
    }
    for ( /*empty*/ ; i < argc; i++) {
	if (StringToNode(hierPtr, argv[2], &rootPtr) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (ApplyToTree(hierPtr, rootPtr, SortNode, flags) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    hierPtr->flags |= HIER_LAYOUT;
    EventuallyRedraw(hierPtr);
    return TCL_OK;
}

/*ARGSUSED*/
static int
ToggleOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;		/* Not used. */
    int argc;
    char **argv;
{
    Tree *rootPtr;
    int result;

    rootPtr = hierPtr->focusPtr;
    if (GetNode(hierPtr, argv[2], &rootPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (rootPtr == NULL) {
	return TCL_OK;
    }
    if (rootPtr->entryPtr->flags & ENTRY_OPEN) {
	ClearSubnodes(hierPtr, rootPtr);
	if (IsAncestor(rootPtr, hierPtr->focusPtr)) {
	    hierPtr->focusPtr = rootPtr;
	    Blt_SetFocusItem(hierPtr->bindTable, hierPtr->focusPtr);
	}
	if (IsAncestor(rootPtr, hierPtr->selAnchorPtr)) {
	    hierPtr->selMarkPtr = hierPtr->selAnchorPtr = NULL;
	}
	result = CloseNode(hierPtr, rootPtr);
    } else {
	result = OpenNode(hierPtr, rootPtr);
    }
    if (result != TCL_OK) {
	return TCL_ERROR;
    }
    hierPtr->flags |= (HIER_LAYOUT | HIER_SCROLL);
    EventuallyRedraw(hierPtr);
    return TCL_OK;
}

static int
XViewOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int width, worldWidth;

    width = VPORTWIDTH(hierPtr);
    worldWidth = hierPtr->worldWidth;
    if (argc == 2) {
	double fract;

	/*
	 * Note that we are bounding the fractions between 0.0 and 1.0
	 * to support the "canvas"-style of scrolling.
	 */
	fract = (double)hierPtr->xOffset / worldWidth;
	Tcl_AppendElement(interp, Blt_Double(interp, CLAMP(fract, 0.0, 1.0)));
	fract = (double)(hierPtr->xOffset + width) / worldWidth;
	Tcl_AppendElement(interp, Blt_Double(interp, CLAMP(fract, 0.0, 1.0)));
	return TCL_OK;
    }
    if (Blt_GetScrollInfo(interp, argc, argv, &(hierPtr->xOffset), worldWidth,
	    width, hierPtr->xScrollUnits, hierPtr->scrollMode) != TCL_OK) {
	return TCL_ERROR;
    }
    hierPtr->flags |= HIER_XSCROLL;
    EventuallyRedraw(hierPtr);
    return TCL_OK;
}

static int
YViewOp(hierPtr, interp, argc, argv)
    Hierbox *hierPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int height, worldHeight;

    height = VPORTHEIGHT(hierPtr);
    worldHeight = hierPtr->worldHeight;
    if (argc == 2) {
	double fract;

	/* Report first and last fractions */
	fract = (double)hierPtr->yOffset / worldHeight;
	Tcl_AppendElement(interp, Blt_Double(interp, CLAMP(fract, 0.0, 1.0)));
	fract = (double)(hierPtr->yOffset + height) / worldHeight;
	Tcl_AppendElement(interp, Blt_Double(interp, CLAMP(fract, 0.0, 1.0)));
	return TCL_OK;
    }
    if (Blt_GetScrollInfo(interp, argc, argv, &(hierPtr->yOffset), worldHeight,
	    height, hierPtr->yScrollUnits, hierPtr->scrollMode) != TCL_OK) {
	return TCL_ERROR;
    }
    hierPtr->flags |= HIER_YSCROLL;
    EventuallyRedraw(hierPtr);
    return TCL_OK;
}

/*
 * --------------------------------------------------------------
 *
 * HierboxInstCmd --
 *
 * 	This procedure is invoked to process the "hierbox" command.
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
    {"bind", 2, (Blt_Operation)BindOp, 2, 5, "tagName ?sequence command?",},
    {"bbox", 2, (Blt_Operation)BboxOp, 3, 0, "index...",},
    {"button", 2, (Blt_Operation)ButtonOp, 2, 0, "args",},
    {"cget", 2, (Blt_Operation)CgetOp, 3, 3, "option",},
    {"close", 2, (Blt_Operation)CloseOp, 2, 0, "?-recurse? index...",},
    {"configure", 2, (Blt_Operation)ConfigureOp, 2, 0, "?option value?...",},
    {"curselection", 2, (Blt_Operation)CurselectionOp, 2, 2, "",},
    {"delete", 1, (Blt_Operation)DeleteOp, 2, 0, "?-recurse? index ?index...?",},
    {"entry", 1, (Blt_Operation)EntryOp, 2, 0, "oper args",},
    {"find", 2, (Blt_Operation)FindOp, 2, 0, "?flags...? ?firstIndex lastIndex?",},
    {"focus", 2, (Blt_Operation)FocusOp, 3, 3, "index",},
    {"get", 1, (Blt_Operation)GetOp, 2, 0, "?-full? index ?index...?",},
    {"hide", 1, (Blt_Operation)HideOp, 2, 0, "?-exact? ?-glob? ?-regexp? ?-nonmatching? ?-name string? ?-full string? ?-data string? ?--? ?index...?",},
    {"index", 3, (Blt_Operation)IndexOp, 3, 5, "?-at index? string",},
    {"insert", 3, (Blt_Operation)InsertOp, 3, 0,
	"?-at index? position label ?label...? ?option value?",},
    {"move", 1, (Blt_Operation)MoveOp, 5, 5, "index into|before|after index",},
    {"nearest", 1, (Blt_Operation)NearestOp, 4, 5, "x y ?varName?",},
    {"open", 1, (Blt_Operation)OpenOp, 2, 0, "?-recurse? index...",},
    {"range", 1, (Blt_Operation)RangeOp, 4, 5, "?-open? firstIndex lastIndex",},
    {"scan", 2, (Blt_Operation)ScanOp, 5, 5, "dragto|mark x y",},
    {"see", 3, (Blt_Operation)SeeOp, 3, 0, "?-anchor anchor? index",},
    {"selection", 3, (Blt_Operation)SelectionOp, 2, 0, "oper args",},
    {"show", 2, (Blt_Operation)ShowOp, 2, 0, "?-exact? ?-glob? ?-regexp? ?-nonmatching? ?-name string? ?-full string? ?-data string? ?--? ?index...?",},
    {"sort", 2, (Blt_Operation)SortOp, 3, 0, "?-recurse? ?-command string? index...",},
    {"toggle", 1, (Blt_Operation)ToggleOp, 3, 3, "index",},
    {"xview", 1, (Blt_Operation)XViewOp, 2, 5,
	"?moveto fract? ?scroll number what?",},
    {"yview", 1, (Blt_Operation)YViewOp, 2, 5,
	"?moveto fract? ?scroll number what?",},
};

static int numSpecs = sizeof(operSpecs) / sizeof(Blt_OpSpec);

static int
HierboxInstCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Information about the widget. */
    Tcl_Interp *interp;		/* Interpreter to report errors back to. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Vector of argument strings. */
{
    Blt_Operation proc;
    Hierbox *hierPtr = (Hierbox *) clientData;
    int result;

    proc = Blt_GetOperation(interp, numSpecs, operSpecs,
	BLT_OPER_ARG1, argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    Tcl_Preserve((ClientData)hierPtr);
    result = (*proc) (hierPtr, interp, argc, argv);
    Tcl_Release((ClientData)hierPtr);
    return result;
}

int
Blt_HierboxInit(interp)
    Tcl_Interp *interp;
{
    Tk_Window tkwin;
    static Blt_CmdSpec cmdSpec =
    {
	"hierbox", HierboxCmd,
    };

    if (Blt_InitCmd(interp, "blt", &cmdSpec) == NULL) {
	return TCL_ERROR;
    }
    tkwin = Tk_MainWindow(interp);
    Tk_DefineBitmap(interp, Tk_GetUid("HierboxFolder"), (char *)folderBits,
	DEF_ICON_WIDTH, DEF_ICON_HEIGHT);
    Tk_DefineBitmap(interp, Tk_GetUid("HierboxFolderMask"),
	(char *)folderMaskBits, DEF_ICON_WIDTH, DEF_ICON_HEIGHT);
    iconBitmap = Tk_GetBitmap(interp, tkwin, Tk_GetUid("HierboxFolder"));
    iconMask = Tk_GetBitmap(interp, tkwin, Tk_GetUid("HierboxFolderMask"));
    iconColor = Tk_GetColor(interp, tkwin, Tk_GetUid("yellow"));

    return TCL_OK;
}

#endif /* NO_HIERBOX */
