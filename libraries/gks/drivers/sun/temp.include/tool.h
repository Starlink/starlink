/*	@(#)tool.h 10.4 88/02/08 SMI	*/

/*
 * Copyright (c) 1983 by Sun Microsystems, Inc.
 */

#ifndef tool_DEFINED
#define	tool_DEFINED

#include <sunwindow/attr.h>
#include <suntool/tool_struct.h>

#define	TOOL_LAYOUT_LOCK	(TOOL_FIRSTPRIV<<2)
	/* Disable sw layout - must be public for tool_layoutsubwindows */

/*
 * A tool is a stylized window that is used as the conceptual entity
 * that glues subwindows together into a functional package.
 * It is usually a child of the root window, has a name stripe at its top,
 * is responsible for providing borders around its subwindows
 * and displays as an icon when a bit is set in the tool windows user flags.
 *
 * A tool will arrange its subwindows in a tiled configuration whenever
 * its own size changes.  It can also provide a subwindow boundary adjustment
 * interface.
 *
 * The default tool window manager positions iconic tools on byte and n%4 line
 * boundaries so that if you include desktop gray in your icon image
 * the patterns will match.  A default icon is used if none is specified.
 *
 * A typical programs sequence:
 * 1) tool_new returns tool struct with the given attributes.
 *  The attribute list is a series of attribute number/value pairs terminated
 *  by a zero.
 * 2) foo_createtoolsubwindow is called to create each "foo" type of subwindow.
 *  The subwindows are created in a left to right and top to bottom sequence
 *  which gives the tool's layout mechanism a chance to do reasonable default
 *  positioning given the passed in width and height.
 * 3) tool_select is called to do select system calls for all the windows.
 *  The ts_io.tio_selected routine is called when a subwindow is "selected".
 * 6) tool_sigwinch is called when a SIGWINCH is received.
 *  tool_select calls * tl_io.tio_handlesigwinch later.
 *  The default implementation of tl_io.tio_handlesigwinch will resize
 *  the subwindows on a tool size change,
 *  calls each ts_io.tio_handlesigwinch and fixes its own damage.
 * 7) tool_destroy called before exiting process.
 */


#define	WIN_ATTR(type, ordinal)		ATTR(ATTR_PKG_TOOL, type, ordinal)

/*
 * Tool attributes:
 */
typedef enum {
    WIN_COLUMNS		= WIN_ATTR(ATTR_X, 1),
    					/* width in columns of internal area */
					/* u_int */
    WIN_LINES		= WIN_ATTR(ATTR_Y, 2),
    					/* height in lines of internal area */
					/* u_int */
    WIN_WIDTH		= WIN_ATTR(ATTR_X, 3),
    					/* width of normal sized window */
					/* u_int */
    WIN_HEIGHT		= WIN_ATTR(ATTR_Y, 4),
    					/* height of normal sized window */
					/* u_int */
    WIN_LEFT		= WIN_ATTR(ATTR_X, 5),
    					/* x position of normal sized window */
					/* int */
    WIN_TOP		= WIN_ATTR(ATTR_Y, 6),
    					/* y position of normal sized window */
					/* int */
    WIN_ICONIC		= WIN_ATTR(ATTR_BOOLEAN, 7),
    					/* window is iconic */
					/* 0 | 1 */
    WIN_DEFAULT_CMS	= WIN_ATTR(ATTR_BOOLEAN, 8),
    					/* use window's colormap as default */
					/* 0 | 1 */
    WIN_REPAINT_LOCK	= WIN_ATTR(ATTR_BOOLEAN, 9),
    					/* supress repainting */
					/* 0 | 1 */
    WIN_LAYOUT_LOCK	= WIN_ATTR(ATTR_BOOLEAN, 10),
    					/* surpress subwindow layout */
					/* 0 | 1 */
    WIN_NAME_STRIPE	= WIN_ATTR(ATTR_BOOLEAN, 11),
    					/* display name stripe */
					/* 0 | 1 */
    WIN_BOUNDARY_MGR	= WIN_ATTR(ATTR_BOOLEAN, 12),
    					/* enable subwindow boundary mover */
					/* 0 | 1 */
    WIN_LABEL		= WIN_ATTR(ATTR_STRING, 13),
    					/* label in name stripe */
					/* char * */
    WIN_FOREGROUND	= WIN_ATTR(ATTR_SINGLE_COLOR_PTR, 14),
    					/* foreground color (rgb) */
					/* struct singlecolor * (pixrect.h) */
    WIN_BACKGROUND	= WIN_ATTR(ATTR_SINGLE_COLOR_PTR, 15),
    					/* background color (rgb) */
					/* struct singlecolor * (pixrect.h) */
    WIN_ICON		= WIN_ATTR(ATTR_ICON_PTR, 16),
    					/* window's icon */
					/* struct icon * (icon.h) */
    WIN_ICON_LEFT	= WIN_ATTR(ATTR_X, 17),
    					/* icon's x position on desktop */
					/* int */
    WIN_ICON_TOP	= WIN_ATTR(ATTR_Y, 18),
    					/* icon's y position on desktop */
					/* int */
    WIN_ICON_LABEL	= WIN_ATTR(ATTR_STRING, 19),
    					/* icon's label */
					/* char * */
    WIN_ICON_IMAGE	= WIN_ATTR(ATTR_PIXRECT_PTR, 20),
    					/* icon's graphic image */
					/* struct pixrect * (pixrect.h) */
    WIN_ICON_FONT	= WIN_ATTR(ATTR_PIXFONT_PTR, 21)
    					/* icon label's font */
					/* struct pixfont * (pixfont.h) */
} Win_attribute;

#define	WIN_ATTR_LIST	ATTR_LIST	/* attribute list within another */


#endif not tool_DEFINED
