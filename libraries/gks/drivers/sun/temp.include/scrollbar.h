/*      @(#)scrollbar.h 10.9 88/02/08 SMI      */

/****************************************************************************/
/*                            scrollbar.h                                   */
/*              Copyright (c) 1985 by Sun Microsystems, Inc.                */
/****************************************************************************/

#ifndef scrollbar_DEFINED
#define scrollbar_DEFINED

#include <sunwindow/attr.h>

/* package generic object for getting default values */
#define SCROLLBAR	-1

/***********************************************************************/
/*  scrollbar psuedo-events                                            */
/***********************************************************************/

#define	SCROLL_FIRST	vuid_first(SCROLL_DEVID)		/* 32256 */
#define	SCROLL_REQUEST  (SCROLL_FIRST+0)			/* 32256 */
#define SCROLL_ENTER	(SCROLL_FIRST+1)			/* 32257 */
#define SCROLL_EXIT	(SCROLL_FIRST+2)			/* 32258 */

/***********************************************************************/
/*  attributes, values for enumerated attributes, typedefs             */
/***********************************************************************/

#define	SCROLL_ATTR(type, ordinal)	ATTR(ATTR_PKG_SCROLLBAR, type, ordinal)

typedef enum {
   SCROLL_NOTIFY_CLIENT		= SCROLL_ATTR(ATTR_OPAQUE,       1),
   SCROLL_OBJECT		= SCROLL_ATTR(ATTR_FUNCTION_PTR, 2),
   SCROLL_MODIFY_PROC		= SCROLL_ATTR(ATTR_FUNCTION_PTR, 3),
   SCROLL_DIRECTION		= SCROLL_ATTR(ATTR_ENUM,         4),
   SCROLL_BAR_DISPLAY_LEVEL	= SCROLL_ATTR(ATTR_ENUM,         5),
   SCROLL_BUBBLE_DISPLAY_LEVEL	= SCROLL_ATTR(ATTR_ENUM,         6),
   SCROLL_FORWARD_CURSOR	= SCROLL_ATTR(ATTR_CURSOR_PTR,   7),
   SCROLL_BACKWARD_CURSOR	= SCROLL_ATTR(ATTR_CURSOR_PTR,   8),
   SCROLL_ABSOLUTE_CURSOR	= SCROLL_ATTR(ATTR_CURSOR_PTR,   9),
   SCROLL_ACTIVE_CURSOR		= SCROLL_ATTR(ATTR_CURSOR_PTR,   10),
   SCROLL_BUBBLE_MARGIN		= SCROLL_ATTR(ATTR_INT,          11),
   SCROLL_BUBBLE_COLOR		= SCROLL_ATTR(ATTR_ENUM,         12),
   SCROLL_BAR_COLOR		= SCROLL_ATTR(ATTR_ENUM,         13),
   SCROLL_BORDER		= SCROLL_ATTR(ATTR_BOOLEAN,      14),
   SCROLL_RECT			= SCROLL_ATTR(ATTR_RECT_PTR,     15),
   SCROLL_TOP			= SCROLL_ATTR(ATTR_Y,            16),
   SCROLL_LEFT			= SCROLL_ATTR(ATTR_X,            17),
   SCROLL_WIDTH			= SCROLL_ATTR(ATTR_X,            18),
   SCROLL_HEIGHT		= SCROLL_ATTR(ATTR_Y,            19),
   SCROLL_PIXWIN		= SCROLL_ATTR(ATTR_PIXWIN_PTR,   20),
   SCROLL_NORMALIZE		= SCROLL_ATTR(ATTR_BOOLEAN,      21),
   SCROLL_MARGIN		= SCROLL_ATTR(ATTR_INT,          22),
   SCROLL_THICKNESS		= SCROLL_ATTR(ATTR_INT,          23),
   SCROLL_PAGE_BUTTONS		= SCROLL_ATTR(ATTR_BOOLEAN,      24),
   SCROLL_PAGE_BUTTON_LENGTH	= SCROLL_ATTR(ATTR_INT,          25),
   SCROLL_REPEAT_TIME		= SCROLL_ATTR(ATTR_INT,          26),
   SCROLL_PLACEMENT		= SCROLL_ATTR(ATTR_ENUM,         27),
   SCROLL_MARK			= SCROLL_ATTR(ATTR_INT,          28),
   SCROLL_VIEW_START		= SCROLL_ATTR(ATTR_INT,          29),
   SCROLL_VIEW_LENGTH		= SCROLL_ATTR(ATTR_INT,          30),
   SCROLL_OBJECT_LENGTH         = SCROLL_ATTR(ATTR_INT,          31),
   SCROLL_ADVANCED_MODE		= SCROLL_ATTR(ATTR_BOOLEAN,      32),
   SCROLL_LINE_HEIGHT		= SCROLL_ATTR(ATTR_INT,          33),
   SCROLL_PAINT_BUTTONS_PROC	= SCROLL_ATTR(ATTR_FUNCTION_PTR, 34),
   SCROLL_PAGE_BUTTON_CURSOR	= SCROLL_ATTR(ATTR_CURSOR_PTR,   35),
   SCROLL_REQUEST_MOTION	= SCROLL_ATTR(ATTR_ENUM,         36),
   SCROLL_REQUEST_OFFSET	= SCROLL_ATTR(ATTR_INT,		 37),
   SCROLL_LAST_VIEW_START	= SCROLL_ATTR(ATTR_INT,          38),
   SCROLL_TO_GRID		= SCROLL_ATTR(ATTR_BOOLEAN,	 39),
   SCROLL_GAP			= SCROLL_ATTR(ATTR_INT,          40),
   SCROLL_END_POINT_AREA	= SCROLL_ATTR(ATTR_INT,          41)

} Scrollbar_attribute;

typedef enum {

SCROLL_ALWAYS,
SCROLL_ACTIVE,
SCROLL_NEVER,
SCROLL_VERTICAL,
SCROLL_HORIZONTAL,
SCROLL_WHITE,
SCROLL_GREY,
SCROLL_BLACK,
SCROLL_MIN,
SCROLL_MAX,
SCROLL_NONE

} Scrollbar_setting;

#define SCROLL_NORTH	SCROLL_MIN
#define SCROLL_SOUTH	SCROLL_MAX
#define SCROLL_WEST	SCROLL_MIN
#define SCROLL_EAST	SCROLL_MAX

typedef caddr_t	Scrollbar;

/****************************************************************************/
/* miscellaneous #defines                                                   */
/****************************************************************************/

#define SCROLL_DEFAULT_WIDTH			14
#define SCROLL_DEFAULT_BUBBLE_MARGIN 		0
#define SCROLL_DEFAULT_NORMALIZE_MARGIN 	4
#define SCROLL_FIT_PIXWIN			-1

/****************************************************************************/
/* types of scrolling motion                                                */
/****************************************************************************/

typedef enum {

SCROLL_ABSOLUTE,	 /* absolute motion */

SCROLL_FORWARD,		 /* forward motions */
SCROLL_MAX_TO_POINT,
SCROLL_PAGE_FORWARD,
SCROLL_LINE_FORWARD,

SCROLL_BACKWARD,	 /* backward motions */
SCROLL_POINT_TO_MAX,
SCROLL_PAGE_BACKWARD,
SCROLL_LINE_BACKWARD

} Scroll_motion;

#define SCROLL_POINT_TO_MIN	SCROLL_FORWARD
#define SCROLL_MIN_TO_POINT	SCROLL_BACKWARD

/****************************************************************************/
/* struct passed with scroll request event                                  */
/****************************************************************************/

typedef struct scroll_request_info {
   Scrollbar		bar;
   long unsigned	bar_length;
   long unsigned	bar_offset;
   caddr_t		object;
   Scrollbar_setting	direction;
   Scroll_motion  	motion;
} Scroll_request_info;

/****************************************************************************/
/* function declarations                                                    */
/****************************************************************************/

extern Scrollbar 	scrollbar_build(); /*old style: should remove */
extern Scrollbar 	scrollbar_create();
extern int		scrollbar_destroy();
extern caddr_t          scrollbar_get();
extern int              scrollbar_set();

extern int 		scrollbar_paint();
extern int 		scrollbar_paint_clear();
extern int 		scrollbar_paint_all();
extern int 		scrollbar_paint_all_clear();
extern int 		scrollbar_paint_bubble();
extern int 		scrollbar_clear_bubble();

extern void 		scrollbar_scroll_to();

#endif
