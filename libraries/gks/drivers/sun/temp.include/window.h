/*	@(#)window.h 10.9 88/02/08 SMI	*/

/****************************************************************************/
/*	                      window.h				            */
/*          Copyright (c) 1985 by Sun Microsystems, Inc.                    */
/****************************************************************************/

#ifndef window_DEFINED
#define window_DEFINED

#ifndef makedev
#include <sys/types.h>
#endif
#include <sunwindow/attr.h>


/****************************************************************************/
/*          	opaque types for wins and useful constants                  */
/****************************************************************************/

#define WINDOW_TYPE	ATTR_PKG_WIN

typedef	caddr_t 	Window;
typedef	Attr_pkg 	Window_type;

#define WIN_EXTEND_TO_EDGE -1


/****************************************************************************/
/*	        		Attributes 			            */
/****************************************************************************/

#define	WIN_ATTR(type, ordinal)	ATTR(ATTR_PKG_WIN, type, ordinal)
#define WIN_ATTR_LIST(ltype, type, ordinal) \
	WIN_ATTR(ATTR_LIST_INLINE((ltype), (type)), (ordinal))

/* Fake types -- This should be resolved someday */
#define ATTR_EVENT		ATTR_OPAQUE
#define ATTR_IMASK		ATTR_OPAQUE
#define ATTR_WIN		ATTR_OPAQUE

/* Reserved for future use */
#define	WIN_ATTR_UNUSED_FIRST	 0
#define	WIN_ATTR_UNUSED_LAST	31


/* ------------------------  WINDOW ATTRIBUTES  --------------------------- */

typedef enum {

    /*                          CREATION ONLY                               */

    WIN_ERROR_MSG		= WIN_ATTR(ATTR_STRING, 32),

    /*                          POSITION and SIZE		            */

    WIN_X			= WIN_ATTR(ATTR_X, 33),
    WIN_Y			= WIN_ATTR(ATTR_Y, 34),
    WIN_RIGHT_OF		= WIN_ATTR(ATTR_WIN, 35),       /* Set only */
    WIN_BELOW			= WIN_ATTR(ATTR_WIN, 36),       /* Set only */
    WIN_WIDTH			= WIN_ATTR(ATTR_X, 37),
    WIN_HEIGHT			= WIN_ATTR(ATTR_Y, 38),
    WIN_PERCENT_WIDTH		= WIN_ATTR(ATTR_INT, 39),
    WIN_PERCENT_HEIGHT		= WIN_ATTR(ATTR_INT, 40),
    WIN_COLUMNS			= WIN_ATTR(ATTR_INT, 41),
    WIN_ROWS			= WIN_ATTR(ATTR_INT, 42),

    WIN_RECT			= WIN_ATTR(ATTR_RECT_PTR, 43),
    WIN_SCREEN_RECT		= WIN_ATTR(ATTR_NO_VALUE, 44),  /* Get only */

    WIN_FIT_HEIGHT		= WIN_ATTR(ATTR_Y, 45),
    WIN_FIT_WIDTH		= WIN_ATTR(ATTR_X, 46),

    WIN_SHOW			= WIN_ATTR(ATTR_BOOLEAN, 47),

    /*                          OBJECTS ATTACHED to WINDOW		    */

    WIN_MENU			= WIN_ATTR(ATTR_OPAQUE, 48),
    WIN_CURSOR			= WIN_ATTR(ATTR_CURSOR_PTR, 49),
    WIN_HORIZONTAL_SCROLLBAR 	= WIN_ATTR(ATTR_OPAQUE, 50),
    WIN_VERTICAL_SCROLLBAR	= WIN_ATTR(ATTR_OPAQUE, 51),

    /*                          CALLOUT PROCEDURES                          */

    WIN_EVENT_PROC		= WIN_ATTR(ATTR_FUNCTION_PTR, 52),

    /*                          INPUT CONTROL                               */

    WIN_INPUT_DESIGNEE		= WIN_ATTR(ATTR_WIN, 53),	/* Set only */
    WIN_GRAB_ALL_INPUT		= WIN_ATTR(ATTR_BOOLEAN, 54),	/* Set only */

    WIN_KBD_INPUT_MASK		= WIN_ATTR(ATTR_IMASK, 55),
    WIN_CONSUME_KBD_EVENT	= WIN_ATTR(ATTR_EVENT, 56),	/* Set only */
    WIN_IGNORE_KBD_EVENT	= WIN_ATTR(ATTR_EVENT, 57),	/* Set only */
    WIN_CONSUME_KBD_EVENTS	= WIN_ATTR_LIST(ATTR_NULL, ATTR_EVENT, 58),/*S*/
    WIN_IGNORE_KBD_EVENTS	= WIN_ATTR_LIST(ATTR_NULL, ATTR_EVENT, 59),/*S*/

    WIN_PICK_INPUT_MASK		= WIN_ATTR(ATTR_IMASK, 60),
    WIN_CONSUME_PICK_EVENT	= WIN_ATTR(ATTR_EVENT, 61),	/* Set only */
    WIN_IGNORE_PICK_EVENT	= WIN_ATTR(ATTR_EVENT, 62),	/* Set only */
    WIN_CONSUME_PICK_EVENTS	= WIN_ATTR_LIST(ATTR_NULL, ATTR_EVENT, 63),/*S*/
    WIN_IGNORE_PICK_EVENTS	= WIN_ATTR_LIST(ATTR_NULL, ATTR_EVENT, 64),/*S*/

    /*                          LAYOUT within the window (margins, spacing)  */

    WIN_FONT			= WIN_ATTR(ATTR_PIXFONT_PTR, 65),
    WIN_TOP_MARGIN		= WIN_ATTR(ATTR_INT, 66),
    WIN_BOTTOM_MARGIN		= WIN_ATTR(ATTR_INT, 67),
    WIN_LEFT_MARGIN		= WIN_ATTR(ATTR_INT, 68),
    WIN_RIGHT_MARGIN		= WIN_ATTR(ATTR_INT, 69),
    WIN_ROW_HEIGHT		= WIN_ATTR(ATTR_INT, 70),
    WIN_COLUMN_WIDTH		= WIN_ATTR(ATTR_INT, 71),
    WIN_ROW_GAP			= WIN_ATTR(ATTR_INT, 72),
    WIN_COLUMN_GAP		= WIN_ATTR(ATTR_INT, 73),

    /*                          WINDOW INFORMATION			     */

    WIN_TYPE			= WIN_ATTR(ATTR_ENUM, 74),
    WIN_OWNER			= WIN_ATTR(ATTR_OPAQUE, 75),	 /* Get only */
    WIN_NAME			= WIN_ATTR(ATTR_STRING, 76),
    WIN_CLIENT_DATA		= WIN_ATTR(ATTR_OPAQUE, 77),
    WIN_IMPL_DATA		= WIN_ATTR(ATTR_OPAQUE, 177),

    /*                          SYSTEM WINDOW INFO related to windows	     */

    WIN_FD			= WIN_ATTR(ATTR_NO_VALUE, 78),   /* Get only */
    WIN_DEVICE_NAME		= WIN_ATTR(ATTR_NO_VALUE, 79),   /* Get only */
    WIN_DEVICE_NUMBER		= WIN_ATTR(ATTR_NO_VALUE, 80),   /* Get only */
    WIN_PIXWIN			= WIN_ATTR(ATTR_OPAQUE, 81),     /* Get only */

    /*                          MISCELLANEOUS				     */

    WIN_MOUSE_XY		= WIN_ATTR(ATTR_XY, 82),  	 /* Set only */
    WIN_SHOW_UPDATES		= WIN_ATTR(ATTR_BOOLEAN, 83),
/*     WIN_LOCK			= WIN_ATTR(ATTR_BOOLEAN, 84),   /* NYI */

    /*                          ADVANCED features for package implementors   */

    WIN_OBJECT			= WIN_ATTR(ATTR_OPAQUE, 85),

    WIN_GET_PROC		= WIN_ATTR(ATTR_FUNCTION_PTR, 86),
    WIN_SET_PROC		= WIN_ATTR(ATTR_FUNCTION_PTR, 87),
    WIN_PRESET_PROC		= WIN_ATTR(ATTR_FUNCTION_PTR, 88),
    WIN_POSTSET_PROC		= WIN_ATTR(ATTR_FUNCTION_PTR, 89),
    WIN_LAYOUT_PROC		= WIN_ATTR(ATTR_FUNCTION_PTR, 90),

    WIN_NOTIFY_DESTROY_PROC	= WIN_ATTR(ATTR_FUNCTION_PTR, 91),
    WIN_NOTIFY_EVENT_PROC 	= WIN_ATTR(ATTR_FUNCTION_PTR, 92),
    WIN_DEFAULT_EVENT_PROC	= WIN_ATTR(ATTR_FUNCTION_PTR, 93),

    WIN_CREATED			= WIN_ATTR(ATTR_BOOLEAN, 94),   /* Get only */

    WIN_COMPATIBILITY		= WIN_ATTR(ATTR_NO_VALUE, 95),
    WIN_COMPATIBILITY_INFO	= WIN_ATTR(ATTR_INT_PAIR, 96),

    WIN_REGISTER		= WIN_ATTR(ATTR_BOOLEAN, 97),
    WIN_NOTIFY_INFO		= WIN_ATTR(ATTR_INT, 98),

    WIN_NULL_ATTR		= WIN_ATTR(ATTR_NO_VALUE, 99),	    /* No-op */

    /*                          ADDED by th on 10/4/85                       */

    WIN_KBD_FOCUS		= WIN_ATTR(ATTR_BOOLEAN, 100),
    WIN_EVENT_STATE		= WIN_ATTR(ATTR_BOOLEAN, 101),

} Window_attribute;


typedef enum {

    WIN_NULL_VALUE = 0,
    WIN_NO_EVENTS,
    WIN_UP_EVENTS,
    WIN_ASCII_EVENTS,
    WIN_UP_ASCII_EVENTS,
    WIN_MOUSE_BUTTONS,
    WIN_IN_TRANSIT_EVENTS,
    WIN_LEFT_KEYS,
    WIN_TOP_KEYS,
    WIN_RIGHT_KEYS,

    /* semantic event classes */

    WIN_SUNVIEW_FUNCTION_KEYS,
    WIN_EDIT_KEYS,
    WIN_MOTION_KEYS,
    WIN_TEXT_KEYS

} Window_input_event;


typedef enum {

    WIN_CREATE,
    WIN_INSTALL,
    WIN_INSERT,
    WIN_REMOVE,
    WIN_DESTROY,

    WIN_ADJUST_X,
    WIN_ADJUST_RIGHT_OF,
    WIN_ADJUST_Y,
    WIN_ADJUST_BELOW,
    WIN_ADJUST_WIDTH,
    WIN_ADJUST_HEIGHT,
    WIN_ADJUST_RECT,
    WIN_ADJUSTED,

    WIN_GET_X,
    WIN_GET_Y,
    WIN_GET_WIDTH,
    WIN_GET_HEIGHT,
    WIN_GET_RECT,

    WIN_LAYOUT,

} Window_layout_op;


/***********************************************************************/
/*      external declarations                                          */
/***********************************************************************/

extern Window		window_create();
extern caddr_t		window_get();
extern int/*bool*/	window_set();
extern int/*bool*/	window_destroy();
extern int/*bool*/	window_done();
extern void		window_main_loop();
extern int		window_fd();
extern void		window_rc_units_to_pixels();
extern caddr_t		window_loop();
extern void		window_return();

/* added by th on 10/4 */
extern void		window_release_event_lock();
extern void		window_refuse_kbd_focus();
extern int		window_read_event();
extern void		window_bell();


/***********************************************************************/
/* macros                                                              */
/***********************************************************************/

#define window_fit(win) \
   (void)window_set(win, WIN_FIT_HEIGHT, 0, WIN_FIT_WIDTH, 0, 0)


#define window_fit_height(win) \
   (void)window_set(win, WIN_FIT_HEIGHT, 0, 0)


#define window_fit_width(win) \
   (void)window_set(win, WIN_FIT_WIDTH, 0, 0)


#define window_event_proc(win, event, arg) \
   (((int (*)())(LINT_CAST(window_get(win, WIN_EVENT_PROC))))(win, event, arg))


#define window_default_event_proc(win, event, arg) \
   (((int (*)())(LINT_CAST(window_get(win, WIN_DEFAULT_EVENT_PROC))))(win, event, arg))

/***********************************************************************/

#ifndef TRUE
#define	TRUE	1
#endif
#ifndef FALSE
#define FALSE	0
#endif
#ifndef NULL
#define NULL	0
#endif

#endif ~window_DEFINED
