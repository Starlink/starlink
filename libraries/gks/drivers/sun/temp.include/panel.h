/*    @(#)panel.h 10.13 88/02/08 SMI      */


/***********************************************************************/
/*	                      panel.h  				       */
/*          Copyright (c) 1985 by Sun Microsystems, Inc.               */
/***********************************************************************/

#ifndef panel_DEFINED
#define panel_DEFINED

#include <sunwindow/attr.h>

/***********************************************************************/
/*	        	Attributes 				       */
/***********************************************************************/

#define	PANEL_ATTR(type, ordinal)	ATTR(ATTR_PKG_PANEL, type, ordinal)

/* panel specific attribute types */
#define	PANEL_INDEX_STRING		ATTR_TYPE(ATTR_BASE_UNUSED_FIRST, 2)
#define	PANEL_INDEX_PIXRECT_PTR		ATTR_TYPE(ATTR_BASE_UNUSED_FIRST + 1, 2)
#define	PANEL_INDEX_BOOLEAN		ATTR_TYPE(ATTR_BASE_UNUSED_FIRST + 2, 2)
#define	PANEL_INDEX_FONT		ATTR_TYPE(ATTR_BASE_UNUSED_FIRST + 3, 2)


/* for compatability with previous releases */
#define	PANEL_ATTRIBUTE_LIST		ATTR_LIST

/* reserved for client use */
#define	PANEL_ATTR_UNUSED_FIRST		0
#define	PANEL_ATTR_UNUSED_LAST		31

/* new attributes should start at PANEL_LAST_ATTR + 1 */
#ifdef ecd.color
#define	PANEL_LAST_ATTR			127
#else
#define	PANEL_LAST_ATTR			125
#endif

typedef enum {
    /* x-coordinates */
    PANEL_ITEM_X		= PANEL_ATTR(ATTR_X, 32),
    PANEL_ITEM_X_GAP		= PANEL_ATTR(ATTR_X, 33),
    PANEL_LABEL_X		= PANEL_ATTR(ATTR_X, 34),
    PANEL_VALUE_X		= PANEL_ATTR(ATTR_X, 35),
    PANEL_SLIDER_WIDTH		= PANEL_ATTR(ATTR_X, 36),
    PANEL_WIDTH			= PANEL_ATTR(ATTR_X, 37),

    /* indexed x-coordinates */
    PANEL_CHOICE_X		= PANEL_ATTR(ATTR_INDEX_X, 38),
    PANEL_MARK_X		= PANEL_ATTR(ATTR_INDEX_X, 39),

    /* lists of x-coordinates */
    PANEL_CHOICE_XS	= PANEL_ATTR(ATTR_LIST_INLINE(ATTR_NULL, ATTR_X), 40),
    PANEL_MARK_XS	= PANEL_ATTR(ATTR_LIST_INLINE(ATTR_NULL, ATTR_X), 41),

   /* y-coordinates */
    PANEL_ITEM_Y		= PANEL_ATTR(ATTR_Y, 42),
    PANEL_ITEM_Y_GAP		= PANEL_ATTR(ATTR_Y, 43),
    PANEL_LABEL_Y		= PANEL_ATTR(ATTR_Y, 44),
    PANEL_VALUE_Y		= PANEL_ATTR(ATTR_Y, 45),
    PANEL_HEIGHT		= PANEL_ATTR(ATTR_Y, 46),

    /* indexed y-coordinates */
    PANEL_CHOICE_Y		= PANEL_ATTR(ATTR_INDEX_Y, 47),
    PANEL_MARK_Y		= PANEL_ATTR(ATTR_INDEX_Y, 48),

    /* lists of y-coordinates */
    PANEL_CHOICE_YS	= PANEL_ATTR(ATTR_LIST_INLINE(ATTR_NULL, ATTR_Y), 49),
    PANEL_MARK_YS	= PANEL_ATTR(ATTR_LIST_INLINE(ATTR_NULL, ATTR_Y), 50),

    /* integers */
    PANEL_VALUE			= PANEL_ATTR(ATTR_INT,  51),
    PANEL_LABEL_DISPLAY_LENGTH	= PANEL_ATTR(ATTR_INT,  52),
    PANEL_VALUE_DISPLAY_LENGTH	= PANEL_ATTR(ATTR_INT,  53),
    PANEL_VALUE_STORED_LENGTH	= PANEL_ATTR(ATTR_INT,  54),
    PANEL_MIN_VALUE		= PANEL_ATTR(ATTR_INT,  55),
    PANEL_MAX_VALUE		= PANEL_ATTR(ATTR_INT,  56),
    PANEL_CHOICE_OFFSET		= PANEL_ATTR(ATTR_INT,  57),
    PANEL_TIMER_SECS		= PANEL_ATTR(ATTR_INT, 58),
    PANEL_TIMER_USECS		= PANEL_ATTR(ATTR_INT, 59),

    /* lists of integers */
    PANEL_MENU_CHOICE_VALUES	=
        PANEL_ATTR(ATTR_LIST_INLINE(ATTR_NULL, ATTR_INT), 60),

    /* booleans */
    PANEL_VALUE_UNDERLINED	= PANEL_ATTR(ATTR_BOOLEAN, 61),
    PANEL_ITEM_BOXED		= PANEL_ATTR(ATTR_BOOLEAN, 62),
    PANEL_LABEL_BOLD		= PANEL_ATTR(ATTR_BOOLEAN, 63),
    PANEL_CHOICES_BOLD		= PANEL_ATTR(ATTR_BOOLEAN, 64),
    PANEL_SHOW_ITEM		= PANEL_ATTR(ATTR_BOOLEAN, 65),
    PANEL_SHOW_VALUE		= PANEL_ATTR(ATTR_BOOLEAN, 66),
    PANEL_SHOW_RANGE		= PANEL_ATTR(ATTR_BOOLEAN, 67),
    PANEL_SHOW_MENU		= PANEL_ATTR(ATTR_BOOLEAN, 68),
    PANEL_SHOW_MENU_MARK	= PANEL_ATTR(ATTR_BOOLEAN, 69),
    PANEL_CHOOSE_ONE		= PANEL_ATTR(ATTR_BOOLEAN, 70),
    PANEL_ADJUSTABLE		= PANEL_ATTR(ATTR_BOOLEAN, 71),
    PANEL_BLINK_CARET		= PANEL_ATTR(ATTR_BOOLEAN, 72),
    PANEL_ACCEPT_KEYSTROKE	= PANEL_ATTR(ATTR_BOOLEAN, 73),
    PANEL_LABEL_SHADED		= PANEL_ATTR(ATTR_BOOLEAN, 121),
    PANEL_READ_ONLY             = PANEL_ATTR(ATTR_BOOLEAN, 124),
    PANEL_BOXED                 = PANEL_ATTR(ATTR_BOOLEAN, 125),


    /* indexed booleans */
    PANEL_TOGGLE_VALUE		= PANEL_ATTR(PANEL_INDEX_BOOLEAN, 120),

    /* enums */
    PANEL_NOTIFY_LEVEL		= PANEL_ATTR(ATTR_ENUM, 74),
    PANEL_DISPLAY_LEVEL		= PANEL_ATTR(ATTR_ENUM, 75),
    PANEL_LAYOUT		= PANEL_ATTR(ATTR_ENUM, 76),
    PANEL_FEEDBACK		= PANEL_ATTR(ATTR_ENUM, 77),
    PANEL_PAINT			= PANEL_ATTR(ATTR_ENUM, 78),
    PANEL_MOUSE_STATE		= PANEL_ATTR(ATTR_ENUM, 79),

    /* characters */
    PANEL_MASK_CHAR		= PANEL_ATTR(ATTR_CHAR, 80),

    /* strings */
    PANEL_LABEL_STRING		= PANEL_ATTR(ATTR_STRING, 81),
    PANEL_VALUE_STRING		= PANEL_ATTR(ATTR_STRING, 82),
    PANEL_NOTIFY_STRING		= PANEL_ATTR(ATTR_STRING, 83),
    PANEL_MENU_TITLE_STRING	= PANEL_ATTR(ATTR_STRING, 84),
    PANEL_NAME			= PANEL_ATTR(ATTR_STRING, 85),

    /* lists of strings */
    PANEL_CHOICE_STRINGS	=
        PANEL_ATTR(ATTR_LIST_INLINE(ATTR_NULL, ATTR_STRING), 86),
    PANEL_MENU_CHOICE_STRINGS	=
        PANEL_ATTR(ATTR_LIST_INLINE(ATTR_NULL, ATTR_STRING), 87),

    /* indexed strings */
    PANEL_CHOICE_STRING		= PANEL_ATTR(PANEL_INDEX_STRING, 88),

    /* indexed font */
    PANEL_CHOICE_FONT		= PANEL_ATTR(PANEL_INDEX_FONT, 123),

    /* pixrect pointers */
    PANEL_LABEL_IMAGE		= PANEL_ATTR(ATTR_PIXRECT_PTR, 89),
    PANEL_MENU_TITLE_IMAGE	= PANEL_ATTR(ATTR_PIXRECT_PTR, 90),
    PANEL_MENU_MARK_IMAGE	= PANEL_ATTR(ATTR_PIXRECT_PTR, 91),
    PANEL_MENU_NOMARK_IMAGE	= PANEL_ATTR(ATTR_PIXRECT_PTR, 92),
    PANEL_TYPE_IMAGE		= PANEL_ATTR(ATTR_PIXRECT_PTR, 93),

    /* lists of pixrect pointers */
    PANEL_CHOICE_IMAGES		=
        PANEL_ATTR(ATTR_LIST_INLINE(ATTR_NULL, ATTR_PIXRECT_PTR), 94),
    PANEL_MENU_CHOICE_IMAGES	=
        PANEL_ATTR(ATTR_LIST_INLINE(ATTR_NULL, ATTR_PIXRECT_PTR), 95),
    PANEL_MARK_IMAGES		=
        PANEL_ATTR(ATTR_LIST_INLINE(ATTR_NULL, ATTR_PIXRECT_PTR), 96),
    PANEL_NOMARK_IMAGES		=
        PANEL_ATTR(ATTR_LIST_INLINE(ATTR_NULL, ATTR_PIXRECT_PTR), 97),

    /* indexed pixrect pointers */
    PANEL_CHOICE_IMAGE		= PANEL_ATTR(PANEL_INDEX_PIXRECT_PTR, 98),
    PANEL_MARK_IMAGE		= PANEL_ATTR(PANEL_INDEX_PIXRECT_PTR, 99),
    PANEL_NOMARK_IMAGE		= PANEL_ATTR(PANEL_INDEX_PIXRECT_PTR, 100),

    /* pixfont pointers */
    PANEL_LABEL_FONT		= PANEL_ATTR(ATTR_PIXFONT_PTR, 101),
    PANEL_VALUE_FONT		= PANEL_ATTR(ATTR_PIXFONT_PTR, 102),
    PANEL_MENU_TITLE_FONT	= PANEL_ATTR(ATTR_PIXFONT_PTR, 103),
    PANEL_FONT			= PANEL_ATTR(ATTR_PIXFONT_PTR, 104),

    /* lists of fonts */
    PANEL_CHOICE_FONTS		=
        PANEL_ATTR(ATTR_LIST_INLINE(ATTR_NULL, ATTR_PIXFONT_PTR), 105),
    PANEL_MENU_CHOICE_FONTS	=
        PANEL_ATTR(ATTR_LIST_INLINE(ATTR_NULL, ATTR_PIXFONT_PTR), 106),

    /* function pointers */
    PANEL_NOTIFY_PROC		= PANEL_ATTR(ATTR_FUNCTION_PTR, 107),
    PANEL_TIMER_PROC		= PANEL_ATTR(ATTR_FUNCTION_PTR, 108),
    PANEL_EVENT_PROC		= PANEL_ATTR(ATTR_FUNCTION_PTR, 109),
    PANEL_BACKGROUND_PROC	= PANEL_ATTR(ATTR_FUNCTION_PTR, 122),

    /* rect pointers */
    PANEL_ITEM_RECT		= PANEL_ATTR(ATTR_RECT_PTR, 110),

    /* pixwin pointers */
    PANEL_PIXWIN		= PANEL_ATTR(ATTR_PIXWIN_PTR, 111),
#ifdef ecd.color
    /* color panel item attributes */
    PANEL_ITEM_COLOR		= PANEL_ATTR(ATTR_INT,126),
    PANEL_PAINT_PIXWIN		= PANEL_ATTR(ATTR_PIXWIN_PTR,127),
#endif

    /* opaque pointers */
    PANEL_CLIENT_DATA		= PANEL_ATTR(ATTR_OPAQUE, 112),
    PANEL_VERTICAL_SCROLLBAR	= PANEL_ATTR(ATTR_OPAQUE, 113),
    PANEL_HORIZONTAL_SCROLLBAR	= PANEL_ATTR(ATTR_OPAQUE, 114),
    PANEL_CURSOR		= PANEL_ATTR(ATTR_OPAQUE, 115),
    PANEL_CARET_ITEM		= PANEL_ATTR(ATTR_OPAQUE, 116),
    PANEL_FIRST_ITEM		= PANEL_ATTR(ATTR_OPAQUE, 117),
    PANEL_NEXT_ITEM		= PANEL_ATTR(ATTR_OPAQUE, 118),
    PANEL_PARENT_PANEL		= PANEL_ATTR(ATTR_OPAQUE, 119),


} Panel_attribute;

/********* values for LEVEL attrs  *********************************/

typedef enum {

    PANEL_CLEAR,        /* painting */
    PANEL_NO_CLEAR,     /* painting */
    PANEL_NONE,		/* text notify, menu, display, painting */
    PANEL_ALL,		/* text notify, slider notify, display */
    PANEL_NON_PRINTABLE,/* text notify */
    PANEL_SPECIFIED,	/* text notify */
    PANEL_CURRENT,	/* display */
    PANEL_DONE,		/* slider notify */
    PANEL_MARKED,	/* feedback */
    PANEL_VERTICAL,	/* layout */
    PANEL_HORIZONTAL,	/* layout */
    PANEL_INVERTED,	/* feedback */

    /* values returnable by notify routines */
    PANEL_INSERT,
    PANEL_NEXT,
    PANEL_PREVIOUS,

    /* mouse state */
    PANEL_NONE_DOWN,	/* no buttons are down */
    PANEL_LEFT_DOWN,	/* left button is down */
    PANEL_MIDDLE_DOWN,	/* middle button is down */
    PANEL_RIGHT_DOWN,	/* right button is down */
    PANEL_CHORD_DOWN	/* chord of buttons are down */
} Panel_setting;

/***********************************************************************/
/* opaque types for panel subwindows                                    */
/***********************************************************************/

typedef	caddr_t 	Panel;
typedef	caddr_t 	Panel_item;
typedef	caddr_t 	Panel_attribute_value;

/***********************************************************************/
/* external declarations                                               */
/***********************************************************************/

/* for window wrapper */
#define PANEL panel_window_object
#define PANEL_TYPE ATTR_PKG_PANEL
extern caddr_t panel_window_object();

/* for PANEL_CYCLE choice item */
extern struct pixrect	panel_cycle_pr;

/* Panel routines */
extern Panel		 panel_begin();
extern caddr_t		 panel_advance_caret();
extern caddr_t		 panel_backup_caret();

/* Panel & Panel_item routines */
extern Panel_attribute_value panel_get();
extern int               panel_set();
extern                   panel_paint();
extern			 panel_free();
extern			 panel_destroy_item();

/* event mapping routines */
extern			 panel_handle_event();
extern			 panel_default_handle_event();
extern			 panel_cancel();

/* Panel_item action routines */
extern			 panel_begin_preview();
extern			 panel_update_preview();
extern			 panel_accept_preview();
extern			 panel_cancel_preview();
extern			 panel_accept_menu();
extern			 panel_accept_key();

/* utilities */
extern Panel_setting	 panel_text_notify();
extern struct pixrect   *panel_button_image();

/* routines to translate event coordinates
 * Note that struct inputevent * is the same as
 * Event *, this is used for compatability with previous
 * releases.
 */
extern struct inputevent	*panel_window_event();
extern struct inputevent	*panel_event();


/***********************************************************************/
/* external declarations for item creation routines.                   */
/***********************************************************************/

extern Panel_item	panel_create_item();
extern Panel_item	panel_button();
extern Panel_item	panel_choice();
extern Panel_item 	panel_message();
extern Panel_item	panel_slider();
extern Panel_item	panel_text();
extern Panel_item	panel_line();

/***********************************************************************/
/* Values for type argument to panel_create_item()                     */
/***********************************************************************/

#define	PANEL_MESSAGE	panel_message
#define	PANEL_BUTTON	panel_button
#define	PANEL_CHOICE	panel_choice
#define	PANEL_SLIDER	panel_slider
#define	PANEL_TEXT	panel_text
#define	PANEL_TOGGLE	PANEL_CHOICE, PANEL_CHOOSE_ONE, FALSE
#define	PANEL_LINE	panel_line
#define PANEL_CYCLE	PANEL_CHOICE,	\
    			PANEL_MARK_IMAGES,          &panel_cycle_pr, 0, \
    			PANEL_NOMARK_IMAGES,        0,              \
    			PANEL_FEEDBACK,             PANEL_MARKED,   \
    			PANEL_DISPLAY_LEVEL,        PANEL_CURRENT



/***********************************************************************/
/* various utility macros                                              */
/***********************************************************************/

#define	panel_get_value(ip) 		panel_get((ip), PANEL_VALUE)
#define	panel_set_value(ip, val)	panel_set((ip), PANEL_VALUE, (val), 0)
#define	panel_fit_height(panel)	panel_set((panel), PANEL_HEIGHT, \
					PANEL_FIT_ITEMS, 0)
#define	panel_fit_width(panel)	panel_set((panel), PANEL_WIDTH, \
					PANEL_FIT_ITEMS, 0)

#define	panel_each_item(panel, ip)	\
   { Panel_item _next; \
   for ((ip) = (Panel_item) panel_get((panel), PANEL_FIRST_ITEM),\
	_next = (Panel_item) panel_get((ip), PANEL_NEXT_ITEM);\
	(ip); \
	(ip) = _next, _next = (Panel_item) panel_get((ip), PANEL_NEXT_ITEM)) {

#define	panel_end_each	}}

/* functions for backward compatability */
extern struct toolsw	*panel_create();
extern caddr_t          *panel_make_list();

/* PANEL_CU() is provided for backward
 * compatability.
 */
#define	PANEL_CU(n)		(ATTR_COL(n))

#define PANEL_FIT_ITEMS 	(15000) 	/* for panel width and height */

/***********************************************************************/
/* miscellaneous constants                                             */
/***********************************************************************/

#define PANEL_ITEM_X_START    4       /* offset from left edge */
#define PANEL_ITEM_Y_START    4       /* offset from top edge */

/* Panel defined events.
 * These are given to the Panel's or Panel_item's event proc
 */
#define	PANEL_EVENT_FIRST	vuid_first(PANEL_DEVID)		/* 32000 */
#define	PANEL_EVENT_CANCEL	(PANEL_EVENT_FIRST + 0)		/* 32000 */
#define	PANEL_EVENT_MOVE_IN	(PANEL_EVENT_FIRST + 1)		/* 32001 */
#define	PANEL_EVENT_DRAG_IN	(PANEL_EVENT_FIRST + 2)		/* 32002 */

/***********************************************************************/
#endif not panel_DEFINED
