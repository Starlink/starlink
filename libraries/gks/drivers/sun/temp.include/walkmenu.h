/*    @(#)walkmenu.h 10.13 88/02/08 SMI      */

/***********************************************************************/
/*	                      walkmenu.h			       */
/*          Copyright (c) 1985 by Sun Microsystems, Inc.               */
/***********************************************************************/

#ifndef walkmenu_DEFINED
#define walkmenu_DEFINED

#include <sunwindow/attr.h>

/***********************************************************************/
/*	        	Attributes 				       */
/***********************************************************************/

#define	MENU_ATTR(type, ordinal)	ATTR(ATTR_PKG_MENU, type, ordinal)
#define MENU_ATTR_LIST(ltype, type, ordinal) \
	MENU_ATTR(ATTR_LIST_INLINE((ltype), (type)), (ordinal))

/* Fake types -- This should be resolved someday */
#define ATTR_MENU			ATTR_OPAQUE
#define ATTR_IMAGE			ATTR_OPAQUE
#define ATTR_MENU_ITEM			ATTR_OPAQUE
#define ATTR_MENU_ITEM_PAIR		ATTR_INT_PAIR
#define ATTR_STRING_VALUE_PAIR		ATTR_INT_PAIR
#define ATTR_IMAGE_VALUE_PAIR		ATTR_INT_PAIR
#define ATTR_STRING_MENU_PAIR		ATTR_INT_PAIR
#define ATTR_IMAGE_MENU_PAIR		ATTR_INT_PAIR
#define ATTR_STRING_FUNCTION_PAIR	ATTR_INT_PAIR
#define ATTR_IMAGE_FUNCTION_PAIR	ATTR_INT_PAIR
#define ATTR_INT_MENU_ITEM_PAIR		ATTR_INT_PAIR

/* Reserved for future use */
#define	MENU_ATTR_UNUSED_FIRST		 0
#define	MENU_ATTR_UNUSED_LAST		31


typedef enum {

    MENU_ACTION_PROC		= MENU_ATTR(ATTR_FUNCTION_PTR, 32),
    MENU_ACTION			= MENU_ACTION_PROC,
    MENU_ACTION_IMAGE		= MENU_ATTR(ATTR_IMAGE_FUNCTION_PAIR, 33),
    MENU_ACTION_ITEM		= MENU_ATTR(ATTR_STRING_FUNCTION_PAIR, 34),
    MENU_APPEND_ITEM		= MENU_ATTR(ATTR_MENU_ITEM, 35),
    MENU_APPEND			= MENU_APPEND_ITEM,

    MENU_BOXED			= MENU_ATTR(ATTR_BOOLEAN, 56),

    MENU_CENTER			= MENU_ATTR(ATTR_BOOLEAN, 156),
    MENU_CLIENT_DATA		= MENU_ATTR(ATTR_OPAQUE, 36),
    MENU_COLUMN_MAJOR		= MENU_ATTR(ATTR_BOOLEAN, 136),

    MENU_DEFAULT		= MENU_ATTR(ATTR_INT, 37),
    MENU_DEFAULT_ITEM		= MENU_ATTR(ATTR_MENU_ITEM, 38),
    MENU_DEFAULT_IMAGE		= MENU_ATTR(ATTR_IMAGE, 39),
    MENU_DISABLE_ITEM		= MENU_ATTR(ATTR_ENUM, 40), /* NYI */
    MENU_DEFAULT_SELECTION	= MENU_ATTR(ATTR_ENUM, 41),
    MENU_DESCEND_FIRST		= MENU_ATTR(ATTR_BOOLEAN, 141), /* menu_find */

    MENU_FEEDBACK		= MENU_ATTR(ATTR_BOOLEAN, 42),
    MENU_FIRST_EVENT		= MENU_ATTR(ATTR_NO_VALUE, 43), /* Get only */
    MENU_FONT			= MENU_ATTR(ATTR_PIXFONT_PTR, 44),

    MENU_GEN_PULLRIGHT		= MENU_ATTR(ATTR_FUNCTION_PTR, 45),
    MENU_GEN_PULLRIGHT_IMAGE	= MENU_ATTR(ATTR_IMAGE_FUNCTION_PAIR, 46),
    MENU_GEN_PULLRIGHT_ITEM	= MENU_ATTR(ATTR_STRING_FUNCTION_PAIR, 47),
    MENU_GEN_PROC		= MENU_ATTR(ATTR_FUNCTION_PTR, 48),
    MENU_GEN_PROC_IMAGE		= MENU_ATTR(ATTR_IMAGE_FUNCTION_PAIR, 49),
    MENU_GEN_PROC_ITEM		= MENU_ATTR(ATTR_STRING_FUNCTION_PAIR, 50),

    MENU_HEIGHT			= MENU_ATTR(ATTR_INT, 51),

    MENU_IMAGE			= MENU_ATTR(ATTR_IMAGE, 52),
    MENU_IMAGE_ITEM		= MENU_ATTR(ATTR_IMAGE_VALUE_PAIR, 53),
    MENU_IMAGES			= MENU_ATTR_LIST(ATTR_NULL, ATTR_IMAGE, 54),

    MENU_INITIAL_SELECTION	= MENU_ATTR(ATTR_ENUM, 55),
/*     MENU_ACCELERATED_SELECTION	= MENU_INITIAL_SELECTION, /* OBS */
    MENU_INITIAL_SELECTION_SELECTED = MENU_ATTR(ATTR_BOOLEAN, 57),
    MENU_INITIAL_SELECTION_EXPANDED = MENU_ATTR(ATTR_BOOLEAN, 58),
/*    MENU_DISPLAY_ONE_LEVEL	= MENU_ATTR(ATTR_INT, 59), !attr58 /* OBS */

    MENU_INACTIVE		= MENU_ATTR(ATTR_BOOLEAN, 60),
    MENU_INSERT			= MENU_ATTR(ATTR_INT_MENU_ITEM_PAIR, 61),
    MENU_INSERT_ITEM		= MENU_ATTR(ATTR_MENU_ITEM_PAIR, 62),
    MENU_ITEM			= MENU_ATTR_LIST(ATTR_RECURSIVE, ATTR_AV, 63),
    MENU_INVERT			= MENU_ATTR(ATTR_BOOLEAN, 64),

    MENU_JUMP_AFTER_SELECTION	= MENU_ATTR(ATTR_BOOLEAN, 65),
    MENU_JUMP_AFTER_NO_SELECTION = MENU_ATTR(ATTR_BOOLEAN, 66),

    MENU_LAST_EVENT		= MENU_ATTR(ATTR_NO_VALUE, 67), /* Get only */
    MENU_LEFT_MARGIN		= MENU_ATTR(ATTR_INT, 68),
    MENU_LIKE			= MENU_ATTR(ATTR_MENU, 69), /* NYI */

    MENU_MARGIN			= MENU_ATTR(ATTR_INT, 70),
    MENU_MENU			= MENU_ATTR(ATTR_NO_VALUE, 71), /* Type only */

    MENU_NITEMS			= MENU_ATTR(ATTR_NO_VALUE, 72), /* Get only */
    MENU_NOTIFY_PROC		= MENU_ACTION_PROC,
    MENU_NTH_ITEM		= MENU_ATTR(ATTR_INT, 73), /* Origin 1 */
    MENU_NCOLS			= MENU_ATTR(ATTR_INT, 74),
    MENU_NROWS			= MENU_ATTR(ATTR_INT, 75),

    MENU_PARENT			= MENU_ATTR(ATTR_NO_VALUE, 76), /* Get only */
    MENU_PULLRIGHT		= MENU_ATTR(ATTR_MENU, 77),
    MENU_PULLRIGHT_DELTA	= MENU_ATTR(ATTR_INT, 78),
    MENU_PULLRIGHT_IMAGE	= MENU_ATTR(ATTR_IMAGE_MENU_PAIR, 79),
    MENU_PULLRIGHT_ITEM		= MENU_ATTR(ATTR_STRING_MENU_PAIR, 80),

    MENU_RELEASE		= MENU_ATTR(ATTR_NO_VALUE, 81),
    MENU_RELEASE_IMAGE		= MENU_ATTR(ATTR_NO_VALUE, 82),
    MENU_REMOVE			= MENU_ATTR(ATTR_INT, 83),
    MENU_REMOVE_ITEM		= MENU_ATTR(ATTR_MENU_ITEM, 84),
    MENU_REPLACE		= MENU_ATTR(ATTR_INT_MENU_ITEM_PAIR, 85),
    MENU_REPLACE_ITEM		= MENU_ATTR(ATTR_MENU_ITEM_PAIR, 86),
    MENU_RIGHT_MARGIN		= MENU_ATTR(ATTR_INT, 87),

    MENU_SELECTED		= MENU_ATTR(ATTR_INT, 88),
    MENU_SELECTED_ITEM		= MENU_ATTR(ATTR_MENU_ITEM, 89),
    MENU_SHADOW			= MENU_ATTR(ATTR_PIXRECT_PTR, 90),
    MENU_STAY_UP		= MENU_ATTR(ATTR_BOOLEAN, 190),
    MENU_STRING			= MENU_ATTR(ATTR_STRING, 91),
    MENU_STRING_ITEM		= MENU_ATTR(ATTR_STRING_VALUE_PAIR, 92),
    MENU_STRINGS		= MENU_ATTR_LIST(ATTR_NULL, ATTR_STRING, 93),

    MENU_TITLE_ITEM		= MENU_ATTR(ATTR_STRING, 94),
    MENU_TITLE_IMAGE		= MENU_ATTR(ATTR_IMAGE, 95),
    MENU_TYPE			= MENU_ATTR(ATTR_NO_VALUE, 96), /* Get only */

    MENU_VALID_RESULT		= MENU_ATTR(ATTR_INT, 97),
    MENU_VALUE			= MENU_ATTR(ATTR_OPAQUE, 98),/* union */

    MENU_WIDTH			= MENU_ATTR(ATTR_INT, 99),

    MENU_NOP			= MENU_ATTR(ATTR_NO_VALUE, 100),

/* Used in menu_show() as named parameters */

    MENU_BUTTON			= MENU_ATTR(ATTR_INT, 101),          /* Call */
    MENU_FD			= MENU_ATTR(ATTR_INT, 102),          /* Call */
    MENU_IE			= MENU_ATTR(ATTR_INT, 103),          /* Call */
    MENU_POS			= MENU_ATTR(ATTR_INT_PAIR, 104),     /* Call */

/* New Attributes for putting lines in between items */
    MENU_LINE_AFTER_ITEM	= MENU_ATTR(ATTR_INT,105),
    MENU_HORIZONTAL_LINE	= MENU_ATTR(ATTR_INT,106),
    MENU_VERTICAL_LINE		= MENU_ATTR(ATTR_INT,107),
} Menu_attribute;


typedef enum {
    MENU_PROVIDE_FEEDBACK,
    MENU_REMOVE_FEEDBACK
} Menu_feedback;

/* This are obsolete generate names and should be removed someday */
#define MENU_CREATE MENU_DISPLAY
#define MENU_DESTROY MENU_DISPLAY_DONE
#define MENU_NOTIFY_CREATE MENU_NOTIFY
#define MENU_NOTIFY_DESTROY MENU_NOTIFY_DONE

/* New generate names intended to be less confusing */
typedef enum {
    MENU_DISPLAY,
    MENU_DISPLAY_DONE,
    MENU_NOTIFY,
    MENU_NOTIFY_DONE
} Menu_generate;


/***********************************************************************/
/*      opaque types for menus and useful constants                    */
/***********************************************************************/

typedef	caddr_t 	Menu;
typedef	caddr_t 	Menu_item;

#define MENU_BUT  	MS_RIGHT
#define	MENU_NULL	((Menu)0)
#define	MENU_ITEM_NULL	((Menu_item)0)
#define MENU_NO_ITEM	MENU_ITEM_NULL
#define MENU_NO_VALUE	0

#define MENU_DEFAULT_SHADOW &menu_gray75_pr
#define MENU_DEFAULT_NOTIFY_PROC menu_return_value
#define MENU_DEFAULT_PULLRIGHT_DELTA 9999

/***********************************************************************/
/*      external declarations                                          */
/***********************************************************************/

   extern Menu		menu_create();
   extern Menu		menu_create_customizable();
   extern Menu_item	menu_create_item();
   extern caddr_t	menu_get();
   extern int		menu_set();
   extern void		menu_destroy();
   extern void		menu_destroy_with_proc();
   extern caddr_t	menu_show();
   extern caddr_t	menu_show_using_fd();
   extern Menu_item	menu_find();

   extern caddr_t	menu_pullright_return_result();
   extern caddr_t	menu_return_value();
   extern caddr_t	menu_return_item();
   extern caddr_t	menu_return_no_value();
   extern caddr_t	menu_return_no_item();

   extern struct pixrect menu_gray25_pr;
   extern struct pixrect menu_gray50_pr;
   extern struct pixrect menu_gray75_pr;


/***********************************************************************/
#endif ~walkmenu_DEFINED
