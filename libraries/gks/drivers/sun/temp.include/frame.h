/*	@(#)frame.h 10.14 88/02/08 SMI	*/

/***********************************************************************/
/*	                      frame.h			               */
/*          Copyright (c) 1985 by Sun Microsystems, Inc.               */
/***********************************************************************/

#ifndef frame_DEFINED
#define frame_DEFINED

#include <sunwindow/attr.h>

/***********************************************************************/
/*      opaque types for frames and useful constants                   */
/***********************************************************************/

#define FRAME_TYPE	ATTR_PKG_FRAME

typedef	caddr_t 	Frame;
#define FRAME		frame_window_object
#define	ROOT_FRAME	((Frame)0)

extern caddr_t frame_window_object();

/***********************************************************************/
/*	        	Attributes 				       */
/***********************************************************************/

#define	FRAME_ATTR(type, ordinal)	ATTR(ATTR_PKG_FRAME, type, ordinal)
#define	TFRAME_ATTR(type, ordinal)	ATTR(ATTR_PKG_TOOL, type, ordinal)

#define FRAME_ATTR_LIST(ltype, type, ordinal) \
	  FRAME_ATTR(ATTR_LIST_INLINE((ltype), (type)), (ordinal))

#define	FRAME_ATTR_UNUSED_FIRST		 0  /* Reserved for future use */
#define	FRAME_ATTR_UNUSED_LAST		31


typedef enum {

/* -----------------  COMPATIBLE with old tool attrs  --------------------- */
/*  WARNING:  These attributes EXACTLY match the win attrs in tool.h        */
/*  		Don't renumber them!				            */

    FRAME_CLOSED		= TFRAME_ATTR(ATTR_BOOLEAN, 7),
    FRAME_INHERIT_COLORS	= TFRAME_ATTR(ATTR_BOOLEAN, 8),
    FRAME_SHOW_LABEL		= TFRAME_ATTR(ATTR_BOOLEAN, 11),
    FRAME_SUBWINDOWS_ADJUSTABLE	= TFRAME_ATTR(ATTR_BOOLEAN, 12),
    FRAME_LABEL			= TFRAME_ATTR(ATTR_STRING, 13),
    FRAME_FOREGROUND_COLOR	= TFRAME_ATTR(ATTR_SINGLE_COLOR_PTR, 14),
    FRAME_BACKGROUND_COLOR	= TFRAME_ATTR(ATTR_SINGLE_COLOR_PTR, 15),
    FRAME_ICON			= TFRAME_ATTR(ATTR_ICON_PTR, 16),
    FRAME_CLOSED_X		= TFRAME_ATTR(ATTR_X, 17),
    FRAME_CLOSED_Y		= TFRAME_ATTR(ATTR_Y, 18),


/* --------------  NEW attr ordinals must be greater than 21  ------------ */

    FRAME_CLOSED_RECT		= FRAME_ATTR(ATTR_RECT_PTR, 22),
    FRAME_CURRENT_RECT		= FRAME_ATTR(ATTR_RECT_PTR, 122),
    FRAME_OPEN_RECT		= FRAME_ATTR(ATTR_RECT_PTR, 23),

    FRAME_ARGS			= FRAME_ATTR(ATTR_INT_PAIR, 24), /* Set only */
    FRAME_ARGC_PTR_ARGV		= FRAME_ATTR(ATTR_INT_PAIR, 25), /* Set only */
    FRAME_CMDLINE_HELP_PROC	= FRAME_ATTR(ATTR_FUNCTION_PTR, 125),/* " " */

    FRAME_BORDER_STYLE		= FRAME_ATTR(ATTR_ENUM, 28),     /* Get only */
    FRAME_EMBOLDEN_LABEL	= FRAME_ATTR(ATTR_BOOLEAN, 128),

    FRAME_NTH_WINDOW		= FRAME_ATTR(ATTR_INT, 29),   /* Zero origin */
    FRAME_NTH_SUBWINDOW		= FRAME_ATTR(ATTR_INT, 30),
    FRAME_NTH_SUBFRAME		= FRAME_ATTR(ATTR_INT, 31),

    FRAME_DONE_PROC		= FRAME_ATTR(ATTR_FUNCTION_PTR, 40),
    FRAME_DEFAULT_DONE_PROC	= FRAME_ATTR(ATTR_FUNCTION_PTR, 41),

    FRAME_NO_CONFIRM		= FRAME_ATTR(ATTR_BOOLEAN, 42),
    FRAME_SHADOW		= FRAME_ATTR(ATTR_INT, 43),
    FRAME_SHOW_SHADOW		= FRAME_ATTR(ATTR_BOOLEAN, 44),
    FRAME_PROPS_ACTION_PROC	= FRAME_ATTR(ATTR_FUNCTION_PTR, 45),
    FRAME_PROPS_ACTIVE		= FRAME_ATTR(ATTR_INT, 46),
    FRAME_NULL_ATTR		= FRAME_ATTR(ATTR_NO_VALUE, 99),    /* No-op */

} Frame_attribute;


typedef enum {			/* values for FRAME_BORDER_STYLE */

    FRAME_DOUBLE,
    FRAME_SHADOWED

} Frame_border_style;


/***********************************************************************/
/* macros                                                              */
/***********************************************************************/

#define frame_fit_all(frame) \
{ \
    Window win; \
    int n = 0; \
    while (win = window_get(frame, FRAME_NTH_SUBWINDOW, n++, 0)) \
	window_fit(win); \
    window_fit(frame); \
}

#define frame_done_proc(frame) \
   (((void (*)())(LINT_CAST(window_get(frame, FRAME_DONE_PROC))))(frame))


#define frame_default_done_proc(frame) \
   (((void (*)())(LINT_CAST(window_get(frame, FRAME_DEFAULT_PROC))))(frame))

/***********************************************************************/
#endif ~frame_DEFINED
