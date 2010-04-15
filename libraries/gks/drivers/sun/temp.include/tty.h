/*	@(#)tty.h 10.9 88/02/08 SMI	*/

/***********************************************************************/
/*                            tty.h                                    */
/*              Copyright (c) 1985 by Sun Microsystems, Inc.           */
/***********************************************************************/

#ifndef tty_DEFINED
#define tty_DEFINED

#include <sunwindow/attr.h>

/***********************************************************************/
/*                      Attributes                                     */
/***********************************************************************/

#define TTY_ATTR(type, ordinal)       ATTR(ATTR_PKG_TTY, type, ordinal)
#define TTY_ATTR_LIST(ltype, type, ordinal) \
        TTY_ATTR(ATTR_LIST_INLINE((ltype), (type)), (ordinal))

typedef enum {

   TTY_ARGV			= TTY_ATTR(ATTR_OPAQUE, 5),
   TTY_CONSOLE			= TTY_ATTR(ATTR_BOOLEAN, 1),
   TTY_PAGE_MODE		= TTY_ATTR(ATTR_BOOLEAN, 2),
   TTY_QUIT_ON_CHILD_DEATH	= TTY_ATTR(ATTR_BOOLEAN, 3),
   TTY_SAVE_PARAMETERS		= TTY_ATTR(ATTR_NO_VALUE, 4),
   TTY_BOLDSTYLE		= TTY_ATTR(ATTR_INT, 6),
   TTY_BOLDSTYLE_NAME		= TTY_ATTR(ATTR_STRING, 7),
   TTY_INVERSE_MODE		= TTY_ATTR(ATTR_INT, 15),
   TTY_SHOW			= TTY_ATTR(ATTR_BOOLEAN, 8),
   TTY_TTY_FD			= TTY_ATTR(ATTR_INT, 9),
   TTY_UNDERLINE_MODE		= TTY_ATTR(ATTR_INT, 20),
   TTY_PID	/*get only*/	= TTY_ATTR(ATTR_INT, 10),
} Tty_attribute;

#define tty_attr_next(attr) (Tty_attribute *)attr_next((caddr_t *)attr)

/***********************************************************************/
/* misc                                                                */
/***********************************************************************/

typedef caddr_t Tty;
#define	TTY_ARGV_DO_NOT_FORK	-1
#define	TTY_INFINITY		((long)0x77777777)

/***********************************************************************/
/* public external functions                                           */
/***********************************************************************/

extern int tty_start();
extern int tty_save_parameters();

/***********************************************************************/
/* external functions private to implementation                        */
/***********************************************************************/

#define TTY tty_window_object
extern caddr_t tty_window_object();

#define TERM term_window_object
extern caddr_t term_window_object();

#define	TTY_TYPE	ATTR_PKG_TTY

/***********************************************************************/
/* escap sequences handled by TTY subwindows                           */
/***********************************************************************/

/*
        \E[1t           - open
        \E[2t           - close (become iconic)
        \E[3t           - move, with interactive feedback
        \E[3;TOP;LEFTt  - move, TOP LEFT in pixels
        \E[4t           - stretch, with interactive feedback
        \E[4;ROWS;COLSt - stretch, ROWS COLS in pixels
        \E[5t           - top (expose)
        \E[6t           - bottom (hide)
        \E[7t           - refresh
        \E[8;ROWS;COLSt - stretch, ROWS COLS in characters
        \E[11t          - report open or iconic, sends \E[1t or \E[2t
        \E[13t          - report position, sends \E[3;TOP;LEFTt
        \E[14t          - report size in pixels, sends \E[8;ROWS;COLSt
        \E[18t          - report size in chars, sends \E[4;ROWS;COLSt
        \E[20t          - report icon label, sends \E]Llabel\E\
        \E[21t          - report tool label, sends \E]llabel\E\
        \E]l<text>\E\   - set tool label to <text>
        \E]I<file>\E\   - set icon file to <file>
        \E]L<label>\E\  - set icon label to <label>
*/
#endif ~tty_DEFINED
