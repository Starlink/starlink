/*	@(#)canvas.h 10.8 88/02/08 SMI	*/

/*
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

#ifndef canvas_DEFINED
#define canvas_DEFINED

#include <sunwindow/attr.h>

typedef caddr_t	Canvas;

#define	CANVAS_ATTR(type, ordinal)	ATTR(ATTR_PKG_CANVAS, type, ordinal)

typedef enum {
    CANVAS_PIXWIN		= CANVAS_ATTR(ATTR_PIXWIN_PTR, 1),
    CANVAS_LEFT			= CANVAS_ATTR(ATTR_X, 2),
    CANVAS_TOP			= CANVAS_ATTR(ATTR_Y, 4),
    CANVAS_WIDTH		= CANVAS_ATTR(ATTR_X, 5),
    CANVAS_HEIGHT		= CANVAS_ATTR(ATTR_Y, 10),
    CANVAS_DEPTH		= CANVAS_ATTR(ATTR_INT, 15),
    CANVAS_MARGIN		= CANVAS_ATTR(ATTR_INT, 17),
    CANVAS_RETAINED		= CANVAS_ATTR(ATTR_BOOLEAN, 18),
    CANVAS_FIXED_IMAGE		= CANVAS_ATTR(ATTR_BOOLEAN, 25),
    CANVAS_REPAINT_PROC		= CANVAS_ATTR(ATTR_FUNCTION_PTR, 30),
    CANVAS_RESIZE_PROC		= CANVAS_ATTR(ATTR_FUNCTION_PTR, 32),
    CANVAS_AUTO_CLEAR		= CANVAS_ATTR(ATTR_BOOLEAN, 35),
    CANVAS_AUTO_EXPAND		= CANVAS_ATTR(ATTR_BOOLEAN, 40),
    CANVAS_AUTO_SHRINK		= CANVAS_ATTR(ATTR_BOOLEAN, 45),
    CANVAS_FAST_MONO		= CANVAS_ATTR(ATTR_BOOLEAN, 50),
} Canvas_attribute;

/* useful macros
 */
#define	canvas_pixwin(canvas)	(Pixwin *) (LINT_CAST(window_get(canvas, CANVAS_PIXWIN)))

/* useful functions
 */
extern Event			*canvas_event();
extern Event			*canvas_window_event();

/* canvas creation routine */
#define	CANVAS			canvas_window_object
extern caddr_t			canvas_window_object();

/* canvas window type */
#define	CANVAS_TYPE	ATTR_PKG_CANVAS

#endif not canvas_DEFINED
