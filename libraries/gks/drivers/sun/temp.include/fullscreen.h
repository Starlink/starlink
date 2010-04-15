/*	@(#)fullscreen.h 10.4 88/02/08 SMI	*/

/*
 * Copyright (c) 1983 by Sun Microsystems, Inc.
 */

/*
 * Sometimes one has to violate window boundaries in order to provide
 * certain kinds of feedback to the user.  Popup menus and window management
 * are examples of the kinds of operations that need to do this.
 * This interface provides a mechanism for doing this.
 *
 * The coordinate space of full screen access is the same as the window
 * that created the full screen object.  Thus, pixwin accesses are not
 * necessarily done in the screen's coordinate space.
 * Also, the screen rect associated with the full screen object are in
 * the window's coordinate space.
 *
 * The original cursor and input mask are cached and later restored when the
 * the full screen access object is destroyed.
 *
 * By definition there should only be one instance of window boundary violating
 * per process at any one time
 * (sunwindow will keep different processes straight) thus the existence of
 * fsglobal global variable below.  However, clean programmers will use the
 * handle passed back from fullscreen_init instead of fsglobal.
 */

struct fullscreen {
	int	fs_windowfd;
	struct	rect fs_screenrect;
	struct	pixwin *fs_pixwin;
	struct	cursor fs_cachedcursor;
	struct	inputmask fs_cachedim;	/* Pick mask */
	int	fs_cachedinputnext;
	struct	inputmask fs_cachedkbdim; /* Kbd mask */
};

extern	struct fullscreen *fullscreen_init(), *fsglobal;

#ifdef	cplus
/*
 * C Library routines specifically related to full screen functions.
 */

/*
 * Full screen operations.
 */
struct	fullscreen *fullscreen_init(int windowfd);
void	fullscreen_destroy(struct fullscreen *fs);
#endif

