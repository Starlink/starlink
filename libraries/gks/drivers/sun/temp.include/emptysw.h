/*	@(#)emptysw.h 10.4 88/02/08 SMI	*/

/*
 * Copyright (c) 1983 by Sun Microsystems, Inc.
 */

/*
 * A empty subwindow is simply a place holding window that simply
 * paints gray if it is the current owner of the window.
 */

struct emptysubwindow {
	int	em_windowfd;
	struct	pixwin *em_pixwin;
};

typedef struct emptysubwindow Emptysw;

extern	Emptysw *esw_begin();

extern	Emptysw *esw_init();
extern	int esw_handlesigwinch();
extern	struct toolsw *esw_createtoolsubwindow();

#ifdef	cplus
/*
 * C Library routines specifically related to empty subwindow functions.
 */

/*
 * Empty subwindow operations.
 */
/* Create a notifier-based empty
 * subwindow.
 */
Emptysw *esw_begin(Tool *tool, char *name, short width, height);


Emptysw *esw_init(int windowfd);
void	esw_handlesigwinch(struct emptysubwindow *esw);
void	esw_done(struct emptysubwindow *esw);

/*
 * Utility for initializing toolsw as empty subwindow in suntool environment
 */
struct	toolsw *esw_createtoolsubwindow(
	Tool *tool, char *name, short width, height);
#endif

