/*	@(#)msgsw.h 10.6 88/02/08 SMI	*/

/*
 * Copyright (c) 1983 by Sun Microsystems, Inc.
 */

/*
 * A msg subwindow is a window in which a simple ascii string can be dispalyed.
 */

/*
 * The contents of struct msgsubwindow is considered private to the
 * implementation of the message subwindow.
 */
typedef	struct msgsubwindow {
	int	msg_windowfd;
	char	*msg_string;
	struct	pixfont *msg_font;
	struct	rect msg_rectcache;
	struct	pixwin *msg_pixwin;
} Msgsw;

#define MSGSW_NULL      ((Msgsw *)0)

extern	Msgsw *msgsw_create();

#ifdef	cplus
/*
 * C Library routines specifically related to msg subwindow functions.
 */

/*
 * Empty subwindow operations.
 */
Msgsw	*msgsw_create(struct tool *tool, char *name, short width, height,
	    char *string, struct pixfont *font);
void	msgsw_setstring(Msgsw *msgsw, char *string);
void	msgsw_display(Msgsw *msgsw);
void	msgsw_done(Msgsw *msgsw);

/*
 * Obsolete (but implemented) operations:
 */
struct	msgsubwindow *msgsw_init(
	    int windowfd, char *string, struct pixfont *font);
struct	toolsw *msgsw_createtoolsubwindow(
	    struct tool *tool, char *name, short width, height,
	    char *string, struct pixfont *font);
void	msgsw_handlesigwinch(struct msgsubwindow *msgsw);
#endif

/*
 * Obsolete (but implemented) operations:
 */
extern	struct msgsubwindow *msgsw_init();
extern	int msgsw_handlesigwinch();
extern	struct toolsw *msgsw_createtoolsubwindow();

