/*	@(#)ttytlsw.h 10.4 88/02/08 SMI	*/

/*
 * Copyright (c) 1983 by Sun Microsystems, Inc.
 */

#ifdef	notdef

A tty tool subwindow is a super class of a straight tty subwindow
(see ttysw.h).  This mean that a tty tool subwindow can do what
a straight tty subwindow can plus some more stuff.  In particular,
a tty tool subwindow knows about tool windows and allows
tty based programs to set/get data about the tool window.

The only public access to a tty tool subwindow is its create/destroy
procedures, ttytlsw_create and ttytlsw_done.  Other than
this, the subwindow should be thought of as a straight tty subwindow.

These are the escape sequences that tty programs can send to a
tty tool subwindow (sending them to a straight tty subwindow will
cause them to be ignored):

	\E[1t		- open
	\E[2t		- close (become iconic)
	\E[3t		- move, with interactive feedback
	\E[3;TOP;LEFTt	- move, TOP LEFT in pixels
	\E[4t		- stretch, with interactive feedback
	\E[4;ROWS;COLSt	- stretch, ROWS COLS in pixels
	\E[5t		- top (expose)
	\E[6t		- bottom (hide)
	\E[7t		- refresh
	\E[8;ROWS;COLSt	- stretch, ROWS COLS in characters
	\E[11t		- report open or iconic, sends \E[1t or \E[2t
	\E[13t		- report position, sends \E[3;TOP;LEFTt
	\E[14t		- report size in pixels, sends \E[8;ROWS;COLSt
	\E[18t		- report size in chars, sends \E[4;ROWS;COLSt
	\E[20t		- report icon label, sends \E]Llabel\E\
	\E[21t		- report tool label, sends \E]llabel\E\
	\E]l<text>\E\	- set tool label to <text>
	\E]I<file>\E\	- set icon file to <file>
	\E]L<label>\E\	- set icon label to <label>

#endif

extern	caddr_t ttytlsw_create();

#ifdef	cplus
/*
 * C Library routines specifically related to ttysw subwindow functions.
 */
void	ttytlsw_done(caddr_t ttysw);

/*
 * Utility for creating a ttytlsw.
 */
caddr_t	ttytlsw_create(struct tool *tool, char *name, short width, height);

/* Obsolete (but implemented) routine */
struct	toolsw *ttytlsw_createtoolsubwindow(
	struct tool *tool, char *name, short width, height);
#endif

/* Obsolete (but implemented) routine */
extern	struct toolsw *ttytlsw_createtoolsubwindow();
