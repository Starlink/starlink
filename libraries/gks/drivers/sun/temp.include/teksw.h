/*	@(#)teksw.h 10.5 88/02/08 SMI	*/

/*
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

/*
 * A tek subwindow is a subwindow type that is used to provide a
 * Tektronix 4014 terminal emulation for teletype based programs.
 * It is usually a child of a tool window and doesn't worry about a border.
 *
 * The caller of teksw_fork typically waits for the child process to die
 * before exiting.
 */

/*
 * This is a dummy teksubwindow structure it is included to maintain
 * the generic subwindow interface and to enable type checking.
 * The "real" structure is in "teksw_imp.h" and is called teksw.
 */
struct teksubwindow {
	char dummy;
};

/*
 * Interface for tools
 */
extern struct toolsw *teksw_createtoolsubwindow();
extern int teksw_fork();

/*
 * Interface for subwindows
 */
extern struct teksubwindow *teksw_init();
extern teksw_done();
extern int teksw_handlesigwinch();
extern int teksw_selected();
