/*	@(#)ttysw.h 10.10 88/02/08 SMI	*/

/*
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

/*
 * A tty subwindow is a subwindow type that is used to provide a
 * terminal emulation for teletype based programs.
 * It is usually a child of a tool window and doesn't worry about a border.
 *
 * The caller of ttysw_start typically waits for the child process to die
 * before exiting.
 */

/* options - controlled by ttysw_getopt(), ttysw_setopt		*/

#define	TTYOPT_PAGEMODE	1
/*	#define TTYOPT_HISTORY	2	for when it's supported	*/
#define TTYOPT_SELSVC	3
#define TTYOPT_TEXT	4		/* CMDSW */

/* styles for rendering boldface characters */
#define TTYSW_BOLD_NONE			0x0
#define TTYSW_BOLD_OFFSET_X		0x1
#define TTYSW_BOLD_OFFSET_Y		0x2
#define TTYSW_BOLD_OFFSET_XY		0x4
#define TTYSW_BOLD_INVERT		0x8
#define TTYSW_BOLD_MAX			0x8

/* Modes for invert and underline */
#define TTYSW_ENABLE			0x0
#define TTYSW_DISABLE			0x1
#define TTYSW_SAME_AS_BOLD		0x2

caddr_t	ttysw_create();

typedef caddr_t	Ttysubwindow;

#ifdef	cplus
/*
 * C Library routines specifically related to ttysw subwindow functions.
 */
caddr_t ttysw_create(Tool *tool, char *name, short width, height);
void	ttysw_becomeconsole(caddr_t ttysw);
int	ttysw_start(caddr_t ttysw, char **argv);
void	ttysw_done(caddr_t ttysw);
void	ttysw_setopt(caddr_t ttysw, int opt, int on);
int	ttysw_getopt(caddr_t ttysw, int opt);

/* Obsolete (but implemented) routines */
struct	toolsw *ttysw_createtoolsubwindow(
	struct tool *tool, char *name, short width, height);
caddr_t	ttysw_init(int windowfd);
void	ttysw_handlesigwinch(caddr_t ttysw);
void	ttysw_selected(caddr_t ttysw, int *ibits, *obits, *ebits,
	    struct timeval **timer);
int	ttysw_fork(caddr_t ttysw, char **argv,
	    int *inputmask, *outputmask, *exceptmask);
#endif

/* Obsolete (but implemented) routines */
extern	caddr_t ttysw_init();
extern	int ttysw_handlesigwinch(), ttysw_selected();
extern	struct toolsw *ttysw_createtoolsubwindow();
