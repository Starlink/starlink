/*	@(#)gfxsw.h 10.4 88/02/08 SMI	*/

/*
 * Copyright (c) 1983 by Sun Microsystems, Inc.
 */

/*
 * Subwindow type for graphics programs that either:
 *	1) take over an existing window as a separate process from the original
 *	owner of the window, or
 *	2) wants to run in a window that it has create as the sole owner.
 * The implications of (1) are that the graphics program wants this package
 * to take care of as many "window considerations" as possible which include:
 *	a) being told to restart or repair damage on clipping changes
 *	depending on whether the the window is retained or not.
 *	b) processing the cmdline in a standard way to determine
 *	whether the window should be made retained "-r" and
 *	how many repetitions of a graphics demo should occur "-n ####".
 * The implications of (2) are that a graphics program need not worry about
 * managing a retained window and can exist in a reasonable window framework
 * (e.g., tool/toolsw package).
 */

struct	gfxsubwindow {
	int	gfx_windowfd;
	int	gfx_flags;
#define	GFX_RESTART	0x01
#define	GFX_DAMAGED	0x02
	int	gfx_reps;
	struct	pixwin *gfx_pixwin;
	struct	rect gfx_rect;
	caddr_t	gfx_takeoverdata;
};

extern	struct gfxsubwindow *gfxsw_init();
#ifdef	TOOL_DONE
extern	struct toolsw *gfxsw_createtoolsubwindow();
#endif

#ifdef	cplus
/*
 * C Library routines specifically related to gfx subwindow functions.
 */

/*
 * Empty subwindow operations.
 */
struct	gfxsubwindow *gfxsw_init(int windowfd, char **argv);
void	gfxsw_handlesigwinch(struct gfxsubwindow *gfxsw);
void	gfxsw_done(struct gfxsubwindow *gfxsw);

/*
 * Utility for initializing toolsw as gfx subwindow in suntool environment
 */
struct	toolsw *gfxsw_createtoolsubwindow(
	struct tool *tool, char *name, short width, height, char **argv);

/*
 * Utility used by client that wants to explicitly have a retained window.
 */
gfxsw_getretained(struct gfxsubwindow *gfx);

/*
 * Called by "non-takeover" graphics programs when recieve SIGWINCH.
 */
void	gfxsw_interpretesigwinch(struct gfxsubwindow *gfx);
/*
 * Used implicitly by "takeover" programs as SIGWINCH signal handler instead
 * of calling gfxsw_interpretesigwinch.
 */
void	gfxsw_catchsigwinch();
/*
 * Used implicitly by "takeover" programs as general cleanup signal handler.
 */
void	gfxsw_cleanup();
/*
 * Used explicitely by "takeover" programs as substitute for tool_select call.
 */
void	gfxsw_select(struct  gfxsubwindow *gfx, int (*selected)(),
	    int inputmask, outputmask, exceptmask, struct timeval *timer);
/*
 * When in selected routine (see gfxsw_select) call this if want to return from
 * gfxsw_select call.
 */
void	gfxsw_selectdone(struct gfxsubwindow *gfx);
/*
 * Calling sequence of selected routine (see gfxsw_select).
 * Can be called only if gfx->gfx_flags has GFX_RESTART bit set.
 */
gfxsw_selected(struct gfxsubwindow *gfx; int *ibits, *obits, *ebits;
	    struct timeval **timer);
{
#endif




