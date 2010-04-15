/*	@(#)wmgr.h 10.8 88/02/08 SMI	*/

#ifndef wmgr.h_DEFINED
#define wmgr.h_DEFINED	1

/*
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

/*
 * This header file describes the interface to a window management mechanism.
 * A menu interface to these functions is also provided.
 * Typically, a tool window is responsible for window management.
 */

/*
 * The following menu is initialized and available to a user program once
 * wmgr_setupmenu is called.  It can be stacked with over menus and
 * presented to the menu.h interface.
 */
extern	struct	menu *wmgr_toolmenu;

#define	WMGR_ICONIC	WUF_WMGR1	/* Indicates window is iconic
					   in user flags of window */
#define WMGR_SUBFRAME   WUF_WMGR3   /* Indicates window is a sub-frame
					   in user flags of window */

#define	WMGR_SETPOS	-1		/* Indicates "use default" in
					   wmgr_figure*rect calls	*/

typedef enum  {
	WM_None, WM_N, WM_NE, WM_E, WM_SE, WM_S, WM_SW, WM_W, WM_NW
}   WM_Direction;

/*
 * C Library routines that provide tool management and root utilities.
 */

/*
 * Routines to manage the wmgr_toolmenu.
 */
void	wmgr_setupmenu(/* int toolfd */);
int	wmgr_handletoolmenuitem(/* struct menu *menu, struct menuitem *mi,
	    int toolfd, rootfd */);

/*
 * Basic window management operations.
 * Move and stretch require user interaction.
 */
void	wmgr_open(/* int toolfd, rootfd */);
void	wmgr_close(/* int toolfd, rootfd */);
void	wmgr_move(/* int toolfd */);
void	wmgr_stretch(/* int toolfd */);
void	wmgr_top(/* int toolfd, rootfd */);
void	wmgr_bottom(/* int toolfd, rootfd */);
void	wmgr_full(/* struct tool *tool, int rootfd */);

/*
 * Utilities
 *
 * Exported by wmgr_menu.c:
 */
int	wmgr_confirm(/* int feedbackfd, char *text */);

/*
 * Exported by wmgr_rect.c:
 */
void		wmgr_completechangerect(/* fd, newrect, oldrect, parent_left, parent_top */ );
void		wmgr_refreshwindow(/* int windowfd */);
WM_Direction	wmgr_compute_grasp();
void		wmgr_providefeedback();
struct rect	wmgr_set_bounds();
int		wmgr_get_placeholders(/* orect, irect */);
int		wmgr_set_placeholders(/* orect, irect */);

/*
 * The following routines are exported by wmgr_policy.c;  they implement
 * the default positioning of icons and open windows for tools.
 * Init_xxx_posiiton sets the initial position from which a sequence
 *	of icons / open windows will propagate.
 * Figure_xxx_rect establishes a new rect for one tool, without
 *	requiring the tool's windowfd;  this implies the window should
 *	not yet be in the display tree when these routines are called.
 * Acquire_xxx_rect does the same job, but takes care of removing /
 *	reinserting the window in the display tree.
 * Inquire_xxx_rect returns the same information Aquire would,
 *	without advancing the global positioning information.
 * Set_xxx_rect handle the rect changes for operations Front / Back
 *	and Full / Open / Close
 */
int	wmgr_init_icon_position();
int	wmgr_init_tool_position();
int	wmgr_figureiconrect(/* int rootfd; struct rect *iconrect */);
int	wmgr_figuretoolrect(/* int rootfd; struct rect *toolrect */);
int	wmgr_acquire_icon_rect(/* int windowfd; struct rect *iconrect */);
int	wmgr_acquire_tool_rect(/* int windowfd; struct rect *toolrect */);
int	wmgr_inquire_icon_rect(/* int windowfd; struct rect *iconrect */);
int	wmgr_inquire_tool_rect(/* int windowfd; struct rect *toolrect */);
int	wmgr_set_icon_rect(/* int windowfd, op */);
int	wmgr_set_tool_rect(/* int windowfd, op */);

/*
 * Exported by wmgr_state.c:
 */
int	wmgr_iswindowopen(/* int windowfd */);
void	wmgr_changestate(/* int windowfd, rootfd, close */);
void	wmgr_changelevel(/* int windowfd, parentfd, top */);
void	wmgr_changelevelonly(/* int windowfd, parentfd, top */);
void	wmgr_winandchildrenexposed(/*struct pixwin *pw; struct rectlist *rl */);
void	wmgr_setlevel(/* int windowfd, prevlink, nextlink */);
void	wmgr_full(/* struct tool *tool, int rootfd */);

/*
 * Fork programname with otherargs.  Place its normal rect at normalrect.
 * Place its icon rect at iconrect.  Iconicflag indicates the original
 * state of the tool.  Positioning/state information are only hints.
 * The tool can ignore them.
 */
int	wmgr_forktool(/* char *programname, *otherargs, struct rect *normalrect,
	   struct rect *iconrect, int iconicflag */);

#endif

