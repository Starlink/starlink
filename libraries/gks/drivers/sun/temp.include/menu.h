/*	@(#)menu.h 10.4 88/02/08 SMI	*/

/*
 * Copyright (c) 1983 by Sun Microsystems, Inc.
 */

/*
 * A menu is a collection of command items that are:
 * 	1) quickly displayed by the user (by a mouse button push),
 *	2) pointed at by the user with a locator device (a mouse), and
 *	3) quickly disappear (after the mouse button is released).
 *
 * A program can use the package that implements this interface to manage
 * its menus.  A menu is an array of keyword/data (menuitem) pairs.
 * Once the menu is displayed, when a menuitem is chosen by the user,
 * a pointer to this menuitem is returned.
 *
 * Multiple menus can be strung together is a list and will be managed
 * by this interface.  They are presented to the user as a stack of menus
 * through which he can cycle.
 *
 * The functions menu_display is called when your program notices the menu
 * button went down.  The inputevent that describes the menu button going
 * down is passed in.  The function returns when an item is selected or
 * the menu button goes up.  The returned value is the menuitem that was
 * invoked or 0 if nothing was selected.  Upon returning, the passed in
 * pointer to a menu pointer is the menu that is currently on top of the
 * menu stack.
 *
 * As a utility sometimes used with menus, a prompt facility is available.
 * Call prompt_display if you want to give the user a quick hint about how
 * to procede once a menu item has been selected.  This function returns
 * on any input event other than a mouse motion and the passed in inputevent
 * pointer is filled out with the event that caused the return.
 */

/*
 * Normal menu button assignments
 */
#define MENU_BUT  	MS_RIGHT
#define SELECT_BUT  	MS_LEFT

#define MENU_BORDER  	2

struct	menu {
	int	m_imagetype;		/* interpretation of m_imagedata */
#define	MENU_IMAGESTRING	0x00	/* imagedata is char * */
#define MENU_GRAPHIC            0x01    /* imagedata is pixrect * */
	caddr_t	m_imagedata;		/* pointer to display data for header */
	int	m_itemcount;		/* number of menuitems in m_items */
	struct	menuitem *m_items;	/* array of menuitems */
	struct	menu *m_next;		/* link to another menu */
	caddr_t	m_data;			/* menu private data (initialize to 0)*/
};

struct	menuitem {
	int	mi_imagetype;		/* interpretation of mi_imagedata */
	caddr_t	mi_imagedata;		/* pointer to display data for item */
	caddr_t	mi_data;		/* item specific data */
};

struct prompt {
	struct	rect prt_rect;		/* where (window relative) to prompt */
#define	PROMPT_FLEXIBLE	-1		/* system decides placement/size */
	int	prt_windowfd;		/* windowfd that will take input from */
	struct	pixfont *prt_font;	/* text font */
	char	*prt_text;		/* text to format into box */
};

extern	struct menuitem *menu_display();

#ifdef	cplus
/*
 * C Library routines that implement this menu interface.
 */

struct	menuitem *menu_display(struct menu **menuptr,
	    struct inputevent *event, int iowindowfd);
void	menu_prompt(struct prompt *prompt,
	    struct inputevent *event, int iowindowfd);

#endif

