/*	@(#)tool_struct.h 10.8 88/02/08 SMI	*/

/*
 * Copyright (c) 1983 by Sun Microsystems, Inc.
 */

#ifndef tool_struct_DEFINED
#define	tool_struct_DEFINED
/*
 * Definitions of "standard" tool icon graphics/text size/proportions
 */
typedef	struct	toolio {
	fd_set	tio_inputmask,	/* Additional fd to select on in tool_select */
		tio_outputmask,	/* (See select system call documentation) */
		tio_exceptmask;
	struct	timeval	*tio_timer; /* Timeout used in tool_select */
	int	(*tio_handlesigwinch)();/* call when win should repair image */
	int	(*tio_selected)();      /* call from tool_select */
} Toolio;

typedef	struct	tool {
	short	tl_flags;	/* tool booleans */
#define	TOOL_NAMESTRIPE		(0x01)	/* include a name stripe */
#define	TOOL_BOUNDARYMGR	(0x02)	/* movable borders between subwindows */
#define	TOOL_ICONIC		(0x04)	/* current state is iconic */
#define	TOOL_SIGCHLD		(0x08)	/* info passed to tool_select */
#define	TOOL_SIGWINCHPENDING	(0x10)	/* need to call tool_handlesigwinch*/
#define	TOOL_DONE		(0x20)	/* need to return from tool_select */
#define	TOOL_FULL		(0x40)	/* current state is full */
#define	TOOL_EMBOLDEN_LABEL	(0x80)	/* embolden tool label */
#define	TOOL_FIRSTPRIV		(0x0100)/* start of private flags range */
#define	TOOL_LASTPRIV		(0x8000)/* end of private flags range */
	int	tl_windowfd;	/* file descriptor of tool window */
	char	*tl_name;	/* string in name stripe & default icon */
	struct	icon *tl_icon;	/* icon */
	struct	toolio tl_io;	/* Tool_select and signal handling */
	struct	toolsw *tl_sw;	/* list of subwindows that tool is managing */
	struct	pixwin *tl_pixwin; /* display mechanism structure */
	struct	rect tl_rectcache; /* rect of tool (tool relative) */
	struct	rect tl_openrect;  /* saved open rect of tool while full */
	caddr_t tl_menu;	/* Menu, Non zero if tool has a walking menu */
	void	(*props_proc)();/* proc to call on props */
	int	props_active;
} Tool;
#define	TOOL_NULL	((Tool *)0)

typedef	struct	toolsw {
	struct	toolsw *ts_next;/* next subwindow */
	int	ts_windowfd;	/* file descriptor of subwindow */
	char	*ts_name;	/* identifies subwindow (for future use) */
	short	ts_width;	/* width at which sw wants to be maintained */
	short	ts_height;	/* height at which sw wants to be maintained */
#define	TOOL_SWEXTENDTOEDGE	-1 /* extend width|height to edge of tool */
	struct	toolio ts_io;	/* Tool_select and signal handling */
	int	(*ts_destroy)();/* call when removing subwindow */
	caddr_t	ts_data; 	/* uninterpreted data passed to functions */
	caddr_t	ts_priv; 	/* tool implementation private data */
} Toolsw;
#define	TOOLSW_NULL	((Toolsw *)0)

/*
 * Standard (but not enforced) constant values
 */
#define	TOOL_BORDERWIDTH	(5)
#define	TOOL_SUBWINDOWSPACING	(TOOL_BORDERWIDTH)
#define	TOOL_NAMESTRIPEXTR	(0)
#define	TOOL_ICONHEIGHT		(64)
#define	TOOL_ICONWIDTH		(64)
#define	TOOL_ICONHEIGHT		(64)
#define	TOOL_ICONMARGIN		(0)

#define	TOOL_ICONIMAGEWIDTH	((TOOL_ICONWIDTH)-2*(TOOL_ICONMARGIN))
#define	TOOL_ICONIMAGEHEIGHT	((TOOL_ICONHEIGHT)-2*(TOOL_ICONMARGIN))
#define	TOOL_ICONIMAGELEFT	(TOOL_ICONMARGIN)
#define	TOOL_ICONIMAGETOP	(TOOL_ICONMARGIN)

#define	TOOL_ICONTEXTWIDTH	(TOOL_ICONIMAGEWIDTH)
#define	TOOL_ICONTEXTHEIGHT	((TOOL_ICONHEIGHT)-(TOOL_ICONHEIGHT)/4)
#define	TOOL_ICONTEXTLEFT	(TOOL_ICONIMAGELEFT)
#define	TOOL_ICONTEXTTOP \
	((TOOL_ICONHEIGHT)-((TOOL_ICONTEXTHEIGHT)+(TOOL_ICONMARGIN)))

/*#define	tool_install(tool)	win_insert((tool)->tl_windowfd)*/
#define	tool_getnormalrect(tool, rectp) \
	    wmgr_getnormalrect((tool)->tl_windowfd, (rectp))
#define	tool_setnormalrect(tool, rectp) \
	    wmgr_setnormalrect((tool)->tl_windowfd, (rectp))

#define	TOOL_SW_ICONIC_OFFSET	2048
#define	tool_sw_iconic_offset(tool)	\
	(((tool)->tl_flags & TOOL_ICONIC) ? TOOL_SW_ICONIC_OFFSET : 0)

#define	tool_is_iconic(tool)	\
	    (!wmgr_iswindowopen((tool)->tl_windowfd))


extern	struct tool *tool_begin();
extern	struct toolsw *tool_createsubwindow();
extern	short tool_stripeheight(), tool_borderwidth(), tool_subwindowspacing();
extern	struct pixrect *tool_bkgrd;

#define	tool_end(tool)	tool_destroy((tool))

#ifdef	cplus
/*
 * C Library routines specifically related to tool functions.
 */

/*
 * Create operations
 */
int	tool *tool_new(int attribute_num, char *value,
	    ...more attribute_num/value pairs..., 0);
struct	toolsw *tool_createsubwindow(struct tool *tool, char *name,
	    short width, height);

/*
 * Cleanup routines
 */
void	tool_destroysubwindow(struct tool *tool, struct toolsw *toolsw);
void	tool_destroy(struct tool *tool);

/*
 * Subwindow layout utilities.
 */
short	tool_stripeheight(struct tool *tool);
short	tool_borderwidth(struct tool *tool);
short	tool_subwindowspacing(struct tool *tool);
int	tool_heightfromlines(struct tool *tool, int lines);
int	tool_widthfromcolumns(struct tool *tool, int columns);
int	tool_linesfromheight(struct tool *tool, int height);
int	tool_columnsfromwidth(struct tool *tool, int width);

/*
 * Input & display functions.
 */
void	tool_select(struct tool *tool, int waitprocessesdie);
void	tool_sigchld(struct tool *tool);
void	tool_done(struct tool *tool);
void	tool_sigwinch(struct tool *tool);
void	tool_handlesigwinchstd(struct tool *tool);
void	tool_selectedstd(struct tool *tool,
	    int *inputbits, *outputbits, *exceptbits, struct timeval *timer);

/*
 * Replacable operations calling sequence.
 */
void	tio_selected(caddr_t ts_data, int *inputbits, *outputbits, *exceptbits,
	    struct timeval *timer);
void	tio_handlesigwinch(caddr_t ts_data);
void	ts_destroy(caddr_t ts_data);
void	tool_layoutsubwindows(struct tool *tool);

/*
 * Attribute operations.
 */
int	tool_set_attributes(struct tool *tool, int attribute_num, char *value,
	    ...more attribute_num/value pairs..., 0);
char *	tool_get_attribute(struct tool *tool, int attribute_num);
void	tool_free_attribute(int attribute_num, char *value);
void	tool_free_attribute_list(char **avlist);
void	tool_find_attribute(char **avlist, int attribute_num, char **value);
int	tool_parse_one(int argc, char **argv, char ***avlist_ptr,
	    char *tool_name);
int	tool_parse_all(int *argc_ptr, char **argv, char ***avlist_ptr,
	    char *tool_name);

/*
 * Obsolete (but implemented) operations
 */
struct	tool *tool_create(char *name, short flags, struct rect *rect,
	    struct icon *icon); /* Use tool_make instead */
int	tool *tool_make(int attribute_num, char *value,
	    ...more attribute_num/value pairs..., 0);
void	tool_display(struct tool *tool); /* Use tool_set_attributes */

#endif

/*
 * Obsolete (but implemented) operations
 */
extern	struct tool *tool_create();	/* Old-fashion */
extern	struct tool *tool_make();	/* Old-fashion */

#endif not tool_struct_DEFINED
