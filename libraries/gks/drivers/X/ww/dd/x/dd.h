/* copyright (c) Mark M Martin. RAL. 1986 */
/*
 * include file for all ww*c routines that go together
 * to build the ww library of routines. jul 85
 */
#define WWFORX
#include "../xdd.h"
/* ---------------------------- Xwindows --------------------------------- */
#include <X11/Xlib.h>
#include <sgtty.h>
#define MAXCUWIDTH	64
#define MAXCUHEIGHT	64
#define WWSELINIT	0
/*
 * jdd->jd_flags &= ~JUSETRICK
 * if you have an X that will do XSetWindowAttributes.backing_store=Always
 * for you, else use the trick of having the background tile equal to the
 * size of the window and therefore the retained bitmap.
 * If you dont JUSETRICK you seem to get "not a pixmap" from
 * bmxcopy's XSetTile if copying from the window.
 */
/*
 * Cannot ask for property of a deleted window, even if just been handed
 * an event (eg Leave) for it! protocol error. Otherwise
#define GETHANDLEFROMX
 */
#define NOTESCREEN(bitmap,box) gk0xnotescreen(bitmap,box)
#define WXD	jdd->jd_xdisplay
#define WXS	jdd->jd_xscreen
#define WXDS	jdd->jd_xdisplay,jdd->jd_xscreen
#define WXGC	jdd->jd_xgc
/* ------------------------------------------------------------------ */
/*
 * more junk info on global setup
 */
typedef struct{
	window		*jd_windows;	/* list of all windows */
	int		jd_flags;
	box		jd_defaultbox;	/* from WWOPTIONS */
	int		*jd_defaultcolour;/* from WWOPTIONS */
	int		jd_defcolsize;	/* size of jd_defaultcolour, ints */
	char		*jd_defaultfont;/* from WWOPTIONS */
	char		*jd_mapname;	/* from WWOPTIONS */
	Display		*jd_xdisplay;
	int		jd_xscreen;
	int		jd_getscreen;	/* count of getscreen's */
	int		jd_depth;	/* real depth (of bitmaps...) */
	Window		jd_root;
	GC		jd_xgc;
	XGCValues	jd_xgcvalues;
	struct sgttyb	jd_sg;		/* the user's settings for erase and kill to restore */
#ifdef GETHANDLEFROMX
	Atom		jd_backhandle;	/* how to get window* from X window */
#endif GETHANDLEFROMX
}jwwstate;
#define JSYNCHINPUT	0x80000000	/* do a sync before each input */
#define JUSETRICK	0x40000000	/* use trick of retained background */
#define JINSTALLMAP	0x20000000	/* install colourmap cos winman wont */
#define JMONO		0x10000000	/* pretend mono only windows */
#define JNOCOPYPLANE	0x08000000	/* dont call XCopyPlane */
#define JSTIPPLE	0x04000000	/* use stipple as retained background */
#define JCOPYAREA	0x02000000	/* use XCopyArea not XFillRectangle */
#define JREPORT		0x01000000	/* report unheeded events */
#define JVISIBILITY	0x00800000	/* Visibility shows iconised, not map */
#define JPARENT		0x00400000	/* parent map shows iconised */
#define JWHITEBORDER	0x00200000	/* border windows white not black */

#define ENTRYSET	(0xff000000)	/* in jd_defaultcolour */

/*
 * more junk info on each window
 */
typedef struct{
	window	*jw_nextwindow;	/* next in list of all windows */
	int	jw_restore;	/* what to restore in window (see wwip.c for bits) */
	int	jw_isoff;	/* whether updating to screen or not */
	jcursor	jw_jcursor;
	stack	jw_cstack;	/* jcursor stack. must init = sizeof(jcursor) */
	int	jw_flags;
	Window	jw_xwindow;
	Colormap jw_cmap;
	int	jw_colmapsize;	/* biggest size colmap for window has got to */
	Pixmap	jw_retained;	/* oh! retained bitmap cheat */
	box	jw_update;		/* box to update on screen */
}jwindow;
/*
 * more junk on bitmaps
 */
typedef struct{
	box		jbm_boxclip;	/* must be 0,0 in top,left on sun/wc if no clip */
	window		*jbm_cursorwin;	/* window this is a cursor[mask] for */
	int		jbm_flags;
	jwindow		*jbm_jwindow;	/* window this bitmap for */
	Pixmap		jbm_memory;
	Drawable	jbm_drawable;
}jbitmap;

DDPUBLIC extern jwwstate *jdd;
/*
 * External Typing: rest of file generated automatically BEWARE
 */
addfont();
cusetup();
cuunset();
gk0xwwseticonpic();
bitmap	*gk0xbmreallyget();
int	 gk0xcoltodepth();
void	 gk0xcubigoff();
void	 gk0xcubigon();
void	 gk0xcudisplay();
void	 gk0xcuinit();
struct	 layout *gk0xmaptomem();
void	 gk0xnotescreen();
void	 gk0xparsewwoptions();
void	 putbyte();
void	 gk0xsetbmrop();
void	 setgcclip();
void	 setgcrop();
void	 gk0xsetsc();
Cursor	 setscreencursor();
char	*setxshare();
void	 gk0xunmapfrommem();
window	*gk0xwinalloc();
window	*gk0xwingetx();
int	 gk0xwinsetraster();
bitmap	*gk0xwwbmget();
cursor	*gk0xwwdefaultcursor();
window	*wwgetthis();
int	 gk0xwwlineop();
int	 gk0xwwrasop();
int	 gk0xwwstartup();
void     gk0xbmfree();
bitmap  *gk0xxxgrey();
