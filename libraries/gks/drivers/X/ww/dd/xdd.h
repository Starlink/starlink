/* copyright (c) Mark M Martin. RAL. 1986 */
/*
 * include file for all ww*c routines that go together
 * to build the ww library of routines. jul 85
 */
#include "../di/di.h"
/*
 * in jd_flags
 */
#define JDWASBLACK	01	/* original window was inverted */
#define JDPATNULL	02	/* couldnt get first cursor pattern */
#define JDW_STAT	04	/* jd_w_stat is valid */
#define JDSGVALID	010	/* jd_sg is valid */
#define JDREVERSE	020	/* X reverse video */
#define JDDEFAULTBOX	040	/* jd_defaultbox given */
/*
 * cursor stack element: junkcursor (not pointed to by anything, just on stack.
 */
typedef struct{
	cursor	jc_cursor;	/* normal cursor */
	cursor	*jc_bigcursor;	/* big cursor or 0 */
	int	jc_flags;
}jcursor;
#define JCISCOPY	01	/* bitmap is a copy made by ww */
#define JCBIG		02	/* big cursor, or masked cursor */
#define JCSMALL		04	/* small cursor but must notice changes in */
/*
 * extra flags to cuset. >WWPUSH etc.
 */
#define WWCUCOPY	0100	/* copy bitmaps, so ignore changes */
#define WWCUBASIC	0200	/* tuncate cursor if too big */
/*
 * more junk info on each window
 */
#define jwin(x) ((jwindow *)((x)->w_junk))
#define RESPAT		01	/* in jw_restore */
#define RESTTY		02
#define RESFUNC		04
#define RESDONT		010
#define RESIPON 	020
#define RESIPNOKEY 	040
#define RESIPRAW	0100
/*
 * in jw_flags
 */
#define JWSCREEN	01	/* this window is really the screen */
#define JWICON		02	/* this jwindow is really an icon */
#define	JWWINDOW	04	/* this is a window, at last */
#define JWOURCURSOR	010	/* ww is implementing a big cursor */
#define JWSCREENIPON	020	/* this is the screen with ipset(on)! */
#define JWDEFAULTCOLOUR	040	/* still using default ww colourmap */
/*
 * macros for list of windows
 */
#define HEADWIN		(jdd->jd_windows)
#define NEXTWIN(x)	(jwin(x)->jw_nextwindow)
/*
 * more junk on bitmaps
 */
#define jbm(x)	((jbitmap *)((x)->bm_junk))
#define JBMCURSOR	01	/* this is for a big or small cursor */
#define JBMMASK		02	/* cursor mask */
#define JBWINDOW	04	/* window */
#define JBICON		010	/* icon */
#define JBMSMALL	020	/* small cursor but must notice changes in */
/*
 * flags to cunote, cusetxy
 */
#define JCUSHOW		01	/* show big cursor at dd->d_x,y */
#define JCUREMOVE	02	/* restore underneath */
