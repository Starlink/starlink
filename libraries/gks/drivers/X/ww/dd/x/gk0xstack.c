/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
/*
 * values in jw_isoff:
 * FALSE: all rasterops to be echoed to window.
 * TRUENOCHANGES: isoff is true, but no rasterops done to window bitmap yet
 * TRUECHANGED: some changes need to be written to window.
 */
#define TRUENOCHANGES	1
#define TRUECHANGED	2
/*
 * for sun and whitechapel keep a box enclosing all the areas changed
 * for a one-and-only rasterop copy from the retained bitmap
 * when the wwstack is switched back on
 */
/*
 * stack screen on and off commands. ON initially  (0 stored in jw_isoff)
 */
DDPUBLIC void gk0xsetsc(jwp,off)jwindow *jwp;{
	if(!off){
		if(jwp->jw_isoff){
			if(!(jdd->jd_flags&JUSETRICK)){
				if(jwp->jw_restore & RESIPON)XFlush(WXD);
				else XSync(WXD,FALSE/*not discard ip events*/);
			}else if(jwp->jw_isoff==TRUECHANGED)
				gk0xupdatescreen(jwp,jwp->jw_update);
			jwp->jw_isoff = FALSE;
		}
	}else{
		if(!jwp->jw_isoff){
			jwp->jw_isoff = TRUENOCHANGES;
		}
	}
}
/* !JUSETRICK=> box b not used */
PRIVATE gk0xupdatescreen(jwp,b)jwindow *jwp;box b;{
	if(jdd->jd_flags&JUSETRICK){
		if(jdd->jd_flags&JSTIPPLE){
			XSetFillStyle(WXD,WXGC,FillOpaqueStippled);
			XSetStipple(WXD,WXGC,jwp->jw_retained);
			XFillRectangle(WXD,jwp->jw_xwindow,WXGC,
				b.b_left,b.b_top,WIDTH(b),HEIGHT(b));
			XSetFillStyle(WXD,WXGC,FillSolid);
		}else XClearArea(WXD,jwp->jw_xwindow,
			b.b_left,b.b_top,WIDTH(b),HEIGHT(b),FALSE);
	}
	if(jwp->jw_restore & RESIPON)XFlush(WXD);
	else XSync(WXD,FALSE/*not discard ip events*/);
}
/* !JUSETRICK=> box b not used */
DDPUBLIC void gk0xnotescreen(bm,b)bitmap *bm;box b;{
	jwindow *jwp;
	jwp = jbm(bm)->jbm_jwindow;
	if(jwp==0 || !(jwp->jw_flags&(JWWINDOW|JWICON)))return;	/* windows & icons */
	if(jbm(bm)->jbm_flags&(JBMCURSOR|JBMMASK))return;
	if(!jwp->jw_isoff)	/* on at the moment, reproduce on window */
		gk0xupdatescreen(jwp,b);	/* !JUSETRICK=> box b not used */
	else if(jwp->jw_isoff==TRUENOCHANGES){
		jwp->jw_update = b;	/* !JUSETRICK=> box b not used */
		jwp->jw_isoff = TRUECHANGED;
	}else if(jdd->jd_flags&JUSETRICK){
		if(b.b_left<jwp->jw_update.b_left)
			jwp->jw_update.b_left = b.b_left;
		if(b.b_right>jwp->jw_update.b_right)
			jwp->jw_update.b_right = b.b_right;
		if(b.b_top<jwp->jw_update.b_top)
			jwp->jw_update.b_top = b.b_top;
		if(b.b_bottom>jwp->jw_update.b_bottom)
			jwp->jw_update.b_bottom = b.b_bottom;
	}
}
