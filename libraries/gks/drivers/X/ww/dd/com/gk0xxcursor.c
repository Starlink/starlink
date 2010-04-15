/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
/*
 *  the cursor ww uses when ip is set on and no cursor has been set before
xxxxxxxx
xxxxxxx
xxxxxx
xxxxx
xxxxxx
xxxxxxx
xx  xxxx
x    xxxx
      xxxx
       xxxx
        xxxx
         xxxx
          xxxx
           xxx
            xx
 */
DDPUBLIC cursor *gk0xwwdefaultcursor(which){
	static char nullchars[] = {WWXOR, 0, 0, 0, 0,
	/* lines= */ 0,1, /* width= */ 0,1,0};
	static char origchars[] = {WWXOR, 0, 0, 0, 0,
	/* lines= */ 0,15, /* width= */ 0,14,
	0377,0,0376,0,
	0374,0,0370,0,
	0374,0,0376,0,
	0317,0,0207,0200,
	03,0300,01,0340,
	0,0360,0,0170,
	0,074,0,034,0,014}; /* ww style */
	static cursor *cu,*cu2;

	if(which==1){
		if(!cu)cu = gk0xcudecode(origchars,ENWWSTYLE);
		return(cu);
	}
	if(!cu2){
		cu2 = gk0xcudecode(nullchars,ENWWSTYLE);
	}
	return(cu2);
}
/*
 * copy the largest common box and clear rest.
 * Keep a cheat size of the bitmap that is just the size of the picture
 * so we can put the hourglass at the bottom right corner of it without a big gap.
 * This means we must restore the correct size when re-using the bitmap!
 * return the to bitmap. if it was null, get it.
 */
PRIVATE bitmap *gk0xcopycursor(jwp,from,basic)jwindow *jwp;bitmap *from;{
	box tobox;
	bitmap *to;

	if(from==NULLPTR(bitmap))gk0xwwpanic("copying null cursor bitmap");
	if(basic){
		to = gk0xbmget(MAXCUWIDTH,MAXCUHEIGHT);
		tobox = gk0xboxop(to->bm_box,from->bm_box,BOXINTERSECT);
		gk0xbmxcopy(from,from->bm_box,to,tobox,WWCOPY);
#ifndef WWFORSUN
		to->bm_box = tobox;
		/* was MAXCUWIDTH*MAXCUHEIGHT: now cheat for busy add-on */
#endif WWFORSUN
	}else{
		to = gk0xbmget(WIDTH(from->bm_box),HEIGHT(from->bm_box));
		gk0xbmxcopy(from,from->bm_box,to,to->bm_box,WWCOPY);
	}
	jwp->jw_jcursor.jc_flags |= JCISCOPY;
	return(to);
}
PUBLIC void gk0xcustack(cp, flag) cursor *cp; int flag;{
	gk0xcuset(cp, ddwin, flag|WWCUCOPY|WWCUBASIC);
}
/*
 * call cuwin or custack depending on size of cursor. dont copy bitmap.
 * same flags as custack
 */
PUBLIC void gk0xcuset(cp, wp, flag) cursor *cp; window *wp; int flag;{
	cursor *cc;
	jwindow *jwp;
	int what,basic,copy;

	if(wp==0)gk0xwwpanic("cuset: null window");
	jwp = jwin(wp);
	cc = &jwp->jw_jcursor.jc_cursor;
	what = flag&~(WWCUCOPY|WWCUBASIC);
	basic = flag&WWCUBASIC;
	copy = flag&WWCUCOPY;
	switch(what){
	case WWPUSHOFF:
		cp = gk0xwwdefaultcursor(2);
		what = WWPUSH;
		basic = TRUE;
		copy = FALSE;
		/* no break */
	case WWPUSHBUSY:
	case WWPUSH:
		STACKPUSH(jwp->jw_cstack,jwp->jw_jcursor);
		gk0xcuunset(wp,FALSE);
#ifdef WWFORSUN
		if(what==WWPUSHBUSY){
			gk0xaddbusy(wp);
			basic = TRUE;
			copy = FALSE;
			cp = 0;
		}
#endif WWFORSUN
		/* no break */
	case WWSET:
		if(what==WWSET){
			gk0xcuunset(wp,TRUE);
			if(cp==0){
				*cc = *gk0xwwdefaultcursor(1);
				basic = TRUE, copy = FALSE;
			}
		}
		if(cp!=0){
			*cc = *cp;
			if(!basic)wp->w_cursor = cp;	/* back compat for user */
		}
		/* new cursor struct now set in *cc */
		if(cc->c_bm==0)gk0xwwpanic("gk0xcuset: null bitmap for cursor");
		if(!basic &&
		  (cc->c_bm->bm_box.b_right>=MAXCUWIDTH ||
		   cc->c_bm->bm_box.b_bottom>=MAXCUHEIGHT ||
		   cc->c_mask!=0))
			jwp->jw_jcursor.jc_flags |= JCBIG;
		else if(!copy){
			/* need to notice user changes in bitmap even though
			 * small cursor */
			jwp->jw_jcursor.jc_flags |= JCSMALL;
		}
#ifndef WWFORSUN
		else if(cc->c_bm->bm_box.b_right!=MAXCUWIDTH-1 ||
		   cc->c_bm->bm_box.b_bottom!=MAXCUHEIGHT-1)copy = TRUE;
		/* machine needs exact size bitmaps for cursors.
		 * even sun, but done in cudisplay for other reasons. */
		if(what==WWPUSHBUSY)copy = TRUE;
#endif WWFORSUN
		if(copy){
			if(!(jwp->jw_jcursor.jc_flags & JCISCOPY))
				cc->c_bm = gk0xcopycursor(jwp,cc->c_bm,basic);
			if(cc->c_mask!=0)
			if(!basic)
				cc->c_mask = gk0xcopycursor(jwp,cc->c_mask,FALSE);
			else cc->c_mask = 0;
		}
#ifndef WWFORSUN
		if(what==WWPUSHBUSY)gk0xaddbusy(wp);
#endif WWFORSUN
		jwp->jw_jcursor.jc_bigcursor = wp->w_cursor;
		gk0xcusetup(wp);
		break;
	case WWPOP:
		if(STACKEMPTY(jwp->jw_cstack))gk0xwwpanic("cursor stack empty");
		gk0xcuunset(wp,TRUE);
		STACKPOP(jwp->jw_cstack,jwp->jw_jcursor);
		wp->w_cursor = jwp->jw_jcursor.jc_bigcursor; /* back compat for user */
		gk0xcusetup(wp);
		break;
	default:
		wwfail("unknown flag to cuset",);
	}
}
DDPUBLIC gk0xcusetup(wp)window *wp;{
	cursor *cc;
	jwindow *jwp;

	jwp = jwin(wp);
	cc = &jwp->jw_jcursor.jc_cursor;
#ifdef WWFORSUN
	if(jwp->jw_jcursor.jc_flags&JCBIG){	/* set null real cursor */
		jcursor jcsave;
		jcsave = jwp->jw_jcursor;
		*cc = *gk0xwwdefaultcursor(2);
		gk0xcudisplay(wp);
		jwp->jw_jcursor = jcsave;
		gk0xcubigon(wp);
	}else{
		if(jwp->jw_jcursor.jc_flags&JCSMALL){
			jbitmap *jbp;
			jbp = jbm(cc->c_bm);
			jbp->jbm_flags |= JBMCURSOR|JBMSMALL;
			jbp->jbm_cursorwin = wp;
		}
		gk0xcudisplay(wp);
	}
#else WWFORSUN
	gk0xcudisplay(wp);
#endif WWFORSUN
	dd->d_x += cc->c_xoffset;
	dd->d_y += cc->c_yoffset;
}
DDPUBLIC gk0xcuunset(wp,tofree)window *wp;{
	cursor *cc;
	jwindow *jwp;

	jwp = jwin(wp);
	cc = &jwp->jw_jcursor.jc_cursor;
#ifdef WWFORSUN
	if(jwp->jw_jcursor.jc_flags&JCBIG)
		gk0xcubigoff(wp);
	else if(jwp->jw_jcursor.jc_flags&JCSMALL)
		jbm(cc->c_bm)->jbm_flags &= ~(JBMCURSOR|JBMSMALL);
#endif WWFORSUN
	if(tofree && (jwp->jw_jcursor.jc_flags&JCISCOPY)){
		gk0xbmfree(cc->c_bm);
		if(cc->c_mask!=0)gk0xbmfree(cc->c_mask);
		cc->c_bm = cc->c_mask = 0;
	}
	dd->d_x -= cc->c_xoffset;
	dd->d_y -= cc->c_yoffset;
	jwp->jw_jcursor.jc_flags = 0;
	wp->w_cursor = 0;
}
/*
 */
PUBLIC void gk0xcufree(cp) cursor *cp;{
	if(cp==NULLPTR(cursor))return;
	gk0xbmfree(cp->c_bm);
	free((char *)cp);
}
#ifdef FORTINTER
FORTINTER void wcustk_(cp,flags)int *cp,*flags;{
	custack((cursor *)*cp,*flags);
}
FORTINTER void wcufre_(cp)int *cp;{
/*	cufree((char *)*cp); ????	*/
	free((char *)*cp);
}
FORTINTER int wcuget_(bm,xh,yh,xo,yo,rop)int *bm,*xh,*yh,*xo,*yo,*rop;{
	cursor *cp = STRUCTMALLOC(cursor);
	cp->c_bm =(bitmap *)*bm;
	cp->c_xhot = *xh;
	cp->c_yhot = *yh;
	cp->c_xoffset = *xo;
	cp->c_yoffset = *yo;
	cp->c_rop = *rop;
	return(int)cp;
}
#ifdef OLDFORTRAN
FORTINTER int wcuexm_(cp,flags)cursor **cp;int *flags;{
	cursor *c = *cp;
	switch(*flags){
		case WXBM:	return(int)c->c_bm;
		case WXROP:	return c->c_rop;
		case WXHOTX:	return c->c_xhot;
		case WXHOTY:	return c->c_yhot;
		case WXOFFX:	return c->c_xoffset;
		case WXOFFY:	return c->c_yoffset;
	}
	return 0;
}
#endif OLDFORTRAN
#endif FORTINTER
DDPUBLIC void gk0xcuinit(win)window *win;{
	jwin(win)->jw_restore |= RESPAT;	/* also tells perq 2 to restore pat when ipoff */
	jwin(win)->jw_jcursor.jc_cursor.c_bm = 0;
	jwin(win)->jw_jcursor.jc_flags = 0;
	gk0xcuunset(win,FALSE);
	gk0xcuset((cursor*)0,win,WWSET|WWCUCOPY|WWCUBASIC);
}
