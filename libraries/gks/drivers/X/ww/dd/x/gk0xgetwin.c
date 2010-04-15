/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
DDPUBLIC int gk0xwinsetraster(wp,w,h)window *wp;{
	jbitmap *jbp;
	jwindow *jwp;

	jbp = jbm(wp->w_bm);
	jwp = jwin(wp);
	if(jwp->jw_xwindow!=jbp->jbm_drawable)
		XFreePixmap(WXD,jbp->jbm_drawable);
	wp->w_bm->bm_box = gk0xboxbuild(0,0,w-1,h-1);
	jbp->jbm_drawable = XCreatePixmap(WXD,jdd->jd_root,
		w,h,(jdd->jd_flags&JSTIPPLE)?1:jdd->jd_depth);
	if(jbp->jbm_drawable==0)
		gk0xwwpanic("gk0xwwxget: couldnt get space for retained pixmap");
	if(!(jdd->jd_flags&JSTIPPLE))XSetWindowBackgroundPixmap(WXD,jwp->jw_xwindow,jbp->jbm_drawable);
	else jwp->jw_retained = jbp->jbm_drawable; /* lazy */
}
#include <X11/Xutil.h>
#include <X11/Xatom.h>
DDPUBLIC window *gk0xwingetx(size,label,flags)box size;char *label;int flags;{
	window *newwin;
	jwindow *jwp,*jiconp;
	bitmap *icon;
	XWMHints wmh;
	XSizeHints sh;
	XSetWindowAttributes satt;
	XEvent ev;
	XWindowAttributes watt;
	char *cp;
	long what;

	newwin = gk0xwinalloc(size);
	jwp = jwin(newwin);
	jwp->jw_flags |= JWWINDOW;
	jbm(newwin->w_bm)->jbm_jwindow = jwp;
/* 	cp = XGetDefault(WXD,label?label:"ww","ReverseVideo");
	if(cp!=0 && strcmp(cp,"on")==0)
		reverse video!
*/	cp = XGetDefault(WXD,label?label:"ww","Geometry");
	if(cp!=0){
		int x,y,w,h;
		x = size.b_left, y = size.b_top, w = WIDTH(size), h = HEIGHT(size);
		XParseGeometry(cp,&x,&y,&w,&h);
		/* returns XValue  XNegative WidthValue etc ored */
		size = gk0xboxbuild(x,y,x+w-1,y+h-1);
		flags &= ~WWSIZEFROMUSER;
	}
	satt.background_pixmap = None;
	what = CWBackingStore|CWEventMask; /* |CWBorderPixel */
	if(!(jdd->jd_flags&JUSETRICK)){
		satt.backing_store = Always;
		what |= CWBackPixel;
	}else{
		satt.backing_store = NotUseful;
		what |= CWBackPixmap;
	}
	satt.event_mask = ExposureMask;
	/* satt.border_pixel = jdd->jd_xgcvalues.foreground; */
	satt.background_pixel = jdd->jd_xgcvalues.background;
	jbm(newwin->w_bm)->jbm_drawable = jwp->jw_xwindow =
	  XCreateWindow(WXD,jdd->jd_root,size.b_left,size.b_top,
		WIDTH(size),HEIGHT(size),2/*borderwidth*/,
		CopyFromParent/*depth*/,InputOutput,
		CopyFromParent/*visual*/,what,
		&satt);
	if(jwp->jw_xwindow==0)gk0xwwpanic("winget: cannot XCreateWindow");
#ifdef GETHANDLEFROMX
	XChangeProperty(WXD,jwp->jw_xwindow,jdd->jd_backhandle,XA_INTEGER,8,PropModeReplace,(char*)&newwin,4);
#endif GETHANDLEFROMX
	if(label!=0){
		XStoreName(WXD,jwp->jw_xwindow,label);
		XSetIconName(WXD,jwp->jw_xwindow,label);
	}
	wmh.flags = StateHint;
	wmh.initial_state = (flags&WWOPENICONISED)?IconicState:ZoomState;
#ifdef NEVER
{	XIconSize *list;
	int count,w,h,x;
	w = h = 0;
	if(0!=XGetIconSizes(WXD,jdd->jd_root,&list,&count))
	while(count>0){
		x = (64-list->min_width)/list->width_inc*list->width_inc;
		if(x>list->max_width)x = list->max_width;
		if(x>w && w!=64)w = x;
		x = (64-list->min_height)/list->height_inc*list->height_inc;
		if(x>list->max_height)x = list->max_height;
		if(x>h && h!=64)h = x;
	}else w = h = 64;
}
#endif NEVER
#ifndef WWFORSUN
	newwin->w_icon = icon = gk0xbmget(64,64);
#else WWFORSUN
	/* doesnt work on suns. did it on wc? */
	newwin->w_icon = icon = gk0xbmreallyget(64,64,2/*colours*/);
#endif WWFORSUN
	/* overwrite icon drawable for time being.
	 * we think its a bitmap, it is a window */
	satt.backing_store = NotUseful;
	satt.background_pixmap = None;
	what = CWBackingStore|CWBackPixmap;
	jbm(icon)->jbm_jwindow = jiconp = STRUCTMALLOC(jwindow);
	jiconp->jw_update.b_left = UNSET;
	jiconp->jw_flags = JWICON;
	jiconp->jw_xwindow = wmh.icon_window =
	  XCreateWindow(WXD,jdd->jd_root,0,0,64,64,0/*borderwidth*/,
		CopyFromParent/*depth*/,InputOutput,
		CopyFromParent/*visual*/,what,
		&satt);
	wmh.flags |= IconWindowHint;
	/* XMapWindow(WXD,wmh.icon_window); */
	XSetWMHints(WXD,jwp->jw_xwindow,&wmh);
	XSetWindowBackgroundPixmap(WXD,jiconp->jw_xwindow,jbm(icon)->jbm_drawable);

	if(!(flags&WWSIZEFROMUSER)){
		/* tell win man size user wants */
		sh.flags = USPosition|USSize;
		sh.x = size.b_left, sh.y = size.b_top;
		sh.width = WIDTH(size), sh.height = HEIGHT(size);
		XSetNormalHints(WXD,jwp->jw_xwindow,&sh);
	}
	XMapWindow(WXD,jwp->jw_xwindow);
	/* have asked for exposure events. Now flush output and
	* wait for first such expose event, showing window exists.
	* cannot do if opening as icon, else hang till uniconised */
	if(flags&WWOPENICONISED)XSync(WXD,FALSE);
	else XWindowEvent(WXD,jwp->jw_xwindow,ExposureMask,&ev);
	/* get real size and position of window */
	XGetWindowAttributes(WXD,jwp->jw_xwindow,&watt);
	newwin->w_xrel = watt.x;
	newwin->w_yrel = watt.y;
	if(!(jdd->jd_flags&JUSETRICK)){
		newwin->w_bm->bm_box.b_right = watt.width-1;
		newwin->w_bm->bm_box.b_bottom = watt.height-1;
	}else gk0xwinsetraster(newwin,watt.width,watt.height);
	XClearWindow(WXD,jwp->jw_xwindow);
	/* ip off again. no events */
	XSelectInput(WXD,jwp->jw_xwindow,NoEventMask);
	/* rely on jw_jcursor fields being 0: the x/y offset and hotspot.
	 * also jw_restore does not have RESPAT set so there is no cursor pattern set
	 */
	/* ask for 2+ planes, get however many window has.
	 * ask for 1 plane, get 1 plane.
	 * bm_colours was set to MONOCHROME in gk0xwinalloc */
	if(dd->d_colours!=MONOCHROME){
		newwin->w_bm->bm_colours = 1<<watt.depth;
		dd->d_colours = newwin->w_bm->bm_colours;
	}
	gk0xcorep(newwin->w_bm,0,0,0,0,0,0,COSETDEFAULT);
	gk0xwwseticonpic(icon);
	gk0xcuinit(newwin);
	return(newwin);
}
