/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
#include <signal.h>
#include <X11/Xatom.h>
#include <sys/types.h>
#ifndef FD_SETSIZE
/* cannot typedef int fd_set; as on Sun fd_set is a struct already */
# define fd_set int
# define FD_SETSIZE		32
# define FD_CLR(fd,in)		*(in) &= ~(1<<fd)
# define FD_SET(fd,in)		*(in) |= 1<<fd
# define FD_ISSET(fd,in)	*(in)&(1<<fd)
#endif FD_SETSIZE
PUBLIC int gk0xddselect(fd,flag)int fd,flag;{
	int was;
	if(flag==0 && dd->d_selectmore==0){
		dd->d_selectmore = (long*)gk0xclrmalloc(FD_SETSIZE/8);
		dd->d_selectlength = FD_SETSIZE;
		dd->d_selectmore[0] = dd->d_select;
		return(0);
	}
	if(fd>=dd->d_selectlength)gk0xwwpanic("gk0xddselect: fd>=d_selectmore");
	was = !!FD_ISSET(fd,(fd_set*)dd->d_selectmore);
	if(flag==WWSELCLEAR){
		FD_CLR(fd,(fd_set*)dd->d_selectmore);
		dd->d_select &= ~(1<<fd);
	}else if(flag==WWSELSET){
		FD_SET(fd,(fd_set*)dd->d_selectmore);
		dd->d_select |= 1<<fd;
	}else if(flag!=WWSELTEST)gk0xwwpanic("gk0xddselect: bad flag");
	return(was);
}

PRIVATE void gk0xipon();
PRIVATE void gk0xipoff();
#define ModsMask (Mod1Mask|Mod2Mask|Mod3Mask|Mod4Mask|Mod5Mask)
PRIVATE void checkshiftkeys(state)int state;{
	int scm;
	if(!(jwin(ddwin)->jw_restore&RESIPRAW))return;
	scm = 	((state&ShiftMask)?IPSHIFT:0) |
		((state&ControlMask)?IPCONTROL:0) |
		((state&ModsMask)?IPMETA:0) ;
	if(scm != dd->d_shift){
		dd->d_shift = scm;
		dd->d_event = IPSHIFTED;
	}
}
#ifdef GETHANDLEFROMX
/*
 * Cannot ask for property of a deleted window, even if just been handed
 * an event (eg Leave) for it! protocol error
 */
PRIVATE window *gk0xgetourhandle(xwindow)Window xwindow;{
	unsigned long numitems;
	Atom actualtype;
	int actualformat,i;
	long bytesafter;
	char *winptr,*toptr;
	window *win;
	if(XGetWindowProperty(WXD,xwindow,jdd->jd_backhandle,
		0L/*offset*/,4L/*length*/,False/*delete*/,XA_INTEGER,
		&actualtype,&actualformat,&numitems,&bytesafter,
		&winptr)!=Success
		|| numitems!=4 || bytesafter!=0)
		gk0xwwpanic("ipwait: couldnt get back handle on window");
	toptr = (char*)&win;
	/* winptr may not be aligned for simple coercion */
	for(i=0;i<4;i++)*toptr++ = *winptr++;
	if(win!=ddwin){
		window *wp;
		for(wp = HEADWIN;wp!=0 && wp!=win;wp = NEXTWIN(wp));
		if(wp==0)win = 0;	/* input from defunct window!!! */
	}
	return(win);
}
#else GETHANDLEFROMX
PRIVATE window *gk0xgetourhandle(xwindow)Window xwindow;{
	window *wp;
	for(wp = HEADWIN;wp!=0 && jwin(wp)->jw_xwindow!=xwindow;wp = NEXTWIN(wp));
	return(wp);	/* 0 for input from defunct window!!! */
}
#endif GETHANDLEFROMX
PRIVATE void gk0xsetxy(x,y){
	dd->d_x = x+jwin(ddwin)->jw_jcursor.jc_cursor.c_xoffset;
	dd->d_y = y+jwin(ddwin)->jw_jcursor.jc_cursor.c_yoffset;
}
PRIVATE Window ourparent(xwindow)Window xwindow;{
	Window root,parentwin,*children;
	unsigned int number;
	if(XQueryTree(WXD,xwindow,&root,&parentwin,&children,&number)==0)
		return(0);
	if(children!=0)XFree((char*)children);
	if(parentwin==root)return(0);
	return(parentwin);
}
/*
 * See if this xwindow is parent of one of our windows
 */
PRIVATE window *ourchildof(xwindow)Window xwindow;{
	Window root,parentwin,*children;
	unsigned int number;
	window *win;
	if(XQueryTree(WXD,xwindow,&root,&parentwin,&children,&number)==0)
		return(0);
	while(number!=0){
		win = gk0xgetourhandle(*children);
		if(win!=0)break;
		number--;
	}
	if(children!=0)XFree((char*)children);
	return(number!=0?win:0);
}
/*
 * all events (unioned) begin:
 * int type;		of event
 * Display *display;	Display the event was read from
 * Window window;	event" window reported relative to
 */
#define MAXEXPOSE	10	/* num exposes with button down before give to user */
/* root subwindow time x y x_root y_root state is_hint same_screen */
/* let's collapse lots of boring motion together, since
 * things are running so slowly in X */
PRIVATE void wwMotionNotify(ev)XEvent ev;{	/* (XMotionEvent)&ev */
	while(ev.type==MotionNotify){
		gk0xsetxy(ev.xmotion.x,ev.xmotion.y);
		if(QLength(WXD)>0){
			XPeekEvent(WXD,&ev);
			if(ev.type==MotionNotify)XNextEvent(WXD,&ev);
		}else ev.type = MotionNotify-1;
	}
	checkshiftkeys(ev.xmotion.state);
}
/* root subwindow time x y x_root y_root state button same_screen */
/* all buttons in ev.xbutton.state: Button1Mask etc ??? */
PRIVATE void wwButtonPress(ev)XButtonEvent ev;{
	switch(ev.button){
	case Button1:
		dd->d_buttons |= dd->d_buttonmap[BUTINDEXLEFT];
		break;
	case Button2:
		dd->d_buttons |= dd->d_buttonmap[BUTINDEXMIDDLE];
		break;
	case Button3:
		dd->d_buttons |= dd->d_buttonmap[BUTINDEXRIGHT];
		break;
	}
	gk0xsetxy(ev.x,ev.y);
}
PRIVATE void wwButtonRelease(ev)XButtonEvent ev;{
	switch(ev.button){
	case Button1:
		dd->d_buttons &= ~dd->d_buttonmap[BUTINDEXLEFT];
		break;
	case Button2:
		dd->d_buttons &= ~dd->d_buttonmap[BUTINDEXMIDDLE];
		break;
	case Button3:
		dd->d_buttons &= ~dd->d_buttonmap[BUTINDEXRIGHT];
		break;
	}
	gk0xsetxy(ev.x,ev.y);
}
PRIVATE void wwMappingNotify(ev)XEvent ev;{
	if(ev.xmapping.request==MappingKeyboard)
		XRefreshKeyboardMapping(&ev);
}
/* type display drawable x y width height count major_code minor_code */
/* eg scroll up from hidden region */
/* major_code;	core is CopyArea or CopyPlane */
/* (XExposeEvent)&ev */
/* x y width height count */
#define MAXEXPOSE	10	/* num exposes with button down before give to user */
PRIVATE void wwExpose(ev)XEvent ev;{	/* (XGraphicsExposeEvent)&ev */
	jwindow *jwp;
	window *win;
	win = gk0xgetourhandle(ev.xexpose.window);
	if(win==0)return;	/* ignore from defunct window */
	dd->d_newwindow = win;
	jwp = jwin(win);
	if(!(jdd->jd_flags&JUSETRICK)){
		static int exposecount = 0;
		/* flush out any other incoming expose events: */
		/* if(ev.xexpose.count!=0) */
		while(0!=XCheckWindowEvent(WXD,jwp->jw_xwindow,ExposureMask,&ev))
			exposecount++;
		/* let's try and hold back these events until a button is
		 * released, or there are lots of them, to try to stop
		 * naive programs doing a lot of repainting */
		if(dd->d_buttons==0 || exposecount>=MAXEXPOSE){
			dd->d_event = IPSIZE;	/* pseudo size event */
			exposecount = 0;
			/* dd->d_buttons = 0; */
		}/* else dd->d_event = IPOTHER; */
	}else{
		if(jdd->jd_flags&JREPORT)fprintf(stderr,"GraphicsExpose or Expose\n");
		/* ignore these: window is repainted from retained pixmap
		 * which is really the background tile */
		while(0!=XCheckWindowEvent(WXD,jwp->jw_xwindow,ExposureMask,&ev))
			;
	}
}
/* type display event window x y width height border_width above override_redirect */
/* change in size, position, border or stacking order */
PRIVATE void wwConfigureNotify(ev)XEvent ev;{	/* (XConfigureEvent)&ev */
	jwindow *jwp;
	window *win;
	win = gk0xgetourhandle(ev.xconfigure.window);
	if(win==0)return;	/* ignore from defunct window */
	dd->d_newwindow = win;
	jwp = jwin(win);
	if(win->w_bm->bm_box.b_right != ev.xconfigure.width-1 ||
	   win->w_bm->bm_box.b_bottom != ev.xconfigure.height-1){
		dd->d_event = IPSIZE;
		if(!(jdd->jd_flags&JUSETRICK)){
			win->w_bm->bm_box.b_right = ev.xconfigure.width-1;
			win->w_bm->bm_box.b_bottom = ev.xconfigure.height-1;
		}else gk0xwinsetraster(win,ev.xconfigure.width,ev.xconfigure.height);
	}/* else dd->d_event = IPOTHER; */
	win->w_xrel = ev.xconfigure.x;
	win->w_yrel = ev.xconfigure.y;
}
/* root subwindow time x y x_root y_root mode detail same_screen focus state */
/* mode = NotifyNormal, NotifyGrab, NotifyUngrab */
/* detail = NotifyAncestor, NotifyVirtual, NotifyInferior, NotifyNonLinear,NotifyNonLinearVirtual */
PRIVATE void wwEnterNotify(ev)XEvent ev;{ /* (XEnterWindowEvent==XCrossingEvent)&ev */
	jwindow *jwp;
	window *win;
	win = gk0xgetourhandle(ev.xcrossing.window);
	if(win==0)return;	/* ignore from defunct window */
	dd->d_newwindow = win;
	jwp = jwin(win);
	gk0xsetxy(ev.xcrossing.x,ev.xcrossing.y);
	/* cannot call checkshiftkeys(ev...state) without ev.state var */
	dd->d_shift = 0;
	if(!(win->w_flags&WINSIDE)){
		dd->d_event = IPENTER;
		win->w_flags |= WINSIDE;
	}
	if((jdd->jd_flags&JINSTALLMAP) && jwp->jw_cmap!=0)
		XInstallColormap(WXD,jwp->jw_cmap);
	XSetWindowBorder(WXD,jwp->jw_xwindow,
		(jdd->jd_flags&JWHITEBORDER)?WhitePixel(WXD,WXS):BlackPixel(WXD,WXS));
}
PRIVATE void wwLeaveNotify(ev)XEvent ev;{ /* (XLeaveWindowEvent==XCrossingEvent)&ev */
	jwindow *jwp;
	window *win;
	win = gk0xgetourhandle(ev.xcrossing.window);
	if(win==0)return;	/* ignore from defunct window */
	dd->d_newwindow = win;
	jwp = jwin(win);
	dd->d_shift = 0;
	gk0xsetxy(ev.xcrossing.x,ev.xcrossing.y);
	if(win->w_flags&WINSIDE){
		dd->d_event = IPLEAVE;
		win->w_flags &= ~WINSIDE;
	}
#ifdef WANTBLACKBORDER
	XSetWindowBorder(WXD,jwp->jw_xwindow,BlackPixel(WXD,WXS));
#else WANTBLACKBORDER
	{	static Pixmap greyborder = 0;
		if(greyborder==0){
			bitmap *bm;
			bm = gk0xxxgrey(50,0);
			greyborder = jbm(bm)->jbm_memory;
			jbm(bm)->jbm_memory = 0;
			gk0xbmfree(bm);
		}
		XSetWindowBorderPixmap(WXD,jwp->jw_xwindow,greyborder);
	}
#endif WANTBLACKBORDER
	if((jdd->jd_flags&JINSTALLMAP) && jwp->jw_cmap!=0)
		XUninstallColormap(WXD,jwp->jw_cmap);
}
/* type display (Window)event! window */
PRIVATE void wwDestroyNotify(ev)XEvent ev;{	/* (XDestroyWindowEvent)&ev */
	window *win;
	win = gk0xgetourhandle(ev.xdestroywindow.window);
	if(win==0)return;	/* ignore from defunct window */
	dd->d_newwindow = win;
	dd->d_event = IPKILLED;
	if(jdd->jd_flags&JREPORT)fprintf(stderr,"DestroyNotify\n");
}
/* With uwm we can see the window is iconised by getting unmap events,
   but with awm it re-parents the window to give it borders, and unmapping
   the parent doesnt give an unmap for the kids.  All we can get is a
   visibility event.  Then poll XA_WM_STATE to see what state the winman
   thinks the window is in.  This field doesnt seem to change. */
/* type display event window override-redirect */
PRIVATE void wwMapNotify(ev)XEvent ev;{ /* (XMapEvent)&ev */
	window *win;
	if(jdd->jd_flags&JVISIBILITY)return;
	win = gk0xgetourhandle(ev.xmap.window);
	if(win==0 && (jdd->jd_flags&JPARENT))
		win = ourchildof(ev.xmap.window);
	if(win==0)return;	/* ignore from defunct window */
	dd->d_newwindow = win;
	win->w_flags &= ~(WICONSEEN|WSEEN);
	win->w_flags |= WSEEN;
	dd->d_event = IPSEEN;
}
/* type display event window from_configure */
PRIVATE void wwUnmapNotify(ev)XEvent ev;{ /* (XunmapEvent)&ev */
	window *win;
	if(jdd->jd_flags&JVISIBILITY)return;
	win = gk0xgetourhandle(ev.xunmap.window);
	if(win==0 && (jdd->jd_flags&JPARENT))
		win = ourchildof(ev.xunmap.window);
	if(win==0)return;	/* ignore from defunct window */
	dd->d_newwindow = win;
	win->w_flags &= ~(WICONSEEN|WSEEN);
	win->w_flags |= WICONSEEN;
	dd->d_event = IPNOTSEEN;
}
PRIVATE void wwReparentNotify(ev)XEvent ev;{
	Window pop;
	if(jdd->jd_flags&JPARENT &&
	  (pop = ourparent(ev.xreparent.window))!=0)
		XSelectInput(WXD,pop,StructureNotifyMask);
	else if(jdd->jd_flags&JREPORT)fprintf(stderr,"ReparentNotify\n");
}
PRIVATE void wwVisibilityNotify(ev)XEvent ev;{/* (XVisibilityEvent)&ev */
	window *win;
	/* type display window state */
	if(!(jdd->jd_flags&JVISIBILITY))return;
	win = gk0xgetourhandle(ev.xvisibility.window);
	if(win==0)return;	/* ignore from defunct window */
	if((ev.xvisibility.state!=VisibilityFullyObscured)
	 !=((win->w_flags&WSEEN)==WSEEN)){
		dd->d_newwindow = win;
		win->w_flags &= ~(WICONSEEN|WSEEN);
		if(ev.xvisibility.state!=VisibilityFullyObscured){
			win->w_flags |= WSEEN;
			dd->d_event = IPSEEN;
		}else{
			win->w_flags |= WICONSEEN;
			dd->d_event = IPNOTSEEN;
		}
	}
#ifdef STATEPROPERTYCHANGES
/* no point doing this as XA_WM_STATE doesnt change value in uwm or awm */
	{	unsigned long numitems;
		Atom actualtype;
		int actualformat,i;
		long bytesafter;
		struct state{ int thestate; Window icon; }state;
		char *stateptr,*toptr;
	if(XGetWindowProperty(WXD,xwindow,XA_WM_STATE,
		0L/*offset*/,4L/*length*/,False/*delete*/,XA_INTEGER,
		&actualtype,&actualformat,&numitems,&bytesafter,
		&stateptr)!=Success
		|| numitems!=4 || bytesafter!=0)
		gk0xwwpanic("ipwait: couldnt get xa_wm_state");
		state = (state)*stateptr;
		if(state.thestate==IconicState)...
	}
#endif STATEPROPERTYCHANGES
}
PRIVATE void wwdefaultinput(ev)XEvent ev;{
	typedef struct{
		int eventtype;
		char *eventname;
	}unevent;
	static unevent unevents[] = {
	NoExpose,"NoExpose",
	FocusIn,"FocusIn",
	FocusOut,"FocusOut",
	KeymapNotify,"KeymapNotify",
	VisibilityNotify,"VisibilityNotify",
	CreateNotify,"CreateNotify",
	DestroyNotify,"DestroyNotify",
	MapRequest,"MapRequest",
	ConfigureRequest,"ConfigureRequest",
	GravityNotify,"GravityNotify",
	ResizeRequest,"ResizeRequest",
	CirculateNotify,"CirculateNotify",
	CirculateRequest,"CirculateRequest",
	PropertyNotify,"PropertyNotify",
	SelectionClear,"SelectionClear",
	SelectionRequest,"SelectionRequest",
	SelectionNotify,"SelectionNotify",
	ColormapNotify,"ColormapNotify",
	ClientMessage,"ClientMessage",
	MappingNotify,"MappingNotify",
	0,"unknown event"};

	if(jdd->jd_flags&JREPORT){
		unevent *unp;
		unp = unevents;
		while(unp->eventtype!=0 && unp->eventtype!=ev.type)unp++;
		fprintf(stderr,"%s\n",unp->eventname);
	}
	/* (XAnyEvent)&ev ev.xany */
	/* dd->d_event = IPOTHER; */
	/* cannot call checkshiftkeys(ev...state) without ev.state var */
}
static int sample = FALSE;
PUBLIC int gk0xipxwait(){
	XEvent ev;
	jwindow *jwp;
	window *win;
	static char buf[80],*bp;
	static int blen = 0;

	dd->d_flags &= ~WWINPUTREADY;
	if(blen>0){	/* deal first with buffered input keys */
		dd->d_event = IPKEY;
		dd->d_char = *bp++;
		blen--;
		if(blen>0)dd->d_flags |= WWINPUTREADY;
		return(blen);
	}
	jwp = jwin(ddwin);
/*	XPutBackEvent(WXD,&ev); many times
*/
	dd->d_event = IPOTHER;
	if(sample && QLength(WXD)==0){
		XSync(WXD,FALSE);
		if(QLength(WXD)==0){
			return(0);
		}
	}else if(jdd->jd_flags&JSYNCHINPUT)XSync(WXD,FALSE);
	else XFlush(WXD);
	if(QLength(WXD)==0){
	/* do our own select because X wont return from XNextEvent, even if
	 * interrupted, until there is an event. We want sigalrm,eg to cancel
	 */
		fd_set ibits,ebits;
		int numfds;
		static int lastselect;	/* holds old d_select */
		/* copy any bits set by user in previous release d_select */
		*dd->d_selectmore |= dd->d_select;
		/* clear any bits cleared by user in d_select */
		*dd->d_selectmore &= ~(~dd->d_select & lastselect);
		lastselect = dd->d_select;
		/* what devices to wait on: all windows, ip on or not */
		ebits = *(fd_set*)dd->d_selectmore;
		ibits = *(fd_set*)dd->d_selectmore;
		numfds = select(dd->d_selectlength,&ibits,(fd_set*)0,&ebits,(struct timeval *)0);
		if(numfds==-1) return(0);/* interrupted */
	}
	XNextEvent(WXD,&ev);	/* flush and wait */

	switch(ev.type){	/* ?==Success */
	case KeyPress:	/* (XKeyEvent)&ev */
		/* not KeyRelease */
		/* root subwindow time x y x_root y_root state keycode same_screen */
		blen = XLookupString(&ev,buf,sizeof(buf),(KeySym*)0,/*(XComposeStatus*)*/0);
		if(blen>0){
			blen--;
			bp = buf;
			dd->d_char = *bp++;
			checkshiftkeys(ev.xkey.state); /* can set d_event */
			dd->d_event = IPKEY;
		}else checkshiftkeys(ev.xkey.state);
		gk0xsetxy(ev.xkey.x,ev.xkey.y);
		break;
	case MotionNotify:wwMotionNotify(ev);
		break;
	case ButtonPress:wwButtonPress(ev);
		break;
	case ButtonRelease:wwButtonRelease(ev);
		break;
	case MappingNotify:wwMappingNotify(ev);
		break;
	case GraphicsExpose:
	case Expose:wwExpose(ev);
		break;
	case ConfigureNotify:wwConfigureNotify(ev);
		break;
	case EnterNotify:wwEnterNotify(ev);
		break;
	case LeaveNotify:wwLeaveNotify(ev);
		break;
	case DestroyNotify:wwDestroyNotify(ev);
		break;
	case MapNotify:wwMapNotify(ev);
		break;
	case UnmapNotify:wwUnmapNotify(ev);
		break;
	case ReparentNotify:wwReparentNotify(ev);
		break;
	case VisibilityNotify:wwVisibilityNotify(ev);
		break;
#ifdef LATERX
	/* flush out any other incoming expose events: */
		while(0!=XCheckWindowEvent(WXD,jwp->jw_xwindow,ExposureMask,&ev));
		ev.xgraphicsexpose
		break;
	case NoExpose:		/* (XNoExposeEvent)&ev */
	/* type display drawable major_code minor_code */
	/* ignore. told our rasterop did incl any obscured region */
		ev.xnoexpose
		break;
	case SelectionClearEvent:
	case SelectionRequestEvent:
	/* (Atom)selection time
		XSelectionClearEvent */
	/* type display (Window)owner requestor selection target property time
		XSelectionRequestEvent */
	/* type display requestor selection target property time
		XSelectionEvent */
		break;
#endif LATERX
	default: wwdefaultinput(ev);
		break;
	}
	if(QLength(WXD)!=0)dd->d_flags |= WWINPUTREADY;
	return((dd->d_flags&WWINPUTREADY)!=0);
}
/*
 * remember or return last cursor set on fullscreen.
 * XDefineCursor on root window will set it! so dont do it until input on.
 */
DDPUBLIC Cursor setscreencursor(wp,xc)window *wp;Cursor xc;{
	static Cursor remember = 0;
	if(xc!=0){
		if(jwin(wp)->jw_restore & RESIPON)
#ifdef XSERVERBUG
			XDefineCursor(WXD,jwin(wp)->jw_xwindow,xc);
#else XSERVERBUG
	/* release and regrab to change cursor pattern as otherwise
	   server (release 2) crashes */
			XUngrabPointer(WXD,CurrentTime);
			XGrabPointer(WXD,jwin(wp)->jw_xwindow,FALSE/*?*/,
			ButtonPressMask|
			ButtonReleaseMask|
			KeyPressMask|
			PointerMotionMask,
			GrabModeAsync/*?*/,
			GrabModeAsync/*?*/,
			None/*confine to */,
			xc,
			CurrentTime);/*==GrabSuccess*/
#endif XSERVERBUG
		if(remember!=0)XFreeCursor(WXD,remember);
		remember = xc;
	}
	if(remember!=0)return remember;
	return None;
}
PRIVATE void gk0xipsetscreen(win,flag)window *win;{
	jwindow *jwp;

	jwp = jwin(win);
	switch(flag){
	case IPON:
	case IPREQUEST:
	case IPSAMPLE:
		if(dd->d_ipwait==0)dd->d_ipwait = gk0xipxwait;
		sample = flag==IPSAMPLE;
		if(jwp->jw_restore & RESIPON)return;
		if(XGrabPointer(WXD,jwp->jw_xwindow,FALSE/*?*/,
			ButtonPressMask|
			ButtonReleaseMask|
			KeyPressMask|
			PointerMotionMask,
			GrabModeAsync/*?*/,
			GrabModeAsync/*?*/,
			None/*confine to */,
			setscreencursor(win,(Cursor)0),
			CurrentTime)==GrabSuccess){
			jwp->jw_flags |= JWSCREENIPON;
			jwp->jw_restore |= RESIPON;
		}
		break;
	case IPOFF:
		if(!(jwp->jw_restore & RESIPON))return;
		XUngrabPointer(WXD,CurrentTime);
		jwp->jw_flags &= ~JWSCREENIPON;
		jwp->jw_restore &= ~RESIPON;
		break;
	default:
		gk0xworry("unknown flag to ipset");
	}
}
/*
 * set the input cursor mode
 */
PUBLIC void gk0xipxset(wp,flag) window *wp;int flag;{
	if(jwin(wp)->jw_flags&JWSCREEN){
		gk0xipsetscreen(wp,flag);
		return;
	}
	switch(flag){
	case IPON:
	case IPNOKEY:
	case IPRAW:
		gk0xipon(wp,flag);
		if(flag==IPRAW)jwin(wp)->jw_restore |= RESIPRAW;
		else jwin(wp)->jw_restore &= ~RESIPRAW;
		return;
	case IPOFF:
		gk0xipoff(wp);
		return;
	case IPREQUEST:
		gk0xipon(wp,flag);
		sample = FALSE;
		break;
	case IPSAMPLE:
		gk0xipon(wp,flag);
		sample = TRUE;
		break;
	default:
		gk0xworry("unknown flag to ipset");
	}
}
PRIVATE void gk0xipon(wp,flag)window *wp;{
	jwindow *jw;
	Window pop;
	int mask;

	jw = jwin(wp);
	if(jw->jw_restore & RESIPON)return;
	if(dd->d_ipwait==0)dd->d_ipwait = gk0xipxwait;
	jw->jw_restore |= RESIPON;
	mask =  ButtonPressMask|
		ButtonReleaseMask|
		EnterWindowMask|
		LeaveWindowMask|
		KeyPressMask|
		PointerMotionMask|
		ExposureMask|
		StructureNotifyMask; /* for size, posn, destroy */
	if(jdd->jd_flags&JVISIBILITY)mask |= VisibilityChangeMask;
	if(jdd->jd_flags&JREPORT)
	mask |= KeymapStateMask|
		VisibilityChangeMask|
		ResizeRedirectMask|
		SubstructureNotifyMask|
		SubstructureRedirectMask|
		FocusChangeMask|
		PropertyChangeMask|
		ColormapChangeMask|
		OwnerGrabButtonMask|
		KeyReleaseMask|
		PointerMotionHintMask;
		/* |Button1MotionMask (2345) */
	XSelectInput(WXD,jw->jw_xwindow,mask);
	if(jdd->jd_flags&JPARENT && (pop = ourparent(jw->jw_xwindow))!=0)
		/* ask for parent Map unMap events */
		XSelectInput(WXD,pop,StructureNotifyMask);
	gk0xipxset(wp,IPREQUEST);
}
PRIVATE void gk0xipoff(wp)window *wp;{
	jwindow *jwp;

	jwp = jwin(wp);
	if(!(jwp->jw_restore & RESIPON))
		return;
	jwp->jw_restore &= ~RESIPON;	/* leave only this bit set */
}
