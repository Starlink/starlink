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
PUBLIC int ddselect(fd,flag)int fd,flag;{
	int was;
	if(flag==0 && dd->d_selectmore==0){
		dd->d_selectmore = (long*)clrmalloc(FD_SETSIZE/8);
		dd->d_selectlength = FD_SETSIZE;
		dd->d_selectmore[0] = dd->d_select;
		return(0);
	}
	if(fd>=dd->d_selectlength)wwpanic("ddselect: fd>=d_selectmore");
	was = !!FD_ISSET(fd,(fd_set*)dd->d_selectmore);
	if(flag==WWSELCLEAR){
		FD_CLR(fd,(fd_set*)dd->d_selectmore);
		dd->d_select &= ~(1<<fd);
	}else if(flag==WWSELSET){
		FD_SET(fd,(fd_set*)dd->d_selectmore);
		dd->d_select |= 1<<fd;
	}else if(flag!=WWSELTEST)wwpanic("ddselect: bad flag");
	return(was);
}
PRIVATE void ipread();
PRIVATE void ipon();
PRIVATE void ipoff();
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
PRIVATE window *getourhandle(xwindow)Window xwindow;{
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
		wwpanic("ipwait: couldnt get back handle on window");
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
PRIVATE window *getourhandle(xwindow)Window xwindow;{
	window *wp;
	for(wp = HEADWIN;wp!=0 && jwin(wp)->jw_xwindow!=xwindow;wp = NEXTWIN(wp));
	return(wp);	/* 0 for input from defunct window!!! */
}
#endif GETHANDLEFROMX
PRIVATE void setxy(x,y){
	dd->d_x = x+jwin(ddwin)->jw_jcursor.jc_cursor.c_xoffset;
	dd->d_y = y+jwin(ddwin)->jw_jcursor.jc_cursor.c_yoffset;
}
/*
 * all events (unioned) begin:
 * int type;		of event
 * Display *display;	Display the event was read from
 * Window window;	event" window reported relative to
 */
#define MAXEXPOSE	10	/* num exposes with button down before give to user */
static int sample = FALSE;
PUBLIC int ipxwait(){
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
		if(numfds==-1)return(0);	/* interrupted */
	}
	XNextEvent(WXD,&ev);	/* flush and wait */

	switch(ev.type){	/* ?==Success */
	case MotionNotify:	/* (XMotionEvent)&ev */
	/* root subwindow time x y x_root y_root state is_hint same_screen */
		/* let's collapse lots of boring motion together, since
		 * things are running so slowly in X */
		while(ev.type==MotionNotify){
			setxy(ev.xmotion.x,ev.xmotion.y);
			if(QLength(WXD)>0){
				XPeekEvent(WXD,&ev);
				if(ev.type==MotionNotify)XNextEvent(WXD,&ev);
			}else ev.type = MotionNotify-1;
		}
		checkshiftkeys(ev.xmotion.state);
		break;
	case ButtonPress:	/* (XButtonEvent)&ev */
	/* root subwindow time x y x_root y_root state button same_screen */
	/* all buttons in ev.xbutton.state: Button1Mask etc ??? */
		switch(ev.xbutton.button){
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
		setxy(ev.xbutton.x,ev.xbutton.y);
		break;
	case ButtonRelease:
		switch(ev.xbutton.button){
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
		setxy(ev.xbutton.x,ev.xbutton.y);
		break;
	case KeyPress:		/* (XKeyEvent)&ev */
	/* not KeyRelease */
	/* root subwindow time x y x_root y_root state keycode same_screen */
		blen = XLookupString(&ev,buf,sizeof(buf),(KeySym*)0,
			/*(XComposeStatus*)*/0);
		if(blen>0){
			blen--;
			bp = buf;
			dd->d_char = *bp++;
			checkshiftkeys(ev.xkey.state); /* can set d_event */
			dd->d_event = IPKEY;
		}else checkshiftkeys(ev.xkey.state);
		setxy(ev.xkey.x,ev.xkey.y);
		break;
	case MappingNotify:
		if(ev.xmapping.request==MappingKeyboard)
			XRefreshKeyboardMapping(&ev);
		break;
	case GraphicsExpose:	/* (XGraphicsExposeEvent)&ev */
	/* type display drawable x y width height count major_code minor_code */
	/* eg scroll up from hidden region */
	/* major_code;	core is CopyArea or CopyPlane */
	case Expose:		/* (XExposeEvent)&ev */
	/* x y width height count */
	win = getourhandle(ev.xexpose.window);
	if(win==0)break;	/* ignore from defunct window */
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
		/* ignore these: window is repainted from retained pixmap
		 * which is really the background tile */
		while(0!=XCheckWindowEvent(WXD,jwp->jw_xwindow,ExposureMask,&ev))
			;
	}
		break;
	case ConfigureNotify:	/* (XConfigureEvent)&ev */
	/* type display event window x y width height border_width above override_redirect */
	/* change in size, position, border or stacking order */
		win = getourhandle(ev.xconfigure.window);
		if(win==0)break;	/* ignore from defunct window */
		dd->d_newwindow = win;
		jwp = jwin(win);
		if(win->w_bm->bm_box.b_right != ev.xconfigure.width-1 ||
		   win->w_bm->bm_box.b_bottom != ev.xconfigure.height-1){
			dd->d_event = IPSIZE;
			if(!(jdd->jd_flags&JUSETRICK)){
				win->w_bm->bm_box.b_right = ev.xconfigure.width-1;
				win->w_bm->bm_box.b_bottom = ev.xconfigure.height-1;
			}else winsetraster(win,ev.xconfigure.width,ev.xconfigure.height);
		}/* else dd->d_event = IPOTHER; */
		win->w_xrel = ev.xconfigure.x;
		win->w_yrel = ev.xconfigure.y;
		break;
	case EnterNotify: /* (XEnterWindowEvent==XCrossingEvent)&ev */
	/* root subwindow time x y x_root y_root mode detail same_screen focus state */
	/* mode = NotifyNormal, NotifyGrab, NotifyUngrab */
	/* detail = NotifyAncestor, NotifyVirtual, NotifyInferior, NotifyNonLinear,NotifyNonLinearVirtual */
		win = getourhandle(ev.xcrossing.window);
		if(win==0)break;	/* ignore from defunct window */
		dd->d_newwindow = win;
		jwp = jwin(win);
		setxy(ev.xcrossing.x,ev.xcrossing.y);
		/* cannot call checkshiftkeys(ev...state) without ev.state var */
		dd->d_shift = 0;
		if(!(win->w_flags&WINSIDE)){
			dd->d_event = IPENTER;
			win->w_flags |= WINSIDE;
		}
		if((jdd->jd_flags&JINSTALLMAP) && jwp->jw_cmap!=0)
			XInstallColormap(WXD,jwp->jw_cmap);
		XSetWindowBorder(WXD,jwp->jw_xwindow,WhitePixel(WXD,WXS));
		break;
	case LeaveNotify: /* (XLeaveWindowEvent==XCrossingEvent)&ev */
		win = getourhandle(ev.xcrossing.window);
		if(win==0)break;	/* ignore from defunct window */
		dd->d_newwindow = win;
		jwp = jwin(win);
		dd->d_shift = 0;
		setxy(ev.xcrossing.x,ev.xcrossing.y);
		if(win->w_flags&WINSIDE){
			dd->d_event = IPLEAVE;
			win->w_flags &= ~WINSIDE;
		}
		XSetWindowBorder(WXD,jwp->jw_xwindow,BlackPixel(WXD,WXS));
		break;
	case DestroyNotify:	/* (XDestroyWindowEvent)&ev */
	/* type display (Window)event! window */
		win = getourhandle(ev.xdestroywindow.window);
		if(win==0)break;	/* ignore from defunct window */
		dd->d_newwindow = win;
		dd->d_event = IPKILLED;
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
	case IPSEEN:
		ddwin->w_flags &= ~(WICONSEEN|WSEEN);
		ddwin->w_flags |= WSEEN;
		break;
	case IPNOTSEEN:
		ddwin->w_flags &= ~(WICONSEEN|WSEEN);
		ddwin->w_flags |= WICONSEEN;
		break;
#endif LATERX
	default: /* FocusIn FocusOut KeymapNotify VisibilityNotify CreateNotify
	DestroyNotify UnmapNotify MapNotify MapRequest ReparentNotify
	ConfigureRequest GravityNotify ResizeRequest
	CirculateNotify CirculateRequest PropertyNotify SelectionClear
	SelectionRequest SelectionNotify ColormapNotify ClientMessage
	MappingNotify */
		/* (XAnyEvent)&ev ev.xany */
		/* dd->d_event = IPOTHER; */
		/* cannot call checkshiftkeys(ev...state) without ev.state var */
		break;
	}
	if(QLength(WXD)!=0)dd->d_flags |= WWINPUTREADY;
	return((dd->d_flags&WWINPUTREADY)!=0);
}
PRIVATE void ipsetscreen(win,flag)window *win;{
	jwindow *jwp;

	jwp = jwin(win);
	switch(flag){
	case IPON:
	case IPREQUEST:
	case IPSAMPLE:
		if(dd->d_ipwait==0)dd->d_ipwait = ipxwait;
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
			None/*cursor */,
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
		worry("unknown flag to ipset");
	}
}
/*
 * set the input cursor mode
 */
PUBLIC void ipxset(wp,flag) window *wp;int flag;{
	if(!(jwin(wp)->jw_flags&JWWINDOW)){/* screen */
		ipsetscreen(wp,flag);
		return;
	}
	switch(flag){
	case IPON:
	case IPNOKEY:
	case IPRAW:
		ipon(wp,flag);
		if(flag==IPRAW)jwin(wp)->jw_restore |= RESIPRAW;
		else jwin(wp)->jw_restore &= ~RESIPRAW;
		return;
	case IPOFF:
		ipoff(wp);
		return;
	case IPREQUEST:
		ipon(wp,flag);
		sample = FALSE;
		break;
	case IPSAMPLE:
		ipon(wp,flag);
		sample = TRUE;
		break;
	default:
		worry("unknown flag to ipset");
	}
}
PRIVATE void ipon(wp,flag)window *wp;{
	jwindow *jw;

	jw = jwin(wp);
	if(jw->jw_restore & RESIPON)return;
	if(dd->d_ipwait==0)dd->d_ipwait = ipxwait;
	jw->jw_restore |= RESIPON;
	XSelectInput(WXD,jw->jw_xwindow,
		ButtonPressMask|
		ButtonReleaseMask|
		EnterWindowMask|
		LeaveWindowMask|
		KeyPressMask|
		PointerMotionMask|
		ExposureMask|
		StructureNotifyMask /* for size, posn, destroy */);
	/* others: KeymapStateMask VisibilityChangeMask ResizeRedirectMask
	SubstructureNotifyMask SubstructureRedirectMask FocusChangeMask
	PropertyChangeMask ColormapChangeMask OwnerGrabButtonMask
	KeyReleaseMask PointerMotionHintMask Button1MotionMask (2345) */
	ipxset(wp,IPREQUEST);
}
PRIVATE void ipoff(wp)window *wp;{
	jwindow *jwp;

	jwp = jwin(wp);
	if(!(jwp->jw_restore & RESIPON))
		return;
	jwp->jw_restore &= ~RESIPON;
}
