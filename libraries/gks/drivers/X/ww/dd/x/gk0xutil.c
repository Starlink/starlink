/*
 *  copyright (c) Mark M Martin. RAL. 1986
 * This source may be copied, distributed, altered or used, but not sold
 * for profit. It is not in the public domain. This notice should remain
 * in the source unaltered, and any changes to the source made by
 * people other than the author should be marked as such.
 */
#include "dd.h"
#include <signal.h>
/*
 * return the pnx/whitechapel/sun rasteroperation:
 * WWCOPY WWXOR WWOR WWAND and the not-source of these
 */
#define LINEOP rasop
static int rasop[] = { GXcopy, GXxor, GXor, GXand, GXcopyInverted, GXequiv, GXorInverted, GXandInverted };
DDPUBLIC int gk0xwwrasop(x){
	if(x<0 || x>7)gk0xwwpanic("illegal raster operation");
	return(rasop[x]);
}
DDPUBLIC int gk0xwwlineop(x){
	if(x<0 || x>7)gk0xwwpanic("illegal line raster operation");
	return(LINEOP[x]);
}
/*
 * need to do opposite of rasterop (not source)
 * in ReverseVideo windows (not pixmaps)
 */
DDPUBLIC void gk0xsetbmrop(xrop,bm)bitmap *bm;{
	static int notsrc[16] = { GXset, GXandInverted, GXnor, GXcopyInverted,
		GXand, GXnoop, GXequiv, GXorInverted, GXandReverse, GXxor,
		GXinvert, GXnand, GXcopy, GXor, GXorReverse,GXclear };
	if((jdd->jd_flags&JDREVERSE) && jbm(bm)->jbm_jwindow!=0)
		xrop = notsrc[xrop&15];
	setgcrop(xrop);
}
DDPUBLIC void setgcrop(xrop){
	if(xrop!=jdd->jd_xgcvalues.function){
		XSetFunction(WXD,WXGC,xrop);
		jdd->jd_xgcvalues.function = xrop;
	}
	if(dd->d_fore!=jdd->jd_xgcvalues.foreground){
		jdd->jd_xgcvalues.foreground = dd->d_fore;
		XSetForeground(WXD,WXGC,jdd->jd_xgcvalues.foreground);
	}
	if(dd->d_back!=jdd->jd_xgcvalues.background){
		jdd->jd_xgcvalues.background = dd->d_back;
		XSetBackground(WXD,WXGC,jdd->jd_xgcvalues.background);
	}
}
DDPUBLIC char *setxshare(what,rlength,wlength)char *what;int *rlength;{
	if(what==0)return(XFetchBytes(WXD,rlength));
	XStoreBytes(WXD,what,wlength);
	return(0);
}
/*
 *	XDisplayName(str) returns DISPLAY value if str==0
*/
static FILE *console = 0;
static int errorreport = 1;	/* 0 or 99 */
PRIVATE void openerror(){
	if(console!=0)return;
	console = fopen("/dev/console","w");
	if(console==NULL)console = stderr;
}
PRIVATE int wwXerror(dpy,event)Display *dpy;XErrorEvent *event;{
	char buffer[BUFSIZ];
	char mesg[BUFSIZ];
	char number[32];
	char *mtype = "XlibMessage";

	if(errorreport==0)return(0);
	openerror();
	XGetErrorText(dpy,event->error_code,buffer,BUFSIZ);
	XGetErrorDatabaseText(dpy,mtype,"XError","X Error",mesg,BUFSIZ);
	fprintf(console,"%s: %s\n",mesg,buffer);
	XGetErrorDatabaseText(dpy,mtype,"MajorCode","Request Major code %d",mesg,BUFSIZ);
	fprintf(console,mesg,event->request_code);
	sprintf(number,"%d",event->request_code);
	XGetErrorDatabaseText(dpy,"XRequest",number,"",	buffer,BUFSIZ);
	fprintf(console," %s\n",buffer);
	XGetErrorDatabaseText(dpy,mtype,"MinorCode","Request Minor code",mesg,BUFSIZ);
	fprintf(console,mesg,event->minor_code);
	fputs("\n",console);
	XGetErrorDatabaseText(dpy,mtype,"ResourceID","ResourceID 0x%x",mesg,BUFSIZ);
	fprintf(console,mesg,event->resourceid);
	fputs("\n",console);
	XGetErrorDatabaseText(dpy,mtype,"ErrorSerial","Error Serial #%d",mesg,BUFSIZ);
	fprintf(console,mesg,event->serial);
	fputs("\n",console);
	XGetErrorDatabaseText(dpy,mtype,"CurrentSerial","Current Serial #%d",mesg,BUFSIZ);
	fprintf(console,mesg,dpy->request);
	fputs("\n",console);
	/* if (event->error_code == BadImplementation)could continue  */
	fflush(console);
	if(errorreport!=1)return(0);
	kill(getpid(),3);	/* get core dump */
	exit(1);
}
/*
 * for XSetIOErrorHandler
 */
PRIVATE int wwXfatal(dis)Display *dis;{
	openerror();
	fprintf(console,"ww: connection to X server lost\n");
	fflush(console);
	kill(getpid(),1);
	exit(1);
}
/*
 * initialise, but dont open any windows yet. return true if ok.
 * dont panic so easily when things cant be found or done. use defaults
 */
static int wwrunning = FALSE;
static int the_tty;	/* for bells and ioctl to get edit chars */
#define DEFAULTERASE	'\177'	/* default erase char if cant open tty */
#define DEFAULTKILL	'\025'	/* default kill char */
DDPUBLIC int gk0xwwstartup(colours){
	if(wwrunning)return(TRUE);
	if(dd==NULLPTR(wwstate))dd = STRUCTMALLOC(wwstate);
	if(jdd==NULLPTR(jwwstate))jdd = STRUCTMALLOC(jwwstate);
	dd->d_buttonmap[BUTINDEXLEFT] = ITEMBUTTON;
	dd->d_buttonmap[BUTINDEXMIDDLE] = MENUBUTTON;
	dd->d_buttonmap[BUTINDEXRIGHT] = SHOWBUTTON;
	gk0xddselect(0,WWSELINIT);
	the_tty = open("/dev/tty",2);		/* open 'the window' */
	dd->d_magic.mc_erase = DEFAULTERASE;
	dd->d_magic.mc_kill = DEFAULTKILL;
	dd->d_magic.mc_delword = '\027';	/* ^w */
	dd->d_magic.mc_retype = '\022';	/* ^r */
	if(the_tty>=0){
		if(ioctl(the_tty,TIOCGETP,&jdd->jd_sg)>=0){
			jdd->jd_flags |= JDSGVALID;
			if(jdd->jd_sg.sg_erase!='\0' && (unsigned char)jdd->jd_sg.sg_erase!=(unsigned char)'\377')
				dd->d_magic.mc_erase = jdd->jd_sg.sg_erase;
			if(jdd->jd_sg.sg_kill!='\0' && (unsigned char)jdd->jd_sg.sg_kill!=(unsigned char)'\377')
				dd->d_magic.mc_kill = jdd->jd_sg.sg_kill;
		}
	}

	WXD = XOpenDisplay((char*)0);
	if(WXD==0)wwfail("gk0xwwinit: cannot XOpenDisplay",FALSE);
	jdd->jd_xscreen = DefaultScreen(WXD);
	gk0xddselect(ConnectionNumber(WXD),WWSELSET);
	XSetErrorHandler(wwXerror);
	XSetIOErrorHandler(wwXfatal);
#ifdef GETHANDLEFROMX
	jdd->jd_backhandle = XInternAtom(WXD,"WWwindowptr",False);
#endif GETHANDLEFROMX

	dd->d_rop = WWCOPY;
	jdd->jd_xgcvalues.function = gk0xwwrasop(dd->d_rop);
	/* hell's teeth, this was a bad one: We dont want graphics exposure
	 * by default cos we get lots (1 per rasterop/line! to window or pixmap)
	 * of NoExpose events if we do. Only switch it on when doing bmcopy
	 * and source is a window */
	jdd->jd_xgcvalues.graphics_exposures = FALSE;
	jdd->jd_xgcvalues.subwindow_mode = IncludeInferiors;
	/* is there any problem leaving this permanently set? */
	jdd->jd_xgcvalues.foreground = BlackPixel(WXD,WXS);
	jdd->jd_xgcvalues.background = WhitePixel(WXD,WXS);
	jdd->jd_xgcvalues.plane_mask = AllPlanes;	/* is default */
	jdd->jd_root = RootWindow(WXD,WXS);
	WXGC = XCreateGC(WXD,jdd->jd_root,
		GCFunction|GCGraphicsExposures|GCSubwindowMode|GCForeground|GCBackground,
		&jdd->jd_xgcvalues);
/*	{
		int w,h,ww,hh;
		w = 64, h = 64;
		XQueryBestCursor(WXD,jdd->jd_root,w,h,&ww,&hh);
		/* lets look at what the best cursor is in dbx
		it takes anything...
	}
*/
	dd->d_line = WWXOR;
	dd->d_flags |= PRINTERR;
	gk0xparsewwoptions((char*)0);
	parseXoptions();
/*
 * on whitechapel hitech need to use 8 planes if screen 8 planes
 * This is now hidden by bmget, with d_colours being 2.
 */
	jdd->jd_depth = gk0xcoltodepth(gk0xwwask(ASKCOLOURS));
	dd->d_colours = colours;
	if(colours!=MONOCHROME)colours = 1<<jdd->jd_depth;
	if(colours<dd->d_colours)dd->d_colours = colours;
	if(jdd->jd_flags&JMONO){
		dd->d_back = 0;
		dd->d_fore = 255;
	}else if(dd->d_colours==MONOCHROME){
		dd->d_back = 0;	/* white */
		dd->d_fore = 1;	/* black, probably */
	}else{
		dd->d_back = 7;	/* white */
		dd->d_fore = 0;	/* black */
	}
	/* dd->d_ipwait = 0; should be ipxwait, but dont want to link it in */
	ddfont = gk0xftload(jdd->jd_defaultfont!=0?jdd->jd_defaultfont:NULLPTR(char));
	if(ddfont==0)return(FALSE);
	HEADWIN = NULLPTR(window);	/* end of an empty list of windows */
	wwrunning = TRUE;
	return(TRUE);
}
PRIVATE parseXoptions(){
	char *cp;

	cp = XGetDefault(WXD,"ww","ReverseVideo");
/* this doesnt seem to work: you get white on white pictures
 * which means you cant see any lines or characters. only rasterops.
 * fixed by inverting all rasterop sources */
 	if(cp!=0 && strcmp(cp,"on")==0)
		jdd->jd_flags |= JDREVERSE;

	XSynchronize(WXD,FALSE);
	cp = XGetDefault(WXD,"ww","InputSync");
	if(cp!=0)
	if(strcmp(cp,"all")==0)XSynchronize(WXD,TRUE);
	else if(strcmp(cp,"on")==0)jdd->jd_flags |= JSYNCHINPUT;
	else if(strcmp(cp,"off")==0)XSynchronize(WXD,FALSE);

	cp = XGetDefault(WXD,"ww","Trick");
	if(!(cp!=0 && strcmp(cp,"off")==0))jdd->jd_flags |= JUSETRICK;
	if(cp!=0 && strcmp(cp,"stipple")==0)jdd->jd_flags |= JSTIPPLE;

	cp = XGetDefault(WXD,"ww","InstallMap");
	if(cp!=0 && strcmp(cp,"mono")==0)jdd->jd_flags |= JMONO;
	else if(!(cp!=0 && strcmp(cp,"off")==0))jdd->jd_flags |= JINSTALLMAP;

	cp = XGetDefault(WXD,"ww","CopyArea");
	if(cp!=0 && strcmp(cp,"on")==0)jdd->jd_flags |= JCOPYAREA;

	cp = XGetDefault(WXD,"ww","Planes");
	if(cp!=0){
		jdd->jd_xgcvalues.plane_mask = atoi(cp);
		XSetPlaneMask(WXD,WXGC,jdd->jd_xgcvalues.plane_mask);
	}
#ifdef WWFORXWC
	jdd->jd_flags |= JNOCOPYPLANE;
#endif WWFORXWC
	cp = XGetDefault(WXD,"ww","CopyPlane");
	if(cp!=0 && strcmp(cp,"off")==0)jdd->jd_flags |= JNOCOPYPLANE;
	else if(cp!=0 && strcmp(cp,"on")==0)jdd->jd_flags &= ~JNOCOPYPLANE;

	cp = XGetDefault(WXD,"ww","ReportEvents");
	if(cp!=0 && strcmp(cp,"on")==0)jdd->jd_flags |= JREPORT;

	cp = XGetDefault(WXD,"ww","Border");
	if(cp!=0 && strcmp(cp,"white")==0)jdd->jd_flags |= JWHITEBORDER;

	cp = XGetDefault(WXD,"ww","Errors");
	if(cp!=0 && strcmp(cp,"ignore")==0)errorreport = 0;
	else if(cp!=0 && strcmp(cp,"report")==0)errorreport = 99;

	cp = XGetDefault(WXD,"ww","Iconise");
	if(cp!=0 && strcmp(cp,"visibility")==0)jdd->jd_flags |= JVISIBILITY;
	else if(cp!=0 && strcmp(cp,"parent")==0)jdd->jd_flags |= JPARENT;
}
/*
 * remove window from list of all windows and delete it.
 */
PUBLIC void gk0xwwfree(win)window *win;{
	window *wp;
	jwindow *jwp;

	jwp = jwin(win);
	wp = HEADWIN;
	if(wp==win)	/* window is head of list */
		HEADWIN = NEXTWIN(wp);
	else{
		while(wp!=NULLPTR(window) && NEXTWIN(wp)!=win)
			wp = NEXTWIN(wp);
		if(wp==NULLPTR(window))gk0xwwpanic("couldnt find window to free");
		NEXTWIN(wp) = NEXTWIN(win);		/* remove win from list */
	}
	XDestroyWindow(WXD,jwp->jw_xwindow);
	XFlush(WXD);
	win->w_bm->bm_window = 0;	/* for bmfree */
	gk0xbmfree(win->w_bm);
	if(win->w_icon!=0){
		gk0xbmfree(win->w_icon);
	}
	free((char *)win->w_junk);
	free((char *)win);
}
PUBLIC void gk0xwwxnoise(win)window *win;{
	if(dd && win && !(dd->d_flags&WWNONOISE))
	XBell(WXD,50/*percent vol*/);
}
PUBLIC void gk0xwwpanic(s) char *s;{
	fflush(stdout);
	openerror();
	fprintf(console,"ww panic: %.40s\n",s);
	fflush(console);
	kill(getpid(),3);	/* get core dump */
	exit(1);
}
/*
 * there are far too many things to free, and far too many to
 * reset to implement an exit that can really be followed by a wwinit.
 */
PUBLIC void gk0xwwexit(){
	if(!wwrunning)return;
	wwrunning = FALSE;
		close(the_tty);
	while(HEADWIN!=0){
		ddwin = HEADWIN;
		gk0xipset(IPOFF);
		gk0xwwfree(HEADWIN);
	}
	XCloseDisplay(WXD);
	free((char *)dd);
	free((char *)jdd);
}
PUBLIC int gk0xwwask(what) int what;{
	Display *display;
	int screen,retn;
	if(WXD==0){
		display = XOpenDisplay((char*)0);
		if(display==0)return(0);
		screen = DefaultScreen(display);
	}else display = WXD, screen = jdd->jd_xscreen;
	switch(what){
	case ASKXPPI:
		retn = DisplayWidth(display,screen)*25/DisplayWidthMM(display,screen);
		break;
	case ASKYPPI:
		retn = DisplayHeight(display,screen)*25/DisplayHeightMM(display,screen);
		break;
	case ASKXSCREEN:
		retn = DisplayWidth(display,screen);
		break;
	case ASKYSCREEN:
		retn = DisplayHeight(display,screen);
		break;
	case ASKCOLOURS:
		retn = 1<<DisplayPlanes(display,screen);
		break;
	default:
		retn = -1;
	}
	if(WXD==0)XCloseDisplay(display);
	if(retn==-1)gk0xwwpanic("wwask: bad flag");
	return(retn);
}
#ifndef WWFORXWC
PUBLIC int gk0xunportask(pointer,what) void *pointer; int what;{
	switch(what){
	case ASKBMMEMORY:
		if(jbm((bitmap *)pointer)->jbm_memory==0)
		return((int)jbm((bitmap *)pointer)->jbm_drawable);
		return((int)jbm((bitmap *)pointer)->jbm_memory);
	case ASKWINDOW:
		return((int)jwin((window *)pointer)->jw_xwindow);
	case ASKDISPLAY: /* Ask display */
		if (wwrunning) return((int)jdd->jd_xdisplay);
		else return(0);
	case ASKSCREEN: /* Ask screen */
		if (wwrunning) return((int)jdd->jd_xscreen);
		else return(0);
	default:
		return(WWERROR);
	}
}
#endif WWFORXWC
