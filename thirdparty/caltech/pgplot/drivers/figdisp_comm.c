/* This file contains the routines used by the TVPCKG and PGPLOT libraries to */
/* access the figdisp display server. */

/* Sam Southard, Jr. */
/* Created: 18-Sep-1991 (from figdisp_xxx.c) */
/* 20-Sep-1991	SNS/CIT	figdisp_maxbuflen added. */
/* 14-Feb-1992	SNS/CIT	Now includes support for multiple devices */
/* 23-Sep-1992	SNS/CIT	Now allows for the server running on a machine with a */
/*			different byte order. */
/* 27-Sep-1992	SNS/CIT	SET_LG_CSCALE now uses ASCII strings. */
/* 23-Nov-1992	SNS/CIT	Now uses XSetErrorHandler so that we don't just go */
/*			away. */
/*  3-Jun-1994  TJP/CIT Create selection atom if it doesn't exist. */

/* The program include files */
#include "commands.h"

/* The system include files */
#include <stdio.h>

/* Get ntohs prototype or macro */

#ifndef VMS
#include <sys/types.h>
#include <netinet/in.h>
#endif

/* The X Window include files */
#include <X11/Xlib.h>
#include <X11/Xatom.h>

/* Other include files */
#ifdef VMS
#include <processes.h>		/* for system() */
#include <signal.h>		/* for sleep() */
#endif

static Display *display;	/* the display we're using */
static int screen;		/* the screen we're using */
static Window window;		/* the window for receiving events */
static Window dispowner;	/* the owner of this display */
static Atom incratom;		/* the incremental transfer atom */
static Atom sel,targ,prop;	/* the atoms used in communications */
static Atom selatom;		/* The selection atom */
static Atom lockatom;		/* The atom used for locking */
static int maxlen;		/* the maximum number of shorts in a transfer */
static int fd_dispopen=0;	/* if the display connection is open */
static int xerror=0;		/* an X error occurred. */

#ifdef VMS
static unsigned short ntohs(netshort)
unsigned short netshort;
{
	unsigned short retval;

	retval = (netshort >> 8) | ((netshort & 0xFF) << 8);
	return (retval);
}

#define htons ntohs
#endif

/* The xerrorhandler routine handles an X error.  All it does is set the */
/* xerror flag to true, reset fd_dispopen. , and print an error message. */

static int xerrorhandler (disp, err)
Display *disp;
XErrorEvent *err;
{
	xerror = 1;
	fd_dispopen = 0;
	fprintf(stderr, "Error on X event stream!\n");

	return 0;
}

/* The sendcommand routine sends the specified command buffer piece by piece */
/* until the entire buffer has been sent */

/* Sam Southard, Jr. */
/* Created: 12-Nov-1990 */
/* 16-Nov-1990	SNS/CIT	Now takes a buffer of shorts (instead of chars). */
/* 13-Sep-1991	SNS/CIT	Now uses "global" display, window, and incratom */
/* 23-Sep-1992	SNS/CIT	Now converts the buffer to network order as needed. */

void figdisp_sendcommand(buffer,len)
short *buffer;		/* the command buffer */
int len;		/* number of shorts in the command buffer */
{
	XEvent event;	/* the received event */
	XEvent sendevent;	/* the outgoing event */

	int itmp;	/* a temporary integer */

	void figdisp_convbufout();

	if (!fd_dispopen) return;
	/* convert the buffer to network byte order */
	figdisp_convbufout(buffer,len);
	while (len && fd_dispopen)
	{
		XNextEvent(display,&event);
		switch(event.type)
		{
		case PropertyNotify:	/* he wants some more */
			/* if it's not a delete, ignore it */
			if (event.xproperty.state != PropertyDelete) break;
			itmp=((len > maxlen) ? maxlen*2 : len*2);
			XChangeProperty(display,event.xproperty.window,prop,
				XA_STRING,8,PropModeAppend,
				(unsigned char *)buffer,itmp);
			len -= itmp>>1;
			buffer += itmp>>1;
			sendevent.xselection.type=SelectionNotify;
			sendevent.xselection.selection=sel;
			sendevent.xselection.target=targ;
			sendevent.xselection.property=prop;
			sendevent.xselection.time=CurrentTime;
			(void)XSendEvent(display,dispowner,True,0L,&sendevent);
			break;
		}
	}
	XFlush(display);
	return;
}

/* The figdisp_getresponse routine waits for a response from the */
/* Figaro/PGPLOT display server and returns it to the calling routine */

/* Sam Southard, Jr. */
/* Created: 19-Nov-1990 */
/* 13-Sep-1991	SNS/CIT Now uses "global" display, window, and selatom */
/* 24-Sep-1992	SNS/CIT	Now converts the buffer back from network byte order. */

short *figdisp_getresponse(len)
int *len;		/* the number of shorts in the response */
{
	XEvent event;	/* for the X events */
	short *retval;	/* the return value */
	Atom acttype;	/* the actual type of the message */
	int actform;	/* the actual format of the message */
	unsigned long bytesleft;	/* the number of bytes left */
	unsigned long nitems;	/* number of items in value */

	void figdisp_convbufin();

	while(fd_dispopen && !xerror)
	{
		XMaskEvent(display,PropertyChangeMask,&event);
		if (event.xproperty.atom == selatom &&
		    event.xproperty.state == PropertyNewValue)
		{
			if (XGetWindowProperty(display,window,selatom,0L,10L,
				True,AnyPropertyType,&acttype,&actform,&nitems,
				&bytesleft,(unsigned char **)&retval)
			    != Success)
			{
				*len=0;
				retval=(short *)NULL;
			} else {
				if (bytesleft)
					printf("OOPS - there was data left!\n");
				/* we're dealing with shorts */
				*len = nitems>>1;
			}
			break;
		}
	}
	if (xerror || !fd_dispopen)
	{
		*len = 0;
		retval = NULL;
	} else {
		figdisp_convbufin(retval, *len);
	}
	return(retval);
}

/* The figdisp_closecomm routine closes the link with the server */

/* Sam Southard, Jr. */
/* Created: 20-Nov-1990 */
/* 13-Sep-1991	SNS/CIT	Now uses static display and window */
/*  6-Nov-1994  MCS/TJP Change uninitialized pointer to 0 */

void figdisp_closecomm()
{
	XEvent event;		/* recevied event */
	XEvent sendevent;	/* send event */
	int more=1;
	int sent=0;

	if (!fd_dispopen) return;
	while (more)
	{
		XNextEvent(display,&event);
		switch(event.type)
		{
		case PropertyNotify:
			if (event.xproperty.state != PropertyDelete || sent)
				break;
			XChangeProperty(display,event.xproperty.window,prop,
				incratom,8,PropModeAppend,
				(unsigned char *)0, 0);
			sendevent.xselection.type=SelectionNotify;
			sendevent.xselection.selection=sel;
			sendevent.xselection.target=targ;
			sendevent.xselection.property=prop;
			sendevent.xselection.time=CurrentTime;
			(void)XSendEvent(display,dispowner,True,0L,&sendevent);
			sent=1;
			break;
		case SelectionClear: /* the server's grabbed the icon */
			if (event.xselectionclear.selection == selatom) more=0;
			break;
		}
	}
	/* get rid of the X stuff */
	XDestroyWindow(display,window);
	XCloseDisplay(display);
	fd_dispopen=0;
	return;
}

/* The figdisp_opencomm routine opens a channel to the display server.  */
/* Maxbuf is the maxmimum number of shorts in the command buffer that will */
/* be sent at one time. */
/* Return Value: */
/* 1	Success */
/* 0	Failure */

/* Sam Southard, Jr. */
/* Created: 13-Sep-1991 */
/* Modification History: */
/* 14-Feb-1992	SNS/CIT	Now includes support for multiple devices */

int figdisp_opencomm(maxbuf,dev)
int maxbuf;
int dev;	/* The figdisp display to open */
{
	XEvent event;		/* the received event */
	XEvent sendevent;	/* the outgoing event */
	char lockname[40];	/* the name of the locking atom */

	if (!fd_dispopen)
	{
		if ((display=XOpenDisplay((char *)0)) == NULL)
		{
			(void)printf("Couldn't open display!\n");
			return(0);
		}
		maxlen= (XMaxRequestSize(display)-10)*2;

		/* we need something to work with */
		if (maxlen <= 0) maxlen=1000;

		screen=DefaultScreen(display);

		(void)sprintf(&lockname[0],"%s_%d_%d",NAME_SELATOM,screen,dev);

		if ((selatom=XInternAtom(display,&lockname[0],False)) == None)
		{
			(void)printf("Can't find the selection atom!\n");
			return(0);
		}

		(void)sprintf(&lockname[0],"%s_%d_%d",NAME_INCRATOM,screen,dev);

		if ((incratom=XInternAtom(display,&lockname[0],False)) == None)
		{
			(void)printf("Can't find the incremental atom!\n");
			return(0);
		}
		/* we need a window to receive events */
		window=XCreateSimpleWindow(display, RootWindow(display,screen),
			1, 1, 1, 1, 1, BlackPixel(display,screen),
			WhitePixel(display,screen));
		(void)sprintf(&lockname[0],"%s_%d_%d",NAME_PROG,screen,dev);
		if ((lockatom=XInternAtom(display,&lockname[0],False)) == None)
			(void)printf("Can't find the locking atom!\n");
		else {
			if (XGetSelectionOwner(display,selatom) == None)
			{
				(void)printf("Attempting to start server\n");
				sprintf(&lockname[0],"figdisp -id %d &",dev);
				system(&lockname[0]);
				/* give it time */
				sleep(3);
				if (XGetSelectionOwner(display,selatom) == None)
				{
					(void)printf(
						"No display server running!\n");
					return(0);
				}
			}
			if (XGetSelectionOwner(display,selatom) !=
				XGetSelectionOwner(display,lockatom)
			    && XGetSelectionOwner(display,selatom) != window
			    || XGetSelectionOwner(display,selatom) == None)
			{
				(void)printf("Someone's using the display!\n");
				return(0);
			}
		}
		XSetSelectionOwner(display,selatom,window,CurrentTime);
		if (XGetSelectionOwner(display,selatom) != window)
		{
			(void)printf("Couldn't own selection atom!\n");
			return(0);
		}
		XSelectInput(display,window,PropertyChangeMask);
		/* set up the data link */
		while(1)
		{
			XNextEvent(display,&event);
			if (event.type == SelectionRequest)
			{
				XSetSelectionOwner(display,
					event.xselectionrequest.property,window,
					event.xselectionrequest.time);
				if (XGetSelectionOwner(display,
					event.xselectionrequest.property)
				    != window)
				{
					(void)printf("can't own property!\n");
					return(0);
				}
				dispowner=event.xselectionrequest.requestor;
				XChangeProperty(display,window,
					event.xselectionrequest.property,
					incratom,32,PropModeAppend,
					(unsigned char *)&maxbuf,1);
				sel=event.xselectionrequest.selection;
				targ=event.xselectionrequest.target;
				prop=event.xselectionrequest.property;
				sendevent.xselection.type=SelectionNotify;
				sendevent.xselection.selection=
					event.xselectionrequest.selection;
				sendevent.xselection.target=
					event.xselectionrequest.target;
				sendevent.xselection.property=
					event.xselectionrequest.property;
				sendevent.xselection.time=
					event.xselectionrequest.time;
				(void)XSendEvent(display,dispowner,True,0L,
					&sendevent);
				break;
			}
		}
		XSetErrorHandler (xerrorhandler);
		fd_dispopen=1;
		xerror=0;
	}
	return(1);
}

/* The figdisp_maxbuflen routine returns the maximum number of shorts that */
/* can be in a single data transfer between the display server and a client */
/* Return values: */
/* -1	An error occured */
/* else	the number of shorts in the maximum transfer */

/* Sam Southard, Jr. */
/* Created: 20-Sep-1991 */

int figdisp_maxbuflen()
{
	Display *tmp=display;	/* necessary only if the display isn't open */
	int retval;

	if (!fd_dispopen)
	{
		if ((tmp = XOpenDisplay((char *)0)) == NULL)
		{
			printf("Could not open display!\n");
			return(-1);
		}
	}
	/* allow a little breathing space */
	retval= (XMaxRequestSize(tmp) - 10)*2;

	if (!fd_dispopen) XCloseDisplay(tmp);

	if (retval <= 0) return(-1);
	else return(retval);
}

/* The figdisp_convbufout routine converts the command buffer to the network */
/* byte order. */

/* Sam Southard, Jr. */
/* Created: 23-Sep-1992 */

void figdisp_convbufout(buf, len)
unsigned short *buf;	/* The command buffer to convert */
int len;		/* The number of shorts in the command buffer. */
{
	unsigned short testshort=0x1234;
	unsigned short realcom;

	/* we may be able to skip this */
	if (testshort == htons(testshort))
		return;
	
	/* we have to work at it */
	while(len > 0)
	{
		realcom = *buf;
		*buf = htons(*buf);
		++buf;
		--len;
		switch(realcom)
		{
		/* this first set of parameters takes 6 parameters.  It */
		/* falls through to the lower cases */
		/* six parameters */
		case SET_BM_CSCALE:
			if (len < 1)
			{
				printf("Incomplete command %d detected!\n",
					realcom);
				return;
			}
			*buf = htons(*buf);
			++buf;
			--len;
		/* five parameters */
		case BM_LINE:
			if (len < 1)
			{
				printf("Incomplete command %d detected!\n",
					realcom);
				return;
			}
			*buf = htons(*buf);
			++buf;
			--len;
		/* four parameters */
		case DRAW_LINE:
		case FILL_RECT:
		case BM_ZOOMPAN:
		case BM_SH_UPDATE:
			if (len < 1)
			{
				printf("Incomplete command %d detected!\n",
					realcom);
				return;
			}
			*buf = htons(*buf);
			++buf;
			--len;
		/* three parameters */
		case SET_BM_SIZE:
			if (len < 1)
			{
				printf("Incomplete command %d detected!\n",
					realcom);
				return;
			}
			*buf = htons(*buf);
			++buf;
			--len;
		/* two parameters */
		case LG_CURS:
		case SET_LG_SIZE:
		case DRAW_DOT:
		case BM_SET_CURS:
			if (len < 1)
			{
				printf("Incomplete command %d detected!\n",
					realcom);
				return;
			}
			*buf = htons(*buf);
			++buf;
			--len;
		/* one parameter */
		case SHOW_LG_WIN:
		case SET_LG_COL:
		case LG_LINE_WID:
		case SHOW_BM_WIN:
			if (len < 1)
			{
				printf("Incomplete command %d detected!\n",
					realcom);
				return;
			}
			*buf = htons(*buf);
			++buf;
			--len;
		/* This set takes no parameters, so nothing more need be done */
		case RESET:
		case CLR_LG_WIN:
		case LG_MAX_DIM:
		case LG_SCALE:
		case LG_DEF_SIZE:
		case BM_GET_CURS:
		case CLR_BM_WIN:
		case BM_MAX_DIM:
		case BM_DEF_SIZE:
		case BM_FLUSH:
			break;
		/* A variable number of parameters, each handled individually */
		case SET_LG_LUT:
		{
			int nluts;

			if (len < 2)
			{
				printf("Incomplete command %d detected!\n",
					realcom);
				return;
			}
			*buf = htons(*buf);
			++buf;
			nluts = *buf;
			*buf = htons(*buf);
			++buf;
			nluts *= 3;
			if (len < nluts+2)
			{
				printf("Incomplete command %d detected!\n",
					realcom);
				return;
			}
			len -= (nluts+2);
			while(nluts-- > 0)
			{
				*buf = htons(*buf);
				++buf;
			}
		}
			break;
		case DRAW_POLY:
		case FILL_POLY:
		{
			int npts;

			if (len < 1)
			{
				printf("Incomplete command %d detected!\n",
					realcom);
				return;
			}
			npts = *buf;
			*buf = htons(*buf);
			++buf;
			npts *= 2;
			if (len < npts+1)
			{
				printf("Incomplete command %d detected!\n",
					realcom);
				return;
			}
			len -= (npts+1);
			while (npts-- > 0)
			{
				*buf = htons(*buf);
				++buf;
			}
		}
			break;
		case SET_BM_LUT:
		{
			int nluts,affluts;

			if (len < 4)
			{
				printf("Incomplete command %d detected!\n",
					realcom);
				return;
			}
			/* starting LUT */
			*buf = htons(*buf);
			++buf;
			/* bits per pixel */
			*buf = htons(*buf);
			++buf;
			/* number of luts */
			nluts = *buf;
			*buf = htons(*buf);
			++buf;
			affluts = *buf & 0x7;
			*buf = htons(*buf);
			++buf;
			len -= 4;
			if (!affluts)
				nluts *= 3;
			if (len < nluts)
			{
				printf("Incomplete command %d detected!\n",
					realcom);
				return;
			}
			len -= nluts;
			while (nluts-- > 0)
			{
				*buf = htons(*buf);
				++buf;
			}
		}
			break;
		case BM_WRITE:
		{
			int bppix,npix;

			if (len < 6)
			{
				printf("Incomplete command %d detected!\n",
					realcom);
				return;
			}
			bppix = *buf;
			*buf = htons(*buf);
			++buf;
			/* startx */
			*buf = htons(*buf);
			++buf;
			/* start y */
			*buf = htons(*buf);
			++buf;
			npix = *buf;
			*buf = htons(*buf);
			++buf;
			npix *= *buf;
			*buf = htons(*buf);
			++buf;
			len -= 5;
			if (bppix == 16 && len < npix || len < (npix+1)/2)
			{
				printf("Incomplete command %d detected!\n",
					realcom);
				return;
			}
			if (bppix == 16)
			{
				len -= npix;
				while (npix-- > 0)
				{
					*buf = htons(*buf);
					++buf;
				}
			} else {
				len -= (npix+1)/2;
				buf += (npix+1)/2;
			}
			break;
		}
		case SET_BM_DSCALE:
		{
			char *cbuf= (char *)buf;
			int i=0;
			int nnul=0;

			while (nnul < 2 && i++ < len*2)
			{
				if (*cbuf++ == '\0')
					++nnul;
			}
			if (nnul < 2)
			{
				printf("Incomplete command %d detected!\n",
					realcom);
				return;
			}
			buf += (i+1)/2;
			len -= (i+1)/2;
		}
			break;
		case LG_PIXLINE:
		{
			int npix;

			if (len < 4)
			{
				printf("Incomplete command %d detected!\n",
					realcom);
				return;
			}
			npix = *buf;
			*buf = htons(*buf);
			++buf;
			/* X & Y coords */
			*buf = htons(*buf);
			++buf;
			*buf = htons(*buf);
			++buf;
			len -= 3;
			if (len < npix)
			{
				printf("Incomplete command %d detected!\n",
					realcom);
				return;
			}
			len -= npix;
			while (npix-- > 0)
			{
				*buf = htons(*buf);
				++buf;
			}
		}
			break;
		case SET_LG_CSCALE:
		{
			char *cbuf= (char *)buf;
			int i=0;
			int nnul=0;

			while (nnul < 4 && i++ < len*2)
			{
				if (*cbuf++ == '\0')
					++nnul;
			}
			if (nnul < 4)
			{
				printf("Incomplete command %d detected!\n",
					realcom);
				return;
			}
			buf += (i+1)/2;
			len -= (i+1)/2;
		}
			break;
		case SET_BM_SH_SIZE:
		{
			int type;

			if (len < 5)
			{
				printf("Incomplete command %d detected!\n",
					realcom);
				return;
			}
			for (type=0 ; type < 3 ; ++type)
			{
				*buf = htons(*buf);
				++buf;
			}
			type = *buf;
			*buf = htons(*buf);
			buf++;
			len -= 4;
			/* note that these will onyl be relevant if they are */
			/* on the exact same system, so we don't need to */
			/* switch anythign else around, we just need to know */
			/* how much to skip */
			switch(type)
			{
			/* only case currently known is 1, the Sun version */
			case 1:
				if (len < 2)
				{
					printf("Incomplete command %d detected!\n",
						realcom);
					return;
				} else {
					*buf = htons(*buf);
					++buf;
					*buf = htons(*buf);
					++buf;
					len -= 2;
				}
			}
		}
			break;
		default:
			printf("Unknown command %d detected!\n", realcom);
			break;
		}
	}

	return;
}

/* The figdisp_convbufin routine converts the response from the network */
/* byte order. */

/* Sam Southard, Jr. */
/* Created: 23-Sep-1992 */

void figdisp_convbufin(buf, len)
unsigned short *buf;	/* The command buffer to convert */
int len;		/* The number of shorts in the command buffer. */
{
	unsigned short testshort=0x1234;
	unsigned short realcom;

	/* we may be able to skip this */
	if (testshort == htons(testshort))
		return;
	
	/* only one response is allowed per buffer */
	*buf = ntohs(*buf);
	switch(*buf++)
	{
	/* six arguments */
	case LG_MAX_DIM:
	case BM_MAX_DIM:
		*buf = ntohs(*buf);
		++buf;

	/* five arguments */
		*buf = ntohs(*buf);
		++buf;

	/* four arguments */
	case LG_SCALE:
	case LG_DEF_SIZE:
	case BM_DEF_SIZE:
		*buf = ntohs(*buf);
		++buf;

	/* three arguments */
	case LG_CURS:
	case BM_GET_CURS:
		*buf = ntohs(*buf);
		++buf;

	/* two arguments */
		*buf = ntohs(*buf);
		++buf;

	/* one argument */
	case SET_BM_SH_SIZE:
		*buf = ntohs(*buf);
		++buf;
		break;
	default:
		printf("Unknown return buffer %d detected!\n", *(buf-1));
		break;
	}

	return;
}
