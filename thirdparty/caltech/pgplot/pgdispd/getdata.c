/* The getdata routine handles the aquisition of data from the user program. */
/* It is called upon receipt of a SelectionNotify event. */
/* Return Value: */
/* 1	If everything went fine */
/* 0	If something happened and the program should terminate. *

/* Sam Southard, Jr. */
/* Created: 15-Dec-1990 from mainloop.c */
/* 18-Dec-1990	SNS/CIT	Cursor information now in wininfo structure. */
/*  2-Apr-1991	SNS/CIT	Image capabilities commented out for inclusion in */
/*			xvideo program. */
/* 11-Apr-1991	SNS/CIT	Modified to update the display if necessary. */
/* 10-May-1991	SNS/CIT	Modified to be shared between Xvideo and pgdisp */
/* 15-Aug-1991	SNS/CIT	No longer includes vista hooks */
/* 20-Aug-1991	SNS/CIT	Now calculates buflen based on XMaxRequestSize */
/*			instead of being hard coded. */
/*  5-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Globals moved to globals.h */
/* 19-Oct-1991	SNS/CIT	No longer mistakenly resets buflen. */
/* 14-Apr-1992	SNS/CIT	Now compiles under VMS */
/* 27-Sep-1992	SNS/CIT	return buffer now stored in network byte order */

/* The system include files */
#include <stdio.h>
#include <sys/types.h>
#include <netinet/in.h>

/* The X include files */
#include <X11/Xlib.h>
#include <X11/Xatom.h>

/* The program include files */
#include "commands.h"
#include "figdisp.h"
#include "globals.h"
#include "messages.h"


int getdata(event,rbuf,rbuflen,srcwin,selset)
XSelectionEvent event;	/* the event we're handling */
short *rbuf;	/* a return buffer, if needed */
int *rbuflen;	/* the length of the return buffer.  If it's 0, no return */
		/* message should be sent. */
Window srcwin;	/* the source of our data */
int *selset;	/* whether or not the selection is owned by a user program */
{
	short *buffer;			/* buffer for the data received */
	/* the max buffer length (in 16-bit words) */
	static long buflen= -1;
	unsigned long nitems;		/* the actual number of items we got */
	unsigned long bytesleft;	/* bytes left in the selection */
	Atom acttype;			/* the actual data type */
	int actform;			/* the actual data format */

	void returnbuf();		/* return data to the user process */
	void clearcurs();		/* clear the list of cursor presses */
	int proccom ();

	if (buflen == -1) buflen= (XMaxRequestSize(display)-10)<<1;
	if (XGetWindowProperty(display, srcwin, event.property, 0L, buflen,
		True, AnyPropertyType, &acttype, &actform, &nitems, &bytesleft,
		(unsigned char **)&buffer) != Success)
	{
		(void)fprintf(stderr,MSG_BADGETPROP);
		return(0);
	}
	if (acttype == XA_STRING)
	{
		if (proccom(buffer,(int)nitems>>1,rbuf,rbuflen)) return(0);
	} else if (acttype == incrtype) {
		if (!nitems)
		{ /* all done, get the selection back */
			XSetSelectionOwner(display,selatom,lg.win,CurrentTime);
			if (XGetSelectionOwner(display,selatom) != lg.win)
			{
				(void)fprintf(stderr, MSG_BADSELOWN);
				return(0);
			}
			*selset=0;
			clearcurs();
			XUngrabKeyboard(display,CurrentTime);
#ifndef PGDISP
			(void)writeimage((short **)NULL,(int *)NULL);
#endif
			(void)proccom(buffer,0,rbuf,rbuflen);
			if (*rbuflen) (void)fprintf(stderr,MSG_REPLYNOTSENT);
			*rbuflen=0;
			XFlush(display);
		}
	} else (void)fprintf(stderr,MSG_BADDATATYPE);
	XFree((char *)buffer);

	/* if no info to send, just go to next event */
	if (!*rbuflen) return(1);

	/* send requested information back to client */
	if (ntohs(*rbuf) != LG_CURS && ntohs(*rbuf) != BM_GET_CURS)
	{ /* cursor information isn't here yet */
		returnbuf(rbuf,*rbuflen,srcwin);
		*rbuflen=0;
	}

	return(1);
}
