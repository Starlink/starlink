/* The waitevent routine waits until there are events in the queue. If the */
/* program which was feeding us data goes away then it grabs ownership of the */
/* selection so that the next program will be able to access things. */
/* Return Value: */
/* 0		The program feeding us data died. */
/* -1		We could not get ownership of the selection atom */
/* 1		If everything went fine. */

/* Sam Southard, Jr. */
/* Created: 12-Dec-1990 from mainloop.c */
/* 13-Dec-1990	SNS/CIT	Now calls writeimage to clean up its state if the */
/*			connection goes away */
/* 15-Aug-1991	SNS/CIT	No longer includes hooks for xvideo */
/*  6-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Globals now in globals.h */
/* 22-Nov-1991	SNS/CIT	Should now only be called if we think there's a */
/*			client program out there (selset argument removed). */
/*			Now gets the time to wait between existance checks */
/*			from an X resource */
/* 27-Nov-1991	SNS/CIT	Modified to lint cleanly */
/* 29-Jan-1992	SNS/CIT	Now uses the correct method to check if an event is */
/*			pending, instead of using the QLength macro (which */
/*			was an error) */
/* 30-Jan-1992	SNS/CIT	Now uses the XEventsQueued call to determine if an */
/*			event is pending.  This means we don't have to push */
/*			events back on the list. */

/*
 * On AIX systems we need to define _BSD before including sys/types.h,
 * to get select() types defined.
 */
#ifdef _AIX
#define _BSD
#endif

/* The system include files */
#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>

/* The X11 include files */
#include <X11/Xlib.h>

/* The program include files */
#include "figdisp.h"
#include "globals.h"
#include "messages.h"
#include "commands.h"

/* Choose one of the following to select either good response time or low */
/* system load, keeping in mind what your system provides.  If none of the */
/* following defines is chosen, BUSY is used. */
/* #define	BUSY	/* Busy loop while waiting for events.  This loads */
			/* system but gives quickest response time and is */
			/* ompletely portable (since it does nothing). */
/* #define	SLEEP	/* Use the sleep(3) call to wait between existance */
			/* checks.  This is much nicer on the system load but */
			/* gives poor response time. */
/* #define	SELECT	/* Use this if possible. */
#ifdef VMS
#define		WAIT	/* use the LIB$WAIT call */
#else
#define		SELECT	/* Use the select call to wait */
#endif

int waitevent()
{
	short retbuf;
#ifdef WAIT
	float waittime;

	waittime=(1e-6)*res.sleeptime;
#endif
#ifdef SLEEP
	int sleeptime;

	if ((sleeptime=res.sleeptime/1000000) < 1) sleeptime=1;
#endif
#ifdef SELECT
	struct timeval timeout;
#endif

	while (!XEventsQueued(display,QueuedAlready))
	{
		XFlush(display);
		/* if the selection is not owned we need to grab it again */
		if (XGetSelectionOwner(display,selatom) == None)
		{
			XSetSelectionOwner(display,selatom,lg.win,CurrentTime);
			if (XGetSelectionOwner(display,selatom) != lg.win)
			{
				(void)fprintf(stderr,MSG_BADSELOWN);
				return(-1);
			}
			XUngrabKeyboard(display,CurrentTime);
			(void)proccom((short *)NULL,0,(short *)NULL,(int *)0);
			return(0);	/* the selection owner was reset */
		}
		/* give the idle message if the user's asked for it */
		if (sendidle)
		{
			retbuf = FIGDISP_IDLE;
			returnbuf (&retbuf, 1, srcwin);
			sendidle = 0;
		}
		/* pause a while */
#ifdef SLEEP
		sleep(sleeptime);
#endif
#ifdef WAIT
		lib$wait(&waittime);
#endif
#ifdef SELECT
		timeout.tv_sec = 0;
		timeout.tv_usec = res.sleeptime;
		select (0, (fd_set *)NULL, (fd_set *)NULL, (fd_set *)NULL,
			&timeout);
#endif
	}

	return(1);	/* the selection owner was not reset */
}
