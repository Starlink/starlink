/* The mainloop routine contains the main event loop. */
/* Return Values: */
/* FAIL		If something went wrong */
/* SUCCEED	If the user requested an exit */

/* Sam Southard, Jr. */
/* Created: 2-Nov-1990 */
/* 15-Nov-1990	SNS/CIT	Modified to get data as a short instead of a char. */
/* 16-Nov-1990	SNS/CIT	Modified to handle variable sized windows and to */
/*			grab ownership of the selection atom if the client */
/*			program goes away. */
/* 19-Nov-1990	SNS/CIT	Modified to be able to get the cursor value and to */
/*			send information back to the client program. */
/* 10-Dec-1990	SNS/CIT	Now puts line graphics window to top when waiting for */
/*			a cursor point.  Now uses wininfo structure.  VMS */
/*			changes merged in.  Now creates a window 10 pixels */
/*			larger than the requested size so that graphics are */
/*			not right at the edge of the window. */
/* 11-Dec-1990	SNS/CIT	No longer exits if it receives a bad data type.  Does */
/*			not warp the pointer if requested to put it at the */
/*			last point. */
/* 12-Dec-1990	SNS/CIT	Now handles a bitmap graphics window.  Some of the */
/*			code from the main case statement moved into separate */
/*			routines. */
/* 13-Dec-1990	SNS/CIT	Now calls writeimage to clean up if the connection to */
/*			the user program goes away for any reason. */
/* 15-Dec-1990	SNS/CIT	Data acquisition moved into getdata(). */
/* 18-Dec-1990	SNS/CIT	Cursor location now in wininfo structure. */
/* 10-May-1991	SNS/CIT	Modified so that we can share some routines with the */
/*			Lick Xvideo program. */
/* 31-Jul-1991	SNS/CIT	Modified to handle fastdisp as well. */
/*  2-Aug-1991	SNS/CIT	Now puts the current value of the cursor as soon as */
/*			cursor tracking is selected. */
/* 14-Aug-1991	SNS/CIT	No longer contains hooks for xvideo */
/* 20-Aug-1991	SNS/CIT	Now handles a patch window */
/* 22-Aug-1991	SNS/CIT	Now handles a location window */
/* 27-Aug-1991	SNS/CIT	Now handles LUT inversion and seeing */
/*  3-Sep-1991	SNS/CIT	Now handles color map window */
/*  5-Sep-1991	SNS/CIT	Modified to lint cleanly */
/* 17-Sep-1991	SNS/CIT	Key presses now handled in the dokey routine. */
/*			Updatetitle moved to its own file. */
/*  1-Oct-1991	SNS/CIT	Slit (general row/column plot) feature added */
/*  4-Oct-1991	SNS/CIT	Now indicates line for line plots with an XOR line */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/* 22-Nov-1991	SNS/CIT	Now handles waitevent returning an error */
/* 29-Jan-1992	SNS/CIT	Now uses the handlexevent routine to process X events */
/*  7-May-1992	SNS/CIT	Unused #includes removed. */

/* The system include files */
#include <stdio.h>

/* The X Window include files */
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>

/* The program include files */
#include "figdisp.h"
#include "globals.h"

int mainloop()
{
	XEvent event;	/* the current event */
	int go_on=1;	/* whether or not we should continue */

	while (go_on)
	{
		/* if we think there's a client out there, we need to time */
		/* out if the client dies */
		if (selset) selset=waitevent();
		if (selset == -1) return(FAIL);
		XNextEvent(display,&event);
		if (handlexevent(event,&go_on) == FAIL) return(FAIL);
	}
	return(SUCCEED);
}
