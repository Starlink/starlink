/* The exposelgwin routine handles an expose event on the line graphics */
/* window.  The argument is the expose event. */

/* Sam Southard, Jr. */
/* Created: 29-Mar-1991 (from figdisp/exposewin) */
/* Modification History: */
/*  2-Apr-1991	SNS/CIT	Pixmap and window are now the same size. */
/* 15-Aug-1991	SNS/CIT	No longer includes hooks for xvideo */
/*  8-Oct-1991	SNS/CIT	Globals moved to globals.h */
/* 10-Oct-1991	SNS/CIT	Now handles winxoff & winyoff */
/* 14-Oct-1992	SNS/CIT	Now flushs the X connection.  RCS id string added. */

#ifndef lint
static char rcsid[]="@(#)$Id$";
#endif

/* The program include files */
#include "figdisp.h"
#include "globals.h"

void exposelgwin(event)
XExposeEvent event;
{
	/* pretty simple */
	XCopyArea(display, lg.pixmap, lg.win, linegc, event.x-lg.winxoff,
		event.y-lg.winyoff, (unsigned)event.width,
		(unsigned)event.height, event.x, event.y);
	XFlush(display);

	return;
}
