/* The resizelgwin routine updates the line graphics window in response to a */
/* ConfigureNotify event. */

/* Sam Southard, Jr. */
/* Created: 29-Mar-1991 (from figdisp/resizewin) */
/* Modification History: */
/*  2-Apr-1991	SNS/CIT	Modified to use a Pixmap the same size as the window, */
/*			instead of always having a LG_MAX_xxx sized pixmap. */
/*  8-Oct-1991	SNS/CIT	Globals now in globals.h */
/* 11-Feb-1992	SNS/CIT	Elimincated some unnecessary code. */
/* 14-Feb-1992	SNS/CIT	Now clears the line graphics window to lg.pix[0] */
/*			instead of BlackPixel */
/* 10-Apr-1992	SNS/CIT	No longer uses minimum or maximum window sizes.  Now */
/*			uses lg.imwidth & lg.imheight as the width & height */
/*			of the pixmap. */

/* The program include files */
#include "figdisp.h"
#include "globals.h"

void resizelgwin(event)
XConfigureEvent event;
{
	int changed=0;	/* whether or not we need to resize the window again */
	static int lastchanged=0;	/* if we changed last time through */
	Pixmap temppixmap;	/* pixmap to hold old data */

	Pixmap XCreatePixmap();

	lg.width=lg.imwidth=event.width;
	lg.height=lg.imheight=event.height;

	/* I know that this won't work right now (changed never gets set). */
	/* However, I'm keeping it around until I know how to detect if */
	/* XCreatePixmap failed */
	if (changed && !lastchanged) {
		lastchanged=1;
		XResizeWindow(display,lg.win,lg.width,lg.height);
	} else {
		lastchanged=0;
		temppixmap=XCreatePixmap(display, RootWindow(display,screen),
			lg.width, lg.height, linedepth);
		/* Clear the pixmap */
		XSetForeground(display, linegcclear, lg.pix[0]);
		XFillRectangle(display, temppixmap, linegcclear, 0, 0,
			lg.width, lg.height);
		XCopyArea(display, lg.pixmap, temppixmap, linegc, 0, 0,
			lg.width, lg.height, 0, 0);
		XFreePixmap(display, lg.pixmap);
		lg.pixmap=temppixmap;
		XCopyArea(display, lg.pixmap, lg.win, linegc, 0, 0,
			lg.width, lg.height, 0, 0);
	}

	return;
}
