/* The cleanup routine cleans up everything necessary to shut down the */
/* FIGARO/PGPLOT display server nicely. */

/* Sam Southard, Jr. */
/* Created: 11-Nov-1990 */
/* 10-Dec-1990	SNS/CIT	Modified to use wininfo structure */
/* 10-May-1991	SNS/CIT	Modified to live with Lick's Xvideo */
/*  7-Aug-1991	SNS/CIT	Now deals with OpenWindows bug. */
/* 15-Aug-1991	SNS/CIT	No longer includes vista hooks. */
/*  8-Oct-1991	SNS/CIT	Modified for cleaner wininfo struct */

/* The X Window include files */
#include <X11/Xlib.h>

/* The program include files */
#include "figdisp.h"
#include "globals.h"

void cleanup()
{
	void restorecolors();

#ifndef PGDISP
#ifndef UNBUGGY
	/* deal with OpenWindows bug an restore the color maps to what they */
	/* used to be */
	restorecolors();
#endif
#endif

	/* Release the line graphics graphics context */
	XFreeGC(display,linegc);

	/* shut down this channel to the display */
	XCloseDisplay(display);

	return;
}
