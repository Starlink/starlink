/* The initwmattr routine notifies the window manager of the approprate */
/* attributes for the given window. */

/* Sam Southard, Jr. */
/* Created: 9-Oct-1991 (from initbmwin) */
/* Modification History: */
/* 14-Apr-1992	SNS/CIT	Now compiles under VMS */

/* The program include files */
#include "commands.h"
#include "figdisp.h"
#include "globals.h"

/* The system include files */
#include <stdio.h>

void initwmattr(wininf,wname,iname,geom)
struct wininfo wininf;	/* the information about this window */
char *wname;	/* The name for the window */
char *iname;	/* the name for the icon */
struct geometry *geom;	/* The windows geometry.  May be NULL */
{
	char winnamebuf[80];	/* a buffer for the window name */
	char *strptr= &winnamebuf[0];	/* pointer to buffer */
	XSizeHints sizehints;		/* the size  & position */
	XWMHints wm_hints;		/* hints to the window manager */

	/* set up the geometry if necessary */
	if (geom != NULL)
	{
		sizehints.x=geom->x;
		sizehints.y=geom->y;
		sizehints.width=geom->w;
		sizehints.height=geom->h;
		sizehints.flags=USSize;
		if (sizehints.x >= 0 && sizehints.y >= 0)
			sizehints.flags |= USPosition;
		XSetWMNormalHints(display, wininf.win, &sizehints);
	}
	
#ifndef _AIX
	/* set up the window and icon names */
	(void)sprintf(&winnamebuf[0], "%s/%s", NAME_PROG, wname);
	if (XStringListToTextProperty(&strptr, 1, &wininf.winname))
		XSetWMName(display, wininf.win, &wininf.winname);
	(void)sprintf(&winnamebuf[0],"%s", iname);
	if (XStringListToTextProperty(&strptr, 1, &wininf.iconname))
		XSetWMIconName(display, wininf.win, &wininf.iconname);
#endif
	
	/* Set up the hints the window manager needs */
	wm_hints.input = True;
	wm_hints.icon_pixmap=wininf.icon;
	wm_hints.initial_state=NormalState;
	wm_hints.flags = InputHint | IconPixmapHint | StateHint;
	XSetWMHints(display, wininf.win, &wm_hints);

	return;
}
