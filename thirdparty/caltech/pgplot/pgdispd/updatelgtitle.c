/* The updatelgtitle image updates the line graphics window's title to */
/* reflect the position of the cursor.  The arguments are the window x and y */
/* position of the cursor event */

/* Sam Southard, Jr. */
/* Created: 12-Feb-1992 (from updatetitle.c) */
/* Modification History: */
/* 14-Feb-1992	SNS/CIT	Now includes the id in the title */
/* 26-Feb-1992	SNS/CIT	Now handles more than 16 colors */
/*  8-Apr-1992	SNS/CIT	Keck title now more appropriate */
/* 10-Apr-1992	SNS/CIT	Can now deal with lg.winxoff and lg.winyoff */
/*  4-Oct-1992	SNS/CIT	No longer needs #ifdef KECK */

#include "figdisp.h"
#include "globals.h"
#include <X11/Xlib.h>
#include <string.h>

void updatelgtitle(x,y)
int x,y;	/* cursor position */
{
	char newtitle[80];
	char tempstr[80];
	char *strptr= &newtitle[0];	/* for the call to set the name */
	double curx,cury;		/* the cursor values */
	int datval;
	XImage *image;
	int i;

#ifndef _AIX
	char *sprintf();
#endif

	/* make sure we don't confuse anything. */
	if (x < lg.winxoff || x >= lg.imwidth+lg.winxoff || y < lg.winyoff ||
	   y >= lg.imheight+lg.winyoff) return;
	else { /* now we can get the transformed values */
		if ((image=XGetImage(display, lg.pixmap, x-lg.winxoff,
			y-lg.winyoff, 1, 1, 0xFFFFFFFF, ZPixmap)) == NULL)
		{
			datval= -1;
		} else {
			datval=XGetPixel(image,0,0);
			XDestroyImage(image);
		}
		for (i=0 ; i < lg.colors ; ++i)
		{
			if (datval==lg.pix[i])
			{
				datval=i;
				break;
			}
		}
		if (i >= lg.colors) datval= -1;
		
		curx=x-lg.winxoff;
		cury=lg.height-(y-lg.winyoff)-1;
		curx=(curx-lg.curxoff)/lg.curxsc;
		cury=(cury-lg.curyoff)/lg.curysc;
		if (datval < 0) (void)sprintf(&newtitle[0],
			"line graphics #%d X: %.6g Y: %.6g", res.id, curx,
			cury);
		else (void)sprintf(&newtitle[0],
			"line graphics #%d X: %.6g Y: %.6g Color Index: %d",
			res.id, curx, cury, datval);
	}
	if (XStringListToTextProperty(&strptr, 1, &lg.winname))
		XSetWMName(display, lg.win, &lg.winname);


	return;
}
