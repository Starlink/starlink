/* The initluts routine initializes the LUTs for the line graphics window */

/* Sam Southard, Jr. */
/* Created: 13-Mar-1991 (from figdisp/initluts) */
/* Modification History: */
/* 15-Aug-1991	SNS/CIT	No longer contains hooks for xvideo */
/*  8-Oct-1991	SNS/CIT	Globals now in globals.h */
/* 14-Feb-1992	SNS/CIT	Now deals with grey scale and read only visuals */

/* The X Window include files */
#include <X11/Xlib.h>

/* The program include files */
#include "figdisp.h"
#include "globals.h"

static unsigned short pgcolors[LG_COLORS][3]= {
	    0,	    0,	    0,
	65535,	65535,	65535,
	65535,	    0,	    0,
	    0,	65535,	    0,
	    0,	    0,	65535,
	    0,	65535,	65535,
	65535,	    0,	65535,
	65535,	65535,	    0,
	65535,	32768,	    0,
	32768,	65535,	    0,
	    0,	65535,	32768,
	    0,	32768,	65535,
	32768,	    0,	65535,
	65535,	    0,	32768,
	21845,	21845,	21845,
	43690,	43690,	43690	};

void initlgluts()
{
	XColor color;			/* The color table entry */
	int i;				/* silly loop variables */

	/* If we're dealing with 2 colors, return right away, since it's */
	/* already done. */
	if (lg.colors == 2) return;

	color.flags=DoRed | DoGreen | DoBlue;

	/* PGPLOT defines the first 16 colors */
	for (i=0 ; i < lg.colors && i < 16 ; ++i)
	{
		if (lg.bw) color.red=color.green=color.blue=
			0.30*pgcolors[i][0]+0.59*pgcolors[i][1]+
			0.11*pgcolors[i][2];
		else {
			color.red=pgcolors[i][0];
			color.green=pgcolors[i][1];
			color.blue=pgcolors[i][2];
		}
		color.pixel=lg.pix[i];
		if (lg.ro) {
			XAllocColor(display, linecmap, &color);
			lg.pix[i]=color.pixel;
		}
		else XStoreColor(display, linecmap, &color);
	}

	/* PGPLOT doesn't define these, but I will.  It's a linear greyscale */
	/* with black at 15 (although 15 is used by another color, since we */
	/* already have a BlackPixel) and white at lg.colors-1. */
	for ( ; i < lg.colors ; ++i)
	{
		color.red=color.green=color.blue=
			((double)(i-15)/(lg.colors-16))*65535;
		color.pixel=lg.pix[i];
		if (lg.ro) XAllocColor(display, linecmap, &color);
		else XStoreColor(display, linecmap, &color);
	}
		
	return;
}
