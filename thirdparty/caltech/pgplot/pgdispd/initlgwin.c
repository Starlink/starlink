/* The initlgwin routine initializes the window we need to do line graphics */
/* (either PGPLOT or row/column printout */
/* Return Values: */
/* FAIL		If something prevented us from doing all we needed to */
/* SUCCEED	If everything went fine */

/* Sam Southard, Jr. */
/* Created: 13-Mar-1991 (from figdisp/initwins) */
/* 31-Jul-1991	SNS/CIT	Now uses the visual member of the wininfo structure */
/* 15-Aug-1991	SNS/CIT	No longer includes hooks for vista */
/*  6-Sep-1991	SNS/CIT	Changes from SSL::TENNANT implemented */
/* 19-Sep-1991	SNS/CIT	Now uses the Keck icon */
/*  3-Oct-1991	SNS/CIT	Now initializes zfac structure member */
/*  8-Oct-1991	SNS/CIT	Globals now in globals.h */
/* 10-Oct-1991	SNS/CIT	Window manager hints now in initwmattr */
/* 22-Oct-1991	SNS/CIT	No longer sets the border pixel */
/* 25-Nov-1991	SNS/CIT	X and Y zoom now separate */
/* 29-Jan-1992	SNS/CIT	Modifications for the Keck data acquisition system */
/*			folded in. */
/* 11-Feb-1992	SNS/CIT	Now looks for pointer motion masks */
/* 14-Feb-1992	SNS/CIT	Now clears the pixmap to lg.pix[0], not BlackPixel. */
/*			Now includes the id in the title */
/* 24-Feb-1992	SNS/CIT	Visual allocation now done in the getvisuals routine */
/* 26-Feb-1992	SNS/CIT	Cursor scaling offsets initialization added. */
/*  6-Apr-1992	SNS/CIT	Now uses a blank cursor so that we can have large */
/*			crosshairs as our cursor */
/*  8-Apr-1992	SNS/CIT	Now has a better title for Keck version */
/* 10-Apr-1992	SNS/CIT	No longer uses minimum or maximum window widths. Now */
/*			uses lg.imwidth & lg.imheight as the width & height */
/*			of the Pixmap */
/*  7-May-1992	SNS/CIT	Now respects .lg.crosshair. */
/* 25-Jun-1992	SNS/CIT	Now flushes the connection before returning. */
/*  4-Oct-1992	SNS/CIT	No longer needs #ifdef KECK */

/* The system include files */
#include <stdio.h>

/* the X Window include files */
#include <X11/Xlib.h>
#include <X11/Xutil.h>

/* The program include files */
#include "figdisp.h"
#include "globals.h"
#include "messages.h"
#include "figdisp.icon"
#include "nocursor.icon"

#define MAX_DEPTH	25	/* Must be greater than the maximum depth */
				/* supported by X Window */

int initlgwin()
{
	XSetWindowAttributes winattr;	/* window attributes */
	XGCValues gcvals;		/* for setting the graphics context */
	char wintitle[80];	/* The window's title */

	unsigned long pmtmp[1];	/* temporary for plane masks */
	XVisualInfo vTemplate;		/* The template for our visual */
	XVisualInfo *visualList;	/* The visuals which matched */
	int visualsMatched;		/* how many matched? */
	int newmap;			/* do we need our own color map */
	Pixmap lgcurspmap;		/* pixmap for line graphics cursor */
	int valuemask;			/* the values set in XCreateWindow */

	Window foobar;

	Pixmap XCreatePixmap();
	void initlgluts();	/* initialize the LUTs */
	char *malloc();
	void initwmattr();

	/* Initialize the line graphics LUTs */
	initlgluts();

	/* initialize the line graphics wininfo structure */
	lg.height=lg.imheight=res.lggeo.h;
	lg.width=lg.imwidth=res.lggeo.w;
	lg.cursx=lg.width>>1;
	lg.cursy=lg.height>>1;
	lg.xzoom=lg.yzoom=0;
	lg.curxsc=lg.curysc=1.0;
	lg.curxoff=lg.curyoff=0.0;
	lg.winxoff=lg.winyoff=0;

	/* Load the cursor icon */
	if (res.lgcross)
	{
	        XColor bg;  /* Background cursor color */
	        XColor fg;  /* Foreground cursor color */
	
        /* Query the X-server for the background and foreground colors */

	        bg.pixel = lg.pix[0];
                XQueryColor(display, linecmap, &bg);
	        fg.pixel = lg.pix[1];
                XQueryColor(display, linecmap, &fg);

		lgcurspmap=XCreateBitmapFromData(display,
			RootWindow(display,screen), nocursor_bits,
			nocursor_width, nocursor_height);
		winattr.cursor=XCreatePixmapCursor(display, lgcurspmap,
			None, &fg, &bg, 0, 0);
	}
	
	/* create a window for line graphics */
	winattr.background_pixmap=None;
	winattr.background_pixel=lg.pix[0];
	winattr.colormap=linecmap;
	winattr.border_pixel=lg.pix[0];
	winattr.bit_gravity=SouthWestGravity;

	valuemask= CWBackPixel | CWColormap | CWBitGravity | CWBackPixmap |
		CWBorderPixel;
	if (res.lgcross)
		valuemask |= CWCursor;
	lg.win=XCreateWindow(display, RootWindow(display,screen), res.lggeo.x,
		res.lggeo.y, lg.width, lg.height, BORDER_WIDTH, (int)linedepth,
		InputOutput, linevisual, valuemask, &winattr);
	lg.mapped=0;	/* it's not mapped yet */

	/* Load the icon */
	lg.icon=XCreateBitmapFromData(display, lg.win, (char *)&figdisp_bits[0],
		figdisp_width, figdisp_height);
	
	/* set up the window manager hints */
	(void)sprintf(&wintitle[0],"line graphics #%d",res.id);
	initwmattr(lg, &wintitle[0], "line graphics", &res.lggeo);

	/* The lock and selection atoms need to be owned by someone */
	XSetSelectionOwner(display,lock,lg.win,CurrentTime);
	XSetSelectionOwner(display,selatom,lg.win,CurrentTime);

	/* was there a problem owning the locking atom? */
	if (XGetSelectionOwner(display,lock) != lg.win)
		(void)fprintf(stderr,MSG_NOLOCK);

	/* if we can't get ownership of the selection atom, we won't */
	/* be able to receive any commands */
	if (XGetSelectionOwner(display,selatom) != lg.win)
	{
		(void)fprintf(stderr,MSG_BADSELOWN);
		return(-1);
	}

	/* Create the line graphics pixmap */
	lg.pixmap=XCreatePixmap(display, RootWindow(display,screen), lg.width,
		lg.height, linedepth);
	
	/* set up a graphics contexts for the line graphics window */
	gcvals.foreground=lg.pix[0];
	linegcclear=XCreateGC(display,lg.win,GCForeground,&gcvals);
	gcvals.background=lg.pix[0];
	gcvals.foreground=lg.pix[1];
	gcvals.fill_style=FillSolid;
	gcvals.fill_rule=EvenOddRule;
	linegc=XCreateGC(display, lg.win,
		GCForeground|GCBackground|GCFillStyle|GCFillRule,&gcvals);
	
	XSetFont(display, linegc, res.textfont->fid);
	XSetFont(display, linegcclear, res.textfont->fid);
	
	/* Clear the pixmap */
	XFillRectangle(display, lg.pixmap, linegcclear, 0, 0, lg.width,
		lg.height);

	/* we need to listen for new data */
	XSelectInput(display,lg.win,PropertyChangeMask | ExposureMask |
		StructureNotifyMask | ButtonPressMask | PointerMotionMask |
		KeyPressMask);

	XFlush(display);
	return(SUCCEED);
}
