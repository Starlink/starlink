/* The getvisuals routine gets a visual or visuals for the bitmap and line */
/* graphics windows.  The following visuals are tried: */
/* (1) The default visual & color map for both of them. */
/* (2) Any visual which could have both of them in the same color map */
/* (3) (figdisp only) Bitmap in the default color map, line in anything */
/* (4) (figdisp only) Line in the default color map, bitmap in anything */
/* (5) (figdisp only) Bitmap and line in separate, non default anything */
/* (6) Reduce lg.colors to 16 (if possible) and repeat */
/* (7) Get a visual for bitmap graphics (steps 1-5) */
/* (8) Try to get a read-only visual & color map for line graphics with the */
/*	original number of colors */
/* (9) Reduce lg.colors to 16 (if possible) and repeat step 8. */
/* (10) Reduce lg.colors to 2 and use BlackPixel and WhitePixel in default */

/* Return Values: */
/* FAIL		If we couldn't get any appropriate visual combination. */
/* SUCCEED	If everything went fine. */

/* Sam Southard, Jr. */
/* Created: 20-Feb-1992 (complete re-write of getbmvisual routine) */
/* Modification History: */
/* 25-Feb-1992	SNS/CIT	Now initializes bit & line graphics color maps to the */
/*			default. */
/*  5-Mar-1992	SNS/CIT	Now handles grayscale and read-only visuals better */
/*  6-Mar-1992	SNS/CIT	getcolors now takes a min and max visual depth. */
/*  9-Mar-1992	SNS/CIT	Silly bug in two color mode fixed */

#include "figdisp.h"
#include "globals.h"
#include "messages.h"

#include <stdio.h>

#define MAX_DEPTH	24	/* the maximum visual depth to use */

int getvisuals()
{
	int i;			/* Silly loop variable */
	int linecolors;		/* max number of line colors */
	XColor color;		/* a color table entry */
	Visual *defvis;		/* The default visual */
	int defro;		/* if the default visual is read-only */

	linecolors=lg.colors=res.lgcolors;	/* let's be optimistic */
	bm.bw=lg.bw=lg.ro=0;		/* more optimism */

	bitvisual=linevisual=NULL;
	bitcmap=linecmap=DefaultColormap(display,screen);
	defvis=DefaultVisual(display, screen);

	/* see if the default visual is read-only */
	if (defvis->class == StaticGray || defvis->class == StaticColor ||
	    defvis->class == TrueColor) defro=1;
	else defro=0;

#ifdef PGDISP
	/* we don't have any bitmap colors in pgdisp */
	res.maxcolors=res.maxpcolors=res.mincolors=res.minpcolors=0;
#endif
	/* we have our standards */
	if (lg.colors < 2) lg.colors=res.lgcolors=linecolors=2;

	/* Go ahead and allocate room for the number of colors we may need. */
	/* Don't worry about waste, since it won't be very large */
	i= lg.colors;
	if (res.savecolors > res.leavecolors) i += res.savecolors;
	else i += res.leavecolors;
	if ((lg.pix=(unsigned long *)malloc(i*sizeof(unsigned long))) == NULL)
	{
		(void)fprintf(stderr,MSG_MALLOC);
		return(FAIL);
	}
	if (res.maxpcolors < res.maxcolors) i=res.maxcolors;
	else i=res.maxpcolors;
	i += lg.colors;
	if (res.savecolors > res.leavecolors) i += res.savecolors;
	else i += res.leavecolors;
	if ((bm.pix=(unsigned long *)malloc(i*sizeof(unsigned long))) == NULL)
	{
		(void)fprintf(stderr,MSG_MALLOC);
		free(lg.pix);
		return(FAIL);
	}

	/* As long as we're not doing Black & White */
	while (lg.colors > 2)
	{
		/* First step is to simply try the default color map, if it's */
		/* not read-only, and then any visual which could handle them */
		/* both.  Bitmap graphics requires an 8-bit visual. */

		if ((!defro &&
		      (bm.colors=getcolors(UseDefaultCmap, &bitvisual, &bitcmap,
			bm.pix, res.maxcolors+res.leavecolors+lg.colors,
			res.mincolors+res.leavecolors+lg.colors, &bitdepth, 8,
			8))) ||
		    (bm.colors=getcolors(UseRWVisual, &bitvisual, &bitcmap,
			bm.pix, res.maxpcolors+res.savecolors+lg.colors,
			res.minpcolors+res.savecolors+lg.colors, &bitdepth, 8,
			8)))
		{ /* success! */
			bm.colors -= lg.colors;	/* we allocated both */

			/* Free up the res.leavecolors extra we got */
			if (res.leavecolors > 0 &&
			    bitcmap == DefaultColormap(display,screen))
			{
				XFreeColors(display, bitcmap,
					bm.pix+bm.colors+lg.colors,
					res.leavecolors, 0);
				bm.colors -= res.leavecolors;
			}

			/* copy some from the default color map */
			if (bitcmap != DefaultColormap(display,screen))
			{
				for (i=0 ; i < res.savecolors ; ++i)
				{
					if (bitvisual->class == DirectColor)
						break;
					color.pixel=bm.pix[i];
					if (bm.pix[i] >= defvis->map_entries)
						continue;
					XQueryColor(display,
						DefaultColormap(display,screen),
						&color);
					color.flags = DoRed | DoGreen | DoBlue;
					XStoreColor(display, bitcmap, &color);
				}
				/* update the pixel table to forget.  Don't */
				/* just change the pointer because we'll get */
				/* in trouble when we do a free */
				for (i=0 ; i <bm.colors+lg.colors ; ++i)
					bm.pix[i]=bm.pix[i+res.savecolors];
				bm.colors -= res.savecolors;
			}

#ifdef PGDISP
			(void)printf("Got %d line colors in ", lg.colors);
#else
			(void)printf("Got %d bitmap and %d line colors in ",
				bm.colors, lg.colors);
#endif
			if (bitcmap == DefaultColormap(display, screen))
				(void)printf("default color map\n");
			else (void)printf("private color map\n");
		
			/* now get the line graphics pixels */
			for (i=0 ; i < lg.colors ; ++i)
				lg.pix[i]=bm.pix[bm.colors+i];

			linevisual=bitvisual;
			linecmap=bitcmap;
			linedepth=bitdepth;

			break; /* don't try other schemes */
		}

#ifndef PGDISP
		/* we can't put them together, maybe we can separate them */
		for (i=0 ; i < 3 ; ++i)
		{
			if (i == 0
			    && (bm.colors=getcolors(UseDefaultCmap, &bitvisual,
				&bitcmap, bm.pix, res.maxcolors+res.leavecolors,
				res.mincolors+res.leavecolors, &bitdepth, 8, 8))
			    && (lg.colors=getcolors(UseRWVisual, &linevisual,
				&linecmap, lg.pix, res.lgcolors+res.savecolors,
				res.lgcolors+res.savecolors, &linedepth,
				MAX_DEPTH, 1))
			    || i == 1
			    && (bm.colors=getcolors(UseRWVisual, &bitvisual,
				&bitcmap, bm.pix, res.maxpcolors+res.savecolors,
				res.minpcolors+res.savecolors, &bitdepth, 8, 8))
			    && (lg.colors=getcolors(UseDefaultCmap, &linevisual,
				&linecmap, lg.pix, res.lgcolors+res.leavecolors,
				res.lgcolors+res.leavecolors, &linedepth,
				MAX_DEPTH, 1))
			    || i == 2
			    && (bm.colors=getcolors(UseRWVisual, &bitvisual,
				&bitcmap, bm.pix, res.maxpcolors+res.savecolors,
				res.minpcolors+res.savecolors, &bitdepth, 8, 8))
			    && (lg.colors=getcolors(UseRWVisual, &linevisual,
				&linecmap, lg.pix, res.lgcolors+res.savecolors,
				res.lgcolors+res.savecolors, &linedepth,
				MAX_DEPTH, 1)))
				break;
			
			/* if we got here, we may have been able to allocate */
			/* some bitmap colors */
			if (bm.colors) XFreeColors(display, bitcmap, bm.pix,
				bm.colors, 0);
		}

		if (bm.colors && lg.colors)
		{ /* whew - something worked */
			if (bitcmap == DefaultColormap(display, screen))
			{
				if (res.leavecolors > 0) XFreeColors(display,
					bitcmap, bm.pix+bm.colors,
					res.leavecolors, 0);
				bm.colors -= res.leavecolors;
			} else { /* copy */
				for (i=0 ; i < res.savecolors ; ++i)
				{
					if (bitvisual->class == DirectColor)
						break;
					color.pixel=bm.pix[i];
					if (bm.pix[i] >= defvis->map_entries)
						continue;
					XQueryColor(display,
						DefaultColormap(display,screen),
						&color);
					color.flags= DoRed | DoGreen | DoBlue;
					XStoreColor(display, bitcmap, &color);
				}
				bm.colors -= res.savecolors;
				/* forget about the first few pixels */
				for (i=0 ; i < bm.colors ; ++i)
					bm.pix[i]=bm.pix[i+res.savecolors];
			}
			(void)printf("Got %d bitmap colors in ", bm.colors);
			if (bitcmap == DefaultColormap(display, screen))
				(void)printf("default color map\n");
			else (void)printf("private color map\n");
				
			if (linecmap == DefaultColormap(display, screen))
			{
				if (res.leavecolors > 0) XFreeColors(display,
					linecmap, lg.pix+lg.colors,
					res.leavecolors, 0);
				lg.colors -= res.leavecolors;
			} else { /* copy */
				for (i=0 ; i < res.savecolors ; ++i)
				{
					if (linevisual->class == DirectColor)
						break;
					color.pixel=lg.pix[i];
					if (lg.pix[i] >= defvis->map_entries)
						continue;
					XQueryColor(display,
						DefaultColormap(display,screen),
						&color);
					color.flags= DoRed | DoGreen | DoBlue;
					XStoreColor(display, linecmap, &color);
				}
				lg.colors -= res.savecolors;
				/* forget about the first few pixels */
				for (i=0 ; i < lg.colors ; ++i)
					lg.pix[i]=lg.pix[i+res.savecolors];
			}
			(void)printf("Got %d line graphics colors in ",
				lg.colors);
			if (linecmap == DefaultColormap(display, screen))
				(void)printf("default color map\n");
			else (void)printf("private color map\n");

			break; /* don't try other schemes */
		}
#endif /* FIGDISP */

		/* we did all we can with this many colors, so try with only */
		/* 16 colors */
		if (res.lgcolors > 16) res.lgcolors=lg.colors=16;
		else res.lgcolors=lg.colors=2;
	}

	if (linevisual != NULL && linevisual->class == GrayScale) lg.bw=1;
	if (bitvisual != NULL && bitvisual->class == GrayScale) bm.bw=1;

	/* did we make it out alive? */
	if (lg.colors > 2) return(SUCCEED);

#ifndef PGDISP
	/* we need a read-write for bitmap graphics */

	if ((bm.colors=getcolors(UseDefaultCmap, &bitvisual, &bitcmap, bm.pix,
		res.maxcolors+res.leavecolors, res.mincolors+res.leavecolors,
		&bitdepth))
	    || (bm.colors=getcolors(UseRWVisual, &bitvisual, &bitcmap, bm.pix,
		res.maxcolors+res.savecolors, res.mincolors+res.savecolors,
		&bitdepth)))
	{ /* success! */
		if (bitcmap == DefaultColormap(display, screen))
		{
			if (res.leavecolors > 0) XFreeColors(display, bitcmap,
				bm.pix+bm.colors, res.leavecolors, 0);
			bm.colors -= res.leavecolors;
		} else { /* copy */
			for (i=0 ; i < res.savecolors ; ++i)
			{
				if (bitvisual->class == DirectColor)
					break;
				color.pixel=bm.pix[i];
				if (bm.pix[i] >= defvis->map_entries)
					continue;
				XQueryColor(display,
					DefaultColormap(display,screen),
					&color);
				color.flags= DoRed | DoGreen | DoBlue;
				XStoreColor(display, bitcmap, &color);
			}
			bm.colors -= res.savecolors;
			/* forget about the first few pixels */
			for (i=0 ; i < bm.colors ; ++i)
				bm.pix[i]=bm.pix[i+res.savecolors];
		}
		(void)printf("Got %d bitmap colors in ", bm.colors);
		if (bitcmap == DefaultColormap(display, screen))
			(void)printf("default color map\n");
		else (void)printf("private color map\n");
	} else { /* we're not gonna make it - we need LUT manipulation */
		(void)fprintf(stderr, MSG_NOCOLORS);
		return(FAIL);
	}
#endif

	/* Let's try a read-only for line graphics */
	res.lgcolors=linecolors;
	lg.ro=1;

	while (res.lgcolors > 2)
	{
		if (lg.colors=getcolors(UseROVisual, &linevisual, &linecmap,
			lg.pix, res.lgcolors, res.lgcolors, &linedepth)) break;

		/* lower our standards */
		if (res.lgcolors > 16) res.lgcolors=16;
		else res.lgcolors=2;
	}

	if (res.lgcolors <= 2)
	{ /* Sorry, all we can do is BlackPixel & WhitePixel */
		lg.pix[0]=BlackPixel(display, screen);
		lg.pix[1]=WhitePixel(display, screen);
		linecmap=DefaultColormap(display, screen);
		linedepth=DefaultDepth(display, screen);
		linevisual=DefaultVisual(display, screen);
		lg.colors=2;
	}

	if (linevisual->class == StaticGray) lg.bw=1;
	if (bitvisual->class == GrayScale) bm.bw=1;

	(void)printf("Got %d read-only line graphics colors in ", lg.colors);
	if (linecmap == DefaultColormap(display, screen))
		(void)printf("default color map\n");
	else (void)printf("private color map\n");

	return(SUCCEED);
}
