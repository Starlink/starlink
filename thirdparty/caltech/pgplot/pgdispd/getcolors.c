/* The getcolors routine allocates between mincolors and maxcolors colors in */
/* the specified kind of visual (default color map, read/write visual, or */
/* read only visual).  Visual is set to the visual used, cmap is set to the */
/* color map used, and pix is set to the pixels allocated.  If the attempt to */
/* allocate colors is unsuccessful the values of these variables are */
/* undefined and may have been changed.  If we are told to use a read only */
/* visual, no color map entries are actually allocated. */

/* Return Values: */
/* 0		We were unable to allocate the required number of colors */
/* other	The number of colors we allocated. */

/* Sam Southard, Jr. */
/* Created: 20-Feb-1992 */
/* Modification History: */
/* 24-Feb-1992	SNS/CIT	Now returns the visual depth as well */
/*  3-Mar-1992	SNS/CIT	Now looks at TrueColor and Direct Color visuals as */
/*			well.  Now examines the user's visual specs. */
/*  6-Mar-1992	SNS/CIT	Now takes max and min depth parameters */
/* 14-Apr-1992	SNS/CIT	Now uses a quickie version of log2 */

#include "figdisp.h"
#include "globals.h"

/* The visual classes to use for r/w color maps, in order of preference */
static int rwvis[]={DirectColor, PseudoColor, GrayScale};
static int nrwvis=sizeof(rwvis)/sizeof(rwvis[0]);

/* The visual classes to use for read only color maps, in order of preference */
static int rovis[]={TrueColor, StaticColor, StaticGray};
static int nrovis=sizeof(rovis)/sizeof(rovis[0]);

int getcolors(vistype, visual, cmap, pix, maxcolors, mincolors, depth,
	maxdepth, mindepth)
int vistype;		/* The type of visual to use */
Visual **visual;	/* The visual actually used */
Colormap *cmap;		/* The color map actually used */
unsigned long *pix;	/* The pixels allocated */
int maxcolors;		/* The maximum number of colors to allocate */
int mincolors;		/* The minimum number of colors to allocate */
int *depth;		/* The depth of the visual actually used */
int maxdepth;		/* The maximum allowed visual depth */
int mindepth;		/* The minimum allowed visual depth */
{
	XVisualInfo vinfo;	/* The template for our visual */
	unsigned long pmtmp[1];	/* temporary for plane masks */
	int i,j;	/* silly loop variables */
	int nvis;	/* The number of visual classes to deal with */
	int class;	/* The visual class to use */
	int colors;
	int defro;	/* if the default visual is read-only */

	/* first free up the color map.  If it's not been set yet, we do no */
	/* harm. */
	if (*cmap != DefaultColormap(display, screen))
	{
		XFreeColormap(display, *cmap);
		*cmap = DefaultColormap(display, screen);
	}

	if (vistype == UseDefaultCmap)
	{ /* This is the easy case */
		*visual=DefaultVisual(display, screen);
		/* make sure the user allows this kind of visual */
		if (res.visclass >= 0 && res.visclass != (*visual)->class)
			return(0);
		*cmap=DefaultColormap(display, screen);
		*depth=DefaultDepth(display, screen);

		/* see if the default visual is read-only */
		if ((*visual)->class == StaticGray ||
		    (*visual)->class == StaticColor ||
		    (*visual)->class == TrueColor) defro=1;
		else defro=0;

		/* Make sure we don't try too hard */
		if (maxcolors > (*visual)->map_entries)
			maxcolors=(*visual)->map_entries;
		
		/* we may have a read-only default color map */
		if (defro && maxcolors >= mincolors)
			return(maxcolors);

		while (maxcolors >= mincolors)
		{
			if (XAllocColorCells(display, *cmap, True, &pmtmp[0],
				0, pix, (unsigned)maxcolors)) break;
			--maxcolors;
		}
		if (maxcolors >= mincolors) return(maxcolors);
		else return(0);
	}

	/* For these cases we have to go looking for a visual */
	if (vistype == UseRWVisual) nvis=nrwvis;
	else nvis=nrovis;

	/* This is just a very specialized log2() */
	for (i=31 ; i >= 0 ; --i)
		if (mincolors & (1<<i)) break;
	if (i < 0) i=0;

	if (i > mindepth) mindepth=i;

	for (i=0 ; i < nvis ; ++i)
	{
		if (vistype == UseRWVisual) class=rwvis[i];
		else class=rovis[i];

		/* Make sure the user allows this kind of visual */
		*visual=DefaultVisual(display, screen);
		if (res.visclass >= 0 && res.visclass != class ||
		    res.visclass == DefaultVis && class != (*visual)->class)
			continue;

		*depth=maxdepth;

		/* find a visual deep enough with enough available colors */
		while (*depth >= mindepth)
		{
			if (XMatchVisualInfo(display, screen, *depth, class,
				&vinfo))
			{ /* we got a visual - can we allocate enough colors? */
				*visual=vinfo.visual;
				*cmap=XCreateColormap(display,
					RootWindow(display, screen), *visual,
					AllocNone);
				if (vistype == UseROVisual)
				{
					if (maxcolors > (*visual)->map_entries)
						maxcolors=
							(*visual)->map_entries;
					if (maxcolors < mincolors)
					{ /* this shouldn't happen... */
						--(*depth);
						continue;
					}
					return(maxcolors);
				}
				/* go ahead and try to allocate the colors */
				if (maxcolors > (*visual)->map_entries)
					colors= (*visual)->map_entries;
				else colors=maxcolors;
				while (colors >= mincolors)
				{
					if (XAllocColorCells(display, *cmap,
						True, &pmtmp[0], 0, pix,
						(unsigned)colors)) break;
					--colors;
				}
				/* Did we make it? */
				if (colors >= mincolors) return(colors);
				/* well, on to the next visual */
				if (*cmap != DefaultColormap(display, screen))
				{
					XFreeColormap(display, *cmap);
					*cmap = DefaultColormap(display,
						screen);
				}
			}
			--(*depth);
		}

		/* we weren't successful, so on to the next visual class */
	}

	/* Oh well...we gave it our best shot */
	return(0);
}
