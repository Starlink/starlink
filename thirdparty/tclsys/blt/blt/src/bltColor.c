
/*
 * bltColor.c --
 *
 *	This module contains routines for color allocation strategies
 *	used with color images in the BLT toolkit.
 *
 * Copyright 1997-1998 Lucent Technologies, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that the copyright notice and warranty
 * disclaimer appear in supporting documentation, and that the names
 * of Lucent Technologies any of their entities not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 *
 * Lucent Technologies disclaims all warranties with regard to this
 * software, including all implied warranties of merchantability and
 * fitness.  In no event shall Lucent Technologies be liable for any
 * special, indirect or consequential damages or any damages
 * whatsoever resulting from loss of use, data or profits, whether in
 * an action of contract, negligence or other tortuous action, arising
 * out of or in connection with the use or performance of this
 * software.
 */

/*
 * Color strategies of 8-bit visuals:
 *
 * Try to "best" represent an N-color image into 8-bit (256 color)
 * colormap.  The simplest method is use the high bits of each RGB
 * value (3 bits for red and green, 2 bits for blue).  But this
 * produces lots of contouring since the distribution of colors tends
 * to be clustered.  Other problems: probably can't allocate even 256
 * colors. Other applications will have already taken some color
 * slots.  Furthermore, we might be displaying several images, and we
 * can't assume that all images are representative of the colors used.
 *
 * If we use a private colormap, we may want to allocate some number
 * of colors from the default colormap to prevent flashing when
 * colormaps are switched.
 *
 * Switches:
 *
 *	-exact boolean		Try to match the colors of the image rather
 *				then generating a "best" color ramp.
 *
 *	-threshold value	Maximum average error. Indicates how far
 *				to reduce the quantized color palette.
 *
 *	-tolerance value	Allow colors within this distance to match.
 *				This will weight allocation towards harder
 *				to match colors, rather than the most
 *				frequent.
 *
 *	-mincolors number	Minimum number of reduced quantized colors.
 *				or color ramp.
 *
 *	-dither	boolean		Turn on/off Floyd-Steinberg dithering.
 *
 *	-keep number		Hint to keep the first N colors in the
 *				in the default colormap.  This only applies to
 *				private colormaps.
 *
 *	-ramp number		Number of colors to use in a linear ramp.
 *
 */

#include "bltInt.h"

#ifndef WIN32

#include "bltImage.h"

#define NCOLORS		256

#define	RED	0
#define	GREEN	1
#define BLUE	2

#define R0	(cubePtr->r0)
#define R1	(cubePtr->r1)
#define G0	(cubePtr->g0)
#define G1	(cubePtr->g1)
#define B0	(cubePtr->b0)
#define B1	(cubePtr->b1)

typedef struct box {
    int r0, r1;			/* min, max values: min exclusive max inclusive */
    int g0, g1;
    int b0, b1;
    int vol;
} Cube;

typedef struct RGB {
    unsigned short int red;
    unsigned short int green;
    unsigned short int blue;
} RGB;

/*
 *----------------------------------------------------------------------
 *
 * Histogram is in elements 1..HISTSIZE along each axis,
 * element 0 is for base or marginal value
 * NB: these must start out 0!
 *----------------------------------------------------------------------
 */

typedef struct ColorStats {
    float gm2[33][33][33];
    long int wt[33][33][33];
    long int mR[33][33][33];
    long int mG[33][33][33];
    long int mB[33][33][33];
} ColorStats;

typedef struct ColorControl {
    XColor usedColors[256];	/* Colors already use, queried from the
				 * current colormap. */
    int numUsedColors;		/* Number of color slots used. */
    int numFreeColors;		/* Number of free slots in the color map */

    int freqArr[NCOLORS];	/* Frequency of quantized colors used */

    int acceptError;		/* Threshold error value, to accept or not
				 * accept a colormap.  The error is the
				 * sum of the distances */

    int useExact;		/* If non-zero, indicates to use the colors
				 * from the image, not a linear color ramp */

    int numKeepColors;		/* When using a private colormap, these are
				 * the number of colors to "keep" from the
				 * default colormap, to limit flashing. */
    int useBest;

    int numWorstColors;		/* Number of the worst matching colors to
				 * allocate before all others. */
} ColorControl;



static int
BuildPalette(palettePtr, numReds, numGreens, numBlues)
    register RGB *palettePtr;
    unsigned int numReds, numGreens, numBlues;
{
    register unsigned int red, green, blue;
    register int numColors;
    unsigned int short redVal, greenVal, blueVal;

    numColors = 0;
    for (red = 0; red < numReds; red++) {
	redVal = (red * USHRT_MAX) / (numReds - 1);
	for (green = 0; green < numGreens; green++) {
	    greenVal = (green * USHRT_MAX) / (numGreens - 1);
	    for (blue = 0; blue < numBlues; blue++) {
		blueVal = (blue * USHRT_MAX) / (numBlues - 1);
		palettePtr->red = (unsigned short int)redVal;
		palettePtr->green = (unsigned short int)greenVal;
		palettePtr->blue = (unsigned short int)blueVal;
		palettePtr++, numColors++;
	    }
	}
    }
    return numColors;
}

/*
 *----------------------------------------------------------------------
 *
 * QueryColormap --
 *
 *	Fills an array or XColors with the color values (RGB and pixel)
 *	currently allocated in the colormap.
 *
 * Results:
 *	The number of colors allocated is returned. The array "colorArr"
 *	will contain the XColor values of each color in the colormap.
 *
 *----------------------------------------------------------------------
 */

static int
QueryColormap(display, colorMap, mapColors, numMapColorsPtr)
    Display *display;
    Colormap colorMap;
    XColor mapColors[];
    int *numMapColorsPtr;
{
    unsigned long int pixelValues[NCOLORS];
    int numAvail, numMapColors;
    register int i;
    register XColor *colorPtr;
    register unsigned long *indexPtr;
    int inUse[NCOLORS];

    /* Initially, we assume all color cells are allocated. */
    memset((char *)inUse, 0, sizeof(int) * NCOLORS);

    /*
     * Start allocating color cells.  This will tell us which color cells
     * haven't already been allocated in the colormap.  We'll release the
     * cells as soon as we find out how many there are.
     */
    numAvail = 0;
    for (indexPtr = pixelValues, i = 0; i < NCOLORS; i++, indexPtr++) {
	if (!XAllocColorCells(display, colorMap, False, NULL, 0, indexPtr, 1)) {
	    break;
	}
	inUse[*indexPtr] = TRUE;/* Indicate the cell is unallocated */
	numAvail++;
    }
    XFreeColors(display, colorMap, pixelValues, numAvail, 0);

    /*
     * Put the indices of the cells already allocated into a color array.
     * We'll use the array to query the RGB values of the allocated colors.
     */
    numMapColors = 0;
    colorPtr = mapColors;
    for (i = 0; i < NCOLORS; i++) {
	if (!inUse[i]) {
	    colorPtr->pixel = i;
	    colorPtr->flags = (DoRed | DoGreen | DoBlue);
	    colorPtr++, numMapColors++;
	}
    }
    XQueryColors(display, colorMap, mapColors, numMapColors);
    *numMapColorsPtr = numMapColors;
#ifndef notdef
    fprintf(stderr, "Number of colors (allocated/free) %d/%d\n", numMapColors,
	numAvail);
#endif
    return numAvail;
}

/*
 * Build 3-D color histogram of counts, R/G/B, c^2
 */
static void
Hist3d(statsPtr, image)
    ColorStats *statsPtr;
    ColorImage image;
{
    register int r, g, b;
    float table[NCOLORS];
    int numPixels;
    Pix32 *pixelPtr;
    register int i;

    /* Precompute table of squares. */
    for (i = 0; i < NCOLORS; i++) {
	table[i] = i * i;
    }
    numPixels = ColorImageWidth(image) * ColorImageHeight(image);
    pixelPtr = ColorImageData(image);
    for (i = 0; i < numPixels; i++, pixelPtr++) {
	/*
	 * Reduce the number of bits (5) per color component. This
	 * will keep the table size (2^15) reasonable without perceptually
	 * affecting the final image.
	 */
	r = (pixelPtr->Red >> 3) + 1;
	g = (pixelPtr->Green >> 3) + 1;
	b = (pixelPtr->Blue >> 3) + 1;
	statsPtr->wt[r][g][b] += 1;
	statsPtr->mR[r][g][b] += pixelPtr->Red;
	statsPtr->mG[r][g][b] += pixelPtr->Green;
	statsPtr->mB[r][g][b] += pixelPtr->Blue;
	statsPtr->gm2[r][g][b] +=
	    table[pixelPtr->Red] + table[pixelPtr->Green] + table[pixelPtr->Blue];
    }
}

/*
 *----------------------------------------------------------------------
 * At conclusion of the histogram step, we can interpret
 *   wt[r][g][b] = sum over voxel of P(c)
 *   mR[r][g][b] = sum over voxel of r*P(c)  ,  similarly for mG, mB
 *   m2[r][g][b] = sum over voxel of c^2*P(c)
 * Actually each of these should be divided by 'size' to give the usual
 * interpretation of P() as ranging from 0 to 1, but we needn't do that here.
 *----------------------------------------------------------------------
 */

/*
 *----------------------------------------------------------------------
 We now convert histogram into moments so that we can rapidly calculate
 * the sums of the above quantities over any desired box.
 *----------------------------------------------------------------------
 */

static void
M3d(wt, mR, mG, mB, gm2)	/* compute cumulative moments. */
    long int wt[33][33][33];	/* # pixels in voxel */
    long int mR[33][33][33];	/* Sum over voxel of red pixel values */
    long int mG[33][33][33];	/* Sum over voxel of green pixel values */
    long int mB[33][33][33];	/* Sum over voxel of blue pixel values */
    float gm2[33][33][33];	/* Variance */
{
    register unsigned char i, r, g, b;
    long int line, line_r, line_g, line_b;
    long int area[33], area_r[33], area_g[33], area_b[33];
    float line2, area2[33];

    for (r = 1; r <= 32; r++) {
	for (i = 0; i <= 32; ++i) {
	    area2[i] = area[i] = area_r[i] = area_g[i] = area_b[i] = 0;
	}
	for (g = 1; g <= 32; g++) {
	    line2 = line = line_r = line_g = line_b = 0;
	    for (b = 1; b <= 32; b++) {
		/* ind1 = RGBIndex(r, g, b); */

		line += wt[r][g][b];
		line_r += mR[r][g][b];
		line_g += mG[r][g][b];
		line_b += mB[r][g][b];
		line2 += gm2[r][g][b];

		area[b] += line;
		area_r[b] += line_r;
		area_g[b] += line_g;
		area_b[b] += line_b;
		area2[b] += line2;

		/* ind2 = ind1 - 1089; [r-1][g][b] */
		wt[r][g][b] = wt[r - 1][g][b] + area[b];
		mR[r][g][b] = mR[r - 1][g][b] + area_r[b];
		mG[r][g][b] = mG[r - 1][g][b] + area_g[b];
		mB[r][g][b] = mB[r - 1][g][b] + area_b[b];
		gm2[r][g][b] = gm2[r - 1][g][b] + area2[b];
	    }
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 *	Compute sum over a box of any given statistic
 *
 *----------------------------------------------------------------------
 */
static INLINE long int
Vol(cubePtr, mmt)
    struct box *cubePtr;
    long int mmt[33][33][33];
{
    return (mmt[R1][G1][B1] - mmt[R1][G1][B0] -
	mmt[R1][G0][B1] + mmt[R1][G0][B0] -
	mmt[R0][G1][B1] + mmt[R0][G1][B0] +
	mmt[R0][G0][B1] - mmt[R0][G0][B0]);
}

/*
 *----------------------------------------------------------------------
 *
 *	The next two routines allow a slightly more efficient
 *	calculation of Vol() for a proposed subbox of a given box.
 *	The sum of Top() and Bottom() is the Vol() of a subbox split
 *	in the given direction and with the specified new upper
 *	bound.
 *
 *----------------------------------------------------------------------
 */

/* Compute part of Vol(cubePtr, mmt) that doesn't depend on r1, g1, or b1 */
/* (depending on dir) */
static long int
Bottom(cubePtr, dir, mmt)
    struct box *cubePtr;
    unsigned char dir;
    long int mmt[33][33][33];
{
    switch (dir) {
    case RED:
	return (-mmt[R0][G1][B1] + mmt[R0][G1][B0] + mmt[R0][G0][B1] -
	    mmt[R0][G0][B0]);

    case GREEN:
	return (-mmt[R1][G0][B1] + mmt[R1][G0][B0] + mmt[R0][G0][B1] -
	    mmt[R0][G0][B0]);

    case BLUE:
	return (-mmt[R1][G1][B0] + mmt[R1][G0][B0] + mmt[R0][G1][B0] -
	    mmt[R0][G0][B0]);
    }
    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * Compute remainder of Vol(cubePtr, mmt), substituting pos for
 * r1, g1, or b1 (depending on dir)
 *
 *----------------------------------------------------------------------
 */
static long int
Top(cubePtr, dir, pos, mmt)
    struct box *cubePtr;
    unsigned char dir;
    int pos;
    long int mmt[33][33][33];
{
    switch (dir) {
    case RED:
	return (mmt[pos][G1][B1] - mmt[pos][G1][B0] -
	    mmt[pos][G0][B1] + mmt[pos][G0][B0]);

    case GREEN:
	return (mmt[R1][pos][B1] - mmt[R1][pos][B0] -
	    mmt[R0][pos][B1] + mmt[R0][pos][B0]);

    case BLUE:
	return (mmt[R1][G1][pos] - mmt[R1][G0][pos] -
	    mmt[R0][G1][pos] + mmt[R0][G0][pos]);
    }
    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 *	Compute the weighted variance of a box NB: as with the raw
 *	statistics, this is really the (variance * size)
 *
 *----------------------------------------------------------------------
 */
static float
Var(cubePtr, statsPtr)
    struct box *cubePtr;
    ColorStats *statsPtr;
{
    float dR, dG, dB, xx;

    dR = Vol(cubePtr, statsPtr->mR);
    dG = Vol(cubePtr, statsPtr->mG);
    dB = Vol(cubePtr, statsPtr->mB);
    xx = (statsPtr->gm2[R1][G1][B1] - statsPtr->gm2[R1][G1][B0] -
	statsPtr->gm2[R1][G0][B1] + statsPtr->gm2[R1][G0][B0] -
	statsPtr->gm2[R0][G1][B1] + statsPtr->gm2[R0][G1][B0] +
	statsPtr->gm2[R0][G0][B1] - statsPtr->gm2[R0][G0][B0]);

    return (xx - (dR * dR + dG * dG + dB * dB) / (float)Vol(cubePtr, statsPtr->wt));
}

/*
 *----------------------------------------------------------------------
 *
 *	We want to minimize the sum of the variances of two subboxes.
 *	The sum(c^2) terms can be ignored since their sum over both
 *	subboxes is the same (the sum for the whole box) no matter
 *	where we split.  The remaining terms have a minus sign in
 *	the variance formula, so we drop the minus sign and MAXIMIZE
 *	the sum of the two terms.
 *
 *----------------------------------------------------------------------
 */
static float
Maximize(cubePtr, dir, first, last, cut, wholeR, wholeG, wholeB, wholeW,
    statsPtr)
    struct box *cubePtr;
    unsigned char dir;
    int first, last, *cut;
    long int wholeR, wholeG, wholeB, wholeW;
    ColorStats *statsPtr;
{
    register long int halfR, halfG, halfB, halfW;
    long int baseR, baseG, baseB, baseW;
    register int i;
    register float temp, max;

    baseR = Bottom(cubePtr, dir, statsPtr->mR);
    baseG = Bottom(cubePtr, dir, statsPtr->mG);
    baseB = Bottom(cubePtr, dir, statsPtr->mB);
    baseW = Bottom(cubePtr, dir, statsPtr->wt);
    max = 0.0;
    *cut = -1;
    for (i = first; i < last; i++) {
	halfR = baseR + Top(cubePtr, dir, i, statsPtr->mR);
	halfG = baseG + Top(cubePtr, dir, i, statsPtr->mG);
	halfB = baseB + Top(cubePtr, dir, i, statsPtr->mB);
	halfW = baseW + Top(cubePtr, dir, i, statsPtr->wt);

	/* Now half_x is sum over lower half of box, if split at i */
	if (halfW == 0) {	/* subbox could be empty of pixels! */
	    continue;		/* never split into an empty box */
	} else {
	    temp = ((float)halfR * halfR + (float)halfG * halfG +
		(float)halfB * halfB) / halfW;
	}
	halfR = wholeR - halfR;
	halfG = wholeG - halfG;
	halfB = wholeB - halfB;
	halfW = wholeW - halfW;
	if (halfW == 0) {	/* Subbox could be empty of pixels! */
	    continue;		/* never split into an empty box */
	} else {
	    temp += ((float)halfR * halfR + (float)halfG * halfG +
		(float)halfB * halfB) / halfW;
	}
	if (temp > max) {
	    max = temp;
	    *cut = i;
	}
    }
    return max;
}

/*
 *----------------------------------------------------------------------
 *----------------------------------------------------------------------
 */
static int
Cut(set1, set2, statsPtr)
    struct box *set1, *set2;
    ColorStats *statsPtr;
{
    unsigned char dir;
    int cutRed, cutGreen, cutBlue;
    float maxRed, maxGreen, maxBlue;
    long int wholeR, wholeG, wholeB, wholeW;

    wholeR = Vol(set1, statsPtr->mR);
    wholeG = Vol(set1, statsPtr->mG);
    wholeB = Vol(set1, statsPtr->mB);
    wholeW = Vol(set1, statsPtr->wt);

    maxRed = Maximize(set1, RED, set1->r0 + 1, set1->r1, &cutRed,
	wholeR, wholeG, wholeB, wholeW, statsPtr);
    maxGreen = Maximize(set1, GREEN, set1->g0 + 1, set1->g1, &cutGreen,
	wholeR, wholeG, wholeB, wholeW, statsPtr);
    maxBlue = Maximize(set1, BLUE, set1->b0 + 1, set1->b1, &cutBlue,
	wholeR, wholeG, wholeB, wholeW, statsPtr);

    if ((maxRed >= maxGreen) && (maxRed >= maxBlue)) {
	dir = RED;
	if (cutRed < 0) {
	    return 0;		/* can't split the box */
	}
    } else {
	dir = ((maxGreen >= maxRed) && (maxGreen >= maxBlue)) ? GREEN : BLUE;
    }
    set2->r1 = set1->r1;
    set2->g1 = set1->g1;
    set2->b1 = set1->b1;

    switch (dir) {
    case RED:
	set2->r0 = set1->r1 = cutRed;
	set2->g0 = set1->g0;
	set2->b0 = set1->b0;
	break;

    case GREEN:
	set2->g0 = set1->g1 = cutGreen;
	set2->r0 = set1->r0;
	set2->b0 = set1->b0;
	break;

    case BLUE:
	set2->b0 = set1->b1 = cutBlue;
	set2->r0 = set1->r0;
	set2->g0 = set1->g0;
	break;
    }
    set1->vol = (set1->r1 - set1->r0) * (set1->g1 - set1->g0) *
	(set1->b1 - set1->b0);
    set2->vol = (set2->r1 - set2->r0) * (set2->g1 - set2->g0) *
	(set2->b1 - set2->b0);
    return 1;
}

/*
 *----------------------------------------------------------------------
 *--------------------------------------------------------------------
 */
static void
Mark(cubePtr, label, tag)
    struct box *cubePtr;
    int label;
    unsigned int tag[33][33][33];
{
    register int r, g, b;

    for (r = R0 + 1; r <= R1; r++) {
	for (g = G0 + 1; g <= G1; g++) {
	    for (b = B0 + 1; b <= B1; b++) {
		tag[r][g][b] = label;
	    }
	}
    }
}

static void
FindClosestColor(colorPtr, mapColors, numMapColors)
    ColorInfo *colorPtr;
    XColor mapColors[];
    int numMapColors;
{
    double r, g, b;
    register int i;
    double dist, min;
    XColor *lastMatch;
    register XColor *mapColorPtr;
    extern double bltPosInfinity;

    min = bltPosInfinity;	/* Any color is closer. */
    lastMatch = NULL;

    /* Linear search of color */

    mapColorPtr = mapColors;
    for (i = 0; i < numMapColors; i++, mapColorPtr++) {
	r = (double)mapColorPtr->red - (double)colorPtr->exact.red;
	g = (double)mapColorPtr->green - (double)colorPtr->exact.green;
	b = (double)mapColorPtr->blue - (double)colorPtr->exact.blue;

#ifdef notdef
	dist = (30 * r * r) + (59 * g * g) + (11 * b * b);
	dist = 212 * r * r + 715 * g * g + 72 * b * b;
	dist = 2.0 * (r * r) + (b * b) + 3.0 * (g * g);
#endif
	dist = (r * r) + (b * b) + (g * g);
	if (dist < min) {
	    min = dist;
	    lastMatch = mapColorPtr;
	}
    }
    colorPtr->best = *lastMatch;
    colorPtr->best.flags = (DoRed | DoGreen | DoBlue);
    colorPtr->error = (float)sqrt(min);
}

static int
CompareColors(a, b)
    void *a, *b;
{
    int diff;
    ColorInfo *i1Ptr, *i2Ptr;

    i1Ptr = *(ColorInfo **) a;
    i2Ptr = *(ColorInfo **) b;
#ifndef notdef
    /* Compare colors based upon frequency */
    diff = i2Ptr->freq - i1Ptr->freq;
    if (ABS(diff) > 100) {
	return diff;
    }
#endif
    if (i2Ptr->error > i1Ptr->error) {
	return 1;
    } else if (i2Ptr->error < i1Ptr->error) {
	return -1;
    }
    return 0;
}

static float
MatchColors(colorTabPtr, rgbPtr, numColors, numAvailColors, numMapColors,
    mapColors)
    struct ColorTable *colorTabPtr;
    register RGB *rgbPtr;
    int numColors;
    int numAvailColors;
    int numMapColors;
    XColor mapColors[NCOLORS];
{
    int numMatched;
    float sum;
    register int i;
    register ColorInfo *colorPtr;

    /*
     * For each quantized color, compute and store the error (i.e
     * the distance from a color that's already been allocated).
     * We'll use this information to sort the colors based upon how
     * badly they match and their frequency to the color image.
     */
    colorPtr = colorTabPtr->colorInfo;
    for (i = 0; i < numColors; i++, colorPtr++, rgbPtr++) {
	colorPtr->index = i;
	colorTabPtr->sortedColors[i] = colorPtr;
	colorPtr->exact.red = rgbPtr->red;
	colorPtr->exact.green = rgbPtr->green;
	colorPtr->exact.blue = rgbPtr->blue;
	colorPtr->exact.flags = (DoRed | DoGreen | DoBlue);
	FindClosestColor(colorPtr, mapColors, numMapColors);
    }

    /* Sort the colors, first by frequency (most to least), then by
     * matching error (worst to best).
     */
    qsort(colorTabPtr->sortedColors, numColors, sizeof(ColorInfo *),
	(QSortCompareProc *)CompareColors);

    for (i = 0; i < numColors; i++) {
	colorPtr = colorTabPtr->sortedColors[i];
	fprintf(stderr, "%d. %04x%04x%04x / %04x%04x%04x = %f (%d)\n", i,
	    colorPtr->exact.red, colorPtr->exact.green, colorPtr->exact.blue,
	    colorPtr->best.red, colorPtr->best.green, colorPtr->best.blue,
	    colorPtr->error, colorPtr->freq);
    }
    sum = 0.0;
    numMatched = 0;
    for (i = numAvailColors; i < numColors; i++) {
	colorPtr = colorTabPtr->sortedColors[i];
	sum += colorPtr->error;
	numMatched++;
    }
    if (numMatched > 0) {
	sum /= numMatched;
    }
    return sum;
}

static int
AllocateColors(numImageColors, colorTabPtr, matchOnly)
    int numImageColors;
    struct ColorTable *colorTabPtr;
    int matchOnly;
{
    register int i;
    register ColorInfo *colorPtr;
    unsigned long int pixelValue;

#ifndef notdef
    if (colorTabPtr->numPixels > 0) {
	fprintf(stderr, "freeing %d pixels\n", colorTabPtr->numPixels);
	XFreeColors(colorTabPtr->display, colorTabPtr->colorMap,
	    colorTabPtr->pixelValues, colorTabPtr->numPixels, 0);
    }
#endif
    for (i = 0; i < numImageColors; i++) {
	colorPtr = colorTabPtr->sortedColors[i];
	if (matchOnly) {
	    XAllocColor(colorTabPtr->display, colorTabPtr->colorMap,
		&colorPtr->best);
	    pixelValue = colorPtr->best.pixel;
	} else {
	    colorPtr->allocated = XAllocColor(colorTabPtr->display,
		colorTabPtr->colorMap, &colorPtr->exact);

	    if (colorPtr->allocated) {
		pixelValue = colorPtr->exact.pixel;
	    } else {
		XAllocColor(colorTabPtr->display, colorTabPtr->colorMap,
		    &colorPtr->best);
		pixelValue = colorPtr->best.pixel;
	    }
	}
	colorTabPtr->pixelValues[colorPtr->index] = pixelValue;
    }
    colorTabPtr->numPixels = numImageColors;
    return 1;
}

/*
 *----------------------------------------------------------------------
 *
 * QuantizeColorImage --
 *
 *	C Implementation of Wu's Color Quantizer (v. 2) (see Graphics Gems
 *	vol. II, pp. 126-133)
 *
 *	Author: Xiaolin Wu
 *		Dept. of Computer Science Univ. of Western
 *		Ontario London, Ontario
 *		N6A 5B7
 *		wu@csd.uwo.ca
 *
 *	Algorithm:
 *		Greedy orthogonal bipartition of RGB space for variance
 *		minimization aided by inclusion-exclusion tricks.  For
 *		speed no nearest neighbor search is done. Slightly
 *		better performance can be expected by more
 *		sophisticated but more expensive versions.
 *
 *	The author thanks Tom Lane at Tom_Lane@G.GP.CS.CMU.EDU for much of
 *	additional documentation and a cure to a previous bug.
 *
 *	Free to distribute, comments and suggestions are appreciated.
 *
 *----------------------------------------------------------------------
 */
static int
QuantizeColorImage(image, colorTabPtr)
    ColorImage image;		/* Source image */
    struct ColorTable *colorTabPtr;
{
    float error;
    int best = 1;
    struct box *cubePtr;
    struct box cubeArr[NCOLORS];
    int numColors, numFreeColors, numQuantized;
    int numBestColors;
    int next;
    register long int i, weight;
    register int k;
    float vv[NCOLORS], temp;
    ColorStats *statsPtr;
    float maxError;
    RGB palette[NCOLORS];
    RGB *rgbPtr;
    int numMapColors, numAvail;
    int matchOnly;
    XColor mapColors[NCOLORS];

    /*
     * Allocated a structure to hold color statistics.
    */
    statsPtr = (ColorStats *) calloc(1, sizeof(ColorStats));
    assert(statsPtr);

    Hist3d(statsPtr, image);
    M3d(statsPtr->wt, statsPtr->mR, statsPtr->mG, statsPtr->mB,
	statsPtr->gm2);

    switch (colorTabPtr->visualInfo.class) {
    case StaticGray:
    case StaticColor:
    case GrayScale:
	maxError = 320000.0;
	break;
    default:
	maxError = 121.0;
	break;
    }


  retry:
    numColors = NCOLORS;
    matchOnly = TRUE;
    if (best) {
	int numReds, numGreens, numBlues;
	static int colorVar[7][3] =
	{
	    {8, 8, 4, /*  #red, #green, #blue */ },
	    {7, 7, 4, /* 8 bits, 198 colors */ },
	    {5, 6, 4, /* 7 bits, 120 colors */ },
	    {4, 5, 3, /* 6 bits, 60 colors */ },
	    {3, 4, 2, /* 5 bits, 24 colors */ },
	    {2, 3, 2, /* 4 bits, 12 colors */ },
	    {2, 2, 2, /* 3 bits, 8 colors */ },
	};

	numAvail = QueryColormap(colorTabPtr->display, colorTabPtr->colorMap,
	    mapColors, &numMapColors);

	numReds = colorVar[1][RED];
	numGreens = colorVar[1][GREEN];
	numBlues = colorVar[1][BLUE];

	for (i = 0; i < 7; i++) {
	    numColors = BuildPalette(palette, numReds, numGreens, numBlues);
	    error = MatchColors(colorTabPtr, palette, numColors, numAvail,
		numMapColors, mapColors);
	    fprintf(stderr, "numColors=%d, error=%f\n", numColors, error);
	    if (error < 1.0) {
		break;
	    }
	    /*
	     * Reduce the number of shades of each primary to about
	     * 3/4 of the previous value.  This should reduce the
	     * total number of colors required to about half the
	     * previous value for PseudoColor displays.
	     * (Huh?. Try doing the math again).
	     */

	    numReds = (numReds * 3 + 2) / 4;
	    numGreens = (numGreens * 3 + 2) / 4;
	    numBlues = (numBlues * 3 + 2) / 4;
	}
	AllocateColors(numColors, colorTabPtr, FALSE);
	numBestColors = numColors;
	/* numColors = NCOLORS; */
	matchOnly = TRUE;
	maxError = 320000.0;
	numFreeColors = 0;
    }
    numAvail = QueryColormap(colorTabPtr->display, colorTabPtr->colorMap,
	mapColors, &numMapColors);

    for (;;) {
	cubeArr[0].r0 = cubeArr[0].g0 = cubeArr[0].b0 = 0;
	cubeArr[0].r1 = cubeArr[0].g1 = cubeArr[0].b1 = 32;

	next = 0;

	for (i = 1; i < numColors; i++) {
	    if (Cut(cubeArr + next, cubeArr + i, statsPtr)) {
		/*
		 * Volume test ensures we won't try to cut one-cell box
		 */
		vv[next] = (cubeArr[next].vol > 1) ?
		    Var(cubeArr + next, statsPtr) : 0.0;
		vv[i] = (cubeArr[i].vol > 1) ?
		    Var(cubeArr + i, statsPtr) : 0.0;
	    } else {
		vv[next] = 0.0;	/* don't try to split this box again */
		i--;		/* didn't create box i */
	    }

	    next = 0;
	    temp = vv[0];
	    for (k = 1; k <= i; k++) {
		if (vv[k] > temp) {
		    temp = vv[k];
		    next = k;
		}
	    }
	    if (temp <= 0.0) {
		numColors = i + 1;
		fprintf(stderr, "Only got %d boxes\n", numColors);
		break;
	    }
	}
	rgbPtr = palette;
	for (cubePtr = cubeArr, k = 0; k < numColors; k++, cubePtr++, rgbPtr++) {
	    weight = Vol(cubePtr, statsPtr->wt);
	    colorTabPtr->colorInfo[k].freq = weight;
	    if (weight) {
		rgbPtr->red =
		    (Vol(cubePtr, statsPtr->mR) / weight) * (NCOLORS + 1);
		rgbPtr->green =
		    (Vol(cubePtr, statsPtr->mG) / weight) * (NCOLORS + 1);
		rgbPtr->blue =
		    (Vol(cubePtr, statsPtr->mB) / weight) * (NCOLORS + 1);
	    } else {
		fprintf(stderr, "bogus box %d\n", k);
		rgbPtr->red = rgbPtr->green = rgbPtr->blue = 0;
	    }
	}
	error = MatchColors(colorTabPtr, palette, numColors, numAvail,
	    numMapColors, mapColors);
	fprintf(stderr, "!!numColors=%d, error=%f\n", numColors, error);
	if ((error > maxError) && (numColors > 32)) {
	    numColors /= 2;
	    continue;
	}
	numQuantized = numColors;
	break;			/* Break out of loop */
    }
    if (!AllocateColors(numQuantized, colorTabPtr, matchOnly)) {
	goto retry;		/* Color map changed */
    }
    for (cubePtr = cubeArr, k = 0; k < numQuantized; k++, cubePtr++) {
	Mark(cubePtr, colorTabPtr->pixelValues[k], colorTabPtr->lut);
    }
    free((char *)statsPtr);
    return numQuantized;
}

/*
 *----------------------------------------------------------------------
 *
 * AllocateBestColors --
 *
 *	C Implementation of Wu's Color Quantizer (v. 2) (see Graphics Gems
 *	vol. II, pp. 126-133)
 *
 *	Author: Xiaolin Wu
 *		Dept. of Computer Science Univ. of Western
 *		Ontario London, Ontario
 *		N6A 5B7
 *		wu@csd.uwo.ca
 *
 *	Algorithm:
 *		Greedy orthogonal bipartition of RGB space for variance
 *		minimization aided by inclusion-exclusion tricks.  For
 *		speed no nearest neighbor search is done. Slightly
 *		better performance can be expected by more
 *		sophisticated but more expensive versions.
 *
 *	The author thanks Tom Lane at Tom_Lane@G.GP.CS.CMU.EDU for much of
 *	additional documentation and a cure to a previous bug.
 *
 *	Free to distribute, comments and suggestions are appreciated.
 *
 *----------------------------------------------------------------------
 */
static int
AllocateBestColors(image, colorTabPtr)
    ColorImage image;		/* Source image */
    struct ColorTable *colorTabPtr;
{
    float error;
    struct box *cubePtr;
    struct box cubeArr[NCOLORS];
    int numColors, numFreeColors;
    int numBestColors;
    int next;
    register long int i, weight;
    register int k;
    float vv[NCOLORS], temp;
    ColorStats *statsPtr;
    float maxError;
    RGB palette[NCOLORS];
    RGB *rgbPtr;
    int numMapColors, numAvail;
    int matchOnly;
    XColor mapColors[NCOLORS];
    unsigned int pixelValues[NCOLORS];
    int numReds, numGreens, numBlues;
    static int paletteChoice[7][3] =
    {
	{8, 8, 4, /*  #red, #green, #blue */ },
	{7, 7, 4, /* 8 bits, 198 colors */ },
	{5, 6, 4, /* 7 bits, 120 colors */ },
	{4, 5, 3, /* 6 bits, 60 colors */ },
	{3, 4, 2, /* 5 bits, 24 colors */ },
	{2, 3, 2, /* 4 bits, 12 colors */ },
	{2, 2, 2, /* 3 bits, 8 colors */ },
    };

    numColors = NCOLORS;
    matchOnly = TRUE;


    numAvail = QueryColormap(colorTabPtr->display, colorTabPtr->colorMap,
	mapColors, &numMapColors);

    numReds = paletteChoice[1][RED];
    numGreens = paletteChoice[1][GREEN];

    numBlues = paletteChoice[1][BLUE];

    for (i = 0; i < 7; i++) {
	numColors = BuildPalette(palette, numReds, numGreens, numBlues);
	error = MatchColors(colorTabPtr, palette, numColors, numAvail,
	    numMapColors, mapColors);
	fprintf(stderr, "numColors=%d, error=%f\n", numColors, error);
	if (error < 1.0) {
	    break;
	}
	/*
	 * Reduce the number of shades of each primary to about
	 * 3/4 of the previous value.  This should reduce the
	 * total number of colors required to about half the
	 * previous value for PseudoColor displays.
	 * (Huh?. Try doing the math again).
	 */
	numReds = (numReds * 3 + 2) / 4;
	numGreens = (numGreens * 3 + 2) / 4;
	numBlues = (numBlues * 3 + 2) / 4;
    }

    AllocateColors(numColors, colorTabPtr, FALSE);

    numBestColors = numColors;
    matchOnly = TRUE;
    maxError = 320000.0;
    numFreeColors = 0;

    /*
     * Allocated a structure to hold color statistics.
     */
    statsPtr = (ColorStats *) calloc(1, sizeof(ColorStats));
    assert(statsPtr);

    Hist3d(statsPtr, image);
    M3d(statsPtr->wt, statsPtr->mR, statsPtr->mG, statsPtr->mB,
	statsPtr->gm2);

    switch (colorTabPtr->visualInfo.class) {
    case StaticGray:
    case StaticColor:
    case GrayScale:
	maxError = 320000.0;
	break;
    default:
	maxError = 1.0;
	break;
    }

    numAvail = QueryColormap(colorTabPtr->display, colorTabPtr->colorMap,
	mapColors, &numMapColors);

    cubeArr[0].r0 = cubeArr[0].g0 = cubeArr[0].b0 = 0;
    cubeArr[0].r1 = cubeArr[0].g1 = cubeArr[0].b1 = 32;

    next = 0;

    numColors = NCOLORS;
    for (i = 1; i < numColors; i++) {
	if (Cut(cubeArr + next, cubeArr + i, statsPtr)) {
	    /*
	     * Volume test ensures we won't try to cut one-cell box
	     */
	    vv[next] = (cubeArr[next].vol > 1) ?
		Var(cubeArr + next, statsPtr) : 0.0;
	    vv[i] = (cubeArr[i].vol > 1) ?
		Var(cubeArr + i, statsPtr) : 0.0;
	} else {
	    vv[next] = 0.0;	/* don't try to split this box again */
	    i--;		/* didn't create box i */
	}

	next = 0;
	temp = vv[0];
	for (k = 1; k <= i; k++) {
	    if (vv[k] > temp) {
		temp = vv[k];
		next = k;
	    }
	}
	if (temp <= 0.0) {
	    numColors = i + 1;
	    fprintf(stderr, "Only got %d boxes\n", numColors);
	    break;
	}
    }
    rgbPtr = palette;
    for (cubePtr = cubeArr, k = 0; k < numColors; k++, cubePtr++, rgbPtr++) {
	weight = Vol(cubePtr, statsPtr->wt);
	colorTabPtr->colorInfo[k].freq = weight;
	if (weight) {
	    rgbPtr->red = (Vol(cubePtr, statsPtr->mR) / weight) * (NCOLORS + 1);
	    rgbPtr->green = (Vol(cubePtr, statsPtr->mG) / weight) * (NCOLORS + 1);
	    rgbPtr->blue = (Vol(cubePtr, statsPtr->mB) / weight) * (NCOLORS + 1);
	} else {
	    fprintf(stderr, "bogus box %d\n", k);
	    rgbPtr->red = rgbPtr->green = rgbPtr->blue = 0;
	}
    }
    error = MatchColors(colorTabPtr, palette, numColors, numAvail,
	numMapColors, mapColors);
    fprintf(stderr, "!!numColors=%d, error=%f\n", numColors, error);

    for (k = 0; k < numColors; k++) {
	pixelValues[k] = colorTabPtr->colorInfo[k].best.pixel;
    }
    for (cubePtr = cubeArr, k = 0; k < numColors; k++, cubePtr++) {
	Mark(cubePtr, pixelValues[k], colorTabPtr->lut);
    }
    free((char *)statsPtr);
    return numColors;
}

ColorTable
Blt_CreateColorTable(tkwin)
    Tk_Window tkwin;
{
    XVisualInfo visualInfo, *visualInfoPtr;
    int numVisuals;
    Visual *visualPtr;
    Display *display;
    struct ColorTable *colorTabPtr;

    display = Tk_Display(tkwin);
    visualPtr = Tk_Visual(tkwin);

    colorTabPtr = (ColorTable) calloc(1, sizeof(struct ColorTable));
    assert(colorTabPtr);
    colorTabPtr->display = Tk_Display(tkwin);
    colorTabPtr->colorMap = Tk_Colormap(tkwin);

    visualInfo.screen = Tk_ScreenNumber(tkwin);
    visualInfo.visualid = XVisualIDFromVisual(visualPtr);
    visualInfoPtr = XGetVisualInfo(display, VisualScreenMask | VisualIDMask,
	&visualInfo, &numVisuals);

    colorTabPtr->visualInfo = *visualInfoPtr;
    XFree(visualInfoPtr);

    return colorTabPtr;
}

void
Blt_FreeColorTable(colorTabPtr)
    struct ColorTable *colorTabPtr;
{
    if (colorTabPtr == NULL) {
	return;
    }
    if (colorTabPtr->numPixels > 0) {
	XFreeColors(colorTabPtr->display, colorTabPtr->colorMap,
	    colorTabPtr->pixelValues, colorTabPtr->numPixels, 0);
    }
    free((char *)colorTabPtr);
}

extern int redAdjust, greenAdjust, blueAdjust;
extern int redMaskShift, greenMaskShift, blueMaskShift;

/*
 *----------------------------------------------------------------------
 *
 * Blt_DirectColorTable --
 *
 *	Creates a color table using the DirectColor visual.  We try
 *	to allocate colors across the color spectrum.
 *
 * Results:
 *	The color image is converted to greyscale.
 *
 *----------------------------------------------------------------------
 */
ColorTable
Blt_DirectColorTable(interp, tkwin, image)
    Tcl_Interp *interp;
    Tk_Window tkwin;
    ColorImage image;
{
    struct ColorTable *colorTabPtr;
    Visual *visualPtr;
    Display *display;
    XColor color;
    int numReds, numGreens, numBlues;
    int redBand, greenBand, blueBand;
    int lastRed, lastGreen, lastBlue;
    unsigned int red, green, blue;
    unsigned int value;
    register int i;

    display = Tk_Display(tkwin);
    visualPtr = Tk_Visual(tkwin);

    colorTabPtr = Blt_CreateColorTable(tkwin);
    /*
     * Compute the number of distinct colors in each band
     */
    numReds = ((unsigned int)visualPtr->red_mask >> redMaskShift) + 1;
    numGreens = ((unsigned int)visualPtr->green_mask >> greenMaskShift) + 1;
    numBlues = ((unsigned int)visualPtr->blue_mask >> blueMaskShift) + 1;

#ifdef notdef
    assert((numReds <= visualPtr->map_entries) &&
	(numGreens <= visualPtr->map_entries) &&
	(numBlues <= visualPtr->map_entries));
#endif
    redBand = NCOLORS / numReds;
    greenBand = NCOLORS / numGreens;
    blueBand = NCOLORS / numBlues;

  retry:
    color.flags = (DoRed | DoGreen | DoBlue);
    lastRed = lastGreen = lastBlue = 0;
    red = green = blue = 0;
    for (i = 0; i < visualPtr->map_entries; i++) {
	if (lastRed < NCOLORS) {
	    red = lastRed + redBand;
	    if (red > NCOLORS) {
		red = NCOLORS;
	    }
	}
	if (lastGreen < NCOLORS) {
	    green = lastGreen + greenBand;
	    if (green > NCOLORS) {
		green = NCOLORS;
	    }
	}
	if (lastBlue < NCOLORS) {
	    blue = lastBlue + blueBand;
	    if (blue > NCOLORS) {
		blue = NCOLORS;
	    }
	}
	color.red = (red - 1) * (NCOLORS + 1);
	color.green = (green - 1) * (NCOLORS + 1);
	color.blue = (blue - 1) * (NCOLORS + 1);

	if (!XAllocColor(display, colorTabPtr->colorMap, &color)) {
	    XFreeColors(display, colorTabPtr->colorMap,
		colorTabPtr->pixelValues, i, 0);
	    if ((colorTabPtr->flags & PRIVATE_COLORMAP) == 0) {
		/*
		 * If we can't allocate a color in the default
		 * colormap, try again, this time with a private
		 * colormap.
		 */
		fprintf(stderr, "Need to allocate private colormap\n");
		colorTabPtr->colorMap = Tk_GetColormap(interp, tkwin, ".");
		
		XSetWindowColormap(display, Tk_WindowId(tkwin), colorTabPtr->colorMap);
		colorTabPtr->flags |= PRIVATE_COLORMAP;
		goto retry;
	    }
	    fprintf(stderr, "Failed to allocate after %d colors\n", i);
	    free((char *)colorTabPtr);
	    return NULL;	/* Ran out of colors in private map? */
	}
	colorTabPtr->pixelValues[i] = color.pixel;
	/*
	 * Fill in pixel values for each band at this intensity
	 */
	value = color.pixel & visualPtr->red_mask;
	while (lastRed < red) {
	    colorTabPtr->red[lastRed++] = value;
	}
	value = color.pixel & visualPtr->green_mask;
	while (lastGreen < green) {
	    colorTabPtr->green[lastGreen++] = value;
	}
	value = color.pixel & visualPtr->blue_mask;
	while (lastBlue < blue) {
	    colorTabPtr->blue[lastBlue++] = value;
	}
    }
    colorTabPtr->numPixels = i;
    return colorTabPtr;
}

/*
 * First attempt:
 *	Allocate colors all the colors in the image (up to NCOLORS). Bail out
 *	on the first failure or if we need more than NCOLORS.
 */
static int
GetUniqueColors(image)
    ColorImage image;
{
    register int i, numColors;
    register Pix32 *pixelPtr;
    Pix32 color;
    Tcl_HashEntry *hPtr;
    int isNew, numPixels;
    int refCount;
    Tcl_HashTable colorTable;
    
    Tcl_InitHashTable(&colorTable, TCL_ONE_WORD_KEYS);
    
    numPixels = ColorImageWidth(image) * ColorImageHeight(image);
    numColors = 0;
    pixelPtr = ColorImageData(image);
    for (i = 0; i < numPixels; i++, pixelPtr++) {
	color.value = pixelPtr->value;
	color.Alpha = 0;	/* Ignore alpha-channel values */
	hPtr = Tcl_CreateHashEntry(&colorTable, (char *)color.value, &isNew);
	if (isNew) {
	    refCount = 1;
	    numColors++;
	} else {
	    refCount = (int)Tcl_GetHashValue(hPtr);
	    refCount++;
	}
	Tcl_SetHashValue(hPtr, (ClientData)refCount);
    }
    Tcl_DeleteHashTable(&colorTable);
    return numColors;
}


#define Blt_DefaultColormap(tkwin)  \
	DefaultColormap(Tk_Display(tkwin), Tk_ScreenNumber(tkwin))


static void
PrivateColormap(interp, colorTabPtr, image, tkwin)
     Tcl_Interp *interp;
     struct ColorTable *colorTabPtr;
     ColorImage image;
     Tk_Window tkwin;
{
    int keepColors = 0;
    int best = 1;
    register int i;
    XColor usedColors[NCOLORS];
    int numFreeColors, numUsedColors;
    Colormap colorMap;
    int inUse[NCOLORS];
    XColor *colorPtr;
    XColor *imageColors;
    
    /*
     * Create a private colormap if one doesn't already exist for the
     * window.
     */
    
    colorTabPtr->colorMap = colorMap = Tk_Colormap(tkwin);
    
    numUsedColors = 0;	/* Number of colors allocated */
    
    if (colorTabPtr->numPixels > 0) {
	XFreeColors(colorTabPtr->display, colorTabPtr->colorMap,
		    colorTabPtr->pixelValues, colorTabPtr->numPixels, 0);
    }
    numFreeColors = QueryColormap(colorTabPtr->display, colorMap, usedColors,
				  &numUsedColors);
    memset((char *)inUse, 0, sizeof(int) * NCOLORS);
    if ((numUsedColors == 0) && (keepColors > 0)) {
	
	/*
	 * We're starting with a clean colormap so find out what colors
	 * have been used in the default colormap.
	 */
	
	numFreeColors = QueryColormap(colorTabPtr->display,
	      Blt_DefaultColormap(tkwin), usedColors, &numUsedColors);
	
	/*
	 * Copy a number of colors from the default colormap into the private
	 * colormap.  We can assume that this is the working set from most
	 * (non-image related) applications. While this doesn't stop our
	 * image from flashing and looking dumb when colormaps are swapped
	 * in and out, at least everything else should remain unaffected.
	 */

	if (numUsedColors > keepColors) {
	    numUsedColors = keepColors;
	}
	/*
	 * We want to allocate colors in the same ordering as the old colormap,
	 * and we can't assume that the colors in the old map were contiguous.
	 * So mark the colormap locations (i.e. pixels) that we find in use.
	 */

    }
    for (colorPtr = usedColors, i = 0; i < numUsedColors; i++, colorPtr++) {
	inUse[colorPtr->pixel] = TRUE;
    }
    
    /*
     * In an "exact" colormap, we try to allocate as many of colors from the
     * image as we can fit.  If necessary, we'll cheat and reduce the number
     * of colors by quantizing.
     */
    imageColors = usedColors + numUsedColors;
    
    if (best) {
	AllocateBestColors(image, colorTabPtr);
    } else {
	QuantizeColorImage(image, colorTabPtr);
    }
    Tk_SetWindowColormap(tkwin, colorMap);
}

ColorTable
Blt_PseudoColorTable(interp, tkwin, image)
    Tcl_Interp *interp;
    Tk_Window tkwin;
    ColorImage image;
{
    struct ColorTable *colorTabPtr;
    Colormap defColorMap;
    int usePrivate;
    
    colorTabPtr = Blt_CreateColorTable(tkwin);
    defColorMap = DefaultColormap(colorTabPtr->display, Tk_ScreenNumber(tkwin));
    if (colorTabPtr->colorMap == defColorMap) {
	fprintf(stderr, "Using default colormap\n");
    }
    /* All other visuals use an 8-bit colormap */
    colorTabPtr->lut = (unsigned int *)malloc(sizeof(unsigned int) * 33 * 33 * 33);
    assert(colorTabPtr->lut);
    
    usePrivate = TRUE;
    if (usePrivate) {
	PrivateColormap(interp, colorTabPtr, image, tkwin);
    } else {
#ifdef notdef
	ReadOnlyColormap(colorTabPtr, image, tkwin);
#endif
    }
    return colorTabPtr;
}



#ifdef notdef

static void
ConvoleColorImage(srcImage, destImage, kernelPtr)
    ColorImage srcImage, destImage;
    ConvoleKernel *kernelPtr;
{
    Pix32 *srcPtr, *destPtr;
    Pix32 *src[MAXROWS];
    register int x, y, i, j;
    int red, green, blue;
    
    /* i = 0 case, ignore left column of pixels */
    
    srcPtr = ColorImageData(srcImage);
    destPtr = ColorImageData(destImage);
    
    width = ColorImageWidth(srcImage);
    height = ColorImageHeight(srcImage);
    
    yOffset = kernelPtr->height / 2;
    xOffset = kernelPtr->width / 2;
    for (y = yOffset; y < (height - yOffset); y++) {
	/* Set up pointers to individual rows */
	for (i = 0; i < kernelPtr->height; i++) {
	    src[i] = srcPtr + (i * width);
	}
	for (x = xOffset; x < (width - xOffset); x++) {
	    red = green = blue = 0;
	    kernPtr = kernelPtr->values;
	    for (i = 0; i < kernelPtr->height; i++) {
		for (j = 0; j < kernelPtr->width; j++) {
		    red += *valuePtr * src[i][j].Red;
		    green += *valuePtr * src[i][j].Green;
		    blue += *valuePtr * src[i][j].Blue;
		    valuePtr++;
		}
	    }
	    destPtr->Red = red / kernelPtr->sum;
	    destPtr->Green = green / kernelPtr->sum;
	    destPtr->Blue = blue / kernelPtr->sum;
	    destPtr++;
	}
	srcPtr += width;
    }
    sum = bot[0].Red +
	red = bot[0].Red + bot[1].Red + mid[1].Red + top[0].Red + top[1].Red;
    green = bot[0].Green + bot[1].Green + mid[1].Green + top[0].Green +
	top[1].Green;
    blue = bot[0].Blue + bot[1].Blue + mid[1].Blue + top[0].Blue + top[1].Blue;
    error = (red / 5) - mid[0].Red;
    redVal = mid[0].Red - (error * blend / blend_divisor);
    error = (green / 5) - mid[0].Green;
    greenVal = mid[0].Green - (error * blend / blend_divisor);
    error = (blue / 5) - mid[0].Blue;
    blueVal = mid[0].Blue - (error * blend / blend_divisor);
    
    out[0].Red = CLAMP(redVal);
    out[0].Green = CLAMP(greenVal);
    out[0].Blue = CLAMP(blueVal);
    
    for (i = 1; i < (width - 1); i++) {
	for (chan = 0; chan < 3; chan++) {
	    total = bot[chan][i - 1] + bot[chan][i] + bot[chan][i + 1] +
		mid[chan][i - 1] + mid[chan][i + 1] +
		top[chan][i - 1] + top[chan][i] + top[chan][i + 1];
	    avg = total >> 3;	/* divide by 8 */
	    diff = avg - mid[chan][i];
	    result = mid[chan][i] - (diff * blend / blend_divisor);
	    out[chan][i] = CLAMP(result);
	}
    }
    /* i = in_hdr.xmax case, ignore right column of pixels */
    for (chan = 0; chan < 3; chan++) {
	total = bot[chan][i - 1] + bot[chan][i] +
	    mid[chan][i - 1] +
	    top[chan][i - 1] + top[chan][i];
	avg = total / 5;
	diff = avg - mid[chan][i];
	result = mid[chan][i] - (diff * blend / blend_divisor);
	out[chan][i] = CLAMP(result);
    }
}

static void
DitherRow(srcImage, destImage, lastRow, curRow)
    ColorImage srcImage;
    ColorImage destImage;
    int width, height;
    int bottom, top;
{
    int width, height;
    
    width = ColorImageWidth(srcImage);
    topPtr = ColorImageData(destPtr) + (width * row);
    rowPtr = topPtr + width;
    botPtr = rowPtr + width;
    
    for (x = 0; x < width; x++) {
	
	/* Clamp current error entry */
	
	midPtr->red = CLAMP(midPtr->red);
	midPtr->blue = CLAMP(midPtr->blue);
	midPtr->green = CLAMP(midPtr->green);
	
	r = (midPtr->red >> 3) + 1;
	g = (midPtr->green >> 3) + 1;
	b = (midPtr->blue >> 3) + 1;
	index = colorTabPtr->lut[r][g][b];
	
	redVal = midPtr->red * (NCOLORS + 1);
	greenVal = midPtr->green * (NCOLORS + 1);
	blueVal = midPtr->blue * (NCOLORS + 1);
	
	error = colorVal - colorMap[index].red;
	if (x < 511) {
	    currRow[x + 1].Red = currRow[x + 1].Red + 7 * error / 16;
	    nextRow[x + 1].Red = nextRow[x + 1].Red + error / 16;
	}
	nextRow[x].Red = nextRow[x].Red + 5 * error / 16;
	if (x > 0) {
	    nextRow[x - 1].Red = nextRow[x - 1].Red + 3 * error / 16;
	}
	error = row[x][c] - colormap[index][c];
	
	value = srcPtr->channel[i] * error[i];
	value = CLAMP(value);
	destPtr->channel[i] = value;
	
	/* Closest pixel */
	pixel = PsuedoColorPixel();
	error[RED] = colorPtr->Red - srcPtr->Red * (NCOLORS + 1);
	
	/* translate pixel to colorInfoPtr to get error */
	colorTabPtr->lut[r][g][b];
	colorPtr = PixelToColorInfo(pixel);
	error = colorPtr->error;

	register rle_pixel *optr;
	register int j;
	register short *thisptr, *nextptr = NULL;
	int chan;
	static int numchan = 0;
	int lastline = 0, lastpixel;
	static int *cval = 0;
	static rle_pixel *pixel = 0;
	
	if (numchan != in_hdr->ncolors)
	    if (cval) {
		free(cval);
		free(pixel);
	    }
	numchan = in_hdr->ncolors;
	if (!cval) {
	    if ((cval = (int *)malloc(numchan * sizeof(int))) == 0)
		MALLOC_ERR;
	    if ((pixel = (rle_pixel *) malloc(numchan * sizeof(rle_pixel))) == 0)
		MALLOC_ERR;
	}
	optr = outrow[RLE_RED];
	
	thisptr = row_top;
	if (row_bottom)
	    nextptr = row_bottom;
	else
	    lastline = 1;
	
	for (x = 0; x < width; x++) {
	    int cmap_index = 0;
	    
	    lastpixel = (x == (width - 1));
	    val = srcPtr->Red;
	    
	    for (chan = 0; chan < 3; chan++) {
		cval[chan] = *thisptr++;
		
		/* 
		 * Current channel value has been accumulating error,
		 * it could be out of range.  
		 */
		if (cval[chan] < 0)
		    cval[chan] = 0;
		else if (cval[chan] > 255)
		    cval[chan] = 255;
		
		pixel[chan] = cval[chan];
	    }
	    
	    /* find closest color */
	    find_closest(map, numchan, maplen, pixel, &cmap_index);
	    *optr++ = cmap_index;
	    
	    /* thisptr is now looking at pixel to the right of current pixel
	     * nextptr is looking at pixel below current pixel
	     * So, increment thisptr as stuff gets stored.  nextptr gets moved
	     * by one, and indexing is done +/- numchan.
	     */
	    for (chan = 0; chan < numchan; chan++) {
		cval[chan] -= map[chan][cmap_index];
		
		if (!lastpixel) {
		    thisptr[chan] += cval[chan] * 7 / 16;
		}
		if (!lastline) {
		    if (j != 0) {
			nextptr[-numchan] += cval[chan] * 3 / 16;
		    }
		    nextptr[0] += cval[chan] * 5 / 16;
		    if (!lastpixel) {
			nextptr[numchan] += cval[chan] / 16;
		    }
		    nextptr++;
		}
	    }
	}
    }
}    


/********************************************/
static ColorImage
DoColorDither(pic24, pic8, w, h, rmap, gmap, bmap, rdisp, gdisp, bdisp, maplen)
    byte *pic24, *pic8, *rmap, *gmap, *bmap, *rdisp, *gdisp, *bdisp;
    int w, h, maplen;
{
    /* takes a 24 bit picture, of size w*h, dithers with the colors in
       rdisp, gdisp, bdisp (which have already been allocated),
       and generates an 8-bit w*h image, which it returns.
       ignores input value 'pic8'
       returns NULL on error
       
       note: the rdisp,gdisp,bdisp arrays should be the 'displayed' colors,
       not the 'desired' colors
       
       if pic24 is NULL, uses the passed-in pic8 (an 8-bit image) as
       the source, and the rmap,gmap,bmap arrays as the desired colors */
    
    byte *np, *ep, *newpic;
    short *cache;
    int r2, g2, b2;
    int *thisline, *nextline, *thisptr, *nextptr, *tmpptr;
    int i, j, rerr, gerr, berr, pwide3;
    int imax, jmax;
    int key;
    long cnt1, cnt2;
    int error[512];	/* -255 .. 0 .. +255 */
    
    /* compute somewhat non-linear floyd-steinberg error mapping table */
    for (i = j = 0; i <= 0x40; i++, j++) {
	error[256 + i] = j;
	error[256 - i] = -j;
    }
    for ( /*empty*/ ; i < 0x80; i++, j += !(i & 1) ? 1 : 0) {
	error[256 + i] = j;
	error[256 - i] = -j;
    }
    for ( /*empty*/ ; i <= 0xff; i++) {
	error[256 + i] = j;
	error[256 - i] = -j;
    }
    
    cnt1 = cnt2 = 0;
    pwide3 = w * 3;
    imax = h - 1;
    jmax = w - 1;
    ep = (pic24) ? pic24 : pic8;
    
    /* attempt to malloc things */
    newpic = (byte *) malloc((size_t) (w * h));
    cache = (short *)calloc((size_t) (2 << 14), sizeof(short));
    thisline = (int *)malloc(pwide3 * sizeof(int));
    nextline = (int *)malloc(pwide3 * sizeof(int));
    if (!cache || !newpic || !thisline || !nextline) {
	if (newpic)
	    free(newpic);
	if (cache)
	    free(cache);
	if (thisline)
	    free(thisline);
	if (nextline)
	    free(nextline);
	return (byte *) NULL;
    }
    np = newpic;
    
    /* Get first line of picture in reverse order. */
    
    srcPtr = ColorImageData(image), tempPtr = tempArr;
    for (x = 0; x < width; x++, tempPtr++, srcPtr--) {
	*tempPtr = *srcPtr;
    }
    
    for (y = 0; y < height; y++) {
	tempPtr = curRowPtr, curRowPtr = nextRowPtr, nextRowPtr = tempPtr;
	
	if (y != (height - 1)) {	/* get next line */
	    for (x = 0; x < width; x++, tempPtr++, srcPtr--)
		*tempPtr = *srcPtr;
	}
    }
    
    
    free(thisline);
    free(nextline);
    free(cache);
    
    return newpic;
}


static void
DitherImage(image)
     ColorImage image;
{
    int width, height;
    
    
    
}
#endif

#endif /* WIN32 */
