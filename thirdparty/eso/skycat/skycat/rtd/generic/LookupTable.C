/*
 * E.S.O. - VLT project / ESO Archive
 *
 * "@(#) $Id: LookupTable.C,v 1.8 1998/07/28 21:23:46 abrighto Exp $" 
 *
 * LookupTable.C - method definitions for class LookupTable, managing image
 *                 pixel lookup tables for converting pixel values to 
 *                 XImage bytes, with scaling algorithms applied.
 *
 * This class is used to convert image pixel values to bytes, with color
 * scaling algorithms applied, for displaying in an XImage.  For example,
 * to convert 16-bit shorts to bytes, a lookup table of size 64k is
 * used. For byte values, a lookup table of size 256 can be used. For
 * integers, floats and doubles, it is best to scale the values down to
 * shorts first and then use a 64k sized lookup table to apply the
 * scaling algorithms (linear, log, sqrt, histeq). Using a large lookup 
 * table takes more memory, but saves time later when converting pixel
 * values for display.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  09 Aug 96  Created
 */
static const char* const rcsId="@(#) $Id: LookupTable.C,v 1.8 1998/07/28 21:23:46 abrighto Exp $";


#include <math.h>
#include <stdlib.h>
#include "define.h"
#include "error.h"
#include "LookupTable.h"
#include "FitsIO.h"

// borrowed this from saoimage
extern "C" {
#include "histeq.h"
}

// this is only defined on SUN ?
#undef expm1
#define expm1(a)  (exp(a)-1)


/*
 * constructor: create new image memory area
 */
LookupTable::LookupTable(int size)
    : rep_(new LookupTableRep(size))
{
}


/*
 * copy constructor - increment the reference count...
 */
LookupTable::LookupTable(const LookupTable& im) 
    : rep_(im.rep_)
{
    if (rep_) 
	rep_->refcnt_++;
}


/*
 * destructor - delete if there are no more references.
 */
LookupTable::~LookupTable() 
{
    if (rep_ && --rep_->refcnt_ <= 0) 
	delete rep_;
}


/*
 * assignment operator
 */
LookupTable& LookupTable::operator=(const LookupTable& im)
{
    im.rep_->refcnt_++;		// protect against "im = im"
    if (rep_ && --rep_->refcnt_ <= 0) {
	delete rep_;
    }
    rep_ = im.rep_;
    return *this;
}


/* ---------- internal rep ----------- */


/*
 * constructor - internal rep
 */
LookupTableRep::LookupTableRep(int size) 
    : lookup_(new unsigned long[size]),
      size_(size),
      refcnt_(1),
      status_(0)
{
    if (lookup_ == NULL)
	status_ = error("no memory for lookup table");
}


/*
 * destructor - internal rep
 */
LookupTableRep::~LookupTableRep() 
{
    delete lookup_;
}


/* -------------- color scaling -------------------- */



/* 
 * fill in the lookup table at the end after the high cut level has been
 * reached.
 *
 * pixval is the color value to use for image values >= imageval.
 * isSigned is a flag: should be set to true if the pixel values are signed.
 */
void LookupTableRep::fillLookup(int pixval, int imageval, int isSigned)
{
    // note: unsigned image values fill up the lookup array sequentially 
    // but for signed image values, -1 is at the end due to the cast to 
    // (ushort) to get a positive index.
    int n = isSigned ? size_/2 : size_;

    while( imageval < n) {
	ushort v = (ushort)imageval++;
	if (v < size_) {
	    lookup_[v] = pixval;  
	}
    }
}


/*
 * local util method: set the values in the lookup table from imageval to
 * imagelim to the given pixel value and increment imageval to the
 * new index. 
 * Returns 1 if we've reached the end of the lookup table and should break
 * the loop.
 */
int LookupTableRep::setLookup(int& imageval, int imagelim, int pixval)
{
    int ret = 0;

    // limit to size of lookup table 
    if (imagelim > size_) {
	imagelim = size_;
	ret = 1;
    }

    while(imageval < imagelim) {
	ushort v = (ushort)imageval++;
	if (v < size_)
	    lookup_[v] = pixval;
    }
    
    return ret;
}


/*
 * set the color value for a specific pixel value (blank pixel, for example)
 */
void LookupTableRep::setPixelColor(int pixval, unsigned long color)
{
    ushort v = (ushort)pixval;
    if (v < size_)
	lookup_[v] = color;
}


/*
 * Set up the color lookup table for linear scaling.  
 *
 * lcut and hcut are the low and high cut levels (scaled to the short range).
 * isSigned is a flag: should be set to true if the pixel values are signed.
 * ncolors is the number of pixel color values in the colors[] array.
 */
void LookupTableRep::linearScale(int lcut, int hcut, int isSigned, 
				 int ncolors, unsigned long* colors)
{
    // input range / output range yields input cells per output cell 
    double scale = (double)(hcut - lcut + 1) / ncolors;

    // upper bound is ideal edge between colors (offset for rounding) 
    double upper_bound = lcut + 0.5;

    int maxcolor = ncolors - 1;
    int imageval = lcut;
    int level = 0;
    register int pixval = colors[0];
    register int imagelim;

    while( level++ < maxcolor ) {
	upper_bound += scale;

	// limit to size of lookup table 
	imagelim = (int)upper_bound;

	if (setLookup(imageval, imagelim, pixval))
	    break;

	// level was inc'd after loop test, make pixval for next round 
	pixval = colors[level];
    }

    // fill in at top if short of highCut 
    fillLookup(pixval, imageval, isSigned);
}


/* 
 * set up the color lookup table for logarithmic scaling:
 * distribute color levels in the map by a logorithmic or
 * exponential curve (powers of e). 
 * 
 * lcut and hcut are the low and high cut levels (scaled to the short range).
 * isSigned is a flag: should be set to true if the pixel values are signed.
 * ncolors is the number of pixel color values in the colors[] array.
 * expo is the optional exponent (def: 10.0).
 */
void LookupTableRep::logScale(int lcut, int hcut, int isSigned, 
			      int ncolors, unsigned long* colors, 
			      double expo)
{
    int level = 0;
    register int imagelim;
    register int pixval = colors[0];
    int maxcolor = ncolors - 1;
    int imageval = lcut;
    double scale;

    // base distribution on e**n as n goes from 0 to expo 
    if(expo >= 0 ) {
	scale = (double)(hcut - lcut + 1) / expm1(expo);
    } else {
	// negative exponents allocate more levels toward the high values 
	scale = (double)(hcut - lcut + 1) / (1.0 - exp(expo));
    }

    while(level++ < maxcolor) {
	if (expo > 0) {
	    imagelim = lcut + (int)
		((expm1(((double)level / ncolors) * expo) * scale) + 0.5);
	} else {
	    imagelim = lcut + (int)
		((1.0-exp(((double)level / ncolors) * expo) * scale) + 0.5);
	}
	// limit map range to image values 
	if (imagelim > hcut)
	    imagelim = hcut;

	if (setLookup(imageval, imagelim, pixval))
	    break;

	// level was inc'd after loop test, make pixval for next round 
	pixval = colors[level];
    }

    // fill in at top if short of highCut 
    fillLookup(pixval, imageval, isSigned);
}


/* 
 * set up the color lookup table for exponential scaling
 * 
 * lcut and hcut are the low and high cut levels (scaled to the short range).
 * isSigned is a flag: should be set to true if the pixel values are signed.
 * ncolors is the number of pixel color values in the colors[] array.
 * expo is the optional exponent (def: 10.0).
 */
void LookupTableRep::sqrtScale(int lcut, int hcut, int isSigned, 
			       int ncolors, unsigned long* colors, 
			       double expo)
{
    int maxcolor = ncolors - 1;
    int imageval = lcut;
    double range = hcut - lcut + 1;
    int level = 0;
    register int imagelim;
    register int pixval = colors[0];

    while( level++ < maxcolor ) {
	imagelim = lcut + (int)
	    ((pow(((double)level / ncolors), expo) * range) + 0.5);

	// limit map range to image values 
	if( imagelim > hcut )
	    imagelim = hcut;

	if (setLookup(imageval, imagelim, pixval))
	    break;

	// level was inc'd after loop test, make pixval for next round 
	pixval = colors[level];
    }
    // fill in at top if short of hcut 
    fillLookup(pixval, imageval, isSigned);
}


/*
 * Set up the color lookup table for histogram equalization scaling
 *
 * lcut and hcut are the low and high cut levels (scaled to the short
 * range).
 *
 * isSigned is a flag: should be set to true if the pixel values are
 * signed.
 *
 * minval is the min image pixel value. This is needed to map an index in
 * xyvalues to a pixel value, since xyvalue[0] corresponds to minval.
 *
 * ncolors is the number of pixel color values in the colors[] array.
 *
 * histogram is an array[SHORT_SIZE] generated from the image and maps 
 * ushort image values to the number of pixels that have that value.
 * 
 * area is the area (w*h) of the image area examined for the histogram.
 */
void LookupTableRep::histeqScale(int lcut, int hcut, int isSigned, 
			         int ncolors, unsigned long* colors,
				 int* histogram, int area)
{
    // if there are not enought pixel levels to distribute, 
    // (or if it is a small lookup table) do linear scaling instead
    if ((hcut - lcut) <= ncolors) {
	linearScale(lcut, hcut, isSigned, ncolors, colors);
	return;
    }

    // int scaleOffset = SHORT_SIZE/2;  // offset from allocated array to zero
    histogram_equalize(lookup_, histogram, area, lcut, hcut, ncolors, colors);
}
