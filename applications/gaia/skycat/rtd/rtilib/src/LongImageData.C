/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: LongImageData.C,v 1.9 1998/03/23 00:47:02 abrighto Exp $" 
 *
 * LongImageData.C - member functions for class LongImageData
 *
 * See the man page RTI(3) for a complete description of this class
 * library.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * Peter W. Draper 24/01/97  Inverted sense of scaled_ test.
 *                           Changed to use macro FITS_LONG as long is 
 *                           64 bit on alphas and the "long" integers
 *                           read in from FITS files are 32 bit.
 */


#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <assert.h>
#include <math.h>
#include "LongImageData.h"


/* 
 * convert the given long int to short by adding the integer bias
 * and checking the range
 */
short LongImageData::convertToShort(FITS_LONG l) 
{
    register int s = l + bias_;
    if (s < LOOKUP_MIN)
	s = LOOKUP_MIN;
    else if (s > LOOKUP_MAX)
	s = LOOKUP_MAX;
    return s;
}


/* 
 * convert the given long int to short by adding the integer bias,
 * scaling, rounding if necessary and checking the range
 */
short LongImageData::scaleToShort(FITS_LONG l) 
{
    short s;
    double d = (l + dbias_) * scale_;
    if (d < 0.0 ) {
	if((d -= 0.5) < LOOKUP_MIN)
	    s = LOOKUP_MIN;
	else
	    s = (short)d;
    } 
    else {
	if((d += 0.5) > LOOKUP_MAX)
	    s = LOOKUP_MAX;
	else
	    s = (short)d;
    }
    return s;
}


/*
 * initialize conversion from base type long to short 
 * and scale the low and high cut levels to short range
 *
 * Method: 3 member variables are set here and used later to convert 
 * the long raw image data to short, which is then used as an index 
 * in the lookup table to get the byte value:
 * 
 *   bias_   = offset
 *   dbias_  = double offset, used when scale != 1.0 
 *   scale_  = scale factor for conversion
 * : 
 */
void LongImageData::initShortConversion() 
{
    if ((highCut_ - lowCut_) <= (LOOKUP_WIDTH)) {
	// if a simple offset suffices 
	scale_ = 1.0;
	if ((lowCut_ >= LOOKUP_MIN) && (highCut_ <= LOOKUP_MAX)) {
	    // if possible to truncate to short without bias, do so. 
	    bias_ = 0;
	} 
	else {
	    // offset by average to center within range 
	    bias_ = (int)(-((lowCut_ + highCut_) / 2.0));
	}
	dbias_ = bias_;
	scaledLowCut_ = convertToShort((FITS_LONG)lowCut_);
	scaledHighCut_ = convertToShort((FITS_LONG)highCut_);
	if (haveBlank_)
	    scaledBlankPixelValue_ = convertToShort(blank_);
    } 
    else {
	// full-up scaling required. (+/- (tmax-tmin)/2) 
	// offset values to be zero centered 
	scale_ = LOOKUP_WIDTH / (highCut_ - lowCut_);
	dbias_ = -((lowCut_ + highCut_) * 0.5);
	bias_ = (int)dbias_;
	scaledLowCut_ = scaleToShort((FITS_LONG)lowCut_);
	scaledHighCut_ = scaleToShort((FITS_LONG)highCut_);
	if (haveBlank_)
	    scaledBlankPixelValue_ = scaleToShort(blank_);
    }
    // set int flag for later quick check if scale_ != 1.0
    scaled_ = (scale_ != 1.0);  // Sense inverted - PWD
}


/*
 * Include some standard methods as (cpp macro) templates:
 * These are methods that are the same for all derived classes of ImageData,
 * except that they work on a different raw data type
 */
#define CLASS_NAME LongImageData
#define DATA_TYPE FITS_LONG
#include "ImageTemplates.C"
