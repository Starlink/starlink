/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: ShortImageData.C,v 1.7 1998/03/23 00:47:02 abrighto Exp $" 
 *
 * ShortImageData.C - member functions for class ShortImageData
 *
 * See the man page RTI(3) for a complete description of this class
 * library.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * T. Herlin       08/12/95  Added color scale functions to avoid sign problem
 */


#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <assert.h>
#include <math.h>
#include "ShortImageData.h"


/*
 * initialize conversion from base type to short (trivial in this case)
 * and scale the low and high cut levels to short range
 */
void ShortImageData::initShortConversion() 
{
    scaledLowCut_ = (short)lowCut_;
    scaledHighCut_ = (short)highCut_;
    if (haveBlank_)
	scaledBlankPixelValue_ = blank_;
}


/*
 * Include some standard methods as (cpp macro) templates:
 * These are methods that are the same for all derived classes of ImageData,
 * except that they work on a different raw data type
 */
#define CLASS_NAME ShortImageData
#define DATA_TYPE short
#include "ImageTemplates.C"
