/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: UShortImageData.C,v 1.7 1998/03/23 00:47:02 abrighto Exp $" 
 *
 * UShortImageData.C - member functions for class UShortImageData
 *
 * See the man page RTI(3) for a complete description of this class
 * library.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 */


#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <math.h>
#include "UShortImageData.h"


/*
 * initialize conversion from base type to short and scale the low and
 * high cut levels to short range.
 */
void UShortImageData::initShortConversion() 
{
    scaledLowCut_ = (ushort)lowCut_;
    scaledHighCut_ = (ushort)highCut_;
    if (haveBlank_)
	scaledBlankPixelValue_ = LOOKUP_BLANK;
}

/*
 * Include some standard methods as (cpp macro) templates:
 * These are methods that are the same for all derived classes of ImageData,
 * except that they work on a different raw data type
 */
#define CLASS_NAME UShortImageData
#define DATA_TYPE ushort
#include "ImageTemplates.C"
