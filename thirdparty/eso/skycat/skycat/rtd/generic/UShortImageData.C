/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: UShortImageData.C,v 1.1.1.1 2006/01/12 16:38:30 abrighto Exp $" 
 *
 * UShortImageData.C - member functions for class UShortImageData
 *
 * See the man page ImageData(3) for a complete description of this class
 * library.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 */


#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <cstring>
#include <cmath>
#include "UShortImageData.h"
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "define.h"


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
#ifndef NTOH
#    define NTOH(x) SWAP16(x)
#endif
#include "ImageTemplates.icc"
#undef CLASS_NAME
#undef DATA_TYPE
#undef NTOH
