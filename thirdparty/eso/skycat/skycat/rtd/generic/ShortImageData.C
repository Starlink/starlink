/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: ShortImageData.C,v 1.1.1.1 2006/01/12 16:38:29 abrighto Exp $" 
 *
 * ShortImageData.C - member functions for class ShortImageData
 *
 * See the man page ImageData(3) for a complete description of this class
 * library.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * T. Herlin       08/12/95  Added color scale functions to avoid sign problem
 */


#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <cstring>
#include <cassert>
#include <cmath>
#include "ShortImageData.h"
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "define.h"


/*
 * initialize conversion from base type to short (trivial in this case)
 * and scale the low and high cut levels to short range
 */
void ShortImageData::initShortConversion() 
{
    scaledLowCut_ = (short)lowCut_;
    scaledHighCut_ = (short)highCut_;
    if (haveBlank_)
	scaledBlankPixelValue_ = LOOKUP_BLANK; // PWD: use last bin
}


/*
 * Include some standard methods as (cpp macro) templates:
 * These are methods that are the same for all derived classes of ImageData,
 * except that they work on a different raw data type
 */
#define CLASS_NAME ShortImageData
#define DATA_TYPE short
#ifndef NTOH
#    define NTOH(x) SWAP16(x)
#endif
#include "ImageTemplates.icc"
#undef CLASS_NAME
#undef DATA_TYPE
#undef NTOH



