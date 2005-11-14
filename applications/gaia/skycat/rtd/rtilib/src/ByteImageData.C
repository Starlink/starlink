/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: ByteImageData.C,v 1.4 2005/02/02 01:43:02 brighton Exp $" 
 *
 * ByteImageData.C - member functions for class ByteImageData
 *
 * See the man page ImageData(3) for a complete description of this class
 * library.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * Peter W. Draper 15/03/99  Blank bin is always 128
 * pbiereic        17/02/03  Added 'using namespace std'.
 */
static const char* const rcsId="@(#) $Id: ByteImageData.C,v 1.4 2005/02/02 01:43:02 brighton Exp $";


using namespace std;
#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <cstring>
#include <cassert>
#include <cmath>
#include "ByteImageData.h"
#include "define.h"


/*
 * This method is called from the parent class to convert the high and
 * low cut values to short range. In this case, there is no conversion
 * needed, since bytes are already in range.
 */

void ByteImageData::initShortConversion()
{ 
    scaledLowCut_ = (int)lowCut_;
    scaledHighCut_ = (int)highCut_;
    if (haveBlank_)
	scaledBlankPixelValue_ = 128;
}


/*
 * Include some standard methods as (cpp macro) templates:
 * These are methods that are the same for all derived classes of ImageData,
 * except that they work on a different raw data type
 */
#define CLASS_NAME ByteImageData
#define DATA_TYPE byte
#define NTOH(x) (x)
#include "ImageTemplates.C"
#undef CLASS_NAME
#undef DATA_TYPE
#undef NTOH


