// -*-c++-*-
#ifndef _LongImageData_h_
#define _LongImageData_h_
/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: LongImageData.h,v 1.11 1998/07/28 21:23:37 abrighto Exp $" 
 * $Id: LongImageData.h,v 1.11 1998/07/28 21:23:37 abrighto Exp $
 *
 * LongImageData.h - class definitions for class LongImageData
 *
 * See the man page RTI(3) for a complete description of this class
 * library.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * Peter W. Draper 24/01/97  Added FITS_LONG macro changes
 */

#include <sys/types.h>
#include "ImageData.h"
#include "config.h"

// The type "long" may have up to 64 bits on alpha machines. FITS
// defines the long we want to use as 32 bits, so use a macro to
// replace the long data type with plain int when appropriate.
#if SIZEOF_LONG == 8
#define FITS_LONG int
#else
#define FITS_LONG long
#endif


// This class is used for images where the raw data is made up of ints

class LongImageData : public ImageData {
private:
    // value of blank pixel, if known (if haveBlankPixel_ is nonzero)
    FITS_LONG blank_;

    int bias_;			// offset for long to short conversion
    double dbias_;		// used when scale != 1.0
    double scale_;		// factor for long to short conversion
    int scaled_;		// flag, true if scale_ != 1.0

    // local methods used to get short index in lookup table
    short convertToShort(FITS_LONG);	// convert long to short by adding bias
    short scaleToShort(FITS_LONG);	// as above, but with scaling

    ushort convertToUshort(FITS_LONG l) { // unsigned
	return ushort(scaled_ ? scaleToShort(l) : convertToShort(l));
    }

    // return X image pixel value for raw image value
    byte lookup(FITS_LONG l) {
	return lookup_[convertToUshort(l)];
    }

protected:

    // initialize conversion from base type to short,
    void initShortConversion();

public:
    // constructor
    LongImageData(const char* name, const ImageIO& imio, int verbose)
	: ImageData(name, imio, verbose), 
	  blank_(0) {}

    // return the data type of the raw data
    int dataType() {return LONG_IMAGE;}

    // return true if the data type is signed
    int isSigned() {return 1;}

    // return a copy of this object
    ImageData* copy() {return new LongImageData(*this);}

    // include declarations for methods that differ only in raw data type
#   include "ImageTemplates.h"
};


#endif _LongImageData_h_
