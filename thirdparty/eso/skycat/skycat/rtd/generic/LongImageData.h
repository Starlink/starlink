// -*-c++-*-
/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: LongImageData.h,v 1.3 2005/02/02 01:43:03 brighton Exp $" 
 * $Id: LongImageData.h,v 1.3 2005/02/02 01:43:03 brighton Exp $
 *
 * LongImageData.h - class definitions for class LongImageData
 *
 * See the man page ImageData(3) for a complete description of this class
 * library.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * Peter W. Draper 24/01/97  Added FITS_LONG macro changes
 * P.Biereichel    22/03/99  Added definitions for bias subtraction
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

class LongImageData : public ImageData 
{
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

    inline ushort convertToUshort(FITS_LONG l) { // unsigned
	return ushort(scaled_ ? scaleToShort(l) : convertToShort(l));
    }

    // return X image pixel value for raw image value
    inline byte lookup(FITS_LONG l) {
	return lookup_[convertToUshort(l)];
    }
    inline unsigned long llookup(FITS_LONG l) {
	return lookup_[convertToUshort(l)];
    }

    // return NTOH converted value evtl. subtracted with corresponding bias value
    FITS_LONG getVal(FITS_LONG* p, int idx);

    int getXsamples(FITS_LONG *rawImage, int idx, int wbox, FITS_LONG *samples);
    int getBsamples(FITS_LONG *rawImage, int idx, int wbox, FITS_LONG *samples);
    int getCsamples(FITS_LONG *rawImage, int idx, int wbox, FITS_LONG *samples);
    FITS_LONG getMedian(FITS_LONG *samples, int n);
    FITS_LONG getBoxVal(FITS_LONG *rawImage, int idx, int wbox, FITS_LONG *samples, int xs);
    FITS_LONG getRMS(FITS_LONG *samples, int n);

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

