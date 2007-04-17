// -*-c++-*-
/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: ShortImageData.h,v 1.1.1.1 2006/01/12 16:38:30 abrighto Exp $" 
 *
 * ShortImageData.h - class definitions for class ShortImageData
 *
 * See the man page ImageData(3) for a complete description of this class
 * library.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * T. Herlin       08/12/95  Added color scale functions to avoid sign problem
 * Allan Brighton  14/12/95  reversed above and fixed the real problem 
 * Peter W. Draper 04/03/98  Added llookup
 *                 14/07/98  Added blank checks in lookup.
 * P.Biereichel    22/03/99  Added definitions for bias subtraction
 */

#include <sys/types.h>
#include "ImageData.h"



// This class is used for images where the raw data is made up of shorts

class ShortImageData : public ImageData {
private:
    // value of blank pixel, if known (if haveBlankPixel_ is nonzero)
    short blank_;
    
    // get value as unsigned short
    ushort convertToUshort(short s) {
	return (ushort)s;
    }

    // return X image pixel value for raw image value
    byte lookup(short s) {
	if ( !haveBlank_ ) return lookup_[(ushort)s];
	if ( s != blank_ ) return lookup_[(ushort)s];
	return lookup_[(ushort)LOOKUP_BLANK];
    }
    unsigned long llookup(short s) {
	if ( !haveBlank_ ) return lookup_[(ushort)s];
	if ( s != blank_ ) return lookup_[(ushort)s];
	return lookup_[(ushort)LOOKUP_BLANK];
    }

    // return NTOH converted value evtl. subtracted with corresponding bias value
    short getVal(short* p, int idx);

    int getXsamples(short *rawImage, int idx, int wbox, short *samples);
    int getBsamples(short *rawImage, int idx, int wbox, short *samples);
    int getCsamples(short *rawImage, int idx, int wbox, short *samples);
    short getMedian(short *samples, int n);
    short getBoxVal(short *rawImage, int idx, int wbox, short *samples, int xs);
    short getRMS(short *samples, int n);

protected:

    // initialize conversion from base type to short
    void initShortConversion();

public:
    // constructor
    ShortImageData(const char* name, const ImageIO& imio, int verbose)
	: ImageData(name, imio, verbose), blank_(0) {}

    // return class name as a string
    virtual const char* classname() { return "ShortImageData"; }

    // return the data type of the raw data
    int dataType() {return SHORT_IMAGE;}

    // return true if the data type is signed
    int isSigned() {return 1;}

    // return a copy of this object
    ImageData* copy() {return new ShortImageData(*this);}

    // include declarations for methods that differ only in raw data type
#   include "ImageTemplates.h"
};
