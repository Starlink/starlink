// -*-c++-*-
#ifndef _ShortImageData_h_
#define _ShortImageData_h_
/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: ShortImageData.h,v 1.9 1998/07/23 23:37:55 abrighto Exp $" 
 *
 * ShortImageData.h - class definitions for class ShortImageData
 *
 * See the man page RTI(3) for a complete description of this class
 * library.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * T. Herlin       08/12/95  Added color scale functions to avoid sign problem
 * Allan Brighton  14/12/95  reversed above and fixed the real problem 
 * Peter W. Draper 04/03/98  Added llookup
 *                 14/07/98  Added blank checks in lookup.
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


protected:

    // initialize conversion from base type to short,
    void initShortConversion();

public:
    // constructor
    ShortImageData(const char* name, const ImageIO& imio, int verbose)
	: ImageData(name, imio, verbose), blank_(0) {}

    // return the data type of the raw data
    int dataType() {return SHORT_IMAGE;}

    // return true if the data type is signed
    int isSigned() {return 1;}

    // return a copy of this object
    ImageData* copy() {return new ShortImageData(*this);}

    // include declarations for methods that differ only in raw data type
#   include "ImageTemplates.h"
};


#endif _ShortImageData_h_
