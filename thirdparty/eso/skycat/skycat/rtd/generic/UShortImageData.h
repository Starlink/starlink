// -*-c++-*-
#ifndef _UShortImageData_h_
#define _UShortImageData_h_
/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: UShortImageData.h,v 1.10 1999/03/19 20:10:01 abrighto Exp $" 
 *
 * UShortImageData.h - class definitions for class UShortImageData
 *
 * See the man page RTI(3) for a complete description of this class
 * library.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * Peter W. Draper 04/03/98  Added llookup.
 *                 14/07/98  Added check for blanks in lookup.
*/

#include "ImageData.h"


// This class is used for images where the raw data is made up of 
// unsigned shorts

class UShortImageData : public ImageData {
private:
    // value of blank pixel, if known (if haveBlankPixel_ is nonzero)
    ushort blank_;

    // get value as unsigned short (dummy method)
    ushort convertToUshort(ushort s) {
	return s;
    }

    // return X image pixel value for raw image value
    byte lookup(ushort s) {
        if ( !haveBlank_ ) return lookup_[s];
        if ( s != blank_ ) return lookup_[s];
        return lookup_[(ushort)LOOKUP_BLANK];
    }
    unsigned long llookup(ushort s) {
        if ( !haveBlank_ ) return lookup_[s];
        if ( s != blank_ ) return lookup_[s];
        return lookup_[(ushort)LOOKUP_BLANK];
    }

protected:

    // initialize conversion from base type to short,
    void initShortConversion();

public:
    // constructor
    UShortImageData(const char* name, const ImageIO& imio, int verbose)
	: ImageData(name, imio, verbose), blank_(0) {}

    // return the data type of the raw data
    int dataType() {return USHORT_IMAGE;}

    // return true if the data type is signed
    int isSigned() {return 0;}

    // return a copy of this object
    ImageData* copy() {return new UShortImageData(*this);}

    // include declarations for methods that differ only in raw data type
#   include "ImageTemplates.h"
};


#endif _UShortImageData_h_
