// -*-c++-*-
#ifndef _ByteImageData_h_
#define _ByteImageData_h_
/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: ByteImageData.h,v 1.8 1998/02/02 20:20:02 abrighto Exp $" 
 *
 * ByteImageData.h - class definitions for class ByteImageData
 *
 * See the man page RTI(3) for a complete description of this class
 * library.
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * Peter W. Draper 05/03/98  Added llookup
 *                 14/07/98  Added check for blanks in lookup.
 */

#include <sys/types.h>
#include "ImageData.h"



// This class is used for images where the raw data is made up of bytes

class ByteImageData : public ImageData {
private:
    // value of blank pixel, if known (if haveBlankPixel_ is nonzero)
    long blank_;

    // get value as unsigned short
    ushort convertToUshort(byte b) {
	return (ushort)b;
    }

    // return X image pixel value for raw image value
    byte lookup(byte b) {
        if ( !haveBlank_ ) return lookup_[(ushort)b];
        if ( b != blank_ ) return lookup_[(ushort)b];
        return lookup_[128];
    } 
    unsigned long llookup(byte b) {
        if ( !haveBlank_ ) return lookup_[(ushort)b];
        if ( b != blank_ ) return lookup_[(ushort)b];
        return lookup_[128];
    }
protected:
    // convert cut values to short range
    void initShortConversion();

public:
    // constructor
    ByteImageData(const char* name, const ImageIO& imio, int verbose)
	: ImageData(name, imio, verbose, 256 /* use a smaller lookup table */),
	  blank_(128) { }

    // return the data type of the raw data
    int dataType() {return BYTE_IMAGE;}

    // return true if the data type is signed
    int isSigned() {return 0;}

    // return a copy of this object
    ImageData* copy() {return new ByteImageData(*this);}

    // include declarations for methods that differ only in raw data type
#   include "ImageTemplates.h"
};


#endif _ByteImageData_h_
