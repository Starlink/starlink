// -*-c++-*-
#ifndef _ByteImageData_h_
#define _ByteImageData_h_
/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: ByteImageData.h,v 1.9 1998/07/23 23:37:55 abrighto Exp $" 
 *
 * ByteImageData.h - class definitions for class ByteImageData
 *
 * See the man page RTI(3) for a complete description of this class
 * library.
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
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
    byte lookup(byte b) {return	lookup_[(ushort)b];} 

protected:
    // convert cut values to short range
    void initShortConversion();

public:
    // constructor
    ByteImageData(const char* name, const ImageIO& imio, int verbose)
	: ImageData(name, imio, verbose),
	  blank_(0) { }

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
