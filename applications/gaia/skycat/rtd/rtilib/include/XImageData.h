// -*-c++-*-
#ifndef _XImageData_h_
#define _XImageData_h_
/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: XImageData.h,v 1.10 1999/03/19 20:10:02 abrighto Exp $" 
 *
 * XImageData.h - class definitions for class XImageData
 *
 * See the man page RTI(3) for a complete description of this class
 * library.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * Peter W. Draper 04/03/98  Added llookup.
 *                 14/07/98  Added blank pixel check for lookup.
 */

#include <sys/types.h>
#include "ImageData.h"



// This class is used for images where the raw data is made up of bytes

class XImageData : public ImageData {
private:
    // value of blank pixel, if known (if haveBlankPixel_ is nonzero)
    byte blank_;

    // get value as unsigned short
    ushort convertToUshort(byte b) {
	return (ushort)b;
    }


    // return X image pixel value for raw image value
    byte lookup(byte b) {
        if ( !haveBlank_ ) return b;
        if ( b != blank_ ) return b;
        return LOOKUP_BLANK;
    } 
    unsigned long llookup(byte b) {
        if ( !haveBlank_ ) return b;
        if ( b != blank_ ) return b;
        return LOOKUP_BLANK;
    }


protected:
    // no conversion necessary
    void initShortConversion() { 
	scaledLowCut_ = 0;
	scaledHighCut_ = 255;
        scaledBlankPixelValue_ = LOOKUP_BLANK;
    }

public:
    // constructor
    XImageData(const char* name, const ImageIO& imio, int verbose)
	: ImageData(name, imio, verbose), blank_(0) {
	    flipY_ = 1;
    }

    // return the data type of the raw data
    int dataType() {return X_IMAGE;}

    // return true if the data type is signed
    int isSigned() {return 0;}

    // return a copy of this object
    ImageData* copy() {return new XImageData(*this);}

    // set the color scale algorithm for the image (redefined from base class)
    void colorScale(int ncolors, unsigned long* colors) {}

    // include declarations for methods that differ only in raw data type
#   include "ImageTemplates.h"
};


#endif _XImageData_h_
