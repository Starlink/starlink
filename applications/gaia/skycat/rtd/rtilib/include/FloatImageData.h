// -*-c++-*-
#ifndef _FloatImageData_h_
#define _FloatImageData_h_
/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: FloatImageData.h,v 1.9 1998/07/23 23:37:55 abrighto Exp $" 
 *
 * FloatImageData.h - class definitions for class FloatImageData
 *
 * See the man page RTI(3) for a complete description of this class
 * library.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * Peter W. Draper 04/03/98  Added llookup
 */

#include <sys/types.h>
#include "ImageData.h"



// This class is used for images where the raw data is made up of ints

class FloatImageData : public ImageData {
private:
    // value of blank pixel, if known (if haveBlankPixel_ is nonzero)
    float blank_;

    double bias_;		// offset for conversion to short lookup index
    double scale_;		// factor for conversion to short lookup index

    // local methods used to get short index in lookup table
    short scaleToShort(float);

    // as above, but unsigned
    ushort convertToUshort(float f) {
	return ushort(scaleToShort(f));
    }

    // Return X image pixel value for raw image value.
    // Convert the given float image value to byte, scaling to short
    // first and then using the short value as an index in the color
    // lookup table.
    byte lookup(float f) {return lookup_[(ushort)scaleToShort(f)];}
    unsigned long llookup(float f) {return lookup_[(ushort)scaleToShort(f)];}


protected:
    // initialize conversion from base type to short,
    void initShortConversion();

    // sprintf format for (x y value)
    virtual char* getXYValueFmt() {return "%.1f %.1f %.2f";}

    // sprintf format for image pixel value
    virtual char* getValueFmt() {return "%.2f";}

public:
    // constructor
    FloatImageData(const char* name, const ImageIO& imio, int verbose)
	: ImageData(name, imio, verbose),
          blank_(0),
	  bias_(0.0),
          scale_(1.0) {}

    // return the data type of the raw data
    int dataType() {return FLOAT_IMAGE;}

    // return true if the data type is signed
    int isSigned() {return 1;}

    // return a copy of this object
    ImageData* copy() {return new FloatImageData(*this);}

    // include declarations for methods that differ only in raw data type
#   include "ImageTemplates.h"
};


#endif _FloatImageData_h_
