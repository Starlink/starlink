// -*-c++-*-
#ifndef _DoubleImageData_h_
#define _DoubleImageData_h_
/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id$" 
 *
 * DoubleImageData.h - class definitions for class DoubleImageData
 *
 * See the man page RTI(3) for a complete description of this class
 * library.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Peter W. Draper 30/05/01  Created
 */

#include <sys/types.h>
#include "ImageData.h"

// This class is used for images where the raw data is made up of ints

class DoubleImageData : public ImageData {
private:
    // value of blank pixel, if known (if haveBlankPixel_ is nonzero)
    double blank_;

    double bias_;		// offset for conversion to short lookup index
    double scale_;		// factor for conversion to short lookup index

    // local methods used to get short index in lookup table
    short scaleToShort( double );

    // as above, but unsigned
    ushort convertToUshort( double f ) {
	return ushort( scaleToShort( f ) );
    }

    // Return X image pixel value for raw image value.
    // Convert the given double image value to byte, scaling to short
    // first and then using the short value as an index in the color
    // lookup table.
    byte lookup( double f ) {
        return lookup_[(ushort)scaleToShort(f)];
    }
    unsigned long llookup( double f ) {
        return lookup_[(ushort)scaleToShort(f)];
    }


protected:
    // initialize conversion from base type to short,
    void initShortConversion();

    // sprintf format for (x y value)
    virtual char* getXYValueFmt() {return "%.1f %.1f %.2f";}

    // sprintf format for image pixel value
    virtual char* getValueFmt() {return "%.2f";}

public:
    // constructor
    DoubleImageData(const char* name, const ImageIO& imio, int verbose)
	: ImageData(name, imio, verbose),
          blank_(0),
	  bias_(0.0),
          scale_(1.0) {}

    // return the data type of the raw data
    int dataType() {return DOUBLE_IMAGE;}

    // return true if the data type is signed
    int isSigned() {return 1;}

    // return a copy of this object
    ImageData* copy() {return new DoubleImageData(*this);}

    // include declarations for methods that differ only in raw data type
#   include "ImageTemplates.h"
};


#endif _DoubleImageData_h_
