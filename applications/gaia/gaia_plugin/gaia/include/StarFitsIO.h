// -*-c++-*-
#ifndef _StarFitsIO_h_
#define _StarFitsIO_h_
/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: StarFitsIO.h,v 1.2 1998/05/18 21:22:39 abrighto Exp $" 
 *
 * StarFitsIO.h - declarations for class StarFitsIO, a class representing the
 *                contents of a FITS image file (or other image source). This
 *                class redefines class FitsIO so that the data in memory is
 *                already byte-swapped, if needed. This is required by the 
 *                starlink image processing routines.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 */

#include "FitsIO.h"


/*
 * This class manages reading and writing FITS files and storing the
 * image data. It is derived from ImageIORep rather than class ImageIO
 * for the sake of reference counting.
 */
class StarFitsIO : public FitsIO {
public:   
    // constructor 
    StarFitsIO(int width, int height, int bitpix, double bzero, 
	   double bscale, const Mem& header, const Mem& data);

    // destructor
    ~StarFitsIO() {}

    // return true if this class uses native byte ordering
    // (FITS uses network byte order, but we swap the bytes in this class first).
    int nativeByteOrder() const {return 1;}

    // return the class name as a string
    const char* classname() const {return "StarFitsIO";}

    // read a FITS file and return a pointer to an allocated StarFitsIO object
    // NULL if an error occurred
    static StarFitsIO* read(const char* filename, int memOptions = 0);

    // write the data to a FITS file 
    int write(const char *filename) const;

    // initialize world coordinates (based on the image header)
    int wcsinit();
};

#endif _StarFitsIO_h_
