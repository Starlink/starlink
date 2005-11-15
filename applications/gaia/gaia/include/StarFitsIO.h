// -*-c++-*-
#ifndef _StarFitsIO_h_
#define _StarFitsIO_h_
/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id$" 
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
 * Peter W. Draper 10/01/00  Changed so that uses FITS byte ordering
 *                           (needed to allow HDU access).
 *                 14/08/00  Added getReadonly member so that assumed
 *                           access mode can be determined.
 *                 16/08/00  Added write and mergeHeader members.
 *                 16/02/04  Added alwaysMerge_ member.
 *                 13/06/05  Added setHDU member.
 */

#include "FitsIO.hxx"


/*
 * This class manages reading and writing FITS files and storing the
 * image data. It is derived from ImageIORep rather than class ImageIO
 * for the sake of reference counting.
 */
class StarFitsIO : public FitsIO {
public:   
    
    // Constructor 
    StarFitsIO(int width, int height, int bitpix, double bzero, 
               double bscale, const Mem& header, const Mem& data,
               fitsfile *fitsio);
    
    //  Destructor
    ~StarFitsIO();
    
    // Return the class name as a string
    const char* classname() const {return "StarFitsIO";}
    
    // Read a FITS file and return a pointer to an allocated StarFitsIO object
    // NULL if an error occurred
    static StarFitsIO* read(const char* filename, int memOptions = 0);
    
    // Initialize world coordinates (based on the image header)
    int wcsinit();
    
    // Return an allocated FitsIO object, given the Mem objects for the
    // header and data and the cfitsio handle to use to access the file.
    static StarFitsIO* initialize(Mem& header, Mem& data, fitsfile* fitsio);
    
    // Return an allocated FitsIO object, given the Mem object for the
    // file header (header.ptr() should point to the entire FITS file
    // contents.) 
    static StarFitsIO* initialize(Mem& header);
    
    // Return an allocated FitsIO object, given the Mem objects for the
    // header and data.
    static StarFitsIO* initialize(Mem& header, Mem& data);
    
    // Return if the data and header are mapped with readonly access.
    int getReadonly();

    // Merge the primary and extension FITS headers.
    void mergeHeader();

    // Write a FITS image to disk.
    int write( const char *filename );

    // Set whether to always merge headers or not.
    static void setAlwaysMerge( int value ) { alwaysMerge_ = value; }

    // Get whether to always merge headers or not.
    static int getAlwaysMerge() { return alwaysMerge_; }

    // Move to the specified HDU and make it the current one
    int setHDU( int num );

protected:
    // Whether to merge headers or not.
    static int alwaysMerge_;

    // Whether a header merge is needed, given the current settings.
    int mergeNeeded();

    // CNF registered pointers.
    void *cnfHeaderPtr_;
    void *cnfDataPtr_;

};

#endif /*_StarFitsIO_h_ */
