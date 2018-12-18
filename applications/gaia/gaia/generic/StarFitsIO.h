// -*-c++-*-
#ifndef _StarFitsIO_h_
#define _StarFitsIO_h_
/*
 * E.S.O. - VLT project

 * "@(#) $Id$"

 * StarFitsIO.h - declarations for class StarFitsIO, a class representing the
 *                contents of a FITS image file (or other image source). This
 *                class redefines class FitsIO so that the data in memory is
 *                already byte-swapped, if needed. This is required by the
 *                starlink image processing routines.

 *  Copyright:
 *     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

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
 *                 28/11/05  Added copy member.
 *                 01/02/11  Add -TAB support.
 */
extern "C" {
#include "ast.h"
}
#include "Fits_IO.h"

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

    // Return a copy of this object that shares the data, but can have a
    // different current HDU
    StarFitsIO* copy();

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

    // Move to the named HDU.
    int setHDUByName( const char *extname, int extver );

    // Does current HDU contain a compressed image?
    int isCompressedImage();

    // Save a compressed image from the current extension to a file.
    int saveCompressedImage( const char *filename, const char *object );

    // Handle -TAB WCS. Public but do not use. Only for AST internal calls.
    int loadTabTable( AstFitsChan *chan, const char *extname,
                      int extver, int extlevel, int *status );

    // Get a column of values from a table.
    int getTableColumn(int col, long *values, int numValues);
    int getTableColumn(int col, LONGLONG* values, int numValues);

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
