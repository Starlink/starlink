/*
 * E.S.O. - VLT project / ESO Archive
 *
 * "@(#) $Id: StarFitsIO.C,v 1.5 1998/12/02 23:53:34 abrighto Exp $" 
 *
 * StarFitsIO.C - method definitions for class StarFitsIO, for operating on
 *                Fits files. This class redefines class FitsIO to keep the
 *                image data byte swapped, if needed, for the Starlink image
 *                processing routines.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  24/03/95  Created
 */
static const char* const rcsId="@(#) $Id: StarFitsIO.C,v 1.5 1998/12/02 23:53:34 abrighto Exp $";

#include <netinet/in.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <time.h>
#include "util.h"
#include "error.h"
#include "Mem.h"
#include "StarWCS.h"
#include "StarFitsIO.h"


/*
 * constructor 
 */
StarFitsIO::StarFitsIO(int width, int height, int bitpix, double bzero, 
	       double bscale, const Mem& header, const Mem& data)
    : FitsIO(width, height, bitpix, bzero, bscale, header, data)
{
    // byte swap the data
    status_ = byteSwapData();
}


/*
 * Read a FITS file and return an initialized StarFitsIO object for it,
 * or NULL if there are errors.
 *
 * If filename is "-", stdin is read into a temp image file and used as
 * the input.
 *
 * The Mem class is used to speed up loading the file. The optional
 * mem_options control whether the memory is mapped read-only or
 * read/write (see class Mem).
 *
 * The data is automatically byte swapped (see constructor).
 */
StarFitsIO* StarFitsIO::read(const char* filename, int mem_options)
{
    // if the file is read-only and read-write access was requested, read
    // the file into memory rather than returning an error.
    int roflag = 0;
    if ((mem_options & Mem::FILE_RDWR) && access(filename, W_OK) != 0) {
	roflag++;
	mem_options = 0;
    }

    FitsIO* fits = FitsIO::read(filename, mem_options);
    if (!fits)
	return (StarFitsIO*)NULL;
    
    // check if we need to make a copy of the data (if the file was read-only)
    Mem header, data;
    if (roflag) {
	// make a memory copy of the header and data
	header = Mem(fits->header().length(), 0);
	data = Mem(fits->data().length(), 0);
	if (header.status() != 0 || data.status() != 0) 
	    return (StarFitsIO*)NULL;
	memcpy(header.ptr(), fits->header().ptr(), fits->header().length());
	memcpy(data.ptr(), fits->data().ptr(), fits->data().length());
    } 
    else {
	// quick reference counted copy
	header = fits->header();
	data = fits->data();
    }

    // use class StarFitsIO to handle byte swapping
    StarFitsIO* sfits = new StarFitsIO(fits->width(), fits->height(), fits->bitpix(), 
				       fits->bzero(), fits->bscale(), header, data);
    delete fits;
    if (sfits->status() != 0) {
	delete sfits;
	return (StarFitsIO*)NULL;
    }
    return sfits;
}


/* 
 * write a fits file from the data and header, if present
 */
int StarFitsIO::write(const char *filename) const
{
    FitsIO fits(width_, height_, bitpix_, bzero_, bscale_, header_, data_);
    if (fits.status() != 0)
	return 1;   // error

    // byte swap the data
    if (fits.byteSwapData() != 0)
	return 1;
    
    // write the file
    return fits.write(filename);
}


/*
 * initialize world coordinates (based on the image header)
 */
int StarFitsIO::wcsinit()
{
    wcs_ = WCS(new StarWCS((const char*)header_.ptr()));
    return wcs_.status();
}

