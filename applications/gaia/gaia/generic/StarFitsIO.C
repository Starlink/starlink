/*
 * E.S.O. - VLT project / ESO Archive
 *
 * "@(#) $Id$"
 *
 * StarFitsIO.C - method definitions for class StarFitsIO, for operating on
 *                Fits files. This class redefines class FitsIO to keep the
 *                image data byte swapped, if needed, for the Starlink image
 *                processing routines.
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  24/03/95  Created
 * Peter W. Draper 11/01/00  Rewrite, only purpose now is to
 *                           initialise WCS using StarWCS object.
 * Peter W. Draper 25/01/00  Now merges primary FITS headers with
 *                           extension headers, if reading an extension.
 */
static const char* const rcsId="@(#) $Id$";

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
 * create and return a temporary file with a copy of stdin.
 * The argument is a char array large enough to hold the filename.
 */
static char* getFromStdin(char* filename)
{
    sprintf(filename, "/tmp/fits%d", getpid());
    FILE* f = fopen(filename, "w");
    if (!f) {
        sys_error("could not create temp file: ", filename);
        return NULL;
    }
    char buf[1024];
    size_t n;
    while((n = fread(buf, 1, sizeof(buf), stdin)) > 0) {
        if (fwrite(buf, 1, n, f) != n) {
            sys_error("error writing temp file: ", filename);
            return NULL;
        }
    }
    fclose(f);
    return filename;
}


/*
 * constructor
 */
StarFitsIO::StarFitsIO(int width, int height, int bitpix, double bzero,
                       double bscale, const Mem& header,
                       const Mem& data, fitsfile* fitsio)
  : FitsIO(width, height, bitpix, bzero, bscale, header, data, fitsio)
{}

/*
 * Read a FITS file and return an initialized StarFitsIO object for it,
 * or NULL if there are errors.
 *
 * If filename is "-", stdin is read into a temp image file and used
 *  as the input.
 *
 * The Mem class is used to speed up loading the file. The optional
 * mem_options control whether the memory is mapped read-only or
 * read/write (see class Mem).
 *
 *  Note this is copy of FitsIO::read member so that we can return a
 *  StarFitsIO object, rather than a FitsIO one.
 */
StarFitsIO* StarFitsIO::read(const char* filename, int mem_options)
{
    char  tmpfile[1024];
    int istemp = 0;

    tmpfile[0] = '\0';
    if (strcmp(filename, "-") == 0) { // use stdin
        // we have to use seek later, so copy to a temp file first
        filename =  getFromStdin(tmpfile);
        if (filename == NULL)
            return NULL;
        istemp++;
    }

    // check the file extension for recognized compression types
    filename = check_compress(filename, tmpfile, sizeof(tmpfile), istemp, 1, 0);
    if (filename == NULL) {
        if (istemp)
            unlink(tmpfile);
        return NULL;
    }

    // map image file to memory to speed up image loading
    if (mem_options == 0 && access(filename, W_OK) == 0)
        mem_options = Mem::FILE_RDWR;

    Mem header(filename, mem_options, 0);
    if (header.status() != 0)
        return NULL;

    if (istemp)
        unlink(filename);       // will be deleted by the OS later

    return initialize(header);
}

/*
 * This static method returns an allocated FitsIO object given a Mem object
 * containing the data for the file. (header points to the data for the entire
 * file...).
 *
 *  Rewrite from FitsIO to return StarFitsIO.
 */
StarFitsIO* StarFitsIO::initialize(Mem& header)
{
    fitsfile* fitsio = openFitsMem(header);
    if (!fitsio)
      return NULL;

    long headStart = 0, dataStart = 0, dataEnd = 0;
    int status = 0;
    if (fits_get_hduaddr(fitsio, &headStart, &dataStart, &dataEnd,
                         &status) != 0) {
        cfitsio_error();
        return NULL;
    }

    if (header.length() < (dataEnd - headStart)) {
        const char* filename = header.filename();
        if (filename)
            log_message("FITS file has the wrong size (too short): %s",
                        filename);
        else
            log_message("FITS data has the wrong size (too short)");
    }

    // The data part is the same mmap area as the header, with an offset
    Mem data(header);
    header.length(dataStart - headStart);  // set usable length of header
    data.offset(dataStart);    // set offset for data
    data.length(dataEnd-dataStart);

    return initialize(header, data, fitsio);
}
/*
 *  This static method returns an allocated FitsIO object, given the cfitsio
 *  handle.
 *
 *  Rewrite from FitsIO to return StarFitsIO.
 */
StarFitsIO* StarFitsIO::initialize(Mem& header, Mem& data, fitsfile* fitsio)
{
    int bitpix = 0, width = 0, height = 0;
    double bzero = 0.0, bscale = 1.0;
    get(fitsio, "NAXIS1", width);
    get(fitsio, "NAXIS2", height);
    get(fitsio, "BITPIX", bitpix);
    get(fitsio, "BSCALE", bscale);
    get(fitsio, "BZERO", bzero);

    return new StarFitsIO( width, height, bitpix, bzero, bscale, header,
                           data, fitsio);
}

/*
 *  Initialize world coordinates (based on the image header)
 */
int StarFitsIO::wcsinit()
{
    //   If there are multiple HDUs, merge the primary header with
    //   the extension header to get all of the WCS info.
    if ( getNumHDUs() > 1 ) {
        int length = header_.length() + primaryHeader_.length();
        mergedHeader_ = Mem( length + 1, 0 );
        if ( mergedHeader_.status() == 0 ) {
            strncpy( (char *)mergedHeader_.ptr(),
                     (char *)header_.ptr(),
                     header_.length() );
            strncpy( (char *)mergedHeader_.ptr() + header_.length(),
                     (char *)primaryHeader_.ptr(),
                     primaryHeader_.length() );

            ( (char *)mergedHeader_.ptr() )[length] = '\0';

            wcs_ = WCS(new StarWCS( (const char *)mergedHeader_.ptr() ) );
            return wcs_.status();
        }
    }
    wcs_ = WCS(new StarWCS( (const char *)header_.ptr() ) );
    return wcs_.status();
}

