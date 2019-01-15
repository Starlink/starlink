/*
 * E.S.O. - VLT project / ESO Archive
 *
 * "@(#) $Id$"
 *
 * StarFitsIO.C - method definitions for class StarFitsIO, for operating on
 *                Fits files. This class redefines class FitsIO to use
 *                the Starlink AST library. It also forces the use of
 *                readonly access, if that is all that is available,
 *                correctly saves a set of merged headers for
 *                extension images and supports the extraction of
 *                compressed extension images to a file.
 *
 *                Also handles the "-TAB" WCS format.
 *
 *  Copyright:
 *     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     Copyright (C) 2007-2011 Science and Technology Facilities Council.
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
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  24/03/95  Created
 * Peter W. Draper 11/01/00  Rewrite, only purpose now is to
 *                           initialise WCS using StarWCS object.
 *                 25/01/00  Now merges primary FITS headers with
 *                           extension headers, if reading an extension.
 *                 14/08/00  Changed readonly access behaviour. This
 *                           is now used when only mechanism possible,
 *                           rather then throwing an error. The
 *                           readonly status should be queried after
 *                           opening. Now also saves the merged
 *                           headers with any extension images.
 *                 16/08/00  Changed extension header merging to be more
 *                           correct. Added write member to added a
 *                           merged set of headers to any new files.
 *                 16/02/04  Changed merging of headers to be optional and
 *                           also to only be done if the primary HDU contains
 *                           a dummy image.
 *                 13/06/05  Added setHDU member. Overrides the FitsIO version
 *                           (which is now virtual), so we can register
 *                           the pointer with CNF.
 *                 23/05/07  Check for INHERIT keyword when testing if headers
 *                           should be merged. That's the standard way
 *                           (nearly).
 *                 24/05/07  Add methods for extracting compressed extension
 *                           images.
 *                 01/03/11  Add support for the -TAB format. Stores look up
 *                           tables to map coordinates to pixel indices in
 *                           a further extension of an MEF.
 *                 31/05/11  Add Pan-STARRS log scaled image hack.
 *                 26/07/11  Add Pan-STARRS asinh scaled image hack.
 */
static const char* const rcsId="@(#) $Id$";

#if HAVE_CONFIG_H
#include "config.h"
#endif

#if ! ( defined(__APPLE__) && defined(__MACH__) )
#include <netinet/in.h>
#endif

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
extern "C" {
#include "cnf.h"
}
#include "gaiaUtils.h"
#include "prm_par.h"

// Initialize static members.
int StarFitsIO::alwaysMerge_ = 0;

enum {FITSBLOCK=2880};
enum {FITSCARD=80};

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

/**
 *  Handle a request from AST (which uses C binding) to locate and load a -TAB
 *  table. Note the given AstFitsChan must have the "this" pointer of the
 *  StarFitsIO object stored so we can call the real member that has access to
 *  the FITS file handle.
 */
static void staticLoadTabTable( AstFitsChan *chan, const char *extname,
                                int extver, int extlevel, int *status )
{
    //  Get the instance and call the read member.
    StarFitsIO *instance = (StarFitsIO *) astChannelData;
    *status = 0;
    instance->loadTabTable( chan, extname, extver, extlevel, status );

    // Local status is 0 for success, AST wants that to be 1.
    if ( *status == 0 ) {
        *status = 1;
    }
}

/*
 * Constructor
 */
StarFitsIO::StarFitsIO( int width, int height, int bitpix, double bzero,
                        double bscale, const Mem& header,
                        const Mem& data, fitsfile* fitsio )
    : FitsIO( width, height, bitpix, bzero, bscale, header, data, fitsio ),
      cnfHeaderPtr_(NULL),
      cnfDataPtr_(NULL)
{
    cnfHeaderPtr_ = header.ptr();
    cnfDataPtr_ = data.ptr();
}

/*
 * Destructor
 */
StarFitsIO::~StarFitsIO()
{
    //  Deregister CNF pointers. Probably should but there may be multiple
    //  registrators. CNF needs reference counting.
    //if ( cnfHeaderPtr_ != NULL ) {
    //    cnfUregp( cnfHeaderPtr_ );
    //}
    //if ( cnfDataPtr_ != NULL ) {
    //    cnfUregp( cnfDataPtr_ );
    //}
}


/*
 * Return a copy of this object that shares the data, but can have a different
 * current HDU . Override FitsIO version so we create StarFitsIO objects.
 */
StarFitsIO* StarFitsIO::copy()
{
    int status = 0;
    fitsfile* newFitsio;
    fits_reopen_file( fitsio_, &newFitsio, &status );
    if ( status != 0 ) {
        return NULL;
    }
    return new StarFitsIO( width_, height_, bitpix_, bzero_, bscale_,
                           header_, data_, newFitsio );
}


/*
 *  Read a FITS file and return an initialized StarFitsIO object for it,
 *  or NULL if there are errors.
 *
 *  If filename is "-", stdin is read into a temp image file and used
 *  as the input.
 *
 *  The Mem class is used to speed up loading the file. The optional
 *  mem_options control whether the memory is mapped read-only or
 *  read/write (see class Mem).
 *
 *  Note this is copy of FitsIO::read member so that we can return a
 *  StarFitsIO object, rather than a FitsIO one, the only
 *  modifications are related to using a copy of the file when
 *  readonly access is just available and handling pointer registration
 *  with CNF for passing into Fortran.
 */
StarFitsIO* StarFitsIO::read( const char* filename, int mem_options )
{
    char tmpfile[1024];
    int istemp = 0;

    tmpfile[0] = '\0';
    if ( strcmp( filename, "-" ) == 0 ) { // use stdin

        // we have to use seek later, so copy to a temp file first
        filename = getFromStdin( tmpfile );
        if ( filename == NULL ) {
            return NULL;
        }
        istemp++;
    }

    //  Check the file extension for recognized compression types.
    filename = check_compress( filename, tmpfile, sizeof(tmpfile),
                               istemp, 1, 0 );
    if ( filename == NULL ) {
        if ( istemp )
            unlink( tmpfile );
        return NULL;
    }

    //  If the file is read-only and read-write access was requested, then
    //  just use readonly.
    if ( mem_options && Mem::FILE_RDWR && access( filename, W_OK ) != 0) {
        mem_options = 0;
    }
    else if ( mem_options == 0 && access( filename, W_OK ) == 0 ) {

        //  FitsIO behaviour. Map image file to memory to speed up image
        //  loading
        mem_options = Mem::FILE_RDWR;
    }

    //  Map the file into memory. When mapped we need to register the header
    //  pointer with CNF so that it can be safely passed into any Fortran
    //  routines. If this pointer cannot be registered (the CNF mapping
    //  function thinks this is already registered) then we need to ask for
    //  another pointer to this file...

    char *where = NULL;
    int reg = 0;
    int pagesize = 10 * (int) sysconf( _SC_PAGESIZE );
    Mem *header = NULL;
    F77_POINTER_TYPE fp;
    void *cp;
    int count = 0;
    while ( count < 1000 )
    {
        count++;
        header = new Mem( filename, mem_options, 0, (void *)where );
        if ( header->status() != 0 ) {
            return NULL;
        }
        reg = cnfRegp( header->ptr() );

        if ( reg == 0 ) {
            //  Cannot register the pointer, perhaps it's already known from
            //  another shared instance. Check this before panicking.
            fp = cnfFptr( header->ptr() );
            cp = cnfCptr( fp );
            if ( cp != header->ptr() ) {
                //  Definitely cannot register pointer, unmap the file and try
                //  a different address, offset from current position by 10
                //  times the pagesize.
                if ( where == NULL ) where = (char *)header->ptr();
                where += pagesize;
                delete header;
                header = NULL;
            }
            else {
                //  Pointer is known as right value.
                break;
            }
        }
        else if ( reg == -1 ) {
            //  Cannot register this because of a failing memory condition.
            if ( header != NULL ) delete header;
        }
        else {
            break;
        }
    }

    if ( istemp ) {
        unlink( filename );       // will be deleted by the OS later
    }
    StarFitsIO *ref = initialize( *header );
    delete header;
    return ref;
}

/*
 * This static method returns an allocated FitsIO object given a Mem object
 * containing the data for the file. (header points to the data for the entire
 * file...).
 *
 *  Rewrite from FitsIO to return StarFitsIO.
 */
StarFitsIO* StarFitsIO::initialize( Mem& header )
{
    fitsfile* fitsio = openFitsMem( header );
    if ( !fitsio )
        return NULL;

    long headStart = 0, dataStart = 0, dataEnd = 0;
    int status = 0;
    if ( fits_get_hduaddr( fitsio, &headStart, &dataStart, &dataEnd,
                           &status ) != 0 ) {
        cfitsio_error();
        return NULL;
    }

    if ( header.length() < (dataEnd - headStart) ) {
        const char* filename = header.filename();
        if ( filename )
            log_message( "FITS file has the wrong size (too short): %s",
                         filename );
        else
            log_message( "FITS data has the wrong size (too short)" );
    }

    // The data part is the same mmap area as the header, with an offset
    Mem data( header );
    header.length( dataStart - headStart );  // set usable length of header
    data.offset( dataStart );                // set offset for data
    data.length( dataEnd - dataStart );

    //  Register this part with CNF. Cannot deal with failure since that would
    //  mean modifying the data starting point.
    cnfRegp( data.ptr() );

    return initialize( header, data, fitsio );
}

/*
 *  This static method returns an allocated FitsIO object, given the cfitsio
 *  handle.
 *
 *  Rewrite from FitsIO to return StarFitsIO.
 */
StarFitsIO* StarFitsIO::initialize( Mem& header, Mem& data, fitsfile* fitsio )
{
    int bitpix = 0, naxis = 0, width = 0, height = 0;
    double bzero = 0.0, bscale = 1.0;
    get( fitsio, "NAXIS", naxis );
    if ( naxis > 0 ) {
        //  Yes this was seen.
        get( fitsio, "NAXIS1", width );
        get( fitsio, "NAXIS2", height );
    }
    get( fitsio, "BITPIX", bitpix );
    get( fitsio, "BSCALE", bscale );
    get( fitsio, "BZERO", bzero );

    return new StarFitsIO( width, height, bitpix, bzero, bscale, header,
                           data, fitsio );
}

/*
 *  Return if the data is mapped readonly (this may happen even if
 *  readwrite access was initially specified).
 */
int StarFitsIO::getReadonly() {
    if ( header_.options() && Mem::FILE_RDWR ) {
        return 0;
    }
    else {
        return 1;
    }
}

/*
 *  Initialize world coordinates (based on the image header).
 */
int StarFitsIO::wcsinit()
{
    //   If there are multiple HDUs, merge the primary header with
    //   the extension header to get all of the WCS info. Pass in a function
    //   that should be called to access any extensions that contain -TAB
    //   tables.
    if ( mergeNeeded() ) {
        mergeHeader();
        wcs_ = WCS( new StarWCS( (const char *)mergedHeader_.ptr(),
                                 mergedHeader_.size(),
                                 (void *) this,
                                 &staticLoadTabTable ) );
        return wcs_.status();
    }
    wcs_ = WCS( new StarWCS( (const char *)header_.ptr(), header_.length(),
                             (void *) this, &staticLoadTabTable ) );
    return wcs_.status();
}

/**
 *  Get an AST FITS table from named extension in the current MEF.
 *  This supports the -TAB coordinate system type.
 */
int StarFitsIO::loadTabTable( AstFitsChan *chan, const char *extname,
                              int extver, int extlevel, int *status )
{
    if ( ! astOK || *status != 0 ) return 0;

    AstFitsTable *table = NULL;
    char card[81];
    char cname[81];
    char name[81];
    char *tform;
    int anyf;
    int naxis2;

    //  Current HDU so we restore before exit.
    int currentHdu = getHDUNum();

    //  Find the table.
    *status = setHDUByName( extname, extver );
    if ( *status != 0 ) {
        return 0;
    }

    //  Start an AST context.
    astBegin;

    //  Create a FitsChan to hold the headers in the extension HDU.
    AstFitsChan *fitschan = astFitsChan( NULL, NULL, " " );

    //  Extract the headers from the HDU and store them.
    int ncard = header_.size() / FITSCARD;
    gaiaUtilsGtFitsChan( (char *) header_.ptr(), ncard, &fitschan );

    //  Since the FITSIO "FTGCF<x>" routine applies any required
    //  scaling (specified by the column's TSCALn and TZEROn header
    //  values) to the returned column values, the values stored in the
    //  FitsTable will be unscaled, and so no TSCALn and TZEROn headers
    //  should be stored in the FitsTable. Delete such headers now.
    astClear( fitschan, "Card" );
    while ( astFindFits( fitschan, "TSCAL%d", card, 0 ) ) {
        astDelFits( fitschan );
    }
    astClear( fitschan, "Card" );
    while ( astFindFits( fitschan, "TZERO%d", card, 0 ) ) {
        astDelFits( fitschan );
    }

    //  Get the number of rows in the table.
    astGetFitsI( fitschan, "NAXIS2", &naxis2 );

    //  Create a FitsTable, creating columns equivalent to those in the
    //  extension header.
    table = astFitsTable( fitschan, " " );

    //  Copy the data for each column from the binary table into the
    //  FitsTable. Loop over all columns.
    int ncol = astGetI(  table, "NColumn" );
    for ( int icol = 1; icol <= ncol; icol++ ) {

        //  Get the column data type.
        const char *sname = astColumnName( table, icol );
        sprintf( cname, "ColumnType(%s)", sname );
        int ctype = astGetI( table, cname );

        //  Get the total number of elements to be read. If a column holds
        //  vector values, then each cell in the column will hold more than
        //  one element.
        sprintf( name, "ColumnLength(%s)", sname );
        int nel = astGetI( table, name );
        int totnel = naxis2 * nel;

        //  Do each data type in turn. First four-byte integer.
        if ( ctype == AST__INTTYPE ) {

            //  Allocate memory to store the column values.
            size_t size = sizeof( int ) * totnel;
            int *ip = new int[totnel];

            //  Read the FITS file to get all the data values in the
            //  column. The FitsTable will have inherited the null value from
            //  the FITS header, and so we do not need to replace null values
            //  in the following call.
            fits_read_col_int( fitsio_, icol, 1, 1, totnel, 0, ip, &anyf,
                               status );

            //  Store the column values in the FitsTable.
            astPutColumnData( table, sname, 0, size, ip );
            delete[] ip;
        }
        //  Do the same for two-byte integer.
        else if ( ctype == AST__SINTTYPE ) {
            size_t size = sizeof( short int ) * totnel;
            short int *ip = new short int[totnel];
            fits_read_col_sht( fitsio_, icol, 1, 1, totnel, 0, ip, &anyf,
                               status );
            astPutColumnData( table, sname, 0, size, ip );
            delete[] ip;
        }
        //  Do the same for one-byte unsigned integer.
        else if ( ctype == AST__BYTETYPE ) {
            size_t size = sizeof( unsigned char ) * totnel;
            unsigned char *ip = new unsigned char[totnel];
            fits_read_col_byt( fitsio_, icol, 1, 1, totnel, 0, ip, &anyf,
                               status );
            astPutColumnData( table, sname, 0, size, ip );
            delete[] ip;
        }
        //  Do the same for double-precision floats. FITS does not allow a bad
        //  value to be specified for floating-point values in binary tables,
        //  so replace NaNs by VAL__BADD in the following call.
        else if ( ctype == AST__DOUBLETYPE ) {
            size_t size = sizeof( double ) * totnel;
            double *ip = new double[totnel];
            fits_read_col_dbl( fitsio_, icol, 1, 1, totnel, VAL__BADD,
                               ip, &anyf, status );
            astPutColumnData( table, sname, 0, size, ip );
            delete[] ip;
        }
        //  Do the same for single-precision floats.
        else if ( ctype == AST__FLOATTYPE ) {
            size_t size = sizeof( float ) * totnel;
            float *ip = new float[totnel];
            fits_read_col_flt( fitsio_, icol, 1, 1, totnel, VAL__BADR,
                               ip, &anyf, status );
            astPutColumnData( table, sname, 0, size, ip );
            delete[] ip;
        }
        //  Things are a bit different for string types.
        else if ( ctype == AST__STRINGTYPE ) {

            //  The strings in the FITS binary table are fixed length, but the
            //  FitsTable class holds null-terminated variable-length
            //  strings. Get the length of each fixed-length string in the
            //  binary table. This is the repeat count in the TFORMn keyword
            //  value, divided by the number of elements in each cell.
            sprintf( name, "TFORM(%d)", icol );
            int clen = 0;
            if ( astGetFitsS( fitschan, name, &tform ) == 0 ) {
                if ( astOK ) {
                    const char *a = strchr( tform, 'A' );
                    int repeat = 0;
                    if ( tform[0] == 'A' ) {
                        repeat = 1;
                    }
                    else if ( a != NULL ) {
                        //  Convert to int, need to trap error.
                        int n = sscanf( tform, "%d", &repeat );
                        if ( n == 0 ) {
                            repeat = 0;
                        }
                    }
                    else {
                        repeat = 0;
                    }
                    clen = repeat / nel;

                    if ( clen <= 0 ) {
                        // XXX error cannot determine the length of
                        // fixed-length strings in column
                    }
                }

                //  Allocate memory to store the column values.
                size_t size = sizeof( char ) * clen * totnel;
                char *ip = new char[totnel];

                //  Read the FITS file.
                char cnull[2] = {" "};
                fits_read_col_str( fitsio_, icol, 1, 1, totnel, cnull, &ip,
                                   &anyf, status );
                astPutColumnData( table, sname, clen, size, ip );
                delete[] ip;
            }
        }
    }

    //  Reinstate the original current HDU in the FITS file.
    setHDU( currentHdu );

    //  If all went well import the table into the FitsChan.
    if (  *status == 0 && astOK ) {
        astPutTable( chan, table, extname );
    }
    else {
        if ( *status != 0 ) {
            astEnd;
            return cfitsio_error();
        }
    }
    if ( ! astOK ) {
        astClearStatus;
        astEnd;
        return error( "failed to load WCS table" );
    }
    astEnd;
    return 0;
}

/**
 * Determine if we need to merge the header with those from the primary HDU or
 * not. If the alwaysMerge_ member is set true, then we always merge,
 * otherwise we only merge if the primary HDU contains a dummy image or if the
 * INHERIT keyword is set.
 */
int StarFitsIO::mergeNeeded()
{
    // Don't need to do anything for the primary HDU
    if ( getNumHDUs() == 1 || getHDUNum() == 1 ) {
        return 0;
    }

    int result = alwaysMerge_;
    if ( !result ) {
        //  Check for INHERIT, if that "T" then we inherit.
        const char *inherit = get( "INHERIT" );
        if ( inherit && inherit[0] == 'T' ) {
            result = 1;
        }
        else {
            //  Need to check the primary image size... Don't want to modify
            //  the current HDU so need to do this the hard way by scanning
            //  the copy of the headers.
            int ncards = primaryHeader_.length() / FITSCARD;
            char *ptr = (char *) primaryHeader_.ptr();
            char *subptr = NULL;
            int naxis = 0;
            for ( int i = 0 ; i < ncards; i++, ptr += FITSCARD ) {
                if ( strncmp( ptr, "NAXIS ", 6 ) == 0 ) {
                    subptr = strstr( ptr, "=" );
                    if ( subptr != NULL ) {
                        sscanf( ++subptr, "%d", &naxis );
                    }
                    break;
                }
            }
            if ( naxis == 0 ) {
                result = 1;
            }
        }
    }
    return result;
}

/*
 *  Merge the current primary and extension header into a single
 *  header. The merge is done by taking the extension header and
 *  adding the any primary items that are not already present.
 *  All 'COMMENT', 'HISTORY' or ' ' (blank) cards are retained.
 */
void StarFitsIO::mergeHeader()
{
    //  Allocate space for merged header. Note this is maximum as some
    //  cards could be duplicated. Also use malloc so we can give
    //  control of memory to a Mem object when completed.
    int plength = primaryHeader_.length();
    int elength = header_.length();
    int maxlen = elength + plength + 1;
    char *newheader = (char *) malloc( (size_t) maxlen );

    //  Copy the extension header into place. Replacing the first card
    //  which should be XTENSION='IMAGE', with SIMPLE = T.
    sprintf( newheader, "%-80s",
             "SIMPLE  =                    T / Fits standard" );
    strncpy( newheader + FITSCARD, (char *)header_.ptr() + FITSCARD,
             elength - FITSCARD );

    //  Get number of cards in each of the headers.
    int fixedcards = elength / FITSCARD;
    int extracards = plength / FITSCARD;

    //  Locate and skip the END card (this is where we start adding
    //  any new cards).
    char *endPtr = (char *) newheader;
    int newlength = 0;
    int i = 0;
    for ( i = 0 ; i < fixedcards; i++, endPtr += FITSCARD,
                                       newlength += FITSCARD ) {
        if ( strncmp( endPtr, "END     ", 8 ) == 0 ) {
            break;
        }
    }
    fixedcards = newlength / FITSCARD;

    //  Loop over all primary headers, adding any new or special
    //  cards to the end of the new headers.
    char *extraPtr = (char *) primaryHeader_.ptr();
    int skip = 0;
    for ( i = 0 ; i < extracards; i++, extraPtr += FITSCARD ) {

        //  Special cards are always added to the end. Note END card
        //  is always copied from primary headers.
        if ( strncmp( extraPtr, "END     ", 8 ) == 0 ) {
            skip = 0;
            i = extracards;  // Time to stop
        }
        else if ( strncmp( extraPtr, "COMMENT ", 8 ) == 0 ||
                    strncmp( extraPtr, "HISTORY ", 8 ) == 0 ||
                    strncmp( extraPtr, "        ", 8 ) == 0 ) {
            skip = 0;
        }
        else {

            //  Need to check for an existing keyword and skip
            //  position if found.
            skip = 0;
            char *mainPtr = (char *) newheader;
            for ( int j = 0; j < fixedcards; j++, mainPtr += FITSCARD ) {
                if ( strncmp( extraPtr, mainPtr, 8 ) == 0 ) {
                    skip = 1;
                    break;
                }
            }
        }
        if ( ! skip ) {

            //  Keyword not seen or special, so append this.
            strncpy( endPtr, extraPtr, FITSCARD );
            endPtr += FITSCARD;
            newlength += FITSCARD;
        }
    }

    //  Truncate memory to that actually used.
    char *finalheader = (char *) realloc( (void *)newheader, newlength );

    //  Now create the merged header Mem object.
    mergedHeader_ = Mem( (void *) finalheader, newlength, 1 );
}

/*
 *  Write a FITS file using the image data and headers.
 *
 *  Override to correctly output merged headers when saving a
 *  extension image.
 */
int StarFitsIO::write( const char *filename )
{
    char tmpfilename[1024];
    int istemp = 1;

    if (fitsio_) {
        // flush any changes done in a memory FITS file
        int status = 0;
        if (fits_flush_file(fitsio_, &status) != 0)
            return cfitsio_error();
    }

    // if the file exists, rename it to make a backup and to avoid
    // crashing if we have the file mapped already
    if ( access( filename, F_OK ) == 0 ) {
        char backup[1024];
        sprintf( backup, "%s.BAK", filename );
        if ( rename( filename, backup ) != 0 ) {
            return sys_error( "cannot create backup file for ", filename );
        }
    }

    FILE *f;
    f = fopen(filename,"w");
    if (f == NULL)
        return error("can't create FITS file: ", filename);

    // if we have a FITS header, use it, otherwise create one from what we know
    // and add some "blank cards" at the end for application use
    int header_length = header_.length();
    if ( header_length > 0 ) {
       char *nextrec = (char *)header_.ptr();
       if ( mergeNeeded() ) {

           //  Need to save the merged version of the headers.  Always
           //  re-merge at this point so that changes to the primary header
           //  are seen on output (this may contain a new WCS).
           mergeHeader();
           header_length = mergedHeader_.length();
           nextrec = (char *)mergedHeader_.ptr();
       }
       fwrite((char *)nextrec, 1, header_length, f);
       padFile(f, header_length);
    }
    else {
        // create a FITS header
        int size = FITSBLOCK/FITSCARD;  // number of keyword lines in FITS header, including END

        // output keywords
        put_keyword(f, "SIMPLE", 'T'); size--;
        int bitpix = bitpix_;
        if (bitpix == -16)
            bitpix = 16;
        put_keyword(f, "BITPIX", bitpix); size--;
        put_keyword(f, "NAXIS", 2); size--;
        put_keyword(f, "NAXIS1", width_); size--;
        put_keyword(f, "NAXIS2", height_); size--;
        if (bitpix_ == -16) {
            put_keyword(f, "BZERO", (double)32768.0); size--;
            put_keyword(f, "BSCALE", (double)1.0); size--;
        }
        put_keyword(f, "COMMENT", "Generated by FitsIO::write on:"); size--;

        // add a timestamp
        char buf2[25];
        time_t clock = time(0);
        strftime(buf2, sizeof(buf2), "%Y-%m-%dT%H:%M:%S", localtime(&clock));
        put_keyword(f, "DATE", buf2); size--;

        // leave some "blank cards" for later modification by other applications
        char buf[10];
        int i = 0;
        while (size > 1) {
            sprintf(buf, "BLANK%02d", ++i);
            put_keyword(f, buf, " "); size--;
        }

        fprintf(f, "%-80s", "END");

        // ... no need for padding, since we filled up the FITS block
    }

    // now write the image data
    int tsize = abs(bitpix_)/8;   // size of a pixel value
    switch(bitpix_) {
    case -8: // note: special non-fits format for a saved XImage
    case 8:
    case 16:
    case 32:
    case -32:
    case  64:
    case -64:
        fwrite((char*)data_.ptr(), tsize, width_*height_, f);
        break;
    case -16:
    {
        // unsigned short needs to be converted (conversion taken from Midas)
        unsigned short *pu = (unsigned short *)data_.ptr();
        int i = width_*height_;
        short *ps_new = new short[i];
        short *ps = ps_new;
        if (ps_new == 0) {
            fclose(f);
            return error("Not enough memory");
        }
        int nn;
        while (i--) {
            nn = (int)(*pu++) - 32768;
            *ps++ = (unsigned int) nn;
        }
        fwrite((char*)ps_new, tsize, width_*height_, f);
        delete ps_new;
    }
    break;
    default:
        fclose(f);
        return error("unsupported image type");
    }

    // round off file size
    padFile(f, width_*height_*tsize);

    fclose(f);

    // check the file extension for recognized compression types

    const char *tmpfile = check_compress(filename, tmpfilename, sizeof(tmpfilename),
                                         istemp, 0, bitpix_);
    if (tmpfile == NULL)
        return ERROR;

    if (strcmp(tmpfile, filename) != 0) {
        if (rename(tmpfile, filename) != 0)
            return sys_error("cannot rename to file ", filename);
    }

    return OK;
}

/*
 * Move to the specified HDU and make it the current one
 */
int StarFitsIO::setHDU( int num )
{
    int result = FitsIO::setHDU( num );

    //  Register new pointers.
    cnfRegp( header_.ptr() );
    cnfRegp( data_.ptr() );

    return result;
}

/**
 *  Move to the named HDU and make it the current one.
 */
int StarFitsIO::setHDUByName( const char *extname, int extver )
{
    int status = 0;

    //  Try to move.
    if ( fits_movnam_hdu( fitsio_, BINARY_TBL, (char *) extname, extver,
                          &status ) != 0 ) {
        return cfitsio_error();
    }
    int hdu;
    fits_get_hdu_num( fitsio_, &hdu );

    //  Success so make it official.
    return FitsIO::setHDU( hdu );
}

/**
 * Get the contents of the given column as an array of ints.
 * The caller should pass an array of numValues ints.
 */
int StarFitsIO::getTableColumn(int col, int *values, int numValues)
{
    if ( !fitsio_ ) return 1;

    int status = 0, anynull = 0;
    if ( fits_read_col( fitsio_, TINT, col, 1, 1, numValues, NULL,
                        values, &anynull, &status ) != 0 )
        return cfitsio_error();

    return 0;
}

/**
 * Get the contents of the given column as an array of longs.
 * The caller should pass an array of numValues longs.
 */
int StarFitsIO::getTableColumn(int col, long *values, int numValues)
{
    if ( !fitsio_ ) return 1;

    int status = 0, anynull = 0;
    if ( fits_read_col( fitsio_, TLONG, col, 1, 1, numValues, NULL,
                        values, &anynull, &status ) != 0 )
        return cfitsio_error();

    return 0;
}

/**
 * Get the contents of the given column as an array of long longs.
 * The caller should pass an array of numValues long longs.
 */
int StarFitsIO::getTableColumn(int col, LONGLONG *values, int numValues)
{
    if ( !fitsio_ ) return 1;

    int status = 0, anynull = 0;
    if ( fits_read_col( fitsio_, TLONGLONG, col, 1, 1, numValues, NULL,
                        values, &anynull, &status ) != 0 )
        return cfitsio_error();

    return 0;
}

//
// Compressed image support
// ========================
//

/**
 * Check if the current HDU contains a compressed image.
 */
int StarFitsIO::isCompressedImage()
{
    int status = 0;
    return fits_is_compressed_image( fitsio_, &status );
}

/**
 * Save the compressed image in the current HDU to a disk file.
 * Also merges the headers with those of the primary image if required.
 */
int StarFitsIO::saveCompressedImage( const char *filename, const char *object )
{
    char card[FITSCARD+1];
    double *array;
    double bscale = 1.0;
    double bzero = 0.0;
    double nulval = 0.0;
    fitsfile *outfptr;
    int anynul = 0;
    int bitpixin = 0;
    int bitpixout = 0;
    int bytepix = 0;
    int intype = 0;
    int naxis = 0;
    int nkeys = 0;
    int outtype = 0;
    int status = 0;
    long naxes[7];
    long nbytes = 0;
    long npix = 0;

    //  If the file exists, rename it to make a backup and to avoid
    //  crashing if we have the file mapped already
    if ( access( filename, F_OK ) == 0 ) {
        char backup[1024];
        sprintf( backup, "%s.BAK", filename );
        if ( rename( filename, backup ) != 0 ) {
            return sys_error( "cannot create backup file for ", filename );
        }
    }

    //  Decompressing is tricky, especially using file mapping, so we will
    //  just use the native cfitsio routines to do this work.
    fits_create_file( &outfptr, filename, &status );
    if ( status != 0 ) {
        return cfitsio_error();
    }

    //  Get dimensions and total number of pixels (may be higher dimensional
    //  than an image, allow that up to the usual 7).
    for ( int i = 0; i < 7; i++ ) naxes[i] = 1;
    fits_get_img_param( fitsio_, 7, &bitpixin, &naxis, naxes, &status );
    if ( status != 0 ) {
        return cfitsio_error();
    }
    npix = 1;
    for ( int i = 0; i < naxis; i++ ) npix *= naxes[i];

    // If this is a Pan-STARRS log or sinh compressed image we'll need to
    // expand it, so change the bitpix of the output image.
    double boffset = 0.0;
    double bsoften = 0.0;
    int panstarr = 0;
    int issinh = 0;
    if ( get( "BOFFSET", boffset ) == 0 ) {
        //  Yes it is.
        panstarr = 1;
        bitpixout = DOUBLE_IMG;

        //  sinh variant.
        if ( get( "BSOFTEN", bsoften ) == 0 ) {
            issinh = 1;
            cout << "is Pan-STARRS sinh compressed image" << endl;
        }
        else {
            cout << "is Pan-STARRS log compressed image" << endl;
        }
    }
    else {
        bitpixout = bitpixin;
    }

    // Create the new image
    fits_create_img( outfptr, bitpixout, naxis, naxes, &status );

    // Set the OBJECT card to hold the original name, if given.
    if ( object != NULL && object[0] != '\0' ) {
        char lcard[1024];
        sprintf( lcard, "OBJECT  = '%s'", object );
        lcard[80] = '\0';
        fits_write_record( outfptr, lcard, &status );
    }

    // Copy all the existing user keywords (not the structural keywords)
    // from the current HDU.
    fits_get_hdrspace( fitsio_, &nkeys, NULL, &status );
    for ( int i = 1; i <= nkeys; i++) {
        fits_read_record( fitsio_, i, card, &status );
        if ( fits_get_keyclass( card ) > TYP_CMPRS_KEY ) {
            fits_write_record( outfptr, card, &status );
        }
    }

    // Same for headers from the primary extension, if needed.
    if ( mergeNeeded() ) {
        int plength = primaryHeader_.length();
        int pcards = plength / FITSCARD;
        char *p = (char *) primaryHeader_.ptr();
        for ( int i = 0 ; i < pcards; i++, p += FITSCARD ) {
            if ( fits_get_keyclass( p ) > TYP_CMPRS_KEY ) {
                fits_write_record( outfptr, p, &status );
            }
        }
    }

    // Work out data characteristics and allocate memory to hold
    // decompressed image.
    switch( bitpixin ) {
    case BYTE_IMG:
        intype = TBYTE;
        break;
    case SHORT_IMG:
        intype = TSHORT;
        break;
    case LONG_IMG:
        intype = TINT;
        break;
    case LONGLONG_IMG:
        intype = TLONGLONG;
        break;
    case FLOAT_IMG:
        intype = TFLOAT;
        break;
    case DOUBLE_IMG:
        intype = TDOUBLE;
        break;
    }

    // If this is a Pan-STARRS log compressed image we'll need to expand it.
    if ( panstarr ) {
        outtype = TDOUBLE;
        intype = TDOUBLE;
    }
    else {
        outtype = intype;
    }
    bytepix = abs( bitpixout ) / 8;
    nbytes = npix * bytepix;
    array = (double *) malloc( nbytes );

    // Turn off any scaling so that we copy the raw pixel values,
    // if needed. Note should be undone by any call that changes HDU.
    fits_set_bscale( fitsio_, bscale, bzero, &status );
    fits_set_bscale( outfptr, bscale, bzero, &status );

    // Read image and write it back to the output file.
    fits_read_img( fitsio_, intype, 1, npix, &nulval, array, &anynul,
                   &status );
    if ( panstarr ) {
        double v;
        double blank;
        long i;
        get( fitsio_, "BSCALE", bscale );
        get( fitsio_, "BZERO", bzero );
        get( fitsio_, "BLANK", blank );
        if ( issinh ) {
            for ( i = 0; i < npix; i++ ) {
                v = array[i];
                if ( v != blank ) {
                    v = v * bscale + bzero;
                    array[i] = 2.0 * bsoften * sinh( v / 1.08573620475813 )
                               + boffset;

                    //  Unscaled to match file BSCALE and BZERO.
                    array[i] = ( array[i] - bzero ) / bscale;
                }
                else {
                    v = FP_NAN;
                }
            }
        }
        else {
            for ( i = 0; i < npix; i++ ) {
                v = array[i];
                if ( v != blank ) {
                    v = v * bscale + bzero;
                    array[i] = pow( 10.0, v ) + boffset;

                    //  Unscaled to match file BSCALE and BZERO.
                    array[i] = ( array[i] - bzero ) / bscale;
                }
                else {
                    v = FP_NAN;
                }
            }
        }
    }
    fits_write_img( outfptr, outtype, 1, npix, array, &status );

    // Expect to be at the end of file.
    if ( status == END_OF_FILE ) {
        status = 0;
    }

    // Close file and release memory.
    fits_close_file( outfptr,  &status );
    free( array );

    if ( status ) {
        return cfitsio_error();
    }
    return OK;
}

