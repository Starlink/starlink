/*
 * E.S.O. - VLT project / ESO Archive
 *
 * "@(#) $Id$"
 *
 * StarFitsIO.C - method definitions for class StarFitsIO, for operating on
 *                Fits files. This class redefines class FitsIO to use
 *                the Starlink AST library. It also forces the use of
 *                readonly access, if that is all that is available
 *                and correctly saves a set of merged headers for
 *                extension images.
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
 */
static const char* const rcsId="@(#) $Id$";

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

// Initialize static members.
int StarFitsIO::alwaysMerge_ = 0;

enum {FITSBLOCK=2880};

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
 *  readonly access is just available.
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
    int bitpix = 0, width = 0, height = 0;
    double bzero = 0.0, bscale = 1.0;
    get( fitsio, "NAXIS1", width );
    get( fitsio, "NAXIS2", height );
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
 *  Initialize world coordinates (based on the image header)
 */
int StarFitsIO::wcsinit()
{
    //   If there are multiple HDUs, merge the primary header with
    //   the extension header to get all of the WCS info.
    if ( mergeNeeded() ) {
        mergeHeader();
        wcs_ = WCS( new StarWCS( (const char *)mergedHeader_.ptr(),
                                 mergedHeader_.size() ) );
        return wcs_.status();
    }
    wcs_ = WCS( new StarWCS( (const char *)header_.ptr(), header_.size() ) );
    return wcs_.status();
}

/**
 * Determine if we need to merge the header with those from the primary HDU or
 * not. If the alwaysMerge_ member is set true, then we always merge,
 * otherwise we only merge if the primary HDU contains a dummy image.
 */
int StarFitsIO::mergeNeeded()
{
    // Don't need to do anything for the primary HDU
    if ( getNumHDUs() == 1 || getHDUNum() == 1 ) {
        return 0;
    }

    int result = alwaysMerge_;
    if ( !result ) {
        //  Need to check the primary image size... Don't want to modify the
        //  current HDU so need to do this the hard way?
        int ncards = primaryHeader_.length() / 80;
        char *ptr = (char *) primaryHeader_.ptr();
        char *subptr = NULL;
        int naxis = 0;
        for ( int i = 0 ; i < ncards; i++, ptr += 80 ) {
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
    strncpy( newheader + 80, (char *)header_.ptr() + 80, elength - 80 );

    //  Get number of cards in each of the headers.
    int fixedcards = elength / 80;
    int extracards = plength / 80;

    //  Locate and skip the END card (this is where we start adding
    //  any new cards).
    char *endPtr = (char *) newheader;
    int newlength = 0;
    int i = 0;
    for ( i = 0 ; i < fixedcards; i++, endPtr += 80, newlength += 80 ) {
        if ( strncmp( endPtr, "END     ", 8 ) == 0 ) {
            break;
        }
    }
    fixedcards = newlength / 80;

    //  Loop over all primary headers, adding any new or special
    //  cards to the end of the new headers.
    char *extraPtr = (char *) primaryHeader_.ptr();
    int skip = 0;
    for ( i = 0 ; i < extracards; i++, extraPtr += 80 ) {

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
            for ( int j = 0; j < fixedcards; j++, mainPtr += 80 ) {
                if ( strncmp( extraPtr, mainPtr, 8 ) == 0 ) {
                    skip = 1;
                    break;
                }
            }
        }
        if ( ! skip ) {

            //  Keyword not seen or special, so append this.
            strncpy( endPtr, extraPtr, 80 );
            endPtr += 80;
            newlength += 80;
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
    if (access(filename, F_OK) == 0) {
	char backup[1024];
	sprintf(backup, "%s.BAK", filename);
	if (rename(filename, backup) != 0)
	    return sys_error("can't create backup file for ", filename);
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
	int size = FITSBLOCK/80;  // number of keyword lines in FITS header, including END

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
