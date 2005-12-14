/*
 * E.S.O. - VLT project / ESO Archive
 *
 * "@(#) $Id: FitsIO.C,v 1.5 2005/02/02 01:43:04 brighton Exp $" 
 *
 * FitsIO.C - method definitions for class FitsIO, for operating on
 *            Fits files.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * P.Biereichel    18/07/96  put_keyword char version added for SIMPLE. BITPIX=-16 fixed
 * Allan Brighton  16/02/98  renamed check_decompress to check_compress and added check
 *                           for bitpix=16 for H_COMPRESS.
 *                 12/03/98  Initialize WCS in constructor.
 * Peter W. Draper 26/01/00  Now adds SIMPLE=T when saving extensions. 
 *                           Made strftime call Y2K compliant.
 * Peter W. Draper 04/02/00  Changed constness of write so that
 *                           non-const member can be used within this
 *                           member. 
 * pbiereic        17/02/03  Added 'using namespace std'. Removed ::std specs.
 * pbiereic        20/07/04  use %20 field width for keywords in methods put_keyword,
 *                           so that other tools like xv, ds9, fv can read stored real-time
 *                           images.
 */
static const char* const rcsId="@(#) $Id: FitsIO.C,v 1.5 2005/02/02 01:43:04 brighton Exp $";

#include "config.h" // tclutil

#include <cstdio>
#include <cstring>
#include <cctype>
#include <string>
#include <iostream>
#include <sstream>
#include <cstdlib>
#include <unistd.h>
#include <cmath>
#include <ctime>
#include "util.h"
#include "error.h"
#include "fitsio.h"
#include "SAOWCS.h"
#include "Compress.hxx"
#include "Mem.h"
#include "FitsIO.hxx"
#include "define.h"

using namespace std;

// The type "long" may have 64 bits.
#if LONGSIZE == 64
#define FITS_ULONG unsigned long
#define FITS_UINT unsigned int
#else 
#define FITS_ULONG unsigned long long
#define FITS_UINT unsigned long
#endif

// size of a FITS block
enum {FITSBLOCK=2880};

// common error message
const char* noFitsErrMsg = "No FITS file is currently open";

const char* noHdrErrMsg = "Can't get keyword: no FITS header.";

// local buffer used to return keyword and table values
static char buf_[1024];

// "current" FitsIO object pointer, needed for cfitsio realloc callback
FitsIO* FitsIO::fits_ = NULL;


/*
 * constructor: initialize from the given header and data objects
 * and, optionally, the cfitsio handle used to open the file.
 * 
 * Note: the public interface is normally via FitsIO::read(),
 * when reading from a file. This constructor may be usefull
 * though when creating a FitsIO object from memory data.
 */
FitsIO::FitsIO(int width, int height, int bitpix, double bzero, 
	       double bscale, const Mem& header, const Mem& data,
	       fitsfile* fitsio)
    : ImageIORep(width, height, bitpix, bzero, bscale, header, data),
      fitsio_(fitsio) 
{
    // Save a reference to the primary header
    primaryHeader_ = header;	// ref counted copy
}


/*
 * Destructor: close the cfitsio object
 */
FitsIO::~FitsIO()
{
    if (fitsio_) {
	int status = 0;
	if (fits_close_file(fitsio_, &status) != 0)
	    cfitsio_error();
	fitsio_ = NULL;
    }
}


/*
 * Return a copy of this object that shares the data, but can have a different current HDU
 */
FitsIO* FitsIO::copy()
{
    int status = 0;
    fitsfile* newFitsio;
    fits_reopen_file(fitsio_, &newFitsio, &status);
    if (status != 0)
	return NULL;
    return new FitsIO(width_, height_, bitpix_, bzero_, bscale_, 
		      header_, data_, newFitsio);
}


/*
 * initialize world coordinates (based on the image header)
 */
int FitsIO::wcsinit()
{
    // if there are multiple HDUs, merge the primary header with 
    // the extension header to get all of the WCS info
    // (requested by Andreas Wicenec <awicenec@eso.org>).
    if (getNumHDUs() > 1) {
	int length = header_.length() + primaryHeader_.length();
	mergedHeader_ = Mem(length+1, 0);
	if (mergedHeader_.status() == 0) {

	    strncpy((char*)mergedHeader_.ptr(), 
		    (char*)header_.ptr(), 
		    header_.length());

	    strncpy(((char*)mergedHeader_.ptr())+header_.length(), 
		    (char*)primaryHeader_.ptr(), 
		    primaryHeader_.length());
	    
	    ((char*)mergedHeader_.ptr())[length] = '\0';

	    wcs_ = WCS(new SAOWCS((const char*)mergedHeader_.ptr(), length));
	    return wcs_.status();
	}
    }
    wcs_ = WCS(new SAOWCS((const char*)header_.ptr(), header_.length()));
    return wcs_.status();
}


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
 * Check if the given file has a suffix that indicates a known
 * compression type, and if so, compress or decompress the file,
 * depending on the value of decompress_flag.
 *
 * The suffixes recognized correspond to the http Content-types:
 *
 *    hfits:                   for H-Compress
 *    gfits, gzfits, gz:       for GZIP
 *    cfits, Z:                for UNIX compress.
 *
 * If istemp is true on entry, filename is overwritten with the
 * decompressed version.  In any case, if any (de)compression is done,
 * the output is written to a temp file and "istemp" is set to 1. 
 *
 * The return value is the name of the file, possibly after decompression.  
 * The "buf" argument gives the space for the temporary filename, if
 * needed.  The last argument is the value of "bitpix", if known, or
 * 0. This is used to check for the correct file type for h_compress,
 * which only works on 16 bit FITS files.
 */
const char* FitsIO::check_compress(const char* filename, char* buf, int bufsz, int& istemp,
				   int decompress_flag, int bitpix)
{
    // check the file extension for recognized compression types
    const char* suffix = strrchr(filename, '.'); 
    if (suffix)
	suffix++;
    else
	suffix = "";

    Compress::CompressType ctype = Compress::NO_COMPRESS;
    if (strcmp(suffix, "hfits") == 0) {
	if (bitpix && abs(bitpix) != 16) {
	    error("H-compress is only allowed for 16 bit FITS images");
	    if (istemp)
		unlink(filename);
	    return NULL;
	}
	ctype = Compress::H_COMPRESS;
    } 
    else if (strcmp(suffix, "gfits") == 0 
	     || strcmp(suffix, "gzfits") == 0
	     || strcmp(suffix, "gz") == 0) {
	ctype = Compress::GZIP_COMPRESS;
    } 
    else if (strcmp(suffix, "cfits") == 0
	     || strcmp(suffix, "Z") == 0) {
	ctype = Compress::UNIX_COMPRESS;
    } 

    if (ctype != Compress::NO_COMPRESS) {
	Compress c;
	char tmpfile[1024];
	static int count = 0;  // for unique filename
	int status = 0;

	if (decompress_flag) {
	    // don't use file's dir when decompressing, since it might not be writable
	    sprintf(tmpfile, "/tmp/fio%d%d.fits", getpid(), count++);
	    status = c.decompress(filename, tmpfile, ctype);
	}
	else {
	    // use file's dir when compressing, so that rename() is possible
	    sprintf(tmpfile, "%s.tmp", filename);
	    status = c.compress(filename, tmpfile, ctype);
	}

	if (istemp)
	    unlink(filename);

	if (status != 0) {
	    unlink(tmpfile);
	    return NULL;
	}

	istemp = 1;
	strncpy(buf, tmpfile, bufsz);
	return buf;
    }
    return filename;
}


/*
 * Read a FITS file and return an initialized FitsIO object for it,
 * or NULL if there are errors. 
 *
 * If filename is "-", stdin is read into a temp image file and used as the input.
 *
 * The Mem class is used to speed up loading the file. The optional mem_options
 * control whether the memory is mapped read-only or read/write (see class Mem).
 */
FitsIO* FitsIO::read(const char* filename, int mem_options)
{
    FILE *f;
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
    filename = check_compress(filename, tmpfile, (int)sizeof(tmpfile), istemp, 1, 0);
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
 	unlink(filename);	// will be deleted by the OS later

    return initialize(header);
}


/* 
 * Report a cfitsio error
 */
int FitsIO::cfitsio_error()
{
    char buf[81];
    ostringstream os;
    int i = 0;
    while (fits_read_errmsg(buf)) {
	os << buf << endl;
	i++;
    }
    fits_clear_errmsg();
    if (i) {
	error("cfitsio: ", os.str().c_str());
    }
    return ERROR;
}


/* 
 * This static method is called by the cfitsio routines when the size of the file
 * has to be increased, such as when adding a new FITS block or table to the file.
 * Since a client data pointer is not part of the interface, we use the "current"
 * FitsIO object, set before any operations that could result in a call to this
 * method.
 */
void* FitsIO::reallocFile(void* p, size_t newsize)
{
    if (!fits_) {
	error("No current FITS file");
	return NULL;
    }
    if (fits_->checkWritable() != 0)
	return NULL;		// not a writable FITS file
    
    // OK, we have a writable FITS file. Extend the size, remap and
    // return the mmap pointer.
    Mem m = fits_->header_;
    m.offset(0);
    if (newsize <= m.size())
	return p;
    
    m.unmap();
    if (m.remap(m.options(), newsize) != 0) 
	return NULL;		// error
    return m.ptr();
}


/*
 * This static method returns a cfitsio handle, given the Mem object for the
 * FITS header.
 */
fitsfile* FitsIO::openFitsMem(Mem& header)
{
    // filename for error reporting
    const char* filename = header.filename();
    int rw_flag;		// true if memory can be written to
    if (filename) {
	rw_flag = ((header.options() & Mem::FILE_RDWR) != 0);
    }
    else {
	filename = "FitsIO";
	rw_flag++;
    } 

    // use the cfitsio library routines to access the file in memory
    fitsfile* fitsio = NULL;
    int status = 0;
    
    // cfitsio wants to have (and save) pointers to the data pointer and size, 
    // so we have to provide the address of these by accessing the internal 
    // MemRep class in the header object.
    // XXX (make sure the values are not actually modified...)
    MemRep* mrep = (MemRep*)header.rep();
    if (fits_open_memfile(&fitsio, filename, rw_flag, &mrep->ptr, (size_t*)&mrep->size, 
			  FITSBLOCK, FitsIO::reallocFile, &status) != 0) {
	cfitsio_error();
	return NULL;
    }
    return fitsio;
}


/* 
 * Check that this object represents a FITS file (and not just some kind of memory)
 * and return 0 if it does. If not, return an error message.
 */
int FitsIO::checkFitsFile()
{
    // For now, only support extending headers in mmap'ed FITS files
    if (!fitsio_ 
	|| (! (header_.filename() && data_.filename() 
	   && strcmp(header_.filename(), data_.filename()) == 0))) { 
	return error("FitsIO: Operation not allowed on memory image");
    }
    return 0;
}


/*
 * This static method returns an allocated FitsIO object given a Mem object
 * containing the data for the file. (header points to the data for the entire
 * file...).
 */
FitsIO* FitsIO::initialize(Mem& header)
{
    fitsfile* fitsio = openFitsMem(header);
    if (!fitsio)
	return NULL;

    long headStart = 0, dataStart = 0, dataEnd = 0;
    int status = 0;
    if (fits_get_hduaddr(fitsio, &headStart, &dataStart, &dataEnd, &status) != 0) {
	cfitsio_error();
	return NULL;
    }

    if (header.length() < (dataEnd - headStart)) {
	const char* filename = header.filename();
	if (filename)
	    log_message("FITS file has the wrong size (too short): %s", filename);
	else 
	    log_message("FITS data has the wrong size (too short)");
	// return NULL;
    }

    // The data part is the same mmap area as the header, with an offset
    Mem data(header);
    header.length(dataStart - headStart);  // set usable length of header
    data.offset(dataStart);    // set offset for data
    data.length(dataEnd-dataStart);

    return initialize(header, data, fitsio);
}


/*
 * This static method returns an allocated FitsIO object, given the cfitsio
 * handle.
 */
FitsIO* FitsIO::initialize(Mem& header, Mem& data, fitsfile* fitsio)
{    
    int bitpix = 0, width = 0, height = 0;
    double bzero = 0.0, bscale = 1.0;
    get(fitsio, "NAXIS1", width);
    get(fitsio, "NAXIS2", height);
    get(fitsio, "BITPIX", bitpix);
    get(fitsio, "BSCALE", bscale);
    get(fitsio, "BZERO", bzero);
    
    return new FitsIO(width, height, bitpix, bzero, bscale, header, data, fitsio);
}


/*
 * This static method returns an allocated FitsIO object given the 
 * header and data.
 */
FitsIO* FitsIO::initialize(Mem& header, Mem& data)
{
    fitsfile* fitsio = openFitsMem(header);
    if (!fitsio)
	return NULL;

    return initialize(header, data, fitsio);
}

   
/*
 * Generate a blank image with a FITS header based on the given fields.
 * The arguments are the position as ra, dec, equinox in double degrees,
 * the radius in arcmin, the width and height in pixels and the color
 * to use for the image (the color value for black, for example).
 *
 * If ra < 0, no world coordinate info is added to the header. This will
 * create just a blank image with no WCS.
 *
 */
FitsIO* FitsIO::blankImage(double ra, double dec, double equinox, 
			   double radius, int width, int height,
			   unsigned long color0)
{
    // generate the fits data
    if (width <= 0 || height <= 0) {
	error("width and height must be positive integers");
	return NULL;
    }

    Mem data(width*height, 0);
    if (data.status() != 0) 
	return NULL;
    char* d = (char*)data.ptr();
    memset(d, color0, width*height);

    Mem header(FITSBLOCK, 0);	// more than large enough to hold the fields below
    if (header.status() != 0) 
	return NULL;

    ostringstream os;
 
    // generate the fits header
    double r = radius/60.0;	// radius in degrees
    
    put_keyword(os, "SIMPLE", "T");          //FITS header    
    put_keyword(os, "BITPIX", 8);	     // No.Bits per pixel  
    put_keyword(os, "NAXIS ", 2);	     // No.dimensions                                   
    put_keyword(os, "NAXIS1", width);        // Length X axis                                   
    put_keyword(os, "NAXIS2", height);       // Length Y axis    
    
    // this causes the pixels to appear black (if the colormap starts with black)
    put_keyword(os, "DATAMIN", int(color0));       // min color    
    put_keyword(os, "DATAMAX", int(color0+256));   // (theoretical) max color
    
                              
    if (ra >= 0) {
	double cdelt2 = sqrt((r*r)/2.0)/(width/2.0);
	double cdelt1 = -cdelt2;
	put_keyword(os, "CTYPE1", "RA---TAN");   // R.A. in tangent plane projection                
	put_keyword(os, "CTYPE2", "DEC--TAN");   // DEC. in tangent plane projection                
	put_keyword(os, "CRPIX1", width/2+0.5);  // Refpix of first axis                            
	put_keyword(os, "CRPIX2", height/2+0.5); // Refpix of second axis                           
	put_keyword(os, "CRVAL1", ra);           // RA at Ref pix in decimal degrees                
	put_keyword(os, "CRVAL2", dec);          // DEC at Ref pix in decimal degrees               
	put_keyword(os, "CDELT1", cdelt1);       // RA pixel step (deg)                             
	put_keyword(os, "CDELT2", cdelt2);       // DEC pixel step (deg) 
	put_keyword(os, "EQUINOX", 2000.0);      // default equinox 
	put_keyword(os, "RADECSYS", "FK5");      // J2000...
    }

    //put_keyword(os, "BLANK", (int)color0);   // blank pixel value

    char buf[81];
    sprintf(buf, "%-80s", "END"); // mark the end of the header

    strncpy((char*)header.ptr(), os.str().c_str(), header.length()); // write to shared memory

    // generate the blank image 
    return new FitsIO(width, height, BYTE_IMAGE, 0.0, 1.0, header, data);
}


/* 
 * write the keyword/value pair to the given stream.
 * (char* value version)
 */
int FitsIO::put_keyword(ostream& os, const char* keyword, char* value) 
{
    char  buf1[81], buf2[81];
    sprintf(buf1, "%-8s= '%s'", keyword, value);
    sprintf(buf2, "%-80s", buf1);
    os << buf2;
    return 0;
}


/* 
 * write the keyword/value pair to the given stream.
 * (char value version)
 */
int FitsIO::put_keyword(ostream& os, const char* keyword, char value) 
{
    char  buf1[81], buf2[81];
    sprintf(buf1, "%-8s= %20c", keyword, value);
    sprintf(buf2, "%-80s", buf1);
    os << buf2;
    return 0;
}


/* 
 * write the keyword/value pair to the given stream.
 * (int value version)
 */
int FitsIO::put_keyword(ostream& os, const char* keyword, int value) 
{
    char  buf1[81], buf2[81];
    sprintf(buf1, "%-8s= %20d", keyword, value);
    sprintf(buf2, "%-80s", buf1);
    os << buf2;
    return 0;
}

/* 
 * write the keyword/value pair to the given stream.
 * (double value version)
 */
int FitsIO::put_keyword(ostream& os, const char* keyword, double value) 
{
    char  buf1[81], buf2[81];
    sprintf(buf1, "%-8s= %20f", keyword, value);
    sprintf(buf2, "%-80s", buf1);
    os << buf2;
    return 0;
}


/* 
 * write the keyword/value pair to the given open file descriptor.
 * (int value version)
 */
int FitsIO::put_keyword(FILE* f, const char* keyword, int value) 
{
    char  buf[81];
    sprintf(buf, "%-8s= %20d", keyword, value);
    fprintf(f, "%-80s", buf);
    return 0;
}

/* 
 * write the keyword/value pair to the given open file descriptor.
 * (double value version)
 */
int FitsIO::put_keyword(FILE* f, const char* keyword, double value) 
{
    char  buf[81];
    sprintf(buf, "%-8s= %20f", keyword, value);
    fprintf(f, "%-80s", buf);
    return 0;
}

/* 
 * write the keyword/value pair to the given open file descriptor.
 * (char* value version)
 */
int FitsIO::put_keyword(FILE* f, const char* keyword, const char* value) 
{
    char  buf[81];
    sprintf(buf, "%-8s= '%s'", keyword, value);
    fprintf(f, "%-80s", buf);
    return 0;
}


/* 
 * write the keyword/value pair to the given open file descriptor.
 * (char value version)
 */
int FitsIO::put_keyword(FILE* f, const char* keyword, char value) 
{
    char  buf[81];
    sprintf(buf, "%-8s= %20c", keyword, value);
    fprintf(f, "%-80s", buf);
    return 0;
}


/*
 * round off the file size to the next FITS block.
 * (size is the current size)
 */
void FitsIO::padFile(FILE* f, int size) 
{
    int rest = (size + FITSBLOCK) % FITSBLOCK;
    if (rest) {
	while (rest < FITSBLOCK) {
	    fputc(' ', f);
	    rest++;
	}
    }
}
    

/* 
 * write a fits file from the data and header, if present
 */
int FitsIO::write(const char *filename)
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
       if ( getNumHDUs() > 1 && getHDUNum() != 1 ) {
          
          //  Saving an image stored in an extension, so need to add
          //  the "SIMPLE" keyword and remove the "XTENSION" one.
          put_keyword(f, "SIMPLE", 'T');
          nextrec += 80;
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
	put_keyword(f, "COMMENT", "Generated by FitsIO::write"); size--;

	// add a timestamp
	char buf2[50];
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
	fwriteNBO((char*)data_.ptr(), tsize, width_*height_, f);
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
	if (BIGENDIAN == usingNetBO()) {  // native byte order?
	    while (i--) {
		nn = (int)(*pu++) - 32768;
		*ps++ = (unsigned int) nn;
	    }
	}
	else {
	    while (i--) {
		nn = (int)(SWAP16(*pu)) - 32768;
		*ps = (unsigned int) SWAP16(nn);
		pu++;
		ps++;
	    }
	}
	fwriteNBO((char*)ps_new, tsize, width_*height_, f);
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
    
    const char *tmpfile = check_compress(filename, tmpfilename, (int)sizeof(tmpfilename), 
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
 * Write data to disk (network byte ordered, NBO).
 * Byte swap is only needed when data are in shm and not
 * in BIGENDIAN.
 * Since data are passed as char* there is no automatic type
 * conversion by gcc.
 */
int FitsIO::fwriteNBO(char *data, int tsize, int size, FILE *f) const
{
    int status;
    int n = size;

    if (tsize == 1 || usingNetBO()) {
        return fwrite(data, tsize, size, f);
    }

    Mem dbuf(size * tsize, 0);
    if (dbuf.status() != 0)
        return 0;

    if (tsize == 2) {
        unsigned short *from = (unsigned short *) data;
        unsigned short *to   = (unsigned short *) dbuf.ptr();
        while (n--) {
            *to++ = SWAP16(*from);
            from++;
        }
    }

    else if (tsize == 4) {
        FITS_UINT *from = (FITS_UINT *) data;
        FITS_UINT *to   = (FITS_UINT *) dbuf.ptr();
        while (n--) {
            *to++ = SWAP32(*from);
            from++;
        }
    }
    else if (tsize == 8) {
        FITS_ULONG *from = (FITS_ULONG *) data;
        FITS_ULONG *to   = (FITS_ULONG *) dbuf.ptr();
        while (n--) {
            *to++ = SWAP64(*from);
            from++;
        }
    }

    status = fwrite(dbuf.ptr(), tsize, size, f);

    return status;
}


/*
 *  write a (ASCII formatted) copy of the FITS header to the given stream.
 * (format it in 80 char lines and replace any NULL chars with blanks)
 */
int FitsIO::getFitsHeader(ostream& os) const
{
    string s((char*)header_.ptr(), header_.length());
    istringstream is(s);
    char buf[81];
    while(is.read(buf, 80)) {
	for (int i = 0; i < 79; i++)
	    if (!isascii(buf[i]))
		buf[i] = ' ';
	buf[79] = '\n';
	os.write(buf, 80);
	if (strncmp(buf, "END     ", 8) == 0)
	    break;
    }
    return 0;
}


/* 
 * Check if we are inserting a new keyword or updating an existing one.
 * If inserting, make sure there is enough space in the header and,
 * if not, try to make space by enlarging the header.
 * Returns 0 if OK.
 */
int FitsIO::checkKeywordSpace(const char* keyword)
{
    // make sure the file was mapped with write permission
    if (checkWritable() != 0) 
	return 1;		// error

    if (!get(keyword)) {	// keyword not found in header?

	// see if there is room in the header for more keys
	int keysExist = 0, moreKeys = 0, status = 0;
	if (fits_get_hdrspace(fitsio_, &keysExist, &moreKeys, &status) != 0) 
	    return cfitsio_error();
	
	// if there is no more room, make some room
	// (Normally cfitsio would do this automatically, but since we 
	//  may be using mmap, it is more complicated.
	if (moreKeys == 0 && extendHeader() != 0)
	    return 1;
    }
    return 0;
}


/*
 * flush any memory changes to the file
 */
int FitsIO::flush() 
{
    int status = 0;
    fits_ = this;		// reallocFile() might be called
    fits_flush_file(fitsio_, &status);
    fits_ = NULL;
    if (status != 0)
	return cfitsio_error();
    return 0;
}


/* 
 * Insert the given FITS keyword and value and return 0 if OK.
 * If there is not enough space in the header, the file size is
 * automatically increased.
 */
int FitsIO::put(const char* keyword, double val, const char* comment) 
{
    // make sure there is enough space in the header
    if (checkKeywordSpace(keyword) != 0)
	return 1;

    int status = 0;
    if (fits_update_key(fitsio_, TDOUBLE, (char*)keyword, &val, (char*)comment, &status) != 0)
	return cfitsio_error();

    return flush();
}


int FitsIO::put(const char* keyword, float val, const char* comment) 
{
    // make sure there is enough space in the header
    if (checkKeywordSpace(keyword) != 0)
	return 1;

    int status = 0;
    if (fits_update_key(fitsio_, TFLOAT, (char*)keyword, &val, (char*)comment, &status) != 0)
	return cfitsio_error();

    return flush();
}


int FitsIO::put(const char* keyword, int val, const char* comment) 
{
    // make sure there is enough space in the header
    if (checkKeywordSpace(keyword) != 0)
	return 1;

    int status = 0;
    if (fits_update_key(fitsio_, TINT, (char*)keyword, &val, (char*)comment, &status) != 0)
	return cfitsio_error();

    return flush();
}


int FitsIO::put(const char* keyword, const char* val, const char* comment) 
{
    // make sure there is enough space in the header
    if (checkKeywordSpace(keyword) != 0)
	return 1;

    int status = 0;
    if (fits_update_key(fitsio_, TSTRING, (char*)keyword, (char*)val, (char*)comment, &status) != 0)
	return cfitsio_error();

    return flush();
}


/*
 * extend the size of the FITS header by one header block and if the
 * header is part of an mmap'ed file, rewrite the file with the
 * new enlarged header. 
 */
int FitsIO::extendHeader()
{
    // Make sure we have a writable FITS file
    if (checkWritable() != 0)
	return 1;		// error

    int status = 0;
    if (fits_write_comment(fitsio_, "FitsIO: added 1 block to header", &status) != 0) 
	return cfitsio_error();

    // calling flush will cause reallocFile() to be called
    if (flush() != 0)
	return 1;		// error

    return setHDU(getHDUNum());		// reset header/data mem offsets
}


/*
 * get value for the given FITS keyword and return 0 if OK (found)
 */
int FitsIO::get(const char* keyword, double& val) const {
    if (! fitsio_)
	return error(noHdrErrMsg);
    int status = 0;
    if (fits_read_key(fitsio_, TDOUBLE, (char*)keyword, &val, NULL, &status) != 0)
	return cfitsio_error();
    return 0;
}


int FitsIO::get(const char* keyword, float& val) const {
    if (! fitsio_)
	return error(noHdrErrMsg);
    int status = 0;
    if (fits_read_key(fitsio_, TFLOAT, (char*)keyword, &val, NULL, &status) != 0)
	return cfitsio_error();
    return 0;
}


int FitsIO::get(const char* keyword, int& val) const {
    if (! fitsio_)
	return error(noHdrErrMsg);
    int status = 0;
    if (fits_read_key(fitsio_, TINT, (char*)keyword, &val, NULL, &status) != 0)
	return cfitsio_error();
    return 0;
}


int FitsIO::get(const char* keyword, long& val) const {
    if (! fitsio_)
	return error(noHdrErrMsg);
    int status = 0;
    if (fits_read_key(fitsio_, TLONG, (char*)keyword, &val, NULL, &status) != 0)
	return cfitsio_error();
    return 0;
}


int FitsIO::get(const char* keyword, unsigned char& val) const {
    if (! fitsio_)
	return error(noHdrErrMsg);
    int status = 0;
    if (fits_read_key(fitsio_, TBYTE, (char*)keyword, &val, NULL, &status) != 0)
	return cfitsio_error();
    return 0;
}


int FitsIO::get(const char* keyword, short& val) const {
    if (! fitsio_)
	return error(noHdrErrMsg);
    int status = 0;
    if (fits_read_key(fitsio_, TSHORT, (char*)keyword, &val, NULL, &status) != 0)
	return cfitsio_error();
    return 0;
}


int FitsIO::get(const char* keyword, unsigned short& val) const {
    if (! fitsio_)
	return error(noHdrErrMsg);
    int status = 0;
    if (fits_read_key(fitsio_, TUSHORT, (char*)keyword, &val, NULL, &status) != 0)
	return cfitsio_error();
    return 0;
}


/*
 * find and return the value for the given FITS keyword, or NULL if not found
 * The returned value, if any, points to static data and should be saved by the
 * caller.
 */
char* FitsIO::get(const char* keyword) const {
    if (! fitsio_) {
	error(noHdrErrMsg);
	return NULL;
    }
    int status = 0;
    if (fits_read_key(fitsio_, TSTRING, (char*)keyword, buf_, NULL, &status) != 0) {
	cfitsio_error();
	return NULL;
    }
    return buf_;
}


/*
 * This is the same as get(const char*), but you supply the buffer to hold 
 * the result, which is then an empty string, if not found.
 */
char* FitsIO::get(const char* keyword, char* buf, int bufsz) const
{
    char* s = get(keyword);
    if (s)
	strncpy(buf, s, bufsz);
    else 
	buf[0] = '\0';
    return buf;
}


/*
 * these are static versions of the above
 */
int FitsIO::get(fitsfile* fitsio, const char* keyword, double& val) {
    if (! fitsio)
	return error(noHdrErrMsg);
    int status = 0;
    if (fits_read_key(fitsio, TDOUBLE, (char*)keyword, &val, NULL, &status) != 0)
	return cfitsio_error();
    return 0;
}


int FitsIO::get(fitsfile* fitsio, const char* keyword, float& val) {
    if (! fitsio)
	return error(noHdrErrMsg);
    int status = 0;
    if (fits_read_key(fitsio, TFLOAT, (char*)keyword, &val, NULL, &status) != 0)
	return cfitsio_error();
    return 0;
}


int FitsIO::get(fitsfile* fitsio, const char* keyword, int& val) {
    if (! fitsio)
	return error(noHdrErrMsg);
    int status = 0;
    if (fits_read_key(fitsio, TINT, (char*)keyword, &val, NULL, &status) != 0)
	return cfitsio_error();
    return 0;
}


int FitsIO::get(fitsfile* fitsio, const char* keyword, long& val) {
    if (! fitsio)
	return error(noHdrErrMsg);
    int status = 0;
    if (fits_read_key(fitsio, TLONG, (char*)keyword, &val, NULL, &status) != 0)
	return cfitsio_error();
    return 0;
}


int FitsIO::get(fitsfile* fitsio, const char* keyword, unsigned char& val) {
    if (! fitsio)
	return error(noHdrErrMsg);
    int status = 0;
    if (fits_read_key(fitsio, TBYTE, (char*)keyword, &val, NULL, &status) != 0)
	return cfitsio_error();
    return 0;
}


int FitsIO::get(fitsfile* fitsio, const char* keyword, unsigned short& val) {
    if (! fitsio)
	return error(noHdrErrMsg);
    int status = 0;
    if (fits_read_key(fitsio, TUSHORT, (char*)keyword, &val, NULL, &status) != 0)
	return cfitsio_error();
    return 0;
}


int FitsIO::get(fitsfile* fitsio, const char* keyword, short& val) {
    if (! fitsio)
	return error(noHdrErrMsg);
    int status = 0;
    if (fits_read_key(fitsio, TSHORT, (char*)keyword, &val, NULL, &status) != 0)
	return cfitsio_error();
    return 0;
}


/*
 * Find and return the value for the given FITS keyword, or NULL if not found.
 * The returned value, if any, points to static data and should be saved by the
 * caller.
 */
char* FitsIO::get(fitsfile* fitsio, const char* keyword) {
    if (! fitsio) {
	error(noHdrErrMsg);
	return NULL;
    }
    int status = 0;
    if (fits_read_key(fitsio, TSTRING, (char*)keyword, buf_, NULL, &status) != 0) {
	cfitsio_error();
	return NULL;
    }
    return buf_;
}


// -- HDU access --
    

/*
 * Return the total number of HDUs
 */
int FitsIO::getNumHDUs()
{
    if (!fitsio_)
	return 0;		// file might be only in memory

    int status = 0, num = 0;
    if (fits_get_num_hdus(fitsio_, &num, &status) != 0) {
	cfitsio_error();
	return 0;
    }
    return num;
}


/*
 * Return the type of the current HDU as a string: "image", "ascii", 
 * or "binary" or NULL if there was an error
 */
const char* FitsIO::getHDUType()
{
    if (!fitsio_) {
	error(noFitsErrMsg);
	return NULL;
    }
	
    int status = 0, type = 0;
    if (fits_get_hdu_type(fitsio_, &type, &status) != 0) {
	cfitsio_error();
	return NULL;
    }
    switch(type) {
    case IMAGE_HDU: return "image";
    case ASCII_TBL: return "ascii";
    case BINARY_TBL: return "binary";
    }
    return NULL;
}


/*
 * Return the number of the current HDU 
 */
int FitsIO::getHDUNum()
{
    if (!fitsio_)
	return error(noFitsErrMsg);
	
    int num = 1;
    return fits_get_hdu_num(fitsio_, &num); 
}


/*
 * Move to the specified HDU and make it the current one 
 * (Note that "num" is one based, so the primary array is num = 1.)
 */
int FitsIO::setHDU(int num)
{
    if (checkFitsFile() != 0)
	return 1;		// error

    int status = 0, type = 0;
    if (fits_movabs_hdu(fitsio_, num, &type, &status) != 0) 
	return cfitsio_error();

    long headStart = 0, dataStart = 0, dataEnd = 0;
    if (fits_get_hduaddr(fitsio_, &headStart, &dataStart, &dataEnd, &status) != 0) {
	return cfitsio_error();
    }

    // update the header and data offsets to point to the new HDU
    header_.offset(headStart);
    header_.length(dataStart - headStart);
    data_.offset(dataStart);
    data_.length(dataEnd-dataStart);

    // update these inherited member variables
    width_ = height_ = bitpix_ = 0;
    bscale_ = 1.0;
    bzero_ = 0.0;
    get(fitsio_, "NAXIS1", width_);
    get(fitsio_, "NAXIS2", height_);
    get(fitsio_, "BITPIX", bitpix_);
    get(fitsio_, "BSCALE", bscale_);
    get(fitsio_, "BZERO", bzero_);

    return 0;
}


/*
 * Delete the given HDU. Any following HDUs are shifted to fill the gap.
 */
int FitsIO::deleteHDU(int num)
{
    // make sure the file was mapped with write permission
    if (checkWritable() != 0) 
	return 1;		// error

    int curHDU = getHDUNum();

    if (setHDU(num) != 0)
	return 1;		// error

    int status = 0;
    if (fits_delete_hdu(fitsio_, NULL, &status) != 0) 
	return cfitsio_error();
    
    // reset to the original HDU
    if (curHDU <= getNumHDUs())
	return setHDU(curHDU);
    return 0;
}


// -- Fits Tables --


/*
 * Get the dimensions of the current FITS table.
 */
int FitsIO::getTableDims(long& rows, int& cols)
{
    if (!fitsio_)
	return error(noFitsErrMsg);

    int status = 0;
    if (fits_get_num_rows(fitsio_, &rows, &status) != 0
	|| fits_get_num_cols(fitsio_, &cols, &status) != 0)
	return cfitsio_error();

    return 0;
}


/*
 * Return the table heading for the given column, or NULL if there is an
 * error. The return value points to static storage... 
 */
char* FitsIO::getTableHead(int col)
{
    if (col <= 0 || col > 999) {
	 error("FITS table column index out of range");
	 return NULL;
    }
    char keyword[16];
    sprintf(keyword, "TTYPE%d", col);
    return get(keyword);
}


/*
 * Get the contents of the given column as an array of doubles.
 * The caller should pass an array of numValues doubles.
 */
int FitsIO::getTableColumn(int col, double* values, int numValues)
{
    if (!fitsio_)
	return error(noFitsErrMsg);

    int status = 0, anynull = 0;
    if (fits_read_col(fitsio_, TDOUBLE, col, 1, 1, numValues, NULL, 
		      values, &anynull, &status) != 0)
	return cfitsio_error();

    return 0;
}


/*
 * Return the value in the current FITS table at the given row 
 * and column, or NULL if there was an error.
 * The returned pointer points to static storage and will be overwritten
 * on the next call to this method or the get(keyword) methods.
 */
char* FitsIO::getTableValue(long row, int col)
{
    if (!fitsio_) {
	error(noFitsErrMsg);
	return NULL;
    }

    buf_[0] = '\0';
    int status = 0, typecode = 0, anynulls = 0;
    long repeat = 0, width = 0;

    if (fits_get_coltype(fitsio_, col, &typecode, &repeat, &width, &status) != 0) {
	cfitsio_error();
	return NULL;
    }
    
    if (width > sizeof(buf_)-1) {
	fmt_error("FITS table value at row %d, col %d is too long", row, col);
	return NULL;
    }

    switch(typecode) {
    case TSTRING:
	char* p[1];
	p[0] = buf_;
	if (fits_read_col(fitsio_, TSTRING, col, row, 1, 1, (char *)"", 
			  p, &anynulls, &status) != 0) {
	    cfitsio_error();
	    return NULL;
	}
	break;

    case TBYTE:
    case TSHORT:
    case TINT:
    case TLONG:
	long l;
	if (fits_read_col(fitsio_, TLONG, col, row, 1, 1, NULL, 
			  &l, &anynulls, &status) != 0) {
	    cfitsio_error();
	    return NULL;
	}
	sprintf(buf_, "%ld", l);
	break;

    case TUSHORT:
    case TUINT:
    case TULONG:
	unsigned long ul;
	if (fits_read_col(fitsio_, TULONG, col, row, 1, 1, NULL, 
			  &ul, &anynulls, &status) != 0) {
	    cfitsio_error();
	    return NULL;
	}
	sprintf(buf_, "%lu", ul);
	break;

    case TFLOAT:
    case TDOUBLE:
	double d;
	if (fits_read_col(fitsio_, TDOUBLE, col, row, 1, 1, NULL, 
			  &d, &anynulls, &status) != 0) {
	    cfitsio_error();
	    return NULL;
	}
	sprintf(buf_, "%lg", d);
	break;

    case TLOGICAL:
	char c;
	if (fits_read_col(fitsio_, TLOGICAL, col, row, 1, 1, NULL, 
			  &c, &anynulls, &status) != 0) {
	    cfitsio_error();
	    return NULL;
	}
	buf_[0] = (c ? 'T' : 'F');
	buf_[1] = '\0';
	break;

    default:
	fmt_error("cfitsio data type (%d) not supported");
	return NULL;
    }

    return buf_;
}

 
/*
 * Create a FITS table and make it the current HDU
 *
 * extname gives the name of the table.
 *
 * The initial size will be rows x cols entries.
 *
 * tform is an array giving the FITS data type for each column
 * (For example: 16A, for a 16 char string, see FITS description.)
 *
 * If asciiFlag is 1, an ASCII table is created, otherwise a binary table.
 */
int FitsIO::createTable(const char* extname, long rows, int cols,
			char** headings, char** tform, int asciiFlag)
{
    // make sure the file was mapped with write permission
    if (checkWritable() != 0) 
	return 1;		// error

    int status = 0;
    int tbltype = (asciiFlag ? ASCII_TBL : BINARY_TBL);

    fits_ = this;		// reallocFile() might be called
    if (fits_create_tbl(fitsio_, tbltype, rows, cols, headings,
			tform, NULL, (char*)extname, &status) != 0) {
	fits_ = NULL;
	return cfitsio_error();
    }
    if (flush() != 0) 
	return 1;		// error flushing data

    if (fits_movnam_hdu(fitsio_, tbltype, (char*)extname, 0, &status) != 0) {
	return cfitsio_error();
    }
    
    return setHDU(getHDUNum());	// remap after modifying file
}


/*
 * Set the value in the current FITS table at the given row and column
 * (For now, all data types are treated as strings)
 */
int FitsIO::setTableValue(long row, int col, const char* value)
{
    // make sure the file was mapped with write permission
    if (checkWritable() != 0) 
	return 1;		// error

    if (row < 1)
	return fmt_error("FITS table row index %d out of range: should be >= 1", row);
    if (col < 1)
	return fmt_error("FITS table column index %d out of range: should be >= 1", col);

    // make sure the file was mapped with write permission
    if (checkWritable() != 0) 
	return 1;		// error

    int status = 0, typecode = 0;
    long repeat = 0, width = 0;

    if (fits_get_coltype(fitsio_, col, &typecode, &repeat, &width, &status) != 0) 
	return cfitsio_error();
    
    switch(typecode) {
    case TSTRING:
	if (fits_write_col(fitsio_, TSTRING, col, row, 1, 1, (char**)&value, &status) != 0) 
	    return cfitsio_error();
	break;

    case TBYTE:
    case TSHORT:
    case TINT:
    case TLONG:
	long l;
	if (sscanf(value, "%ld", &l) != 1) 
	    return error("invalid int value: ", value);
	if (fits_write_col(fitsio_, TLONG, col, row, 1, 1, &l, &status) != 0) 
	    return cfitsio_error();
	break;

    case TUSHORT:
    case TUINT:
    case TULONG:
	unsigned long ul;
	if (sscanf(value, "%lu", &l) != 1) 
	    return error("invalid unsigned value: ", value);
	if (fits_write_col(fitsio_, TULONG, col, row, 1, 1, &l, &status) != 0) 
	    return cfitsio_error();
	break;

    case TFLOAT:
    case TDOUBLE:
	double d;
	if (sscanf(value, "%lf", &d) != 1) 
	    return error("invalid floating point value: ", value);
	if (fits_write_col(fitsio_, TDOUBLE, col, row, 1, 1, &d, &status) != 0) 
	    return cfitsio_error();
	break;

    case TLOGICAL:
	char c;
	if (fits_write_col(fitsio_, TLOGICAL, col, row, 1, 1, (char*)value, &status) != 0) 
	    return cfitsio_error();
	break;

    default:
	return fmt_error("cfitsio data type (%d) not supported");
    }

    return flush();
}


/*
 * Make sure that this object represents a writable FITS file that was
 * also mapped with read/write permission. The default for FitsIO::read()
 * is to map the file read-only. If you plan to edit the file by
 * inserting keywords or tables, you must pass the read-write flag, for
 * example:
 *
 *    FitsIO* fits = FitsIO::read(filename, Mem::FILE_RDWR);
 *
 * If this was not the case, this method attempts to remap the file read/write.
 *
 * The return value is 0 if the file was or could be mapped read-write, 
 * otherwise 1.
 */
int FitsIO::checkWritable()
{
    if (!fitsio_) 
	return error(noFitsErrMsg);

    if (checkFitsFile() != 0)
	return 1;
    
    if (!(header_.options() & Mem::FILE_RDWR)) {
	if (access(header_.filename(), W_OK) != 0)
	    return error("FitsIO: no wite permission on file: ", header_.filename());
	return header_.remap(Mem::FILE_RDWR);
    }

    return 0;
}
