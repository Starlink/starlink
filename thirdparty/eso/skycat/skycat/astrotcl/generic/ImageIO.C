/*
 * E.S.O. - VLT project / ESO Archive
 *
 * "@(#) $Id: ImageIO.C,v 1.2 2006/01/18 17:56:53 abrighto Exp $" 
 *
 * ImageIO.C - method definitions for class ImageIO, for managing image
 *             I/O and storage
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  07/03/96  Created
 *
 *                 12/03/98  Remove dependency on FitsIO (delegated to
 *                           class FitsIO or other class derived from
 *                           ImageIORep.
 * Peter W. Draper 24/06/99  Changed to use FITS_LONG as type in byte
 *                           swapping. "long" is 8 bytes on alphas and 64 SUNs.
 */
static const char* const rcsId="@(#) $Id: ImageIO.C,v 1.2 2006/01/18 17:56:53 abrighto Exp $";

/* see Apple Developer Connection Tech Notes
  http://developer.apple.com/technotes/tn2002/tn2071.html */
#if ! ( defined(__APPLE__) && defined(__MACH__) )
#include <netinet/in.h>*
#endif
#include <arpa/inet.h>
#include <cmath>
#include <cstdlib>
#if HAVE_CONFIG_H
#include "config.h"
#endif
#include "error.h"
#include "ImageIO.h"
#include "fitsio2.h"

// The type "long" may have 64 bits.
#if LONGSIZE == 64
#define FITS_LONG int 
#else 
#define FITS_LONG long 
#endif

/*
 * copy constructor - increment the reference count...
 */
ImageIO::ImageIO(const ImageIO& im) 
    : rep_(im.rep_)
{
    if (rep_) 
	rep_->refcnt_++;
}


/*
 * destructor - delete if there are no more references.
 */
ImageIO::~ImageIO() 
{
    if (rep_ && --rep_->refcnt_ <= 0) 
	delete rep_;
}


/*
 * assignment operator
 */
ImageIO& ImageIO::operator=(const ImageIO& im)
{
    if (im.rep_)
	im.rep_->refcnt_++;		// protect against "im = im"
    if (rep_ && --rep_->refcnt_ <= 0) 
	delete rep_;
    rep_ = im.rep_;
    return *this;
}


// -----------------------------------------------------------------------
// ImageIORep: base class of internal representation
// -----------------------------------------------------------------------

/*
 * replace header
 */
int ImageIORep::header(const Mem& m)
{
    header_ = m;
    return 0;
}

    
/*
 * replace data with data of same size
 */
int ImageIORep::data(const Mem& m)
{
    if (m.length() < width_* height_ * (abs(bitpix_)/8))
	return error("image memory area is too small");
    data_ = m;
    return 0;
}


/*
 * If byte swapping is needed for this machine and image, make a byte
 * swapped copy of the image data, otherwise, do nothing.
 * Returns 0 if all is OK.
 */
int ImageIORep::byteSwapData() 
{
    int dsize = abs(bitpix_)/8;
    FITS_LONG l = 1;
    if (ntohl(l) == l || dsize == 1) {
	// no byte swapping needed
	return 0;
    }
    
    // make a byte-swapped copy of the image in memory. 
    // Note: if this causes problems with huge images, maybe we should
    // make a byte swapped file copy and mmap it.
    int n = width_ * height_;
    int datalen = n * dsize;
    Mem data(datalen, 0);
    if (data.status() != 0)
	return 1;

    // copy the data and swap bytes
    if (dsize == 2) {
	// copy shorts (could be an odd number of them...)
	unsigned short* from = (unsigned short*)data_.ptr();
	unsigned short* to = (unsigned short*)data.ptr(); 
	while(n--) {
	    *to++ = ntohs(*from);
	    // note: ntohs could be a macro that references its arg more than once...
	    from++; 
	}
    }
    else if (dsize == 4) {
	// copy longs
	unsigned FITS_LONG* from = (unsigned FITS_LONG*)data_.ptr();
	unsigned FITS_LONG* to = (unsigned FITS_LONG*)data.ptr(); 
 	while(n--) {
	    *to++ = ntohl(*from);
	    from++;
	}
    }
    else {
	return fmt_error("ImageIO: unexpected value for bitpix: %d", bitpix_);
    }
    
    // replace the image data with the byte swapped data
    // (This deletes the old data Mem object and replaces it with the new one)
    data_ = data;
    return 0;
}

