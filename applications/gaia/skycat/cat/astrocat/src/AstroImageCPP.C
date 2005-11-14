/*
 * E.S.O. - VLT project/ESO Archive
 * $Id$
 *
 * AstroImage.C - method definitions for class AstroImage
 *
 * ------------------------------------------------------------------
 * NOTE: This class is obsolete, please use the AstroCatalog class
 *       instead.
 * ------------------------------------------------------------------
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 */
static const char* const rcsId="@(#) $Id$";


#include <sys/types.h>
#include <sys/stat.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <cstdio>
#include <unistd.h>
#include <cstring>
#include "error.h"
#include "Compress.hxx"
#include "AstroImage.h"



/*
 * constructor - used internally only, public interface uses "open(name)" 
 */
AstroImage::AstroImage(CatalogInfoEntry* e) 
    : entry_(e),
      status_(OK)
{
    // set default temp file for holding images
    char buf[32];
    sprintf(buf, "/tmp/img%d.fits", (int)getpid());
    tmpfile_ = strdup(buf);
}

/*
 * destructor - close catalog and free any resources
 */
AstroImage::~AstroImage() 
{
    unlink(tmpfile_);
    if (tmpfile_)
	free(tmpfile_);
}


/*
 * copy constructor
 */
AstroImage::AstroImage(const AstroImage& a) 
{
    tmpfile_ = a.tmpfile_ ? strdup(a.tmpfile_) : (char*)NULL;
}

/*
 * set the name of the temp file used to hold the image
 */
void AstroImage::tmpfile(const char* name) 
{
    if (tmpfile_) 
	free(tmpfile_);  
    tmpfile_ = strdup(name);
}


/*
 * open the named image server and return a pointer to an AstroImage
 * object for it, or NULL if errors occur
 */
AstroImage* AstroImage::open(const char* name) 
{
    // get the entry for this image server type
    CatalogInfoEntry* e = CatalogInfo::lookup(name);
    if (!e) 
	return NULL;

    if (strcmp(e->servType(), "imagesvr") == 0) {
	return new AstroImage(e);
    }

    fmt_error("'%s' is of type '%s', and not 'imagesvr' as required here", 
	      name, e->servType());
    return NULL;
}


/*
 * Request an image from the image server and return 0 if all is ok.
 * The name of a FITS file containing the resulting image can be accessed as
 * this->tmpfile().
 *
 * The catalog config file defines the URL used to get the image. The URL
 * may contain variables to be expanded:
 *
 * %ra, %dec - replaced with the ra,dec world coords of the center position
 *
 * %x, %y    - replaced with the x,y image coords of the center position
 *
 * %w, %h    - replaced with the width and heith arguments in arcmin for
 *             world coords or pixels for image coords.
 *
 * Note each catalog supports either world coords or image coords, but not
 * both.
 *
 */
int AstroImage::getImage(const WorldOrImageCoords& pos, double width, double height)
{
    if (pos.isNull() || width <= 0 || height <= 0) {
	return error("must set position, width and height for image request");
    } 

    // if the first URL doesn't work, try the others, if specified
    const char* urls[3];
    urls[0] = entry_->url();
    urls[1] = entry_->backup1();
    urls[2] = entry_->backup2();

    // for each url, backup-url, etc...
    for (int i = 0; i < 3 && urls[i]; i++) {

	// generate the http url command
	std::ostringstream os;

	// expand the variables in the http server command
	const char* p = urls[i];
	while(*p) {
	    if (*p == '%') {
		p++;
		if (strncmp(p, "ra", 2) == 0) {
		    os << pos.ra();
		    p += 2;
		}
		else if (strncmp(p, "dec", 3) == 0) {
		    // include plus sign, if needed
		    os << pos.dec();
		    p += 3;
		}
		else if (*p == 'x') {
		    os << pos.x();
		    p++;
		}
		else if (*p == 'y') {
		    os << pos.y();
		    p++;
		}
		else if (*p == 'w') {
		    os << width;
		    p++;
		}
		else if (*p == 'h') {
		    os << height;
		    p++;
		}
		else if (strncmp(p, "mime-type", 9) == 0) {
		    // os << "image/x-gfits"; // ?should be hard coded in the config file
		    os << "application/x-fits"; // ?should be hard coded in the config file
		    p += 9;
		}
	    }
	    else {
		os << *p++;
	    }
	}

	if (getImage(os.str().c_str()) == 0)
	    return 0;

	// don't go to backup URL if it was a request for authorization
	if (http_.authorizationRequired())
	    return 1;
    }
    return 1;			// error
}

 
/*
 * Given a URL for the image, request the image from the image server and
 * return 0 if all is ok. The name of the FITS file containing the
 * resulting image can be accessed with the method "this->tmpfile()"
 */
int AstroImage::getImage(const char* url)
{
    // open the tmp file
    std::ofstream f(tmpfile_);
    if (!f) {
	return error("could not open file for writing", tmpfile_);
    }
	
    if (http_.get(url, f) != 0)
	return ERROR;
    f.close();

    // check the Content-type of the return image to determine whether it
    // needs to be decompressed and if so, how...
    char* ctype = http_.content_type();
    if (!ctype)
	ctype = "";

    // if the Content-type is not recognized...
    if (strncmp(ctype, "image/", 6) != 0) {
	// check if it might still be a FITS file:
	std::ifstream is(tmpfile_);
	char buf[81];
	if (is && is.get(buf, 80) && strncmp(buf, "SIMPLE", 6) == 0)
	    return 0;
	
	// if not a FITS file, try to interpret as a HTML error
	is.seekg(0);
	return http_.html_error(is);
    }
    char* t = ctype+6;

    // In some cases the Content-type only gives the general type and
    // we need to check the Content-Encoding also. For example "file.fits.gz"
    // might have a Content-type of image/x-fits and Content-Encoding of
    // x-gzip
    char* ce = http_.content_encoding();
    if (strcmp(t, "x-fits") == 0 && ce != NULL) {
	if (strcmp(ce, "x-gzip") == 0)
	    t = "x-gfits";
 	else if (strcmp(ce, "x-compress") == 0)
	    t = "x-cfits";
    }

    if (strcmp(t, "x-fits") == 0) { // Pure FITS file
	return 0;	// not compressed, just return filename
    }

    Compress::CompressType type = Compress::NO_COMPRESS;
    if (strcmp(t, "x-hfits") == 0) { // Hcompressed FITS file
	type = Compress::H_COMPRESS;
    }
    else if (strcmp(t, "x-gfits") == 0) { // GZIPed FITS file
	type = Compress::GZIP_COMPRESS;
    }
    else if (strcmp(t, "x-cfits") == 0) { // Compressed FITS file (UNIX)
	type = Compress::UNIX_COMPRESS;
    }
    else if (strcmp(t, "x-sfits") == 0) { // Compressed FITS file (Stark)
	// type = Compress::S_COMPRESS;
	return error("x-sfits compression (Stark) not supported");
    }
    else {
	return error("unknown image Content-type: ", ctype);
    }

    // do the decompression
    FILE* feedback = http_.feedback();
    if (feedback) {
	fprintf(feedback, "decompressing image...\n");
	fflush(feedback);
    }
    
    Compress c;
    if (c.decompress(tmpfile_, type) != 0) {
	return ERROR;
    }

    // if we got here, then we have the FITS file
    return 0;
}
