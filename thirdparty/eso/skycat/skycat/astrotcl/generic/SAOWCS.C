/*
 * E.S.O. - VLT project / ESO Archive
 *
 * "@(#) $Id: SAOWCS.C,v 1.8 1998/11/16 21:23:08 abrighto Exp $" 
 *
 * SAOWCS.C - method definitions for class SAOWCS, an implementation
 *            of the abstract WCS (WCSRep) class interface based on 
 *            Doug Mink's (saoimage) WCS library.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  30 Sep 96  Created
 *
 *                 09 Jan 98  Fixed problem with [xy]SecPix_ values for
 *                            some (HST) images (affects distance conversion 
 *                            from WCS to pixel)
 *                 17 Mar 98  Renamed from WSCRep, made WCSRep abstract,
 *                            to allow new implementations based on other
 *                            libraries.
 */
static const char* const rcsId="@(#) $Id: SAOWCS.C,v 1.8 1998/11/16 21:23:08 abrighto Exp $";


#include <string.h>
#include <stdlib.h>
#include "error.h"
#include "SAOWCS.h"


/*
 * constructor: make an SAOWCS object from the FITS header string
 * and the length of the header string.
 */
SAOWCS::SAOWCS(const char* header, int headerLength)
    : WCSRep(),
      wcs_(NULL),
      equinox_(0.0),
      ra_(0.0),
      dec_(0.0),
      width_(0.0),
      height_(0.0),
      xSecPix_(0.0),
      ySecPix_(0.0)
{

    equinoxStr_[0] = '\0';
    if (header && headerLength) {
	// The wcssubs/hget routines depend on a static variable being set by hlength()...
	// (The header string is probably not null terminated, since mmap'ed)
	hlength((char*)header, headerLength);
	wcs_ = wcsninit(header, headerLength);
	if (isWcs()) {
	    // get image center and dimensions
	    wcsfull(wcs_, &ra_, &dec_, &width_, &height_);
	    xSecPix_ = width_*3600./pixWidth();
	    ySecPix_ = height_*3600./pixHeight();

	    // set the equinox value and string
	    setEquinox();
	}
    }
}


/*
 * destructor
 */
SAOWCS::~SAOWCS()
{
    if (wcs_)
	free(wcs_);
}


/*
 * util method to set the equinox value and string
 */
void SAOWCS::setEquinox()
{
    // make sure equinox has a valid value
    equinox_ = wcs_->equinox;
    strcpy(equinoxStr_, wcs_->radecout);
}


/*
 * convert the given x,y image coordinates to world coordinates, if
 * possible, and write the result to the given buffer as a list of
 * the form "RA DEC EQUINOX".  
 * If no conversion can be done, buf will contain an empty list.
 *
 * If hms_flag is 1, the result is always in H:M:S D:M:S, otherwise the
 * format is the one used in the pix2wcst routine (H:M:S for equatorial,
 * otherwise degrees).
 */
char* SAOWCS::pix2wcs(double x, double y, char* buf, int bufsz, int hms_flag) const
{
    buf[0] = '\0';
    if (isWcs() && x > 0 && y > 0 && x < pixWidth() && y < pixHeight()) {
	if (hms_flag == 0) {
	    ::pix2wcst(wcs_, x, y, buf, bufsz);
	}
	else {
	    double ra, dec;
	    ::pix2wcs(wcs_, x, y, &ra, &dec);
	    if (!wcs_->offscl) {
		char rastr[16], decstr[16];
		::ra2str(rastr, sizeof(rastr), ra, 3);
		::dec2str(decstr, sizeof(decstr), dec, 2);
		sprintf (buf, "%s %s %s", rastr, decstr, equinoxStr_);
	    }
	}
    }
    return buf;
}


/*
 * convert the given x,y image coordinates to world coordinates, if
 * possible, and write the results to the arguments ra and dec as doubles
 * in degrees.  If no conversion can be done, ra and dec are set to 0.0
 * and 1 is returned, otherwise 0 is returned.
 */
int SAOWCS::pix2wcs(double x, double y, double& ra, double& dec) const
{
    if (!isWcs()) 
	return error("image does not support world coords");

    if (x <= 0 || y <= 0 || x > pixWidth() || y > pixHeight()) 
	return error("coordinates out of range");

    // note: start at origin = (1,1) rather than (0,0)
    ra = dec = 0.0;
    ::pix2wcs(wcs_, x, y, &ra, &dec);
   
    if (wcs_->offscl)
	return error("can't convert world coordinates: out of range");
    return 0;
}


/*
 * convert the given world coordinates (ra and dec, in degrees) to x,y
 * image coordinates and put the results in x and y.
 */
int SAOWCS::wcs2pix(double ra, double dec, double &x, double &y) const
{
    x = y = 0.0;

    if (!isWcs())  
	return error("image does not support world coords");

    int	offscl = 0;		// set to 1 if offscale
    ::wcs2pix(wcs_, ra, dec, &x, &y, &offscl);

    if (offscl)
	return error("can't convert world coords: off scale");
    return 0;
}


/*
 * convert the given world coordinates distance (ra and dec, in degrees) 
 * to an x,y image coordinates distance and put the results in x and y.
 */
int SAOWCS::wcs2pixDist(double ra, double dec, double &x, double &y) const
{
    // get degrees per pixel
    double xDegPix = xSecPix_/3600.;
    double yDegPix = ySecPix_/3600.;
    if (xDegPix == 0. || yDegPix == 0.)
	return error("can't convert world coordinate to image distance");
    x = fabs(ra/xDegPix);
    y = fabs(dec/yDegPix);
    return 0;
}


/*
 * convert the given image coordinates distance (x,y) to a world coordinates 
 * distance (ra and dec, in degrees J2000) and put the results in ra and dec.
 */
int SAOWCS::pix2wcsDist(double x, double y, double& ra, double& dec) const
{
    double xDegPix = xSecPix_/3600.;
    double yDegPix = ySecPix_/3600.;
    if (xDegPix == 0. || yDegPix == 0.)
	return error("can't convert image to world coordinate distance");
    ra = fabs(x*xDegPix);
    dec = fabs(y*yDegPix);
    return 0;
}


/*
 * return the radius of the image in world coordinate arc-minutes
 * (the distance from the center of the image to the origin)
 */
double SAOWCS::radius() const
{
    if (!isWcs())
	return 0.0;

    // convert to WCS
    double ra0 = 0.0, dec0 = 0.0, ra1 = 0.0, dec1 = 0.0;
    ::pix2wcs(wcs_, 0, 0, &ra0, &dec0);
    ::pix2wcs(wcs_, pixWidth()/2, pixHeight()/2, &ra1, &dec1);
    return ::wcsdist(ra0, dec0, ra1, dec1) * 60;
}



/*
 * reset the center of the WCS structure
 *
 * Args:
 * 	ra        = New center right ascension in degrees 
 * 	dec       = New center declination in degrees 
 * 	equinox   = (must be 2000 or 1950)
 */
int SAOWCS::shift(double ra, double dec, double equinox)
{
    char* coorsys;
    if (equinox == 2000.)
	coorsys = "FK5";
    else if (equinox == 1950.)
	coorsys = "FK4";
    else
	return error("expected equinox of 1950 or 2000");

    ::wcsshift(wcs_, ra, dec, coorsys);
    ra_ = ra;
    dec_ = dec;

    return 0;
}


/*
 * set up the WCS structure from the given information about the image
 *
 * Args:
 * 	ra      = Center right ascension in degrees 
 * 	dec     = Center declination in degrees 
 * 	secpix  = Number of arcseconds per pixel 
 * 	xrefpix = Reference pixel X coordinate
 * 	yrefpix	= Reference pixel Y coordinate
 * 	nxpix   = Number of pixels along x-axis 
 * 	nypix   = Number of pixels along y-axis 
 * 	rotate  = Rotation angle (clockwise positive) in degrees 
 * 	equinox = Equinox of coordinates, 1950 and 2000 supported 
 * 	epoch   = Epoch of coordinates, used for FK4/FK5 conversion no effect if 0 
 * 	proj    = Projection 
 */
int SAOWCS::set(double ra, double dec, 
		double secpix, 
		double xrefpix, double yrefpix,
		int nxpix, int nypix, 
		double rotate, 
		int equinox, double epoch,
		const char* proj)
{
    if (wcs_) {
	free(wcs_);
	wcs_ = NULL;
    }
    
    wcs_ = ::wcsxinit(ra, dec, secpix, xrefpix, yrefpix, nxpix, nypix, 
		    rotate, equinox, epoch, (char*)proj);

    wcsfull(wcs_, &ra_, &dec_, &width_, &height_);
    xSecPix_ = ySecPix_ = secpix;
    setEquinox();

    return 0;
}


/*
 * return the world coordinates of the image center
 */
WorldCoords SAOWCS::center() const
{
    return WorldCoords(ra_, dec_, equinox());
}

