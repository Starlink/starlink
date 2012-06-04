/*+
 *  Name:
 *     StarWCS

 *  Language:
 *     C++

 *  Purpose:
 *     Defines the members of the StarWCS class

 *  Authors:
 *     P.W. Draper (PWD)
 *     Allan Brighton, ESO (ALLAN)

 *  Copyright:
 *     Copyright (C) 1997-1999 Central Laboratory of the Research Councils
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     Copyright (C) 2007-2009 Science and Technology Facilities Council.
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

 *  History:
 *     23-JUL-1997 (PWD):
 *        Original version. Created to replace WCS with a layer
 *        based on AST.
 *     17-OCT-1997 (PWD):
 *        Added changes to allow situation when axis 1 isn't the
 *        RA axis (previews from CADC HST archive sometimes have
 *        these reversed!).
 *     9-JAN-1998 (PWD):
 *        Removed checks for pix2wcs and wcs2pix out of bounds. It
 *        can be useful for these to succeed at times (i.e. when
 *        fitting new WCS systems that are initially inaccurate).
 *    16-MAR-1998 (ALLAN)
 *        Renamed local class WCSRep to StarWCS and removed local class WCS,
 *        to be compatible with the main Rtd/Skycat release.
 *        Now we define a subclass of the original rtd WCSRep that defines the
 *        new behavior and uses the Starlink routines.
 *    17-MAR-1998 (ALLAN)
 *        Added pix2wcsDist, for compat with base class
 *    22-APR-98 (ALLAN)
 *        Fixed xSecPix() and ySecPix() methods, added setSecPix() to note
 *        the values for later access.
 *     8-MAR-1998 (PWD):
 *        Moved astNorm calls to be before any corrections for RA/Dec
 *        reversal.
 *    24-JUN-1998 (PWD):
 *        Increased digits of display to 8 (from a default of 7). This
 *        is inline with vanilla RTD.
 *    13-JAN-1999 (PWD):
 *        Merged in Allan's changes (see history above).
 *    19-NOV-1999 (PWD):
 *        Added test for sky coordinates and members to query this.
 *    15-SEP-2000 (PWD):
 *        Fixed ::set member so that it works as advertised.
 *     7-DEC-2000 (PWD):
 *        Added methods to return a list of domains and to set an AST
 *        attribute.
 *    22-JAN-2003 (PWD)
 *        Added methods to support extra precision in output. When
 *        enabled this is supposed to show milli-arcsec resolution.
 *    10-JUN-2003 (PWD):
 *        Reworked make2D for case when there are more than 2
 *        dimensions. Should work better with NDF sections which have
 *        insignificant dimensions (in the base frame).
 *    16-NOV-2005 (PWD):
 *        Added deltset from 2.7.4.
 *    19-DEC-2005 (PWD):
 *        Implemented projection and xRefPix and yRefPix. These are now
 *        used in Skycat.
 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include "define.h" //  from tclutil

#include <cstring>
#include <cstdlib>
#include <cmath>
#include <cfloat>
#include <iostream>
#include <sstream>
#include "error.h"
#include "StarWCS.h"
#include "gaiaUtils.h"

//  Trig conversion factors.
static const double pi_ = 3.14159265358979323846;
static const double r2d_ = 57.295779513082323;   // (180.0/pi_)
static const double d2r_ = 0.017453292519943295; // (pi_/180.0);

//  Number of characters in a FITS header card.
static const int FITSCARD = 80;

//  Initialize static members.
int StarWCS::carlin_ = 1;
int StarWCS::forceDegrees_ = 0;

//
//  Define a function to wrap the AST TranN function using a
//  C-binding. This function doesn't like being called from C++ using
//  the DEC CXX command (v6). Note we also need "-std arm" for this to
//  work (to get past the ast.h include file)...
//
extern "C" {
    static void WCSAstTranN( AstFrameSet *map, int npoint, int ncoord_in,
                             int indim, const double (*in)[1], int forward,
                             int ncoord_out, int outdim, double (*out)[1] )
    {
        astTranN( map, npoint, ncoord_in, indim, (const double *)in,
                  forward, ncoord_out, outdim, (double *)out );
    }
}

//
//  Utility routines for formating FITS cards.
//
static void ccard( char *card, const char *keyword, char const *value )
{
    char buf[FITSCARD+1];
    sprintf(buf, "%-8.8s= '%.21s' /", keyword, value);
    sprintf(card, "%-80s", buf);
}
static void icard( char *card, const char *keyword, int value )
{
    char buf[FITSCARD+1];
    sprintf(buf, "%-8.8s= %20d /", keyword, value);
    sprintf(card, "%-80s", buf);
}
static void dcard(char *card, const char *keyword, double value)
{
    char  buf[FITSCARD+1];
    sprintf(buf, "%-8.8s= %20f /", keyword, value);
    sprintf(card, "%-80s", buf);
}

//
//  Constructor.
//
//  The channelData pointer will be stored in the FitsChannel
//  and can be retrieved using a call to astChannelData, if the
//  tabSource function is called. That function should handle
//  access to -TAB tables that define a look up table for the
//  WCS.
//
StarWCS::StarWCS( const char *header, const size_t lheader,
                  void *channelData,
                  void (* tabSource)( AstFitsChan *, const char *,
                                      int, int, int * ) )
  : wcs_(NULL),
    equinox_(0.0),
    extraPrecision_(0),
    xrefpix_(0.0),
    yrefpix_(0.0),
    raIndex_(1),
    decIndex_(2),
    xSecPix_(0.0),
    ySecPix_(0.0),
    issky_(1),
    warnings_(NULL)
{
    equinoxStr_[0] = '\0';
    projection_[0] = '\0';

    // If any errors from previous states are active then cancel them.
    if ( ! astOK ) astClearStatus;

    if ( header ) {
        if ( lheader > 1 ) {

            // Have a character buffer which can be read in as a AST object.
            // This should be a FITS header which we need to read it in
            // through a FITS channel.
            AstFitsChan *fitschan = astFitsChan( NULL, NULL, " " );

            // If given register to read -TAB tables.
            if ( tabSource != NULL ) {
                astSetI( fitschan, "TabOK", 1 );
                astTableSource( fitschan, tabSource );

                //  Store user pointer to context information.
                if ( channelData != NULL ) {
                    astPutChannelData( fitschan, channelData );
                }
            }

            int ncard = (int) lheader / FITSCARD;
            gaiaUtilsGtFitsChan( (char *) header, ncard, &fitschan );

            // Look for the image dimensions and store these if found. We
            // need these for calculating the size of the image in world
            // coordinates and AST doesn't retain this information.
            ndims_ = 2;
            astClear( fitschan, "Card" );
            char card[FITSCARD+1];
            char *ptr;
            if ( astFindFits( fitschan, "NAXIS", card, 1 ) ) {
                if ( ( ptr = strstr( card, "=" ) ) != (char *)NULL ) {
                    sscanf( ++ptr, "%d", &ndims_ );
                }
            }
            int i = 0;
            for ( i = 0; i < MAXDIM; i++ ) {
                dims_[i] = 1;
            }
            astClear( fitschan, "Card" );
            i = 0;
            while( astFindFits( fitschan, "NAXIS%d", card, 1 ) ) {
                if ( ( ptr = strstr( card, "=" ) ) != (char *)NULL ) {
                    sscanf( ++ptr, "%d", & dims_[i++] );
                }
            }
            nxpix_ = dims_[0];
            nypix_ = dims_[1];

            // Record axis rotation.
            rotate_ = 0.0;
            astClear( fitschan, "Card" );
            if ( astFindFits( fitschan, "CROTA1", card, 1) ) {
                if ( ( ptr = strstr( card, "=" ) ) != (char *)NULL ) {
                    float value;
                    sscanf( ++ptr, "%g", &value );
                    rotate_ = (double) value;
                }
            }

            // Record projection from CTYPE1. If not CTYPE1 value then it's
            // UNKNOWN (probably no WCS or AST native).
            astClear( fitschan, "Card" );
            strcpy( projection_, "UNKNOWN" );
            if ( astFindFits( fitschan, "CTYPE1", card, 1) ) {
                if ( ( ptr = strstr( card, "RA---" ) ) != (char *)NULL ||
                     ( ptr = strstr( card, "DEC--" ) ) != (char *)NULL ) {
                    ptr += 5;
                    strncpy( projection_, ptr, 3 );
                    projection_[3] = '\0';
                }
            }

            // Record CRPIX1 and CRPIX2. Set to 0.0 if not found.
            astClear( fitschan, "Card" );
            xrefpix_ = 0.0;
            yrefpix_ = 0.0;
            if ( astFindFits( fitschan, "CRPIX1", card, 1) ) {
                if ( ( ptr = strstr( card, "=" ) ) != (char *)NULL ) {
                    float value;
                    sscanf( ++ptr, "%g", &value );
                    xrefpix_ = (double) value;
                }
                if ( astFindFits( fitschan, "CRPIX2", card, 1) ) {
                    if ( ( ptr = strstr( card, "=" ) ) != (char *)NULL ) {
                        float value;
                        sscanf( ++ptr, "%g", &value );
                        yrefpix_ = (double) value;
                    }
                }
            }
            else {
                //  Reference pixel is centre of image.
                xrefpix_ = nxpix_ / 2;
                yrefpix_ = nypix_ / 2;
            }

            //  CAR projections are sometimes incorrect and what is
            //  required in a linear transformation. This is
            //  controlled by the static member carlin_.
            astSetI( fitschan, "CarLin", carlin_ );

            //  Establish which error conditions we'd like to see mentioned
            //  in the ASTWARN cards. These should be shown to the user when
            //  convenient.
            astSet( fitschan, "Warnings=%s", astGetC(fitschan, "AllWarnings"));

            //   If the encoding is FITS-anything, then we effectively
            //   default to FK5/J2000 (really ICRS) when values like
            //   RADESYS etc. are not defined. In 2009 this is probably
            //   more likely to be true.
            char const *default_encoding = astGetC( fitschan, "Encoding" );
            if ( strncmp( "FITS-", default_encoding, 5 ) == 0 ) {
                astSet( fitschan, "DefB1950=0" );
            }

            // Now try to read in the FITS headers to create a frameset
            // (this contains frames for the image pixels and how to map
            // to the world coordinate systems available).
            astClear( fitschan, "Card" );
            AstFrameSet *fitsset = (AstFrameSet *) astRead( fitschan );
            if ( fitsset == AST__NULL ) {
                astClearStatus;
            }
            else {
                // Read a WCS from ther FITS headers. It may have
                // more than two dimensions. In this case we need to
                // select out. If this fails then use nothing.
                wcs_ = fitsset;
                if ( !make2D() ) {
                    astClearStatus;
                    wcs_ = (AstFrameSet *) astAnnul( wcs_ );
                    print_error( "Failed to read a 2D World Coordinate System"
                                 " from FITS headers");
                }
                else {
                    initCelestial();
                }
            }

            //  Construct status message and check for any warning
            //  cards.
            astClear( fitschan, "Card" );
            constructWarning( default_encoding, wcs_ == NULL, fitschan );

            //  Release the fitschan.
            fitschan = (AstFitsChan *) astAnnul( fitschan );
        }
    }
}

//
// Destructor
//
StarWCS::~StarWCS()
{
    if ( wcs_ ) {
        wcs_ = (AstFrameSet *) astAnnul( wcs_ );
    }
    if ( warnings_ ) {
        delete[] warnings_;
        warnings_ = NULL;
    }
}

//
// Local initialisations when coordinate system is celestial (equinox,
// RA-Dec axes, pixel scale).
//
void StarWCS::initCelestial()
{
    //  Initialisations for all coordinate systems.
    raIndex_ = 1;
    decIndex_ = 2;
    equinoxStr_[0] = '\0';
    xSecPix_ = 0.0;
    ySecPix_ = 0.0;

    //  If celestial need additional information about equinox, which
    //  axes are RA and Dec and the image scales.
    setCelestial();
    if ( issky_ ) {
        setEquinox();
        int astime2 = astGetI( wcs_, "astime(2)" );
        if ( astime2 ) {
            raIndex_  = 2;
            decIndex_ = 1;
        }
        setSecPix();
    }
}

//
// Replace the current WCS FrameSet with one given (this is so that
// the current WCS can be copied and modified without destroying this
// version, it can then be used to replace this one if the
// modifications are accepted, or re-replaced etc., see astWCSCopy
// also). Note FrameSet is cloned, so further modifications of it will
// be seen immediately. When the control of AST is established then
// if suitable primitives are located to replace this open door then
// they should be implemented instead (and this member becomes private).
//
int StarWCS::astWCSReplace( AstFrameSet *newwcs )
{
    if ( astIsAFrameSet( newwcs ) ) {
        AstFrameSet *astcopy = (AstFrameSet *) wcs_;
        wcs_ = (AstFrameSet *) astClone( newwcs );
        if ( !make2D() ) {
            astClearStatus;
            wcs_ = (AstFrameSet *) astAnnul( wcs_ );
            wcs_ = astcopy;
            error( "Failed to read a 2D World Coordinate System from FITS headers");
            return 0;
        }
        else {

            initCelestial();

            // Release the old WCS.
            if ( astcopy != NULL ) {
                astcopy = (AstFrameSet *) astAnnul( astcopy );
            }
        }
    }
    else {

        //  Not a valid FrameSet
        if ( !astOK ) astClearStatus;
        error( "not a valid WCS system" );
        return 0;
    }
    if ( !astOK ) astClearStatus;
    return 1;
}


//
//   Utility method to note the values for xSecPix_ and ySecPix_
//   (number of arcsecs per pixel) for later access. (allan: 22.4.98)
//
void StarWCS::setSecPix()
{
    if ( !isWcs() ) {
        xSecPix_ = 0.0;
        ySecPix_ = 0.0;
    }

    double point1[2], point2[2];
    double xin[2], yin[2], xout[2], yout[2];
    double dist;
    double xcen, ycen;

    //  Compute the scales the the sizes of a pixel near the centre of
    //  the image.
    xcen = 0.5 * ( (double) nxpix_ );
    ycen = 0.5 * ( (double) nypix_ );
    xin[0] = xcen - 0.5;
    xin[1] = xcen + 0.5;
    yin[0] = yin[1] = ycen;

    // Transform these image positions into sky coordinates.
    astTran2( wcs_, 2, xin, yin, 1, xout, yout );

    // And now get the distance between these positions in degrees.
    point1[0] = xout[0];
    point1[1] = yout[0];
    point2[0] = xout[1];
    point2[1] = yout[1];

    dist = astDistance( wcs_, point1, point2 );
    if ( ! astOK ) astClearStatus;
    if ( dist == AST__BAD ) {
        xSecPix_ = 0.0;
    }
    else {
        xSecPix_ = dist * r2d_ * 3600.0;
    }

    //  Same procedure for Y.
    xin[0] = xin[1] = xcen;
    yin[0] = ycen - 0.5;
    yin[1] = ycen + 0.5;

    // Transform these image positions into sky coordinates.
    astTran2( wcs_, 2, xin, yin, 1, xout, yout );

    // And now get the distance between these positions in degrees.
    point1[0] = xout[0];
    point1[1] = yout[0];
    point2[0] = xout[1];
    point2[1] = yout[1];

    dist = astDistance( wcs_, point1, point2 );
    if ( ! astOK ) astClearStatus;
    if ( dist == AST__BAD ) {
        ySecPix_ = 0.0;
    }
    else {
        ySecPix_ = dist * r2d_ * 3600.0;
    }
}

//
//  Utility method to set the equinox value and its character representation.
//
void StarWCS::setEquinox()
{
    if ( wcs_ == NULL ) {
        return;
    }
    equinoxStr_[0] = '\0';

    //  Make sure equinox has a valid value.
    equinox_ = astGetD( wcs_, "Equinox" );
    if ( ! astOK ) astClearStatus;
    const char *system = astGetC( wcs_, "System" );
    if ( astOK && system ) {

        //  Make sure system should have an equinox associated with it.
        if ( strncmp( "FK", system, 2 ) == 0 ||
             strcmp( "ECLIPTIC", system ) == 0 ) {

            //  Get a string version of the equinox to display.
            if ( equinox_ == 2000.0 ) {
                strcpy( equinoxStr_, "J2000" );
            }
            else if ( equinox_ == 1950.0 ) {
                strcpy( equinoxStr_, "B1950" );
            }
            else {
                sprintf( equinoxStr_, "%g %s", equinox_, system );
                if ( ! astOK ) astClearStatus;
            }
        }
        else if ( strncmp( "ICRS", system, 4 ) == 0 ) {
            //  ICRS doesn't have an equinox, but we can pretend it does to a
            //  high level of precision and just assume this is
            //  FK5/J2000. This simplies transformations etc. that Skycat
            //  supports.
            strcpy( equinoxStr_, "J2000" );
        }
    }
    if ( ! astOK ) astClearStatus;

    //  Set the number of digits for displaying coordinates. This is
    //  done once here, rather than every time a coordinate is
    //  requested. The HMS class has a print that effects much of the
    //  catalogues outputs, so we toggle that too for consistency.
    if ( extraPrecision_ ) {
        astSet( wcs_, "digits(%d) = 11", raIndex_ );
        if ( ! astOK ) astClearStatus;
        astSet( wcs_, "digits(%d) = 11", decIndex_ );
        if ( ! astOK ) astClearStatus;
        HMS::extra_precision = 1;
    }
    else {
        astSet( wcs_, "digits(%d) = 9", raIndex_ );
        if ( ! astOK ) astClearStatus;
        astSet( wcs_, "digits(%d) = 9", decIndex_ );
        if ( ! astOK ) astClearStatus;
        HMS::extra_precision = 0;
    }
}

//
// Set the display of extra precision.
//
void StarWCS::extraPrecision( int value )
{
   if ( extraPrecision_ != value ) {
      extraPrecision_ = value;
      setEquinox();
   }
}

//
//  Return the epoch.
//
double StarWCS::epoch() const
{
    double value = 0.0;
    if ( wcs_ ) {
        value = astGetD( wcs_, "Epoch" );
        if ( ! astOK ) astClearStatus;
    }
    return value;
}

//
//  Convert the given x,y image coordinates to world coordinates, if
//  possible, and write the result to the given buffer as a list of
//  the form "RA DEC EQUINOX".
//  If no conversion can be done, buf will contain an empty list.
//  This (compatibility) version requires that the x,y position is on
//  the image.
//
//  If hms_flag is 1, the result is always in H:M:S D:M:S, otherwise
//  the result is returned in decimal degrees, unless the use of
//  degrees is forced.
//
char* StarWCS::pix2wcs( double x, double y, char* buf, int bufsz,
                        int hms_flag ) const
{
    pix2wcs( x, y, 0, buf, bufsz, hms_flag );
    return buf;
}

//
//  Convert the given x,y image coordinates to world coordinates, if
//  possible, and write the result to the given buffer as a list of
//  the form "RA DEC EQUINOX".
//  If no conversion can be done, buf will contain an empty list.
//  This version allows a return if the result doesn't lie on the image.
//
//  If hms_flag is 1, the result is always in H:M:S D:M:S, otherwise
//  the result is returned in decimal degrees, unless the use of
//  degrees is forced.
//
//  The behaviour with hms_flag may not be the same after moving to AST.
//
char* StarWCS::pix2wcs( double x, double y, int notbound, char* buf,
                        int bufsz, int hms_flag ) const
{
    if ( StarWCS::forceDegrees_ ) {
        hms_flag = 0;
    }

    buf[0] = '\0';
    int onimage = ( x >= 0 && y >= 0 &&
                    x <= ( nxpix_ + 1 ) && y <= ( nypix_ + 1 ) );
    if ( isWcs() && ( onimage || notbound ) ) {
        double newx[1], newy[1], oldx[1], oldy[1];
        double point[2];
        oldx[0] = x;
        oldy[0] = y;
        astTran2( wcs_, 1, oldx, oldy, 1, newx, newy );

        //  Normalize the result into the correct range.
        point[0] = newx[0];
        point[1] = newy[0];
        astNorm( wcs_, point );
        double ra, dec;
        if ( raIndex_ == 1 ) {
            ra = point[0];
            dec = point[1];
        }
        else {
            dec = point[0];
            ra = point[1];
        }
        if ( astOK ) {
            if ( hms_flag ) {
                const char *rastr = astFormat( wcs_, raIndex_, ra );
                const char *decstr = astFormat( wcs_, decIndex_, dec );
                if ( rastr && decstr ) {
                    sprintf (buf, "%s %s %s", rastr, decstr, equinoxStr_);
                }
            }
            else {

                // If hms_flag is not set then return the result in degrees.
                sprintf (buf, "%.12g %.12g %s", ra * r2d_, dec * r2d_, equinoxStr_);
            }
        }
        if ( !astOK ) astClearStatus;
    }
    return buf;
}


//
//  Convert the given x,y image coordinates to world coordinates, if
//  possible, and write the results to the arguments ra and dec as doubles
//  in degrees.  If no conversion can be done, ra and dec are set to 0.0
//  and 1 is returned, otherwise 0 is returned.
///
int StarWCS::pix2wcs(double x, double y, double& ra, double& dec) const
{
    if ( !isWcs() ) {
        return error("image does not support world coords");
    }

    //    if (x <= 0 || y <= 0 || x > nxpix_ || y > nypix_)
    //	return error("coordinates out of range");

    // note: start at origin = (1,1) rather than (0,0)
    ra = dec = 0.0;
    double newx[1], newy[1], oldx[1], oldy[1];
    oldx[0] = x;
    oldy[0] = y;
    astTran2( wcs_, 1, oldx, oldy, 1, newx, newy );
    double point[2];
    point[0] = newx[0];
    point[1] = newy[0];
    astNorm( wcs_, point );
    if ( ! astOK ) {
        astClearStatus;
        return error("can't convert world coordinates: out of range");
    }
    else {

        // Return values are in degrees and swapped if necessary.
        if ( raIndex_ == 1 ) {
            ra = point[0] * r2d_;
            dec = point[1] * r2d_;
        }
        else {
            dec = point[0] * r2d_;
            ra = point[1] * r2d_;
        }
    }
    return 0;
}


//
//  Convert the given world coordinates (ra and dec, in degrees) to x,y
//  image coordinates and put the results in x and y.
///
int StarWCS::wcs2pix(double ra, double dec, double &x, double &y) const
{
    x = y = 0.0;

    if ( !isWcs() ) {
        return error("image does not support world coords");
    }

    double oldx[1], oldy[1], newx[1], newy[1];
    if ( raIndex_ == 1 ) {
        oldx[0] = ra * d2r_;  // Convert into radians.
        oldy[0] = dec * d2r_;
    }
    else {
        oldy[0] = ra * d2r_;
        oldx[0] = dec * d2r_;
    }
    astTran2( wcs_, 1, oldx, oldy, 0, newx, newy );
    if ( ! astOK ) {
        astClearStatus;
        return error("can't convert world coords");
    }
    else {
        x = newx[0];
        y = newy[0];

        // Check return values are not "offscale" (this is an emulation
        // of the previous behaviour of this method and may not exactly
        // correspond). -- switched off PWD 9/1/98
        //  if (x <= 0 || y <= 0 || x > nxpix_ || y > nypix_)
        //	return error("coordinates out of range");
    }
    return 0;
}


//
//  Convert the given world coordinates to x,y image coordinates and
//  put the results in x and y. The input coordinates are assumed to
//  be in the correct units, unless the current frame is celestial,
//  and notcelestial is false, in which case they are assumed to be in
//  degrees.
//

int StarWCS::anyWcs2pix( double inx, double iny, int notcelestial,
                         double &outx, double &outy ) const
{
    outx = outy = 0.0;

    if ( !isWcs() ) {
        return error( "image does not support world coords" );
    }

    if ( issky_ && ! notcelestial ) {
        return wcs2pix( inx, iny, outx, outy );
    }

    double oldx[1], oldy[1], newx[1], newy[1];
    oldx[0] = inx;
    oldy[0] = iny;
    astTran2( wcs_, 1, oldx, oldy, 0, newx, newy );
    if ( ! astOK ) {
        astClearStatus;
        return error( "can't convert world coords" );
    }
    else {
        outx = newx[0];
        outy = newy[0];
    }
    return 0;
}

//+
// convert the given image coordinates distance (x,y) to a world coordinates
// distance (ra and dec, in degrees J2000) and put the results in ra and dec.
//-
int StarWCS::pix2wcsDist(double x, double y, double& ra, double& dec) const
{
    double xDegPix = xSecPix() / 3600.;
    double yDegPix = ySecPix() / 3600.;
    if ( xDegPix == 0.0 || yDegPix == 0.0 ) {
        return error("can't convert image to world coordinate distance");
    }
    ra = fabs( x * xDegPix );
    dec = fabs( y * yDegPix );
    return 0;
}

//
//  Convert the given world coordinates distance (ra and dec, in degrees)
//  to an x,y image coordinates distance and put the results in x and y.
//
int StarWCS::wcs2pixDist(double ra, double dec, double &x, double &y) const
{
    if ( !isWcs() ) {
        return 0;
    }

    //  Method is to get a scale factor for x,y to ra,dec at the image
    //  origin and then use these values to scale the ra and decs.
    //  This is similar to the original (somewhat vague) treatment which
    //  was based on the CRDEL values.
    double xin[2], yin[2], xout[2], yout[2];
    double delta_ra, delta_dec;
    double point1[2], point2[2];

    // Transform a step of one pixel along X axis and then get the
    // equivalent distance in world coordinates.
    if ( raIndex_ == 1 ) {
        xin[0] = 0.0;
        xin[1] = 1.0;
        yin[0] = 0.0;
        yin[1] = 0.0;
    }
    else {
        xin[0] = 0.0;
        xin[1] = 0.0;
        yin[0] = 0.0;
        yin[1] = 1.0;
    }
    astTran2( wcs_, 2, xin, yin, 1, xout, yout );
    point1[0] = xout[0];
    point1[1] = yout[0];
    point2[0] = xout[1];
    point2[1] = yout[1];
    delta_ra = astDistance( wcs_, point1, point2 );

    // Now same for Y axis.
    if ( raIndex_ == 1 ) {
        xin[0] = 0.0;
        xin[1] = 0.0;
        yin[0] = 0.0;
        yin[1] = 1.0;
    }
    else {
        xin[0] = 0.0;
        xin[1] = 1.0;
        yin[0] = 0.0;
        yin[1] = 0.0;
    }
    astTran2( wcs_, 2, xin, yin, 1, xout, yout );
    point1[0] = xout[0];
    point1[1] = yout[0];
    point2[0] = xout[1];
    point2[1] = yout[1];
    delta_dec = astDistance( wcs_, point1, point2 );

    if ( delta_dec == AST__BAD || delta_ra == AST__BAD ) {
        if ( !astOK ) astClearStatus;
        return error ( "cannot convert world coordinates to distance" );
    }
    x = fabs( ra / ( delta_ra * r2d_ ) );
    y = fabs( dec / ( delta_dec * r2d_ ) );
    if ( !astOK ) astClearStatus;
    return 0;
}

//
//  Return the distance between two positions in world coordinates.
//
double StarWCS::dist(double ra0, double dec0, double ra1, double dec1) const
{
    if ( !isWcs() ) {
        return 0.0;
    }

    double point1[2], point2[2];
    if ( raIndex_ == 1 ) {
        point1[0] = ra0 * d2r_, point2[0] = ra1 * d2r_;
        point1[1] = dec0 * d2r_, point2[1] = dec1 * d2r_;
    }
    else {
        point1[1] = ra0 * d2r_, point2[1] = ra1 * d2r_;
        point1[0] = dec0 * d2r_, point2[0] = dec1 * d2r_;
    }
    double dist = astDistance( wcs_, point1, point2 );
    if ( ! astOK ) astClearStatus;
    if ( dist == AST__BAD ) {
        return 0.0;
    }
    return dist * r2d_;
}

//
//  Return the width of the image in world coordinate arc-minutes
//
double StarWCS::width() const
{
    if ( !isWcs() ) {
        return 0.0;
    }

    double point1[2], point2[2];
    double xin[2], yin[2], xout[2], yout[2];
    double dist1, dist2, dist;

    // Compute image width in two parts, 1->nxpix_/2, nxpix_/2->nxpix_ along
    // the centre of the image. This tries to avoid problems when the image
    // edges are at the same point on the sky (all-sky images).
    yin[0] = yin[1] = 0.5 * (double) nypix_;

    xin[0] = 1.0;
    xin[1] = (double) nxpix_/2;
    astTran2( wcs_, 2, xin, yin, 1, xout, yout );

    point1[0] = xout[0];
    point1[1] = yout[0];
    point2[0] = xout[1];
    point2[1] = yout[1];

    dist1 = astDistance( wcs_, point1, point2 );
    if ( ! astOK ) astClearStatus;
    if ( dist1 == AST__BAD ) {
        return 0.0;
    }

    xin[0] = (double) nxpix_/2;
    xin[1] = (double) nxpix_;
    astTran2( wcs_, 2, xin, yin, 1, xout, yout );

    point1[0] = xout[0];
    point1[1] = yout[0];
    point2[0] = xout[1];
    point2[1] = yout[1];

    dist2 = astDistance( wcs_, point1, point2 );
    if ( ! astOK ) astClearStatus;
    if ( dist2 == AST__BAD ) {
        return 0.0;
    }

    dist = dist1 + dist2;

    //  Check that distance isn't 0, this indicates that edge of image
    //  is same coordinate (and above didn't help). If so use arcsec per pixel
    //  estimate.
    if ( dist == 0.0 || dist < DBL_EPSILON ) {
        dist = xSecPix_ * nxpix_;
    }
    else {
        dist *= 60.0 * r2d_;
    }
    return dist;
}

//
//  Return the height of the image in world coordinate arc-minutes
//
double StarWCS::height() const
{
    if ( !isWcs() ) {
        return 0.0;
    }

    double point1[2], point2[2];
    double xin[2], yin[2], xout[2], yout[2];
    double dist1, dist2, dist;

    // Compute image width in two parts, 1->nypix_/2, nypix_/2->nypix_ along
    // the centre of the image. This tries to avoid problems when the image
    // edges are at the same point on the sky (all-sky images).
    xin[0] = xin[1] = 0.5 * (double) nxpix_;

    yin[0] = 1.0;
    yin[1] = (double) nypix_/2;
    astTran2( wcs_, 2, xin, yin, 1, xout, yout );

    point1[0] = xout[0];
    point1[1] = yout[0];
    point2[0] = xout[1];
    point2[1] = yout[1];

    dist1 = astDistance( wcs_, point1, point2 );
    if ( ! astOK ) astClearStatus;
    if ( dist1 == AST__BAD ) {
        return 0.0;
    }

    yin[0] = (double) nypix_/2;
    yin[0] = (double) nypix_;
    astTran2( wcs_, 2, xin, yin, 1, xout, yout );

    point1[0] = xout[0];
    point1[1] = yout[0];
    point2[0] = xout[1];
    point2[1] = yout[1];

    dist2 = astDistance( wcs_, point1, point2 );
    if ( ! astOK ) astClearStatus;
    if ( dist2 == AST__BAD ) {
        return 0.0;
    }

    dist = dist1 + dist2;

    //  Check that distance isn't 0, this indicates that edge of image
    //  is same coordinate (and above didn't help). If so use arcsec per pixel
    //  estimate.
    if ( dist == 0.0 || dist < DBL_EPSILON ) {
        dist = ySecPix_ * nypix_;
    }
    else {
        dist *= 60.0 * r2d_;
    }
    return dist;
}


//
//  Return the radius of the image in world coordinate arc-minutes
//  (the distance from the center of the image to the origin)
//
double StarWCS::radius() const
{
    if ( !isWcs() ) {
        return 0.0;
    }

    double point1[2], point2[2];
    double xin[2], yin[2], xout[2], yout[2];
    double dist;

    xin[0] = 0.0;
    xin[1] = 0.5 * (double) nxpix_;
    yin[0] = 0.0;
    yin[1] = 0.5 * (double) nypix_;

    // Transform these image positions into sky coordinates.
    astTran2( wcs_, 2, xin, yin, 1, xout, yout );

    // And now get the distance between these positions in radians.
    point1[0] = xout[0];
    point1[1] = yout[0];
    point2[0] = xout[1];
    point2[1] = yout[1];

    dist = astDistance( wcs_, point1, point2 );
    if ( ! astOK ) astClearStatus;

    if ( dist == AST__BAD ) {
        return 0.0;
    }

    //  Check that distance isn't 0, this indicates that edge of image
    //  is same coordinate as centre! If so use arcsec per pixel
    //  estimates.
    if ( dist == 0.0 || dist < DBL_EPSILON ) {
        dist = sqrt ( 0.25 * xSecPix_ * nxpix_ * xSecPix_ * nxpix_
                      + 0.25 * ySecPix_ * nypix_ * ySecPix_ * nypix_ );

    }
    else {
        dist *= 60.0 * r2d_;
    }

    // The above is sensitive to sky-sized images (which have a maximum radius
    // of pi/2, when we want 2pi), so try some other guesses and pick the
    // largest, within reason (width/height can blow up at poles).
    double hwidth = width() * 0.5;
    double hheight = height() * 0.5;
    double maxdist = dist * 3.0;
    if ( hwidth < maxdist ) {
        dist = max( hwidth, dist );
    }
    if ( hheight < maxdist ) {
        dist = max( hheight, dist );
    }
    return dist;
}

//
//  Create a frameset for the WCS mapping from information supplied.
//  Note this mimics a FITS-WCS system.
//
//  Args:
//      ra      = Center right ascension in degrees
//      dec     = Center declination in degrees
//      secpix  = Number of arcseconds per pixel
//      xrefpix = Reference pixel X coordinate
//      yrefpix = Reference pixel Y coordinate
//      nxpix   = Number of pixels along x-axis
//      nypix   = Number of pixels along y-axis
//      rotate  = Rotation angle (clockwise positive) in degrees
//      equinox = Equinox of coordinates, 1950 and 2000 supported
//      epoch   = Epoch of coordinates, used for FK4/FK5 conversion no effect if 0
//      proj    = Projection
//
int StarWCS::set( double ra, double dec,
                  double secpix,
                  double xrefpix, double yrefpix,
                  int nxpix, int nypix,
                  double rotate,
                  int equinox, double epoch,
                  const char *proj )
{
    if ( wcs_ ) {
        wcs_ = (AstFrameSet *) astAnnul( wcs_ );
    }

    //  Create a FITS channel to which we will send our header cards.
    AstFitsChan *fitschan = astFitsChan( NULL, NULL, " " );
    char card[FITSCARD+1];
    icard( card, "NAXIS", 2 );
    astPutFits( fitschan, card, 0 );

    icard( card, "NAXIS1", nxpix );
    astPutFits( fitschan, card, 0 );
    nxpix_ = nxpix;

    icard( card, "NAXIS2", nypix );
    astPutFits( fitschan, card, 0 );
    nypix_ = nypix;

    icard( card, "EQUINOX", equinox );
    astPutFits( fitschan, card, 0 );

    if ( epoch != 0 ) {
        dcard( card, "EPOCH", epoch );
        astPutFits( fitschan, card, 0 );
    }

    //  Don't use the default "UNKNOWN", this is probably just being returned
    //  from a query.
    char buf[20];
    if ( strncmp( proj, "UNKNOWN", 7 ) != 0 ) {
        sprintf( buf, "RA---%s", proj );
        ccard( card, "CTYPE1", buf );
        astPutFits( fitschan, card, 0 );

        sprintf( buf, "DEC--%s", proj );
        ccard( card, "CTYPE2", buf );
        astPutFits( fitschan, card, 0 );
    }

    dcard( card, "CRVAL1", ra );
    astPutFits( fitschan, card, 0 );

    dcard( card, "CRVAL2", dec );
    astPutFits( fitschan, card, 0 );

    dcard( card, "CDELT1", -secpix / 3600.0 );
    astPutFits( fitschan, card, 0 );

    dcard( card, "CDELT2", secpix / 3600.0 );
    astPutFits( fitschan, card, 0 );

    dcard( card, "CRPIX1", xrefpix );
    astPutFits( fitschan, card, 0 );

    dcard( card, "CRPIX2", yrefpix );
    astPutFits( fitschan, card, 0 );

    dcard( card, "CROTA1", rotate );
    rotate_ = rotate;
    astPutFits( fitschan, card, 0 );

    //  Now read the headers back as a suitable frameset.
    astClear( fitschan, "Card" );
    AstFrameSet *fitsset = (AstFrameSet *) astRead( fitschan );
    if ( fitsset != AST__NULL ) {
        wcs_ = fitsset;
    }
    else {
        if ( ! astOK ) astClearStatus;
        fitschan = (AstFitsChan *) astAnnul( fitschan );
        return error("Cannot locate a valid world coordinate system");
    }
    fitschan = (AstFitsChan *) astAnnul( fitschan );
    initCelestial();
    if ( ! astOK ) astClearStatus;
    return 0;
}

//
//  Return the world coordinates of the image center
//
WorldCoords StarWCS::center() const
{
    double ra, dec;
    double x = (double) nxpix_/2.0, y = (double) nypix_/2.0;
    pix2wcs(x, y, ra, dec);
    return WorldCoords(ra, dec, equinox());
}

//
//  Return the value of an AST attribute.
//
const char *StarWCS::astGetAttrib( char *attrib )
{
    if ( ! wcs_ ) {
        return (char *)NULL;
    }
    const char *result = astGetC( wcs_, attrib );
    if ( !astOK ) {
        astClearStatus;
        return (char *)NULL;
    }
    return result;
}

//
//  Set an AST attribute. Name and value supplied as characters.
//
//  Success (1) or failure (0) returned as result.
//
int StarWCS::astSetAttrib( const char *what, const char *value )
{
    if ( ! wcs_ ) {
        return 0;
    }

    //  Check for a change in coordinates, we need to update when that
    //  occurs.
    int oldcurrent = astGetI( wcs_, "Current" );

    astSetC( wcs_, what, value );

    int newcurrent = astGetI( wcs_, "Current" );
    if ( oldcurrent != newcurrent ) {
        initCelestial();
    }
    if ( !astOK ) {
        astClearStatus;
        return 0;
    }
    return 1;
}

//+
//  Name:
//    make2D
//
//  Purpose:
//     Ensures that a frameset has only two dimensions in the current
//     and base frames.
//
//  Description:
//     This function checks the base (which should be a pixel frame of
//     some kind) and current (which should be a skyframe of somekind)
//     to see if they have only two dimensions. If so then nothing is
//     done, otherwise the first two significant dimensions of the base
//     frame set selected and mapped into a new base frame and/or a
//     new skyframe is mapped.
//-

int StarWCS::make2D()
{
    double zero[MAXDIM];
    int i;
    int inperm[MAXDIM];
    int outperm[MAXDIM];

    for ( i = 0; i < MAXDIM; i++ ) {
        zero[i] = 0.0;
    }

    // Find out how many dimensions the current and base frames have.
    AstFrame *baseframe = (AstFrame *) astGetFrame( wcs_, AST__BASE );
    AstFrame *skyframe = (AstFrame *) astGetFrame( wcs_, AST__CURRENT );
    int nbase = astGetI( baseframe, "Naxes" );
    int nsky = astGetI( skyframe, "Naxes" );
    int ibase = astGetI( wcs_, "Base" );
    int isky = astGetI( wcs_, "Current" );

    if ( nbase == 2 && nsky == 2 ) {
        baseframe = (AstFrame *) astAnnul( baseframe );
        skyframe = (AstFrame *) astAnnul( skyframe );
        return 1;
    }

    // Add idents to the base and current frames, so we know which ones they
    // where.
    astSet( skyframe, "Ident=ORIGSKY" );
    astSet( baseframe, "Ident=ORIGBASE" );

    if ( nsky < 2 ) {

        // Current frame has too few axes. Add in a dummy one to make it 2D.
        outperm[0] = 1;
        outperm[1] = 0;
        AstFrame* newFrame =
            (AstFrame *) astPickAxes( skyframe, 2, outperm, NULL );

        // Need a permmap that looses the second dimension.
        inperm[0] = 1;
        inperm[1] = -1;
        outperm[0] = 1;
        outperm[1] = -1;
        AstMapping *map =
            (AstMapping *) astPermMap( 1, inperm, 2, outperm, zero, " " );

        astAddFrame( wcs_, nsky, map, newFrame );

        nsky = 2;
        astAnnul( skyframe );
        skyframe = (AstFrame *) astGetFrame( wcs_, AST__CURRENT );
        isky = astGetI( wcs_, "Current" );

        astAnnul( map );
        astAnnul( newFrame );
    }

    if ( nbase > MAXDIM || nsky > MAXDIM ) {
        baseframe = (AstFrame *) astAnnul( baseframe );
        skyframe = (AstFrame *) astAnnul( skyframe );
        error( "Input WCS has two many dimensions" );
        return 0;
    }

    // Add the necessary frames to make the base frame 2D. For these we need
    // to pick out the significant dimensions. Only two are allowed.
    int sigaxes = 0;
    for ( i = 0; i < MAXDIM; i++ ) {
        outperm[i] = 0;                  /* nbase can be greater than ndims_ */
        inperm[i] = -1;                  /* need initialisation for all axis */
    }
    for ( i = 0; i < ndims_; i++ ) {
        if ( dims_[i] > 1 && sigaxes < 2 ) {
            inperm[i] = sigaxes + 1;
            outperm[sigaxes] = i + 1;
            sigaxes++;
            if ( sigaxes == 1 ) {
                nxpix_ = dims_[i];
            }
            if ( sigaxes == 2 ) {
                nypix_ = dims_[i];
            }
        }
    }
    if ( sigaxes < 2 ) {
        // Just pick out first two, as we don't seem to have enough
        // significant axes. Could be a spectrum.
        outperm[0] = 1;
        outperm[1] = 2;
        inperm[0] = 1;
        inperm[1] = 2;
        nxpix_ = dims_[0];
        nypix_ = dims_[1];
        if ( nbase < 2 ) {
            inperm[1] = -1;
            outperm[1] = 0;
            nypix_ = 1;
        }
    }

    AstFrame *newframe =
        (AstFrame *) astPickAxes( baseframe, 2, outperm, NULL );

    // Create a mapping for this permutation that doesn't have <bad>
    // values as the result.
    AstMapping *map =
        (AstMapping *) astPermMap( nbase, inperm, 2, outperm, zero, " " );

    // Now add this frame to the FrameSet and make it the base
    // one. Also reinstate the skyframe as the current frame.
    astAddFrame( wcs_, ibase, map, newframe );
    int iframe = astGetI( wcs_, "Current" );
    astSetI( wcs_, "Base", iframe );
    astSetI( wcs_, "Current", isky );
    newframe = (AstFrame *) astAnnul( newframe );
    map = (AstMapping *) astAnnul( map );

    // Now deal with skyframe. In an attempt to make sure we pick the
    // correct axes that correspond to those chosen for the image we
    // try a transformation to see which axes are jiggled. Note this
    // takes two goes as any other axes can be fixed at a given value
    // (and will be returned as this, say a constant frequency for the
    // whole image) so we need a genuine movement on the image to
    // detect the correct axes.
    double in1[2][1];
    double out1[MAXDIM][1];
    in1[0][0] = 1.0;
    in1[1][0] = 1.0;
    for ( i = 0; i < MAXDIM; i++ ) out1[i][0] = 0.0;
    WCSAstTranN( wcs_, 1, 2, 1, in1, 1, nsky, 1, out1 );

    double in2[2][1];
    double out2[MAXDIM][1];
    in2[0][0] = (double) (nxpix_-1);
    in2[1][0] = (double) (nypix_-1);
    for ( i = 0; i < MAXDIM; i++ ) out2[i][0] = 0.0;
    WCSAstTranN( wcs_, 1, 2, 1, in2, 1, nsky, 1, out2 );

    //  Check to see which dimensions have jiggled.
    int n = 0;
    for ( i = 0; i < nsky; i++ ) {
        if ( out1[i][0] != AST__BAD &&  out2[i][0] != AST__BAD ) {
            if ( fabs( out1[i][0] - out2[i][0] ) > DBL_EPSILON ) {
                n++;
            }
        }
    }
    if ( ! astOK ) astClearStatus;

    if ( n != 2 ) {

        // Either too many signficant dimensions or our attempt to
        // pick out significant axes has failed.
        int naxes = 0;

        // Let's just try to pick out a skyframe, (only a skyframe should have
        // the AsTime attribute and it should only have 2 dimensions) failing
        // this use the first two axes.
        char astime[10];
        for ( i = 1; i <= nsky; i++ ) {
            sprintf( astime, "AsTime(%d)", i );
            if ( astGetC( wcs_, astime ) ) {
                naxes++;
                out1[i-1][0] = 1.0;
                out2[i-1][0] = 3.0;
                if ( naxes == 2 ) {
                    break;
                }
            }
            else {
                astClearStatus;
            }
        }
        if ( naxes != 2 ) {
            //  Use first two axes.
            for ( i = 0; i < nsky; i++ ) {
                out1[i][0] = 0.0;
                out2[i][0] = 0.0;
            }
            out1[0][0] = 1.0;
            out2[0][0] = 2.0;
            out1[1][0] = 1.0;
            out2[1][0] = 2.0;
        }
    }

    //  Choose the selected axes from the skyframe (should be just 2 by
    //  now).
    n = 0;
    int axes[MAXDIM];
    for ( i = 0; i < nsky; i++ ) {
        if ( fabs( out1[i][0] - out2[i][0] ) > DBL_EPSILON ) {
            axes[n++] = i + 1;
        }
    }

    //  And extract them. Use the ATL routine as this also does some
    //  work to make sure that any ROIs are picked up and Ident'd. That's
    //  important for grid plotting.
    int lbnd[2];
    int ubnd[2];
    lbnd[0] = 1;
    lbnd[1] = 1;
    ubnd[0] = nxpix_;
    ubnd[1] = nypix_;
    double *work = new double[max(nxpix_,nypix_)*2+1];
    char *error_mess;
    if ( gaiaUtilsAtlAxTrm( wcs_, axes, lbnd, ubnd, work, &error_mess ) == 0 ) {
        astSetStatus( 1 );     /* Trip a !astOK */
        free( error_mess );
    }
    delete[] work;

    //  Release local frames.
    baseframe = (AstFrame *) astAnnul( baseframe );
    skyframe = (AstFrame *) astAnnul( skyframe );

    // If the above went well then assume we're in the clear, otherwise
    // indicate an error.
    if ( !astOK ) {
        return 0;
    }
    else {
        return 1;
    }
}

//+
//  Reset the center of the WCS structure
//
//  Args:
//      ra        = New center right ascension in degrees
//      dec       = New center declination in degrees
//      equinox   = (must be 2000 or 1950)
//-
int StarWCS::shift(double ra, double dec, double equinox)
{
    // Does nothing, I guess this should setup a new Frame in some way
    // that transforms the current system and is then added to the
    // frameset as the current frame? What is is centre in this case
    // (reference pixel? -- actually this is assumed to be the centre
    // of the image.).
    cerr << "WCS::shift, this function is not implemented -- sorry."
         << std::endl;
    return 0;
}

//+
//  Set rotation and scaling.
//
int StarWCS::deltset( double cdelt1, double cdelt2, double rotation )
{
    //  No meaning for AST WCS.
    cerr << "WCS::deltset, this function is not implemented -- sorry."
         << std::endl;
    return 0;
}

//
//  Return the distance between two positions in world coordinates,
//  assuming input units are correct and no conversions are required.
//
double StarWCS::plaindist(double x0, double y0, double x1, double y1) const
{
    if ( !isWcs() ) {
        return 0.0;
    }

    double point1[2], point2[2];
    point1[0] = x0;
    point2[0] = x1;
    point1[1] = y0;
    point2[1] = y1;

    double dist = astDistance( wcs_, point1, point2 );

    if ( ! astOK ) astClearStatus;
    if ( dist == AST__BAD ) {
        return 0.0;
    }
    return dist;
}

//
//  Check the WCS to see if current coordinates are a celestial
//  coordinate system.
//
void StarWCS::setCelestial()
{
    AstFrame *frame = (AstFrame *) astGetFrame( wcs_, AST__CURRENT );
    issky_ = astIsASkyFrame( frame );
    frame = (AstFrame *) astAnnul( frame );
}

//
//  Construct a single warning message about the status of the WCS
//  construction process and include any warnings about it from a
//  fitschan. This may be used to report any problem with the FITS
//  headers that AST has identified. The encoding of the original FITS
//  channel is always reported. It also checks if one of the cards
//  reports the absence of equinox and resets the equinox to blank.
//
void StarWCS::constructWarning( const char *encoding, int failed,
                                AstFitsChan *fitschan )
{
    //  Release previous warnings.
    if ( warnings_ ) {
        delete warnings_;
        warnings_ = NULL;
    }

    //  Set up variables and a stream to write to.
    char card[FITSCARD+1];
    char *equinox;
    int nwarns = 0;
    std::ostringstream os;

    //  Add the encoding message.
    if ( encoding ) {
        os << "Result of attempt to read WCS encoded as: " << encoding
           << std::endl;
        os << std::endl;
    }
    if ( failed ) {
        os << "Failed, look for error messages on the terminal" << std::endl;
        os << std::endl;
    }
    else if ( fitschan ) {
        os << "Succeeded" << std::endl;
        os << std::endl;

        //  Construct warning card message like:
        //     ASTWARN = 'The message'
        //  or
        //     ASTWARN = '           '
        //  for empty cards. We just concatenate these together.
        astClear( fitschan, "Card" );
        while ( astFindFits( fitschan, "ASTWARN", card, 1 ) ) {

            //  See if this is a report about the equinox. If so don't
            //  show a valid one.
            equinox = strstr( card, "equinox" );
            if ( equinox != NULL ) {
                equinoxStr_[0] = '\0';
            }
            nwarns++;
            os << card << std::endl;
        }
        astClear( fitschan, "Card" );
    }
    if ( nwarns > 0 || encoding || failed ) {
        std::string istring = os.str();  /* Keep reference in scope so memory
                                          * is not freed immediately as in
                                          * os.str().c_str(). */
        const char *str = istring.c_str();

        //  os falls out of scope so we need a copy.
        warnings_ = new char[ strlen(str) + 1 ];
        strcpy( warnings_, str );
    }
}

//
//  Get access to any ASTWARN cards that have been produced during
//  construction. Note caller shouldn't delete this string.
//
const char *StarWCS::getWarning()
{
    if ( warnings_ ) {
        return warnings_;
    }
    else {
        return NULL;
    }
}

//
//  Get a list of the domains available in the frameset. The returned
//  string should be freed by the caller. If dimens is true then pairs of
//  values are returned that include the number of dimensions that the domains
//  have.
//
char *StarWCS::getDomains( int dimens )
{
    if ( ! wcs_ ) {
        return NULL;
    }

    // Record the indices of the current frame (this is changed to get
    // each domainname).
    int icur = astGetI( wcs_, "Current" );

    int insert = 0;
    const int chunk = 128;
    char *namelist = (char *) malloc( (size_t) chunk );
    namelist[0] = '\0';
    int length = chunk;
    char dval[6];
    char sdomain[chunk];
    char *domain;

    int nframe = astGetI( wcs_, "nframe" );
    int newlength;
    for ( int i = 1; i <= nframe; i++ ) {
        astSetI( wcs_, "Current", i );
        domain = (char *) astGetC( wcs_, "Domain" );

        newlength = (int) strlen( domain );
        if ( newlength == 0 ) { // No domain name, so make one up.
            domain = sdomain;
            sprintf( domain, "Domain%d", i );
        }

        if ( dimens ) {
            newlength += 6;
            sprintf( dval, "%d", astGetI( wcs_, "Naxes" ) );
        }

        if ( insert + newlength > length ) {
            length += max( chunk, newlength + 1 );
            namelist = (char *) realloc( namelist, length );
        }

        strcat( namelist, " " );
        strcat( namelist, domain );
        insert += newlength + 1;
        if ( dimens ) {
            strcat( namelist, " " );
            strcat( namelist, dval );
            insert += 6;
        }
    }

    //  Restore original current frame and return list of names.
    astSetI( wcs_, "Current", icur );
    if ( !astOK ) {
        astClearStatus;
    }
    return namelist;
}

//
//  Convert the given x,y image coordinates to world coordinates of any
//  dimensionality. The wcs array must be greater or equal to the expected
//  dimensionality which is returned as ndim (usually MAXDIM).
///
int StarWCS::pix2wcs( double x, double y, double wcs[], int &ndim ) const
{
    if ( !isWcs() ) {
        return error( "image does not support world coordinates" );
    }

    // Dimensionality of the current domain.
    ndim = astGetI( wcs_, "Naxes" );

    // Transform the position.
    double in[2][1];
    double out[MAXDIM][1];
    in[0][0] = x;
    in[1][0] = y;

    WCSAstTranN( wcs_, 1, 2, 1, in, 1, ndim, 1, out );
    for ( int i = 0; i < ndim; i++ ) {
        wcs[i] = out[i][0];
    }

    // Normalise this (for RA and Dec).
    astNorm( wcs_, wcs );

    if ( ! astOK ) {
        astClearStatus;
        return error( "can't convert world coordinates: out of range" );
    }
    return 0;
}
