//+
//  Name:
//     StarWCS
//
//  Language:
//     C++
//
//  Purpose:
//     Defines the members of the StarWCS class
//
//  Authors:
//     P.W. Draper (PWD)
//     Allan Brighton, ESO (ALLAN)
//
//  Copyright:
//     Copyright (C) 1997-1998 Central Laboratory of the Research Councils
//
//  History:
//     23-JUL-1997 (PWD):
//        Original version. Created to replace WCS with a layer
//        based on AST.
//     17-OCT-1997 (PWD):
//        Added changes to allow situation when axis 1 isn't the
//        RA axis (previews from CADC HST archive sometimes have
//        these reversed!).
//     9-JAN-1998 (PWD):
//        Removed checks for pix2wcs and wcs2pix out of bounds. It 
//        can be useful for these to succeed at times (i.e. when 
//        fitting new WCS systems that are initially inaccurate).
//    16-MAR-1998 (ALLAN)
//        Renamed local class WCSRep to StarWCS and removed local class WCS, 
//        to be compatible with the main Rtd/Skycat release. 
//        Now we define a subclass of the original rtd WCSRep that defines the
//        new behavior and uses the Starlink routines.
//    17-MAR-1998 (ALLAN)
//        Added pix2wcsDist, for compat with base class
//    22-APR-98 (ALLAN)
//        Fixed xSecPix() and ySecPix() methods, added setSecPix() to note
//        the values for later access.
//     8-MAR-1998 (PWD):
//        Moved astNorm calls to be before any corrections for RA/Dec
//        reversal.
//-

#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include "error.h"
#include "StarWCS.h"


//
//  Constructor
//
StarWCS::StarWCS(const char* header)
  : wcs_(NULL),
    equinox_(0.0),
    raIndex_(1),
    decIndex_(2),
    xSecPix_(0.0),
    ySecPix_(0.0)
    
{
  equinoxStr_[0] = '\0';

  // If any errors from previous states are active then cancel them.
  if ( ! astOK ) astClearStatus;

  if (header && strlen(header)) {
    int lheader = strlen( header );
    if ( lheader ) {

      // Have a character buffer which can be read in as a AST object.
      // This should be a FITS header which we need to read it in
      // through a FITS channel.
      // XXX ??? how to deal with other sources.
      AstFitsChan *fitschan = astFitsChan( NULL, NULL, "" );
      char card[81];
      char *ptr = (char *) header;
      int ncard = lheader / 80;
      for ( int i = 0 ; i < ncard; i++, ptr += 80 ) {
        memcpy( card, (void *)ptr, (size_t) 80 );
        card[80] = '\0';

        //  Read all cards up to, but not including, the END card.
        if ( ! ( card[0] == 'E' && card[1] == 'N' && card[2] == 'D'
                 && ( card[3] == '\0' || card[3] == ' ' ) ) ) {
          astPutFits( fitschan, card, 0 );
          if ( !astOK ) {

            //  If an error occurs with a card, just continue, it's
            //  almost certainly something trivial like a formatting
            //  probelm.
            astClearStatus;
          }
        } else {
          break;
        }
      }

      // Look for the image dimensions and store these if found. We
      // need these for calculating the size of the image in world
      // coordinates and AST doesn't retain this information.
      nxpix_ = 1;
      astClear( fitschan, "Card" );
      if ( astFindFits( fitschan, "NAXIS1", card, 1) ) {
        if ( ptr = strstr( card, "=" ) )
          sscanf( ++ptr, "%d", &nxpix_ );
      }
      nypix_ = 1;
      astClear( fitschan, "Card" );
      if ( astFindFits( fitschan, "NAXIS2", card, 1) ) {
        if ( ptr = strstr( card, "=" ) )
          sscanf( ++ptr, "%d", &nypix_ );
      }

      // Record axis rotation.
      rotate_ = 0.0;
      astClear( fitschan, "Card" );
      if ( astFindFits( fitschan, "CROTA1", card, 1) ) {
        if ( ptr = strstr( card, "=" ) ) {
          float value;
          sscanf( ++ptr, "%g", &value );
          rotate_ = (double) value;
        }
      }

      // Now try to read in the FITS headers to create a frameset
      // (this contains frames for the image pixels and how to map
      // to the world coordinate systems available).
      astClear( fitschan, "Card" );
      AstFrameSet *fitsset = (AstFrameSet *) astRead( fitschan );
      fitschan = (AstFitsChan *) astAnnul( fitschan );
      if ( fitsset == AST__NULL ) {
        astClearStatus;
        print_error( "Failed to read World Coordinate System from FITS headers \n" );
      } else {

        // FITS headers may have more than two dimensions. In this
        // case we need to select out. If this fails then use
        // nothing.
        wcs_ = fitsset;
        if ( !make2D() ) {
          astClearStatus;
          wcs_ = (AstFrameSet *) astAnnul( wcs_ );
          print_error( "Failed to read a 2D World Coordinate System from FITS headers \n");
        } else {

          // Set the equinox value and string.
          setEquinox();

          // Finally work out which axes are longitude and which are
          // latitude (might be a better way to do this, note we leave
          // at defaults if neither is a time axis).
          int astime2 = astGetI( wcs_, "astime(2)" );
          if ( astime2 ) {
            raIndex_  = 2;
            decIndex_ = 1;
          }

	  // note the number of arcsecs per pixel for later access
	  setSecPix();
        }
      }
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
      error( "Failed to read a 2D World Coordinate System from FITS headers \n");
      return 0;
    } else {
      // Set the equinox value and string.
      setEquinox();

      // Release the old WCS.
      astcopy = (AstFrameSet *) astAnnul( astcopy );
      
      // Finally work out which axes are longitude and which are
      // latitude (might be a better way to do this, note we leave
      // at defaults if neither is a time axis).
      int astime2 = astGetI( wcs_, "astime(2)" );
      if ( astime2 ) {
	raIndex_  = 2;
	decIndex_ = 1;
      } else {
	raIndex_  = 1;
	decIndex_ = 2;
      }

      // note the number of arcsecs per pixel for later access
      setSecPix();

    }
  } else {

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
  // Note the WCS width and height
  xSecPix_ = width()*60./pixWidth();
  ySecPix_ = height()*60./pixHeight();
}


//
//  Utility method to set the equinox value and its character representation.
//
void StarWCS::setEquinox()
{
  equinoxStr_[0] = '\0';

  //  Make sure equinox has a valid value.
  equinox_ = astGetD( wcs_, "Equinox" );
  const char *system = astGetC( wcs_, "System" );
  int ok = 1;
  if ( ! astOK ) { 
    astClearStatus;
    ok = 0;
  } else if ( system ) {

    //  Make sure system should have an equinox associated with it.
    if ( strncmp( "FK", system, 2 ) == 0 || 
         strcmp( "ECLIPTIC", system ) == 0 ) {

      //  Get a string version of the equinox to display.
      if ( equinox_ == 2000.0 ) {
        strcpy( equinoxStr_, "J2000" );
      } else if ( equinox_ == 1950.0 ) {
        strcpy( equinoxStr_, "B1950" );
      } else {
        if ( ok ) {
          sprintf( equinoxStr_, "%g %s", equinox_, system );
          if ( ! astOK ) astClearStatus;
        }
      }
    } 
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
//
//  If hms_flag is 1, the result is always in H:M:S D:M:S, otherwise
//  the result is returned in decimal degrees.
//
//  XXX The behaviour with hms_flag may not be the same after moving to AST.
//
char* StarWCS::pix2wcs(double x, double y, char* buf, int bufsz, int hms_flag) const
{
  buf[0] = '\0';
  if (isWcs() && x > 0 && y > 0 && x < nxpix_ && y < nypix_ ) {

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
    } else {
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
      } else {

        // If hms_flag is not set then return the result in degrees.
        sprintf (buf, "%g %g %s", ra * R2D, dec * R2D, equinoxStr_);
      }
    }
  }
  if ( ! astOK ) astClearStatus;
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
    if (!isWcs())
	return error("image does not support world coords");

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
    } else {

      // Return values are in degrees and swapped if necessary.
      if ( raIndex_ == 1 ) {
        ra = point[0] * R2D;
        dec = point[1] * R2D;
      } else {
        dec = point[0] * R2D;
        ra = point[1] * R2D;
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

    if (!isWcs())
	return error("image does not support world coords");

    double oldx[1], oldy[1], newx[1], newy[1];
    if ( raIndex_ == 1 ) {
      oldx[0] = ra * D2R;  // Convert into radians.
      oldy[0] = dec * D2R;
    } else {
      oldy[0] = ra * D2R;
      oldx[0] = dec * D2R;
    }
    astTran2( wcs_, 1, oldx, oldy, 0, newx, newy );
    if ( ! astOK ) {
      astClearStatus;
      return error("can't convert world coords");
    } else {
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


//+
// convert the given image coordinates distance (x,y) to a world coordinates 
// distance (ra and dec, in degrees J2000) and put the results in ra and dec.
//-
int StarWCS::pix2wcsDist(double x, double y, double& ra, double& dec) const
{
    double xDegPix = xSecPix()/3600.;
    double yDegPix = ySecPix()/3600.;
    if (xDegPix == 0. || yDegPix == 0.)
	return error("can't convert image to world coordinate distance");
    ra = fabs(x*xDegPix);
    dec = fabs(y*yDegPix);
    return 0;
}

//
//  Convert the given world coordinates distance (ra and dec, in degrees)
//  to an x,y image coordinates distance and put the results in x and y.
//
int StarWCS::wcs2pixDist(double ra, double dec, double &x, double &y) const
{
  if (!isWcs())
    return 0;

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
  } else {
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
  } else {
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
  x = fabs( ra / ( delta_ra * R2D ) );
  y = fabs( dec / ( delta_dec * R2D ) );
  if ( !astOK ) astClearStatus;
  return 0;
}

//
//  Return the distance between two positions in world coordinates.
//
double StarWCS::dist(double ra0, double dec0, double ra1, double dec1) const
{
  if (!isWcs())
    return 0.0;

  double point1[2], point2[2];
  if ( raIndex_ == 1 ) {
    point1[0] = ra0 * D2R, point2[0] = ra1 * D2R;
    point1[1] = dec0 * D2R, point2[1] = dec1 * D2R;
  } else {
    point1[1] = ra0 * D2R, point2[1] = ra1 * D2R;
    point1[0] = dec0 * D2R, point2[0] = dec1 * D2R;
  }
  double dist = astDistance( wcs_, point1, point2 );
  if ( ! astOK ) astClearStatus;
  if ( dist == AST__BAD ) {
    return 0.0;
  }
  return dist * R2D;
}


//
//  Return the width of the image in world coordinate arc-minutes
//
double StarWCS::width() const
{
    if (!isWcs())
	return 0.0;

    double point1[2], point2[2];
    double xin[2], yin[2], xout[2], yout[2];
    double dist;

    // Compute the image width as a distance 1.0 -> nxpix_ about the
    // centre of the image, so first set up image coordinates
    // describing this position.
    xin[0] = 1.0;
    xin[1] = (double) nxpix_;
    yin[0] = yin[1] = 0.5 * (double) nypix_;

    // Transform these image positions into sky coordinates.
    astTran2( wcs_, 2, xin, yin, 1, xout, yout );

    // And now get the distance between these positions in degrees.
    if ( raIndex_ == 1 ) {
      point1[0] = xout[0];
      point1[1] = yout[0];
      point2[0] = xout[1];
      point2[1] = yout[1];
    } else {
      point1[1] = xout[0];
      point1[0] = yout[0];
      point2[1] = xout[1];
      point2[0] = yout[1];
    }
    dist = astDistance( wcs_, point1, point2 );
    if ( ! astOK ) astClearStatus;
    if ( dist == AST__BAD ) {
      return 0.0;
    }
    return dist * 60.0 * R2D;
}

//
//  Return the height of the image in world coordinate arc-minutes
//
double StarWCS::height() const
{
    if (!isWcs())
	return 0.0;

    double point1[2], point2[2];
    double xin[2], yin[2], xout[2], yout[2];
    double dist;

    // Compute the image height as a distance 1.0 -> nypix_ about the
    // centre of the image, so first set up image coordinates
    // describing this position.
    xin[0] = xin[1] = 0.5 * (double) nxpix_;
    yin[0] = 1.0;
    yin[1] = (double) nypix_;

    // Transform these image positions into sky coordinates.
    astTran2( wcs_, 2, xin, yin, 1, xout, yout );

    // And now get the distance between these positions in radians.
    if ( raIndex_ == 1 ) {
      point1[0] = xout[0];
      point1[1] = yout[0];
      point2[0] = xout[1];
      point2[1] = yout[1];
    } else {
      point1[1] = xout[0];
      point1[0] = yout[0];
      point2[1] = xout[1];
      point2[0] = yout[1];
    }
    dist = astDistance( wcs_, point1, point2 );
    if ( ! astOK ) astClearStatus;
    if ( dist == AST__BAD ) {
      return 0.0;
    }
    return dist * 60.0 * R2D;
}


//
//  Return the radius of the image in world coordinate arc-minutes
//  (the distance from the center of the image to the origin)
//
double StarWCS::radius() const
{
    if (!isWcs())
	return 0.0;

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
    if ( raIndex_ == 1 ) {
      point1[0] = xout[0];
      point1[1] = yout[0];
      point2[0] = xout[1];
      point2[1] = yout[1];
    } else {
      point1[1] = xout[0];
      point1[0] = yout[0];
      point2[1] = xout[1];
      point2[0] = yout[1];
    }
    dist = astDistance( wcs_, point1, point2 );
    if ( ! astOK ) astClearStatus;
    if ( dist == AST__BAD ) {
      return 0.0;
    }

    //  Return value is converted into arcminutes.
    return dist * 60.0 * R2D;
}


//
//  Create a frameset for the WCS mapping from information supplied.
//  Note this mimics a FITS-WCS system.
//
//  Args:
// 	ra      = Center right ascension in degrees
// 	dec     = Center declination in degrees
// 	secpix  = Number of arcseconds per pixel
// 	xrefpix = Reference pixel X coordinate
// 	yrefpix	= Reference pixel Y coordinate
// 	nxpix   = Number of pixels along x-axis
// 	nypix   = Number of pixels along y-axis
// 	rotate  = Rotation angle (clockwise positive) in degrees
// 	equinox = Equinox of coordinates, 1950 and 2000 supported
// 	epoch   = Epoch of coordinates, used for FK4/FK5 conversion no effect if 0
// 	proj    = Projection
//
int StarWCS::set(double ra, double dec,
		double secpix,
		double xrefpix, double yrefpix,
		int nxpix, int nypix,
		double rotate,
		int equinox, double epoch,
		const char* proj)
{
    if ( wcs_ ) {
	wcs_ = (AstFrameSet *) astAnnul( wcs_ );
    }

    //  Create a FITS channel to which we will send our header cards.
    AstFitsChan *fitschan = astFitsChan( NULL, NULL, "" );
    char card[80];
    astPutFits( fitschan, "NAXIS   = 2" , 0 );
    sprintf( card, "NAXIS1  = %d", nxpix );
    nxpix_ = nxpix;
    astPutFits( fitschan, card, 0 );
    sprintf( card, "NAXIS2  = %d", nypix );
    nypix_ = nypix;
    astPutFits( fitschan, card, 0 );
    sprintf( card, "CRVAL1  = %g", ra );
    astPutFits( fitschan, card, 0 );
    sprintf( card, "CRVAL2  = %g", dec );
    astPutFits( fitschan, card, 0 );
    sprintf( card, "CRDELT1  = %g", secpix / 3600.0 );
    astPutFits( fitschan, card, 0 );
    sprintf( card, "CRDELT2  = %g", secpix / 3600.0 );
    astPutFits( fitschan, card, 0 );
    sprintf( card, "CRPIX1   = %g", xrefpix );
    astPutFits( fitschan, card, 0 );
    sprintf( card, "CRPIX2   = %g", yrefpix );
    astPutFits( fitschan, card, 0 );
    sprintf( card, "CROTA1   = %g", rotate );
    rotate_ = rotate;
    astPutFits( fitschan, card, 0 );
    sprintf( card, "EQUINOX  = %d", equinox );
    astPutFits( fitschan, card, 0 );
    sprintf( card, "EPOCH    = %g", epoch );
    astPutFits( fitschan, card, 0 );
    sprintf( card, "CTYPE1   = %s", proj );
    astPutFits( fitschan, card, 0 );

    //  Now read the headers back as a suitable frameset.
    AstFrameSet *fitsset = (AstFrameSet *) astRead( fitschan );
    if ( fitsset != AST__NULL ) {
      wcs_ = fitsset;
    } else {
      if ( ! astOK ) astClearStatus;
      fitschan = (AstFitsChan *) astAnnul( fitschan );
      return error("Cannot locate a valid world coordinate system");
    }
    fitschan = (AstFitsChan *) astAnnul( fitschan );
    setEquinox();
    setSecPix();
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
const char *StarWCS::astGet( char *attrib )
{
  if ( !isWcs() ) {
    return (char *)NULL;
  }
  const char *result = astGetC( wcs_, attrib );
  if ( !astOK ) {
    astClearStatus;
    return (char *)NULL;
  }
  return result;
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

  // Find out how many dimensions the current and base frames have.
  AstFrame *baseframe = (AstFrame *) astGetFrame( wcs_, AST__BASE );
  AstFrame *skyframe = (AstFrame *) astGetFrame( wcs_, AST__CURRENT );
  int nbase = astGetI( baseframe, "Naxes" );
  int nsky = astGetI( skyframe, "Naxes" );
  if ( nbase == 2 && nsky == 2 ) {
    baseframe = (AstFrame *) astAnnul( baseframe );
    skyframe = (AstFrame *) astAnnul( skyframe );
    return 1;
  } else if ( nbase < 2 || nsky < 2 ) {

    // Only one dimension. Cannot process this.
    error( "Input WCS has only one dimension, need 2" );
    return 0;
  } else if ( nbase > MAXDIM || nsky > MAXDIM ) {
    error( "Input WCS has two many dimensions" );
    return 0;
  }

  // Record the indices of the current and base frames.
  int ibase = astGetI( wcs_, "Base" );
  int isky = astGetI( wcs_, "Current" );

  // Add the necessary frames to make the base frame 2D.
  int outperm[MAXDIM];
  outperm[0] = 1;
  outperm[1] = 2;
  AstFrame *newframe = (AstFrame *) astPickAxes( baseframe, 2,
                                                 outperm, NULL );

  // Create a mapping for this permutation that doesn't have <bad>
  // values as the result.
  int inperm[MAXDIM];
  inperm[0] = 1;
  inperm[1] = 2;
  int i;
  for( i = 2; i < nsky; i++ ) inperm[i] = -1;
  double zero = 0.0;
  AstMapping *map = (AstMapping *)astPermMap( nsky, inperm, 2, outperm, &zero, "" );

  // Now add this frame to the FrameSet and make it the base
  // one. Also reinstate the skyframe as the current frame.
  astAddFrame( wcs_, ibase, map, newframe );
  int iframe = astGetI( wcs_, "Current" );
  astSetI( wcs_, "Base", iframe );
  astSetI( wcs_, "Current", isky );
  newframe = (AstFrame *) astAnnul( newframe );
  map = (AstMapping *) astAnnul( map );

  //  Now deal with skyframe. In an attempt to make sure we pick the
  //  correct axes that correspond to those chosen for the image we
  //  try a transformation to see which axes are jiggled. Note this
  //  takes two goes as any other axes can be fixed at a given value
  //  (and will be returned as this, say a constant frequency for the
  //  whole image) so we need a genuine movement on the image to
  //  detect the correct axes.
  double in1[2][1];
  double out1[MAXDIM][1];
  in1[0][0] = 0.0;
  in1[1][0] = 0.0;
  for ( i = 0; i < MAXDIM; i++ ) out1[i][0] = 0.0;
  astTranN( wcs_, 1, 2, 1, (const double (*)[])in1, 1, nsky, 1, out1 );

  double in2[2][1];
  double out2[MAXDIM][1];
  in2[0][0] = (double) nxpix_;
  in2[1][0] = (double) nypix_;
  for ( i = 0; i < MAXDIM; i++ ) out2[i][0] = 0.0;
  astTranN( wcs_, 1, 2, 1, (const double (*)[])in2, 1, nsky, 1, out2 );

  //  Check to see which dimensions have jiggled.
  int n = 0;
  for ( i = 0; i < nsky; i++ ) {
    if ( fabs( out1[i][0] - out2[i][0] ) > DBL_EPSILON ) n++;
  }
  if ( n > 2 || !astOK ) {
    // Too many dimensions, must be a tricky case with a mapping that
    // transforms from 2D into possibly all the other dimensions. Give
    // up.
    error( "Input WCS is too complex" );
    return 0;

  } else if ( n < 2 ) {
    //  Something is horribly wrong here. All transformed values are
    //  0.0, this probably means we cannot easily pick a reference
    //  position to transform. Let's just try to pick out a skyframe,
    //  (only skyframe should have the AsTime attribute) failing this
    //  use the first two axes.
    char astime[10];
    int naxes = 0;
    for ( i = 1; i <= nsky; i++ ) {
      sprintf( astime, "AsTime(%d)", i );
      if ( astGetC( wcs_, astime ) ) {
        naxes++;
        out1[i][0] = 1.0;
        out2[i][0] = 3.0;
        if ( naxes == 2 ) break;
      } else {
        astClearStatus;
      }
    }
    if ( naxes != 2 ) {
      out1[0][0] = 1.0;
      out1[1][0] = 1.0;
      out2[0][0] = 2.0;
      out2[1][0] = 2.0;
    }
  }

  //  Probably only two dimensions gave valid results. Select these as
  //  the SkyFrame.
  n = 0;
  for ( i = 0; i < nsky; i++ ) {
    if ( fabs( out1[i][0] - out2[i][0] ) > DBL_EPSILON ) {
      outperm[n++] = i + 1;
      inperm[i] = i + 1;
    } else {
      inperm[i] = -1;
    }
  }
  newframe = (AstFrame *) astPickAxes( skyframe, 2, outperm, NULL );

  // Create a mapping for this permutation that doesn't have <bad>
  // values as the result.
  zero = 0.0;
  map = (AstMapping *)astPermMap( nsky, inperm, 2, outperm, &zero, "" );

  // Now add this frame to the FrameSet.
  astAddFrame( wcs_, isky, map, newframe );
  newframe = (AstFrame *) astAnnul( newframe );
  map = (AstMapping *) astAnnul( map );

  // If the above went well then assume we're in the clear, otherwise
  // indicate an error.
  if ( !astOK ) {
    return 0;
  } else {
    return 1;
  }
}

//
//  Reset the center of the WCS structure
//
//  Args:
// 	ra        = New center right ascension in degrees
// 	dec       = New center declination in degrees
// 	equinox   = (must be 2000 or 1950)
//
int StarWCS::shift(double ra, double dec, double equinox)
{
  // Does nothing, I guess this should setup a new Frame in some way
  // that transform the current system and is then added to the
  // frameset as the current frame? What is is centre in this case
  // (reference pixel? -- actually this is assumed to be the centre of
  // the image.).
  cerr << "WCS::shift, this function is not implemented -- sorry." << endl;
  return 0;
}
