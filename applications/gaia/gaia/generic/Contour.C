/*+
 *  Name:
 *     Contour.C

 *  Language:
 *     C++

 *  Purpose:
 *     Define a class for contouring an image, possibly over another
 *     image.

 *  Description:
 *     This class controls the drawing of contours lines. The contours
 *     are generated using image data which is given as a ImageIO
 *     object. The positions where the contours are actually plotted
 *     are decided by using coordinates described by an AstPlot. The
 *     AstPlot should be set up so that the current frame is the GRID
 *     coordinates of the image (i.e. the pixel indices) and the base
 *     frame is in the coordinate system of the graphics device. Using
 *     this scheme the AstPlot may transform from GRID coordinates to
 *     Graphics coordinate via a system based on another image.
 *
 *     The properties of each contour (i.e. colour, line thickness
 *     etc.) are defined as a list of AST attributes. This list should
 *     either consist of one set, which will be applied to all
 *     contours, or a set for each contour
 *
 *     The "careful_" member variable indicates that contours should
 *     be drawn using geodesics, rather than plain straight-lines.
 *     Drawing straight-lines is faster, but could be confused when
 *     using difficult astrometries.
 *
 *     See the descriptions with the member functions (here and in the
 *     associated class definition file) for how to use this class.

 *  Algorithm:
 *     The following notes describe the current algorithm used to
 *     locate the contours (taken from KPS1_CNTF). Note we use this
 *     algorithm as it draws the contours in sensible polylines,
 *     rather than many line segments, this should suit a Tk canvas.
 *
 *     The routine makes a separate pass through the image for each
 *     contour to be plotted.  The image is divided into "cells"
 *     (groups of four adjacent pixels) and each is examined in turn.
 *     Cells contining "bad" pixels are ignored, but for all others
 *     the minimum and maximum cell data values are found.  If the
 *     contour level currently being plotted lies between these two
 *     values, then a contour crosses the cell in question, otherwise
 *     the cell is skipped over on this pass.
 *
 *     Having identified a cell containing a contour, the contour
 *     following algorithm is triggered.  Each cell side (a "side" is
 *     one of the lines joining pixel centres) is examined to
 *     determine if the contour passes through it and, if so, at what
 *     position.  If the contour only passes through two cell sides,
 *     then the cell is "simple" and is only crossed by a single
 *     contour line.  In this case, the contour entry and exit points
 *     are put into a list of positions (to be plotted), the cell is
 *     flagged as "done" and the algorithm moves on to the cell
 *     adjacent to the contour exit position, where the process is
 *     repeated - thereby "following" the contour.
 *
 *     Contour following continues until the next cell is off the edge
 *     of the image, has already been "done" on this pass, contains a
 *     "bad" pixel or is "confused" (i.e. is crossed by more than one
 *     contour line).  In "confused" cells, all four cell sides are
 *     crossed by contours, and the algorithm pairs the points to form
 *     two line segents to plot which do not cross and which produce
 *     the shortest total length of contour line within the cell.
 *     Contour- following also terminates if the buffer containing the
 *     list of points to plot becomes full.
 *
 *     When contour following terminates, all pending output is
 *     plotted with the appropriate pen (there are two separate lines
 *     to plot if the final cell was confused).  The scan through the
 *     data (looking for cells which are crossed by the current
 *     contour) then resumes at the cell following the one which
 *     initiated the last episode of contour-following.  Cells which
 *     are already flagged as "done" do not subsequently trigger
 *     further contour-following on this pass.

 *  Implementation Deficiencies:
 *     The contours are not smooth and the scanning algorithm can be made
 *     many times faster by not examining all pixels at all heights.

 *
 *  Authors:
 *     P.W. Draper (PWD)
 *
 *  Copyright:
 *     Copyright (C) 1999-2005 Central Laboratory of the Research Councils
 *     Copyright (C) 2006-2007 Particle Physics & Astronomy Research Council.
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
 *  History:
 *     23-MAY-1999 (PWD):
 *        Original version. The contouring algorithm is based on
 *        Rodney Warren-Smith's used in the KPS1_CNTF. See the
 *        drawContour member for how this works.
 *     06-JUL-1999 (PWD):
 *        Added BSCALE and BZERO corrections to contour level.
 *     {enter_changes_here}
 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <climits>
#include <cstring>
#include <cstdlib>
#include <cfloat>
#include "Contour.h"
extern "C" {
#include "grf.h"
}

//
//  Constructor, only imio and plot are required.
//
Contour::Contour( const ImageIO imio, const AstPlot *plot,
                  const double levels[], const int nlevels,
                  const char *prefs[], const int nprefs )
   : imageio_(imio),
     plot_(NULL),
     nlevels_(0),
     levels_(NULL),
     nprefs_(0),
     prefs_(NULL),
     xlower_(1),
     ylower_(1),
     xsize_(0),
     ysize_(0),
     careful_(1),
     swap_(0),
     isfits_(0),
     userBuffer_(NULL)
{

   //  Make a clone of the plot and simplify it. This adds a new
   //  Current Frame into the Plot. This can help to speed up the
   //  drawing, and also avoids the possibility of the Mapping going
   //  via a Frame in which the positions are undefined
   if ( plot != (AstPlot *) NULL ) {
      plot_ = (AstPlot *) astClone( plot );
      AstMapping *oldmap = (AstMapping *) astGetMapping( plot_,
                                                         AST__BASE,
                                                         AST__CURRENT );
      AstMapping *newmap = (AstMapping *) astSimplify( oldmap );
      AstFrame *frame = (AstFrame *) astGetFrame( plot_, AST__CURRENT );
      astAddFrame( plot_, AST__BASE, newmap, frame );

      //  Release intermediary products.
      astAnnul( oldmap );
      astAnnul( newmap );
      astAnnul( frame );
   }

   //  Copy the contour levels.
   if ( nlevels > 0 ) {
      setLevels( levels, nlevels );
   }

   //  And the preferences, usually one per contour level. This should
   //  consist of AST attributes (i.e. "width(curve)=1,colour(curve)=2").
   if ( nprefs > 0 ) {
      setPrefs( prefs, nprefs );
   }
}

//
// Destructor
//
Contour::~Contour()
{
   //  Free all resources:
   //  Clone of the plot.
   if ( plot_ != (AstPlot *) NULL ) {
      plot_ = (AstPlot *) astAnnul( plot_ );
   }

   //  Contour levels.
   freeLevels();

   //  Preferences.
   freePrefs();

   //  User preferences buffer.
   if ( userBuffer_ != (char *) NULL ) {
      delete userBuffer_;
   }
}

//
//  Set new contour levels.
//
void Contour::setLevels( const double levels[], const int nlevels )
{
   freeLevels();
   if ( nlevels > 0 ) {
      levels_ = new double[nlevels];
      for ( int i = 0; i < nlevels; i++ ) {
         levels_[i] = levels[i];
      }
      nlevels_ = nlevels;
   }
}

//
//  Release existing contours.
//
void Contour::freeLevels() {
   if ( nlevels_ > 0 ) {
      delete [] levels_;
      nlevels_ = 0;
   }
}

//
//  Get a contour level (indexed up to nlevels_).
//
double Contour::getLevel( const int ilevel )
{
   if ( ilevel < nlevels_ ) {
      return levels_[ilevel];
   }
   return DBL_MIN;
}

//
//  Set/reset the line preferences.
//
void Contour::setPrefs( const char *prefs[], const int nprefs )
{
   //  Release any existing preferences.
   freePrefs();

   //  And set the new ones.
   if ( nprefs > 0 ) {
      prefs_ = new char *[nprefs];
      for ( int i = 0; i < nprefs; i++ ) {
         prefs_[i] = new char[strlen(prefs[i])+1];
         strcpy( prefs_[i], prefs[i] );
      }
      nprefs_ = nprefs;
   }
}

//
//  Release preferences.
//
void Contour::freePrefs()
{
   if ( nprefs_ > 0 ) {
      for ( int i = 0; i < nprefs_; i++ ) {
         delete [] prefs_[i];
      }
      delete [] prefs_;
      nprefs_ = 0;
   }
}

//
//  Get a copy of a set of preferences. Note caller must copy this
//  before next call to getPrefs, otherwise the space will be deleted.
//
char *Contour::getPrefs( const int ipref )
{
   if ( ipref < nprefs_ ) {
      if ( userBuffer_ != (char *) NULL ) {
         delete userBuffer_;
      }
      userBuffer_ = new char[strlen(prefs_[ipref])+1];
      return userBuffer_;
   }
   return NULL;
}

//
//   Set the limits of the region to be contoured. Note this may be
//   modified to fit the image.
//
void Contour::setRegion( const int xlower, const int ylower,
                         const int xsize, const int ysize )
{
   xlower_ = xlower;
   ylower_ = ylower;
   xsize_ = xsize;
   ysize_ = ysize;
}

//
//  Get the region that is to be contoured.
//
void Contour::getRegion( int& xlower, int& ylower, int& xsize, int& ysize )
{
   xlower = xlower_;
   ylower = ylower_;
   xsize = xsize_;
   ysize = ysize_;
}

//
//  Draw the contours. Returns the total number of points drawn.
//
int Contour::drawContours()
{
   //  Local variables.
   double cval;
   AstPlot *lplot;
   int ndrawn = 0;
   int totaldrawn = 0;

   //  Get image data properties.
   void *image = (void *) imageio_.dataPtr();
   int nx = imageio_.width();
   int ny= imageio_.height();
   int type = imageio_.bitpix();
   double bscale = imageio_.bscale();
   double bzero = imageio_.bzero();

   //  Make sure the part of the image to be drawn fits onto the image.
   int xsize = xsize_;
   int ysize = ysize_;
   int xlower = xlower_;
   int ylower = ylower_;
   if ( xsize <= 0 || xsize > nx ) xsize = nx;
   if ( ysize <= 0 || ysize > ny ) ysize = ny;
   if ( xlower <= 0 || xlower > nx ) xlower = 1;
   if ( ylower <= 0 || ylower > ny ) ylower = 1;
   if ( xsize + xlower > nx ) xsize = nx - xlower;
   if ( ysize + ylower > ny ) ysize = ny - ylower;

   //  Get some workspace for locating pixels that have already been
   //  "done".
   char *done = new char[xsize * ysize];

   //  Scan through each contour level.
   for ( int icont = 0; icont < nlevels_; icont++ ) {
      cval = levels_[icont];

      //  Correct the contour level for any scale and zero factors (FITS
      //  scaled images only).
      cval = ( cval - bzero ) / bscale;

      // If different properties are being used, produce a modified Plot
      // which draws curves with the pen style supplied for this
      // contour.
      if ( nprefs_ > 1 && icont < nprefs_ ) {

         // Take a deep copy of the supplied Plot. This Plot will be
         // modified using the supplied attribute settings. A copy is
         // used so that the original plotting attributes can be
         // re-instated later.
         lplot = (AstPlot *) astCopy( plot_ );

         //  Set the AST Attribute settings from properties lists.
         astSet( lplot, prefs_[icont] );
      } else {

         //  If the same properties are being used for all contours, just
         //  clone the supplied Plot pointer.
         lplot = (AstPlot *) astClone( plot_ );

         //  And set the values. If any are available.
         if ( nprefs_ > 0 ) {
            astSet( lplot, prefs_[0] );
         }
      }

      //  If not plotting carefully then need to to (re-)establish the
      //  attributes of the line by hand (not using astPolyCurve). Do
      //  this now rather than every line segment, but remember do not
      //  do any AST graphics (else these will be ignored).
      if ( ! careful_ ) {
         double colour = astGetD( lplot, "Colour(Curve)" );
         astGAttr( GRF__COLOUR, colour, (double *)NULL, GRF__LINE );
         double width = astGetD( lplot, "Width(Curve)" );
         astGAttr( GRF__WIDTH, width, (double *)NULL, GRF__LINE );
         double style = astGetI( lplot, "Style(Curve)" );
         astGAttr( GRF__STYLE, style, (double *)NULL, GRF__LINE );
      }

      //  Initialise the store of cells done.
      memset( done, '\0', xsize * ysize * sizeof( char ) );

      //  Scan for this contour and plot it. Call appropriate member for
      //  data format.
      switch ( type ) {
         case BYTE_IMAGE:
            if ( swap_ ) {
               ndrawn = scanSwapImageNDF( (unsigned char *) image, nx, ny,
                                       lplot, cval, xlower, ylower,
                                       xsize, ysize, done );
            } else {
               ndrawn = scanNativeImageNDF( (unsigned char *) image, nx, ny,
                                         lplot, cval, xlower, ylower,
                                         xsize, ysize, done );
            }
            break;
         case X_IMAGE:
            if ( swap_ ) {
               ndrawn = scanSwapImageNDF( (char *) image, nx, ny,
                                       lplot, cval, xlower, ylower, xsize,
                                       ysize, done );
            } else {
               ndrawn = scanNativeImageNDF( (char *) image, nx, ny,
                                         lplot, cval, xlower, ylower, xsize,
                                         ysize, done );
            }
            break;
         case USHORT_IMAGE:
            if ( swap_ ) {
               ndrawn = scanSwapImageNDF( (ushort *) image, nx, ny,
                                       lplot, cval, xlower, ylower,
                                       xsize, ysize, done );
            } else {
               ndrawn = scanNativeImageNDF( (ushort *) image, nx, ny,
                                         lplot, cval, xlower, ylower,
                                         xsize, ysize, done );
            }
            break;
         case SHORT_IMAGE:
            if ( swap_ ) {
               ndrawn = scanSwapImageNDF( (short *) image, nx, ny, lplot,
                                       cval, xlower, ylower, xsize,
                                       ysize, done );
            } else {
               ndrawn = scanNativeImageNDF( (short *) image, nx, ny, lplot,
                                         cval, xlower, ylower, xsize,
                                         ysize, done );
            }
            break;
         case LONG_IMAGE:
            if ( swap_ ) {
               ndrawn = scanSwapImageNDF( (FITS_LONG *) image, nx, ny,
                                       lplot, cval, xlower, ylower,
                                       xsize, ysize, done );
            } else {
               ndrawn = scanNativeImageNDF( (FITS_LONG *) image, nx, ny,
                                         lplot, cval, xlower, ylower,
                                         xsize, ysize, done );
            }
            break;
         case FLOAT_IMAGE:
             if ( isfits_ ) {
                 if ( swap_ ) {
                     ndrawn = scanSwapImageFITS( (float *) image, nx, ny,
                                                 lplot, cval, xlower, ylower,
                                                 xsize, ysize, done );
                 } else {
                     ndrawn = scanNativeImageFITS( (float *) image, nx, ny,
                                                   lplot, cval, xlower,
                                                   ylower, xsize,
                                                   ysize, done );
                 }
            }
            else if ( swap_ ) {
                ndrawn = scanSwapImageNDF( (float *) image, nx, ny, lplot,
                                           cval, xlower, ylower, xsize,
                                           ysize, done );
            } else {
                ndrawn = scanNativeImageNDF( (float *) image, nx, ny, lplot,
                                             cval, xlower, ylower, xsize,
                                             ysize, done );
            }
            break;
         case DOUBLE_IMAGE:
            if ( isfits_ ) {
                if ( swap_ ) {
                    ndrawn = scanSwapImageFITS( (double *) image, nx, ny,
                                                lplot, cval, xlower, ylower,
                                                xsize, ysize, done );
                } else {
                    ndrawn = scanNativeImageFITS( (double *) image, nx, ny,
                                                  lplot, cval, xlower, ylower,
                                                  xsize, ysize, done );
                }
            } else if ( swap_ ) {
                ndrawn = scanSwapImageNDF( (double *) image, nx, ny, lplot,
                                           cval, xlower, ylower, xsize,
                                           ysize, done );
            } else {
                ndrawn = scanNativeImageNDF( (double *) image, nx, ny, lplot,
                                             cval, xlower, ylower, xsize,
                                             ysize, done );
            }
            break;
         default:
            ndrawn = 0;
      }
      totaldrawn += ndrawn;

      //  Annul the temporary copy of the supplied Plot which was used
      //  to do the drawing.
      lplot = (AstPlot *) astAnnul( lplot );

      //  Abort if failing:
      if ( ! astOK ) {
         break;
      }
   }

   //  Free the workspace.
   delete [] done;

   //  Return number of points drawn.
   return totaldrawn;
}

//
//  Plot a contour line.
//
void Contour::contPlot( const AstPlot *plot, const int npts,
                        const double x[], const double y[] )
{
   if ( careful_ ) {
      //  Carefully draw geodesics.

      //  Copy each point into a correctly formatted buffer.
      double xydata[2][MAXPTS];
      for ( int i = 0; i < npts; i++ ) {
         xydata[0][i] = x[i];
         xydata[1][i] = y[i];
      }

      //  Draw the geodesic.
      astPolyCurve( plot, npts, 2, MAXPTS, (const double *) xydata );
   }
   else {
      //  Draw straight-lines (graphics surface wise) at the resolution
      //  of the contoured image for speed. Note that line attributes
      //  should be established before calling this routine.

      //  First transform positions to graphics coordinates.
      double xgraph[MAXPTS], ygraph[MAXPTS];
      astTran2( plot, npts, x, y, 0, xgraph, ygraph );

      //  Now go positions breaking for BAD values.
      float xfloat[MAXPTS], yfloat[MAXPTS];
      int igood = 0;
      for ( int i = 0; i < npts; i++ ) {
         if ( xgraph[i] != AST__BAD && ygraph[i] != AST__BAD ) {
            xfloat[igood] = (float) xgraph[i];
            yfloat[igood] = (float) ygraph[i];
            igood++;

            //  Skip to next position.
            continue;
         }

         //  BAD point, so plot what we have and break line.
         if ( igood > 0 ) {
            astGLine( igood, xfloat, yfloat );
         }
         igood = 0;
      }

      //  Plot all remaining positions.
      if ( igood > 0 ) {
         astGLine( igood, xfloat, yfloat );
      }
      igood = 0;
   }
}

//  Define members that are data type dependent. See
//  ContourTemplates.icc for which ones.

#define DATA_TYPE char
#define DATA_FORMAT NDF
#include "ContourTemplates.icc"
#undef DATA_TYPE
#undef DATA_FORMAT

#define DATA_TYPE unsigned char
#define DATA_FORMAT NDF
#include "ContourTemplates.icc"
#undef DATA_TYPE
#undef DATA_FORMAT

#define DATA_TYPE short
#define DATA_FORMAT NDF
#include "ContourTemplates.icc"
#undef DATA_TYPE
#undef DATA_FORMAT

#define DATA_TYPE unsigned short
#define DATA_FORMAT NDF
#include "ContourTemplates.icc"
#undef DATA_TYPE
#undef DATA_FORMAT

#define DATA_TYPE FITS_LONG
#define DATA_FORMAT NDF
#include "ContourTemplates.icc"
#undef DATA_TYPE
#undef DATA_FORMAT

#define DATA_TYPE float
#define DATA_FORMAT NDF
#include "ContourTemplates.icc"
#undef DATA_TYPE
#undef DATA_FORMAT

#define DATA_TYPE double
#define DATA_FORMAT NDF
#include "ContourTemplates.icc"
#undef DATA_TYPE
#undef DATA_FORMAT

//  Be careful about NaNs in FITS data.
#define DATA_TYPE float
#define DATA_FORMAT FITS
#include "ContourTemplates.icc"
#undef DATA_TYPE
#undef DATA_FORMAT

#define DATA_TYPE double
#define DATA_FORMAT FITS
#include "ContourTemplates.icc"
#undef DATA_TYPE
#undef DATA_FORMAT
