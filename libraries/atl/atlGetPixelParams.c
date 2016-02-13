#include "ast.h"
#include "atl.h"
#include "mers.h"
#include "sae_par.h"
#include <math.h>

void atlGetPixelParams( AstFrameSet *fset, int *dims, int degs,
                        double *crpix, double *crval, double *cdelt,
                        double *crota, int *status ){
/*
*+
*  Name:
*     atlGetPixelParams

*  Purpose:
*     Find typical values for "FITS-like" parameters describing a FrameSet.

*  Invocation:
*     void atlGetPixelParams( AstFrameSet *fset, int *dims, int degs,
*                             double *crpix, double *crval, double *cdelt,
*                             double *crota, int *status )

*  Description:
*     This function finds values that resemble the the FITS keywords
*     CRVAL1/2/3.., CRPIX1/2/3..., CRDELT1/2/3... and CROTA2, on the
*     assumption that the base Frame in the supplied FrameSet describe
*     GRID coords (i.e. FITS pixel coords), and the current Frame describe
*     the required WCS.  It is not restricted to 2D FrameSets.
*
*     If the FrameSet can be written to a FitsChan successfully using
*     FITS-WCS encoding, the the resulting keyword values are returned.
*     Otherwise, the values are estimated by transforming closely spaced
*     pixel positions along each axis. If the current Frame contains a
*     SkyFrame, and the SkyFrame has a defined reference position, then
*     this position specifies the returned CRVAL values. Otherwise, the
*     reference position is assumed to be at the central pixel.

*  Arguments:
*     fset
*        The FrameSet.
*     dims
*        Pointer to an array supplied holding the number of pixels along
*        each edge of the pixel array. The number of elements in this array
*        should match the number of axes in the base Frame of "fset".
*     degs
*        If non-zero, then the crval, cdelt and crota values for sky axes
*        are returned in units of degrees. Otherwise they are returned in
*        radians.
*     crpix
*        Pointer to an array returned holding the position of the
*        reference pixel in the base Frame of "fset". The number of
*        elements in this array should match the number of axes in the base
*        Frame of "fset".
*     crval
*        Pointer to an array returned holding the position of the
*        reference pixel in the current Frame of "fset". The number of
*        elements in this array should match the number of axes in the
*        current Frame of "fset".
*     cdelt
*        Pointer to an array returned holding the geodesic distance
*        along each edge of the reference pixel, measured within the
*        current Frame of "fset". The number of elements in this array
*        should match the number of axes in the base Frame of "fset".
*     crota
*        Pointer to a double in which to return the angle from north in
*        the current frame of "fset" to the second spatial pixel axis,
*        measured positive through east. This will be returned set to
*        AST__BAD if the current frame of "fset" does not contain a SkyFrame.
*     status
*        The global status.

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     13-DEC-2013 (DSB):
*        Original version.
*     {enter_further_changes_here}
*-
*/


/* Local Variables: */
   AstFitsChan *fc;
   AstMapping *map;
   int npix;
   int nwcs;
   int lataxis;
   int lonaxis;
   char name[20];
   const char *cval;
   int ipix;
   double dval1;
   double dval2;
   int ival;
   int iwcs;
   double pixpos[ ATL__MXDIM ];
   double wcspos[ ATL__MXDIM ];
   int pixaxes[ ATL__MXDIM ];
   int wcsaxes[ ATL__MXDIM ];
   int skyaxis1;
   int skyaxis2;

/* Initialise returned values. */
   *crota = AST__BAD;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Begin an AST context so that all AST objects created in this function
   are freed automatically when the context is ended. */
   astBegin;

/* Get the number of pixel axes (base frame) and wcs axes (current
   frame). */
   npix = astGetI( fset, "Nin" );
   if( npix > ATL__MXDIM && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( "", "atlGetPixelParams: Too many pixel axes (%d). Must be "
               "no more than %d.", status, npix, ATL__MXDIM );
   }

   nwcs = astGetI( fset, "Nout" );
   if( nwcs > ATL__MXDIM && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( "", "atlGetPixelParams: Too many WCS axes (%d). Must be "
               "no more than %d.", status, nwcs, ATL__MXDIM );
   }

/* Attempt to find a pair of sky axes in the current Frame by checking
   the "Domain" value for each WCS axis. */
   lataxis = -1;
   lonaxis = -1;
   skyaxis1 = -1;
   skyaxis2 = -1;

   for( iwcs = 0; iwcs < nwcs; iwcs++ ) {
      sprintf( name, "Domain(%d)", iwcs + 1 );
      cval = astGetC( fset, name );
      if( cval && !strcmp( cval, "SKY" ) ) {

/* Determine if this sky axis is a longitude or latitude axis, and record
   the zero-based indices of the longitude and latitude axes. */
         sprintf( name, "IsLatAxis(%d)", iwcs + 1 );
         ival = astGetI( fset, name );
         if( ival ) {
            lataxis = iwcs;
         } else {
            lonaxis = iwcs;
         }
      }
   }

/* If a pair of sky axes were found in the current Frame, get the indices
   of the corresponding pair of pixel axes. */
   if( lonaxis >= 0 && lataxis >= 0 ) {

/* If there are only two pixel axes, they must be the sky axes. */
      if( nwcs == 2 ) {
         skyaxis1 = 0;
         skyaxis2 = 1;

/* If there are more than two pixel axes, we need to work harder. */
      } else {

/* Use astMapSplit to find the two pixel axes that feed the two sky axes.
   astMapSplit identifes outputs corresponding to specified mapping inputs,
   so we need to invert the FrameSet first so that the WCS Frame becomes
   the input (i.e. base Frame). Remember to un-invert the FrameSet
   afterwards. */
         astInvert( fset );
         wcsaxes[ 0 ] = lataxis + 1;
         wcsaxes[ 1 ] = lonaxis + 1;
         astMapSplit( fset, 2, wcsaxes, pixaxes, &map );
         astInvert( fset );

/* If the wcs->pixel mapping was split succesfully, the pixaxes array
   will contain the one-based pixel axes that feed the sky axes. Convert
   them to zero-based and note the lowest and highest. */
         if( map && astGetI( map, "Nout" ) == 2 ) {
            if( pixaxes[ 0 ] < pixaxes[ 1 ] ) {
               skyaxis1 = pixaxes[ 0 ] - 1;
               skyaxis2 = pixaxes[ 1 ] - 1;
            } else {
               skyaxis1 = pixaxes[ 1 ] - 1;
               skyaxis2 = pixaxes[ 0 ] - 1;
            }

/* If it could not be split, it means the spatial and non-spatial axes are
   tangle up by the pixel->wcs mapping to such an extent that they cannot
   be separated. */
         } else if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( "", "atlGetPixelParams: Cannot separate the spatial "
                     "axes from the non-spatial axes.", status );
         }
      }
   }

/* Attempt to write the supplied FrameSet to FitsChan using FITS-WCS
   encoding. If successful, retrieve the required values. Convert sky
   values from degrees to radians if required. */
   fc = astFitsChan( NULL, NULL, "Encoding=FITS-WCS" );
   if( astWrite( fc, fset ) == 1 ) {

      for( ipix = 0; ipix < npix; ipix++ ) {
         sprintf( name, "CRPIX%d", ipix + 1 );
         if( !astGetFitsF( fc, name, crpix + ipix ) && *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( "", "atlGetPixelParams: %s not found in FitsChan "
                     "(possible programming error).", status, name );
         }

         sprintf( name, "CDELT%d", ipix + 1 );
         if( !astGetFitsF( fc, name, cdelt + ipix ) && *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( "", "atlGetPixelParams: %s not found in FitsChan "
                     "(possible programming error).", status, name );
         }

         if( !degs && ( ipix == skyaxis1 || ipix == skyaxis2 ) ){
            cdelt[ ipix ] *= AST__DD2R;
         }

      }

      for( iwcs = 0; iwcs < nwcs; iwcs++ ) {

         sprintf( name, "CRVAL%d", iwcs + 1 );
         if( !astGetFitsF( fc, name, crval + iwcs ) && *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( "", "atlGetPixelParams: %s not found in FitsChan "
                     "(possible programming error).", status, name );
         }

         if( !degs && ( iwcs == lonaxis || iwcs == lataxis ) ){
            crval[ iwcs ] *= AST__DD2R;
         }

      }

/* Derive the position angle (within the sky frame) of the second spatial
   pixel axis, based on the PCi_j rotation matrix elements. Note, there is
   no assumption here that the latitude and longitude axes are orthogonal
   in the pixel frame. FITS-WCS allows for shear, so this value says
   nothing about the orientation of the first spatial pixel axis. But in
   practice images nearly always have no shear. */
      if( lataxis >= 0 && lonaxis >= 0 ) {
         sprintf( name, "PC%d_%d", lonaxis + 1, skyaxis1 + 1 );
         if( !astGetFitsF( fc, name, &dval1 ) && *status == SAI__OK ) {
            dval1 = ( lonaxis == skyaxis1 ) ? 1.0 : 0.0;
         }

         sprintf( name, "PC%d_%d", lonaxis + 1, skyaxis2 + 1 );
         if( !astGetFitsF( fc, name, &dval2 ) && *status == SAI__OK ) {
            dval2 = ( lonaxis == skyaxis2 ) ? 1.0 : 0.0;
         }

         *crota = atan2( dval2, dval1 );
         if( *crota < 0.0 ) *crota += 2*AST__DPI;
         if( degs ) *crota *= AST__DR2D;
      }

/* If the supplied FrameSet could not be converted to a set of
   FITS-WCS keywords, we derive similar values by looking at small
   increments of pixel position. */
   } else {

/* First job is to decide on the reference position. By default we use
   the central pixel. Store the corresponding pixel coords. */
      for( ipix = 0; ipix < npix; ipix++ ) {
         crpix[ ipix ] = ( 1.0 + dims[ ipix ] )/2.0;
      }

/* Convert this pixel position to the WCS Frame. */
      astTranN( fset, 1, npix, 1, crpix, 1, nwcs, 1, crval );

/* If the current Frame contains a pair of sky axes, then the associated
   SkyFrame may include a reference position. If so, we will use it
   instead of the central pixel. First test to see if the SkyFrame has a
   reference position. If so get the reference longitude and latitude in
   radians, and convert the new reference position back to pixel coords. */
      if( lonaxis >= 0 && lataxis >= 0 ) {
         sprintf( name, "SkyRef(%d)", lonaxis + 1 );
         if( astTest( fset, name ) ) {
            crval[ lonaxis ] = astGetD( fset, name );
            sprintf( name, "SkyRef(%d)", lataxis + 1 );
            crval[ lataxis ] = astGetD( fset, name );

            astTranN( fset, 1, nwcs, 1, crval, 0, npix, 1, crpix );

/* If we have sky axes but the skyframe has no reference position, we
   need to check that the central pixel is a good default. For instance,
   it may be off the edge of an all-sky map, in which case it is no good
   as a reference position. */
         } else if( ( crval[ lonaxis ] == AST__BAD ||
                      crval[ lataxis ] == AST__BAD ) && *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( "", "atlGetPixelParams: No reference position can be "
                     "determined.", status );
         }
      }

/* Normalize the reference position. */
      astNorm( fset, crval );

/* Now we find the pixel size on each pixel axis. First take a copy of
   the pixel reference position. */
      memcpy( pixpos, crpix, npix*sizeof( *pixpos ) );
      for( ipix = 0; ipix < npix; ipix++ ) {

/* Store a pixel position which is offset away from the reference position
   by one pixel along the current pixel axis, and then transform it into
   the WCS Frame. */
         pixpos[ ipix ] += 1.0;
         astTranN( fset, 1, npix, 1, pixpos, 1, nwcs, 1, wcspos );
         pixpos[ ipix ] -= 1.0;

/* Find the geodesic distance between this WCS position and the reference
   position. */
         cdelt[ ipix ] = astDistance( fset, crval, wcspos );
      }

/* Find the crota value if we have a pair of sky axes. */
      if( lonaxis >= 0 && lataxis >= 0 ){

/* Get a WCS position about one arc-second north of the reference position. */
         memcpy( wcspos, crval, nwcs*sizeof( *wcspos ) );
         wcspos[ lataxis ] += 5.0E-6;

/* Transform to pixel coordinates. */
         astTranN( fset, 1, nwcs, 1, wcspos, 0, npix, 1, pixpos );

/* Get the required angle. */
         *crota = atan2( pixpos[ skyaxis1 ] - crpix[ skyaxis1 ],
                         pixpos[ skyaxis1 ] - crpix[ skyaxis2 ] );
         if( *crota < 0.0 ) *crota += 2*AST__DPI;
         if( !degs ) *crota *= AST__DR2D;
      }

/* Convert the returned angles to degrees if required. */
      if( !degs && ( lonaxis >= 0 && lataxis >= 0 ) ){
         crval[ lataxis ] *= AST__DR2D;
         crval[ lonaxis ] *= AST__DR2D;
         cdelt[ skyaxis1 ] *= AST__DR2D;
         cdelt[ skyaxis2 ] *= AST__DR2D;
      }
   }

/* End the AST context. This will annull all AST objects created in this
   function. */
   astEnd;

}

