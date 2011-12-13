#include "sae_par.h"
#include "cupid.h"
#include "math.h"

int cupidDefMinPix( int ndim, double *fwhm, double thresh, double minhgt,
         int *status ){
/*
*+
*  Name:
*     cupidDefMinPix

*  Purpose:
*     Get the default value for the MinPix configuration parameter.

*  Language:
*     Starlink C

*  Synopsis:
*     int cupidDefMinPix( int ndim, double *fwhm, double thresh,
*                            double minhgt, int *status )

*  Description:
*     This function returns a default value for the MinPix configuration
*     parameter, which gives the minimum area (in pixels) needed for a
*     significant clump. The default value returned by this function is
*     the area of a gaussian clump with the specified beam widths, taken
*     at the specified threshold level. The clump is assumed to have the
*     specified peak value.
*
*     The returned value is never less than a limiting value that depends
*     on the number of axes in the data; for 1D data the limit is 3, for
*     2D the limit is 7 and for 3D data the limit is 16.

*  Parameters:
*     ndim
*        The number of pixel axes in the data; 1, 2 or 3.
*     fwhm
*        Pointer to an array holding the FWHM of the intrumental beam
*        associated with each pixel axis.
*     thresh
*        The background level at which clumps are truncated.
*     minhgt
*        The minimum peak value for a significant clump.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     The default MinPix value.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     5-APR-2006 (DSB):
*        Original version.
*     10-MAY-2007 (DSB):
*        Make the lower limit less if there are fewer than 3 axes.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */

   double f;
   int ret;
   int lowlim;

/* Initialise */
   lowlim = ( ndim == 1 ) ? 3 : ( ( ndim == 2 ) ? 7 : 16 );
   ret = lowlim;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* Find the required constant that the defines level at which we find the
   find of the Gaussian bell. */
   f = log( minhgt/thresh )/log(2.0);

/* For 1D data, the number of pixels is the width of the Gaussian bell. */
   if( ndim == 1 ) {
      ret = fwhm[ 0 ]*sqrt( f );

/* For 2D data, the number of pixels is the area of the Gaussian bell. */
   } else if( ndim == 2 ) {
      ret = 3.14159*fwhm[ 0 ]*fwhm[ 1 ]*f/4.0;

/* For 3D data, the number of pixels is the volume of the Gaussian bell. */
   } else {
      ret = 3.14159*fwhm[ 0 ]*fwhm[ 1 ]*fwhm[ 2 ]*pow(f,1.5)/4.0;

   }

/* Ensure a default of at least 16 is used. */
   if( ret < lowlim ) ret = lowlim;

/* Return the result. */
   return ret;
}
