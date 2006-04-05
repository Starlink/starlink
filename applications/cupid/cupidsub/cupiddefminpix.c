#include "sae_par.h"
#include "cupid.h"
#include "math.h"

int cupidDefMinPix( int ndim, double *fwhm, double thresh, double minhgt ){
/*
*  Name:
*     cupidDefMinPix

*  Purpose:
*     Get the default value for the MinPix configuration parameter.

*  Synopsis:
*     int cupidDefMinPix( int ndim, double *fwhm, double thresh, 
*                            double minhgt )

*  Description:
*     This function returns a default value for the MinPix configuration
*     parameter, which gives the minimum area (in pixels) needed for a
*     significant clump. The default value returned by this function is
*     the area of a gaussian clump with the specified beam widths, taken
*     at the specified threshold level. The clump is assumed to have the
*     specified peak value.
*
*     If the returned value would be less than 16 pixels, 16 is returned.

*  Parameters:
*     ndim
*        The number fo pixel axes in the data;1, 2 or 3.
*     fwhm
*        Pointer to an array holding the FWHM of the intrumental beam
*        associated with each pixel axis.
*     thresh
*        The background level at which clumps are truncated.
*     minhgt
*        The minimum peak value for a significant clump.

*  Returned Value:
*     The default MinPix value.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     5-APR-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   double f;
   int ret;

/* Initialise */
   ret = 16;

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
   if( ret < 16 ) ret = 16;

/* Return the result. */
   return ret;
}
