/*
*+
*  Name:
*     smf_smoothmask

*  Purpose:
*     Smooth a 2D boolean mask using a Gaussian, and then threshold it to
*     produce a new boolean mask.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     double *smf_smoothmask( ThrWorkForce *wf, smf_qual_t qual,
*                             const smf_qual_t *mapqual, int dim0, int dim1,
*                             AstFrameSet *fset, double fwhm, double low,
*                             int *status )

*  Arguments:
*     wf = ThrWorkForce *(Given)
*        Pointer to thread workforce to use for threaded tasks.
*     qual = smf_qual_t (Given)
*        The quality used to define the mask.
*     mapqual = const smf_qual_t * (Given)
*        Pointer to the original 2D mask of quality flags.
*     dim0 = int (Given)
*        The number of pixels along the first dimension of the mask.
*     dim1 = int (Given)
*        The number of pixels along the second dimension of the mask.
*     fset = AstFrameSet * (Given)
*        FrameSet describing the WCS of the mask.
*     fwhm = double (Given)
*        The FWHM of the Gaussian filter, in arcsec.
*     low = double (Given)
*        The threshold at which to cut the smoothed mask.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     Pointer to memory holding the new mask. It should be freed using
*     astFree when no longer needed.

*  Description:
*     This function smooths a 2D mask with a Gaussian PSF, and then
*     produces a new mask by thresholding the smoothed mask.

*  Authors:
*     David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     31-JAN-2012 (DSB):
*        Original version.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

double *smf_smoothmask( ThrWorkForce *wf, smf_qual_t qual,
                        const smf_qual_t *mapqual, int dim0, int dim1,
                        AstFrameSet *fset, double fwhm, double low,
                        int *status ){

/* Local Variables: */
   double *result;
   dim_t ixy;
   dim_t nxy;

/* Check the inherited status */
   if( *status != SAI__OK ) return NULL;

/* Store the number of pixels in the mask. */
   nxy = dim0*dim1;

/* Allocate the memory for the returned mask, and check it was allocated
   succesfully. */
   result = astMalloc( nxy*sizeof( *result ) );
   if( astOK ) {

/* Tell the user what is happening. */
      msgOutiff( MSG__DEBUG, "", "smf_smoothmask: calculating new SNR mask "
                 "with smoothing of %g arc-secs", status, fwhm );

/* Copy the supplied mask into the returned array. */
      for( ixy = 0; ixy < nxy; ixy++ ) {
         result[ ixy ] = ( mapqual[ ixy ] & qual ) ? 0.0 : 1.0;
      }

/* Put the mask into a smfData wrapper */
      smfData *filtermap = smf_create_smfData( 0, status );
      if( *status == SAI__OK ) {
         filtermap->isFFT = -1;
         filtermap->dtype = SMF__DOUBLE;
         filtermap->pntr[0] = result;
         filtermap->ndims = 2;
         filtermap->dims[0] = dim0;
         filtermap->dims[1] = dim1;
         filtermap->hdr->wcs = astClone( fset );

/* Create a Gaussian filter to smooth the mask. */
         smfFilter *filt = smf_create_smfFilter( filtermap, status );
         smf_filter2d_gauss( filt, fwhm, status );

/* Smooth the mask. */
         smf_filter_execute( wf, filtermap, filt, 0, 0, status );

/* If the threshold for clipping the smoothed mask is negative, it
   indicates that the absolute value of the threshold is a multiple of the
   height to which a single solated pixel is reduced after smoothing. */
         if( low < 0.0 ) {

/* Create an array holding a single non-zero pixel at its centre. */
            double *tmask = astCalloc( nxy, sizeof(*tmask) );
            if( tmask ) {
               tmask[ ( dim1/2 )*dim0 + dim0/2 ] = 1.0;

/* Smooth this array with the Gaussian filter. */
               filtermap->pntr[0] = tmask;
               smf_filter_execute( wf, filtermap, filt, 0, 0, status );

/* Find the largest value in the smoothed array. */
               double vmax = 0.0;
               for( ixy = 0; ixy < nxy; ixy++ ) {
                  if( tmask[ ixy ] > vmax ) vmax = tmask[ ixy ];
               }

/* Normalised the threshold value using this maximum value, and switch its
   sign to make it positive.  */
               msgOutiff( MSG__DEBUG, "", "smf_smoothmask: changing SNR mask "
                          "threshold from %g to %g", status, low , -low*vmax );
               low *= -vmax;

/* Free the array */
               tmask = astFree( tmask );
            }
         }

/* Free the filter. */
         filt = smf_free_smfFilter( filt, status );

/* Threshold the smoothed mask. */
         msgOutiff( MSG__DEBUG, "", "smf_smoothmask: thresholding smoothed "
                  "SNR mask at %g", status, low );
         for( ixy = 0; ixy < nxy; ixy++ ) {
           if( result[ ixy ] < low ) result[ ixy ] = VAL__BADD;
         }

/* Unset pointers to avoid freeing them */
         filtermap->pntr[0] = NULL;

      }

      smf_close_file( wf, &filtermap, status );
   }

/* Return the pointer to the new mask. */
   return result;
}

