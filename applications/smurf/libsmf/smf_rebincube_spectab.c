
/*
*+
*  Name:
*     smf_rebincube_spectab

*  Purpose:
*     Creates a table giving output channel index for each input channel
*     index.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_rebincube_spectab( dim_t nchan, dim_t nchanout, AstMapping ssmap,
*                            int **pspectab, int *status )

*  Arguments:
*     nchan = dim_t (Given)
*        The number of spectral channels in the input (or template) time
*        series NDF.
*     nchanout = dim_t (Given)
*        The number of spectral channels in the output (or sky cube) NDF.
*     ssmap = AstMapping * (Given)
*        A Mapping that goes from time series spectral grid axis (pixel
*        axis 1) to the sky cube spectral grid axis (pixel axis 3).
*     pspectab = int ** (Returned)
*        An address at which to store a pointer to an array of "int"s.
*        This array will have "nchan" elements, and each element will
*        hold the integer index (zero-based) of the nearest neighbouring
*        output channel. A value of -1 will be stored for input channels
*        that do not have any corresponding output channel. The returned
*        pointer should be freed using astFree when no longer needed. A
*        NULL pointer is returned if an error occurs.
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Description:
*     The returned array allows conversion between input and output
*     spectral channel number, using nearest neighbour interpolation.

*  Authors:
*     David S Berry (JAC, UClan)
*     {enter_new_authors_here}

*  History:
*     23-APR-2006 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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

#include <stdio.h>
#include <math.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_rebincube_spectab"

void smf_rebincube_spectab( dim_t nchan, dim_t nchanout, AstMapping *ssmap,
                            dim_t **pspectab, int *status ){

/* Local Variables */
   double *w1 = NULL;          /* Work space */
   double *w2 = NULL;          /* Work space */
   dim_t *spectab = NULL;      /* Returned pointer */
   dim_t ichan;                /* Index of current channel */

/* Initialise */
   *pspectab = NULL;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Allocate memory for the returned array of integer channel numbers. */
   spectab = astMalloc( sizeof( *spectab )*nchan );

/* Allocate memory for work arrays holding floating point spectral GRID
   values. */
   w1 = (double *) astMalloc( sizeof( double )*nchan );
   w2 = (double *) astMalloc( sizeof( double )*nchan );

/* Store the GID axis value at the centre of each input spectral channel. */
   for( ichan = 0; ichan < nchan; ichan++ ) w1[ ichan ] = ichan + 1.0;

/* Use the supplied mapping to get the corresponding output spectral GRID
   values. */
   astTran1( ssmap, nchan, w1, 1, w2 );

/* Find the zero based index of the output channel containing each of
   these transformed values. Store -1 for any points that cannot be
   transformed or which are outside the spectral bounds of the outptu cube. */
   for( ichan = 0; ichan < nchan; ichan++ ) {
      if( w2[ ichan ] != AST__BAD ) {
         spectab[ ichan ] = floor( w2[ ichan ] + 0.5 ) - 1;
         if( spectab[ ichan ] < 0 || spectab[ ichan ] >= nchanout ) {
            spectab[ ichan ] = -1;
         }
      } else {
         spectab[ ichan ] = -1;
      }
   }

/* Free the work arrays. */
   w1 = astFree( w1 );
   w2 = astFree( w2 );

/* Return the array of output integer channel numbers. */
   if( *status == SAI__OK ) {
      *pspectab = spectab;
   } else {
      spectab = astFree( spectab );
   }
}
