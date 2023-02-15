/*
*+
*  Name:
*     smf_find_bad_dets

*  Purpose:
*     Find bad detectors in a group of time ordered data files.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     dim_t *smf_find_bad_dets( Grp *igrp,  int size, int *nbaddet, int *status );

*  Arguments:
*     igrp = Grp * (Given)
*        Group of input NDFs.
*     size = int (Given)
*        Number of elements in igrp
*     nbaddet = int * (Returned)
*        Returned holding the number of bad detectors found. This will
*        equal the number of zeros in the returned nmask array.

*  Returned Value:
*     A pointer to a 1D array with length equal to the maximum number of
*     detectors found in any one input NDF. Each element will be non-zero
*     if, and only if, one or more good data values were found for the
*     detector within the input NDFs. The array should be freed using
*     astFree when no longer needed.

*  Description:
*     This function checks the DATA components of all the input NDF. Any
*     detectors for which no good values are found are flagged by a zero
*     in the returned array.

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     31-MAR-2008 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
#include "star/ndg.h"
#include "ndf.h"
#include "prm_par.h"
#include "sae_par.h"
#include "smf.h"

dim_t *smf_find_bad_dets( Grp *igrp,  int size, int *nbaddet, int *status ){

/* Local Variables */
   dim_t *result;
   dim_t dims[ 3 ];
   dim_t i;
   dim_t j;
   dim_t k;
   dim_t outlen;
   float *p = NULL;
   int ifile;
   int indf;
   int ndim;
   size_t el;

/* Initialise returned values. */
   *nbaddet = 0;
   result = NULL;
   outlen = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Loop round all the input NDFs. */
   for( ifile = 1; ifile <= size && *status == SAI__OK; ifile++ ) {

/* Get an NDF identifier for the input NDF. */
      ndgNdfas( igrp, ifile, "READ", &indf, status );

/* Get the dimensions of the NDF. */
      ndfDim( indf, 3, dims, &ndim, status );

/* If the returned array is shorter than the number of detectors in this
   NDF, extend the returned array and initialise the new elements to
   zero. */
      if( outlen < dims[ 1 ] ) {
         result = astGrow( result, dims[ 1 ], sizeof( *result ) );
         if( astOK ) {
            for( i = outlen; i < dims[ 1 ]; i++ ) result[ i ] = 0;
            outlen = dims[ 1 ];
         }
      }

/* Map the data array of the NDF. */
      ndfMap( indf, "Data", "_REAL", "READ", (void *) &p, &el, status );

/* Loop round all elements of the NDF. */
      for( k = 0; k < dims[ 2 ]; k++ ) {
         for( j = 0; j < dims[ 1 ]; j++ ) {
            for( i = 0; i < dims[ 0 ]; i++ ) {

/* If this data value is good, indicate that the detector has some good
   values, and break out of the "i" loop to move on to the next spectrum. */
               if( *p != VAL__BADR ) {
                  result[ j ] = 1;
                  p += dims[ 0 ] - i;
                  break;
               } else {
                  p++;
               }
            }
         }
      }

/* Annul the input NDF identifier. */
      ndfAnnul( &indf, status );
   }

/* Count the number of bad detectors. */
   for( i = 0; i < outlen; i++ ) {
      if( result[ i ] == 0 ) (*nbaddet)++;
   }

/* Return the result. */
   return result;
}
