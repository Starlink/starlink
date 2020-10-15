/*
*+
*  Name:
*     smf_maskacsis

*  Purpose:
*     Remove nominated detectors from an NDF ACSIS extension.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_maskacsis( int *ndf, dim_t *mask, int *status );

*  Arguments:
*     indf = Grp * (Given)
*        The NDF identifier. The NDF should already contains an ACSIS
*        extension. The ACSIS extension will be modified on exit.
*     mask = dim_t * (Given)
*        Pointer to an array which defines which detectors to remove from
*        the ACSIS extension. The length of the array should be equal to
*        the number of detectors described by the ACSIS extension in the
*        supplied NDF. Each element should be zero if the corresponding
*        detector is to be removed, and non-zero if it is to be retained.
*     status = int * (Given and Returned)
*        The inherited status.

*  Description:
*     This function modifies the ACSIS extension in the supplied NDF by
*     removing detectors flagged using the suspplied "mask" array.

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     3-APR-2008 (DSB):
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
#include "ndf.h"
#include "smf.h"
#include "sae_par.h"
#include "star/hds.h"

#define NCOMP 6
#define TEMP_NAME "SMURF_TEMP"

void smf_maskacsis( int indf, dim_t *mask, int *status ){


/* Local Variables */
   HDSLoc *loc1 = NULL;
   HDSLoc *loc2 = NULL;
   HDSLoc *loc3 = NULL;
   char type[ DAT__SZNAM + 1 ];
   dim_t dims_in[ NDF__MXDIM ];
   dim_t dims_out[ NDF__MXDIM ];
   dim_t ndet_in;
   dim_t ndet_out;
   int i;
   int j;
   int maxis;
   int ndim;
   size_t el;
   size_t len;
   void *pin = NULL;
   void *pout = NULL;

   const char *comp[ NCOMP ] = { "RECEPTORS", "FPLANEX", "FPLANEY",
                                 "RECEPPOS", "TSYS", "TRX" };
   int axis[ NCOMP ] = { 0, 0, 0, 1, 0, 0 };


/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Initialise some stuff. */
   ndet_out = 0;

/* Get a locator to the existing ACSIS extension in the supplied NDF. */
   ndfXloc( indf, "ACSIS", "UPDATE", &loc1, status );

/* Loop round each component that is to be masked. */
   for( i = 0; i < NCOMP; i++ ) {

/* Note the index of the detector axis in the current component. */
      maxis = axis[ i ];

/* Get a locator to the component. */
      datFind( loc1, comp[ i ], &loc2, status );

/* Get its data type and shape. */
      datType( loc2, type, status );
      datShape( loc2, NDF__MXDIM, dims_in, &ndim, status );

/* Map the component. */
      datMapV( loc2, type, "READ", &pin, &el, status );

/* If not already done, note the length of the mask array and count the number
   of non-zero values in the supplied mask. This will equal the number of
   detectors in the returned ACSIS extension. */
      if( i == 0 ) {
         ndet_in = dims_in[ maxis ];
         for( j = 0; j < ndet_in; j++ ) {
            if( mask[ j ] ) ndet_out++;
         }
      }

/* Create a new component in the ACSIS extension to hold the masked
   input component values, giving it a temporary name (it will be renamed
   to the correct name later). Get a locator to it and map it. */
      for( j = 0; j < ndim; j++ ) dims_out[ j ] = dims_in[ j ];
      dims_out[ maxis ] = ndet_out;

      datNew( loc1, TEMP_NAME, type, ndim, dims_out, status );
      datFind( loc1, TEMP_NAME, &loc3, status );
      datMapV( loc3, type, "WRITE", &pout, &el, status );

/* Copy the required data from the mapped input array to the mapped
   output array. */
      datLen( loc3, &len, status );
      smf_detmask( type, pin, (int) len, ndim, dims_in, maxis, mask, pout, status );

/* Erase the original component. */
      datAnnul( &loc2, status );
      datErase( loc1, comp[ i ], status );

/* Rename the new component. */
      datRenam( loc3, comp[ i ], status );

/* Free remaining resources. */
      datAnnul( &loc3, status );
   }

/* Free remaining resources. */
   datAnnul( &loc1, status );

}
