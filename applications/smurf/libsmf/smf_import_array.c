/*
*+
*  Name:
*     smf_import_array

*  Purpose:
*     Import the data array from an NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_import_array( smfData *refdata, const char *name,
*                            int bad, double *dataptr, int *status )

*  Arguments:
*     refdata = smfData * (Given)
*        Pointer to a smfData that defines the data ordering and dimensions
*        required of the imported NDF.
*     name = const char * (Given)
*        The name of the NDF to be imported.
*     bad = int (Given)
*        Indicates how bad values within the input NDF should be handled:
*        0 - Retain them
*        1 - Replace them with zero.
*        2 - Replace them with the mean value in the plane, or with the
*            most recent valid mean plane value, if the plane has no good
*            values.
*     dataptr = double * (Given)
*        The array in which to store the imported NDF data values. Must
*        have the same dimensions as "refdata".
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function imports the data array from a specified NDF into a
*     supplied array, checking that the data ordering and dimensions are the
*     same as for a specified reference smfData. If the data ordering is
*     incorrect, it is changed to the required data ordering, before checking
*     the dimensions. Any bad values in the imported array are replaced by
*     the value indicated by "bad".

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     22-OCT-2012 (DSB):
*        Original version.
*     {enter_further_changes_here}

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
#include "star/grp.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"


void smf_import_array( smfData *refdata, const char *name, int bad,
                       double *dataptr, int *status ) {

/* Local Variables: */
   Grp *igrp;                  /* Group holding NDF name */
   double *p2;                 /* Pointer to start of plane */
   double *pin;                /* Pointer to next input value */
   double *pout;               /* Pointer to next output value */
   double mean;                /* Mean value int he plane */
   double vsum;                /* Sum of good data values */
   int nbad;                   /* Number of bad data values */
   int ngood;                  /* Number of good data values */
   size_t i;                   /* Loop count */
   size_t j;                   /* Loop count */
   size_t k;                   /* Loop count */
   size_t nel = 1;             /* Number of elements in array */
   smfData *data;              /* Model for one sub-array */

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Attempt to open the NDF. */
   igrp = grpNew( " ", status );
   grpPut1( igrp, name, 0, status );
   smf_open_file( igrp, 1, "READ", 0, &data, status );
   grpDelet( &igrp, status );

/* Ensure the smfData read from the NDF uses the same data ordering as the
   reference smfData. */
   smf_dataOrder( data, refdata->isTordered, status );
   if( *status == SAI__OK ) {

/* Check the data type and dimensions of the NDF are the same as the
   reference NDF. */
      if( data->dtype != SMF__DOUBLE ) {
         *status = SAI__ERROR;
         errRepf( " ", "EXT model supplied in '%s' has incorrect data "
                  "type - should be DOUBLE PRECISION.", status, name );

      } else if( data->ndims != refdata->ndims ) {
         *status = SAI__ERROR;
         errRepf( " ", "EXT model supplied in '%s' is %zu dimensional - "
                  "must be %zu dimensional.", status, name, data->ndims,
                  refdata->ndims );

      } else {
         for( i = 0; i < refdata->ndims; i++ ) {
            if( data->dims[i] != refdata->dims[i] &&
                *status == SAI__OK ){
               *status = SAI__ERROR;
               errRepf( " ", "EXT model supplied in '%s' has incorrect "
                        "dimension %zu on pixel axis %zu - should be %zu.", status,
                        name, data->dims[i], i + 1, refdata->dims[i] );
            } else if( data->lbnd[i] != refdata->lbnd[i] &&
                *status == SAI__OK ){
               *status = SAI__ERROR;
               errRepf( " ", "EXT model supplied in '%s' has incorrect "
                        "lower bound %d on pixel axis %zu - should be %d.", status,
                        name, data->lbnd[i], i + 1, refdata->lbnd[i] );
            }
            nel *= data->dims[i];
         }
      }

/* Copy the values into the model array, replacing bad values as required. */
      if( *status == SAI__OK ) {
         pin = data->pntr[0];
         pout = dataptr;
         if( data->ndims < 3 ) data->dims[2] = 1;
         if( data->ndims < 2 ) data->dims[1] = 1;

/* Retain bad valiues. */
         if( bad == 0 )  {
            memcpy( pout, pin, nel*sizeof(*pin) );

/* Replace bad values with zero. */
         } else if( bad == 1 )  {
            for( i = 0; i < nel; i++,pin++ ) {
               if( *pin != VAL__BADD ) {
                  *(pout++) = *pin;
               } else {
                  *(pout++) = 0.0;
               }
            }

/* Replace bad values with the mean value in the plane. */
         } else if( bad == 2 )  {
            mean = VAL__BADD;

            for( i = 0; i < data->dims[2]; i++ ) {
               vsum = 0.0;
               ngood = 0;
               nbad = 0;
               p2 = pout;
               for( j = 0; j < data->dims[1]; j++ ) {
                  for( k = 0; k < data->dims[0]; k++,pin++,pout++ ) {
                     if( *pin != VAL__BADD ) {
                        *pout = *pin;
                        vsum += *pin;
                        ngood++;
                     } else {
                        *pout = VAL__BADD;
                        nbad++;
                     }
                  }
               }

               if( ngood > 0 ) mean = vsum/ngood;

               if( nbad > 0 ) {
                  if( mean != VAL__BADD ) {
                     for( j = 0; j < data->dims[1]; j++ ) {
                        for( k = 0; k < data->dims[0]; k++,p2++ ) {
                           if( *p2 == VAL__BADD ) *p2 = mean;
                        }
                     }
                  } else {
                     *status = SAI__ERROR;
                     errRepf( " ", "smf_import_array: Plane %d has no good values.", status, i );
                     break;
                  }
               }
            }
         }
      }

/* Close the NDF. */
      smf_close_file( &data, status );
   }
}

