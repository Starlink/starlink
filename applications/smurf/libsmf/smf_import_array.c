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
*     void smf_import_array( ThrWorkForce *wf, smfData *refdata,
*                            const char *name, int bad, int expand,
*                            smf_dtype type, void *dataptr, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     refdata = smfData * (Given)
*        Pointer to a smfData that defines the data ordering and dimensions
*        required of the imported NDF.
*     name = const char * (Given)
*        The name of the NDF to be imported.
*     bad = int (Given)
*        Indicates how bad values within the input NDF should be handled:
*        0 - Retain them
*        1 - Replace them with zero.
*        2 - Replace them with the mean value in the time-slice, or with the
*            most recent valid mean time-slice value, if the time-slice has
*            no good values.
*     expand = int (Given)
*        If non-zero, then expand 1D arrays into 3D arrays.
*     type = smf_dtype (Given)
*        Data type of the "dataptr" array. Currently, only SMF__DOUBLE
*        and SMF__INTEGER arrays are supported.
*     dataptr = void * (Given)
*        The array in which to store the imported NDF data values. Must
*        have the same dimensions as "refdata" (butmay have a different
*        data type).
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
*     6-DEC-2012 (DSB):
*        - Improve error messages.
*        - Correct ordering of pixel axes when replacing bad values with
*        time-slice mean values.
*     22-JAN-2013 (DSB):
*        Add argument expand.
*     7-JAN-2014 (DSB):
*        Added support for SMF__INTEGER arrays.
*     10-JAN-2014 (DSB):
*        Added argument wf.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012-2014 Science & Technology Facilities Council.
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
#include "star/thr.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"


void smf_import_array( ThrWorkForce *wf, smfData *refdata, const char *name, int bad,
                       int expand, smf_dtype type, void *dataptr,
                       int *status ){

/* Local Variables: */
   Grp *igrp;                  /* Group holding NDF name */
   dim_t nbolo;                /* Number of bolometers */
   dim_t ntslice;              /* Number of time slices */
   double *pin;                /* Pointer to next input value */
   double *pout;               /* Pointer to next output value */
   int *ipin;                  /* Pointer to next input value */
   int *ipout;                 /* Pointer to next output value */
   double mean;                /* Mean value in the plane */
   double vsum;                /* Sum of good data values */
   int nbad;                   /* Number of bad data values */
   int ngood;                  /* Number of good data values */
   size_t bstride;             /* Stride between bolometer values */
   size_t i;                   /* Loop count */
   size_t j;                   /* Loop count */
   dim_t nel;                  /* Number of elements in array */
   size_t tstride;             /* Stride between time slices */
   smfData *data;              /* Model for one sub-array */

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Attempt to open the NDF. */
   igrp = grpNew( " ", status );
   grpPut1( igrp, name, 0, status );
   smf_open_file( wf, igrp, 1, "READ", SMF__NOTTSERIES, &data, status );
   grpDelet( &igrp, status );

/* Ensure the smfData read from the NDF uses the same data ordering as the
   reference smfData. */
   smf_dataOrder( data, refdata->isTordered, status );
   if( *status == SAI__OK ) {

/* Check the data type and dimensions of the NDF are the same as the
   reference NDF. */
      if( data->dtype != type ) {
         const char *stype = smf_dtype_str( type, status );
         *status = SAI__ERROR;
         errRepf( " ", "NDF '%s' has incorrect data type - should be "
                  "%s.", status, name, stype );

      } else if( data->ndims != refdata->ndims ) {
         *status = SAI__ERROR;
         errRepf( " ", "NDF '%s' is %zu dimensional - must be %zu "
                  "dimensional.", status, name, data->ndims, refdata->ndims );

      } else if( !expand || refdata->ndims != 3 ) {
         expand = 0;

         for( i = 0; i < refdata->ndims; i++ ) {
            if( data->dims[i] != refdata->dims[i] &&
                *status == SAI__OK ){
               *status = SAI__ERROR;
               errRepf( " ", "NDF '%s' has incorrect dimension %zu on "
                        "pixel axis %zu - should be %zu.", status,
                        name, data->dims[i], i + 1, refdata->dims[i] );
            } else if( data->lbnd[i] != refdata->lbnd[i] &&
                *status == SAI__OK ){
               *status = SAI__ERROR;
               errRepf( " ", "NDF '%s' has incorrect lower bound %d on "
                        "pixel axis %zu - should be %d.", status,
                        name, data->lbnd[i], i + 1, refdata->lbnd[i] );
            }
         }

      } else {
         for( i = 0; i < refdata->ndims; i++ ) {

            if( data->dims[i] == 1 ) {

            } else if( data->dims[i] != refdata->dims[i] &&
                *status == SAI__OK ){
               *status = SAI__ERROR;
               errRepf( " ", "NDF '%s' has incorrect dimension %zu on "
                        "pixel axis %zu - should be %zu.", status,
                        name, data->dims[i], i + 1, refdata->dims[i] );
            } else if( data->lbnd[i] != refdata->lbnd[i] &&
                *status == SAI__OK ){
               *status = SAI__ERROR;
               errRepf( " ", "NDF '%s' has incorrect lower bound %d on "
                        "pixel axis %zu - should be %d.", status,
                        name, data->lbnd[i], i + 1, refdata->lbnd[i] );
            }
         }

      }

/* Get the smfData dimensions and strides. */
      smf_get_dims( refdata, NULL, NULL, &nbolo, &ntslice, &nel, &bstride,
                    &tstride, status );

/* Copy the values into the model array, replacing bad values as required. */
      if( *status == SAI__OK && type == SMF__DOUBLE ) {
         pin = data->pntr[0];
         pout = (double *) dataptr;
         if( data->ndims < 3 ) data->dims[2] = 1;
         if( data->ndims < 2 ) data->dims[1] = 1;

         /* Copy the data into the returned array unchanged. */
         if( expand ) {

            pin = (double *) data->pntr[0];
            for( i = 0; i < ntslice; i++,pin++ ) {
               pout = (double *) dataptr + i*tstride;

               for( j = 0; j < nbolo; j++ ) {
                  if( *pin != VAL__BADD ) {
                     *pout = *pin;
                  } else {
                     *pout = VAL__BADD;
                  }
                  pout += bstride;
               }
            }

         } else {
            memcpy( pout, pin, nel*sizeof(*pin) );
         }

/* Retain bad values. */
         if( bad == 0 )  {

/* Replace bad values with zero. */
         } else if( bad == 1 )  {
            pout = (double *) dataptr;
            for( i = 0; i < nel; i++,pout++ ) {
               if( *pout == VAL__BADD ) *pout = 0.0;
            }

/* Replace bad values with the mean value in the time slice. */
         } else if( bad == 2 )  {
            pout = (double *) dataptr;
            mean = VAL__BADD;

            for( i = 0; i < ntslice; i++ ) {
               vsum = 0.0;
               ngood = 0;
               nbad = 0;
               pout = (double *) dataptr + i*tstride;

               for( j = 0; j < nbolo; j++ ) {
                  if( *pout != VAL__BADD ) {
                     vsum += *pout;
                     ngood++;
                  } else {
                     nbad++;
                  }
                  pout += bstride;
               }

               if( ngood > 0 ) mean = vsum/ngood;

               if( nbad > 0 ) {
                  if( mean != VAL__BADD ) {
                     pout = (double *) dataptr + i*tstride;
                     for( j = 0; j < nbolo; j++ ) {
                        if( *pout == VAL__BADD ) *pout = mean;
                        pout += bstride;
                     }
                  } else {
                     *status = SAI__ERROR;
                     errRepf( " ", "NDF '%s' has no good values in plane "
                              "%zu.", status, name, i );
                     break;
                  }
               }
            }
         }

/* Now do the same for integer data. */
      } else if( *status == SAI__OK && type == SMF__INTEGER ) {
         ipin = data->pntr[0];
         ipout = (int *) dataptr;
         if( data->ndims < 3 ) data->dims[2] = 1;
         if( data->ndims < 2 ) data->dims[1] = 1;

         /* Copy the data into the returned array unchanged. */
         if( expand ) {

            ipin = (int *) data->pntr[0];
            for( i = 0; i < ntslice; i++,ipin++ ) {
               ipout = (int *) dataptr + i*tstride;

               for( j = 0; j < nbolo; j++ ) {
                  if( *ipin != VAL__BADI ) {
                     *ipout = *ipin;
                  } else {
                     *ipout = VAL__BADI;
                  }
                  ipout += bstride;
               }
            }

         } else {
            memcpy( ipout, ipin, nel*sizeof(*ipin) );
         }

/* Retain bad values. */
         if( bad == 0 )  {

/* Replace bad values with zero. */
         } else if( bad == 1 )  {
            ipout = (int *) dataptr;
            for( i = 0; i < nel; i++,ipout++ ) {
               if( *ipout == VAL__BADI ) *ipout = 0;
            }

/* Replace bad values with the mean value in the time slice. */
         } else if( bad == 2 )  {
            ipout = (int *) dataptr;
            mean = VAL__BADD;

            for( i = 0; i < ntslice; i++ ) {
               vsum = 0.0;
               ngood = 0;
               nbad = 0;
               ipout = (int *) dataptr + i*tstride;

               for( j = 0; j < nbolo; j++ ) {
                  if( *ipout != VAL__BADI ) {
                     vsum += *ipout;
                     ngood++;
                  } else {
                     nbad++;
                  }
                  ipout += bstride;
               }

               if( ngood > 0 ) mean = vsum/ngood;

               if( nbad > 0 ) {
                  if( mean != VAL__BADD ) {
                     ipout = (int *) dataptr + i*tstride;
                     for( j = 0; j < nbolo; j++ ) {
                        if( *ipout == VAL__BADI ) *ipout = mean;
                        ipout += bstride;
                     }
                  } else {
                     *status = SAI__ERROR;
                     errRepf( " ", "NDF '%s' has no good values in plane "
                              "%zu.", status, name, i );
                     break;
                  }
               }
            }
         }

      } else if( *status == SAI__OK ) {
         const char *stype = smf_dtype_str( type, status );
         *status = SAI__ERROR;
         errRepf( " ", "smf_import_array: Data type '%s' not supported "
                  "(programming error).", status, stype );
      }

/* Close the NDF. */
      smf_close_file( &data, status );
   }
}

