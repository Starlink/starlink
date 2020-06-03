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
*                            const char *dumpdir, const char *name, int bad,
*                            int expand, smf_dtype type, void *dataptr,
*                            int *lut_lbnd, int *lut_ubnd, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     refdata = smfData * (Given)
*        Pointer to a smfData that defines the data ordering and dimensions
*        required of the imported NDF.
*     dumpdir = const char * (Given)
*        The path to the directory from which the file should be read.
*        May be NULL. If not NULL, it is assumed to be terminated with a
*        "/" character.
*     name = const char * (Given)
*        The name of the NDF to be imported. Any leading directory
*        path within this string is ignored if a non-NULL value is supplied
*        for dumpdir, and the path given by dumpdir is used instead.
*     bad = int (Given)
*        Indicates how bad values within the input NDF should be handled:
*        0 - Retain them
*        1 - Replace them with zero.
*        2 - Replace them with the mean value in the time-slice, or with
*            the closest valid mean time-slice value, if the time-slice
*            has no good values.
*     expand = int (Given)
*        If non-zero, then expand 1D arrays into 3D arrays.
*     type = smf_dtype (Given)
*        Data type of the "dataptr" array. Currently, only SMF__DOUBLE
*        and SMF__INTEGER arrays are supported.
*     dataptr = void * (Given)
*        The array in which to store the imported NDF data values. Must
*        have the same dimensions as "refdata" (butmay have a different
*        data type).
*     lut_lbnd = int * (Given)
*        If a LUT is being imported, this should be a pointer to the
*        lower bounds of the map to which the values in the LUT refer.
*        An error is reported if the supplied NDF holds a LUT that refers
*        to a map with different bounds. Ignored if NULL.
*     lut_ubnd = int * (Given)
*        If a LUT is being imported, this should be a pointer to the
*        upper bounds of the map to which the values in the LUT refer.
*        An error is reported if the supplied NDF holds a LUT that refers
*        to a map with different bounds. Ignored if NULL.
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
*        Added argument wf (needed by smf_open_file).
*     14-JAN-2014 (DSB):
*        Multi-thread.
*     14-MAR-2018 (DSB):
*        Added argument "dumpdir".
*     11-MAY-2018 (DSB):
*        Handle cases where there are no good values in the first plane
*        of the supplied NDF.
*     22-MAY-2019 (DSB):
*        Fix broken algorithm for filling in bad values in the "means"
*        array using linear interpolation. This bug could cause catastrophically
*        bad values to be inserted into the EXT model when running
*        skyloop, if the original EXT model had any bad values.
*     3-JUN-2020 (DSB):
*        Added arguments lut_lbnd and lut_ubnd.
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

/* Prototypes for local static functions. */
static void smf1_import_array( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfImportArrayData {
   const char *name;
   int operation;
   size_t i1;
   size_t i2;
   size_t t1;
   size_t t2;
   size_t bstride;
   size_t tstride;
   dim_t nbolo;
   smf_dtype type;
   void *din;
   void *dout;
} SmfImportArrayData;

void smf_import_array( ThrWorkForce *wf, smfData *refdata, const char *dumpdir,
                       const char *name, int bad, int expand, smf_dtype type,
                       void *dataptr, int *lut_lbnd, int *lut_ubnd, int *status ){

/* Local Variables: */
   Grp *igrp;                  /* Group holding NDF name */
   SmfImportArrayData *pdata;
   SmfImportArrayData *job_data = NULL;
   char *ename;
   const char *bn;
   const char *cname;
   dim_t nbolo;                /* Number of bolometers */
   dim_t nel;                  /* Number of elements in array */
   dim_t ntslice;              /* Number of time slices */
   int iw;
   int lbndx;
   int lbndy;
   int nc;
   int nw;
   int ubndx;
   int ubndy;
   size_t bstride;             /* Stride between bolometer values */
   size_t i;                   /* Loop count */
   size_t istep;
   size_t tstep;
   size_t tstride;             /* Stride between time slices */
   smfData *data;              /* Model for one sub-array */

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* If a value is supplied for "dumpdir", use it to replace any directory
   specification in "name". */
   if( dumpdir ) {
      nc = strlen( dumpdir );
      ename = astStore( NULL, dumpdir, nc+1 );

      bn = strrchr( name, '/' );
      if( bn ) {
         bn++;
      } else {
         bn = name;
      }

      ename = astAppendString( ename, &nc, bn );
      cname = ename;
   } else {
      cname = name;
   }

/* Attempt to open the NDF. */
   igrp = grpNew( " ", status );
   grpPut1( igrp, cname, 0, status );
   smf_open_file( wf, igrp, 1, "READ", SMF__NOTTSERIES, &data, status );
   grpDelet( &igrp, status );

/* Ensure the smfData read from the NDF uses the same data ordering as the
   reference smfData. */
   smf_dataOrder( wf, data, refdata->isTordered, status );
   if( *status == SAI__OK ) {

/* Check the data type and dimensions of the NDF are the same as the
   reference NDF. */
      if( data->dtype != type ) {
         const char *stype = smf_dtype_str( type, status );
         *status = SAI__ERROR;
         errRepf( " ", "NDF '%s' has incorrect data type - should be "
                  "%s.", status, cname, stype );

      } else if( data->ndims != refdata->ndims ) {
         *status = SAI__ERROR;
         errRepf( " ", "NDF '%s' is %zu dimensional - must be %zu "
                  "dimensional.", status, cname, data->ndims, refdata->ndims );

      } else if( !expand || refdata->ndims != 3 ) {
         expand = 0;

         for( i = 0; i < refdata->ndims; i++ ) {
            if( data->dims[i] != refdata->dims[i] &&
                *status == SAI__OK ){
               *status = SAI__ERROR;
               errRepf( " ", "NDF '%s' has incorrect dimension %zu on "
                        "pixel axis %zu - should be %zu.", status,
                        cname, data->dims[i], i + 1, refdata->dims[i] );
            } else if( data->lbnd[i] != refdata->lbnd[i] &&
                *status == SAI__OK ){
               *status = SAI__ERROR;
               errRepf( " ", "NDF '%s' has incorrect lower bound %d on "
                        "pixel axis %zu - should be %d.", status,
                        cname, data->lbnd[i], i + 1, refdata->lbnd[i] );
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
                        cname, data->dims[i], i + 1, refdata->dims[i] );
            } else if( data->lbnd[i] != refdata->lbnd[i] &&
                *status == SAI__OK ){
               *status = SAI__ERROR;
               errRepf( " ", "NDF '%s' has incorrect lower bound %d on "
                        "pixel axis %zu - should be %d.", status,
                        cname, data->lbnd[i], i + 1, refdata->lbnd[i] );
            }
         }
      }

/* If LUT bounds have been supplied, check the bounds in the supplied NDF
   match the supplied bounds. */
      if( lut_lbnd || lut_ubnd ) {
         if( !lut_lbnd || !lut_ubnd ) {
            if( *status == SAI__OK ) {
               *status = SAI__ERROR;
               errRep( " ", "smf_import_array: called with one and only "
                       "one of lut_lbnd and lut_ubnd supplied - "
                       "programming error.", status );
            }

         } else {
            ndfXgt0i( data->file->ndfid, SMURF__EXTNAME, "LUT_LBNDX",
                      &lbndx, status );
            ndfXgt0i( data->file->ndfid, SMURF__EXTNAME, "LUT_LBNDY",
                      &lbndy, status );
            ndfXgt0i( data->file->ndfid, SMURF__EXTNAME, "LUT_UBNDX",
                      &ubndx, status );
            ndfXgt0i( data->file->ndfid, SMURF__EXTNAME, "LUT_UBNDY",
                      &ubndy, status );

            if( lbndx != lut_lbnd[ 0 ] || ubndx != lut_ubnd[ 0 ] ||
                lbndy != lut_lbnd[ 1 ] || ubndy != lut_ubnd[ 1 ] ){
               if( *status == SAI__OK ) {
                  *status = SAI__ERROR;
                  errRepf( " ", "NDF '%s' holds a LUT that refers to a map "
                           "with bounds (%d:%d,%d:%d) but the map being created "
                           "has bounds (%d:%d,%d:%d).", status, cname, lbndx,
                            ubndx, lbndy, ubndy, lut_lbnd[ 0 ], lut_ubnd[ 0 ],
                            lut_lbnd[ 1 ], lut_ubnd[ 1 ] );
               }
            }
         }
      }

/* Get the smfData dimensions and strides. */
      smf_get_dims( refdata, NULL, NULL, &nbolo, &ntslice, &nel, &bstride,
                    &tstride, status );

/* How many threads do we get to play with */
      nw = wf ? wf->nworker : 1;

/* Find how many elements and time slices to process in each worker thread. */
      istep = nel/nw;
      if( istep == 0 ) istep = 1;
      tstep = ntslice/nw;
      if( tstep == 0 ) tstep = 1;

/* Allocate job data for threads, and store common values. Ensure that the
   last thread picks up any left-over elements or time slices.  */
      job_data = astCalloc( nw, sizeof(*job_data) );
      if( *status == SAI__OK ) {
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->i1 = iw*istep;
            pdata->t1 = iw*tstep;
            if( iw < nw - 1 ) {
               pdata->i2 = pdata->i1 + istep - 1;
               pdata->t2 = pdata->t1 + tstep - 1;
            } else {
               pdata->i2 = nel - 1;
               pdata->t2 = ntslice - 1;
            }

            pdata->din = data->pntr[0];
            pdata->dout = dataptr;
            pdata->type = type;
            pdata->bstride = bstride;
            pdata->tstride = tstride;
            pdata->nbolo = nbolo;
            pdata->name = cname;
         }
      }

/* Copy the values into the model array, replacing bad values as required. */
      if( *status == SAI__OK ) {
         if( data->ndims < 3 ) data->dims[2] = 1;
         if( data->ndims < 2 ) data->dims[1] = 1;

/* First copy the data into the returned array unchanged. */
         if( expand ) {
            for( iw = 0; iw < nw; iw++ ) {
               pdata = job_data + iw;
               pdata->operation = 1;
               thrAddJob( wf, 0, pdata, smf1_import_array, 0, NULL, status );
            }
            thrWait( wf, status );

         } else {
            memcpy(  dataptr, data->pntr[0], nel*smf_dtype_sz( type, status ) );
         }

/* Retain bad values. */
         if( bad == 0 )  {

/* Replace bad values with zero. */
         } else if( bad == 1 )  {
            for( iw = 0; iw < nw; iw++ ) {
               pdata = job_data + iw;
               pdata->operation = 2;
               thrAddJob( wf, 0, pdata, smf1_import_array, 0, NULL, status );
            }
            thrWait( wf, status );

/* Replace bad values with the mean value in the time slice, or with the
   mean value in the closest non-bad time slice if the whole time slice
   is bad. */
         } else if( bad == 2 )  {
            for( iw = 0; iw < nw; iw++ ) {
               pdata = job_data + iw;
               pdata->operation = 3;
               thrAddJob( wf, 0, pdata, smf1_import_array, 0, NULL, status );
            }
            thrWait( wf, status );
         }
      }

/* Free resources. */
      job_data = astFree( job_data );
      smf_close_file( wf, &data, status );
   }
   if( dumpdir ) ename = astFree( ename );
}

static void smf1_import_array( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_import_array

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_import_array.

*  Invocation:
*     smf1_import_array( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfImportArrayData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfImportArrayData *pdata;
   dim_t nbolo;
   double *pin;
   double *pout;
   int badmean;
   int *ipin;
   int *ipout;
   size_t bstride;
   size_t i1;
   size_t i2;
   size_t i;
   size_t j;
   size_t t1;
   size_t t2;
   size_t tstride;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfImportArrayData *) job_data_ptr;

   i1 = pdata->i1;
   i2 = pdata->i2;
   t1 = pdata->t1;
   t2 = pdata->t2;
   tstride = pdata->tstride;
   bstride = pdata->bstride;
   nbolo = pdata->nbolo;

   if( pdata->operation == 1 ){
      if( pdata->type == SMF__DOUBLE ) {
         pin = (double *) pdata->din + t1;
         for( i = t1; i <= t2; i++,pin++ ) {
            pout = (double *) pdata->dout + i*tstride;
            for( j = 0; j < nbolo; j++ ) {
               if( *pin != VAL__BADD ) {
                  *pout = *pin;
               } else {
                  *pout = VAL__BADD;
               }
               pout += bstride;
            }
         }

      } else if( pdata->type == SMF__INTEGER ) {
         ipin = (int *) pdata->din + t1;
         for( i = t1; i <= t2; i++,pin++ ) {
            ipout = (int *) pdata->dout + i*tstride;
            for( j = 0; j < nbolo; j++ ) {
               if( *ipin != VAL__BADI ) {
                  *ipout = *ipin;
               } else {
                  *ipout = VAL__BADI;
               }
               pout += bstride;
            }
         }

      } else if( *status == SAI__OK ) {
         const char *stype = smf_dtype_str( pdata->type, status );
         *status = SAI__ERROR;
         errRepf( " ", "smf_import_array: Data type '%s' not supported "
                  "(programming error).", status, stype );
      }

   } else if( pdata->operation == 2 ){
      if( pdata->type == SMF__DOUBLE ) {
         pout = (double *) pdata->dout + i1;
         for( i = i1; i <= i2; i++,pout++ ) {
            if( *pout == VAL__BADD ) *pout = 0.0;
         }

      } else if( pdata->type == SMF__INTEGER ) {
         ipout = (int *) pdata->dout + i1;
         for( i = i1; i <= i2; i++,ipout++ ) {
            if( *ipout == VAL__BADI ) *ipout = 0.0;
         }

      } else if( *status == SAI__OK ) {
         const char *stype = smf_dtype_str( pdata->type, status );
         *status = SAI__ERROR;
         errRepf( " ", "smf_import_array: Data type '%s' not supported "
                  "(programming error).", status, stype );
      }

   } else if( pdata->operation == 3 ){
      double *mean;
      double *pend;
      double *pnext;
      double delta;
      double vnext;
      double vprev;
      double vsum;
      int gap;
      size_t *nbad;
      size_t ngood;

      double *means = astMalloc((t2-t1+1)*sizeof(*means));
      size_t *nbads = astMalloc((t2-t1+1)*sizeof(*nbads));
      if( means && nbads ) {
         pend = means + t2 - t1;  /* Pointer to final "means" value */

/* Get the mean value and the number of bad values in each time slice. */
         badmean = 0;
         mean = means;
         nbad = nbads;

         if( pdata->type == SMF__DOUBLE ) {

            for( i = t1; i <= t2; i++,nbad++,mean++ ) {
               vsum = 0.0;
               ngood = 0;
               *nbad = 0;
               pout = (double *) pdata->dout + i*tstride;

               for( j = 0; j < nbolo; j++ ) {
                  if( *pout != VAL__BADD ) {
                     vsum += *pout;
                     ngood++;
                  } else {
                     (*nbad)++;
                  }
                  pout += bstride;
               }
               if( ngood > 0 ) {
                  *mean = vsum/ngood;
               } else {
                  *mean = VAL__BADD;
                  badmean = 1;
               }
            }

         } else if( pdata->type == SMF__INTEGER ) {
            for( i = t1; i <= t2; i++,nbad++,mean++ ) {
               vsum = 0.0;
               ngood = 0;
               *nbad = 0;
               ipout = (int *) pdata->dout + i*tstride;

               for( j = 0; j < nbolo; j++ ) {
                  if( *ipout != VAL__BADI ) {
                     vsum += *ipout;
                     ngood++;
                  } else {
                     (*nbad)++;
                  }
                  ipout += bstride;
               }
               if( ngood > 0 ) {
                  *mean = vsum/ngood;
               } else {
                  *mean = VAL__BADD;
                  badmean = 1;
               }
            }

         } else if( *status == SAI__OK ) {
            const char *stype = smf_dtype_str( pdata->type, status );
            *status = SAI__ERROR;
            errRepf( " ", "smf_import_array: Data type '%s' not supported "
                     "(programming error).", status, stype );
         }

/* Replace any sections of bad values in the array of mean values using
   linear interpolation between the neighbouring good values. Take care
   with any bad values at the start and end. */
         if( badmean && *status == SAI__OK ) {
            mean = means;
            for( i = t1; i <= t2; i++,mean++ ) {
               if( *mean == VAL__BADD ){
                  pnext = mean;
                  while( pnext < pend && *(++pnext) == VAL__BADD ) ;

                  vprev = ( mean > means ) ? mean[-1] : VAL__BADD;
                  vnext = ( *pnext == VAL__BADD )? vprev : *pnext;
                  gap = pnext - mean + 1;
                  if( vprev != VAL__BADD && vnext != VAL__BADD ) {
                     delta = ( vnext - vprev )/gap;

                     vprev += delta;
                     *mean = vprev;
                     pnext = mean;
                     while( pnext < pend && *(++pnext) == VAL__BADD ) {
                        vprev += delta;
                        *pnext = vprev;
                     }

                  } else if( vnext != VAL__BADD ) {
                     pnext = mean;
                     while( pnext < pend && *(++pnext) == VAL__BADD ) {
                        *pnext = vnext;
                     }

                  } else if( vprev != VAL__BADD ) {
                     pnext = mean;
                     while( pnext < pend && *(++pnext) == VAL__BADD ) {
                        *pnext = vprev;
                     }

                  } else {
                     *status = SAI__ERROR;
                     errRepf( " ", "smf_import_array: Large sections of "
                              "NDF '%s' are bad.", status, pdata->name );
                     return;
                  }
               }
            }
         }

/* Now replace any bad values with the mean value for the whole time
   slice. */
         mean = means;
         nbad = nbads;
         if( pdata->type == SMF__DOUBLE ) {
            for( i = t1; i <= t2; i++,nbad++,mean++ ) {
               if( *nbad > 0 ) {
                  pout = (double *) pdata->dout + i*tstride;
                  for( j = 0; j < nbolo; j++ ) {
                     if( *pout == VAL__BADD ) *pout = *mean;
                     pout += bstride;
                  }
               }
            }
         } else if( pdata->type == SMF__INTEGER ) {
            for( i = t1; i <= t2; i++,nbad++,mean++ ) {
               if( *nbad > 0 ) {
                  ipout = (int *) pdata->dout + i*tstride;
                  for( j = 0; j < nbolo; j++ ) {
                     if( *ipout == VAL__BADI ) *ipout = *mean;
                     ipout += bstride;
                  }
               }
            }
         }
      }

      means = astFree( means );
      nbads = astFree( nbads );

   } else {
      *status = SAI__ERROR;
      errRepf( "", "smf1_import_array: Invalid operation (%d) supplied.",
               status, pdata->operation );
   }
}



