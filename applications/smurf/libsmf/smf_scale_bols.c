/*
*+
*  Name:
*     smf_scale_bols

*  Purpose:
*     Scale all bolometer values using factors read from a named NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_scale_bols( smfWorkForce *wf, smfData *data, const smfData * scaledata,
*                          const char *path, const char *param, int *status )

*  Arguments:
*     wf = smfWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     data = smfData * (Given and Returned)
*        The data to be corrected (in-place).
*     scaledata = const smfData * (Given)
*        2-D smfData with dimensions matching the dimensions of "data"
*        and of type _DOUBLE. Each bolometer value in "data" is
*        multiplied by the value of the same bolometer in "scaledata".
*        Bad values are ignored (i.e. no change is made to the corresponding
*        bolometer values - so bad values are equivalent to a value of 1.0).
*     path = const char * (Given)
*        The path to the NDF containing the correction factors. This
*        should be a 2D NDF with pixel bounds (0:31,0:39). Each bolometer
*        value in the supplied smfData is multiplied by the value of the
*        same bolometer in the NDF. Bad values are ignored (i.e. no
*        change is made to the corresponding bolometer values - so bad
*        values are equivalent to a value of 1.0). Will not be accessed if
*        "scaledata" is supplied.
*     param = const char * (Given)
*        The name of the configuration parameter from which the NDF path
*        was obtained. This is included in the error message if the NDF
*        cannot be used. May be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Each bolometer value in the supplied smfData is multiplied by the
*     value of the same bolometer in the supplied NDF.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-AUG-2011 (DSB):
*        Original version.
*     2011-09-06 (TIMJ):
*        Allow a smfData scaling array to be supplied externally.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "ndf.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"

/* Local data types: */
typedef struct smfScaleBolsJobData {
   dim_t nbolo;
   dim_t ntslice;
   dim_t t1;
   dim_t t2;
   double *corr;
   double *dat;
   size_t bstride;
   size_t tstride;
} smfScaleBolsJobData;

/* Prototypes for local functions. */
static void smf1_scale_bols_job( void *job_data, int *status );


void smf_scale_bols( smfWorkForce *wf, smfData *data, const smfData * scaledata,
                     const char *path, const char *param, int *status ){

/* Local Variables */
   dim_t nbolo;
   dim_t ntslice;
   dim_t tstep;
   double *corr = NULL;
   double *dat = NULL;
   dim_t drows = 0;
   dim_t dcols = 0;
   int el;
   int indf = NDF__NOID;
   int iworker;
   int lbnd[ 2 ];
   int ndim;
   int nworker;
   int place;
   int ubnd[ 2 ];
   size_t bstride;
   size_t tstride;
   smfScaleBolsJobData *job_data = NULL;
   smfScaleBolsJobData *pdata = NULL;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Check we have double precision data. */
   if( !smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status ) ) return;

/* Get pointers to data array. */
   dat = data->pntr[ 0 ];

/* Report an error if it is missing. */
   if( !dat ) {
      *status = SAI__ERROR;
      errRep( "", "smf_scale_bols: smfData does not contain a DATA component",
              status );
   }

/* Get the data dimensions and strides. */
   smf_get_dims( data, &drows, &dcols, &nbolo, &ntslice, NULL, &bstride,
                 &tstride, status );

   if (scaledata) {
     dim_t scrows;
     dim_t sccols;

     /* Sanity checks */
     if( !smf_dtype_check_fatal( scaledata, NULL, SMF__DOUBLE, status ) ) return;

     smf_get_dims( scaledata, &scrows, &sccols, NULL, NULL, NULL, NULL, NULL,
                   status );

     if (*status == SAI__OK) {
       if ( drows != scrows || dcols != sccols ) {
         *status = SAI__ERROR;
         smf_smfFile_msg( scaledata->file, "F", 1, "" );
         errRepf( "", "Dimensions of scaling file ^F are (%zu, %zu)"
                  " but flatfield has dimensions (%zu, %zu)",
                  status, (size_t)scrows, (size_t)sccols,
                  (size_t)drows, (size_t)dcols);
       }
     }

     corr = (scaledata->pntr)[0];

   } else if (path) {
/* Open the NDF, check its pixel bounds are as expected, and map the data
   array. */

     ndfOpen( NULL, path, "Read", "Old", &indf, &place, status );
     ndfBound( indf, 2, lbnd, ubnd, &ndim, status );
     if( ( lbnd[ 0 ] != 0 || ubnd[ 0 ] != 31 ||
           lbnd[ 1 ] != 0 || ubnd[ 1 ] != 39 ) && *status == SAI__OK ){
       *status = SAI__ERROR;
       msgSeti( "L1", lbnd[ 0 ] );
       msgSeti( "U1", ubnd[ 0 ] );
       msgSeti( "L2", lbnd[ 1 ] );
       msgSeti( "U2", ubnd[ 1 ] );
       errRep( " ", "The corrections NDF has incorrect pixel bounds "
               "(^L1:^U1,^L2:^U2) - should be (0:31,0:39).", status );
     }
     ndfMap( indf, "Data", "_DOUBLE", "Read", (void *) &corr, &el, status );
   } else {
     if (*status == SAI__OK) {
       *status = SAI__ERROR;
       errRep(" ", "Must supply either scaledata or path argument"
              " (possible programming error)", status );
     }
   }

/* Create structures used to pass information to the worker threads. */
   nworker = wf ? wf->nworker : 1;
   job_data = astMalloc( nworker*sizeof( *job_data ) );

/* Check all pointers can be used safely. */
   if( *status == SAI__OK ) {

/* Determine which time slices are to be processed by which threads. */
      tstep = ntslice/nworker;
      if( tstep < 1 ) tstep = 1;

      for( iworker = 0; iworker < nworker; iworker++ ) {
         pdata = job_data + iworker;
         pdata->t1 = iworker*tstep;
         pdata->t2 = pdata->t1 + tstep - 1;
      }

/* Ensure that the last thread picks up any left-over time slices */
      pdata->t2 = ntslice - 1;

/* Store all the other info needed by the worker threads, and submit the
   jobs to fix the steps in each bolo, and then wait for them to complete. */
      for( iworker = 0; iworker < nworker; iworker++ ) {
         pdata = job_data + iworker;

         pdata->nbolo = nbolo;
         pdata->ntslice = ntslice;
         pdata->bstride = bstride;
         pdata->tstride = tstride;
         pdata->dat = dat;
         pdata->corr = corr;

/* Pass the job to the workforce for execution. */
         smf_add_job( wf, SMF__REPORT_JOB, pdata, smf1_scale_bols_job, 0, NULL,
                      status );
      }

/* Wait for the workforce to complete all jobs. */
      smf_wait( wf, status );
   }

/* Free resources. */
   job_data = astFree( job_data );

/* Close the NDF. */
   if (indf != NDF__NOID) ndfAnnul( &indf, status );

/* If anything went wrong, give a context message. */
   if( *status != SAI__OK ) {
      msgSetc( "NDF", path );
      if( param ) {
         msgSetc( "P", param );
         errRep( " ", "Failed to apply bolometer corrections from NDF '^NDF' "
                 "(specified by config parameter ^P).", status );
      } else {
         errRep( " ", "Failed to apply bolometer corrections from NDF '^NDF'.",
                 status );
      }
   }
}



static void smf1_scale_bols_job( void *job_data, int *status ) {
/*
*  Name:
*     smf1_scale_bols_job

*  Purpose:
*     Apply correction factors to a set of time slices.

*  Invocation:
*     void smf1_scale_bols_job( void *job_data, int *status )

*  Arguments:
*     job_data = void * (Given)
*        Pointer to the data needed by the job. Should be a pointer to a
*        smfScaleBolsJobData structure.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine applys the supplied correction factors to all
*     bolometers in a given range of time slice index.

*/

/* Local Variables: */
   dim_t ibolo;
   dim_t itime;
   dim_t nbolo;
   dim_t ntslice;
   dim_t t1;
   dim_t t2;
   double *corr;
   double *dat;
   double *p1;
   double *p2;
   size_t bstride;
   size_t tstride;
   smfScaleBolsJobData *pdata;
   struct timeval tv1;
   struct timeval tv2;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer to the job data, and then extract its contents into a
   set of local variables. */
   pdata = (smfScaleBolsJobData *) job_data;

   t1 = pdata->t1;
   t2 = pdata->t2;
   nbolo = pdata->nbolo;
   ntslice = pdata->ntslice;
   bstride = pdata->bstride;
   tstride = pdata->tstride;
   dat = pdata->dat;
   corr = pdata->corr;

/* Check we have something to do. */
   if( t1 < ntslice ) {

/* Debugging message indicating thread started work */
      msgOutiff( MSG__DEBUG, "", "smfScaleBols: thread starting on slices %zu -- %zu",
                 status, t1, t2 );
      smf_timerinit( &tv1, &tv2, status);

/* Loop round all time slices to be processed by this thread. */
      for( itime = t1; itime <= t2 && *status==SAI__OK; itime++ ) {

/* Get the pointer to the first bolometer value in the current time slice. */
         p1 = dat + itime*tstride;

/* Get the pointer to the first bolometer value in the correction array. */
         p2 = corr;

/* Loop round every bolometer in the time slice. */
         for( ibolo = 0; ibolo < nbolo; ibolo++,p2++ ) {

/* If both bolometer and corection value are good, scale the bolometer
   value. */
            if( *p1 != VAL__BADD && *p2 != VAL__BADD ) *p1 *= *p2;

/* Get a pointer to the next bolometer value. */
            p1 += bstride;
         }
      }

/* Report the time taken in this thread. */
      msgOutiff( SMF__TIMER_MSG, "",
                 "smfScaleBols: thread finishing slices %zu -- %zu (%.3f sec)",
                 status, t1, t2, smf_timerupdate( &tv1, &tv2, status ) );
   }
}
