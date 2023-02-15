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
*     void smf_scale_bols( ThrWorkForce *wf, smfData *data, const smfData * scaledata,
*                          const char *path, const char *param, int div,
*                          int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     data = smfData * (Given and Returned)
*        The data to be corrected (in-place).
*     scaledata = const smfData * (Given)
*        A 2-D smfData holding the correction values. The dimensions must
*        match the dimensions of "data" and the type must be _DOUBLE.
*        value of 1.0). May be NULL, in which case the correction values
*        are obtained using "path".
*     path = const char * (Given)
*        The path to the NDF containing the correction factors. This
*        should be a 2D NDF with pixel bounds (0:31,0:39). Will not be
*        accessed if "scaledata" is supplied.
*     param = const char * (Given)
*        The name of the configuration parameter from which the NDF path
*        was obtained. This is included in the error message if the NDF
*        cannot be used. May be NULL.
*     div = int (Given)
*        Specifies whether "data" should be divided or multipled by the
*        values in "scaledata" or "path". If non-zero, then divide,
*        otherwise multiply.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Each bolometer value in the supplied smfData "data" is multiplied or
*     divided by the corresponding correction value, depending on the
*     value of "div". The correction values can either be supplied in a
*     smfData (specified by "scaledata") or in a named NDF (specified by
*     "path"). If a correction value is VAL__BADD, and the supplied
*     smfData has an associated quality array, all values for the
*     corresponding bolometer are assigned the qualities SMF__Q_BADEF and
*     SMF__Q_BADB, and the data value is left unchanged. If the supplied
*     smfData does not have an associated quality array, the bolometer
*     values are set to VAL__BADD.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-AUG-2011 (DSB):
*        Original version.
*     2011-09-06 (TIMJ):
*        Allow a smfData scaling array to be supplied externally.
*     19-SEP-2011 (DSB):
*        - Added argument "div".
*        - Set quality for samples with bad corrections, rather than
*        using an implicit correction value of 1.0.
*     20-SEP-2011 (DSB):
*        Change expected NDF pixel bounds to the usual (1:32,1:40).
*     27-SEP-2011 (DSB):
*        Change expected NDF pixel bounds back to (0:31,0:39).

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
   smf_qual_t *qua;
   int div;
   int newbad;
   dim_t bstride;
   dim_t tstride;
} smfScaleBolsJobData;

/* Prototypes for local functions. */
static void smf1_scale_bols_job( void *job_data, int *status );


void smf_scale_bols( ThrWorkForce *wf, smfData *data, const smfData * scaledata,
                     const char *path, const char *param, int div, int *status ){

/* Local Variables */
   dim_t bstride;
   dim_t dcols = 0;
   dim_t drows = 0;
   dim_t lbnd[ 2 ];
   dim_t nbolo;
   dim_t ntslice;
   dim_t tstep;
   dim_t tstride;
   dim_t ubnd[ 2 ];
   double *corr = NULL;
   double *dat = NULL;
   int indf = NDF__NOID;
   int iworker;
   int ndim;
   int nworker;
   int place;
   size_t el;
   smfScaleBolsJobData *job_data = NULL;
   smfScaleBolsJobData *pdata = NULL;
   smf_qual_t *qua = NULL;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Check we have double precision data. */
   if( !smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status ) ) return;

/* Get pointers to data and (optionally) quality arrays. */
   dat = data->pntr[ 0 ];
   qua = smf_select_qualpntr( data, NULL, status );

/* Report an error if the data array is missing. */
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
                  status, (dim_t)scrows, (dim_t)sccols,
                  (dim_t)drows, (dim_t)dcols);
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
       msgSetk( "L1", lbnd[ 0 ] );
       msgSetk( "U1", ubnd[ 0 ] );
       msgSetk( "L2", lbnd[ 1 ] );
       msgSetk( "U2", ubnd[ 1 ] );
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
         pdata->qua = qua;
         pdata->corr = corr;
         pdata->div = div;

/* Pass the job to the workforce for execution. */
         thrAddJob( wf, THR__REPORT_JOB, pdata, smf1_scale_bols_job, 0, NULL,
                      status );
      }

/* Wait for the workforce to complete all jobs. */
      thrWait( wf, status );

/* Report the number of new BADB bolometers. */
      if( job_data->newbad == 1 ) {
         msgOutiff( MSG__VERB, "", "smfScaleBols: rejecting one previously "
                    "good bolometer because of bad scaling factors", status );
      } else if( job_data->newbad > 1 ) {
         msgOutiff( MSG__VERB, "", "smfScaleBols: rejecting %d previously "
                    "good bolometers because of bad scaling factors", status,
                    job_data->newbad );
      }
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
   int newbad;
   dim_t bstride;
   dim_t tstride;
   smfScaleBolsJobData *pdata;
   smf_qual_t *qua;
   smf_qual_t *q1;
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
   qua = pdata->qua;
   corr = pdata->corr;

/* Initialise number of new BADB bolometers */
   newbad = 0;

/* Check we have something to do. */
   if( t1 < ntslice ) {

/* Debugging message indicating thread started work */
      msgOutiff( SMF__TIMER_MSG, "", "smfScaleBols: thread starting on slices %zu -- %zu",
                 status, t1, t2 );
      smf_timerinit( &tv1, &tv2, status);

/* First handle division. */
      if( pdata->div ) {

/* Loop round all time slices to be processed by this thread. */
         for( itime = t1; itime <= t2 && *status==SAI__OK; itime++ ) {

/* Get the pointer to the first bolometer data and quality value in the current
   time slice. */
            p1 = dat + itime*tstride;
            q1 = qua ? qua + itime*tstride : NULL;

/* Get the pointer to the first bolometer value in the correction array. */
            p2 = corr;

/* Loop round every bolometer in the time slice. */
            for( ibolo = 0; ibolo < nbolo; ibolo++,p2++ ) {

/* If the correction value is good, scale the data value if it too is
   good. If the data value is bad, leave it bad. This happens reagdless
   of the quality of the data value. */
               if( *p2 != VAL__BADD ) {
                  if( *p1 != VAL__BADD ) *p1 /= *p2;

/* If the correction value is bad, and a quality array is available, leave
   the data value unchanged but assign the quality SMF__Q_BADEF and
   SMF__Q_BADB to the sample. For the first time slice only, count the
   number of new bolometers that are set to BADB. */
               } else if( q1 ) {
                  if( itime == t1 && !( *q1 & SMF__Q_BADB ) ) newbad++;
                  *q1 |= ( SMF__Q_BADEF | SMF__Q_BADB );

/* If the correction value is bad, and no quality array is available, set
   the data value to VAL__BADD. */
               } else {
                  *p1 = VAL__BADD;
               }

/* Get pointers to the next bolometer data and quality value. */
               p1 += bstride;
               if( q1 ) q1 += bstride;
            }
         }

/* Now handle multiplication in the same way. */
      } else {
         for( itime = t1; itime <= t2 && *status==SAI__OK; itime++ ) {
            p1 = dat + itime*tstride;
            q1 = qua ? qua + itime*tstride : NULL;
            p2 = corr;
            for( ibolo = 0; ibolo < nbolo; ibolo++,p2++ ) {
               if( *p2 != VAL__BADD ) {
                  if( *p1 != VAL__BADD ) *p1 *= *p2;
               } else if( q1 ) {
                  if( itime == t1 && !( *q1 & SMF__Q_BADB ) ) newbad++;
                  *q1 |= ( SMF__Q_BADEF | SMF__Q_BADB );
               } else {
                  *p1 = VAL__BADD;
               }
               p1 += bstride;
               if( q1 ) q1 += bstride;
            }
         }
      }

/* Report the time taken in this thread. */
      msgOutiff( SMF__TIMER_MSG, "",
                 "smfScaleBols: thread finishing slices %zu -- %zu (%.3f sec)",
                 status, t1, t2, smf_timerupdate( &tv1, &tv2, status ) );
   }

/* Return the number of bolometers set to BADB that were not previously
   set to BADB. */
   pdata->newbad = newbad;
}
