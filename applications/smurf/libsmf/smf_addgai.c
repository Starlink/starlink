/*
*+
*  Name:
*     smf_addgai

*  Purpose:
*     Factor in a GAI model into all bolometers in a smfData.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_addgai( ThrWorkForce *wf, smfData *data, const double *gai,
*                 int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads that will do the work.
*     data = smfData * (Given)
*        Pointer to the smfData structure to which the GAI model
*        will be added. Must be time-ordered (see smf_dataOrder.c).
*     gai = const double * (Given)
*        A 2D data array holding the GAI factor for each bolometer.
*        Returns immediately if "gai" is NULL.
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Description:
*     The supplied 2D GAI model values are multiplied into all bolometers
*     in the supplied smfData.

*  Authors:
*     David S Berry (JAC, UClan)
*     {enter_new_authors_here}

*  History:
*     13-MAY-2015 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2015 East Asian Observatory.
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
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Structure containing information about blocks of time slices that each
   thread will process */
typedef struct smfAddGaiData {
   const double *gai;
   dim_t nbolo;
   double *out;
   dim_t t1;
   dim_t t2;
} smfAddGaiData;

/* Prototypes for local functions. */
static void smf1AddGai( void *job_data_ptr, int *status );


void smf_addgai( ThrWorkForce *wf, smfData *data, const double *gai,
                 int *status ){

/* Local Variables */
   dim_t nbolo;
   dim_t ntslice;
   dim_t step;
   int iw;
   int nw;
   smfAddGaiData *job_data;
   smfAddGaiData *pdata;

/* Check the inherited status. */
   if( *status != SAI__OK || !gai ) return;

/* Check supplied smfData is time ordered (i.e. bstride=1, tstride=nbolo). */
   if( !data->isTordered ) {
      *status = SAI__ERROR;
      errRep( " ", "smf_addgai: Supplied smfData is not time-ordered.",
              status );
   }

/* Note the number of time slices and bolometers. */
   smf_get_dims( data, NULL, NULL, &nbolo, &ntslice, NULL, NULL, NULL,
                 status );

/* Store the number of workers in the work force. */
   nw = wf ? wf->nworker : 1;

/* Allocate job data for threads */
   job_data = astMalloc( nw*sizeof(*job_data) );
   if( *status == SAI__OK ) {

/* Get the number of time slices to process in each thread. */
      if( nw > (int) ntslice ) {
         step = 1;
      } else {
         step = ntslice/nw;
      }

/* Set up the job data for each thread. */
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;

/* The first and last time slice to be processed by the thread. */
         pdata->t1 = iw*step;
         if( iw < nw - 1 ) {
            pdata->t2 = ( iw + 1 )*step - 1;
         } else {
            pdata->t2 = ntslice - 1;
         }

/* Pointer to the first output data element for the first time slice. */
         pdata->out = ( (double *) data->pntr[0] ) + pdata->t1*nbolo;

/* Pointer to the GAI data. */
         pdata->gai = gai;

/* Number of bolometers. */
         pdata->nbolo = nbolo;
      }

/* Add each job to the job queue. */
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         (void) thrAddJob( wf, 0, pdata, smf1AddGai, 0, NULL, status );
      }

/* Wait until all of the jobs have completed */
      thrWait( wf, status );
   }

/* Free resources. */
   job_data = astFree( job_data );
}


/* Function to be executed in thread. */
static void smf1AddGai( void *job_data_ptr, int *status ) {

/* Local Variables: */
   const double *gai;
   dim_t ibolo;
   dim_t itime;
   dim_t nbolo;
   dim_t t1;
   dim_t t2;
   double *out;
   smfAddGaiData *pdata;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Pointer to the data that this thread will process */
   pdata = job_data_ptr;

/* Extract values from pdata */
   out = pdata->out;
   t1 = pdata->t1;
   t2 = pdata->t2;
   nbolo = pdata->nbolo;

/* Loop round every time slice to be processed by this thread. */
   for( itime = t1; itime <= t2; itime++ ){

/* Factor in the GAI model into all bolometers. */
      gai = pdata->gai;
      for( ibolo = 0; ibolo < nbolo; ibolo++,out++,gai++ ) {
         if( *gai != VAL__BADD && *out != VAL__BADD ) {
            *out *= *gai;
         } else {
            *out = VAL__BADD;
         }
      }
   }
}



