/*
*+
*  Name:
*     smf_calcmodel_pca

*  Purpose:
*     Calculate the model holding corrlated time components identified
*     using Princpal Component Analysis.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_pca( ThrWorkForce *wf, smfDIMMData *dat, int
*			 chunk, AstKeyMap *keymap, smfArray
*			 **allmodel, int flags, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     dat = smfDIMMData * (Given)
*        Struct of pointers to information required by model calculation
*     chunk = int (Given)
*        Index of time chunk in allmodel to be calculated
*     keymap = AstKeyMap * (Given)
*        Parameters that control the iterative map-maker
*     allmodel = smfArray ** (Returned)
*        Array of smfArrays (each time chunk) to hold result of model calc
*     flags = int (Given )
*        Control flags: not used
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     The Principal Component Analysis (PCA) model represents a set of
*     strong components within the time streams that are correlated from
*     bolometer to bolometer. It should usually be used as an alternative
*     to the COM (common-mode) model.

*  Authors:
*     David Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     15-JUN-2015 (DSB):
*        Original version.

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

#if HAVE_CONFIG_H
#include <config.h>
#endif

#define _GNU_SOURCE   // gives us feenableexcept on older gcc's
#define __USE_GNU     // gives us feenableexcept on newer gcc's
#include <fenv.h>

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "star/thr.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Local constants */
#define PCATHRESH_FROZEN -123456

/* Prototypes for local static functions. */
static void smf1_calcmodel_pca( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfCalcModelPCAData {
   dim_t b1;
   dim_t b2;
   dim_t nbolo;
   dim_t ntslice;
   dim_t t1;
   dim_t t2;
   double *model_data;
   double *res_data;
   int *lut_data;
   int oper;
   smf_qual_t *qua_data;
   unsigned char *mask;
} SmfCalcModelPCAData;


void smf_calcmodel_pca( ThrWorkForce *wf, smfDIMMData *dat, int chunk,
                        AstKeyMap *keymap, smfArray **allmodel, int flags,
                        int *status ){

/* Local Variables: */
   AstKeyMap *kmap;
   AstObject *obj;
   SmfCalcModelPCAData *job_data = NULL;
   SmfCalcModelPCAData *pdata;
   dim_t bstep;
   dim_t idx;
   dim_t nbolo;
   dim_t ntslice;
   dim_t tstep;
   double *model_data;
   double *res_data;
   double pcathresh;
   double pcathresh_freeze = 0.0;
   double thresh;
   int iw;
   int nw;
   int skip;
   size_t ipix;
   smfArray *lut=NULL;
   smfArray *model;
   smfArray *res;
   smf_qual_t *qua_data;
   unsigned char *mask = NULL;

   static int lmask = 1;
   static int ncomp[4] = {0,0,0,0};

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* feenableexcept(FE_DIVBYZERO| FE_INVALID|FE_OVERFLOW); */

/* Start an AST context to record details of AST Objects created in
   this function. */
   astBegin;

/* Ensure the data is bolo-ordered (i.e. adjacent values in memory are
   adjacent time slices from the same bolometer, so tstride is 1 and
   bstride is ntslice). */
   smf_model_dataOrder( wf, dat, NULL, chunk, SMF__RES|SMF__QUA, 0, status );

/* Obtain pointers to relevant smfArrays for this chunk */
   res = dat->res[ chunk ];
   model = allmodel[ chunk ];
   lut = dat->lut[ chunk ];

/* Get a pointer to the KeyMap holding parameters controlling the
   common-mode correction model. */
   astMapGet0A( keymap, "PCA", &obj );
   kmap = (AstKeyMap *) obj;

/* Get the value that determines how many PCA components to include in
   the model (i.e. how many components to remove from the residuals). */
   astMapGet0D( kmap, "PCATHRESH", &pcathresh );

/* Get the value that determines when (if at all) to freeze the number of PCA
   components removed from the data. */
   astMapGet0D( kmap, "PCATHRESH_FREEZE", &pcathresh_freeze );

/* Obtain dimensions of the data (assumed to be the same for all subarrays). */
   smf_get_dims( res->sdata[0],  NULL, NULL, &nbolo, &ntslice, NULL,
                 NULL, NULL, status );

/* How many threads do we get to play with */
   nw = wf ? wf->nworker : 1;

/* Find how many bolometers or time slices to process in each worker thread. */
   bstep = nbolo/nw;
   if( bstep == 0 ) bstep = 1;

   tstep = ntslice/nw;
   if( tstep == 0 ) tstep = 1;

/* Allocate job data for threads. */
   job_data = astMalloc( nw*sizeof(*job_data) );

/* Store the range of bolos to be processed by each one. Ensure that the
   last thread picks up any left-over bolos. Also store other stuff
   needed by the threads that is the same for all subarrays. */
   if( *status == SAI__OK ) {
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->b1 = iw*bstep;
         pdata->t1 = iw*tstep;
         if( iw < nw - 1 ) {
            pdata->b2 = pdata->b1 + bstep - 1;
            pdata->t2 = pdata->t1 + tstep - 1;
         } else {
            pdata->b2 = nbolo - 1 ;
            pdata->t2 = ntslice - 1 ;
         }

/* Store other values common to all jobs. */
         pdata->ntslice = ntslice;
         pdata->nbolo = nbolo;
      }
   }

/* If we are inverting the model, just add the model values onto the
   residuals. This also clears any SMF__Q_PCA flags set on the previous
   iteration. */
   if( flags & SMF__DIMM_INVERT ) {

/* Process each sub-array in turn. */
      for( idx = 0; idx < res->ndat && *status == SAI__OK; idx++ ) {

         res_data = (res->sdata[idx]->pntr)[0];
         qua_data = smf_select_qualpntr( res->sdata[idx], NULL, status );
         model_data = (model->sdata[idx]->pntr)[0];

         for( iw = 0; iw < nw; iw++ ) {
           pdata = job_data + iw;
           pdata->qua_data = qua_data;
           pdata->model_data = model_data;
           pdata->res_data = res_data;
           pdata->oper = 1;
           thrAddJob( wf, 0, pdata, smf1_calcmodel_pca, 0, NULL, status );
         }
         thrWait( wf, status );

      }

/* If we are calculating a new model... */
   } else {

/* Get the number of AST-skipped iteration. */
      if( astMapGet0A( keymap, "AST", &obj ) ){
         AstKeyMap *kastmap = (AstKeyMap *) obj;
         astMapGet0I( kastmap, "SKIP", &skip );
         kastmap = astAnnul( kastmap );
      }

/* See if a mask should be used to exclude bright source areas from
   the PCA model. */
      mask = smf_get_mask( wf, SMF__PCA, keymap, dat, flags, status );

/* See if the number of PCA components to remove as the PCA model is now
   frozen. If not, the number of components to remove as the PCA model is
   determined by the "PCA.PCATHRESH" parameter. It is frozen if it was
   frozen on a previous iteration (indicated by pcathresh being
   PCATHRESH_FROZEN), we do not need to check. */
      if( pcathresh == PCATHRESH_FROZEN ) {

/* Otherwise, if pcathresh_freeze is positive... */
      } else if( ( pcathresh_freeze > 0 && (

/* ... and pcathresh_freeze is below 1, it specifies a mapchange value. We
   freeze when this mapchange value is first achieved, but only if no PCA
   mask was used on the previous iteration. */
             ( pcathresh_freeze < 1.0 && dat->mapchange < pcathresh_freeze && !lmask ) ||

/* If pcathresh_freeze is above 1, it specifies an integer number of
   iterations, in addition to the initial skip iterations. We freeze
   when we have done this number of iterations. */
             ( pcathresh_freeze >= 1.0 && dat->iter > (int)( pcathresh_freeze + 0.5 ) + skip ) ) )

/* If pcathresh_freeze is negative, we freeze when all skipped iterations
   have been done. */
        ||  ( pcathresh_freeze < 0 && dat->iter > skip ) ) {

/* We are not using pcathresh. Ensure it is never used again, in case the
   normalised change increases again. */
         pcathresh = PCATHRESH_FROZEN;
         astMapPut0D( kmap, "PCATHRESH", pcathresh, NULL );
         msgOutiff(MSG__VERB, "","   No further changes in the number of "
                   "PCA components removed as the PCA model will be made "
                   "because PCA.PCATHRESH_FREEZE is set to %g.", status,
                   pcathresh_freeze );
      }

/* Remember if a mask is being used on this iteration. This flag is
   declared static and is used on the next iteration. */
      lmask = ( mask != NULL );

/* If we have a mask, copy it into the quality array of the map.
   Also set map pixels that are not used (e.g. corner pixels, etc)
   to be background pixels in the mask. Do not do this on the first
   iteration as the map has not yet been determined.*/
      if( mask ) {
         double *map = dat->map;
         smf_qual_t *mapqual = dat->mapqual;
         double *mapvar = dat->mapvar;

         for( ipix=0; ipix<dat->msize; ipix++ ) {
            if( mask[ipix] ) {
               mapqual[ipix] |= SMF__MAPQ_PCA;

            } else if( dat->iter > 0 && ( map[ipix] == VAL__BADD || mapvar[ipix] == VAL__BADD || mapvar[ipix] <= 0.0 ) ) {
               mask[ipix] = 1;
               mapqual[ipix] |= SMF__MAPQ_PCA;

            } else {
               mapqual[ipix] &= ~SMF__MAPQ_PCA;
            }
         }
      }

/* Remove strongly correlated components from each sub-array. */
      for( idx = 0; idx < res->ndat && *status == SAI__OK; idx++ ) {

/* Get pointers to data, quality and PCA model arrays for the current
   subarray. */
         res_data = (res->sdata[idx]->pntr)[0];
         model_data = (model->sdata[idx]->pntr)[0];
         qua_data = smf_select_qualpntr( res->sdata[idx], NULL, status );

/* Copy the supplied residuals into the current model arrays. */
         memcpy(  model_data, res_data, nbolo*ntslice*sizeof(*res_data) );

/* Get the value that determines how many components to remove. If this
   is now frozen, use the number of components stored in the static "ncomp"
   array. */
         thresh = ( pcathresh == PCATHRESH_FROZEN ) ? -ncomp[idx] : pcathresh;

/* Find the most correlated principal components in the data and remove
   them. The number of components removed is returned. */
         ncomp[idx] = smf_pca( wf, res->sdata[idx], lut ? lut->sdata[idx] : NULL,
                               mask, thresh, status );

/* Get the PCA model by subtracting the corrected residuals from the
   original residuals. */
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->model_data = model_data;
            pdata->res_data = res_data;
            pdata->qua_data = qua_data;
            pdata->oper = 2;
            thrAddJob( wf, 0, pdata, smf1_calcmodel_pca, 0, NULL, status );
         }
         thrWait( wf, status );
      }

   }

/* Free resources. */
   job_data = astFree( job_data );

/* End the AST context, thus deleting any AST objects created in this
   function. */
   astEnd;
}

static void smf1_calcmodel_pca( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_calcmodel_pca

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_calmodel_pca.

*  Invocation:
*     smf1_calcmodel_pca( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfCalcModelPCAData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfCalcModelPCAData *pdata;
   dim_t b1;
   dim_t b2;
   dim_t ibolo;
   dim_t itime;
   dim_t ntslice;
   double *pm;
   double *pr;
   size_t ibase;
   smf_qual_t *pq;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* feenableexcept(FE_DIVBYZERO| FE_INVALID|FE_OVERFLOW); */

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfCalcModelPCAData *) job_data_ptr;

   ntslice = pdata->ntslice;
   b1 = pdata->b1;
   b2 = pdata->b2;

/* Add the PCA model onto the residuals.
   ==================================== */
   if( pdata->oper == 1 ) {

/* Loop round all bolos to be processed by this thread, maintaining the
   index of the first time slice for the current bolo. We know tstride
   is 1, and bstride is ntslice. */
      ibase = b1*ntslice;
      for( ibolo = b1; ibolo <= b2; ibolo++ ) {

/* Check that the whole bolometer has not been flagged as bad. */
         if( !( (pdata->qua_data)[ ibase ] & SMF__Q_BADB ) ) {

/* Get a pointer to the first residual, quality and model value for the
   current bolo. */
            pr = pdata->res_data + ibase;
            pq = pdata->qua_data + ibase;
            pm = pdata->model_data + ibase;

/* Loop over all time slices. */
            for( itime = 0; itime < ntslice; itime++ ) {

/* Remove the SMF__Q_PCA flags. */
               *pq &= ~SMF__Q_PCA;

/* Add the model back onto the residuals, if the sample is modifiable. */
               if( !(*pq & SMF__Q_MOD) ) *pr += *pm;

/* Move pointers on to the next time sample. */
               pr++;
               pq++;
               pm++;
            }
         }

/* Increment the index of the first value associated with the next
   bolometer. */
         ibase += ntslice;
      }

/* Get the model by subtracting corrected residuals from the original residuals.
   The supplied model array holds the original residuals.
   ============================================================================ */
   } else if( pdata->oper == 2 ) {

/* Loop round all bolos to be processed by this thread, maintaining the
   index of the first time slice for the current bolo. We know tstride
   is 1 and bstride is ntslice. */
      ibase = b1*ntslice;
      for( ibolo = b1; ibolo <= b2; ibolo++ ) {

/* Check that the whole bolometer has not been flagged as bad. */
         if( !( (pdata->qua_data)[ ibase ] & SMF__Q_BADB ) ) {

/* Get a pointer to the first residual, quality and model value for the
   current bolo. */
            pr = pdata->res_data + ibase;
            pq = pdata->qua_data + ibase;
            pm = pdata->model_data + ibase;

/* Loop over all time slices. */
            for( itime = 0; itime < ntslice; itime++ ) {

/* Get the model value. */
               if( *pm != VAL__BADD && *pr != VAL__BADD ) {
                  if( !(*pq & SMF__Q_MOD) ) *pm -= *pr;
               }

/* Move pointers on to the next time sample. */
               pr++;
               pq++;
               pm++;
            }
         }

/* Increment the index of the first value associated with the next
   bolometer. */
         ibase += ntslice;
      }

/* Report an error if the worker was to do an unknown job.
   ====================================================== */
   } else {
      *status = SAI__ERROR;
      errRepf( "", "smf1_calcmodel_pca: Invalid operation (%d) supplied.",
               status, pdata->oper );
   }
}



