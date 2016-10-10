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
   size_t bstride;
   size_t tstride;
   smf_qual_t *qua_data;
   unsigned char *mask;
} SmfCalcModelPCAData;


void smf_calcmodel_pca( ThrWorkForce *wf, smfDIMMData *dat, int chunk,
                        AstKeyMap *keymap, smfArray **allmodel, int flags,
                        int *status ){

/* Local Variables: */
   AstKeyMap *kmap;
   AstObject *obj;
   SmfCalcModelPCAData *pdata;
   SmfCalcModelPCAData *job_data = NULL;
   dim_t bstep;
   dim_t idx;
   dim_t nbolo;
   dim_t ntslice;
   dim_t tstep;
   double *model_data;
   double *res_data;
   double pcathresh;
   int iw;
   dim_t ipix;
   int nw;
   size_t bstride;
   size_t pcalen;
   size_t tstride;
   smfArray *lut=NULL;
   smfArray *model;
   smfArray *res;
   smf_qual_t *qua_data;
   unsigned char *mask;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

#if HAVE_FEENABLEEXCEPT
feenableexcept(FE_DIVBYZERO| FE_INVALID|FE_OVERFLOW);
#endif

/* Start an AST context to record details of AST Objects created in
   this function. */
   astBegin;

/* Obtain pointers to relevant smfArrays for this chunk */
   res = dat->res[ chunk ];
   lut = dat->lut[ chunk ];
   model = allmodel[ chunk ];

/* Get a pointer to the KeyMap holding parameters controlling the
   common-mode correction model. */
   astMapGet0A( keymap, "PCA", &obj );
   kmap = (AstKeyMap *) obj;

/* Get the required parameter values. */
   astMapGet0D( kmap, "PCATHRESH", &pcathresh );
   astMapGet0I( kmap, "PCALEN", &iw );
   pcalen = iw;

/* Obtain dimensions of the data (assumed to be the same for all subarrays). */
   smf_get_dims( res->sdata[0],  NULL, NULL, &nbolo, &ntslice, NULL,
                 &bstride, &tstride, status );

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
         pdata->bstride = bstride;
         pdata->tstride = tstride;
         pdata->ntslice = ntslice;
         pdata->nbolo = nbolo;
      }
   }

/* If we are inverting the model, just add the model values onto the
   residuals. */
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

/* See if a mask should be used to exclude bright source areas from
   the PCA model. */
      mask = smf_get_mask( wf, SMF__PCA, keymap, dat, flags, status );

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

/* Determine the PCA for each subarray in turn. */
      for( idx = 0; idx < res->ndat && *status == SAI__OK; idx++ ) {

/* Get pointers to data, quality and model for the current subarray. */
         res_data = (res->sdata[idx]->pntr)[0];
         model_data = (model->sdata[idx]->pntr)[0];
         qua_data = smf_select_qualpntr( res->sdata[idx], NULL, status );

/* Save the original residuals in the model. */
         if( *status == SAI__OK ) {
            memcpy(  model_data, res_data, nbolo*ntslice*sizeof(*res_data) );

/* If we are using a mask, flag the residual values that fall within
   the source regions. The gaps thus produced will be filled within
   smf_clean_pca_chunks. */
            if( mask ) {
               for( iw = 0; iw < nw; iw++ ) {
                  pdata = job_data + iw;
                  pdata->model_data = model_data;
                  pdata->qua_data = qua_data;
                  pdata->mask = mask;
                  pdata->lut_data = (lut->sdata[idx]->pntr)[0];
                  pdata->oper = 3;
                  thrAddJob( wf, 0, pdata, smf1_calcmodel_pca, 0, NULL, status );
               }
               thrWait( wf, status );
            }

/* Obtain the required PCA components (i.e. the model values). */
            smf_clean_pca_chunks( wf, model->sdata[idx], pcalen, pcathresh,
                                  1, 0, kmap, status );

/* Subtract the model from the original residuals. */
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
*     smf_calmodel_com.

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
   dim_t nbolo;
   dim_t ntslice;
   dim_t t1;
   dim_t t2;
   double *pm;
   double *pr;
   int *pl;
   size_t bstride;
   size_t ibase;
   size_t tstride;
   smf_qual_t *pq;

/* Check inherited status */
   if( *status != SAI__OK ) return;

#if HAVE_FEENABLEEXCEPT
feenableexcept(FE_DIVBYZERO| FE_INVALID|FE_OVERFLOW);
#endif

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfCalcModelPCAData *) job_data_ptr;

   tstride = pdata->tstride;
   bstride = pdata->bstride;
   nbolo = pdata->nbolo;
   ntslice = pdata->ntslice;
   b1 = pdata->b1;
   b2 = pdata->b2;
   t1 = pdata->t1;
   t2 = pdata->t2;

/* Add the PCA model onto the residuals.
   ==================================== */
   if( pdata->oper == 1 ) {

/* For efficiency, choose the scheme that ensures that the fastest changing
   axis is the inner loop. First case: time is the inner loop. */
      if( tstride == 1 ) {

/* Loop round all bolos to be processed by this thread, maintaining the
   index of the first time slice for the current bolo. */
         ibase = b1*bstride;
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
            ibase += bstride;
         }

/* Second case: bstride must be 1 so bolometer is the inner loop. */
      } else {

/* Loop round all time slices to be processed by this thread, maintaining
   the index of the first bolometer value for the current time slice. */
         ibase = t1*tstride;
         for( itime = t1; itime <= t2; itime++ ) {

/* Get a pointer to the residual, quality and model value for the
   first bolo at the current time slice. */
            pr = pdata->res_data + ibase;
            pq = pdata->qua_data + ibase;
            pm = pdata->model_data + ibase;

/* Loop over all bolometers. */
            for( ibolo = 0; ibolo < nbolo; ibolo++ ) {

/* Remove the SMF__Q_PCA flags. */
               *pq &= ~SMF__Q_PCA;

/* Add the model back onto the residuals, if the sample is modifiable. */
               if( !(*pq & SMF__Q_MOD) ) *pr += *pm;

/* Move pointers on to the next bolometer. */
               pr++;
               pq++;
               pm++;
            }

/* Increment the index of the first value associated with the next
   time slice. */
            ibase += tstride;
         }
      }

/* Form the model as the difference between the original residuals and
   the cleaned residuals.
   =================================================================== */
   } else if( pdata->oper == 2 ) {

/* For efficiency, choose the scheme that ensures that the fastest changing
   axis is the inner loop. First case: time is the inner loop. */
      if( tstride == 1 ) {

/* Loop round all bolos to be processed by this thread, maintaining the
   index of the first time slice for the current bolo. */
         ibase = b1*bstride;
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

/* Subtract off the model value. */
                  if( *pm != VAL__BADD && *pr != VAL__BADD ) {
                     *pr -= *pm;
                  }

/* Move pointers on to the next time sample. */
                  pr++;
                  pq++;
                  pm++;
               }
            }

/* Increment the index of the first value associated with the next
   bolometer. */
            ibase += bstride;
         }

/* Second case: bstride must be 1 so bolometer is the inner loop. */
      } else {

/* Loop round all time slices to be processed by this thread, maintaining
   the index of the first bolometer value for the current time slice. */
         ibase = t1*tstride;
         for( itime = t1; itime <= t2; itime++ ) {

/* Get a pointer to the residual, quality and model value for the
   first bolo at the current time slice. */
            pr = pdata->res_data + ibase;
            pq = pdata->qua_data + ibase;
            pm = pdata->model_data + ibase;

/* Loop over all bolometers. */
            for( ibolo = 0; ibolo < nbolo; ibolo++ ) {

/* Subtract off the model value. */
               if( !( *pq & SMF__Q_BADB ) &&
                   *pm != VAL__BADD && *pr != VAL__BADD ) {
                  *pr -= *pm;
               }

/* Move pointers on to the next bolometer. */
               pr++;
               pq++;
               pm++;
            }

/* Increment the index of the first value associated with the next
   time slice. */
            ibase += tstride;
         }
      }

/* Mask out samples that fall within source regions.
   ================================================= */
   } else if( pdata->oper == 3 ) {

/* For efficiency, choose the scheme that ensures that the fastest changing
   axis is the inner loop. First case: time is the inner loop. */
      if( tstride == 1 ) {

/* Loop round all bolos to be processed by this thread, maintaining the
   index of the first time slice for the current bolo. */
         ibase = b1*bstride;
         for( ibolo = b1; ibolo <= b2; ibolo++ ) {

/* Check that the whole bolometer has not been flagged as bad. */
            if( !( (pdata->qua_data)[ ibase ] & SMF__Q_BADB ) ) {

/* Get a pointer to the first LUT, quality and model value for the
   current bolo. */
               pl = pdata->lut_data + ibase;
               pq = pdata->qua_data + ibase;
               pm = pdata->model_data + ibase;

/* Loop over all time slices. */
               for( itime = 0; itime < ntslice; itime++ ) {

/* Set the model bad if it falls in the source region in the PCA mask. */
                  if( *pl == VAL__BADI || !pdata->mask[ *pl ] ) {
                     *pm = VAL__BADD;
                  }

/* Move pointers on to the next time sample. */
                  pl++;
                  pq++;
                  pm++;
               }
            }

/* Increment the index of the first value associated with the next
   bolometer. */
            ibase += bstride;
         }

/* Second case: bstride must be 1 so bolometer is the inner loop. */
      } else {

/* Loop round all time slices to be processed by this thread, maintaining
   the index of the first bolometer value for the current time slice. */
         ibase = t1*tstride;
         for( itime = t1; itime <= t2; itime++ ) {

/* Get a pointer to the LUT, quality and model value for the
   first bolo at the current time slice. */
            pl = pdata->lut_data + ibase;
            pq = pdata->qua_data + ibase;
            pm = pdata->model_data + ibase;

/* Loop over all bolometers. */
            for( ibolo = 0; ibolo < nbolo; ibolo++ ) {

/* Set the model bad if it falls in the source region in the PCA mask. */
               if( !( *pq & SMF__Q_BADB ) ) {
                  if( *pl == VAL__BADI || !pdata->mask[ *pl ] ) {
                     *pm = VAL__BADD;
                  }
               }

/* Move pointers on to the next bolometer. */
               pl++;
               pq++;
               pm++;
            }

/* Increment the index of the first value associated with the next
   time slice. */
            ibase += tstride;
         }
      }


/* Report an error if the worker was to do an unknown job.
   ====================================================== */
   } else {
      *status = SAI__ERROR;
      errRepf( "", "smf1_calcmodel_pca: Invalid operation (%d) supplied.",
               status, pdata->oper );
   }
}



