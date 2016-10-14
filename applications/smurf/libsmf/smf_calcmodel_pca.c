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
   int comfill;
   int iw;
   int nw;
   size_t ipix;
   smfArray **oldgai;
   smfArray **oldres;
   smfArray *lut=NULL;
   smfArray *model;
   smfArray *res;
   smf_qual_t *qua_data;
   unsigned char *mask = NULL;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

#if HAVE_FEENABLEEXCEPT
feenableexcept(FE_DIVBYZERO| FE_INVALID|FE_OVERFLOW);
#endif

/* Start an AST context to record details of AST Objects created in
   this function. */
   astBegin;

/* Ensure the data is bolo-ordered (i.e. adjacent values in memory are
   adjacent time slices from the same bolometer, so tstride is 1 and
   bstride is ntslice). This is the order required and enforced by the
   call to smf_calcmodel_com below. */
   smf_model_dataOrder( wf, dat, NULL, chunk, SMF__RES|SMF__QUA, 0, status );

/* Obtain pointers to relevant smfArrays for this chunk */
   res = dat->res[ chunk ];
   model = allmodel[ chunk ];
   lut = dat->lut[ chunk ];

/* Get a pointer to the KeyMap holding parameters controlling the
   common-mode correction model. */
   astMapGet0A( keymap, "PCA", &obj );
   kmap = (AstKeyMap *) obj;

/* Get the required parameter values. */
   astMapGet0D( kmap, "PCATHRESH", &pcathresh );

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

/* Copy the supplied residuals into the current model arrays. The PCA
   model shares the residuals' quality array. */
      for( idx = 0; idx < res->ndat && *status == SAI__OK; idx++ ) {
         res_data = (res->sdata[idx]->pntr)[0];
         model_data = (model->sdata[idx]->pntr)[0];
         if( *status == SAI__OK ) {
            memcpy(  model_data, res_data, nbolo*ntslice*sizeof(*res_data) );
         }
      }

/* Like FLT, PCA analysis requires all bad values and gaps to be filled
   before doing the analysis. The smf_clean_pca routine will use smf_fillgaps
   to fill gaps, but we seem to get a better PCA model if instead we fill
   gaps using a COM/GAI-like model (i.e. a scaled common-mode). So call
   smf_calcmodel_com to do this, filling the gaps in the PCA model arrays
   initialised above, rather than the main residuals arrays. The
   SMF__DIMM_PCACOM flag tells it to use the COM model to fill gaps in the
   data, rather than subtracting the COM model from the data. It also
   tells it apply any masking requested for the PCA model rather than the
   COM model. Temporarily replace pointers in "dat" so that the PCA model
   is used instead of the residuals, and so that the PCA GAI model is used
   in stead of the COM GAI model.

   Note, doing this is only a good idea if there is a strong common mode
   present in the supplied residuals. This will usually only be the case
   if PCA is the first model in the modelorder. So provide an option to
   omit this COM-filling and instead rely on the usual linear interpolation
   provided by smf_fillgaps. */
      astMapGet0I( kmap, "COMFILL", &comfill );
      if( comfill ) {
         oldgai = dat->gai;
         oldres = dat->res;
         dat->gai = dat->pcagai;
         dat->res = &model;

         msgOutif( MSG__VERB, "", "  Evaluating a COM model as part of "
                   "the PCA model...", status );
         msgOutif( MSG__VERB, "", "  ---------------------------------",
                   status );
         smf_calcmodel_com( wf, dat, 0, keymap, dat->pcacom,
                            flags | SMF__DIMM_PCACOM, status );
         msgOutif( MSG__VERB, "", "  ---------------------------------",
                   status );

         dat->gai = oldgai;
         dat->res = oldres;

/* If "comfill" is non-zero, any requested PCA mask will have been
   applied - and the consequent gaps filled - within smf_calcmodel_com.
   Otherwise, we need to do any requested PCA masking in this function. */
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
      }

/* If comfill is specified, the above results in the PCA model arrays
   containing a copy of the supplied residual data, but with no gaps.
   In other words, all values in the PCA model - except for padding and
   dead bolometers - are representative of the typical time-stream residual
   values. The quality array however is unchanged, except that some samples
   may have been flagged using SMF__Q_PCA - these are the samples where the
   residuals are poorly correlated to the common-mode. We can now proceed
   to do a PCA analysis of each sub-array. */
      for( idx = 0; idx < res->ndat && *status == SAI__OK; idx++ ) {

/* If we are doing masking within this function, flag the residual values
   that fall within the source regions. The gaps thus produced will be
   filled within smf_clean_pca using linear interpolation. */
         if( mask ) {
            res_data = (res->sdata[idx]->pntr)[0];
            model_data = (model->sdata[idx]->pntr)[0];
            qua_data = smf_select_qualpntr( res->sdata[idx], NULL, status );

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

/* Do a PCA analysis of the values in the PCA model arrays. The model
   arrays are returned containing time-stream values made up from the
   strongest components in the data (as determined by pcathresh). These
   are the required final PCA model values. If filling has already been
   done using a COM model, tell smf_clean_pca to fill only gaps flagged
   by smf_calcmodel_com above (using the SMF__Q_PCA bit) before doing the
   analysis. All other gaps have been filled above using the COM model. */
         smf_clean_pca( wf, model->sdata[idx], 0, 0, pcathresh, NULL,
                        NULL, 0, 0, kmap, comfill ? SMF__Q_PCA : SMF__Q_GAP,
                        status );

/* Get pointers to data, quality and PCA model arrays for the current
   subarray. */
         res_data = (res->sdata[idx]->pntr)[0];
         model_data = (model->sdata[idx]->pntr)[0];
         qua_data = smf_select_qualpntr( res->sdata[idx], NULL, status );

/* Subtract the PCA model from the original residuals. */
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
   int *pl;
   size_t ibase;
   smf_qual_t *pq;

/* Check inherited status */
   if( *status != SAI__OK ) return;

#if HAVE_FEENABLEEXCEPT
feenableexcept(FE_DIVBYZERO| FE_INVALID|FE_OVERFLOW);
#endif

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

/* Subtract the model from the original residuals.
   ============================================== */
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
         ibase += ntslice;
      }

/* Mask out samples that fall within source regions.
   ================================================= */
   } else if( pdata->oper == 3 ) {

/* Loop round all bolos to be processed by this thread, maintaining the
   index of the first time slice for the current bolo. */
      ibase = b1*ntslice;
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



