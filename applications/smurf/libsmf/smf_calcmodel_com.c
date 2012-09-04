/*
*+
*  Name:
*     smf_calcmodel_com

*  Purpose:
*     Calculate the COMmon-mode model signal component

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_com( ThrWorkForce *wf, smfDIMMData *dat, int
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
*     Calculate the common-mode signal measured at every time-slice.
*
*     For each time slice, find a sigma-clipped mean of all bolometers -
*     including those flagged as "unusual" by previous COM estimations.
*     Then find the gains, offsets and correlation coefficients by
*     comparing each bolometer block against this new COM signal. Flag
*     unusual bolo-blocks with the SMF__Q_COM flag.

*  Notes;
*     - At the moment, the old COM algorithm is still available and can
*     be used by setting COM.OLDALG non-zero. When the new algorithm
*     implemented in this function has been verified sufficiently,
*     support for the old algorithm should probably be removed in order
*     to avoid accumulatrion of historical clutter.
*     - In order to avoid accumulation of historical clutter, this version
*     does not include facilities that are not likely to be used any
*     more. The following keymap items are no longer used: com.boxcar,
*     com.boxfact, com.boxcard, con.boxmin, com .notfirst, com.noremove,
*     gai.flatfield. Uses of these parameters in other places should be
*     removed (e.g. gai.flatfield is used in smf_calcmodel_gai).

*  Authors:
*     David Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     7-MAY-2012 (DSB):
*        Complete re-write of the original smf_calcmodel_com to avoid using
*        different sets of bolometers to estimate COM within each block.
*        This was the approach used by the previous version of
*        smf_calcmodel_com, and led to discontinuities at the edges of
*        blocks, which in turn caused ringing in the FLT model.
*     3-SEP-2012 (DSB):
*        Tidy up the code to clarify the fact that the common mode 
*        esimate is the unweighted mean of the unnormalised bolometer 
*        residuals.

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.
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
#include "star/thr.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Prototypes for local static functions. */
static void smf1_calcmodel_com( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfCalcModelComData {
   dim_t b1;
   dim_t b2;
   dim_t icom;
   dim_t idx_hi;
   dim_t idx_lo;
   dim_t nbolo;
   dim_t ntslice;
   dim_t t1;
   dim_t t2;
   double mean;
   double nsigma;
   double stddev;
   int flag;
   int gain_box;
   int nblock;
   int niter;
   int operation;
   smfArray *gai;
   smfArray *lut;
   smfArray *model;
   smfArray *res;
   unsigned char *mask;
} SmfCalcModelComData;


void smf_calcmodel_com( ThrWorkForce *wf, smfDIMMData *dat, int chunk,
                        AstKeyMap *keymap, smfArray **allmodel, int flags,
                        int *status ){

/* Local Variables: */
   AstKeyMap *kmap;
   AstObject *obj;
   SmfCalcModelComData *job_data = NULL;
   SmfCalcModelComData *pdata;
   dim_t bolostep;
   dim_t gain_box;
   dim_t idx_hi;
   dim_t idx_lo;
   dim_t nbolo;
   dim_t ntslice;
   dim_t timestep;
   double nsigma;
   int *nrej = NULL;
   int iblock;
   int icom;
   int iw;
   int nblock;
   int ncom;
   int niter;
   int nw;
   int oldalg;
   int perarray;
   smfArray *gai;
   smfArray *lut;
   smfArray *model;
   smfArray *res;
   smf_qual_t qmask;
   unsigned char *mask;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Get a pointer to the KeyMap holding parameters controlling the
   common-mode model. */
   astMapGet0A( keymap, "COM", &obj );
   kmap = (AstKeyMap *) obj;

/* If the old algorithm is to be used, invoked it and return. */
   astMapGet0I( kmap, "OLDALG", &oldalg );
   if( oldalg ) {
      kmap = astAnnul( kmap );
      smf_calcmodel_com_old( wf, dat, chunk, keymap, allmodel, flags,
                             status );
      return;

/* If using the new algorithm, check no options have been requested
   that are not supported by the new algorithm. */
   } else {
      int i, temp;
      const char *dead[] = { "BOXCAR", "NOTFIRST", "NOREMOVE" };
      for( i = 0; i < 3; i++ ) {
         astMapGet0I( kmap, dead[ i ], &temp );
         if( temp && *status == SAI__OK ) {
            errRepf( "", "Cannot use COM.%s with new COM algorithm.",
                     status, dead[ i ] );
         }
      }
   }

/* Start an AST context to record details of AST Objects created in
   this function. */
   astBegin;

/* Define a quality mask that defines all samples to be included in the
   new COM estimate. Note, we explicitly exclude the SMF__Q_COM flag from
   this mask so that we do not exclude samples from the new COM estimate
   just because they were flagged as unusual on the previous makemap
   iteration. Some samples (e.g. source samples) may have looked unusual
   on early iterations but may become "normal" on later iterations once
   the source signal has been moved into the AST model. */
   qmask = ( SMF__Q_GOOD & ~SMF__Q_COM );

/* Ensure the data is bolo-ordered (i.e. adjacent values in memory are
   adjacent time slices from the same bolometer). */
   smf_model_dataOrder( dat, NULL, chunk, SMF__RES|SMF__QUA|SMF__GAI|
                        SMF__LUT, 0, status );

/* Obtain pointers to relevant smfArrays for this chunk */
   res = dat->res[ chunk ];
   lut = dat->lut ? dat->lut[ chunk ] : NULL;
   gai = dat->gai ? dat->gai[ chunk ] : NULL;
   model = allmodel[ chunk ];

/* See if a mask should be used to exclude bright source areas from
   the COM model. Cannot mask if no LUT is available. */
   mask = lut ? smf_get_mask( wf, SMF__COM, keymap, dat, flags, status ) : NULL;

/* Get the required configuration parameters. */
   smf_get_nsamp( kmap, "GAIN_BOX", res->sdata[ 0 ], &gain_box, status );
   astMapGet0I( kmap, "PERARRAY", &perarray );
   astMapGet0I( kmap, "NITER", &niter );
   astMapGet0D( kmap, "NSIGMA", &nsigma );

/* If "perarray" is non-zero, a separate common mode signal is calculated
   for each available subarrays and is stored as a separate 1d vector.
   If "perarray" is zero, a single common mode signal is calculated from
   all available subarrays and is stored as a 1d vector. The corresponding
   smfData is at position 0 in the model sdata. Store the number of COM
   models to create. */
   if( perarray ) {
      ncom = model->ndat;
      if( (int) res->ndat != ncom && *status == SAI__OK  ) {
         *status = SAI__ERROR;
         errRep( "", "smf_calcmodel_com: COM model and residuals contain "
                 "different number of data arrays!", status);
      }
   } else {
      ncom = 1;
   }

/* Get the number of time slices in the first residuals smfData. We
   report an error if any subsequent smfData has a different number of
   time slices (except for GAI). */
   smf_get_dims( res->sdata[ 0 ],  NULL, NULL, &nbolo, &ntslice, NULL, NULL,
                 NULL, status );
   if( *status != SAI__OK ) goto L999;

/* Find the number of blocks of time slices per bolometer. Each block
   contains "gain_box" time slices (except possibly for the last time slice
   which may contain more than gain_box but will be less than 2*gain_box).
   Each block of time slices from a single bolometer has its own gain, offset
   and correlation values. */
   nblock = ntslice/gain_box;
   if( nblock == 0 ) nblock = 1;

/* Allocate an array to hold number of blocks rejected - used by
   smf_find_gains, one for each block. Fill them with ones to indicate
   that all blocks should be fitted. */
   if( gai ) {
      nrej = astMalloc( nblock*sizeof( *nrej ) );
      if( *status == SAI__OK ) {
         for( iblock = 0; iblock < nblock; iblock++ ) {
            nrej[ iblock ] = 1;
         }
      }
   }

/* How many threads do we get to play with */
   nw = wf ? wf->nworker : 1;

/* Find how many time slices to process in each worker thread. */
   timestep = ntslice/nw;
   if( timestep == 0 ) timestep = 1;

/* Find how many bolometers to process in each worker thread. */
   bolostep = nbolo/nw;
   if( bolostep == 0 ) bolostep = 1;

/* Allocate job data for threads, and store the range of times slices,
   bolos, etc, to be processed by each one. Ensure that the last thread
   picks up any left-over time slices, bolos, etc.  */
   job_data = astCalloc( nw, sizeof(*job_data) );
   if( *status == SAI__OK ) {
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->t1 = iw*timestep;
         pdata->b1 = iw*bolostep;
         if( iw < nw - 1 ) {
            pdata->t2 = pdata->t1 + timestep - 1;
            pdata->b2 = pdata->b1 + bolostep - 1;
         } else {
            pdata->t2 = ntslice - 1 ;
            pdata->b2 = nbolo - 1 ;
         }

/* Store other values common to all jobs. */
         pdata->ntslice = ntslice;
         pdata->nbolo = nbolo;
         pdata->res = res;
         pdata->gai = gai;
         pdata->lut = lut;
         pdata->model = model;
         pdata->nblock = nblock;
         pdata->gain_box = gain_box;
         pdata->mask = mask;
         pdata->niter = niter;
         pdata->nsigma = nsigma;
      }
   }

/* Loop round creating each COM model. */
   for( icom = 0; icom < ncom && *status == SAI__OK; icom++ ) {

/* Set the index of the first and last subarray that contributes to the
   current COM model. */
      if( perarray ) {
         idx_lo = icom;
         idx_hi = icom;
         msgSeti( "I", icom + 1 );
         msgSeti( "N", ncom );
         msgOutif( MSG__VERB, "", "  Calculating COM model for array ^I of ^N",
                   status );
      } else {
         idx_lo = 0;
         idx_hi = res->ndat - 1;
      }

/* Set up jobs to add the previous estimate of COM back on to the
   residuals, and then wait for the jobs to complete. These jobs also
   clear any SMF__Q_COM flags set by previous iterations. */
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->operation = 1;
         pdata->icom = icom;
         pdata->idx_lo = idx_lo;
         pdata->idx_hi = idx_hi;
         thrAddJob( wf, 0, pdata, smf1_calcmodel_com, 0, NULL, status );
      }
      thrWait( wf, status );

/* If we are using a GAI model, reset gains and correlation coefficients to
   unity and offsets to zero. */
      if( gai ) {
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->operation = 2;
            thrAddJob( wf, 0, pdata, smf1_calcmodel_com, 0, NULL, status );
         }
         thrWait( wf, status );
      }

/* Form a refined COM model. This is just the sigma-clipped mean of the
   residuals at every time slice. */
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->operation = 3;
         thrAddJob( wf, 0, pdata, smf1_calcmodel_com, 0, NULL, status );
      }
      thrWait( wf, status );

/* If we are using a GAI model, evaluate the gains and offsets of each
   bolometer block by doing a least squares linear fit between the residuals
   in the bolometer block, and the current estimate of COM. Additionally
   flag time slices within unusual bolometer blocks with the SMF_Q_COM flag.
   The correlation coefficient for such blocks is set to VAL__BADD. */
      if( gai ) {
         if( perarray ) {
            smf_find_gains( wf, 6, res->sdata[ icom ], mask,
                            lut ? lut->sdata[ icom ] : NULL,
                            model->sdata[ icom ]->pntr[0], kmap, qmask,
                            SMF__Q_COM, gai->sdata[ icom ], nrej, status );
         } else {
            smf_find_gains_array( wf, 6, res, mask, lut,
                                  model->sdata[ icom ]->pntr[0], kmap,
                                  qmask, SMF__Q_COM, gai, nrej, status );
         }
      }

/* Subtract the COM estimate from every bolometer. */
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->operation = 5;
         thrAddJob( wf, 0, pdata, smf1_calcmodel_com, 0, NULL, status );
      }
      thrWait( wf, status );
   }


/* Free resources allocated in this function. */
L999:
   if( nrej ) nrej = astFree( nrej );
   job_data = astFree( job_data );

/* End the AST context, thus deleting any AST objects created in this
   function. */
   astEnd;

/* The kmap pointer was allocated before the ATS context was started and
   so needs to be annulled explicitly. */
   kmap = astAnnul( kmap );
}

static void smf1_calcmodel_com( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_calcmodel_com

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_calmodel_com.

*  Invocation:
*     smf1_calcmodel_com( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfCalcModelComData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfCalcModelComData *pdata;
   dim_t btime;
   size_t gbstride;
   size_t gcstride;
   dim_t ibolo;
   dim_t idx;
   dim_t itime;
   dim_t nbolo;
   dim_t ntslice;
   double *gai_data;
   double *model_data;
   double *pb;
   double *pm;
   double *pr;
   double *pwg;
   double *pwoff;
   double *res_data;
   double *resbuf;
   double *wg;
   double *woff;
   int *pl;
   int iblock;
   size_t izero;
   smf_qual_t *pq;
   smf_qual_t *qua_data;
   smf_qual_t qmask;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfCalcModelComData *) job_data_ptr;

/* Add or remove the previous estimate of COM back to/from the residuals.
   ================================================================== */
   if( pdata->operation == 1 || pdata->operation == 5 ) {

/* Allocate work space */
      wg = astMalloc( ( pdata->t2 - pdata->t1 + 1 )*sizeof( *wg ) );
      woff = astMalloc( ( pdata->t2 - pdata->t1 + 1 )*sizeof( *woff ) );

/* Get a pointer to the model data. The same one is used for all
   subarrays from idx_lo to idx_hi. */
      model_data = pdata->model->sdata[ pdata->icom ]->pntr[ 0 ];

/* Do each required subarray. */
      for( idx = pdata->idx_lo; idx <= pdata->idx_hi; idx++ ) {

/* Get the number of bolometers and time slices for this subarray. */
         smf_get_dims( pdata->res->sdata[ idx ],  NULL, NULL, &nbolo, &ntslice,
                       NULL, NULL, NULL, status );

/* Report an error if the number of time slices or bolometers is not the
   same as for the first array. */
         if( ntslice != pdata->ntslice && *status == SAI__OK ) {
            errRepf( "", "smf1_calcmodel_com: array idx=%d has %d time slices "
                     "- expecting %d.", status, (int) idx, (int) ntslice,
                     (int) pdata->ntslice );
            break;
         } else if( nbolo != pdata->nbolo && *status == SAI__OK ) {
            errRepf( "", "smf1_calcmodel_com: array idx=%d has %d bolometer "
                     "- expecting %d.", status, (int) idx, (int) nbolo,
                     (int) pdata->nbolo );
            break;
         }

/* Get required data pointers. These are known to be bolo-ordered. */
         res_data = pdata->res->sdata[ idx ]->pntr[ 0 ];
         qua_data = smf_select_qualpntr( pdata->res->sdata[ idx ], NULL, status );
         gai_data = pdata->gai ? pdata->gai->sdata[ idx ]->pntr[ 0 ] : NULL;

/* Get the strides in the GAI model. */
         if( gai_data ) {
            smf_get_dims( pdata->gai->sdata[ idx ],  NULL, NULL, NULL, NULL,
                          NULL, &gbstride, &gcstride, status );
         } else {
            gbstride = 0;
            gcstride = 0;
         }

/* Loop over all bolometers. */
         izero = 0;
         for( ibolo = 0; ibolo < nbolo; ibolo++ ) {

/* Skip bad bolometers */
            if( !(qua_data[ izero ] & SMF__Q_BADB ) ) {

/* Get the gain and offset for each time slice being processed by this
   thread. They are returned in "wg" and "woff". */
               smf_gandoff( ibolo, pdata->t1, pdata->t2, ntslice, gbstride,
                            gcstride, gai_data, pdata->nblock, pdata->gain_box,
                            wg, woff, NULL, status );

/* Get pointers to the first time slice to be processed by this thread,
   for the current bolometer. */
               pr = res_data + izero + pdata->t1;
               pq = qua_data + izero + pdata->t1;
               pm = model_data + pdata->t1;
               pwg = wg;
               pwoff = woff;

/* Loop over the time slices to be processed by this thread. */
               for( itime = pdata->t1; itime <= pdata->t2;
                    itime++,pr++,pq++,pm++,pwg++,pwoff++ ) {

/* Scale the common mode using the gain and offset for the current
   bolometer, and add it back onto the residuals. We include samples that
   have been flagged by this function as "unusual" on previous iterations.
   Note, SMF__Q_MOD does not include SMF__Q_COM). */
                  if( !( (*pq) & SMF__Q_MOD ) && (*pm) != VAL__BADD ) {
                     if( pdata->operation == 1 ) {
                        (*pr) += (*pwg)*(*pm) + (*pwoff);

/* Ensure no samples have the SMF__Q_COM flag. */
                        (*pq) &= ~SMF__Q_COM;

                     } else {
                        (*pr) -= (*pwg)*(*pm) + (*pwoff);
                     }
                  }
               }
            }

/* Increment the index to sample zero in the next bolometer. */
            izero += ntslice;
         }
      }

/* Free works space. */
      wg = astFree( wg );
      woff = astFree( woff );

/* Reset GAI model to default value.
   =============================== */
   } else if( pdata->operation == 2 ) {

      for( idx = pdata->idx_lo; idx <= pdata->idx_hi; idx++ ) {
         gai_data = pdata->gai->sdata[ idx ]->pntr[ 0 ];
         gai_data += pdata->b1*pdata->nblock*3;
         for( ibolo = pdata->b1; ibolo <= pdata->b2; ibolo++ ) {
            for( iblock = 0; iblock < pdata->nblock; iblock++ ) {
               *(gai_data++) = 1.0; /* Gain */
            }

            for( iblock = 0; iblock < pdata->nblock; iblock++ ) {
               *(gai_data++) = 0.0; /* Offset */
            }

            for( iblock = 0; iblock < pdata->nblock; iblock++ ) {
               *(gai_data++) = 1.0; /* Correlation coefficient */
            }

         }
      }

/* Form new estimate of the COM model.
   ================================== */
   } else if( pdata->operation == 3 ) {

/* Quality mask that includes samples previously flagged by the COM
   model. */
      qmask = ( SMF__Q_FIT & ~SMF__Q_COM );

/* Store the index of the block containing the first time slice to be
   processed by this thread. Also store the index of the time slice at which
   the next block begins. */
      iblock = pdata->t1/pdata->gain_box;
      if( iblock >= pdata->nblock ) iblock = pdata->nblock - 1;
      if( iblock == pdata->nblock - 1 ){
         btime = pdata->ntslice;
      } else {
         btime = ( iblock + 1 )*pdata->gain_box;
      }

/* Initialise a pointer to the next model value. */
      pm = pdata->model->sdata[ pdata->icom ]->pntr[ 0 ];
      pm += pdata->t1;

/* Buffer to hold all bolometer residuals at a single time slice. */
      resbuf = astMalloc( pdata->nbolo*( pdata->idx_hi - pdata->idx_lo + 1 )*
                          sizeof( *resbuf ) );

/* Loop over the time slices to be processed by this thread. */
      for( itime = pdata->t1; itime <= pdata->t2 && *status == SAI__OK;
           itime++,pm++ ) {

/* If we have reached the first time slice in the next block, increment
   the block index and the index of the first time slice in the next
   block. The last block (index (nblock-1) ) contains any left over time
   slices and so may contain more than gain_box elements. */
         if( itime == btime ) {
            iblock++;
            if( iblock == pdata->nblock - 1 ) {
               btime = pdata->ntslice;
            } else {
               btime += pdata->gain_box;
            }
         }

/* Initialise pointers to the buffers holding the normalised residual,
   and the weights. */
         pb = resbuf;

/* Loop over all subarrays that contribute to the current common-mode
   model. */
         for( idx = pdata->idx_lo; idx <= pdata->idx_hi; idx++ ) {

/* Get required data pointers. These are known to be bolo-ordered.
   Increment them to point to the first time slice processed by this thread. */
            pr = pdata->res->sdata[ idx ]->pntr[ 0 ];
            pr += itime;
            pq = itime + smf_select_qualpntr( pdata->res->sdata[ idx ], NULL, status );
            if( pdata->lut ) {
               pl = pdata->lut->sdata[ idx ]->pntr[ 0 ];
               pl += itime;
            } else {
               pl = NULL;
            }

/* Loop over all bolometers. */
            for( ibolo = 0; ibolo < pdata->nbolo; ibolo++ ) {

/* Check the sample has not been flagged as unusable. */
               if( !(*pq & qmask) && *pr != VAL__BADD ) {

/* If a mask and LUT have been supplied, check that the sample is not
   masked out. If not, store it in the sample buffer. */
                  if( !pdata->mask || !pl || *pl == VAL__BADI ||
                       pdata->mask[ *pl ] ) *(pb++) = *pr;
               }

/* Increment residual and quality pointers to point to the next
   bolometer. */
               pr += pdata->ntslice;
               if( pl ) pl += pdata->ntslice;
               pq += pdata->ntslice;
            }
         }

/* Find the mean of the samples at the current time slice, including
   nsigma-clipping. */
         *pm = smf_sigmaclip( (int)( pb - resbuf ), resbuf, NULL,
                              pdata->nsigma, pdata->niter, NULL, status );
      }

/* Free resources. */
      resbuf = astFree( resbuf );


/* Report an error if the worker was to do an unknown job.
   ====================================================== */
   } else {
      *status = SAI__ERROR;
      errRepf( "", "smf1_calcmodel_com: Invalid operation (%d) supplied.",
               status, pdata->operation );
   }
}








