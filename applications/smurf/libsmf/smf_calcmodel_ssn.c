/*
*+
*  Name:
*     smf_calcmodel_ssn

*  Purpose:
*     Calculate the model holding scan-synchronous noise corrections

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_ssn( ThrWorkForce *wf, smfDIMMData *dat, int
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
*     The Scan Synchronous Noise (SSN) model represents a noise (probably
*     magnetic pickup in some form) that seems to be a smoothish function
*     mainly of azimuth offset from the tracking centre (although PONGs
*     seem also to have an extra smaller dependency on the absolute rate
*     of change of azimuth offset). Different bolometers have different
*     SSN patterns, so a model is created independently for each bolometer
*     (although the models for neighbouring bolometers seem to be
*     correlated). In addition, the SSN pattern for an individual
*     bolometer can vary with time through an observation. Therefore the
*     time stream for each bolometer is split into several ection, and a
*     separate model created for each section (the final model is the
*     concatenation of these individual section models).
*
*     For each section of each bolometer, the residuals are binned into an
*     (el,az) map in whch the X axis is elevation offset from the tracking
*     centre, and the Y axis is the azimuth offset from the tracking centre.
*     The sigma-clipped mean value in each azimith row is then found to
*     create a profile that gives mean residual value at each azimuth offset.
*     The profile is then smoothed using a 1D Gauassian kernel. These
*     smoothed profiles are then interpolated at the time and azimuth offset
*     of each time slice, and the resulting interpolated value is stored as
*     the SSN modle value, and subtracted from the residuals.
*
*     By itself the above process is heavily influenced by astronomical
*     sources, resulting in erroneous structuyre in the azimuth profile,
*     which in turn causes containing stripes in the final map that run
*     parallel to azimuth. To avoid this, a weight is associated with
*     each value in the (el,az) map. To find these weights, the variance
*     of the data in each row of the (el,az) map is found, and likewise
*     the variance of the data in each column of the (el,az) map is found.
*     The weight for each (el,az) value is then the reciprocal of the
*     smaller of the column variance and the row variance.
*
*     In addition, any section of time slices for which the RMS of the values
*     in the azimuth profile is not significantly larger than the noise
*     level in the data is assumed to have zero SSN. This is because it
*     correcting for a poorly defined SSN is more likely to introduce noise
*     rather than remove noise.

*  Authors:
*     David Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     16-DEC-2014 (DSB):
*        Original version.

*  Copyright:
*     Copyright (C) 2014 Science and Technology Facilities Council.
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

#define WLIM 0.7

/* Prototypes for local static functions. */
static void smf1_calcmodel_ssn( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfCalcModelSSNData {
   dim_t b1;
   dim_t b2;
   dim_t nbolo;
   dim_t ntslice;
   dim_t seclen;
   dim_t t1;
   dim_t t2;
   double *kernel;
   double *model_data;
   double *noi_data;
   double *res_data;
   double binsize;
   double dazhi;
   double dazlo;
   double delhi;
   double dello;
   double thresh;
   double wlim;
   int *lut_data;
   int kernel_length;
   int oper;
   size_t bstride;
   size_t tstride;
   size_t vbstride;
   size_t vtstride;
   smfHead *hdr;
   smf_qual_t *qua_data;
   unsigned char *mask;
   dim_t nzero;
} SmfCalcModelSSNData;


void smf_calcmodel_ssn( ThrWorkForce *wf, smfDIMMData *dat, int chunk,
                        AstKeyMap *keymap, smfArray **allmodel, int flags,
                        int *status ){

/* Local Variables: */
   AstKeyMap *kmap;
   AstObject *obj;
   JCMTState *state;
   SmfCalcModelSSNData *pdata;
   SmfCalcModelSSNData *job_data = NULL;
   dim_t bstep;
   dim_t idx;
   dim_t itime;
   dim_t nbolo;
   dim_t ndata;
   dim_t ntslice;
   dim_t nzero;
   dim_t seclen;
   dim_t tstep;
   dim_t vntslice;
   double *kernel;
   double *model_data;
   double *noi_data;
   double daz_ave;
   double daz_last;
   int ispos;
   dim_t itime_last;
   double psum;
   int np;
   double *res_data;
   double binsize;
   double dazhi;
   double dazlo;
   double delhi;
   double dello;
   double ksigma;
   double ksum;
   double period;
   double thresh;
   int *lut_data=NULL;
   int ibin;
   int iw;
   int kernel_length;
   int notfirst;
   int nsection;
   int nw;
   size_t bstride;
   size_t ipix;
   size_t tstride;
   size_t vbstride;
   size_t vtstride;
   smfArray *lut=NULL;
   smfArray *model;
   smfArray *noi;
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
   noi = dat->noi ? dat->noi[ chunk ] : NULL;
   model = allmodel[ chunk ];

/* Get a pointer to the KeyMap holding parameters controlling the
   common-mode correction model. */
   astMapGet0A( keymap, "SSN", &obj );
   kmap = (AstKeyMap *) obj;

/* Are we skipping the SSN model on this iteration? */
   astMapGet0I( kmap, "NOTFIRST", &notfirst );
   if( notfirst > dat->iter ) {
      msgOutiff( MSG__VERB, "", "    Skipping SSN this "
                 "iteration (iteration %d, ssn.notfirst=%d)", status,
                 dat->iter, notfirst  );
      astEnd;
      return;
   }

/* Get the required parameter values. */
   astMapGet0D( kmap, "THRESH", &thresh );
   astMapGet0D( kmap, "BINSIZE", &binsize );
   astMapGet0D( kmap, "KSIGMA", &ksigma );
   astMapGet0I( kmap, "SECLEN", &iw );
   seclen = iw;

/* The size of the azimuth offset bins used to form the initial model. If
   zero, use the map pixel size. If negative, the bin size is a multiple
   of the map pixel size. If positive, it is given directly in arc-seconds.
   Finally, convert it to radians. */
   if( binsize == 0.0 ) {
      binsize = dat->pixsize;
   } else if( binsize < 0.0 ) {
      binsize = (-binsize)*dat->pixsize;
   }
   binsize = AST__DD2R*(binsize/3600.0);

/* Ensure everything is in the same data order */
   smf_model_dataOrder( wf, dat, NULL, chunk,
                        SMF__LUT|SMF__RES|SMF__QUA|SMF__NOI,
                        lut->sdata[0]->isTordered, status );

/* Obtain dimensions of the data (assumed to be the same for all subarrays). */
   smf_get_dims( res->sdata[0],  NULL, NULL, &nbolo, &ntslice,
                 &ndata, &bstride, &tstride, status );

/* Get the strides etc for the noise array. */
   if( noi ) {
      smf_get_dims( noi->sdata[0], NULL, NULL, NULL, &vntslice,
                    NULL, &vbstride, &vtstride, status );
      if( vntslice == 1 ) vtstride = 0;
   }

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
         pdata->vtstride = vtstride;
         pdata->vbstride = vbstride;
      }
   }

/* If we are inverting the model, just add the model values onto the
   residuals. */
   if( flags & SMF__DIMM_INVERT ) {

/* Process each sub-array in turn. */
      for( idx = 0; idx < res->ndat && *status == SAI__OK; idx++ ) {

/* Get pointers to data/quality/model for the current subarray. */
         res_data = (res->sdata[idx]->pntr)[0];
         qua_data = smf_select_qualpntr( res->sdata[idx], NULL, status );
         model_data = (model->sdata[idx]->pntr)[0];

/* If we are inverting the the model, loop round undoing each old SSN model. */
         for( iw = 0; iw < nw; iw++ ) {
           pdata = job_data + iw;
           pdata->qua_data = qua_data;
           pdata->model_data = model_data;
           pdata->res_data = res_data;
           pdata->oper = 1;
           thrAddJob( wf, 0, pdata, smf1_calcmodel_ssn, 0, NULL, status );
         }
         thrWait( wf, status );

      }

/* If we are calculating a new model... */
   } else {

/* See if a mask should be used to exclude bright source areas from
   the SSN model. */
      mask = smf_get_mask( wf, SMF__SSN, keymap, dat, flags, status );

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
             mapqual[ipix] |= SMF__MAPQ_SSN;

          } else if( dat->iter > 0 && ( map[ipix] == VAL__BADD || mapvar[ipix] == VAL__BADD || mapvar[ipix] <= 0.0 ) ) {
             mask[ipix] = 1;
             mapqual[ipix] |= SMF__MAPQ_SSN;

          } else {
             mapqual[ipix] &= ~SMF__MAPQ_SSN;
          }
        }
      }

/* Create a 1-dimensional Gaussian smoothing kernel. Ensure it has a
   total sum of 1.0.  */
      if( ksigma > 0 ) {
         kernel_length = 10*ksigma + 1;
         kernel = astMalloc( kernel_length*sizeof( *kernel ) );
         ksum = 0;
         if( *status == SAI__OK ) {
            for( ibin = 0; ibin < kernel_length; ibin++ ) {
               double x = ( ibin - kernel_length/2)/ksigma;
               kernel[ ibin ] = exp( -x*x );
               ksum += kernel[ ibin ];
            }
            for( ibin = 0; ibin < kernel_length; ibin++ ) {
               kernel[ ibin ] /= ksum;
            }
         }
      } else {
         kernel = NULL;
      }

/* Find the extreme limits of azimith and elevation offset, in radians.
   Also determine the period between sucessive crossings of azimuth
   offset = 0. */
      state = res->sdata[0]->hdr->allState;
      dazlo = VAL__MAXD;
      dazhi = VAL__MIND;
      dello = VAL__MAXD;
      delhi = VAL__MIND;
      daz_last = 0;
      ispos = 0;
      itime_last = 0;
      psum = 0.0;
      np = -3;
      for( itime = 0; itime < ntslice; itime++,state++ ) {
         if( state->tcs_az_ac1 != VAL__BADD &&
             state->tcs_az_ac2 != VAL__BADD ){
            double daz = state->tcs_az_ac1 - state->tcs_az_bc1;
            if( daz < dazlo ) dazlo = daz;
            if( daz > dazhi ) dazhi = daz;

            daz_ave = 0.5*( daz + daz_last );
            daz_last = daz;
            if( (daz_ave < 0.0 && ispos ) || ( daz_ave > 0.0 && !ispos ) ) {
               ispos = ( daz_ave > 0.0 );
               period = itime - itime_last;
               itime_last = itime;
                  if( np >= 0 ) psum += period; /* ignore the first few since
                                                they may not be accurate */
               np++;
            }

            double del = state->tcs_az_ac2 - state->tcs_az_bc2;
            if( del < dello ) dello = del;
            if( del > delhi ) delhi = del;
         }
      }

      period = 2*psum/np;

/* The scan synchronous noise seems to vary with time during an observation.
   Therefore the time stream from each bolometer is split up into sections,
   and a sepearte SSN model created for each. Get the length of each
   section, in time slices (the "seclen" parameter is a multiple of the
   period found above). */
      seclen *= period;
      if( seclen > 0 && seclen <= ntslice/2 ) {
         nsection = ntslice/seclen;
         seclen = ntslice/nsection;
         msgOutiff( MSG__DEBUG, "", "    Splitting time stream into %d "
                    "sections each of length %zu time slices.", status,
                     nsection, seclen );
      } else {
         nsection = 1;
         seclen = ntslice;
         msgOutiff( MSG__DEBUG, "", "    Using while time stream without "
                    "splitting.", status );
      }

/* Determine the SSN for each subarray in turn. */
      for( idx = 0; idx < res->ndat && *status == SAI__OK; idx++ ) {

/* Get pointers to data/quality/model for the current subarray. */
         model_data = (model->sdata[idx]->pntr)[0];
         res_data = (res->sdata[idx]->pntr)[0];
         lut_data = (lut->sdata[idx]->pntr)[0];
         qua_data = smf_select_qualpntr( res->sdata[idx], NULL, status );
         noi_data = noi ? (noi->sdata[idx]->pntr)[0] : NULL;

/* Calculate the SSN model. Each thread handles a group of bolometers. */
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->qua_data = qua_data;
            pdata->model_data = model_data;
            pdata->res_data = res_data;
            pdata->noi_data = noi_data;
            pdata->lut_data = lut_data;
            pdata->hdr = res->sdata[idx]->hdr;
            pdata->dazlo = dazlo;
            pdata->dazhi  = dazhi;
            pdata->dello = dello;
            pdata->delhi  = delhi;
            pdata->kernel = kernel;
            pdata->kernel_length = kernel_length;
            pdata->mask = mask;
            pdata->binsize = binsize;
            pdata->seclen = seclen;
            pdata->thresh = thresh;
            pdata->oper = 3;
            pdata->wlim = WLIM;
            thrAddJob( wf, 0, pdata, smf1_calcmodel_ssn, 0, NULL, status );
         }
         thrWait( wf, status );

/* Add up and display the total number of time slices for which an SSN
   value fo zero was assumed. */
         nzero = 0;
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            nzero += pdata->nzero;
         }
         msgOutiff( MSG__DEBUG, "", "    %.1f %% of data in subarray %zu "
                    "has been given an SSN value of zero.", status,
                    100.0*(double)nzero/(double)(ntslice*nbolo), idx );
      }

/* Subtract the SSN model from the residuals. */
      for( idx = 0; idx < res->ndat && *status == SAI__OK; idx++ ) {
         res_data = (res->sdata[idx]->pntr)[0];
         model_data = (model->sdata[idx]->pntr)[0];
         lut_data = (lut->sdata[idx]->pntr)[0];
         qua_data = smf_select_qualpntr( res->sdata[idx], NULL, status );

         for( iw = 0; iw < nw; iw++ ) {
           pdata = job_data + iw;
           pdata->qua_data = qua_data;
           pdata->res_data = res_data;
           pdata->model_data = model_data;
           pdata->oper = 2;
           thrAddJob( wf, 0, pdata, smf1_calcmodel_ssn, 0, NULL, status );
         }
         thrWait( wf, status );
      }

/* Free resources. */
      kernel = astFree( kernel );
   }
   job_data = astFree( job_data );

/* End the AST context, thus deleting any AST objects created in this
   function. */
   astEnd;
}

static void smf1_calcmodel_ssn( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_calcmodel_ssn

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_calmodel_com.

*  Invocation:
*     smf1_calcmodel_ssn( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfCalcModelSSNData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   JCMTState *state;
   SmfCalcModelSSNData *pdata;
   dim_t b1;
   dim_t b2;
   dim_t ibolo;
   dim_t itime;
   dim_t itime_hi;
   dim_t itime_lo;
   dim_t nbolo;
   dim_t ntotal;
   dim_t ntslice;
   dim_t seclen;
   dim_t t1;
   dim_t t2;
   double *prof_az;
   double *prof_azhi;
   double *prof_azlo;
   double *prof_azs;
   double *kernel;
   double *pm;
   double *pr;
   double *ps;
   double *pv;
   double *pw;
   double *sum;
   double *var_az;
   double *var_el;
   double *wgt_az;
   double *wsum;
   double binsize;
   double c1;
   double c2;
   double c3;
   double c4;
   double daz;
   double dazhi;
   double dazlo;
   double del;
   double delhi;
   double dello;
   double faz;
   double fhi;
   double flo;
   double jsum;
   double ksum;
   double kwsum;
   double sum1;
   double sum2;
   double thresh;
   double v;
   double vhi;
   double vlo;
   double waz;
   double wel;
   double wgt;
   double wlim;
   int *pl;
   int ibin;
   int ibin_az;
   int ibin_el;
   int ibin_hi;
   int ibin_lo;
   int ihi;
   int ilo;
   int isection;
   int jbin;
   int kernel_length;
   int msum;
   int nbin;
   int nbin_az;
   int nbin_el;
   int ngood;
   int nsection;
   size_t bstride;
   size_t ibase0;
   size_t ibase;
   size_t ivbase;
   size_t tstride;
   size_t vbstride;
   size_t vtstride;
   smfHead *hdr;
   smf_qual_t *pq;
   unsigned char *mask;

/* Check inherited status */
   if( *status != SAI__OK ) return;

#if HAVE_FEENABLEEXCEPT
feenableexcept(FE_DIVBYZERO| FE_INVALID|FE_OVERFLOW);
#endif

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfCalcModelSSNData *) job_data_ptr;

   tstride = pdata->tstride;
   bstride = pdata->bstride;
   vtstride = pdata->vtstride;
   vbstride = pdata->vbstride;
   nbolo = pdata->nbolo;
   ntslice = pdata->ntslice;
   b1 = pdata->b1;
   b2 = pdata->b2;
   t1 = pdata->t1;
   t2 = pdata->t2;
   hdr = pdata->hdr;
   binsize = pdata->binsize;
   dazlo = pdata->dazlo;
   dazhi = pdata->dazhi;
   dello = pdata->dello;
   delhi = pdata->delhi;
   kernel_length = pdata->kernel_length;
   kernel = pdata->kernel;
   mask = pdata->mask;
   wlim = pdata->wlim;
   thresh = pdata->thresh;

/* Add the SSN model onto the residuals.
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

/* Remove the SMF__Q_SSN flags. */
                  *pq &= ~SMF__Q_SSN;

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

/* Subtract the SSN model from the residuals.
   ======================================== */
   } else if( pdata->oper == 2 ) {
      ibase = b1*bstride;
      for( ibolo = b1; ibolo <= b2 && *status == SAI__OK; ibolo++ ) {

         if( !( (pdata->qua_data)[ ibase ] & SMF__Q_BADB ) ) {
            pr = pdata->res_data + ibase;
            pq = pdata->qua_data + ibase;
            pm = pdata->model_data + ibase;

            for( itime = 0; itime < ntslice; itime++ ) {
               if( *pm == VAL__BADD ) {
                  *pm = 0.0;
                  *pq |= SMF__Q_SSN;
               } else if( !(*pq & SMF__Q_MOD) && *pr != VAL__BADD ) {
                  *pr -= *pm;
               }
               pr += tstride;
               pq += tstride;
               pm += tstride;
            }
         }
         ibase += bstride;
      }

/* Form initial estimate of SSN in each bolo.
   ========================================== */
   } else if( pdata->oper == 3 ) {

/* Determine the number of time slices in each section, and the number of
   sections. A separate SSN model is calculated for each section, for
   each bolometer. This allows for time variation in the scan-synchronous
   noise. */
      seclen = pdata->seclen;
      nsection = ntslice/seclen;
      seclen = ntslice/nsection;

/* Set up the constants needed to convert (az,el) offsets (in radians) into
   bin indicies. */
      nbin_az = 4 + ( dazhi - dazlo )/binsize;
      c1 = 1.0/binsize;
      c2 = 0.5*( nbin_az - (dazlo + dazhi)/binsize );

      nbin_el = 4 + ( delhi - dello )/binsize;
      c3 = 1.0/binsize;
      c4 = 0.5*( nbin_el - (dello + delhi)/binsize );

/* Total number of bins in the (az,el) histogram. */
      nbin = nbin_az*nbin_el;

/* Allocate work arrays. */
      sum = astMalloc( nbin*sizeof( *sum ) );
      wsum = astMalloc( nbin*sizeof( *wsum ) );
      var_az = astMalloc( nbin_az*sizeof( *var_az ) );
      var_el = astMalloc( nbin_el*sizeof( *var_el ) );
      prof_azs = astMalloc( nsection*nbin_az*sizeof( *prof_azs ) );
      wgt_az = astMalloc( nbin_az*sizeof( *wgt_az ) );

/* Process each usable bolometer. */
      pdata->nzero = 0;
      ibase = b1*bstride;
      ivbase = b1*vbstride;
      for( ibolo = b1; ibolo <= b2 && *status == SAI__OK; ibolo++ ) {
         if( !( (pdata->qua_data)[ ibase ] & SMF__Q_BADB ) ) {

/* Process each section of the time stream from the current bolometer. */
            for( isection = 0; isection < nsection; isection++ ) {

/* The final azimuth profiles for all sections are be stored in the prof_azs
   array. Find the start of the profile for the current section. */
               prof_az = prof_azs + isection*nbin_az;

/* Get the index of the first and last time slice in the current section. */
               itime_lo = isection*seclen;
               itime_hi = itime_lo + seclen - 1;
               if( itime_hi >= ntslice ) itime_hi = ntslice - 1;

/* Offset to the first sample in the current section. */
               ibase0 = ibase + itime_lo*tstride;

/* Bin the data for the current section of the current bolometer into a map
   in which the axes are azimuth and elevation offset. If required, ignore
   samples that fall in the area covered by the SSN mask. */
               memset( sum, 0, nbin*sizeof( *sum ) );
               memset( wsum, 0, nbin*sizeof( *wsum ) );

               pr = pdata->res_data + ibase0;
               pq = pdata->qua_data + ibase0;
               pl = pdata->lut_data + ibase0;
               pv = pdata->noi_data ? pdata->noi_data + ivbase : NULL;

               ntotal = 0;

               state = hdr->allState + itime_lo;
               for( itime = itime_lo; itime <=itime_hi; itime++,state++ ) {

                  if( state->tcs_az_ac1 != VAL__BADD &&
                      state->tcs_az_ac2 != VAL__BADD &&
                      !(*pq & SMF__Q_FIT) && *pr != VAL__BADD &&
                      *pl != VAL__BADI && ( !mask || mask[ *pl ] ) ) {

                     daz = state->tcs_az_ac1 - state->tcs_az_bc1;
                     del = state->tcs_az_ac2 - state->tcs_az_bc2;
                     ibin_az = daz*c1 + c2;
                     ibin_el = del*c3 + c4;
                     if( ibin_az >= 0 && ibin_az < nbin_az &&
                         ibin_el >= 0 && ibin_el < nbin_el ) {

                        ibin = ibin_el + ibin_az*nbin_el;

                        if( pv ) {
                           wgt = 1.0/(*pv);
                           sum[ ibin ] += wgt*(*pr);
                           wsum[ ibin ] += wgt;
                        } else {
                           sum[ ibin ] += *pr;
                           wsum[ ibin ] += 1.0;
                        }
                        ntotal++;
                     }
                  }

                  pr += tstride;
                  pq += tstride;
                  pl += tstride;
                  if( pv ) pv += vtstride;
               }

               for( ibin = 0; ibin < nbin; ibin++ ){
                  if( wsum[ ibin ] > 0.0 ) {
                     sum[ ibin ] /= wsum[ ibin ];
                  } else {
                     sum[ ibin ] = VAL__BADD;
                  }
               }

/* If the current section does not contain much valid data, just re-use the
   existing azimuth profile ("prof_az") from the previous section (if any).
   The SSN model does not vary very quickly with time. */
               if( ntotal < seclen/2 ) {
                  if( isection == 0 ) {
                     for( ibin_az = 0; ibin_az < nbin_az; ibin_az++ ) {
                        prof_az[ ibin_az ] = 0.0;
                     }
                     pdata->nzero += seclen;
                  } else {
                     ps = prof_az - nbin_az;
                     for( ibin_az = 0; ibin_az < nbin_az; ibin_az++ ) {
                        prof_az[ ibin_az ] = *(ps++);
                     }
                  }

/* Otherwise find a new SSN model for the current section. */
               } else if( *status == SAI__OK ) {

/* First job is to identify source regions so that they can be excluded
   form the estimation of the SSN. To do this we identify rows and
   columns in the (el,az) map that have high variance (i.e. source may be
   present) and give them low weight. First get a 1D array holding the
   variance of the values in each elevation column of the above map. Ignore
   rows that have good values for less than one quarter of the full range
   of azimuth. */
                  for( ibin_el = 0; ibin_el < nbin_el; ibin_el++ ) {
                     ps = sum + ibin_el;
                     msum = 0;
                     sum1 = 0.0;
                     sum2 = 0.0;

                     for( ibin_az = 0; ibin_az < nbin_az; ibin_az++ ) {
                        if( *ps != VAL__BADD ) {
                           v = *ps;
                           sum1 += v;
                           sum2 += v*v;
                           msum++;
                        }
                        ps += nbin_el;
                     }

/* Ignore columns that have good values for less than one quarter of the full
   range of azimuth (such rows may have systematically different variances
   to the other because they sample a smaller range of the SSN ramp). */
                     if( msum > nbin_az/4 ) {
                        sum1 /= msum;
                        var_el[ ibin_el ] = sum2/msum - sum1*sum1;
                     } else {
                        var_el[ ibin_el ] = VAL__BADD;
                     }

                  }

/* Find the mean of the variances, using sigma-clipping to ignore extreme
   values (e.g. columns that pass though a source region). */
                  sum1 = smf_sigmaclipD( nbin_el, var_el, NULL, 2.0, 3, NULL,
                                         status );

/* Normalize the variances to the above mean value. */
                  if( sum1 != VAL__BADD ) {
                     for( ibin_el = 0; ibin_el < nbin_el; ibin_el++ ) {
                        if( var_el[ ibin_el ] != VAL__BADD ) {
                           var_el[ ibin_el ] /= sum1;
                        }
                     }
                  } else {
                     for( ibin_el = 0; ibin_el < nbin_el; ibin_el++ ) {
                        var_el[ ibin_el ] = VAL__BADD;
                     }
                  }

/* Similarly, get a 1D array holding the variance of the values in each
   azimuth row of the (el,az) map. */
                  ps = sum;
                  for( ibin_az = 0; ibin_az < nbin_az; ibin_az++ ) {
                     msum = 0;
                     sum1 = 0.0;
                     sum2 = 0.0;
                     for( ibin_el = 0; ibin_el < nbin_el; ibin_el++,ps++ ) {
                        if( *ps != VAL__BADD ) {
                           sum1 += *ps;
                           sum2 += (*ps)*(*ps);
                           msum++;
                        }
                     }

                     if( msum > nbin_el/4 ) {
                        sum1 /= msum;
                        var_az[ ibin_az ] = sum2/msum - sum1*sum1;
                     } else {
                        var_az[ ibin_az ] = VAL__BADD;
                     }

                  }

/* Find the mean of the variances, using sigma-clipping to ignore extreme
   values (e.g. rows that pass though a source region). */
                  sum1 = smf_sigmaclipD( nbin_az, var_az, NULL, 2.0, 3, NULL,
                                         status );

/* Normalize the variances to the above mean value. */
                  if( sum1 != VAL__BADD ) {
                     for( ibin_az = 0; ibin_az < nbin_az; ibin_az++ ) {
                        if( var_az[ ibin_az ] != VAL__BADD ) {
                           var_az[ ibin_az ] /= sum1;
                        }
                     }
                  }

/* Set up a map ("wsum") holding the weight for each pixel in the (el,az) map.
   Each weight is the maximum of the weight associated with the row and column
   that intersect at the pixel (weights are the reciprocal of the corresponding
   variances). This results in low weights for regions that have a high
   variance (e.g. source regions). */
                  ps = sum;
                  pw = wsum;
                  for( ibin_az = 0; ibin_az < nbin_az; ibin_az++ ) {
                     waz = var_az[ ibin_az ];
                     if( waz != VAL__BADD && waz > 0.0 ) {
                        waz = 1.0/waz;
                     } else {
                        waz = 0.0;
                     }

                     sum1 = 0.0;
                     msum = 0;

                     for( ibin_el = 0; ibin_el < nbin_el; ibin_el++,pw++,ps++ ) {
                        if( *ps != VAL__BADD ) {
                           wel = var_el[ ibin_el ];
                           if( wel != VAL__BADD && wel > 0.0 ) {
                              wel = 1.0/wel;
                           } else {
                              wel = 0.0;
                           }
                           *pw = ( waz > wel ) ? waz : wel;
                           if( *pw > 0.0 ) {
                              sum1 += *pw;
                              msum++;
                           } else {
                              *pw = VAL__BADD;
                           }
                        } else {
                           *pw = VAL__BADD;
                        }
                     }

/* Normalize the weights for each azimuth row so that they have a mean
   of unity. Reject (el,az) cells if the weight is too low. */
                     pw -= nbin_el;
                     if( msum > 2 ) {
                        sum1 /= msum;
                        for( ibin_el = 0; ibin_el < nbin_el; ibin_el++,pw++ ) {
                           if( *pw != VAL__BADD ) {
                              *pw /= sum1;
                              if( *pw < wlim ) *pw = VAL__BADD;
                           }
                        }
                     } else {
                        for( ibin_el = 0; ibin_el < nbin_el; ibin_el++,pw++ ) {
                           *pw = VAL__BADD;
                        }
                     }
                  }

/* Form the azimuth profile using the above weights. The value at a given
   azimuth is the sigma-clipped mean of the values for that azimuth (each
   at a different elevation) in the above sum array. Temporarily re-use
   the "var_az" array to store the azimuth profile. The standard deviation
   of the values contributing to each azimuth profile value is stored in
   array "wgt_az", and used later to weight each value when smoothing the
   profile. */
                  ngood = 0;
                  ps = sum;
                  pw = wsum;
                  sum1 = 0.0;
                  sum2 = 0.0;
                  for( ibin_az = 0; ibin_az < nbin_az; ibin_az++ ) {
                     var_az[ ibin_az ] = smf_sigmaclipD( nbin_el, ps, pw, 2.0,
                                                         4, wgt_az + ibin_az,
                                                         status );
                     if( var_az[ ibin_az ] != VAL__BADD ) {
                        ngood++;
                        sum1 += var_az[ ibin_az ]*var_az[ ibin_az ];
                        sum2 += wgt_az[ ibin_az ];
                     }
                     ps += nbin_el;
                     pw += nbin_el;
                  }

/* If the RMS of the profile values is small compared to the noise, we cannot
   estimate the SSN very well so use an SSN value of zero for this section. */
                  if( ngood > 0 && sqrt( sum1/ngood ) < thresh*sum2/ngood ) {
                     for( ibin_az = 0; ibin_az < nbin_az; ibin_az++ ) {
                        prof_az[ ibin_az ] = 0.0;
                     }
                     pdata->nzero += seclen;

/* If the remaining valid data in the azimuth profile covers less than one
   third of the total azimuth range, re-use the azimuth profile from the
   previous section (if any). */
                  } else if( ngood <  nbin_az/3 ) {
                     if( isection == 0 ) {
                        for( ibin_az = 0; ibin_az < nbin_az; ibin_az++ ) {
                           prof_az[ ibin_az ] = 0.0;
                        }
                        pdata->nzero += seclen;
                     } else {
                        ps = prof_az - nbin_az;
                        for( ibin_az = 0; ibin_az < nbin_az; ibin_az++ ) {
                           prof_az[ ibin_az ] = *(ps++);
                        }
                     }

/* Otherwise smooth the new azimuth profile, putting the results in
   "prof_az". If no smoothing is required, just copy the profile unchanged
   into "prof_az". */
                  } else if( kernel_length <= 0 ) {
                     memcpy( prof_az, var_az, nbin_az*sizeof(*prof_az) );

/* Smooth by applying the supplied kernel centred on each value in the
   azimuth profile. The weights are based on the standard deviation in
   each azimuth row in the (el,az) map created earlier. Note, the
   "kernel" array is normalised to a total data sum of 1.0. */
                  } else {
                     sum1 = 0.0;
                     sum2 = 0.0;
                     ibin_lo = -kernel_length/2;
                     ibin_hi = kernel_length/2;
                     for( ibin = 0; ibin < nbin_az; ibin++ ){
                        jsum = 0.0;
                        kwsum = 0;
                        ksum = 0;
                        for( jbin = ibin_lo; jbin <= ibin_hi; jbin++ ){
                           if( jbin >= 0 && jbin < nbin_az ) {
                              if( wgt_az[ jbin ] > 0.0 ) {
                                 wgt = 1.0/wgt_az[ jbin ];
                                 wgt *= wgt*kernel[ jbin - ibin_lo ];
                                 jsum += wgt*var_az[ jbin ];
                                 kwsum += wgt;
                                 ksum += kernel[ jbin - ibin_lo ];
                              }
                           }
                        }

                        if( ksum > 0.5 ) {
                           prof_az[ ibin ] = jsum/kwsum;
                        } else {
                           prof_az[ ibin ] = VAL__BADD;
                        }

                        ibin_lo++;
                        ibin_hi++;
                     }
                  }
               }
            }

/* We now have the smoothed azimuth profiles for all sections of the time
   stream for the current bolometer. Form the SSN model by expanding these
   azimuth profiles. Use linear interpolation within each smoothed azimuth
   profile to get the SSN value for each azmith offset. Also use linear
   interpolation between adjacent sections to avoid sudden changes. The
   "wgt_az" array holds the increment between SSN values in adjacent time
   slices. The "itime_lo" variable holds the time at which the "prof_azl"
   values should be used. The "itime_hi" variable holds the time at which
   new values for "wgt_az" should be found. */
            itime_lo = seclen/2;
            itime_hi = itime_lo + seclen;

            prof_azlo = prof_azs;
            prof_azhi = prof_azs + nbin_az;

            if( nsection > 1 ) {
               for( ibin_az = 0; ibin_az < nbin_az; ibin_az++ ) {
                  if( prof_azlo[ ibin_az ] != VAL__BADD &&
                      prof_azhi[ ibin_az ] != VAL__BADD ) {
                     wgt_az[ ibin_az ] = ( prof_azhi[ ibin_az ] -
                                           prof_azlo[ ibin_az ] )/seclen;
                  } else {
                     wgt_az[ ibin_az ] = VAL__BADD;
                  }
               }
            } else {
               for( ibin_az = 0; ibin_az < nbin_az; ibin_az++ ) {
                  if( prof_azlo[ ibin_az ] != VAL__BADD ) {
                     wgt_az[ ibin_az ] = 0.0;
                  } else {
                     wgt_az[ ibin_az ] = VAL__BADD;
                  }
               }
            }

            isection = 0;
            pm = pdata->model_data + ibase;
            state = hdr->allState;
            for( itime = 0; itime < ntslice; itime++,state++ ) {

/* If it is time to move on to a new pair of neighbouring sections (for
   the purpose of interpolation between sections), update the increments
   between the new pair of neighbouring sections. */
               if( itime == itime_hi ) {
                  isection++;
                  itime_lo += seclen;
                  if( isection < nsection - 2 ) {
                     itime_hi += seclen;
                  } else {
                     itime_hi = ntslice;
                  }

/* If there are only one or two sections in total, then we already have
   the required increments in "wgt_az" and the required base values in
   "prof_azlo". */
                  if( nsection > 2 ) {
                     prof_azlo += nbin_az;
                     prof_azhi += nbin_az;

                     for( ibin_az = 0; ibin_az < nbin_az; ibin_az++ ) {
                        if( prof_azlo[ ibin_az ] != VAL__BADD &&
                            prof_azhi[ ibin_az ] != VAL__BADD ) {
                           wgt_az[ ibin_az ] = ( prof_azhi[ ibin_az ] -
                                                 prof_azlo[ ibin_az ] )/seclen;
                        } else {
                           wgt_az[ ibin_az ] = VAL__BADD;
                        }
                     }
                  }

               }

/* Check the pointing info is good. */
               if( state->tcs_az_ac1 != VAL__BADD &&
                   state->tcs_az_bc1 != VAL__BADD ) {

/* Get the azimuth offset for this time slice. */
                  daz = state->tcs_az_ac1 - state->tcs_az_bc1;

/* Determine the interpolation factors for the required bins of the
  azimuth profile. */
                  faz = daz*c1 + c2;
                  ibin_az = faz;
                  if( ibin_az >= 0 && ibin_az < nbin_az ) {
                     if( faz - ibin_az > 0.5 ) {
                        ilo = ibin_az;
                        ihi = ilo + 1;
                        if( ihi >= nbin_az ) ihi = nbin_az - 1;
                        flo = 1.5 - faz + ibin_az;
                        fhi = 1.0 - flo;
                     } else {
                        ihi = ibin_az;
                        ilo = ihi - 1;
                        if( ilo < 0 ) ilo = 0;
                        fhi = 0.5 + faz - ibin_az;
                        flo = 1.0 - fhi;
                     }

/* Get the required values form the azimuth profile, interpolating
   between sections. */
                     if( wgt_az[ ilo ] != VAL__BADD ) {
                        vlo = prof_azlo[ ilo ] + wgt_az[ ilo ]*
                                          ( (int) itime - (int) itime_lo );
                     } else {
                        vlo = VAL__BADD;
                     }

                     if( wgt_az[ ihi ] != VAL__BADD ) {
                        vhi = prof_azlo[ ihi ] + wgt_az[ ihi ]*
                                          ( (int) itime - (int) itime_lo );
                     } else {
                        vhi = VAL__BADD;
                     }

/* Get the required SSN value, interpolating between azimuth bins. */
                     if( vlo != VAL__BADD && vhi != VAL__BADD ) {
                        *pm = flo*vlo + fhi*vhi;
                     } else {
                        *pm = VAL__BADD;
                     }

                  } else {
                     *pm = VAL__BADD;
                  }
               }
               pm += tstride;
            }
         }
         ibase += bstride;
         ivbase += vbstride;
      }

      sum = astFree( sum );
      wsum = astFree( wsum );
      var_az = astFree( var_az );
      var_el = astFree( var_el );
      prof_azs = astFree( prof_azs );
      wgt_az = astFree( wgt_az );

/* Report an error if the worker was to do an unknown job.
   ====================================================== */
   } else {
      *status = SAI__ERROR;
      errRepf( "", "smf1_calcmodel_ssn: Invalid operation (%d) supplied.",
               status, pdata->oper );
   }
}



