/*
*+
*  Name:
*     smf_calcmodel_flt

*  Purpose:
*     Calculate the frequency domain FiLTered data model

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_flt( ThrWorkForce *wf, smfDIMMData *dat, int
*			 chunk, AstKeyMap *keymap, smfArray
*			 **allmodel, int flags, int *status)

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
*        Control flags.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Execute a frequency domain filter, but store what was removed with a
*     time-domain representation for easy visualization. The data should
*     already have been padded (but not apodized) in a pre-processing step.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2009-03-10 (EC):
*        Initial Version
*     2009-04-17 (EC)
*        - switch to subkeymap notation in config file
*     2009-04-27 (EC)
*        Enable multiple threads in call to smf_filter_execute
*     2009-09-30 (EC)
*        Measure normalized change in model between iterations (dchisq)
*     2009-12-07 (EC)
*        - optionally delay calculation until after 1st iteration (notfirst)
*     2010-05-04 (TIMJ):
*        Simplify KeyMap access. We now trigger an error if a key is missing
*        and we ensure all keys have corresponding defaults.
*     2010-09-23 (EC):
*        Calculate model as complementary filter applied to residual, rather
*        than as the residual minus the filtered residual.
*     2010-09-28 (DSB):
*        Fill gaps, allowing padded regions to be replaced with artifical
*        data.
*     2010-10-13 (EC):
*        Move calculation of complementary filter into smf_filter_execute
*     2011-04-14 (DSB):
*        Remove gap filling since it is now done in smf_filter_execute.
*     2012-03-16 (DSB):
*        Allow FLT model to be masked.
*     2012-05-22 (DSB):
*        Multi-thread some loops.
*     2012-06-05 (DSB):
*        Allow the old FLT model to be added back into the residuals at the
*        start of the iteration, rather than just before finding the new FLT
*        model. This is controlled by config parameter FLT.UNDOFIRST.
*     2013-02-17 (DSB):
*        Do not modify the flt mask to exclude unused map pixels on the first
*        iteration as the map has not yet been determined.
*     2013-03-18 (DSB):
*        Allow a different filter size to be used on the last iteration
*        (specified by parameters "FLT.xxx_LAST").
*     2013-06-10 (DSB):
*        - The FLT mask was mis-placed by a number of samples equal to
*        twice the padding plus the apodisation.
*        - Reorder the LUT model if any masking is being one.
*     2013-10-21 (DSB):
*        - Provide an option to flag samples that appear to suffer from
*        ringing  after the FLT model has been removed.
*     2013-08-22 (DSB):
*        Include extra checks for bad values stored for BADDA samples.
*     2021-9-9 (DSB):
*        Added time-based masking based on an FFCLEAN-like algorithm
*        prior to applying the filter. See parameter "flt.clip".
*     2022-2-1 (DSB):
*        Modified time-based masking to use two clipping levels.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2009-2010 University of British Columbia.
*     Copyright (C) 2010-2013 Science & Technology Facilities Council.
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
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* Prototypes for local static functions. */
static void smf1_calcmodel_flt( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfCalcModelFltData {
   dim_t b1;
   dim_t b2;
   dim_t nointslice;
   dim_t ntslice;
   double *model_data;
   double *model_data_copy;
   double *noi_data;
   double *res_data;
   double chisquared;
   double dchisq;
   double ring_nsigma;
   int *lut_data;
   int chisq;
   int clear_ring;
   int oper;
   int ring_box1;
   int ring_box2;
   int ring_minsize;
   int ring_wing;
   dim_t bstride;
   dim_t nclose;
   dim_t ndchisq;
   dim_t noibstride;
   dim_t noitstride;
   dim_t tstride;
   smf_qual_t *qua_data;
   unsigned char *mask;
} SmfCalcModelFltData;



#define FUNC_NAME "smf_calcmodel_flt"

void smf_calcmodel_flt( ThrWorkForce *wf, smfDIMMData *dat, int chunk,
                        AstKeyMap *keymap, smfArray **allmodel,
                        int flags, int *status) {

  /* Local Variables */
  dim_t bolostep;               /* Number of bolos per thread */
  int box;                      /* No. of samples across largest feature */
  dim_t bstride;                /* bolo stride */
  double clip[2];               /* No of sigmas at which to clip within smf_ffmask */
  double dchisq=0;              /* this - last model residual chi^2 */
  int dofft;                    /* flag if we will actually do any filtering */
  int domask;                   /* Mask residuals prior to finding FLT? */
  int do_ringing;               /* Apply ringing filter? */
  smfFilter *filt=NULL;         /* Pointer to filter struct */
  dim_t i;                      /* Pixel index */
  dim_t idx=0;                  /* Index within subgroup */
  int iw;                       /* Thread index */
  SmfCalcModelFltData *job_data = NULL; /* Data describing worker jobs */
  AstKeyMap *kmap=NULL;         /* Pointer to FLT-specific keys */
  smfArray *lut=NULL;           /* Pointer to LUT at chunk */
  int *lut_data = NULL;         /* Array holding themap index for each sample */
  unsigned char *ringmask=NULL; /* Pointer to 2D AST mask map */
  unsigned char *mask;          /* Pointer to 2D FLT mask map */
  smfArray *model=NULL;         /* Pointer to model at chunk */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  double *model_data_copy=NULL; /* Copy of model_data for one bolo */
  dim_t nbolo=0;                /* Number of bolometers */
  dim_t ndata=0;                /* Total number of data points */
  dim_t ndchisq=0;             /* number of elements contributing to dchisq */
  smfArray *noi=NULL;           /* Pointer to NOI at chunk */
  double *noi_data=NULL;        /* Pointer to DATA component of model */
  dim_t noibstride;            /* bolo stride for noise */
  dim_t nointslice;             /* number of time slices for noise */
  dim_t noitstride;            /* Time stride for noise */
  int notfirst=0;               /* flag for delaying until after 1st iter */
  size_t nsharp;                /* No. of samples within sharp features */
  dim_t ntslice=0;              /* Number of time slices */
  int nval;                     /* Number of values obtained */
  int nw;                       /* Number of worker threads */
  int order_list;               /* List of models to re-order */
  SmfCalcModelFltData *pdata = NULL; /* Data describing worker jobs */
  smfArray *qua=NULL;           /* Pointer to QUA at chunk */
  smf_qual_t *qua_data=NULL; /* Pointer to quality data */
  smfArray *res=NULL;           /* Pointer to RES at chunk */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  double ring_box1;             /* Small scale box size for ringing filter */
  double ring_box2;             /* Small scale box size for ringing filter */
  int ring_freeze;              /* When to freeze the ringing filter */
  int ring_mask;                /* Do not apply ringing filter to source regions? */
  double ring_minsize;          /* Smallest section of ringing samples to flag */
  double ring_nsigma;           /* Clipping limit for ringing filter */
  double ring_wing;             /* Size of wings for ringing filter */
  int skip;                     /* Number of AST models being skipped */
  dim_t tstride;               /* Time slice stride in data array */
  int undofirst = 1;            /* Undo FLT model at start of iteration? */
  int whiten;                   /* Applying whitening filter? */
  double period;                /* Period of lowest passed frequency */
  int zeropad;                  /* Pad with zeros? */
  size_t t_first;               /* Index of first usable time slice */
  size_t t_last;                /* Index of last usable time slice */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* See if a mask should be used to exclude bright source areas from
     the FLT model. */
  mask = smf_get_mask( wf, SMF__FLT, keymap, dat, flags, &domask, status );

  /* If we have a mask, copy it into the quality array of the map.
     Also set map pixels that are not used (e.g. corner pixels, etc)
     to be background pixels in the mask. Do not do this on the firrst
     iteration as the map has not yet been determined.*/
  if( mask ) {
    double *map = dat->map;
    smf_qual_t *mapqual = dat->mapqual;
    double *mapvar = dat->mapvar;

    for( i=0; i<dat->msize; i++ ) {
      if( mask[i] ) {
         mapqual[i] |= SMF__MAPQ_FLT;

      } else if( dat->iter > 0 && ( map[i] == VAL__BADD || mapvar[i] == VAL__BADD || mapvar[i] <= 0.0 ) ) {
         mask[i] = 1;
         mapqual[i] |= SMF__MAPQ_FLT;

      } else {
         mapqual[i] &= ~SMF__MAPQ_FLT;
      }
    }
  }

  /* Obtain pointer to sub-keymap containing FLT filter
     parameters. Something will always be available.*/
  astMapGet0A( keymap, "FLT", &kmap );

  /* Was the FLT model undone at the start of the iteration. If so, we do
     not need to undo it again here. */
  astMapGet0I( kmap, "UNDOFIRST", &undofirst );

  /* Are we skipping the first iteration? */
  astMapGet0I(kmap, "NOTFIRST", &notfirst);

  if( notfirst && (flags & SMF__DIMM_FIRSTITER) ) {
    msgOutif( MSG__VERB, "", FUNC_NAME
              ": skipping FLT this iteration", status );
    return;
  }

  /* Are we padding with zeros or artifical data? */
  astMapGet0I( kmap, "ZEROPAD", &zeropad );

  /* Get parameters used by the ringing filter. */
  astMapGet0D( kmap, "RING_BOX1", &ring_box1 );
  astMapGet0D( kmap, "RING_BOX2", &ring_box2 );
  astMapGet0D( kmap, "RING_NSIGMA", &ring_nsigma );
  astMapGet0D( kmap, "RING_WING", &ring_wing );
  astMapGet0D( kmap, "RING_MINSIZE", &ring_minsize );
  astMapGet0I( kmap, "RING_FREEZE", &ring_freeze );
  astMapGet0I( kmap, "RING_MASK", &ring_mask );

  /* If the ringing filter flags are to be frozen at any point,
     we need to find out how many initial AST-skipped iterations
     are being used (the FLT.RING_FREEZE value does not include
     any iterations specified by AST.SKIP). */
  if( ring_freeze ) {
    AstKeyMap *kamap;
    astMapGet0A( keymap, "AST", &kamap );
    astMapGet0I( kamap, "SKIP", &skip );
    kamap = astAnnul( kamap );
    ring_freeze += skip;
  }

  /* Set a flag indicating if a ringing filter should be applied */
  do_ringing = ( ring_box1 > 0.0 && !dat->ast_skipped &&
                 ( ring_freeze <= 0 || dat->iter <= ring_freeze ) );

  /* If we are applying a ringing filter, and RING_MASK indicates that
     source regions should not be filtered, then save a pointer to the mask
     to be used. If the FLT mask is not available, use the AST mask. */
  if( do_ringing && ring_mask ) {
     if( mask ) {
        ringmask = mask;
     } else {
        ringmask = smf_get_mask( wf, SMF__AST, keymap, dat, flags, NULL, status );
     }
  }

  /* Assert bolo-ordered data */
  order_list = SMF__RES|SMF__QUA|SMF__NOI;
  if( mask ) order_list |= SMF__LUT;
  smf_model_dataOrder( wf, dat, allmodel, chunk, order_list, 0, status );

  /* Obtain pointers to relevant smfArrays for this chunk */
  res = dat->res[chunk];
  qua = dat->qua[chunk];
  if (dat->lut) lut = dat->lut[chunk];

  smf_get_dims( res->sdata[0],  NULL, NULL, NULL, NULL,
                &ndata, NULL, NULL, status);

  if(dat->noi) {
    noi = dat->noi[chunk];
    model_data_copy = astCalloc( ndata, sizeof(*model_data_copy) );
  }
  model = allmodel[chunk];

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* Allocate job data for threads. */
  job_data = astCalloc( nw, sizeof(*job_data) );

  /* The number of samples set bad because they fall in sharp features
     within the tim streams. */
  nsharp = 0;

  /* Process each sub-array in turn. */
  for( idx=0; (*status==SAI__OK)&&(idx<res->ndat); idx++ ) {

    /* Obtain dimensions of the data */
    smf_get_dims( res->sdata[idx],  NULL, NULL, &nbolo, &ntslice,
                  &ndata, &bstride, &tstride, status);

    /* Get pointers to data/quality/model */
    res_data = (res->sdata[idx]->pntr)[0];
    qua_data = (qua->sdata[idx]->pntr)[0];
    model_data = (model->sdata[idx]->pntr)[0];
    if (lut) lut_data = (lut->sdata[idx]->pntr)[0];

    if( noi ) {
      smf_get_dims( noi->sdata[idx],  NULL, NULL, NULL, &nointslice,
                    NULL, &noibstride, &noitstride, status);
      noi_data = (double *)(noi->sdata[idx]->pntr)[0];
    }

    if( (res_data == NULL) || (model_data == NULL) || (qua_data == NULL) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": Null data in inputs", status);
    } else {

      /* Find how many bolometers to process in each worker thread. */
      bolostep = nbolo/nw;
      if( bolostep == 0 ) bolostep = 1;

      /* Store information for use by the worker threads. */
      for( iw = 0; iw < nw; iw++ ) {
        pdata = job_data + iw;
        pdata->b1 = iw*bolostep;
        if( iw < nw - 1 ) {
           pdata->b2 = pdata->b1 + bolostep - 1;
        } else {
           pdata->b2 = nbolo - 1 ;
        }

        pdata->mask = mask;
        pdata->ntslice = ntslice;
        pdata->nointslice = nointslice;
        pdata->qua_data = qua_data;
        pdata->model_data = model_data;
        pdata->model_data_copy = model_data_copy;
        pdata->res_data = res_data;
        pdata->noi_data = noi_data;
        pdata->lut_data = lut_data;
        pdata->bstride = bstride;
        pdata->tstride = tstride;
        pdata->noibstride = noibstride;
        pdata->noitstride = noitstride;
      }

      /* If we are just inverting the model, place last iteration of filtered
         signal back into residual. This also clears any SMF__Q_RING flags (this
         should be done at the start of each iteration before re-estimating COM
         to avoid problems with convergence). */
      if( flags & SMF__DIMM_INVERT ) {
        for( iw = 0; iw < nw; iw++ ) {
          pdata = job_data + iw;
          pdata->oper = 1;
          pdata->clear_ring = ( ring_box1 > 0.0 && ( ring_freeze <= 0 ||
                                dat->iter <= ring_freeze ) );
          thrAddJob( wf, 0, pdata, smf1_calcmodel_flt, 0, NULL, status );
        }
        thrWait( wf, status );

      /* Otherwise, estimate and remove the FLT model. */
      } else {

        /* If not already done, add the old FLT model back on again, and clear
           SMF__Q_RING flags. */
        if( ! undofirst ) {
          for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->clear_ring = ( ring_box1 > 0.0 && ( ring_freeze <= 0 ||
                                  dat->iter <= ring_freeze ) );
            pdata->oper = 1;
            thrAddJob( wf, 0, pdata, smf1_calcmodel_flt, 0, NULL, status );
          }
          thrWait( wf, status );
        }

        /* Create a filter */
        filt = smf_create_smfFilter( res->sdata[idx], status );


        smf_filter_fromkeymap( filt, kmap,
                               (flags & SMF__DIMM_LASTITER) ? "_LAST" : NULL,
                               res->sdata[idx]->hdr, &dofft, &whiten,
                               &box, status );

        if( *status == SMF__INFREQ ) {
          /* If a bad frequency was specified just annul the error and
             skip the FLT model component */
          dofft = 0;
          errAnnul( status );
          msgOut( "", FUNC_NAME ": invalid frequency for filter specified. "
                  "Skipping FLT model component.", status );
        } else {
          if( !dofft ) {
            msgOutif( MSG__VERB, " ", FUNC_NAME
                      ": No valid filter specifiers for FLT given", status );
          }
        }

        if( *status == SAI__OK ) {

          /* Make a copy of the last model if calculating dchisq */
          if( noi ) {
            memcpy( model_data_copy, model_data,
                    ndata*smf_dtype_size(res->sdata[idx], status ) );
          }

          /* Copy the residual+old model into model_data where it will be
             filtered again in this iteration. */
          memcpy( model_data, res_data,
                  ndata*smf_dtype_size(res->sdata[idx],status) );

          /* Set samples bad that correspond to source pixels in the mask map.
             This seems to improve the speed of convergence and could reduce
             ringing caused by bright sources. */
          if( mask && lut ) {

            /* We do not allow the masking to extend close to the start or
               end of the time stream since this would mean that the FLT
               model is poorly constrained in the masked area. "Close" is
               defined as "napod + 2*npad" samples from either end, where
               "napod" is the number of apodised samples and "npad" is the
               number of padded samples at each end. */
            dim_t npad, npad_or_apod, nclose;
            smf_get_goodrange( qua_data, ntslice, tstride, SMF__Q_PAD |
                               SMF__Q_APOD, &npad_or_apod, NULL, status );
            smf_get_goodrange( qua_data, ntslice, tstride, SMF__Q_PAD,
                               &npad, NULL, status );
            nclose = npad + npad_or_apod;

            /* Do the masking. Bad values are filled by smf_fillgaps, regardless
               of quality, so we just set the model values bad. */
            for( iw = 0; iw < nw; iw++ ) {
              pdata = job_data + iw;
              pdata->nclose = nclose;
              pdata->oper = 2;
              thrAddJob( wf, 0, pdata, smf1_calcmodel_flt, 0, NULL, status );
            }
            thrWait( wf, status );
          }
        }

        /* If required, identifiy sharp features in the time-streams and
           replace them with bad values. */
        if( domask ){
           astMapGet1D( kmap, "CLIP", 2, &nval, clip );
           if( clip[ 0 ] > 0.0 ){
              if( nval == 1 ) clip[ 1 ] = clip[ 0 ];

              smf_get_goodrange( qua_data, ntslice, tstride, SMF__Q_BOUND,
                                 &t_first, &t_last, status );
              if( (int) t_first > box/2 ){
                 t_first -= box/2;
              } else {
                 t_first = 0;
              }
              t_last += box/2;
              if( t_last >= ntslice ) t_last = ntslice - 1;
              nsharp += smf_ffmask( wf, model_data, qua_data, nbolo, ntslice,
                                    tstride, bstride, t_first, t_last, box,
                                    clip[0], clip[1], status );
           }
        }

        /* Apply the complementary filter to the copy of the
           residual+old model stored in the model. So, for example, if
           the goal is to remove low-frequency noise with a high-pass filter,
           this operation will place the low-frequencies into the model (and
           we can then subtract it from the residual).
        */
        if( dofft ) {
          smf_filter_execute( wf, model->sdata[idx], filt, -1, whiten, status );
        }

        /* Now remove the filtered signals from the residual by subtracting
           off the new iteration of the model. */
        if( *status == SAI__OK ) {

          /* Submit the jobs */
          for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->oper = 3;
            thrAddJob( wf, 0, pdata, smf1_calcmodel_flt, 0, NULL, status );
          }

          /* Wait for the jobs to finish. */
          thrWait( wf, status );

          /* Accumlate the values returned by the jobs. */
          for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            ndchisq += pdata->ndchisq;
            dchisq += pdata->dchisq;
          }

          /* If required, locate and flag any residuals that seem to suffer
             from ringing now that the low frequency FLT model has been
             removed. DO not apply a ringing filter unless an AST model
             was used on the previous iteration. */
          if( do_ringing ){
             msgOutif( MSG__DEBUG, "", "Flagging residuals that appear "
                       "to suffer from ringing.", status );

             smf_filter_getlowf( filt, res->sdata[idx]->hdr, &period, status );
             for( iw = 0; iw < nw; iw++ ) {
               pdata = job_data + iw;
               pdata->ring_box1 = (int)( ring_box1*period + 0.5 );
               pdata->ring_box2 = (int)( ring_box2*period + 0.5 );
               pdata->ring_nsigma = ring_nsigma;
               pdata->ring_wing = (int)( ring_wing*period + 0.5 );
               pdata->ring_minsize = (int)( ring_minsize*period + 0.5 );
               pdata->mask = ringmask;
               pdata->oper = 4;
               thrAddJob( wf, 0, pdata, smf1_calcmodel_flt, 0, NULL, status );
             }
             thrWait( wf, status );
          }
        }

        /* Free the filter */
        filt = smf_free_smfFilter( filt, status );
      }
    }
  }

  /* Print normalized residual chisq for this model */
  if( (*status==SAI__OK) && noi && (ndchisq>0) ) {
    dchisq /= (double) ndchisq;
    msgOutiff( MSG__VERB, "", "    normalized change in FLT model: %lg", status,
               dchisq );
  }

  /* Print the number of samples masked due to being part of a sharp
     feature in a time stream. */
  if( nsharp ) msgOutiff( MSG__VERB, "", "    %zu samples masked as "
                          "sharp features within the FLT model", status,
                          nsharp );

  job_data = astFree( job_data );
  if( kmap ) kmap = astAnnul( kmap );
  model_data_copy = astFree( model_data_copy );
}




static void smf1_calcmodel_flt( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_calcmodel_flt

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_calmodel_flt.

*  Invocation:
*     smf1_calcmodel_flt( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfCalcModelFltData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfCalcModelFltData *pdata;
   dim_t ibolo;
   dim_t itime;
   double *pmc;
   double *pm;
   double *pn;
   double *pr;
   int *pl;
   dim_t ibase;
   smf_qual_t *pq;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfCalcModelFltData *) job_data_ptr;

/* Place last iteration of filtered signal back into residual */
   if( pdata->oper == 1 ) {

/* Loop round all bolos to be processed by this thread, maintaining the
   index of the first time slice for the current bolo. */
      ibase = pdata->b1*pdata->bstride;
      for( ibolo = pdata->b1; ibolo <= pdata->b2; ibolo++ ) {

/* Check that the whole bolometer has not been flagged as bad. */
         pq = pdata->qua_data + ibase;
         if( !( *pq & SMF__Q_BADB ) ) {

/* Get a pointer to the first residual and model value for the current bolo,
   and then loop round all time slices. */
            pr = pdata->res_data + ibase;
            pm = pdata->model_data + ibase;
            for( itime = 0; itime < pdata->ntslice; itime++ ) {

/*  Add the model value on to the residual. BADDA samples will have bad
    values so check for them. */
               if( *pr != VAL__BADD ) *pr += *pm;

/* Clear any SMF__Q_RING flags. */
               if( pdata->clear_ring) *pq &= ~SMF__Q_RING;

/* Move residual and model pointers on to the next time slice. */
               pr += pdata->tstride;
               pm += pdata->tstride;
               pq += pdata->tstride;
            }
         }

/* Increment the index of the first value associated with the next
   bolometer. */
         ibase += pdata->bstride;
      }

/* Do the masking. Bad values are filled by smf_fillgaps, regardless
   of quality, so we just set the model values bad. */
   } else if( pdata->oper == 2 ) {
      ibase = pdata->b1*pdata->bstride;
      for( ibolo = pdata->b1; ibolo <= pdata->b2; ibolo++ ) {
         if( !( (pdata->qua_data)[ ibase ] & SMF__Q_BADB ) ) {
            pl = pdata->lut_data + ibase + pdata->nclose*pdata->tstride;
            pm = pdata->model_data + ibase + pdata->nclose*pdata->tstride;
            for( itime = pdata->nclose; itime < pdata->ntslice - pdata->nclose;
                                                                    itime++ ) {
               if( *pl != VAL__BADI && !pdata->mask[ *pl ] ) *pm = VAL__BADD;
               pl += pdata->tstride;
               pm += pdata->tstride;
            }
         }
         ibase += pdata->bstride;
      }

/* Now remove the filtered signals from the residual by subtracting
   off the new iteration of the model. */
   } else if( pdata->oper == 3 ) {

      pdata->dchisq = 0.0;
      pdata->ndchisq = 0;

      ibase = pdata->b1*pdata->bstride;
      for( ibolo = pdata->b1; ibolo <= pdata->b2; ibolo++ ) {
         pq = pdata->qua_data + ibase;
         if( !( *pq & SMF__Q_BADB ) ) {
            pr = pdata->res_data + ibase;
            pm = pdata->model_data + ibase;
            pmc = pdata->model_data_copy + ibase;
            pn = pdata->noi_data + ibolo*pdata->noibstride;

            for( itime = 0; itime < pdata->ntslice; itime++ ) {
               if( *pr != VAL__BADD ) {
                  *pr -= *pm;
                  if( pdata->noi_data && !( *pq & SMF__Q_GOOD ) ) {
                     double change = *pm - *pmc;
                     pdata->dchisq += change*change /
                                      pn[ (itime % pdata->nointslice)*pdata->noitstride ];
                     pdata->ndchisq++;
                  }
               }

               pr += pdata->tstride;
               pq += pdata->tstride;
               pm += pdata->tstride;
               pmc += pdata->tstride;
            }
         }
         ibase += pdata->bstride;
      }

/* Locate and flag samples that suffer from ringing. */
   } else if( pdata->oper == 4 ) {
      for( ibolo = pdata->b1; ibolo <= pdata->b2; ibolo++ ) {
         pq = pdata->qua_data + ibolo*pdata->bstride;
         if( !( *pq & SMF__Q_BADB ) ) {
            pr = pdata->res_data + ibolo*pdata->bstride;
            pl = pdata->lut_data + ibolo*pdata->bstride;
            smf_flag_rings( pr, pdata->tstride, pdata->ntslice,
                            pdata->ring_box1, pdata->ring_box2,
                            pdata->ring_nsigma, pdata->ring_wing,
                            pdata->ring_minsize, pq, SMF__Q_FIT,
                            pdata->mask, pl, status );
         }
      }

   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( "", "smf1_calcmodel_flt: Illegal operation %d requested.",
               status, pdata->oper );
   }
}

