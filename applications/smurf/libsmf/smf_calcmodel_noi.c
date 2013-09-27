/*
*+
*  Name:
*     smf_calcmodel_noi

*  Purpose:
*     Calculate the NOIse model for the bolometers

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_noi( ThrWorkForce *wf, smfDIMMData *dat, int
*                        chunk, AstKeyMap *keymap, smfArray
*                        **allmodel, int flags, int *status)

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
*        Control flags: estimate VARIANCE only if SMF__DIMM_FIRSTITER set
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Perform noise measurements on the detectors. The basic idea is to
*     measure the white-noise level in each detector for the first iteration
*     from the individual power spectra. For subsequent iterations, if
*     called after all other model components have been fit, it will also
*     estimate chi^2 by comparison the scatter in the final residual to the
*     white noise level. The (reduced) chi^2 calculated should ideally
*     converge to 1 provided that the models are correct, and the white noise
*     has been measured correctly.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     David Berry (JAC, Hawaii)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2007-03-02 (EC):
*        Initial Version
*     2007-03-05 (EC)
*        Modified bit flags
*     2007-06-13 (EC)
*        Fixed res_data pointer assignment bug
*     2007-07-10 (EC)
*        Use smfArray instead of smfData
*     2008-03-04 (EC)
*        Modified interface to use smfDIMMData
*     2008-04-03 (EC)
*        Use QUALITY
*     2008-04-14 (EC)
*        Added optional despiking config (NOISPIKETHRESH/NOISPIKEITER)
*     2008-04-17 (EC)
*        -fixed nbolo/ntslice calculation
*        -store variance instead of standard deviation
*        -use smf_quick_noise instead of smf_simple_stats on whole array;
*         added config parameters (NOISAMP/NOICHUNK)
*     2008-04-18 (EC)
*        -Only calculate the white noise level once (first iteration)
*        -Add chisquared calculation
*     2008-05-02 (EC)
*        - Use different levels of verbosity in messages
*     2009-04-17 (EC)
*        - switch to subkeymap notation in config file
*     2009-07-21 (EC)
*        - Remove NOISAMP/NOICHUNK related parameters since they aren't used
*     2009-07-31 (EC)
*        - handle 2d variance arrays
*     2010-01-12 (EC)
*        - Handle gap filling
*     2010-01-19 (DSB)
*        - Add dcthresh2 config parameter.
*     2010-02-23 (DSB)
*        - Replace dcthresh2 with dcmedianwidth config parameter.
*     2010-05-04 (TIMJ):
*        Simplify KeyMap access. We now trigger an error if a key is missing
*        and we ensure all keys have corresponding defaults.
*     2010-05-21 (DSB):
*        Add dclimcorr argument to smf_fix_steps.
*     2010-06-28 (DSB):
*        Do apodization in smf_bolonoise
*     2010-07-02 (TIMJ):
*        Use same apodization as used by CALCNOISE.
*     2010-09-10 (DSB):
*        Change smf_fix_steps argument list.
*     2010-09-15 (DSB):
*        Call smf_flag_spikes2 instead of smf_flag_spikes.
*     2010-09-20 (EC):
*        Optionally calculate noise weights in advance with NOI.CALCFIRST
*     2010-09-23 (DSB):
*        Allow data to be padded with artificial data rather than zeros.
*     2010-10-08 (DSB):
*        Move gap filling to before smf_bolonoise in preparation for the
*        FFT performed within smf_bolonoise.
*     2011-04-14 (DSB):
*        Remove gap filling since it is done in smf_fft_data (called by
*        bolonoise).
*     2011-05-16 (TIMJ):
*        Fix memory leak. We were allocating memory inside a loop but
*        freeing it outside the loop.
*     2011-12-9 (DSB):
*        Ensure a mean-shift filter is used when fixing DC steps. A
*        mean-shift filter should work OK here since the COM signal will
*        already have been removed and so the residuals should be
*        basically flat.
*     2012-03-27 (DSB):
*        Modified to allow separate noise estimates for blocks of time slices.
*     2012-05-22 (DSB):
*        Multi-thread the chi-squared calculation.

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2005-2010 University of British Columbia.
*     Copyright (C) 2010-2012 Science & Technology Facilities Council.
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
static void smf1_calcmodel_noi( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfCalcModelNoiData {
   dim_t b1;
   dim_t b2;
   dim_t mntslice;
   dim_t ntslice;
   double *model_data;
   double *res_data;
   double chisquared;
   int chisq;
   size_t bstride;
   size_t mbstride;
   size_t mtstride;
   size_t nchisq;
   size_t tstride;
   smf_qual_t *qua_data;
} SmfCalcModelNoiData;




#define FUNC_NAME "smf_calcmodel_noi"


void smf_calcmodel_noi( ThrWorkForce *wf, smfDIMMData *dat, int chunk,
                        AstKeyMap *keymap, smfArray **allmodel, int flags,
                        int *status) {

  /* Local Variables */
  dim_t bolostep;               /* Number of bolos per thread */
  dim_t boxsize;                /* No. of time slices in each noise box */
  smfData *box = NULL;          /* SmfData holding one box of input data */
  size_t bstride;               /* bolometer stride */
  int calcfirst=0;              /* Were bolo noises already measured? */
  int dclimcorr;                /* Min number of correlated steps */
  int dcmaxsteps;               /* Maximum allowed number of dc jumps */
  dim_t dcfitbox;               /* Width of box for DC step detection */
  double dcthresh;              /* Threshold for DC step detection */
  dim_t dcsmooth;               /* Width of median filter in DC step detection*/
  double *din;                  /* Pointer to next input value */
  double *dout;                 /* Pointer to next output value */
  int fillgaps;                 /* If set perform gap filling */
  dim_t i;                      /* Loop counter */
  dim_t ibolo;                  /* Bolometer index */
  int ibox;                     /* Index of current noise box */
  int import;                   /* Has an external NOI model been imported? */
  dim_t itime;                  /* Time slice index */
  dim_t idx=0;                  /* Index within subgroup */
  JCMTState *instate=NULL;      /* Pointer to input JCMTState */
  int iw;                       /* Thread index */
  dim_t j;                      /* Loop counter */
  AstKeyMap *kmap=NULL;         /* Local keymap */
  size_t mbstride;              /* model bolometer stride */
  dim_t mntslice;               /* Number of model time slices */
  size_t mtstride;              /* model time slice stride */
  smfArray *model=NULL;         /* Pointer to model at chunk */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  dim_t nbolo;                  /* Number of bolometers */
  int nbox = 0;                 /* Number of noise boxes */
  size_t nchisq;                /* Number of data points in chisq calc */
  dim_t nelbox;                 /* Number of data points in a noise box */
  dim_t ndata;                  /* Total number of data points */
  size_t nflag;                 /* Number of new flags */
  int nleft;                    /* Number of samples not in a noise box */
  dim_t ntslice;                /* Number of time slices */
  int nw;                       /* Number of worker threads */
  size_t pend;                  /* Last non-PAD sample */
  size_t pstart;                /* First non-PAD sample */
  smf_qual_t *qin;              /* Pointer to next input quality value */
  smf_qual_t *qout;             /* Pointer to next output quality value */
  smfArray *qua=NULL;           /* Pointer to RES at chunk */
  smf_qual_t *qua_data=NULL; /* Pointer to RES at chunk */
  smfArray *res=NULL;           /* Pointer to RES at chunk */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  dim_t spikebox=0;             /* Box size for spike detection */
  double spikethresh=0;         /* Threshold for spike detection */
  size_t tend;                  /* Last input sample to copy */
  size_t tstart;                /* First input sample to copy */
  size_t tstride;               /* time slice stride */
  double *var=NULL;             /* Sample variance */
  size_t xbstride;              /* Box bolometer stride */
  int zeropad;                  /* Pad with zeros? */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Obtain pointer to sub-keymap containing NOI parameters */
  astMapGet0A( keymap, "NOI", &kmap );

  /* Assert bolo-ordered data */
  smf_model_dataOrder( dat, allmodel, chunk, SMF__RES|SMF__QUA, 0, status );

  /* Obtain pointers to relevant smfArrays for this chunk */
  res = dat->res[chunk];
  qua = dat->qua[chunk];
  model = allmodel[chunk];

  /* Obtain parameters for NOI */

  /* Data-cleaning parameters  */
  smf_get_cleanpar( kmap, NULL, res->sdata[0], NULL, &dcfitbox, &dcmaxsteps, &dcthresh,
                    &dcsmooth, &dclimcorr, NULL, &fillgaps, &zeropad, NULL,
                    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                    &spikethresh, &spikebox, NULL, NULL, NULL, NULL, NULL, NULL,
                    NULL, NULL, NULL, NULL, NULL, NULL, NULL, status );

  /* Did we already calculate the noise on each detector? */
  astMapGet0I( kmap, "CALCFIRST", &calcfirst );

  /* Did we import an external BOI model? */
  astMapGet0I( kmap, "IMPORT", &import );

  /* Initialize chisquared */
  dat->chisquared[chunk] = 0;
  nchisq = 0;

  /* Loop over index in subgrp (subarray) */
  for( idx=0; idx<res->ndat; idx++ ) {

    /* Get pointers to DATA components */
    res_data = (res->sdata[idx]->pntr)[0];
    model_data = (model->sdata[idx]->pntr)[0];
    qua_data = (qua->sdata[idx]->pntr)[0];

    if( (res_data == NULL) || (model_data == NULL) || (qua_data == NULL) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": Null data in inputs", status);
    } else {

      /* Get the raw data dimensions */
      smf_get_dims( res->sdata[idx], NULL, NULL, &nbolo, &ntslice, &ndata,
                    &bstride, &tstride, status );

      /* NOI model dimensions */
      smf_get_dims( model->sdata[idx], NULL, NULL, NULL, &mntslice, NULL,
                    &mbstride, &mtstride, status );

      /* Only estimate the white noise level once at the beginning - the
         reason for this is to make measurements of the convergence
         easier. We either do it prior to the start of iterations (in which
         case the relative weights will be influeced by low-frequency noise,
         this is initialized in smf_model_create), or else we calculate
         the noise after the first iteration. We also skip this
         calculation if we have already imported external noise values into
         the NOI model. */
      if( (flags & SMF__DIMM_FIRSTITER) && (!calcfirst) && (!import) ) {

        /* There are two forms for the NOI model: one constant noise value
           for each bolometer, or "ntslice" noise values for each bolometer.
           Handle the first case now. */
        if( mntslice == 1 ) {

          var = astMalloc( nbolo*sizeof(*var) );

          if (var) {

            /* Measure the noise from power spectra */
            smf_bolonoise( wf, res->sdata[idx], -1.0, 0, 0.5, SMF__F_WHITELO,
                           SMF__F_WHITEHI, 0, zeropad ? SMF__MAXAPLEN : SMF__BADSZT,
                           var, NULL, NULL, status );

            for( i=0; i<nbolo; i++ ) if( !(qua_data[i*bstride]&SMF__Q_BADB) ) {
                /* Loop over time and store the variance for each sample */
                for( j=0; j<mntslice; j++ ) {
                  model_data[i*mbstride+(j%mntslice)*mtstride] = var[i];
                }
              }

            var = astFree( var );
            dat->noi_boxsize = ntslice;
          }


        /* If the NOI model is of the second form, the noise is estimated
           in boxes of samples lasting "NOI.BOX_SIZE" seconds, and then the
           noise level in the box is assigned to all samples in the box. */
        } else if( mntslice == ntslice ) {

          /* If not already done, get NOI.BOX_SIZE and convert from seconds to
             samples. */
          if( idx == 0 ) {
            boxsize = 0;
            smf_get_nsamp( kmap, "BOX_SIZE", res->sdata[0], &boxsize, status );

            msgOutf( "", FUNC_NAME ": Calculating a NOI variance for each "
                     "box of %d samples.", status, (int) boxsize );

            /* Find the indices of the first and last non-PAD sample. */
            smf_get_goodrange( qua_data, ntslice, tstride, SMF__Q_PAD,
                               &pstart, &pend, status );

            /* How many whole boxes fit into this range? */
            nbox = ( pend - pstart + 1 ) / boxsize;
            if( nbox == 0 ) nbox = 1;

            /* How many samples would be left over at the end if we used this
               many boxes? */
            nleft = ( pend - pstart + 1 ) - nbox*boxsize;

            /* Increase "boxsize" to reduce this number as far as possible.
               Any samples that are left over after this increase of boxsize
               will not be used when calculating the noise levels in each
               bolometer. */
            boxsize += nleft/nbox;

            /* Create a smfData to hold one box-worth of input data. We
               do not need to copy jcmtstate information. */
            if( res->sdata[idx]->hdr ) {
               instate = res->sdata[idx]->hdr->allState;
               res->sdata[idx]->hdr->allState = NULL;
            }
            box = smf_deepcopy_smfData( res->sdata[idx], 0,
                                        SMF__NOCREATE_DATA |
                                        SMF__NOCREATE_VARIANCE |
                                        SMF__NOCREATE_QUALITY,
                                        0, 0, status );
            if( instate ) res->sdata[idx]->hdr->allState = instate;

            /* Set the length of the time axis to the box size plus padding,
               and create empty data and quality arrays for it. */
            if( *status == SAI__OK ) {
               box->dims[  box->isTordered?2:0 ] = boxsize + pstart + (ntslice - pend - 1);
               smf_get_dims( box, NULL, NULL, NULL, NULL, &nelbox,
                             &xbstride, NULL, status );
               box->pntr[0] = astMalloc( sizeof( double )*nelbox );
               box->qual = astMalloc( sizeof( smf_qual_t )*nelbox );

               /* For every bolometer, flag the start and end of the quality
                  array as padding, and store zeros in the data array. */
               for( ibolo = 0; ibolo < nbolo; ibolo++ ) {
                  dout = ((double *) box->pntr[0]) + xbstride*ibolo;
                  qout = box->qual + xbstride*ibolo;
                  for( itime = 0; itime < pstart; itime++ ) {
                     *(qout++) = SMF__Q_PAD;
                     *(dout++) = 0.0;
                  }

                  dout = ((double *) box->pntr[0]) + xbstride*ibolo + pstart + boxsize;;
                  qout = box->qual + xbstride*ibolo + pstart + boxsize;
                  for( itime = pend + 1; itime < ntslice; itime++ ) {
                     *(qout++) = SMF__Q_PAD;
                     *(dout++) = 0.0;
                  }
               }
            }
          }

          /* Work space to hold the variance for each bolometer in a box */
          var = astMalloc( nbolo*sizeof(*var) );
          if( *status == SAI__OK ) {

            /* Index of the first time slice within the input smfData
               that is included in the first box. */
            tstart = pstart;

            /* Loop round each noise box */
            for( ibox = 0; ibox < nbox; ibox++ ) {

               /* Copy the data and quality values for this box from the
                 input smfData into "box", leaving room for padding at
                 both ends of box. Note, data is bolo-ordered so we
                 can assume that "tstride" is 1. */
               din = ((double *)(res->sdata[idx]->pntr[0])) + tstart;
               dout = ((double *)(box->pntr[0])) + pstart;
               qin = qua_data + tstart;
               qout = box->qual + pstart;

               for( ibolo = 0; ibolo < nbolo; ibolo++ ) {
                  memcpy( dout, din, boxsize*sizeof( *din ) );
                  memcpy( qout, qin, boxsize*sizeof( *qin ) );
                  din += bstride;
                  dout += xbstride;
                  qin += bstride;
                  qout += xbstride;
               }

               /* Measure the noise from power spectra in the box. */
               smf_bolonoise( wf, box, 0.1, 0, 0.5, SMF__F_WHITELO, SMF__F_WHITEHI,
                              0, zeropad ? SMF__MAXAPLEN : SMF__BADSZT, var,
                              NULL, NULL, status );

               /* Loop over time and store the variance for each sample in
                  the NOI model. On the last box, pick up any left over time
                  slices. */
               if( ibox < nbox - 1 ) {
                  tend = tstart + boxsize - 1;
               } else {
                  tend = pend;
               }

               for( ibolo = 0; ibolo < nbolo; ibolo++ ) {
                  if( !( qua_data[ ibolo*bstride ] & SMF__Q_BADB ) ) {
                     dout =  model_data + ibolo*bstride + tstart;
                     qout =  qua_data + ibolo*bstride + tstart;
                     for( itime = tstart; itime <= tend; itime++,dout++,qout++ ) {
                        if( var[ ibolo ] != VAL__BADD ) {
                           *dout = var[ ibolo ];
                        } else {
                           *qout |= SMF__Q_NOISE;
                        }
                     }
                  }
               }

               /* Update the index of the first time slice within the input
                  smfData that is included in the next box. */
               tstart += boxsize;
            }

            var = astFree( var );
            dat->noi_boxsize = boxsize;
          }

        /* Report an error if the number of samples for each bolometer in
           the NOI model is not 1 or "ntslice". */
        } else if( *status == SAI__OK ) {
           *status = SAI__ERROR;
           errRepf( "", FUNC_NAME ": NOI model has %d samples - should be "
                    "%d or 1.", status, (int) mntslice, (int) ntslice);
        }
      }

      if( kmap ) {
        /* Flag spikes in the residual after first iteration */
        if( spikethresh && !(flags&SMF__DIMM_FIRSTITER) ) {
          /* Now re-flag */
          smf_flag_spikes( wf, res->sdata[idx], SMF__Q_MOD,
                           spikethresh, spikebox, &nflag, status );
          msgOutiff(MSG__VERB," ", "   flagged %zu new %lf-sig spikes",
                    status, nflag, spikethresh );
        }

        if( dcthresh && dcfitbox ) {
          smf_fix_steps( wf, res->sdata[idx], dcthresh, dcsmooth,
                         dcfitbox, dcmaxsteps, dclimcorr, 1, &nflag, NULL,
                         NULL, status );
          msgOutiff(MSG__VERB, "","   detected %zu bolos with DC steps\n",
                    status, nflag);
        }

      }

      /* Now calculate contribution to chi^2. This bit takes along time
         if there is a lot of data so share the work out amongst the available
         worker threads. How many threads do we get to play with */
      nw = wf ? wf->nworker : 1;

      /* Find how many bolometers to process in each worker thread. */
      bolostep = nbolo/nw;
      if( bolostep == 0 ) bolostep = 1;

      /* Allocate job data for threads, and store the range of bolos to be
         processed by each one. Ensure that the last thread picks up any
         left-over bolos. */
      SmfCalcModelNoiData *job_data = astCalloc( nw, sizeof(*job_data) );
      if( *status == SAI__OK ) {
        SmfCalcModelNoiData *pdata;

        for( iw = 0; iw < nw; iw++ ) {
           pdata = job_data + iw;
           pdata->b1 = iw*bolostep;
           if( iw < nw - 1 ) {
              pdata->b2 = pdata->b1 + bolostep - 1;
           } else {
              pdata->b2 = nbolo - 1 ;
           }

           /* Store other values common to all jobs. */
           pdata->ntslice = ntslice;
           pdata->mntslice = mntslice;
           pdata->qua_data = qua_data;
           pdata->model_data = model_data;
           pdata->res_data = res_data;
           pdata->bstride = bstride;
           pdata->tstride = tstride;
           pdata->mbstride = mbstride;
           pdata->mtstride = mtstride;

           /* Submit the job to the workforce. */
           thrAddJob( wf, 0, pdata, smf1_calcmodel_noi, 0, NULL, status );
        }

        /* Wait for all jobs to complete. */
        thrWait( wf, status );

        /* Accumulate the results from all the worker threads. */
        for( iw = 0; iw < nw; iw++ ) {
           pdata = job_data + iw;
           dat->chisquared[chunk] += pdata->chisquared;
           nchisq += pdata->nchisq;
        }

/* Free the job data. */
        job_data = astFree( job_data );
      }
    }
  }

  /* Free resources */
  if( box ) {
     box->pntr[0] = astFree( box->pntr[0] );
     box->qual = astFree( box->qual );
     smf_close_file( &box, status );
  }

  /* Normalize chisquared for this chunk */
  if( (*status == SAI__OK) && (nchisq >0) ) {
    dat->chisquared[chunk] /= (double) nchisq;
  }

  /* Clean Up */
  if( kmap ) kmap = astAnnul( kmap );
}





static void smf1_calcmodel_noi( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_calcmodel_noi

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_calmodel_noi.

*  Invocation:
*     smf1_calcmodel_noi( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfCalcModelNoiData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfCalcModelNoiData *pdata;
   dim_t ibolo;
   dim_t itime;
   double *pm;
   double *pr;
   size_t ibase;
   smf_qual_t *pq;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfCalcModelNoiData *) job_data_ptr;

/* Initialise returned chisquared increment and count of values. */
   pdata->chisquared = 0.0;
   pdata->nchisq = 0;

/* Loop round all bolos to be processed by this thread, maintaining the
   index of the first time slice for the current bolo. */
   ibase = pdata->b1*pdata->bstride;
   for( ibolo = pdata->b1; ibolo <= pdata->b2; ibolo++ ) {

/* Get a pointer ot the first quality value for the current bolo, and
   check that the whole bolometer has not been flagged as bad. */
      pq = pdata->qua_data + ibase;
      if( !( *pq & SMF__Q_BADB ) ) {

/* Get a pointer to the first residual for the current bolo, and then
   loop round all time slices. */
         pr = pdata->res_data + ibase;
         for( itime = 0; itime < pdata->ntslice; itime++ ) {

/* Get a pointer to the model value (noise) to be used with the current
   bolometer sample. Multiple bolometer samples may share the same noise
   value, so we caclulate the index into the model_data array separately,
   rather than just using "ibase" as for the residual and quality
   pointers. */
            pm = pdata->model_data + ibolo*pdata->mbstride +
                                  ( itime % pdata->mntslice )*pdata->mtstride;

/* If the noise is positive and the bolometer sample is good, increment
   the chi-squared value and the number of samples included in the sum. */
            if( *pm > 0 && !(*pq & SMF__Q_GOOD) ) {
              pdata->chisquared += (*pr)*(*pr) / (*pm);
              pdata->nchisq++;
            }

/* Move residual and quality pointers on to the next time slice. */
            pq += pdata->tstride;
            pr += pdata->tstride;

         }
      }

/* Increment the index of the first value associated with the next
   bolometer. */
      ibase += pdata->bstride;
   }
}
