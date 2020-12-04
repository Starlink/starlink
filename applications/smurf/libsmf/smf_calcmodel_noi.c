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
*     Perform noise measurements on the detectors. The default method is to
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
*     2013-10-21 (DSB):
*        Provide option for small boxes to have noise estimates formed by
*        simply looking at the variance of the residuals in each group of
*        noi.box_size samples. This can result in far fewer samples being
*        flagged as unusable.
*     2014-01-23 (DSB):
*        Use the value of the first NOI data value as an indicator of whether
*        the noise values have already been calculated, rather than relying on
*        knowledge of when this will be the case.
*     2014-01-24 (DSB):
*        dat->noi_boxsize should be set to the number of times each NOI value is
*        duplicated. For box type 2 each time slice has a different NOI value and
*        so dat.noi_boxsize should be 1 in this case.
*     2014-12-11 (DSB):
*        Ensure that the NOI model has no bad values. Also ensure
*        that there are none of the initial unity values (stored by
*        smf_model_create) left. This can happen, for instance, if
*        boxes of samples are flagged (e.g. by COM) on the ifrst
*        iteration. These unity values can then be treated literally
*        if the flags are later cleared.
*     2016-5-19 (DSB):
*        Ensure dat->noi_boxsize is assigned a value even if the NOI model
*        has been pre-calculated as part of the cleaning phase (i.e. if
*        model_data[ 0 ] is not 1.0 on entry on the first iteration).
*     2020-12-4 (DSB):
*        Add a facility to return the weighted chisquared, where each sample is
*        weighted by the squared SNR in the corresponding map pixel. This should
*        give a general measure of the goodness of the fit in the source regions.
*        The established "chisquared" value includes background regions too, which
*        reduces its usefulness as a measure of the goodness of fit in the region
*        of interest.

*  Copyright:
*     Copyright (C) 2016-2020 East Asian Observatory.
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
   dim_t box;
   dim_t mntslice;
   dim_t ntslice;
   double *map;
   double *mapvar;
   double *model_data;
   double *res_data;
   double chisquared;
   double wchisquared;
   double wchisq;
   int chisq;
   int *lut_data;
   int operation;
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
  int box_type;                 /* How to calculate the noise */
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
  dim_t itime;                  /* Time slice index */
  dim_t idx=0;                  /* Index within subgroup */
  JCMTState *instate=NULL;      /* Pointer to input JCMTState */
  int iw;                       /* Thread index */
  dim_t j;                      /* Loop counter */
  AstKeyMap *kmap=NULL;         /* Local keymap */
  smfArray *lut=NULL;           /* Pointer to LUT at chunk */
  int *lut_data=NULL;           /* Pointer to DATA component of lut */
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
  SmfCalcModelNoiData *pdata;   /* Pointer to data for a single job */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Obtain pointer to sub-keymap containing NOI parameters */
  astMapGet0A( keymap, "NOI", &kmap );

  /* Assert bolo-ordered data */
  smf_model_dataOrder( wf, dat, allmodel, chunk, SMF__RES|SMF__QUA, 0, status );

  /* Obtain pointers to relevant smfArrays for this chunk */
  res = dat->res[chunk];
  qua = dat->qua[chunk];
  model = allmodel[chunk];
  lut = dat->lut[chunk];

  /* Get the raw data dimensions */
  smf_get_dims( res->sdata[idx], NULL, NULL, &nbolo, &ntslice, &ndata,
                &bstride, &tstride, status );

  /* How many threads do we get to play with */
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
       pdata->bstride = bstride;
       pdata->tstride = tstride;
    }
  }

  /* Obtain parameters for NOI */

  /* Data-cleaning parameters  */
  smf_get_cleanpar( kmap, NULL, res->sdata[0], NULL, &dcfitbox, &dcmaxsteps, &dcthresh,
                    &dcsmooth, &dclimcorr, NULL, &fillgaps, &zeropad, NULL,
                    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                    &spikethresh, &spikebox, NULL, NULL, NULL, NULL, NULL, NULL,
                    NULL, NULL, NULL, NULL, NULL, NULL, NULL, status );

  /* Did we already calculate the noise on each detector? */
  astMapGet0I( kmap, "CALCFIRST", &calcfirst );

  /* Use FFT method or simple variance? */
  astMapGet0I( kmap, "BOX_TYPE", &box_type );

  /* Initialize chisquared */
  dat->chisquared[chunk] = 0;
  nchisq = 0;

  /* Loop over index in subgrp (subarray) */
  for( idx=0; idx<res->ndat; idx++ ) {

    /* Get pointers to DATA components */
    res_data = (res->sdata[idx]->pntr)[0];
    model_data = (model->sdata[idx]->pntr)[0];
    qua_data = (qua->sdata[idx]->pntr)[0];
    lut_data = (lut->sdata[idx]->pntr)[0];

    if( (res_data == NULL) || (model_data == NULL) || (qua_data == NULL) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": Null data in inputs", status);
    } else {

      /* NOI model dimensions */
      smf_get_dims( model->sdata[idx], NULL, NULL, NULL, &mntslice, NULL,
                    &mbstride, &mtstride, status );

      /* If not already done, set dat->noi_boxsize to the number of times each
         NOI value is duplicated. This is used to determine the degree of
         compression that can be used when exporting the NOI model. */
      if( dat->noi_boxsize == 0 ) {

         /* Handle cases where there is one constant noise value for
            each bolometer (i.e. "ntslice" slices share the same noise
            value). */
         if( mntslice == 1 ) {
            dat->noi_boxsize = ntslice;

         /* Handle cases where the noise is estimated in boxes of samples
            lasting "NOI.BOX_SIZE" seconds (the noise level in the box is
            then assigned to all samples in the box). Get NOI.BOX_SIZE and
            convert from seconds to samples. */
         } else {
            boxsize = 0;
            smf_get_nsamp( kmap, "BOX_SIZE", res->sdata[0], &boxsize, status );

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

            dat->noi_boxsize = boxsize;
         }
      }

      /* Only estimate the white noise level once at the beginning - the
         reason for this is to make measurements of the convergence
         easier. We may have done it prior to the start of iterations (in which
         case the relative weights will be influeced by low-frequency noise,
         this is initialized in smf_model_create), or or we may have already
         imported external noise values into the NOI model. If not, we calculate
         the noise now. */
      if( model_data[ 0 ] == 1.0 ) {

        /* First ensure the initial value is not 1.0. This is
           used as a test in smf_calcmodel_ast to check that the
           NOI model values ahave been set. The first element
           will be 1.0 if they have not been set. */
        model_data[ 0 ] = VAL__BADD;

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
          }


        /* If the NOI model is of the second form, the noise is estimated
           in boxes of samples lasting "NOI.BOX_SIZE" seconds, and then the
           noise level in the box is assigned to all samples in the box. */
        } else if( mntslice == ntslice ) {

          /* RecIf not already done, get NOI.BOX_SIZE and convert from seconds to
             samples. */
          if( idx == 0 ) {
            boxsize = dat->noi_boxsize;

            /* More initialisation needed for the 2 to 10 Hz power method. */
            if( box_type == 0 ) {
               boxsize = dat->noi_boxsize;
               msgOutf( "", FUNC_NAME ": Calculating a NOI variance for each "
                        "box of %d samples using 2-10 Hz noise.", status,
                        (int) boxsize );

              /* Create a smfData to hold one box-worth of input data. We
                 do not need to copy jcmtstate information. */
              if( res->sdata[idx]->hdr ) {
                 instate = res->sdata[idx]->hdr->allState;
                 res->sdata[idx]->hdr->allState = NULL;
              }
              box = smf_deepcopy_smfData( wf, res->sdata[idx], 0,
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

            } else {
               msgOutf( "", FUNC_NAME ": Calculating a NOI variance for each "
                        "box of %d samples using variance of neighbouring residuals.",
                        status, (int) boxsize );
            }
          }

          /* If required, find the noise as the mean power in the 2 to 10 Hz band. */
          if( box_type == 0 ) {

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

              /* Set dat->noi_boxsize to the number of times each NOI value is
                 duplicated. This is used to determine the degree of
                 compression that can be used when exporting the NOI model. */
              dat->noi_boxsize = boxsize;
            }

          /* Otherwise, set the noise to the variance of the neighbouring residuals. */
          } else {

            for( iw = 0; iw < nw; iw++ ) {
               pdata = job_data + iw;
               pdata->qua_data = qua_data;
               pdata->model_data = model_data;
               pdata->res_data = res_data;
               pdata->box = boxsize;
               pdata->operation = 2;
               thrAddJob( wf, 0, pdata, smf1_calcmodel_noi, 0, NULL, status );
            }
            thrWait( wf, status );

            /* Set dat->noi_boxsize to the number of times each NOI value is
               duplicated. This is used to determine the degree of
               compression that can be used when exporting the NOI model. */
            dat->noi_boxsize = 1;

          }

          /* Ensure that there are no BAD values in the NOI model. Also
             ensure that none of the initial unity values are left (this
             may be the case for isntance if an entire block has been
             flagged as bad (e.g. by the common-mode). We replace bad
             values and unity values with the mean noise level in the
             bolometer. */
          for( iw = 0; iw < nw; iw++ ) {
             pdata = job_data + iw;
             pdata->qua_data = qua_data;
             pdata->model_data = model_data;
             pdata->res_data = res_data;
             pdata->box = boxsize;
             pdata->operation = 3;
             thrAddJob( wf, 0, pdata, smf1_calcmodel_noi, 0, NULL, status );
          }
          thrWait( wf, status );

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
      if( *status == SAI__OK ) {

        for( iw = 0; iw < nw; iw++ ) {
           pdata = job_data + iw;
           pdata->mntslice = mntslice;
           pdata->qua_data = qua_data;
           pdata->model_data = model_data;
           pdata->res_data = res_data;
           pdata->mbstride = mbstride;
           pdata->mtstride = mtstride;
           pdata->lut_data = lut_data;
           pdata->map = dat->map;
           pdata->mapvar = dat->mapvar;
           pdata->operation = 1;

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
           dat->wchisquared[chunk] += pdata->wchisquared;
           dat->wchisq[chunk] += pdata->wchisq;
        }

      }
    }
  }

  /* Free resources */
  if( box ) {
     box->pntr[0] = astFree( box->pntr[0] );
     box->qual = astFree( box->qual );
     smf_close_file( wf, &box, status );
  }

  /* Free the job data. */
  job_data = astFree( job_data );

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
   dim_t nsum;
   double *pm;
   double *pr;
   double chisq;
   double dval;
   double mean;
   double sum;
   double vval;
   double wgt;
   int *pl;
   size_t ibase;
   smf_qual_t *pq;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfCalcModelNoiData *) job_data_ptr;

   if( pdata->operation == 1 ) {

/* Initialise returned chisquared increment and count of values. */
      pdata->chisquared = 0.0;
      pdata->nchisq = 0;

/* Initialise returned weighted chisquared increment and sum of weights. */
      pdata->wchisquared = 0.0;
      pdata->wchisq = 0.0;

/* Loop round all bolos to be processed by this thread, maintaining the
   index of the first time slice for the current bolo. */
      ibase = pdata->b1*pdata->bstride;
      for( ibolo = pdata->b1; ibolo <= pdata->b2; ibolo++ ) {

/* Get a pointer to the first quality value for the current bolo, and
   check that the whole bolometer has not been flagged as bad. */
         pq = pdata->qua_data + ibase;
         if( !( *pq & SMF__Q_BADB ) ) {

/* Get a pointer to the first residual and LUT value for the current bolo,
   and then loop round all time slices. */
            pr = pdata->res_data + ibase;
            pl = pdata->lut_data + ibase;
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
                  chisq = (*pr)*(*pr) / (*pm);
                  pdata->chisquared += chisq;
                  pdata->nchisq++;

/* Now form the chi-squared in the source regions. If the lut has a bad
   value, the sample is off the edge of the map. */
                  if( *pl != VAL__BADI ) {

/* If the map pixel that contains the current pixel has good data and
   variance values, get its SNR and use this as a weight to find
   the sum of weighted squared residuals. */
                     dval = pdata->map[ *pl ];
                     vval = pdata->mapvar[ *pl ];

                     if( dval != VAL__BADD && vval != VAL__BADD &&
                         vval > 0.0 ) {
                        wgt = fabs( dval )/sqrt( vval );
                        pdata->wchisquared += wgt*chisq;
                        pdata->wchisq += wgt;
                     }
                  }
               }

/* Move residual, lut and quality pointers on to the next time slice. */
               pq += pdata->tstride;
               pl += pdata->tstride;
               pr += pdata->tstride;
            }
         }

/* Increment the index of the first value associated with the next
   bolometer. */
         ibase += pdata->bstride;
      }

/* Set the noise to the variance of the neighbouring residuals. */
   } else if( pdata->operation == 2  ) {
      ibase = pdata->b1*pdata->bstride;
      for( ibolo = pdata->b1; ibolo <= pdata->b2; ibolo++ ) {
         pq = pdata->qua_data + ibase;
         if( !( *pq & SMF__Q_BADB ) ) {
            pr = pdata->res_data + ibase;
            pm = pdata->model_data + ibase;
            smf_boxcar1D( pr, pdata->ntslice, pdata->tstride, pdata->box, pq,
                          SMF__Q_FIT, 0, pm, status);
         }
         ibase += pdata->bstride;
      }

/* Replace bad values and unity values with the mean noise value in each
   bolometer. */
   } else if( pdata->operation == 3 ) {

/* Loop round all bolos to be processed by this thread, maintaining the
   index of the first time slice for the current bolo. */
      ibase = pdata->b1*pdata->bstride;
      for( ibolo = pdata->b1; ibolo <= pdata->b2; ibolo++ ) {

/* Get a pointer to the first quality value for the current bolo, and
   check that the whole bolometer has not been flagged as bad. */
         pq = pdata->qua_data + ibase;
         if( !( *pq & SMF__Q_BADB ) ) {

/* This operation is only used in cases where the NOI model has the same
   shape and size as the residuals. Get a pointer to the first NOI value
   for the current bolometer. */
            pm = pdata->model_data + ibase;

/* Loop round all time slices, foring the sums needed to calculate the
   mean of the non-bad, non-unity noise values. */
            sum = 0.0;
            nsum = 0;
            for( itime = 0; itime < pdata->ntslice; itime++ ) {

/* If the noise value is usable, increment the sums */
               if( *pm > 0 && *pm != VAL__BADD && *pm != 1.0 ) {
                  sum += *pm;
                  nsum++;
               }

/* Point to the next NOI value. */
               pm += pdata->tstride;
            }

/* If any bad or unity noise values were found, replace them with the
   mean noise value. */
            if( nsum < pdata->ntslice && nsum > 0 ) {
               mean = sum/nsum;
               pm = pdata->model_data + ibase;
               for( itime = 0; itime < pdata->ntslice; itime++ ) {
                  if( *pm < 0 || *pm == VAL__BADD || *pm == 1.0 ) {
                     *pm = mean;
                  }
                  pm += pdata->tstride;
               }
            }
         }

/* Increment the index of the first value associated with the next
   bolometer. */
         ibase += pdata->bstride;
      }


   } else {
      *status = SAI__ERROR;
      errRepf( "", "smf_calcmodel_noi: Illegal operation %d", status, pdata->operation );
   }
}
