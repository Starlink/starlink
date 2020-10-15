/*
*+
*  Name:
*     smf_fix_steps

*  Purpose:
*     Detect and correct any steps in the DC level in each bolometer.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_fix_steps( ThrWorkForce *wf, smfData *data, double dcthresh,
*                         int dcsmooth, int dcfitbox, int dcmaxsteps,
*                         int dclimcorr, int meanshift, dim_t *nrej,
*                         smfStepFix **steps, dim_t *nsteps, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     data = smfData * (Given and Returned)
*        The data that will be repaired (in-place). Locations of steps
*        will have bit SMF__Q_JUMP set.
*     dcthresh = double (Given)
*        The minimum ratio of (residual difference between adjacent
*        median-smoothed samples) to (the local RMS in the residual
*        differences) for a real jump. Should be of the order ot 20-30.
*     dcsmooth = int (Given)
*        The width in samples of the box to use when median smoothing the
*        original bolometer data. Should be not much larger than the
*        expected maximum width of a step (say 50).
*     dcfitbox = int (Given)
*        Length of box (in samples) over which each linear fit is
*        performed. If zero, no steps will be corrected. Should be
*        smallish (say 40).
*     dcmaxsteps = int (Given)
*        The maximum number of steps that can be corrected in each minute
*        of good data (taking into account the data step-time) from a
*        bolometer before the entire bolometer is flagged as bad. A value
*        of zero will cause a bolometer to be rejected if any steps are
*        found in the bolometer data stream.
*     dclimcorr = int (Given)
*        The detection threshold for steps that occur at the same time in
*        many bolometers. Set it to zero to suppress checks for correlated
*        steps. If dclimcorr is greater than zero, and a step is found at
*        the same time in more than "dclimcorr" bolometers, then all
*        bolometers are assumed to have a step at that time, and the step
*        is fixed no matter how small it is.
*     meanshift = int (Given)
*        If non-zero, smooth each bolometer times stream using a mean-shift
*        filter before doing anything else. A mean-shift filter is an
*        edge-preserving smooth. It can help to identify smaller steps,
*        but does not work well if there are strong gradients in the
*        bolometer time stream. Therefore, "meanshift" should only be
*        used once the common-mode signal has been subtracted. The
*        spatial width of the filter is given by dcsmooth, and the range of
*        data values accepted by the filter is 5 times the local RMS in
*        the original time stream.
*     nrej = dim_t * (Returned)
*        The number of bolometers rejected due to having too many steps.
*     steps = smfStepFix ** (Returned)
*        Address of a pointer to the first element of an array of
*        smfStepFix structures. If the pointer is NULL on entry, then a
*        new array is allocated and a pointer to the array is stored at the
*        supplied address on exit (the array should be freed using
*        astFree when no longer needed). If the supplied address is NULL,
*        then no array is allocated.
*
*        On exit, the number of elements in the array (if it exists) will
*        be equal to the value of "*nstep". Each element is a smfStepFix
*        structure that describes a single fixed step.
*     nstep = dim_t * (Returned)
*        The number of fixed steps.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     DC jumps are found and fixed in each bolometer independently of all
*     other bolometers.
*
*     The algorithm detects and measures DC jumps within the median
*     smoothed bolometer data. This is because median smoothing reduces
*     the noise whilst retaining the sharp edges at each DC jump (so long
*     as the smoothing is not too heavy), and also reduces the effect of
*     spikes, etc. After detecting and measuring all jump in the smoothed
*     bolometer data, the original unsmoothed bolometer data is corrected
*     to remove these jumps. An extra constant correction is applied in
*     order to retain the orignal mean data value in each bolometer.
*
*     The jump detection and measurement algorithm for a single bolometer
*     proceeds as follow:
*
*     The gradient at each sample is estimated by taking the difference
*     between the median smoothed data values on either side of the sample.
*     These differences contain both the local gradient in the
*     noise-free data, which is assumed to vary smoothly, and the
*     residual gradients caused by noise, spikes, DC jumps, etc. We need
*     to estimate the smoothly varying local gradient so that the residual
*     gradients - including DC jumps - can then be found. But we cannot
*     just smooth the differences to find the local gradient as jumps,
*     spikes etc will produce extended bumps in the local gradient. So we
*     first remove all differences which are more than three times the RMS
*     value of the differences (we also remove a small neighbourhood of
*     values around each such clipped value). We then smooth the remaining
*     differences, using a mean filter rather than a median for better
*     accuracy, to get the local gradient. Holes in this array are filled
*     in by linear interpolation.
*
*     The smooth local gradient is then subtracted off the original
*     differences to get the residual differences caused by noise, jumps,
*     spikes, etc. The local RMS value of these residual differences at
*     each sample is then found (this is done by smoothing the squared
*     differences using a median filter, and then taking the square root
*     of the smoothed squared differences). Blocks of samples that have
*     unusually large residual differences compared to the local RMS
*     value are then found (small gaps of low residual gradient are
*     allowed within these blocks). For each such block, a linear fit is
*     made to the median smoothed bolometer data just before the block
*     (leaving a small gap to avoid the areas of high gradient that seem
*     to preceed or follow many jumps). A similar linear fit is made just
*     after the end of the block. These two fits are used to produce
*     estimates of the expected data value at the centre of the block. If
*     this difference is large compared to the noise in the bolometer,
*     and also compared to the uncertainty in the jump height, then a
*     DC step is deemed to exist at the centre of the block, with height
*     equal to the difference between the central data value estimated by
*     the two fits. The uncertainty in the jump height is determined by
*     doing several other linear fits to the data is slightly shifted
*     boxes, and seeing how the jump height varies.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-MAR-2010 (DSB):
*        Original version.
*     25-MAR-2010 (DSB):
*        - Change dcminstepwidth from 20 to 0.8*dcwidth (instantaneous
*        steps should have a width of dcwidth).
*        - Change the way the median filtering is done to use one filter
*        rather than two. This should be faster.
*        - Reject blocks of steep samples if the median value at the
*        start and end of the block are insufficiently different.
*        - Extract median filtering code into another routine.
*     13-MAY-2010 (DSB):
*        - The RMS value in the gradient array could be badly affected
*        by very large steps. So do three 3*sigma rejection iterations
*        to make the rms more robust.
*        - Replace incorrect "break" with "continue". This could
*        cause the whole function to return early if a bolometer
*        with too few usable values was found.
*        - Correct counting of rejected bolometers (returned in *nrej).
*     18-MAY-2010 (DSB):
*        Ensure the first and last dcwidth samples in each bolometer
*        are continuous with the intermediate data.
*     21-MAY-2010 (DSB):
*        Added dclimcorr.
*     24-MAY-2010 (DSB):
*        - Do not alter the padding values at start and end of each bolometer
*        time series.
*        - Apodize the initial and final correction for each bolometer
*        time series, in the same way that smf_apodize does.
*     25-MAY-2010 (DSB):
*        - Increase dcmaxstepwidth from 1.8*dcwidth to
*        3.0*dcwidth (Remo has data which shows steps that include
*        two distinct sub-steps, separated by around 50 samples).
*        - Change dcminstepgap from 50 to 0.5*dcwidth to allow
*        such sub-steps (sometimes) to be processed as two separate steps.
*        - Re-organise the debugging facilities.
*        - If a step occurs too close to the start or end to be fixed,
*        flag all samples as a jump up tp the start or end.
*     28-MAY-2010 (DSB):
*        - Exclude data previously flagged as a jump when fitting data before
*        and after a candidate step.
*        - Fix incorrect indexing of quality array when steps are found close
*        to the start or end of the time series.
*        - Clip at 2 sigma (not 3) when finding the RMS gradient, and
*        then correct for the heavy clipping using the factor appropriate
*        for pure Gaussian noise. heavier clipping does better in the
*        presence of lots of steps.
*     11-JUN-2010 (DSB):
*        Report the number of fixed steps if in verbose mode.
*     25-JUN-2010 (DSB):
*        Apodisation is now done later in smf_execute_filter, so there
*        is no need to apodise in this function.
*     6-JUL-2010 (DSB):
*        - Rename old "nstep" argument as "nrej".
*        - Added arguments "steps" and "nstep".
*     27-AUG-2010 (DSB):
*        Complete re-write. The main difference is that the jumps are now
*        detected and measured in the median s,moothed bolometer data,
*        rather than the original bolometer data. But there are manu
*        other less significant changes too.
*     13-SEP-2010 (DSB):
*        Added argument "bcount".
*     21-SEP-2010 (DSB):
*        Ignore steps that occur close to bright sources. Such sources
*        cause flat sections in the median smoothed data that give the
*        appearance of a step in the residual differences.
*     23-SEP-2010 (DSB):
*        - Flat sections in the median smoothed data can be caused by
*        things other than bright sources, so change the test for
*        proximity to a bright source.
*        - Removed argument "bcount".
*        - Re-instate DCLIMCORR and correction of correlated steps.
*     29-APR-2011 (DSB):
*        Assign Q_JUMP to bolometers that are totally rejected (as well
*        as Q_BADB).
*     23-NOV-2011 (DSB):
*        Made changes so that steps that are close together (within a few
*        hundred samples) get handled better.
*     9-DEC-2011 (DSB):
*        Added mean shift filter option.
*     7-OCT-2011 (DSB):
*        Several of the constants used in this algorithm assume a fixed
*        sample. In pratice, sample rates vary because of down-sampling
*        etc. In particular, the sample rate for time-streams holding
*        POL-2 Stokes parameters (produced by calcqu) is 2 Hz, very much
*        lower than normal SCUBA-2 maps. The usual constant values are
*        inappropriate for this is very low sample rate and cause lots of
*        problems in the step-fixing algorithm. So we need to change the
*        constants to take the actual sample rate into account. But such
*        a change could cause potentially important changes to appear in
*        normal SCUBA_2 maps, which would require a long investigation to
*        evaluate. We should maybe do this some day, but at he moment the
*        top priority is getting POL-2 working. So today's code change
*        only changes the constants for data at sample rates below 20 Hz
*        (corresponding to a scan speed of 80 arcsec/sec for 4 arcsec
*        pixels). This should fix the problems for POL-2 but leave normal
*        SCUBA-2 maps unchanged. The values affected are dcfill, dcmaxwidth,
*        dcsmooth2, dcnlow, dcpeakoff, dcpeakwidth, dcpeakminwidth. In
*        addition the dcmaxsteps value is also scaled, but this has to be
*        applied at all sample rates since it is a configuration parameter
*        and not defined locally. The scaling of dcmaxsteps leaves the
*        value unchanged for a sample rate of 80 Hz (a scan speed of 320
*        arcsec/sec at 4 arcsec pixels).
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     Copyright (C) 2016 East Asian Observatory.
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
#include "libsmf/smf_typ.h"
#include "libsmf/smf_err.h"


/* Local data types: */
typedef struct smfFixStepsJobData {
   dim_t b1;
   dim_t b2;
   int dcfitbox;
   int dcsmooth;
   dim_t nbolo;
   dim_t ntslice;
   double *bolonoise;
   double *dat;
   double dccut;
   double dcthresh2;
   double dcthresh3;
   double dcthresh;
   dim_t *bcount;
   dim_t dcfill;
   int dclimcorr;
   int dcmaxsteps;
   dim_t dcmaxwidth;
   int dcsmooth2;
   int meanshift;
   dim_t nfixed;
   dim_t nstep;
   dim_t bstride;
   dim_t nrej;
   dim_t tstride;
   smfStepFix *steps;
   smf_qual_t *qua;
   smfData *data;
} smfFixStepsJobData;


/* Define macro DEBUG_STEPS here, if debugging info is needed. NOTE, only
   use debugging facilities in SINGLE THREADED mode.
#define DEBUG_STEPS 1
*/

typedef struct Step {
   dim_t start;
   dim_t end;
   double minjump;

#ifdef DEBUG_STEPS
   double error;
   double jump;
   dim_t ok;
   dim_t ibolo;
   dim_t peak;
   double peak1;
   double peak2;
   dim_t peakwidth;
   double vlo;
   double vlo_mean;
   double vlo_sigma;
   double vhi;
   double vhi_mean;
   double vhi_sigma;
#endif

} Step;


#define RECORDED_BOLO 1076

#ifdef DEBUG_STEPS

#ifdef RECORDED_BOLO
static dim_t debug_bolo = RECORDED_BOLO;
#else
static dim_t debug_bolo = -1;
#endif
static dim_t get_debug_bolo( void );

#define RECORD_BOLO (get_debug_bolo()==ibolo)


#define RECORD_BOLO2 (1)

#define TOPCAT(fd, x) \
   if( x != VAL__BADD ) { \
      fprintf( fd, "%g ", x ); \
   } else { \
      fprintf( fd, "null " ); \
   }

typedef struct TimeData {
   double indata;
   dim_t inquality;
   double outdata;
   dim_t outquality;
   dim_t ibolo;
   double median;
   double meanshift;
   double diff;
   double thresh;
   double sdiff;
   double rdiff;
   double mdiff;
   double mdiff2;
   double diff2;
   double rms;
   dim_t instep;
   dim_t step_width;
   double total;
   double snr;
} TimeData;


#endif



/* Prototypes for private functions defined in this file. */

static dim_t smf1_correct_steps( dim_t ntslice, double *dat, smf_qual_t *qua,
                               dim_t tstride, double *median, double *snr,
                               int dcfitbox, double dcthresh2,
                               dim_t nbstep, Step *bsteps, dim_t ibolo,
                               int meanshift, double steptime,
                               smfStepFix **steps, dim_t *nsteps,
                               double *grad, double *off, dim_t *bcount,
                               int *status );

static void smf1_fix_steps_job( void *job_data, int *status );
static void smf1_fix_correlated_steps_job( void *job_data, int *status );



/* Main entry point. */

void smf_fix_steps( ThrWorkForce *wf, smfData *data, double dcthresh,
                    int dcsmooth, int dcfitbox, int dcmaxsteps,
                    int dclimcorr, int meanshift, dim_t *nrej,
                    smfStepFix **steps, dim_t *nstep, int *status ) {

/* Local Variables */
   dim_t *bcount;
   dim_t bstep;
   dim_t bstride;
   dim_t istep;
   dim_t itime;
   dim_t iworker;
   dim_t nbolo;
   dim_t nfixed;
   dim_t ntslice;
   dim_t nworker;
   dim_t tstride;
   double *bolonoise = NULL;
   double *dat = NULL;
   smfFixStepsJobData *job_data = NULL;
   smfFixStepsJobData *pdata = NULL;
   smf_qual_t *qua = NULL;

/* The minimum number of samples between steps. Large differences that
   are separated by less than "dcfill" samples are considered to be part of
   the same jump. */
   dim_t dcfill = 100;

/* The maximum width of a step. Candidate steps that are wider than this
   number of samples are left uncorrected. */
   dim_t dcmaxwidth = 2*dcfill;

/* The size of the median filter to use when estimating the local RMS at
   each point. */
   int dcsmooth2 = 200;

/* The sigma threshold for points to be included in the estimation of the
   local gradient at each point. */
   double dccut = 4.0;

/* The sigma threshold for acceptable jumps - the ratio of jump height to
   the uncertainty in jump height caused by moving the fitting area. */
   double dcthresh2 = 1.5;

/* The smallest jump to be corrected, as a factor of the local RMS
   noise in the bolometer data. */
   double dcthresh3 = meanshift ? 0.4 : 4.0;

/* Initialise... */
   if( nstep ) *nstep = 0;
   if( nrej ) *nrej = 0;
   nfixed = 0;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Check we have double precision data. */
   if( !smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status ) ) return;

/* Used to dump the input data (e.g. for use in a regression test). Must
   be time ordered, otherwise errors occurr when smf_open_file opens the
   file. */
#ifdef DUMP_INPUT
   dim_t isT = data->isTordered;
   msgOut( "", "Dumping smf_fix_steps input data to NDF 'fix_steps_input.sdf'.",
           status );
   smf_dataOrder( wf, data, 1, status );
   smf_write_smfData ( wf, data, NULL,
                       "fix_steps_input", NULL, 0, 0, MSG__VERB, 0, status );
   smf_dataOrder( wf, data, isT, status );
#endif

/* Get pointers to data and quality arrays. */
   dat = data->pntr[ 0 ];
   qua = smf_select_qualpntr( data, NULL, status );

/* Report an error if either are missing. */
   if( !qua ) {
      *status = SAI__ERROR;
      errRep( "", "smf_fix_steps: No valid QUALITY array was provided", status );

   } else if( !dat ) {
      *status = SAI__ERROR;
      errRep( "", "smf_fix_steps: smfData does not contain a DATA component",
              status );

/* Report an error if no header is available. */
   } else if( !data->hdr ) {
      *status = SAI__ERROR;
      errRep( "", "smf_fix_steps: smfData does not contain a header",
              status );
   }

/* Get the data dimensions and strides. */
   smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, NULL, &bstride,
                 &tstride, status );

/* Report an error if the data stream is too short for the box size. */
   if( dcfitbox*2 > ntslice && *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSetk( "NTSLICE", ntslice );
      msgSeti( "DCFITBOX", dcfitbox );
      errRep( " ", "smf_fix_steps: Can't find jumps: ntslice=^NTSLICE, "
              "must be > dcfitbox (=^DCFITBOX)*2", status );
   }

/* Check for valid threshold */
   if( dcthresh <= 0  && *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSetd( "DCTHRESH", dcthresh );
      errRep( " ", "smf_fix_steps: Can't find jumps: dcthresh "
              "(^dcthresh) must be > 0", status );
   }


/* Scale algorithm parameters that are defind as a number of samples to
   take account of the actual scan speed. The initial values set above
   have been shown to be appropriate for "typcal" SCUBA-2 maps, but POL-2
   uses a much lower scan speed and so the above values are inappropriate.
   We scale the initalised values so that they are unchanged for  scan
   speed of 320 arcsec/sec (a value representative of non-POL2 data).
   This means that the scaling should have minimal effect on the
   established behaviour of this function for normal SCUBA-2 maps.
   To ensure that normal SCUBA-2 maps are left unchanged, we only do the
   scaling for sample rates below 20 Hz (i.e. scan speeds below 80
   arcsec/sec assuming 4 arcsec pixels). */
   if( 1.0/data->hdr->steptime < 20.0 ) {

/* The minimum number of samples between steps. Large differences that
   are separated by less than "dcfill" samples are considered to be part of
   the same jump. */
      dcfill = 0.5 + dcfill/(320.0*data->hdr->steptime );
      if( dcfill < 3 ) dcfill = 3;

/* The maximum width of a step. Candidate steps that are wider than this
   number of samples are left uncorrected. */
      dcmaxwidth = 2*dcfill;

/* The size of the median filter to use when estimating the local RMS at
   each point. */
      dcsmooth2 = (int)( 0.5 + dcsmooth2/(320.0*data->hdr->steptime ) );
      if( dcsmooth2 < 40 ) dcsmooth2 = 40;
   }

/* Allocate a work array to hold the noise level in each bolometer. */
   bolonoise = astMalloc( nbolo*sizeof( *bolonoise ) );

/* Allocate a work array to hold the number of bolometers that show a
   jump at each time slice. */
   bcount = ( dclimcorr > 0 ) ? astCalloc( ntslice, sizeof( *bcount ) ) : NULL;

/* Create structures used to pass information to the worker threads. */
   nworker = wf ? wf->nworker : 1;

#ifdef DEBUG_STEPS
   nworker = 1;
#endif

   job_data = astMalloc( nworker*sizeof( *job_data ) );

/* Find and repair DC steps. */
   if( dcfitbox && (*status == SAI__OK) ) {

/* Determine which bolometers are to be processed by which threads. */
      bstep = nbolo/nworker;
      if( bstep < 1 ) bstep = 1;

      for( iworker = 0; iworker < nworker; iworker++ ) {
         pdata = job_data + iworker;
         pdata->b1 = iworker*bstep;
         pdata->b2 = pdata->b1 + bstep - 1;
      }

/* Ensure that the last thread picks up any left-over bolometers */
      pdata->b2 = nbolo - 1;

/* Store all the other info needed by the worker threads, and submit the
   jobs to fix the steps in each bolo, and then wait for them to complete. */
      for( iworker = 0; iworker < nworker; iworker++ ) {
         pdata = job_data + iworker;

         pdata->bstride = bstride;
         pdata->bolonoise = bolonoise;
         pdata->data = data;
         pdata->dat = dat;
         pdata->dccut = dccut;
         pdata->dcfill = dcfill;
         pdata->dcfitbox = dcfitbox;
         pdata->dcmaxsteps = dcmaxsteps;
         pdata->dcmaxwidth = dcmaxwidth;
         pdata->dclimcorr = dclimcorr;
         pdata->dcsmooth = dcsmooth;
         pdata->dcthresh = dcthresh;
         pdata->dcsmooth2 = dcsmooth2;
         pdata->dcthresh2 = dcthresh2;
         pdata->dcthresh3 = dcthresh3;
         pdata->meanshift = meanshift;
         pdata->nbolo = nbolo;
         pdata->ntslice = ntslice;
         pdata->qua = qua;
         pdata->tstride = tstride;
         pdata->nstep = ( steps && nstep ) ? 1 : 0;

/* Allocate work array to hold, for each time slice, the total number of
   bolometers found to be in a jump at that time slice. Initialise it to
   hold zero at every element. */
         pdata->bcount = bcount ?
                         astCalloc( ntslice, sizeof( *(pdata->bcount) ) ) :
                         NULL;

/* Pass the job to the workforce for execution. */
         thrAddJob( wf, THR__REPORT_JOB, pdata, smf1_fix_steps_job, 0, NULL,
                      status );
      }

/* Wait for the workforce to complete all jobs. */
      thrWait( wf, status );

/* Accumuate the returned values from each thread. */
      for( iworker = 0; iworker < nworker; iworker++ ) {
         pdata = job_data + iworker;

         nfixed += pdata->nfixed;
         if( nrej ) *nrej += pdata->nrej;

         if( steps && nstep ) {
            istep = *nstep;
            *nstep += pdata->nstep;
            *steps = astGrow( *steps, *nstep, sizeof( **steps ) );

            if( *status == SAI__OK ) {
               memcpy( *steps + istep, pdata->steps,
                       pdata->nstep*sizeof( **steps ) );
            }
         }

         pdata->steps = astFree( pdata->steps );

         if( bcount ) {
            for( itime = 0; itime < ntslice; itime++ ) {
               bcount[ itime ] += pdata->bcount[ itime ];
            }
            pdata->bcount = astFree( pdata->bcount );
         }

      }

/* If required, fix correlated steps. */
      if( dclimcorr > 0 ) {

/* Update the info needed by the worker threads, and submit the jobs to
   fix the correlated steps in each bolo, and then wait for them to
   complete. */
         for( iworker = 0; iworker < nworker; iworker++ ) {
            pdata = job_data + iworker;
            pdata->bcount = bcount;
            thrAddJob( wf, THR__REPORT_JOB, pdata,
                         smf1_fix_correlated_steps_job, 0, NULL, status );
         }

/* Wait for the workforce to complete all jobs. */
         thrWait( wf, status );

/* Accumuate the returned values from each thread. */
         for( iworker = 0; iworker < nworker; iworker++ ) {
            pdata = job_data + iworker;

            nfixed += pdata->nfixed;
            if( nrej ) *nrej += pdata->nrej;

            if( steps && nstep ) {
               istep = *nstep;
               *nstep += pdata->nstep;
               *steps = astGrow( *steps, *nstep, sizeof( **steps ) );

               if( *status == SAI__OK ) {
                  memcpy( *steps + istep, pdata->steps,
                          pdata->nstep*sizeof( **steps ) );
               }
            }
            pdata->steps = astFree( pdata->steps );
         }
      }
   }

/* Report the number of fixed steps */
   msgOutiff( MSG__VERB, " ", "smf_fix_steps: fixed %d steps.",
              status, (int) nfixed );

/* Report the number of rejected bolometers. */
   if( *nrej > 0 ) {
      msgOutiff( MSG__VERB, " ", "smf_fix_steps: flagged %zu bad bolos.",
                 status, *nrej );
   }

/* Return the number of fixed steps. */
   if( nstep ) *nstep = nfixed;

/* Free resources. */
   job_data = astFree( job_data );
   bolonoise = astFree( bolonoise );
   bcount = astFree( bcount );
}


static void smf1_fix_steps_job( void *job_data, int *status ) {
/*
*  Name:
*     smf1_fix_steps_job

*  Purpose:
*     Fix steps for a block of bolometers.

*  Invocation:
*     void smf1_fix_steps_job( void *job_data, int *status )

*  Arguments:
*     job_data = void * (Given)
*        Pointer to the data needed by the job. Should be a pointer to a
*        smfFixStepsJobData structure.
*     status = dim_t * (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine finds and fixes steps in a block of bolometers. It
*     runs within a thread instigated by smf_fix_steps.

*/

/* Local Variables: */
   Step *bsteps = NULL;
   dim_t *bcount;
   dim_t *mw2;
   dim_t *mw3;
   dim_t allequal;
   dim_t b1;
   dim_t b2;
   dim_t base;
   dim_t bstride;
   dim_t dcfill;
   dim_t dcmaxwidth;
   dim_t gap_start;
   dim_t ibolo;
   dim_t ibstep;
   dim_t iter;
   dim_t itime;
   dim_t jhi;
   dim_t jlo;
   dim_t jtime;
   dim_t lbad;
   dim_t maxsteps;
   dim_t mbstep;
   dim_t msize;
   dim_t nbolo;
   dim_t nbstep;
   dim_t nfixed;
   dim_t nrej;
   dim_t nstep;
   dim_t nsum;
   dim_t ntslice;
   dim_t pad;
   dim_t step_end;
   dim_t step_limit;
   dim_t step_start;
   dim_t step_width;
   dim_t tstride;
   double *bolonoise;
   double *dat = NULL;
   double *mw1;
   double *pd;
   double *pw1_hi;
   double *pw1_lo;
   double *pw2;
   double *pw3;
   double *pw4;
   double *w1;
   double *w2;
   double *w3;
   double *w4;
   double *w5;
   double dccut;
   double dcthresh2;
   double dcthresh3;
   double dcthresh;
   double delta;
   double diff2;
   double diff;
   double lval;
   double max_snr_jump;
   double rdiff;
   double rms;
   double sdiff;
   double snr_jump;
   double sum2;
   double thresh;
   double total;
   double vlo;
   int dcfitbox;
   int dcmaxsteps;
   int dcsmooth2;
   int dcsmooth;
   int meanshift;
   smfData *data = NULL;
   smfFixStepsJobData *pdata;
   smfStepFix *steps;
   smf_qual_t *pq;
   smf_qual_t *qua = NULL;
   smf_qual_t *wq = NULL;
   struct timeval tv1;
   struct timeval tv2;

   dim_t dcgappad = 50;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer to the job data, and then extract its contents into a
   set of local variables. */
   pdata = (smfFixStepsJobData *) job_data;

   b1 = pdata->b1;
   b2 = pdata->b2;
   bstride = pdata->bstride;
   bcount = pdata->bcount;
   dat = pdata->dat;
   data = pdata->data;
   dccut = pdata->dccut;
   dcfill = pdata->dcfill;
   dcfitbox = pdata->dcfitbox;
   dcmaxsteps = pdata->dcmaxsteps;
   dcmaxwidth = pdata->dcmaxwidth;
   dcsmooth = pdata->dcsmooth;
   dcsmooth2 = pdata->dcsmooth2;
   dcthresh = pdata->dcthresh;
   dcthresh2 = pdata->dcthresh2;
   dcthresh3 = pdata->dcthresh3;
   meanshift = pdata->meanshift;
   nbolo = pdata->nbolo;
   ntslice = pdata->ntslice;
   qua = pdata->qua;
   tstride = pdata->tstride;
   bolonoise = pdata->bolonoise;

/* Initialise the returned values. */
   nrej = 0;
   bsteps = NULL;
   steps = NULL;
   nstep = 0;
   nfixed = 0;

/* Check we have something to do. */
   if( b1 < nbolo ) {

/* Debugging message indicating thread started work */
      msgOutiff( SMF__TIMER_MSG, "", "smfFixSteps: thread starting on bolos %zu -- %zu",
                 status, b1, b2 );
      smf_timerinit( &tv1, &tv2, status);

#ifdef DEBUG_STEPS
   FILE *fd2 = fopen( "timedata.asc", "w" );
   fprintf( fd2, "# itime indata inquality outdata outquality ibolo meanshift median "
            "diff thresh sdiff rdiff mdiff mdiff2 diff2 rms instep step_width total snr\n");

   FILE *fd3 = fopen( "stepdata.asc", "w" );
   fprintf( fd3, "# ibstep start end minjump error jump ok ibolo "
            "jump peak peak1 peak2 peakwidth vlo vlo_mean vlo_sigma vhi vhi_mean vhi_sigma\n" );

   TimeData *timedata = astMalloc( ntslice*sizeof( *timedata ) );
#endif

/* Ensure dcfitbox is odd. */
      dcfitbox = 2*( dcfitbox/2 ) + 1;

/* Allocate work arrays. */
      msize = 3*dcfitbox;
      if( ntslice > msize ) msize = ntslice;
      w1 = astMalloc( sizeof( *w1 )*msize );
      w2 = astMalloc( sizeof( *w2 )*msize );
      w3 = astMalloc( sizeof( *w3 )*msize );
      w4 = astMalloc( sizeof( *w4 )*msize );
      w5 = astMalloc( sizeof( *w5 )*msize );
      if( tstride > 1 && meanshift ) wq =  astMalloc( sizeof( *wq )*msize );

      msize = dcsmooth;
      if( dcsmooth2 > msize ) msize = dcsmooth2;
      mw1 = astMalloc( sizeof( *mw1 )*msize );
      mw2 = astMalloc( sizeof( *mw2 )*msize );
      mw3 = astMalloc( sizeof( *mw3 )*msize );

/* Loop round all bolometers to be processed by this thread. "base" holds the
   offset to the start of the data for the bolometer.  */
      for( ibolo = b1; ibolo <= b2 && *status==SAI__OK; ibolo++ ) {
         base = ibolo*bstride;
         pq = qua + base;
         if( !(*pq & SMF__Q_BADB) ) {

/* Get a contiguous copy of the quality array. Only needed if doing a
   mean shift filter. */
            if( meanshift ) {
               if( tstride > 1 ) {
                  for( itime = 0; itime < ntslice - 1; itime++ ) {
                     wq[ itime ] = *pq;
                     pq += tstride;
                  }
               } else {
                  wq = pq;
               }
            }

#ifdef DEBUG_STEPS

   if( RECORD_BOLO ) {
      dim_t kk;
      pd = dat + base;
      pq = qua + base;
      for( kk = 0; kk < ntslice; kk++ ) {
         timedata[ kk ].indata = *pd;
         timedata[ kk ].inquality = (int) *pq;
         timedata[ kk ].outdata = VAL__BADD;
         timedata[ kk ].outquality = 0;
         timedata[ kk ].ibolo = ibolo;
         timedata[ kk ].meanshift = VAL__BADD;
         timedata[ kk ].median = VAL__BADD;
         timedata[ kk ].diff = VAL__BADD;
         timedata[ kk ].thresh = VAL__BADD;
         timedata[ kk ].sdiff = VAL__BADD;
         timedata[ kk ].rdiff = VAL__BADD;
         timedata[ kk ].mdiff = VAL__BADD;
         timedata[ kk ].mdiff2 = VAL__BADD;
         timedata[ kk ].diff2 = VAL__BADD;
         timedata[ kk ].rms = VAL__BADD;
         timedata[ kk ].instep = 0;
         timedata[ kk ].step_width = 0;
         timedata[ kk ].total = VAL__BADD;
         timedata[ kk ].snr = VAL__BADD;

         pd += tstride;
         pq += tstride;
      };
      pq = qua + base;
   }

#endif



/* Get an estimate of the noise in the bolometer. Use zero if the
   bolometer has insufficient good samples. */
            if( *status == SAI__OK ) {
               rms = smf_quick_noise( data, ibolo, 20, 50, SMF__Q_GOOD, status );
               if( *status != SAI__OK ) {
                  rms = 0.0;
                  errAnnul( status );
               }
            }

/* Smooth the input data with an edge-preserving mean-shift filter, putting
   the results in w5. */
            if( meanshift ) {
               smf_meanshift( dat + base, w5, ntslice, tstride, dcsmooth, 5*rms,
                              qua + base, SMF__Q_GOOD, 0.0, status );

#ifdef DEBUG_STEPS
if( RECORD_BOLO ) {
   printf("Quick RMS is %g\n", rms );
   for( itime = 1; itime < ntslice - 1; itime++ ) {
      timedata[ itime ].meanshift = w5[itime];
   }
}
#endif

/* Median smooth the above data, putting the results in w1. */
               smf_median_smooth( dcsmooth, SMF__FILT_MEDIAN, -1.0, ntslice,
                                  w5, wq, 1, SMF__Q_GOOD, w1, mw1, mw2, mw3,
                                  status );

/* If mean-shift filter is not required, just median filter. */
            } else {
               smf_median_smooth( dcsmooth, SMF__FILT_MEDIAN, -1.0, ntslice,
                                  dat + base, qua + base, tstride, SMF__Q_GOOD,
                                  w1, mw1, mw2, mw3, status );
            }

#ifdef DEBUG_STEPS
if( RECORD_BOLO ) {
   printf("Quick RMS is %g\n", rms );
   for( itime = 1; itime < ntslice - 1; itime++ ) {
      timedata[ itime ].median = w1[itime];
   }
}
#endif


/* Into each sample of w2 put the difference between the two adjacent
   median smoothed data values. Store a copy of these differences in w3.
   Also form the sums needed to calculate the RMS of the differences. */
            pw1_lo = w1;
            pw1_hi = w1 + 2;
            pw2 = w2 + 1;
            pw3 = w3 + 1;
            allequal = 1;
            nsum = 0;
            sum2 = 0.0;

            w2[ 0 ] = w3[ 0 ] = VAL__BADD;

            for( itime = 1; itime < ntslice - 1; itime++,pw1_lo++,pw1_hi++ ) {

               if( *pw1_lo != VAL__BADD && *pw1_hi != VAL__BADD ) {
                  diff = *pw1_hi - *pw1_lo;
                  sum2 += diff*diff;
                  nsum++;
                  if( diff != 0.0 ) allequal = 0;
               } else {
                  diff = VAL__BADD;
               }

#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      timedata[ itime ].diff = diff;
   }
#endif

               *(pw2++) = *(pw3++) = diff;
            }

            w2[ itime ] = w3[ itime ] = VAL__BADD;

/* If the time stream contains too few values, or all values are equal,
   flag the entire bolometer as bad, and pass on to the next bolometer. */
            if( nsum <= SMF__MINSTATSAMP || allequal ) {
               msgOutiff( MSG__DEBUG, "", "smf_fix_steps: flagging "
                          "entire bad bolo %" DIM_T_FMT ", due to "
                          "insufficient samples", status, ibolo );
               pq = qua + base;
               for( itime = 0; itime < ntslice; itime++) {
                 *pq |= ( SMF__Q_BADB | SMF__Q_JUMP );
                  pq += tstride;
               }
               nrej++;
               continue;
            }

/* Do two sigma-clipping iterations to prevent the RMS being heavily
   affected by abberant values. */
            for( iter = 0; iter < 2 && nsum > 0; iter++ ) {

/* Get the RMS of the difference values. */
               rms = sqrt( sum2/nsum );

/* Set the w3 differences bad if they are bigger than "dccut" times the
   rms, and update the sums to exclude these samples. */
               thresh = dccut*rms;
               pw3 = w3;
               for( itime = 0; itime < ntslice; itime++,pw3++ ) {
                  if( *pw3 != VAL__BADD && fabs( *pw3 ) > thresh ) {
                     diff = *pw3;
                     *pw3 = VAL__BADD;
                     sum2 -= diff*diff;
                     nsum--;
                  }
               }
            }



#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      pw3 = w3;
      for( itime = 0; itime < ntslice; itime++ ) {
         timedata[ itime ].mdiff = *(pw3++);
      }
   }
#endif



/* Expand sections of bad samples in w3 to avoid using samples on the
   edges of steps when estimating the local gradient. Each section is
   doubled in width, whilst retaining the original centre. First take a
   copy of w3 (in w4), and then modify the values in w3. */
            w4 = astStore( w4, w3, ntslice*sizeof( *w3 ) );
            pw3 = w3;
            pw4 = w4;
            lbad = 0;
            jlo = -1;
            for( itime = 0; itime < ntslice; itime++,pw3++,pw4++ ) {
               if( lbad ) {
                  if( *pw4 != VAL__BADD ) {
                     jhi = itime - 1;
                     lbad = 0;

                     pad = ( jhi - jlo )/2;
                     if( pad < 4 ) pad = 4;
                     if( pad > 100 ) pad = 100;

                     jhi = jhi + pad;
                     if( jhi >= ntslice ) jhi = ntslice - 1;

                     jlo = jlo - pad;
                     if( jlo < 0 ) jlo = 0;

                     for( jtime = jlo; jtime <= jhi; jtime++ ) w3[ jtime ] = VAL__BADD;
                  }
               } else {
                  if( *pw4 == VAL__BADD ) {
                     jlo = itime;
                     lbad = 1;
                  }
               }
            }


#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      pw3 = w3;
      for( itime = 0; itime < ntslice; itime++ ) {
         timedata[ itime ].mdiff2 = *(pw3++);
      }
   }
#endif

/* Find the RMS of the difference between adjacent samples in the
   bolometer data, excluding aberrant points. We need to be more cautious
   when dealing with the raw bolometer data rather than the
   median-smoothed bolometer data, since it will contain more abberant
   points. So first widen the bad pixel ranges.  */
            w4 = astStore( w4, w3, ntslice*sizeof( *w3 ) );
            pw3 = w3;
            pw4 = w4;
            lbad = 0;
            for( itime = 0; itime < ntslice; itime++,pw3++,pw4++ ) {
               if( lbad ) {
                  if( *pw3 != VAL__BADD ) {
                     jhi = ntslice - itime;
                     if( jhi > dcgappad ) jhi = dcgappad;
                     for( jtime = 0; jtime < jhi; jtime++ ) pw4[ jtime ] = VAL__BADD;
                     lbad = 0;
                  }
               } else {
                  if( *pw3 == VAL__BADD ) {
                     jlo = -itime;
                     if( jlo < -dcgappad ) jlo = -dcgappad;
                     for( jtime = jlo; jtime < 0; jtime++ ) pw4[ jtime ] = VAL__BADD;
                     lbad = 1;
                  }
               }
            }

/* Now find the RMS noise in the remaining bolometer data. */
            nsum = 0;
            sum2 = 0.0;
            pw4 = w4;
            pd = dat + base;
            lval = VAL__BADD;
            for( itime = 0; itime < ntslice; itime++,pw4++ ) {
               if( *pw4 != VAL__BADD ){
                  if( *pd != VAL__BADD && lval != VAL__BADD ) {
                     diff = *pd - lval;
                     sum2 += diff*diff;
                     nsum++;
                  }
                  lval = *pd;
               } else {
                  lval = VAL__BADD;
               }
               pd += tstride;
            }

            if( nsum <= SMF__MINSTATSAMP ) {
               msgOutiff( MSG__DEBUG, "", "smf_fix_steps: flagging "
                          "entire bad bolo %" DIM_T_FMT ", due to "
                          "insufficient samples", status, ibolo );
               pq = qua + base;
               for( itime = 0; itime < ntslice; itime++) {
                 *pq |= ( SMF__Q_BADB | SMF__Q_JUMP );
                  pq += tstride;
               }
               nrej++;
               continue;
            }

            bolonoise[ ibolo ] = 0.707*sqrt( sum2/nsum );

/* Smooth the clipped differences with a mean tophat filter to get an
   estimate of the underlying local gradient. */
            smf_tophat1D( w3, ntslice, dcsmooth, NULL, 0, 0.2, status );

/* Fill any holes in the smoothed differences using linear interpolation. */
            gap_start = -1;
            vlo = VAL__BADD;

            pw3 = w3;
            for( itime = 0; itime < ntslice; itime++,pw3++ ) {
               if( *pw3 == VAL__BADD ) {
                  if( gap_start == -1 ) gap_start = itime;

               } else {
                  if( gap_start != -1 && vlo != VAL__BADD ) {

                     delta = ( *pw3 - vlo )/( itime - gap_start + 1 );
                     for( jtime = gap_start; jtime < itime; jtime++ ) {
                        vlo += delta;
                        w3[ jtime ] = vlo;
                     }

                     gap_start = -1;
                  }

                  vlo = *pw3;
               }
            }

/* Subtract the smoothed differences from the original differences, and
   store the squared residual differences in w3. */
            pw2 = w2;
            pw3 = w3;
            for( itime = 0; itime < ntslice; itime++,pw2++,pw3++ ) {
               if( *pw2 != VAL__BADD && *pw3 != VAL__BADD ) {
                  sdiff = *pw3;
                  rdiff = *pw2 - sdiff;
                  *pw2 = rdiff;
                  *pw3 = rdiff*rdiff;

#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      timedata[ itime ].sdiff = sdiff;
      timedata[ itime ].rdiff = rdiff;
      timedata[ itime ].diff2 = *pw3;
   }
#endif

               } else {
                  *pw2 = *pw3 = VAL__BADD;
               }
            }

/* Smooth the squared residual differences with a median filter. */
            smf_median_smooth( dcsmooth2, SMF__FILT_MEDIAN, 0.0, ntslice,
                               w3, NULL, 1, 0, w4, mw1, mw2, mw3, status );

/* Find the start and end of each block of contiguous high differences.
   Each is a candidate step. */
            nbstep = 0;
            step_start = -1;
            step_end = 0;
            step_limit = -1;
            total = 0.0;
            max_snr_jump = 0.0;
            diff2 = 0.0;

            pw2 = w2;
            pw3 = w3;
            pw4 = w4;
            for( itime = 0; itime < ntslice; itime++,pw2++,pw3++,pw4++ ) {

               diff = *pw2;

/* Median smoothed data values can be identical over a range of samples,
   leading to zero noise. If there is zero noise, use the most recent
   non-zero estimate of the noise. */
               if( *pw4 > 0.0 ) diff2 = *pw4;

               if( diff != VAL__BADD && diff2 != VAL__BADD &&
                   diff2 > 0.0 ) {

/* Get the RMS of the residual differences around the current sample, and
   store the signal to noise ratio for the step in w3. */
                  rms = sqrt( diff2 );
                  *pw3 = diff/rms;

#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      timedata[ itime ].snr = diff/rms;
   }
#endif

/* If the residual difference at the current sample is large compared to
   the RMS, it is considered to be a jump. */
                  if( fabs( diff ) > dcthresh*rms ) {

/* A jump may extend over more than 1 sample, so we group localised clumps
   of high differences together. If we are not currently within such a
   group, record the current time index as the start of a new group. */
                     if( step_start == -1 ) {
                        step_start = itime;
                        total = 0.0;
                        max_snr_jump = 0.0;
                     }

/* The group extends at least as far as the current sample. */
                     step_end = itime;

/* Do not consider the group to be finished until we have found "dcfill"
   small differences following the last large difference. */
                     step_limit = itime + dcfill;

/* Find the maximum SNR jump. */
                     snr_jump = diff/rms;
                     if( fabs( snr_jump ) > max_snr_jump ) {
                        max_snr_jump = fabs( snr_jump );
                     }

/* Increment the total jump in value over the group. */
                     total += snr_jump;

/* If the current residual difference is small, and we have had "dcfill"
   small residuals since the end of the last group, we now know where the
   last group ended so we can go on to process the group. */
                  } else if( itime == step_limit ) {

/* Check the group is not too wide, and that the total jump in value is not
   too small. */
                     step_width = step_end - step_start + 1;

#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      for( jtime = step_start; jtime <= step_end; jtime++ ) {
         timedata[ jtime ].step_width = step_width;
         timedata[ jtime ].total = total;
      }
   }
#endif

                     if( step_width <= dcmaxwidth && fabs( total ) > dcthresh
                         && fabs( total ) > 0.5*max_snr_jump ) {

/* Create a new structure to describe the group, and add it to the array
   of such groups. */
                        ibstep = nbstep++;
                        bsteps = astGrow( bsteps, nbstep, sizeof( *bsteps ) );
                        if( *status == SAI__OK ) {
                           bsteps[ ibstep ].start = step_start;
                           bsteps[ ibstep ].end = step_end;
                           bsteps[ ibstep ].minjump = dcthresh3*bolonoise[ ibolo ];


#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      for( jtime = step_start; jtime <= step_end; jtime++ ) {
         timedata[ jtime ].instep = 1;
      }
   }
#endif


                        }
                     }

/* We can now start looking for a new step. */
                     step_start = -1;
                  }

#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      timedata[ itime ].rms = rms;
   }
#endif

               } else {
                  *pw3 = VAL__BADD;
               }
            }

/* We now have the starting and ending times for all the candidate jumps
   in the current bolometer. Attempt to measure each candidate step, and
   correct the bolometer data for each succesfully measured step. */
            mbstep = smf1_correct_steps( ntslice, dat + base, qua + base,
                                         tstride, w1, w3, dcfitbox, dcthresh2,
                                         nbstep, bsteps, ibolo, meanshift,
                                         data->hdr->steptime,
                                         (pdata->nstep)?&steps:NULL,
                                         (pdata->nstep)?&nstep:NULL,
                                         w4, w5, bcount, status );

#ifdef DEBUG_STEPS
   if( RECORD_BOLO2 ) {
      for( ibstep = 0; ibstep < nbstep; ibstep++ ) {
         fprintf( fd3, "%d ", ibstep);
         fprintf( fd3, "%d ", bsteps[ibstep].start );
         fprintf( fd3, "%d ", bsteps[ibstep].end );
         TOPCAT( fd3, bsteps[ibstep].minjump );
         TOPCAT( fd3, bsteps[ibstep].error );
         TOPCAT( fd3, bsteps[ibstep].jump );
         fprintf( fd3, "%d ", bsteps[ibstep].ok );
         fprintf( fd3, "%d ", bsteps[ibstep].ibolo );
         TOPCAT( fd3, bsteps[ibstep].jump );
         fprintf( fd3, "%d ", bsteps[ibstep].peak );
         TOPCAT( fd3, bsteps[ibstep].peak1 );
         TOPCAT( fd3, bsteps[ibstep].peak2 );
         fprintf( fd3, "%d ", bsteps[ibstep].peakwidth );
         TOPCAT( fd3, bsteps[ibstep].vlo );
         TOPCAT( fd3, bsteps[ibstep].vlo_mean );
         TOPCAT( fd3, bsteps[ibstep].vlo_sigma );
         TOPCAT( fd3, bsteps[ibstep].vhi );
         TOPCAT( fd3, bsteps[ibstep].vhi_mean );
         TOPCAT( fd3, bsteps[ibstep].vhi_sigma );
         fprintf( fd3, "\n" );
      }
   }
#endif


/* Reject the whole bolometer if too many steps were fixed. */
            maxsteps = dcmaxsteps*nsum*data->hdr->steptime/60.0;
            if( maxsteps < 4 ) maxsteps = 4;

            if( dcmaxsteps > 0 && mbstep > maxsteps ) {
               pq = qua + base;
               for( itime = 0; itime < ntslice; itime++ ) {
                  *pq |= ( SMF__Q_BADB | SMF__Q_JUMP );
                  pq += tstride;
               }

               msgOutiff( MSG__DEBUG, " ", "smf_fix_steps: "
                          "flagging bad bolo %" DIM_T_FMT,
                          status, ibolo );

               nrej++;
            }

/* Increment the total number of steps that have been fixed. */
            nfixed += mbstep;

#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      pd = dat + base;
      pq = qua + base;

      for( itime = 0; itime < ntslice; itime++ ) {
         timedata[ itime ].outdata = *pd;
         timedata[ itime ].outquality = (int) *pq;
         pd += tstride;
         pq += tstride;

         fprintf( fd2, "%d ", itime);
         TOPCAT( fd2, timedata[itime].indata );
         fprintf( fd2, "%d ", timedata[itime].inquality);
         TOPCAT( fd2, timedata[itime].outdata );
         fprintf( fd2, "%d ", timedata[itime].outquality);
         fprintf( fd2, "%d ", timedata[itime].ibolo);
         TOPCAT( fd2, timedata[ itime ].meanshift );
         TOPCAT( fd2, timedata[ itime ].median );
         TOPCAT( fd2, timedata[ itime ].diff );
         TOPCAT( fd2, timedata[ itime ].thresh );
         TOPCAT( fd2, timedata[ itime ].sdiff );
         TOPCAT( fd2, timedata[ itime ].rdiff );
         TOPCAT( fd2, timedata[ itime ].mdiff );
         TOPCAT( fd2, timedata[ itime ].mdiff2 );
         TOPCAT( fd2, timedata[ itime ].diff2 );
         TOPCAT( fd2, timedata[ itime ].rms );
         fprintf( fd2, "%d ", timedata[itime].instep );
         fprintf( fd2, "%d ", timedata[itime].step_width );
         TOPCAT( fd2, timedata[ itime ].total );
         TOPCAT( fd2, timedata[ itime ].snr );
         fprintf( fd2, "\n" );
      }
   }
#endif


         }
      }

/* Free workspace */
      w1 = astFree( w1 );
      w2 = astFree( w2 );
      w3 = astFree( w3 );
      w4 = astFree( w4 );
      w5 = astFree( w5 );
      bsteps = astFree( bsteps );
      mw1 = astFree( mw1 );
      mw2 = astFree( mw2 );
      mw3 = astFree( mw3 );
      if( tstride > 1 && meanshift ) wq = astFree( wq );

/* Report the time taken in this thread. */
      msgOutiff( SMF__TIMER_MSG, "",
                 "smfFixSteps: thread finishing bolos %zu -- %zu (%.3f sec)",
                 status, b1, b2, smf_timerupdate( &tv1, &tv2, status ) );


#ifdef DEBUG_STEPS
   fclose( fd2 );
   fclose( fd3 );
#endif


   }

/* Return values. */
   pdata->nrej = nrej;
   pdata->steps = steps;
   pdata->nstep = nstep;
   pdata->nfixed = nfixed;

}




static dim_t smf1_correct_steps( dim_t ntslice, double *dat, smf_qual_t *qua,
                               dim_t tstride, double *median, double *snr,
                               int dcfitbox, double dcthresh2, dim_t nbstep,
                               Step *bsteps, dim_t ibolo, int meanshift,
                               double steptime, smfStepFix **steps,
                               dim_t *nsteps, double *grad, double *off,
                               dim_t *bcount, int *status ){
/*
*  Name:
*     smf1_correct_steps

*  Purpose:
*     Measure each candidate step for a single bolometer, and correct the
*     bolometer data.

*  Invocation:
*     dim_t smf1_correct_steps( dim_t ntslice, double *dat, smf_qual_t *qua,
*                             dim_t tstride, double *median, double *snr,
*                             int dcfitbox, double dcthresh2, dim_t nbstep,
*                             Step *bsteps, dim_t ibolo, int meanshift,
*                             double steptime, smfStepFix **steps,
*                             dim_t *nsteps, double *grad, double *off,
*                             dim_t *bcount, int *status )

*  Arguments:
*     ntslice = dim_t (Given)
*        The number of bolometer samples.
*     dat = double * (Given and Returned)
*        The bolometer data. On exit, it is corrected to remove each
*        succesfully measured jump. A constant offset is added to all
*        samples in order to retain the original mean value.
*     qua = smf_qual_t * (Given and Returned)
*        The bolometer quality. On exit, each step within a succesfully
*        measured jump is flagged with SMF__Q_JUMP.
*     tstride = dim_t (Given)
*        The the number of elements in "dat" between adjacent bolometer
*        samples.
*     median = double * (Given)
*        The median filtered bolometer data. Used to determine the height
*        of each jump.
*     snr = double * (Given)
*        The ratio of the residual difference to the local RMS at every
*        sample. May be NULL.
*     dcfitbox = int (Given)
*        Length of box (in samples) over which each linear fit is
*        performed. Two fits are performed - one just below the step and
*        one just above. These are used to determine the height of the jump.
*     dcthresh2 = double (Given)
*        N-sigma threshold for acceptable jumps. A jump must be more than
*        dcthresh2 times the larger of the RMS deviations in the two
*        linear fits.
*     nbstep = dim_t (Given)
*        The number of candidate steps to be measured.
*     bsteps = Step * (given)
*        An array of structures describing each candidate step.
*     ibolo = dim_t (Given)
*        The index of the bolometer being fixed.
*     steptime = double (Given)
*        The time between samples, in seconds.
*     meanshift = int (Given)
*        If non-zero, smooth each bolometer times stream using a mean-shift
*        filter before doing anything else. A mean-shift filter is an
*        edge-preserving smooth. It can help to identify smaller steps,
*        but does not work well if there are strong gradients in the
*        bolometer time stream. Therefore, "meanshift" should only be
*        used once the common-mode signal has been subtracted. The
*        spatial width of the filter is given by dcsmooth, and the range of
*        data values accepted by the filter is 5 times the local RMS in
*        the original time stream.
*     steps = smfStepFix ** (Given and Returned)
*        An address at which to store a pointer to an array holding a
*        description of each sucessfully fixed step. The supplied array
*        (if any) is extended on exit to hold descriptions of the steps
*        fixed by the current invocation of this function.
*     nsteps = dim_t * (Given and Returned)
*        On entry, the number of elements in the supplied "steps" array.
*        On exit, the number of elements in the returned "steps" array.
*     grad = double * (Given and Returned)
*        Pointer to a work array with at least 3*dcfitbox elements.
*     off = double * (Given and Returned)
*        Pointer to a work array with at least 3*dcfitbox elements.
*     bcount = dim_t * (Given and Returned)
*        Pointer to a work array with one element for each time slice.
*        Each element holds the number of bolometers found to be within a
*        step at the corresponding time slice. May be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Each candidate jump described by the "steps" array is measured
*     within the supplied median filtered bolometer data, and if the jump
*     is significant compared to the variation in the nearby data, then
*     the supplied bolometer data is corrected to remove the jump. The
*     supplied quality array is also flagged with SMF__Q_JUMP for each
*     sample within the step.
*
*     The height of each jump is found by fitting two straight lines to
*     the median smoothed data; one just before the step and one just
*     after the step. Each of these two fits are used to estimate the
*     expected data value at the centre of the step, and the jump height
*     is then equal to the difference between these two estimates.
*
*     This jump height is compared to the RMS deviation of the data values
*     within each of the two fits. To be accepted, the jump must be more than
*     "dcthresh2" times the larger of the two RMS deviations.
*
*     Finally, a constant offset is added to the corrected bolometer data
*     in order to retain the original mean data value.

*/


/* Local Variables: */
   Step *step;
   dim_t at_peak;
   dim_t count;
   dim_t ibstep;
   dim_t itime;
   dim_t jhi;
   dim_t jhilim;
   dim_t jlo;
   dim_t jlolim;
   dim_t jmax;
   dim_t jmin;
   dim_t jtime;
   dim_t ncorr;
   dim_t nsum;
   dim_t peakwidth;
   dim_t result;
   dim_t step_centre;
   dim_t step_end;
   dim_t step_start;
   double *pd2;
   double *pd;
   double *pg;
   double *po;
   double *psnr;
   double corr;
   double dbig;
   double dmax;
   double dminhi;
   double dminlo;
   double dsmall;
   double err0;
   double error;
   double jump;
   double mean;
   double scorr;
   double snrv;
   double sum1;
   double sum2;
   double v;
   double vhi;
   double vhi_mean;
   double vhi_var;
   double vlo;
   double vlo_mean;
   double vlo_var;
   smf_qual_t *pq2;
   smf_qual_t *pq;

#ifdef DEBUG_STEPS
   double peak1;
   double peak2;
#endif

/* Provate configuration parameters */
   double dcnbox = 3.5;
   dim_t dcnlow = 5;
   double dcsiglow = 8.0;
   dim_t dcpeakoff = 20;
   dim_t dcpeakwidth = 150;
   double dcpeakthresh1 = meanshift ? 10.0 : 1.0;
   double dcpeakthresh2 = 2.5;
   dim_t dcpeakminwidth = 5;

/* Initialise result prior to checking status. */
   result = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Change private configuration parameters for very slow scan speeds. */
   if( 1.0/steptime < 20.0 ) {
      dcnlow = 3;
      dcpeakoff = 5;
      dcpeakwidth = 15;
      dcpeakminwidth = 2;
      dcnbox = 1.5;
   }

/* Initialise other things. */
   pd = dat;
   pq = qua;
   itime = 0;
   corr = 0.0;
   scorr = 0.0;
   ncorr = 0;

/* Loop round each candidate step. */
   step = bsteps;
   for( ibstep = 0; ibstep  < nbstep; ibstep++,step++ ) {

/* Note the step centre for later use. */
      step_centre = ( step->start + step->end )/2;

/* Note the bounds of the section of the time stream between the previous
   and next steps. */
      if( ibstep > 0 ) {
         jlolim = bsteps[ ibstep - 1 ].end + dcpeakminwidth;
         if( jlolim > step->start - 2*dcfitbox ) {
            jlolim = step->start - 2*dcfitbox;
            if( jlolim < 0 ) jlolim = 0;
         }
      } else {
         jlolim = 0;
      }

      if( ibstep < nbstep - 1 ) {
         jhilim = bsteps[ ibstep + 1 ].start - dcpeakminwidth;
         if( jhilim < step->end + 2*dcfitbox ) {
            jhilim = step->end + 2*dcfitbox;
            if( jhilim >= ntslice ) jhilim = ntslice - 1;
         }
      } else {
         jhilim =  ntslice - 1;
      }

/* The supplied start and end times are the times at which the SNR peak
   exceeds "dcthresh". If an snr array was supplied, we now extend these
   to include lower SNR values down to "dcsiglow". First work downwards
   from the suppliedstep start until we have found "dcnlow" consecutive
   samples that are below "dcsiglow" sigma. */
      if( snr ) {
         psnr = snr + step->start;
         jtime = step->start;
         count = 0;
         while( --jtime >= jlolim ){
            if( ( snrv = *(--psnr) ) != VAL__BADD ) {
               if( fabs( snrv ) < dcsiglow ) {
                  if( ++count == dcnlow ) break;
               } else {
                  count = 0;
               }
            }
         }

         step_start = jtime + dcnlow;

/* Now work upwards from the supplied step step end until we have found
   "dcnlow" consecutive samples that are below "dcsiglow" sigma. */
         psnr = snr + step->end;
         jtime = step->end;
         count = 0;
         while( ++jtime <= jhilim ){
            if( ( snrv = *(++psnr) ) != VAL__BADD ) {
               if( fabs( snrv ) < dcsiglow ) {
                  if( ++count == dcnlow ) break;
               } else {
                  count = 0;
               }
            }
         }

         step_end = jtime - dcnlow;

/* If no snr array was supplied, use the supplied step start and end
   times without change. */
      } else {
         step_start = step->start;
         step_end = step->end;
      }

/* The edges of a bright source can appear as a step. So we ignore steps
   that occur close to bright sources. First, find the maximum bolometer
   value within a range of dcpeakoff on either side of the step. */
      jlo = step_start - dcpeakoff;
      if( jlo < jlolim  ) jlo = jlolim;
      jhi = step_end + dcpeakoff;
      if( jhi > jhilim ) jhi = jhilim;

      dmax = VAL__MIND;
      jmax = -1;

      pd2 = dat + jlo*tstride;
      pq2 = qua + jlo*tstride;
      for( jtime = jlo; jtime <= jhi; jtime++ ) {
         if( !(*pq2 & SMF__Q_GOOD ) && *pd2 != VAL__BADD ) {
            if( *pd2 > dmax ) {
               dmax = *pd2;
               jmax = jtime;
            }
         }
         pd2 += tstride;
         pq2 += tstride;
      }

/* Find the mean of the 3 samples centred on the maximum value. */
      jlo = jmax - 1;
      jhi = jmax + 1;
      if( jlo < jlolim ) jlo = jlolim;
      if( jhi > jhilim ) jhi = jhilim;

      sum1 = 0.0;
      nsum = 0;
      pd2 = dat + jlo*tstride;
      pq2 = qua + jlo*tstride;
      for( jtime = jlo; jtime <= jhi; jtime++ ) {
         if( !(*pq2 & SMF__Q_GOOD ) && *pd2 != VAL__BADD ) {
            sum1 += *pd2;
            nsum++;
         }
         pd2 += tstride;
         pq2 += tstride;
      }
      dmax = sum1/nsum;

/* Find the smallest bolometer value in a small box just before the peak. */
      jlo = jmax - dcpeakwidth/2;
      if( jlo < jlolim ) jlo = jlolim;

      jmin = jmax;
      dminlo = dmax;
      pd2 = dat + jmax*tstride;
      pq2 = qua + jmax*tstride;
      for( jtime = jmax; jtime >= jlo; jtime-- ) {
         if( !(*pq2 & SMF__Q_GOOD ) && *pd2 != VAL__BADD ) {
            if( *pd2 < dminlo ) {
               dminlo = *pd2;
               jmin = jtime;
            }
         }
         pd2 -= tstride;
         pq2 -= tstride;
      }

/* Check the minimum is not too close to the peak. */
      at_peak = 0;
      if( jmin < jmax - dcpeakminwidth ) {

/* Find the mean of the 5 samples centred on the smallest value before
   the peak. */
         jlo = jmin - 2;
         jhi = jmin + 2;
         if( jlo < jlolim ) jlo = jlolim;
         if( jhi > jhilim ) jhi = jhilim;

         sum1 = 0.0;
         nsum = 0;
         pd2 = dat + jlo*tstride;
         pq2 = qua + jlo*tstride;
         for( jtime = jlo; jtime <= jhi; jtime++ ) {
            if( !(*pq2 & SMF__Q_GOOD ) && *pd2 != VAL__BADD ) {
               sum1 += *pd2;
               nsum++;
            }
            pd2 += tstride;
            pq2 += tstride;
         }
         dminlo = sum1/nsum;

/* Find the smallest bolometer value in a small box just after the peak. */
         jhi = jmax + dcpeakwidth/2;
         if( jhi > jhilim ) jhi = jhilim;

         jmin = jmax;
         dminhi = dmax;
         pd2 = dat + jmax*tstride;
         pq2 = qua + jmax*tstride;
         for( jtime = jmax; jtime <= jhi; jtime++ ) {
            if( !(*pq2 & SMF__Q_GOOD ) && *pd2 != VAL__BADD ) {
               if( *pd2 < dminhi ) {
                  dminhi = *pd2;
                  jmin = jtime;
               }
            }
            pd2 += tstride;
            pq2 += tstride;
         }

/* Check the minimum is not too close to the peak. */
         if( jmin > jmax + dcpeakminwidth ) {

/* Find the mean of the 5 samples centred on the smallest value after
   the peak. */
            jlo = jmin - 2;
            jhi = jmin + 2;
            if( jlo < jlolim ) jlo = jlolim;
            if( jhi > jhilim ) jhi = jhilim;

            sum1 = 0.0;
            nsum = 0;
            pd2 = dat + jlo*tstride;
            pq2 = qua + jlo*tstride;
            for( jtime = jlo; jtime <= jhi; jtime++ ) {
               if( !(*pq2 & SMF__Q_GOOD ) && *pd2 != VAL__BADD ) {
                  sum1 += *pd2;
                  nsum++;
               }
               pd2 += tstride;
               pq2 += tstride;
            }
            dminhi = sum1/nsum;

/* Find the rough width (i.e. ignoring the noise) of the peak at the
   higher of these two values found above. */
            peakwidth = 2*dcpeakminwidth;
            pd2 = dat + jmax*tstride;
            pq2 = qua + jmax*tstride;

            if( dminlo < dminhi && dminlo < dmax) {
               jhi = jmax + dcpeakwidth/2;
               if( jhi > jhilim ) jhi = jhilim;
               for( jtime = jmax; jtime <= jhi; jtime++ ) {
                  if( !(*pq2 & SMF__Q_GOOD ) && *pd2 != VAL__BADD ) {
                     if( *pd2 <= dminhi ) {
                        peakwidth = 2*( jtime - jmax );
                        break;
                     }
                  }
                  pd2 += tstride;
                  pq2 += tstride;
               }

            } else if( dminhi < dminlo && dminhi < dmax) {
               jlo = jmax - dcpeakwidth/2;
               if( jlo < jlolim ) jlo = jlolim;
               for( jtime = jmax; jtime >= jlo; jtime-- ) {
                  if( !(*pq2 & SMF__Q_GOOD ) && *pd2 != VAL__BADD ) {
                     if( *pd2 <= dminlo ) {
                        peakwidth = 2*( jmax - jtime );
                        break;
                     }
                  }
                  pd2 -= tstride;
                  pq2 -= tstride;
               }
            }


/* The step is close to a peak, and should thus be left uncorrected, if:

   1) dmax, dminlo and dminhi were all found
   2) dmax is greater than both dminlo and dminhi
   3) the smaller of (dmax-dminlo) and (dmax-dminhi) is more than a
      specified multiple of the noise in the bolometer.
   4) the larger of (dmax-dminlo) and (dmax-dminhi) is no more than a
      specified multiple of the smaller of (dmax-dminlo) and
      (dmax-dminhi).
   5) the peak width is not tiny (i.e. a spike)
*/

#ifdef DEBUG_STEPS
            peak1 = peak2 = VAL__BADD;
#endif
            at_peak = 1;
            if( dmax != VAL__MIND && dminlo != VAL__MIND && dminhi != VAL__MIND ){
               at_peak = 0;
               if( dmax > dminlo && dmax > dminhi ){

                  if( dminlo > dminhi ) {
                     dsmall =  dmax - dminlo;
                     dbig =  dmax - dminhi;
                  } else {
                     dsmall =  dmax - dminhi;
                     dbig =  dmax - dminlo;
                  }

                  if( dsmall > dcpeakthresh1*step->minjump &&
                      dbig < dcpeakthresh2*dsmall &&
                      peakwidth > dcpeakminwidth ) at_peak = 1;

#ifdef DEBUG_STEPS
                  peak1 = dsmall/step->minjump;
                  peak2 = dbig/dsmall;
#endif
               }
            }
         }
      }

#ifdef DEBUG_STEPS
   if( RECORD_BOLO2 ) {
      step->peak = at_peak;
      step->error = VAL__BADD;
      step->jump = VAL__BADD;
      step->ok = 0;
      step->ibolo = ibolo;
      step->peak1 = peak1;
      step->peak2 = peak2;
      step->peakwidth = peakwidth;
      step->vlo = VAL__BADD;
      step->vlo_mean = VAL__BADD;
      step->vlo_sigma = VAL__BADD;
      step->vhi = VAL__BADD;
      step->vhi_mean = VAL__BADD;
      step->vhi_sigma = VAL__BADD;
   }
#endif

      vlo = 0.0;
      if( !at_peak ) {

/* Perform linear least squares fits to the median-smoothed data for a
   range of adjacent samples prior to the start of the step found above. */
         jhi = step_start - dcfitbox;
         jlo = jhi - dcnbox*dcfitbox;
         if( jlo < jlolim ) jlo = jlolim;
         jlo += 0.5*dcfitbox;
         if( jlo < jhi - 2 ) {
            smf_rolling_fit( dcfitbox, 0.1, ntslice, jlo, jhi, median, grad,
                             off, NULL, status );

/* Use each of the fits created above to estimate the data value at the
   centre of the jump, and find the the mean and variance of these
   estimates. */
            sum1 = 0.0;
            sum2 = 0.0;
            nsum = 0;

            pg = grad;
            po = off;
            for( jtime = jlo; jtime <= jhi; jtime++,po++,pg++ ) {
               if( *pg != VAL__BADD && *po != VAL__BADD ) {
                  v = ( (int) step_centre - jtime )*( *pg ) + ( *po );
                  sum1 += v;
                  sum2 += v*v;
                  nsum++;
                  vlo = v;
               }
            }

            if( nsum > 0 ) {
               vlo_mean = sum1/nsum;
               vlo_var = sum2/nsum - vlo_mean*vlo_mean;
               err0 = vlo - vlo_mean;
               vlo_var += err0*err0;
            } else {
               vlo_mean = VAL__BADD;
               vlo_var = VAL__BADD;
            }

         } else {
            vlo_mean = VAL__BADD;
            vlo_var = VAL__BADD;
         }

/* Perform linear least squares fits to the median-smoothed data for a
   range of adjacent samples after to the step. */
         jlo = step_end + dcfitbox;
         jhi = jlo + dcnbox*dcfitbox;
         if( jhi > jhilim ) jhi = jhilim;
         jhi -= 0.5*dcfitbox;

         if( jlo < jhi - 2 ) {
            smf_rolling_fit( dcfitbox, 0.1, ntslice, jlo, jhi, median, grad,
                             off, NULL, status );

/* Use each of the fits created above to estimate the data value at the
   centre of the jump, and find the the mean and variance of these
   estimates. */
            sum1 = 0.0;
            sum2 = 0.0;
            nsum = 0;
            vhi = VAL__BADD;

            pg = grad;
            po = off;
            for( jtime = jlo; jtime <= jhi; jtime++,po++,pg++ ) {
               if( *pg != VAL__BADD && *po != VAL__BADD ) {
                  v = ( (int) step_centre - jtime )*( *pg ) + ( *po );
                  sum1 += v;
                  sum2 += v*v;
                  nsum++;
                  if( vhi == VAL__BADD ) vhi = v;
               }
            }

            if( nsum > 0 ) {
               vhi_mean = sum1/nsum;
               vhi_var = sum2/nsum - vhi_mean*vhi_mean;
               err0 = vhi - vhi_mean;
               vhi_var += err0*err0;
            } else {
               vhi_mean = VAL__BADD;
               vhi_var = VAL__BADD;
            }

         } else {
            vhi_mean = VAL__BADD;
            vhi_var = VAL__BADD;
         }


#ifdef DEBUG_STEPS
   if( RECORD_BOLO2 ) {
      step->vlo = vlo;
      step->vlo_mean = vlo_mean;
      step->vlo_sigma = ( vlo_var != VAL__BADD ) ? sqrt( vlo_var ) : VAL__BADD;
      step->vhi = vhi;
      step->vhi_mean = vhi_mean;
      step->vhi_sigma = ( vhi_var != VAL__BADD ) ? sqrt( vhi_var ) : VAL__BADD;
   }
#endif

/* Form the total sigma in the jump size. */
         if( vlo_var != VAL__BADD && vhi_var != VAL__BADD ) {
            error = vhi_var + vlo_var;
            error = ( error > 0.0 ) ? sqrt( error ) : 0.0;

/* We prefer the jump estimated from data closer to the step, so get the
   jump height from the closest fit boxes used above. */
            jump = vhi - vlo;

#ifdef DEBUG_STEPS
   if( RECORD_BOLO2 ) {
      step->error = error;
      step->jump = jump;
   }
#endif

/* Check that the jump is large compared to the uncertainty in jump size
   caused by the variation of the gradient in the neighbourhood of the
   jump. Also check the absolute size of the jump is large enough to be
   usable. */
            if( fabs( jump ) >= dcthresh2*fabs( error ) &&
                fabs( jump ) > step->minjump ) {

/* Increment the number of fixed steps. */
               result++;

/* Add the current correction onto all data samples since the centre of
   the previous step, up to the centre of the current step. Also
   increment running sums needed to find the mean correction. */
               for( ; itime < step_centre; itime++ ) {
                  if( !(*pq & SMF__Q_MOD )) {
                     *pd += corr;
                     scorr += corr;
                     ncorr++;
                  }
                  pd += tstride;
                  pq += tstride;
               }

/* Set the correction to use up to the next step. */
               corr -= jump;

/* Flag the samples with high SNR. */
               pq2 = qua + step_start*tstride;
               for( jtime = step_start; jtime <= step_end; jtime++ ) {
                   *pq2 |= SMF__Q_JUMP;
                   pq2 += tstride;
               }

/* Increment the number of bolometers that have a step at each time slice. */
               if( bcount ) {
                  for( jtime = step->start; jtime <= step->end; jtime++ ) {
                     bcount[ jtime ]++;
                  }
               }

/* If required, store details of the step in the returned structure. */
               if( steps ) {
                  *steps = astGrow( *steps, ++(*nsteps), sizeof( **steps ) );
                  if( *status == SAI__OK ) {
                     (*steps)[ *nsteps - 1 ].start = step_start;
                     (*steps)[ *nsteps - 1 ].end = step_end;
                     (*steps)[ *nsteps - 1 ].ibolo = ibolo;
                     (*steps)[ *nsteps - 1 ].size = jump;
                     (*steps)[ *nsteps - 1 ].id = *nsteps - 1;
                     (*steps)[ *nsteps - 1 ].corr = 0;
                  }
               }

#ifdef DEBUG_STEPS
   if( RECORD_BOLO2 ) {
      step->ok = 1;
   }
#endif

            }
         }
      }
   }

/* Apply the final correction up to the last sample. */
   if( corr != 0.0 ) {
      for( ; itime < ntslice; itime++ ) {
         if( !(*pq & SMF__Q_MOD )) {
            *pd += corr;
            scorr += corr;
            ncorr++;
         }
         pd += tstride;
         pq += tstride;
      }
   }

/* Now subtract off the mean correction. */
   if( scorr != 0.0 ) {
      mean = scorr/ncorr;
      pd = dat;
      pq = qua;
      for( itime = 0; itime < ntslice; itime++ ) {
         if( !(*pq & SMF__Q_MOD )) *pd -= mean;
         pd += tstride;
         pq += tstride;
      }
   }

/* Return the result */
   return result;
}

static void smf1_fix_correlated_steps_job( void *job_data, int *status ) {
/*
*  Name:
*     smf1_fix_correlated_steps_job

*  Purpose:
*     Fix correlated steps for a block of bolometers.

*  Invocation:
*     void smf1_fix_correlated_steps_job( void *job_data, int *status )

*  Arguments:
*     job_data = void * (Given)
*        Pointer to the data needed by the job. Should be a pointer to a
*        smfFixStepsJobData structure.
*     status = dim_t * (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine finds and fixes correlated steps in a block of bolometers.
*     It runs within a thread instigated by smf_fix_steps.

*/

/* Local Variables: */
   Step *bsteps = NULL;
   dim_t *bcount;
   dim_t *mw2;
   dim_t *mw3;
   dim_t *pbc;
   dim_t b1;
   dim_t b2;
   dim_t base;
   dim_t bstride;
   dim_t dcfill;
   dim_t dcmaxwidth;
   dim_t ibolo;
   dim_t ibstep;
   dim_t itime;
   dim_t mbstep;
   dim_t msize;
   dim_t nbolo;
   dim_t nbstep;
   dim_t nfixed;
   dim_t nstep;
   dim_t ntslice;
   dim_t old_jump;
   dim_t step_end;
   dim_t step_limit;
   dim_t step_start;
   dim_t step_width;
   dim_t tstride;
   double *bolonoise;
   double *dat = NULL;
   double *mw1;
   double *w1;
   double *w4;
   double *w5;
   double dcthresh2;
   double dcthresh3;
   double rms;
   int dcfitbox;
   int dclimcorr;
   int dcsmooth;
   int meanshift;
   smfData *data;
   smfFixStepsJobData *pdata;
   smfStepFix *steps;
   smf_qual_t *pq;
   smf_qual_t *qua = NULL;
   smf_qual_t *wq = NULL;

   double dcgaincorr = 5.0;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer to the job data, and then extract its contents into a
   set of local variables. */
   pdata = (smfFixStepsJobData *) job_data;

   b1 = pdata->b1;
   b2 = pdata->b2;
   bcount = pdata->bcount;
   bolonoise = pdata->bolonoise;
   bstride = pdata->bstride;
   dat = pdata->dat;
   dcfill = pdata->dcfill;
   dcfitbox = pdata->dcfitbox;
   dclimcorr = pdata->dclimcorr;
   dcmaxwidth = pdata->dcmaxwidth;
   dcsmooth = pdata->dcsmooth;
   dcthresh2 = 0.2*pdata->dcthresh2;
   dcthresh3 = 0.2*pdata->dcthresh3;
   meanshift = pdata->meanshift;
   nbolo = pdata->nbolo;
   ntslice = pdata->ntslice;
   qua = pdata->qua;
   tstride = pdata->tstride;
   data = pdata->data;

/* Initialise the returned values. */
   bsteps = NULL;
   steps = NULL;
   nstep = 0;
   nfixed = 0;

#ifdef DEBUG_STEPS
   FILE *fd2 = fopen( "timedata_c.asc", "w" );
   fprintf( fd2, "# itime indata inquality outdata outquality ibolo meanshift median "
            "diff thresh sdiff rdiff mdiff mdiff2 diff2 rms instep step_width total snr\n");

   FILE *fd3 = fopen( "stepdata_c.asc", "w" );
   fprintf( fd3, "# ibstep start end minjump error jump ok ibolo "
            "jump vlo vlo_mean vlo_sigma vhi vhi_mean vhi_sigma\n" );

   TimeData *timedata = astMalloc( ntslice*sizeof( *timedata ) );
#endif

/* Check we have something to do. */
   if( b1 < nbolo ) {

/* Ensure dcfitbox is odd. */
      dcfitbox = 2*( dcfitbox/2 ) + 1;

/* Relax the criteria for accepting correlated steps, compared to primary
   steps. */
      dcthresh2 /= dcgaincorr;
      dcthresh3 /= dcgaincorr;

/* Allocate work arrays. */
      msize = 3*dcfitbox;
      if( ntslice > msize ) msize = ntslice;
      w1 = astMalloc( sizeof( *w1 )*msize );
      w4 = astMalloc( sizeof( *w4 )*msize );
      w5 = astMalloc( sizeof( *w5 )*msize );
      if( tstride > 1 && meanshift ) wq =  astMalloc( sizeof( *wq )*msize );

      mw1 = astMalloc( sizeof( *mw1 )*dcsmooth );
      mw2 = astMalloc( sizeof( *mw2 )*dcsmooth );
      mw3 = astMalloc( sizeof( *mw3 )*dcsmooth );

/* Loop round all bolometers to be processed by this thread. "base" holds the
   offset to the start of the data for the bolometer.  */
      for( ibolo = b1; ibolo <= b2 && *status==SAI__OK; ibolo++ ) {
         base = ibolo*bstride;
         pq = qua + base;
         if( !(*pq & SMF__Q_BADB) ) {

/* Get a contiguous copy of the quality array. Only needed if doing a
   mean shift filter. */
            if( meanshift ) {
               if( tstride > 1 ) {
                  for( itime = 0; itime < ntslice - 1; itime++ ) {
                     wq[ itime ] = *pq;
                     pq += tstride;
                  }
               } else {
                  wq = pq;
               }
            }



#ifdef DEBUG_STEPS

   if( RECORD_BOLO ) {
      dim_t kk;
      double *pd = dat + base;
      pq = qua + base;
      for( kk = 0; kk < ntslice; kk++ ) {
         timedata[ kk ].indata = *pd;
         timedata[ kk ].inquality = (int) *pq;
         timedata[ kk ].outdata = VAL__BADD;
         timedata[ kk ].outquality = 0;
         timedata[ kk ].ibolo = ibolo;
         timedata[ kk ].meanshift = VAL__BADD;
         timedata[ kk ].median = VAL__BADD;
         timedata[ kk ].diff = VAL__BADD;
         timedata[ kk ].thresh = VAL__BADD;
         timedata[ kk ].sdiff = VAL__BADD;
         timedata[ kk ].rdiff = VAL__BADD;
         timedata[ kk ].mdiff = VAL__BADD;
         timedata[ kk ].mdiff2 = VAL__BADD;
         timedata[ kk ].diff2 = VAL__BADD;
         timedata[ kk ].rms = VAL__BADD;
         timedata[ kk ].instep = 0;
         timedata[ kk ].step_width = 0;
         timedata[ kk ].total = VAL__BADD;
         timedata[ kk ].snr = VAL__BADD;

         pd += tstride;
         pq += tstride;
      };
      pq = qua + base;
   }

#endif

/* Get an estimate of the noise in the bolometer. */
            rms = smf_quick_noise( data, ibolo, 20, 50, SMF__Q_GOOD, status );

/* Smooth the input data with an edge-preserving mean-shift filter, putting
   the results in w5. */
            if( meanshift ) {
               smf_meanshift( dat + base, w5, ntslice, tstride, dcsmooth, 5*rms,
                              qua + base, SMF__Q_GOOD, 0.0, status );

#ifdef DEBUG_STEPS
if( RECORD_BOLO ) {
   printf("Quick RMS is %g\n", rms );
   for( itime = 1; itime < ntslice - 1; itime++ ) {
      timedata[ itime ].meanshift = w5[itime];
   }
}
#endif

/* Median smooth the above data, putting the results in w1. */
               smf_median_smooth( dcsmooth, SMF__FILT_MEDIAN, -1.0, ntslice,
                                  w5, wq, 1, SMF__Q_GOOD, w1, mw1, mw2, mw3,
                                  status );

/* If mean-shift filter is not required, just median filter. */
            } else {
               smf_median_smooth( dcsmooth, SMF__FILT_MEDIAN, -1.0, ntslice,
                                  dat + base, qua + base, tstride, SMF__Q_GOOD,
                                  w1, mw1, mw2, mw3, status );
            }

#ifdef DEBUG_STEPS
if( RECORD_BOLO ) {
   printf("Quick RMS is %g\n", rms );
   for( itime = 1; itime < ntslice - 1; itime++ ) {
      timedata[ itime ].median = w1[itime];
   }
}
#endif


/* Find the start and end of each block of contiguous high bolometer
   counts in the supplied "bcount" array. This array holds the number of
   bolometers found to have a step at each time slice. */
            nbstep = 0;
            step_start = -1;
            step_end = 0;
            step_limit = -1;
            old_jump = 0;

            pbc = bcount;
            pq = qua + base;
            for( itime = 0; itime < ntslice; itime++,pbc++,pq++ ) {

#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      timedata[ itime ].snr = *pbc;
   }
#endif

/* If the bcount value at the current time slice is larger than the
   threshold, it is considered to be in a jump. */
               if( *pbc > dclimcorr ) {

/* A jump may extend over more than 1 sample, so we group localised clumps
   of high bcount together. If we are not currently within such a
   group, record the current time index as the start of a new group. */
                  if( step_start == -1 ) {
                     step_start = itime;
                     old_jump = 0;
                  }

/* Set a flag if a previous jump is encountered within the new step. */
                  if( (*pq & SMF__Q_JUMP ) ) old_jump = 1;

/* The group extends at least as far as the current sample. */
                  step_end = itime;

/* Do not consider the group to be finished until we have found "dcfill"
   small differences following the last large difference. */
                  step_limit = itime + dcfill;

/* If the current bcount value is small, and we have had "dcfill" small
   values since the end of the last group, we now know where the last
   group ended so we can go on to process the group. */
               } else if( itime == step_limit ) {

/* Check the group is not too wide, and that it does not include a
   previously corrected jump. */
                  step_width = step_end - step_start + 1;

#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      dim_t jtime;
      for( jtime = step_start; jtime <= step_end; jtime++ ) {
         timedata[ jtime ].step_width = step_width;
      }
   }
#endif

                  if( step_width <= dcmaxwidth && ! old_jump ){

/* Create a new structure to describe the group, and add it to the array
   of such groups. */
                     ibstep = nbstep++;
                     bsteps = astGrow( bsteps, nbstep, sizeof( *bsteps ) );
                     if( *status == SAI__OK ) {
                        bsteps[ ibstep ].start = step_start;
                        bsteps[ ibstep ].end = step_end;
                        bsteps[ ibstep ].minjump = dcthresh3*bolonoise[ ibolo ];

#ifdef DEBUG_STEPS
   if( RECORD_BOLO ) {
      dim_t jtime;
      for( jtime = step_start; jtime <= step_end; jtime++ ) {
         timedata[ jtime ].instep = 1;
      }
   }
#endif

                     }
                  }

/* We can now start looking for a new step. */
                  step_start = -1;
               }
            }

/* We now have the starting and ending times for all the candidate
   correlated jumps in the current bolometer. Attempt to measure each
   candidate correlated step, and correct the bolometer data for each
   succesfully measured step. */
            mbstep = smf1_correct_steps( ntslice, dat + base, qua + base,
                                         tstride, w1, NULL, dcfitbox,
                                         dcthresh2, nbstep, bsteps, ibolo,
                                         meanshift, data->hdr->steptime,
                                         (pdata->nstep)?&steps:NULL,
                                         (pdata->nstep)?&nstep:NULL,
                                         w4, w5, bcount, status );

#ifdef DEBUG_STEPS
   if( RECORD_BOLO2 ) {
      for( ibstep = 0; ibstep < nbstep; ibstep++ ) {
         fprintf( fd3, "%d ", ibstep);
         fprintf( fd3, "%d ", bsteps[ibstep].start );
         fprintf( fd3, "%d ", bsteps[ibstep].end );
         TOPCAT( fd3, bsteps[ibstep].minjump );
         TOPCAT( fd3, bsteps[ibstep].error );
         TOPCAT( fd3, bsteps[ibstep].jump );
         fprintf( fd3, "%d ", bsteps[ibstep].ok );
         fprintf( fd3, "%d ", bsteps[ibstep].ibolo );
         TOPCAT( fd3, bsteps[ibstep].jump );
         TOPCAT( fd3, bsteps[ibstep].vlo );
         TOPCAT( fd3, bsteps[ibstep].vlo_mean );
         TOPCAT( fd3, bsteps[ibstep].vlo_sigma );
         TOPCAT( fd3, bsteps[ibstep].vhi );
         TOPCAT( fd3, bsteps[ibstep].vhi_mean );
         TOPCAT( fd3, bsteps[ibstep].vhi_sigma );
         fprintf( fd3, "\n" );
      }
   }

   if( RECORD_BOLO ) {
      double *pd = dat + base;
      pq = qua + base;

      for( itime = 0; itime < ntslice; itime++ ) {
         timedata[ itime ].outdata = *pd;
         timedata[ itime ].outquality = (int) *pq;
         pd += tstride;
         pq += tstride;

         fprintf( fd2, "%d ", itime);
         TOPCAT( fd2, timedata[itime].indata );
         fprintf( fd2, "%d ", timedata[itime].inquality);
         TOPCAT( fd2, timedata[itime].outdata );
         fprintf( fd2, "%d ", timedata[itime].outquality);
         fprintf( fd2, "%d ", timedata[itime].ibolo);
         TOPCAT( fd2, timedata[ itime ].meanshift );
         TOPCAT( fd2, timedata[ itime ].median );
         TOPCAT( fd2, timedata[ itime ].diff );
         TOPCAT( fd2, timedata[ itime ].thresh );
         TOPCAT( fd2, timedata[ itime ].sdiff );
         TOPCAT( fd2, timedata[ itime ].rdiff );
         TOPCAT( fd2, timedata[ itime ].mdiff );
         TOPCAT( fd2, timedata[ itime ].mdiff2 );
         TOPCAT( fd2, timedata[ itime ].diff2 );
         TOPCAT( fd2, timedata[ itime ].rms );
         fprintf( fd2, "%d ", timedata[itime].instep );
         fprintf( fd2, "%d ", timedata[itime].step_width );
         TOPCAT( fd2, timedata[ itime ].total );
         TOPCAT( fd2, timedata[ itime ].snr );
         fprintf( fd2, "\n" );
      }
   }
#endif

/* Increment the total number of steps that have been fixed. */
            nfixed += mbstep;
         }
      }

/* Free workspace */
      w1 = astFree( w1 );
      w4 = astFree( w4 );
      w5 = astFree( w5 );
      bsteps = astFree( bsteps );
      mw1 = astFree( mw1 );
      mw2 = astFree( mw2 );
      mw3 = astFree( mw3 );
      if( tstride > 1 && meanshift ) wq = astFree( wq );
   }

#ifdef DEBUG_STEPS
   fclose( fd2 );
   fclose( fd3 );
#endif

/* Return values. */
   pdata->nrej = 0;
   pdata->steps = steps;
   pdata->nstep = nstep;
   pdata->nfixed = nfixed;

}


#ifdef DEBUG_STEPS
static dim_t get_debug_bolo( void ) {
   while( debug_bolo < 0 ) {
      printf("Enter zero-based index of bolometer to record: ");
      scanf( "%d", &debug_bolo );
   }
   return debug_bolo;
}
#endif
