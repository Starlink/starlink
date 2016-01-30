/*
*+
*  Name:
*     smf_calc_smoothedwvm

*  Purpose:
*     Given a WVM time series, calculate a tau for each time slice

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calc_smoothedwvm ( ThrWorkForce * wf, const smfArray * alldata,
*                            const smfData * adata, AstKeyMap* extpars, double **wvmtau,
*                            size_t *nframes, size_t *ngood, int * status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     alldata = const smfArray * (Given)
*        Related smfDatas assumed to cover the same time range. A
*        single WVM tau will be calculated for all input smfDatas.
*        IF supplied, "adata" must be NULL.
*     adata = const smfData * (Given)
*        If alldata is NULL a single smfData can be supplied.
*     extpars = AstKeyMap * (Given)
*        Extinction parameters from the config file. Uses the
*        ext.smoothwvm parameter to control smoothing. A positive
*        value enables smoothing using a tophat of the specified
*        size.
*     wvmtau = double ** (Returned)
*        Pointer to array of double to receive the WVM tau data.
*        Will be malloced by this routine and should be freed
*        by the caller.
*     nframes = size_t * (Returned)
*        Number of elements in "wvmtau" array.
*     ngood = size_t * (Returned)
*        Number of good values in the "wvmtau" array.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     The WVM reads out at a much slower rate than the rest of SCUBA-2.
*     The individual 1 second measurements are also quite noisy and prone
*     to steps which can introduce steps into the bolometer time-series
*     following extinction correction.
*
*     This function calculates a WVM tau from the raw WVM data and
*     telescope information and then returns it to the caller in a buffer.
*     The data are optionally smoothed. Accepts a smfArray or a single
*     smfData.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - WVM_TIME is stored as TAI MJD but the accuracy of the measurement
*     when compared to RTS_END depends on the accuracy of the computer
*     hosting the enviro task.
*     - The WVM_TIME reports the time one half cycle after the data were
*     taken. At the time of writing the readout rate is 1.2 seconds so the
*     WVM_TIME is about 1.2 seconds ahead of the middle of the reported
*     reading.

*  History:
*     2011-04-06 (TIMJ):
*        Initial version
*     2012-01-18 (TIMJ):
*        Multi-threaded implementation.
*     2012-06-29 (TIMJ):
*        Fix critical indexing bugs in multi-threaded implementation
*     2016-01-29 (GSB):
*        Add despiking step.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 Science and Technology Facilities Council.
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

#include "sae_par.h"
#include "mers.h"

#include "smf.h"
#include "smurf_par.h"

#include <math.h>
#include <sys/time.h>

  /* Macro to find a smfData in the smfGroup that contains valid
     data at this time slice */
#define SELECT_DATA( ALLDATA, SMFDATA, BADVAL, ITEM, INDEX )     \
  {                                                              \
    size_t _ii;                                                  \
    SMFDATA = NULL;                                              \
    for (_ii=0; _ii<nrelated; _ii++) {                           \
      smfData *DATA = (ALLDATA->sdata)[_ii];                     \
      smf_tslice_ast( DATA, INDEX, 0, NO_FTS, status );          \
      if ( DATA->hdr->state->ITEM != BADVAL ) {                  \
        SMFDATA = DATA;                                          \
        break;                                                   \
      }                                                          \
    }                                                            \
  }

/* Local data type */
typedef struct {
  double airmass;
  smfArray * thesedata;
  AstKeyMap * extpars;
  double *taudata;
  size_t ngood;
  size_t maxgap;
  dim_t t1;
  dim_t t2;
  dim_t nframes;
} smfCalcWvmJobData;

void smf__print_wvm_data(double* taudata, smfHead *hdr, size_t nframes,
                         int* status);

void smf__calc_wvm_job( void *job_data, int *status );

#define FUNC_NAME "smf_calc_smoothedwvm"

void smf_calc_smoothedwvm ( ThrWorkForce *wf, const smfArray * alldata,
                            const smfData * adata, AstKeyMap* extpars, double **wvmtau,
                            size_t *nelems, size_t *ngoodvals, int * status ) {
  size_t i;
  size_t nrelated = 0;          /* Number of entries in smfArray */
  size_t nframes = 0;           /* Number of timeslices */
  size_t ngood = 0;             /* Number of elements with good tau */
  double *taudata = NULL;       /* Local version of WVM tau */
  const smfArray * thesedata = NULL;  /* Collection of smfDatas to analyse */
  smfArray * tmpthesedata = NULL; /* Local version of adata in a smfArray */

  if (*status != SAI__OK) return;

  if (alldata && adata) {
    *status = SAI__ERROR;
    errRep("", "smf_calc_smoothedwvm can not be given non-NULL alldata and non-NULL adata arguments"
           " (possible programming error)", status );
    return;
  }

  if (!alldata && !adata) {
    *status = SAI__ERROR;
    errRep("", "smf_calc_smoothedwvm: One of alldata or adata must be non-NULL",
           status);
    return;
  }

  if (!wvmtau) {
    *status = SAI__ERROR;
    errRep("", "Must supply a non-NULL pointer for wvmtau argument"
           " (possible programming error)", status );
    return;
  }

  /* if we have a single smfData put it in a smfArray */
  if (alldata) {
    if (alldata->ndat == 0 ) {
      *status = SAI__ERROR;
      errRep("", "No smfDatas present in supplied smfArray for WVM smoothing"
             " (possible programming error)", status );
      return;
    }
    thesedata = alldata;
  } else {
    tmpthesedata = smf_create_smfArray( status );
    if (tmpthesedata) {
      tmpthesedata->owndata = 0; /*not owned by the smfArray */

      /* we know that the smfData here will not be touched in this
         function so we do the BAD thing of casting const to non-const */
      smf_addto_smfArray( tmpthesedata, (smfData *)adata, status );
    }
    thesedata = tmpthesedata;
  }

  /* Check that we have headers and that the smfData are the same length */
  nrelated = thesedata->ndat;

  for (i = 0; i < nrelated; i++ ) {
    smfData * data = (thesedata->sdata)[i];
    smfHead * hdr = data->hdr;
    dim_t thisframes = 0;
    if ( !hdr) {
      *status = SAI__ERROR;
      errRepf( "", "smfData %zu has no header. Aborting WVM smoothing",
               status, i );
      return;
    }

    smf_get_dims( data, NULL, NULL, NULL, &thisframes, NULL, NULL, NULL, status );
    if (!nframes) nframes = thisframes;
    if (thisframes != nframes) {
      *status = SAI__ERROR;
      errRepf( "", "smfData %zu has different length. Aborting WVM smoothing",
               status, i );
      return;
    }
  }

  /* We will need the earliest and last airmass value in order
     to calculate a zenith tau */

  /* As a first step, just fill the time series with calculated WVM
     tau values even though we know there are about 240 fewer tau
     readings in reality. This initial approach will make it easier to
     use the smoothed data directly rather than having to interpolate
     from the 1.2 second data back into the 200 Hz data. */

  taudata = astCalloc( nframes, sizeof(*taudata) );

  if (*status == SAI__OK) {
    double amprev = VAL__BADD;
    double steptime;
    size_t maxgap;
    struct timeval tv1;
    struct timeval tv2;
    smfCalcWvmJobData *job_data = NULL;
    int nworker;

    /* We need to know the steptime so we can define the max good gap
       in seconds and convert it to steps*/

    steptime = (thesedata->sdata)[0]->hdr->steptime;
    maxgap = (size_t)( 5.0 / steptime );  /* 5 seconds is just larger than 2 WVM readings */

    /* Assume all files have the same airmass information */
    smf_find_airmass_interval( (thesedata->sdata)[0]->hdr, &amprev, NULL, NULL, NULL, status );

    smf_timerinit( &tv1, &tv2, status );


    /* Create structures used to pass information to the worker threads. */
    nworker = wf ? wf->nworker : 1;
     job_data = astMalloc( nworker*sizeof( *job_data ) );

    if (*status == SAI__OK) {
      dim_t tstep;
      int iworker;
      smfCalcWvmJobData *pdata = NULL;

      /* Get the number of time slices to process in each thread. */
      if( nworker > (int) nframes ) {
        tstep = 1;
      } else {
        tstep = nframes/nworker;
      }

      /* to return the same values for one thread and multiple threads
         we need to break the threads on wvm sample boundaries wherever
         possible. We make an initial estimate of the number of WVM measurements
         by assuming one every two seconds. */
      {
        smfData * curdata = NULL;
        size_t nwvm = 0;
        double prevtime = VAL__BADD;
        double curtime;
        size_t *boundaries = astGrow(NULL, nframes*(size_t)(steptime/2.0), sizeof(*boundaries));
        for (i=0; i<nframes; i++) {
          if (!curdata) {
            SELECT_DATA( thesedata, curdata, VAL__BADD, wvm_time, i );
          }

          if (curdata) smf_tslice_ast( curdata, i, 0, NO_FTS, status );

          if ( !curdata || curdata->hdr->state->wvm_time == VAL__BADD ) {
            /* Try the other datas */
            SELECT_DATA( thesedata, curdata, VAL__BADD, wvm_time, i );
          }
          if (*status != SAI__OK) break;

          if (!curdata) {
            curtime = VAL__BADD;
          } else {
            curtime = curdata->hdr->state->wvm_time;
          }

          if (curtime != prevtime || nwvm == 0 ) {
            /* Store the index in the boundaries array */
            nwvm++;
            boundaries = astGrow(boundaries, nwvm, sizeof(*boundaries));
            if (!boundaries) { /* this is serious */
              if (*status == SAI__OK) *status = SAI__ERROR;
              errRep("", "Error allocating temporary memory for WVM calculation\n",
                     status );
              break;
            }
            boundaries[nwvm-1] = i;
            prevtime = curtime;
          }
        }

        /* No point using too many threads */
        if (*status == SAI__OK) {
          if (nworker >= (int)nwvm) {
            nworker = nwvm;

            /* Allocate a measurement per thread */
            for( iworker = 0; iworker < nworker; iworker++ ) {
              pdata = job_data + iworker;
              pdata->t1 = boundaries[iworker];
              if (iworker+1 < nworker) pdata->t2 = boundaries[iworker+1]-1;
            }

            /* Ensure that the last thread picks up any left-over time slices */
            pdata->t2 = nframes - 1;

          } else {
            /* Allocate the workers to slices of approximate size tstep */
            size_t prevend = 0; /* End of previous slice */
            size_t prevbnd = 0; /* Index into previous boundaries[] array selection */
            for( iworker = 0; iworker < nworker; iworker++ ) {
              size_t belowidx = prevend+1;
              size_t aboveidx = nframes;
              size_t lbnd;
              size_t ubnd;
              size_t j;
              size_t guess;

              pdata = job_data + iworker;

              if (iworker == 0) { /* always start at the beginning */
                pdata->t1 = 0;
              } else { /* Start one after the previous block */
                pdata->t1 = prevend + 1;
              }

              /* Now we have to find the end of this slice */
              guess = (iworker*tstep) + tstep - 1;
              if (guess <= pdata->t1) guess = pdata->t1 + tstep;

              /* find nearest boundaries */
              for (j=prevbnd; j<nwvm; j++) {
                if ( boundaries[j] > guess ) {
                  aboveidx = boundaries[j];
                  ubnd = j;
                  if (j>0) {
                    belowidx = boundaries[j-1];
                    lbnd = j -1 ;
                  } else {
                    lbnd = 0;
                  }
                  break;
                }
              }

              /* Choose the closest, making sure that we are not choosing t1 */
              if ( (guess - belowidx < aboveidx - guess) && belowidx > pdata->t1 ) {
                pdata->t2 = belowidx - 1;
                prevbnd = lbnd;
              } else {
                pdata->t2 = aboveidx - 1;
                prevbnd = ubnd;
              }

              prevend = pdata->t2;

              if (prevend == nframes - 1 && iworker < nworker-1 ) {
                /* we have run out of slices so just use fewer workers */
                nworker = iworker + 1;
                break;
              }

            }

            /* Ensure that the last thread picks up any left-over time slices */
            pdata->t2 = nframes - 1;

          }

          /* Tidy up */
          boundaries = astFree( boundaries );
        }
      }

      /* Store all the other info needed by the worker threads, and submit the
         jobs to fix the steps in each bolo, and then wait for them to complete. */
      for( iworker = 0; iworker < nworker; iworker++ ) {
        smfArray *thrdata = NULL;
        pdata = job_data + iworker;

        pdata->nframes = nframes;
        pdata->airmass = amprev; /* really need to get it from the start of each chunk */
        pdata->taudata = taudata;
        pdata->maxgap = maxgap;

        /* Need to copy the smfDatas and create a new smfArray for each
           thread */
        thrdata = smf_create_smfArray( status );
        for (i=0;i<nrelated;i++) {
          smfData *tmpdata = NULL;
          tmpdata = smf_deepcopy_smfData( wf, (thesedata->sdata)[i], 0, SMF__NOCREATE_FILE |
                                          SMF__NOCREATE_DA |
                                          SMF__NOCREATE_FTS |
                                          SMF__NOCREATE_DATA |
                                          SMF__NOCREATE_VARIANCE |
                                          SMF__NOCREATE_QUALITY, 0, 0,
                                          status );
          smf_lock_data( tmpdata, 0, status );
          smf_addto_smfArray( thrdata, tmpdata, status );
        }
        pdata->thesedata = thrdata;

        /* Need to do a deep copy of ast data and unlock them */
        pdata->extpars = astCopy(extpars);
        astUnlock( pdata->extpars, 1 );

        /* Pass the job to the workforce for execution. */
        thrAddJob( wf, THR__REPORT_JOB, pdata, smf__calc_wvm_job, 0, NULL,
                   status );
      }

      /* Wait for the workforce to complete all jobs. */
      thrWait( wf, status );

      /* Now free the resources we allocated during job creation
         and calculate the number of good values */
      for( iworker = 0; iworker < nworker; iworker++ ) {
        smfArray * thrdata;
        pdata = job_data + iworker;
        astLock( pdata->extpars, 0 );
        pdata->extpars = astAnnul( pdata->extpars );
        thrdata = pdata->thesedata;
        for (i=0;i<thrdata->ndat;i++) {
          smf_lock_data( (thrdata->sdata)[i], 1, status );
        }
        smf_close_related( wf, &thrdata, status );
        ngood += pdata->ngood;
      }
    }
    job_data = astFree( job_data );

    msgOutiff( SMF__TIMER_MSG, "", FUNC_NAME ": %f s to calculate unsmoothed WVM tau values",
               status, smf_timerupdate(&tv1,&tv2,status) );

  }




  if (*status == SAI__OK && extpars) {
    /* Read extpars to see if we need to despike and/or smooth */
    double despiketime = VAL__BADD;
    double despiketol = VAL__BADD;
    double smoothtime = VAL__BADD;

    if (astMapGet0D(extpars, "DESPIKEWVM", &despiketime)
            && astMapGet0D(extpars, "DESPIKEWVMTOL", &despiketol)) {
        if  ((despiketime != VAL__BADD) && (despiketol != VAL__BADD)) {
            /* Use this to get the raw WVM output for debugging before despiking */
            /*
            smf__print_wvm_data(taudata, (thesedata->sdata)[0]->hdr, nframes, status);
            */

            msgOutiff(MSG__VERB, "",
                      "Despiking WVM data with %f s window and %f tolerance",
                      status, despiketime, despiketol);

            /* Apply despiking routine and subtract the (returned) number
               of removed samples from those considered good. */
            ngood -= smf_despike_wvm(
                taudata, nframes,
                (int) (despiketime / (thesedata->sdata)[0]->hdr->steptime),
                despiketol, status);
        }
    }

    if (astMapGet0D( extpars, "SMOOTHWVM", &smoothtime ) ) {
      if (smoothtime != VAL__BADD && smoothtime > 0.0) {
        /* Use this to get the raw WVM output for debugging before smoothing */
        /*
        smf__print_wvm_data(taudata, (thesedata->sdata)[0]->hdr, nframes, status);
        */

        smfData * data = (thesedata->sdata)[0];
        double steptime = data->hdr->steptime;
        dim_t boxcar = (dim_t)( smoothtime / steptime );

        msgOutiff( MSG__VERB, "",
                   "Smoothing WVM data with %f s tophat function",
                   status, smoothtime );

        smf_tophat1D( taudata, nframes, boxcar, NULL, 0, 0.0, status );

        /* The tophat smoothing puts a bad value at the start and end of
           the time series so we replace that with the adjacent value since
           the step time is much smaller than WVM readout time. If more than
           one value is bad we do not try to find the good value. */
        taudata[0] = taudata[1];
        taudata[nframes-1] = taudata[nframes-2];
      }
    }
  }

  /* Use this to get the raw WVM output for debugging */
  /*
  smf__print_wvm_data(taudata, (thesedata->sdata)[0]->hdr, nframes, status);
  */

  /* Free resources */
  if (tmpthesedata) smf_close_related( wf, &tmpthesedata, status );

  if (*status != SAI__OK) {
    if (taudata) taudata = astFree( taudata );
    *nelems = 0;
    *ngoodvals = 0;
  } else {
    *wvmtau = taudata;
    *nelems = nframes;
    *ngoodvals = ngood;
  }

}

/* Debugging routine to print out the raw WVM data. */
void smf__print_wvm_data(double* taudata, smfHead *hdr, size_t nframes,
                         int* status) {
  size_t i;
  if (*status == SAI__OK) {
    fprintf(stderr, "# IDX TAU RTS_NUM RTS_END WVM_TIME\n");
    for (i=0; i<nframes;i++) {
      JCMTState * state = &(hdr->allState)[i];
      fprintf(stderr, "%zu %.*g %d %.*g %.*g\n",
              i, DBL_DIG, taudata[i], state->rts_num,
              DBL_DIG, state->rts_end, DBL_DIG, state->wvm_time);
    }
    fprintf(stderr, "\n\n");
  }
}

/* Routine called by thread queue to calculate a chunk of WVM data.
   Actual arguments are passed in through job_data struct defined
   above. */

void smf__calc_wvm_job( void *job_data, int *status ) {

  struct timeval tv1;
  struct timeval tv2;
  smfData * curdata = NULL;
  smfArray * thesedata;
  double prevtime = VAL__BADD;
  double prevtau = VAL__BADD;
  double lastgoodtau = VAL__BADD;   /* most recent good tau */
  size_t lastgoodidx = SMF__BADSZT; /* index of most recent good value */
  size_t nbadidx = 0;    /* number of time slices in the current gap */
  size_t maxgap;
  dim_t t1;
  dim_t t2;
  size_t nrelated;

  double * taudata = NULL;
  size_t ngood = 0;
  dim_t i;
  smfCalcWvmJobData *pdata;
  double amprev;
  AstKeyMap * extpars;

  if (*status != SAI__OK) return;

  pdata = (smfCalcWvmJobData *)job_data;
  t1 = pdata->t1;
  t2 = pdata->t2;
  amprev = pdata->airmass;
  thesedata = pdata->thesedata;
  taudata = pdata->taudata;
  nrelated = thesedata->ndat;
  extpars = pdata->extpars;
  maxgap = pdata->maxgap;

  /* Lock the AST pointers to this thread */
  astLock( extpars, 0 );
  for (i=0;i<thesedata->ndat;i++) {
    smf_lock_data( (thesedata->sdata)[i], 1, status );
  }

/* Debugging message indicating thread started work */
  msgOutiff( SMF__TIMER_MSG, "", "smfCalcSmoothedWVM: thread starting on slices %" DIM_T_FMT
             " -- %" DIM_T_FMT,
             status, t1, t2 );
  smf_timerinit( &tv1, &tv2, status);

  for (i=t1; i<=t2; i++) {

    if (!curdata) {
      SELECT_DATA( thesedata, curdata, VAL__BADD, wvm_time, i );
    }

    if (curdata) smf_tslice_ast( curdata, i, 0, NO_FTS, status );

    if ( !curdata || curdata->hdr->state->wvm_time == VAL__BADD ) {
      /* Try the other datas */
      SELECT_DATA( thesedata, curdata, VAL__BADD, wvm_time, i );
    }
    if (*status != SAI__OK) break;

    /* if we have no good data we store a bad value */
    if (!curdata) {
      prevtau = VAL__BADD;
    } else {
      const JCMTState * state = NULL;
      state = curdata->hdr->state;

      /* if we have old values from the WVM or no value we don't trust them */
      if ( state->wvm_time != VAL__BADD &&
           (fabs(state->wvm_time - state->rts_end) * SPD) < 60.0 ) {
        /* Only calculate a tau when we have new values */
        if ( prevtime != state->wvm_time ) {
          double thistau = VAL__BADD;
          double airmass = VAL__BADD;

          prevtime = state->wvm_time;

          airmass = state->tcs_airmass;
          if (airmass == VAL__BADD) {
            airmass = amprev;
          } else {
            amprev = airmass;
          }

          thistau = smf_calc_wvm( curdata->hdr, airmass, extpars, status );

          /* Check status and/or value of tau */
          if ( thistau == VAL__BADD ) {
            if ( *status == SAI__OK ) {
              *status = SAI__ERROR;
              errRepf("", "Error calculating tau from WVM temperatures at time slice %" DIM_T_FMT,
                      status, i);
            }
          } else if ( thistau < 0.0 ) {
            msgOutiff( MSG__QUIET, "", "WARNING: Negative WVM tau calculated (%g). Ignoring.",
                       status, thistau );
            prevtau = VAL__BADD;
          } else {
            prevtau = thistau;
          }
        } else {
          /* We use the previous tau since we should have calculated it earlier */
        }
      } else {
        /* No good reading so tau is bad */
        prevtau = VAL__BADD;
      }
    }

    /* Prevtau is the tau that should be assigned to the current position */

    /* see about gaps */
    if (prevtau == VAL__BADD) {
      nbadidx++;
    } else {
      /* we have a good value so we now have to see if there is a gap to fill */
      if (i > 0 && lastgoodidx != (i-1) ) {
        /* the previous value was bad so we may have to patch up if small */
        if ( nbadidx < maxgap ) {
          size_t j;
          if (lastgoodidx == SMF__BADSZT) {
            /* gap is at the start so fill with current value */
            for (j=t1; j<i;j++) {
              taudata[j] = prevtau;
              ngood++;
            }
          } else {
            /* replace with mean value */
            double meantau = (lastgoodtau + prevtau) / 2.0;
            for (j=lastgoodidx+1; j<i; j++) {
              taudata[j] = meantau;
              ngood++;
            }
          }
        }
      }

      /* we know this index was good */
      lastgoodidx = i;
      lastgoodtau = prevtau;
      nbadidx = 0;
      ngood++;

    }

    /* Store the current tau value */
    taudata[i] = prevtau;

  }

  /* if the last value in the time series was bad we need to see about
     filling with the last good value */
  if (*status == SAI__OK && nbadidx > 0 && nbadidx < maxgap) {
    for (i=lastgoodidx+1; i<=t2; i++) {
      taudata[i] = lastgoodtau;
      ngood++;
    }
  }

/* Report the time taken in this thread. */
  msgOutiff( SMF__TIMER_MSG, "",
             "smfCalcSmoothedWVM: thread finishing slices %" DIM_T_FMT
             " -- %" DIM_T_FMT " (%zu good) (%.3f sec)",
             status, t1, t2, ngood, smf_timerupdate( &tv1, &tv2, status ) );

  /* Store number of good values */
  pdata->ngood = ngood;

  /* Unlock the AST pointers from this thread */
  astUnlock( extpars, 1 );
  for (i=0;i<thesedata->ndat;i++) {
    smf_lock_data( (thesedata->sdata)[i], 0, status );
  }

}
