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
*     smf_calc_smoothedwvm ( const smfArray * alldata, const smfData * adata,
*                            AstKeyMap* extpars, double **wvmtau,
*                            size_t *nframes, size_t *ngood, int * status );

*  Arguments:
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "sae_par.h"
#include "mers.h"

#include "smf.h"
#include "smurf_par.h"

#include <math.h>

void smf_calc_smoothedwvm ( const smfArray * alldata, const smfData * adata,
                            AstKeyMap* extpars, double **wvmtau,
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

  taudata = astCalloc( nframes, sizeof(*taudata), 1 );

  /* Macro to find a smfData in the smfGroup that contains valid
     data at this time slice */
#define SELECT_DATA( ALLDATA, SMFDATA, BADVAL, ITEM, INDEX )     \
  {                                                              \
    size_t _ii;                                                  \
    SMFDATA = NULL;                                              \
    for (_ii=0; _ii<nrelated; _ii++) {                           \
      smfData *DATA = (ALLDATA->sdata)[_ii];                     \
      smf_tslice_ast( DATA, INDEX, 0, status );                  \
      if ( DATA->hdr->state->ITEM != BADVAL ) {                  \
        SMFDATA = DATA;                                          \
        break;                                                   \
      }                                                          \
    }                                                            \
  }

  if (*status == SAI__OK) {
    smfData *curdata = NULL;
    double prevtime = VAL__BADD;
    double prevtau = VAL__BADD;
    double amprev = VAL__BADD;
    double lastgoodtau = VAL__BADD;   /* most recent good tau */
    size_t lastgoodidx = SMF__BADSZT; /* index of most recent good value */
    size_t nbadidx = 0;    /* number of time slices in the current gap */
    double steptime;
    size_t maxgap;

    /* We need to know the steptime so we can define the max good gap
       in seconds and convert it to steps*/

    steptime = (thesedata->sdata)[0]->hdr->steptime;
    maxgap = (size_t)( 5.0 / steptime );  /* 5 seconds is just larger than 2 WVM readings */

    /* Assume all files have the same airmass information */
    smf_find_airmass_interval( (thesedata->sdata)[0]->hdr, &amprev, NULL, NULL, NULL, status );

    for (i=0; i<nframes; i++) {

      if (!curdata) {
        SELECT_DATA( thesedata, curdata, VAL__BADD, wvm_time, i );
      }

      if (curdata) smf_tslice_ast( curdata, i, 0, status );

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
            float twvm[3]; /* WVM temperatures */
            double thistau = VAL__BADD;
            double airmass = VAL__BADD;

            prevtime = state->wvm_time;
            twvm[0] = state->wvm_t12;
            twvm[1] = state->wvm_t42;
            twvm[2] = state->wvm_t78;

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
                errRepf("", "Error calculating tau from WVM temperatures at time slice %zu",
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
              for (j=0; j<i;j++) {
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
      for (i=lastgoodidx+1; i<nframes; i++) {
        taudata[i] = lastgoodtau;
        ngood++;
      }
    }

  }

  if (*status == SAI__OK && extpars) {
    /* Read extpars to see if we need to smooth */
    double smoothtime = VAL__BADD;

    if (astMapGet0D( extpars, "SMOOTHWVM", &smoothtime ) ) {
      if (smoothtime != VAL__BADD && smoothtime > 0.0) {
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
  if (*status == SAI__OK) {
    smfData *data = (thesedata->sdata)[0];
    smfHead *hdr = data->hdr;
    for (i=0; i<nframes;i++) {
      JCMTState * state;
      state = &(hdr->allState)[i];
      printf("%zu %.*g %d %.*g %.*g\n", i, DBL_DIG, taudata[i], state->rts_num,
             DBL_DIG, state->rts_end, DBL_DIG, state->wvm_time);
    }
  }
  */

  /* Free resources */
  if (tmpthesedata) smf_close_related( &tmpthesedata, status );

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
