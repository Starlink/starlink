/*
*+
*  Name:
*     smf_flat_fastflat

*  Purpose:
*     Convert a fast flat ramp to a standard flatfield data set

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     void smf_flat_fastflat( const smfData * fflat,
*                smfData **bolvald, int *status );

*  Arguments:
*     fflat = const smfData * (Given)
*        smfData containing flatfield ramp data.
*     bolvald = smfData ** (Returned)
*        Collapsed flatfield data. First two dimensions match "fflat"
*        and the 3rd dimension will match the number of distinct heater
*        measurements. Data have not been shifted to standard power values.
*        The heater values themselves will be returned in the "heatval"
*        entry of the associated smfDA struct.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Given a fast flatfield ramp, select all the data with identical
*     heater settings, calculate the mean and standard deviation (without
*     correcting for resistance) and stores the result and the heater
*     settings in a standard form suitable for processing by the flatfield
*     routines.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - To simplify the API the heater settings for each flatfield
*       reading are returned in the smfDa component of "bolvald" using
*       the "heatval" slot.
*     - See also smf_flat_standardpow and smf_flat_polyfit.

*  History:
*     2010-03-03 (TIMJ):
*        Initial version
*     2010-03-11 (TIMJ):
*        Compensate for sky variation during ramp by subtracting a
*        polynomial fit to the reference heater values.
*     2010-03-12 (TIMJ):
*        Allow a single ramp of a single measurment to propogate through.
*        Variance on the single is not used even if present.
*     2010-03-16 (TIMJ):
*        Store the flatfield name in the smfFile
*     2010-04-16 (TIMJ):
*        The flatfield ramp does not start at the reference value so the sky
*        subtraction was failing to anchor the start of the observation. This
*        resulted in little steps in the coadded ramp. Now the code extrapolates
*        back to the reference heater and uses that in the sky fit.
*     2010-07-23 (TIMJ):
*        Set status to BADFLAT if we encounter bad values in SC2_HEAT.
*     2010-12-27 (TIMJ):
*        Fix off-by-one error when counting backwards.

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
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

/* System includes */
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "msg_par.h"
#include "ast.h"
#include "prm_par.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

static int smf__sort_ints ( const void * a, const void * b );
static double smf__calc_refheat_meas ( int indata[], dim_t boloffset, dim_t tstride, dim_t nframes,
                                       double heatdata[], double buffer[], dim_t nmeas, int heatref,
                                       int forward, int *status );

void smf_flat_fastflat( const smfData * fflat, smfData **bolvald, int *status ) {

  dim_t bstride = 0;          /* Bolometer stride */
  smfHead * hdr = NULL;       /* Local header of fflat */
  int heatbounds[] = {0,0,0,0}; /* first and last two heater values in ramps */
  AstKeyMap * heatmap = NULL; /* KeyMap of heater settings */
  int heatref = 0;            /* Reference heater setting */
  char heatstr[20];           /* Buffer for heater settings as strings */
  int * heatval = NULL;       /* Heater values in sorted order */
  dim_t i = 0;
  int maxfound = 0;           /* Maximum number found */
  dim_t meas_per_heat = 1;    /* Measurements per single heater value per ramp */
  dim_t nbols = 0;            /* number of bolometers */
  int nheat = 0;              /* Number of distinct heater settings */
  dim_t nframes = 0;          /* Total number of frames in fflat */
  const dim_t skyorder = 3;   /* Order to use for sky correction */

  dim_t tstride = 0;          /* Time stride */

  if (*status != SAI__OK) return;

  if (!smf_validate_smfData( fflat, 1, 0, status) ) return;

  if ( ! fflat->isTordered ) {
    *status = SAI__ERROR;
    errRep( "", "Flatfield ramp data must be time ordered", status );
    return;
  }

  if (fflat->ndims != 3 || (fflat->ndims == 3 && (fflat->dims)[2] < 3)) {
    *status = SAI__ERROR;
    errRep( "", "A flatfield ramp must be 3 dimensional and have at least 3 measurements",
            status );
    return;
  }

  /* Check that we are dealing with integers */
  if ( !smf_dtype_check_fatal( fflat, NULL, SMF__INTEGER, status ) ) return;

  /* Local copies of pointers */
  hdr = fflat->hdr;

  /* get properties of data array */
  smf_get_dims( fflat, NULL, NULL, &nbols, &nframes, NULL, &bstride, &tstride, status );

  /* Find the reference heater setting */
  smf_fits_getI( hdr, "PIXHEAT", &heatref, status );
  msgOutiff( MSG__VERB, " ", "Reference heater setting: %d", status, heatref );

  /* First analyse the heater settings to see how many distinct values we have. We are going
     to use a KeyMap and treat this whole thing like we would a perl hash. Store the indices
     of each repeat of a given heater setting in a vector . */
  heatmap = astKeyMap( " " );
  maxfound = 0;
  for ( i = 0; i < nframes; i++ ) {
    JCMTState * tmpstate = &(hdr->allState)[i];
    int counter = 0;
    sprintf( heatstr, "%d", tmpstate->sc2_heat );

    /* push the current index on to the end */
    astMapPutElemK( heatmap, heatstr, -1, i );

    /* and ask how many elements we have now */
    counter = astMapLength( heatmap, heatstr );

    /* and record the largest number */
    if (counter > maxfound) maxfound = counter;

  }

  /* also track the first two heater values and the last two heater values in the ramp
     in case we need them later. Bounds are first two values (0,1)
     and last two values (2,3). This will also tell us how many readings we do per heater
     setting on a single ramp. */
  if (*status == SAI__OK) {
    heatbounds[0] = (hdr->allState)[0].sc2_heat;
    heatbounds[3] = (hdr->allState)[nframes-1].sc2_heat;
    for (i = 0; i < nframes; i++ ) {
      int thisheat = (hdr->allState)[i].sc2_heat;
      if (thisheat == VAL__BADUW) {
        /* flat ramps should never have bad heater values so if we see one we decide
           that this ramp is invalid */
        if (*status == SAI__OK) {
          *status = SMF__BADFLAT;
          errRep( "", "Bad values in the SC2_HEAT state structure. Can not calculate flatfield.",
                  status);
        }
        break;
      }
      if (thisheat != heatbounds[0] && heatbounds[1] == 0 ) {
        heatbounds[1] = thisheat;
        break;
      }
    }
    for ( i = nframes-1; i >= 1; i-- ) {
      int thisheat = (hdr->allState)[i].sc2_heat;
      if (thisheat != heatbounds[3] && heatbounds[2] == 0 ) {
        heatbounds[2] = thisheat;
        /* the start of a flat ramp has many more steps at the initial heater
           setting than we really are using so derive meas_per_heat from the
           end instead */
        meas_per_heat = nframes - i - 1;
        break;
      }
    }
  }

  msgOutiff( MSG__VERB, "", "Flatfield ramp used %d heater settings of step %d, each of %d measurements in groups of %zd",
             status, astMapSize( heatmap ), abs(heatbounds[1]-heatbounds[0]), maxfound, meas_per_heat );

  /* See how many distinct heater values we have */
  nheat = astMapSize( heatmap );

  /* sort the heater settings in an integer array */
  heatval = astCalloc( nheat, sizeof(*heatval) );
  if (*status == SAI__OK) {
    int iheat;
    for (iheat = 0; iheat < nheat; iheat++ ) {
      int h;
      const char * key = astMapKey( heatmap, iheat );
      char * endptr = NULL;
      /* this must work, we can miss this step if we had another
         keymap that had this key and an integer value ! */
      h = (int) strtol( key, &endptr, 0 );

      /* should never get a 0 for heater value but some buggy files
         do have it so we use the correct test */
       if ( endptr == key ) {
        *status = SAI__ERROR;
        errRepf("", "Unexpected failure to parse '%s' to a integer", status,
                key );
        break;
      }

      heatval[iheat] = h;

    }

    /* sort */
    qsort( heatval, nheat, sizeof( *heatval ), smf__sort_ints );


  }

  /* Now we need to create a bolval smfData to match the standard
     flatfield data measurements */
  smf_flat_malloc( nheat, fflat, NULL, bolvald, status );

  if (*status == SAI__OK) {
    double * bolval = (*bolvald)->pntr[0];
    double * bolvalvar = (*bolvald)->pntr[1];
    double *coeff = NULL;
    double *coeffvar = NULL;
    int * indices = NULL;
    int * idata = NULL;
    int * ffdata = (fflat->pntr)[0];
    JCMTState * instate = hdr->allState;
    JCMTState * outstate = NULL;
    double * skycoeffs = NULL;
    double * skycoeffsvar = NULL;
    int nind = 0;
    dim_t bol;

    /* get some memory for the indices */
    indices = astCalloc( maxfound, sizeof(*indices) );

    /* and equivalent memory for the readings at each index */
    idata = astCalloc( maxfound, smf_dtype_sz( fflat->dtype, status ) );

    /* Need some memory for the JCMTSTATE information. */
    outstate = astMalloc( nheat*sizeof(*outstate) );
    (*bolvald)->hdr->allState = outstate;

    /* First need to compensate for any drift in the DC sky level.
       We do this by looking at the heater measurements for the reference
       heater as a function of time (index) for each bolometer and then
       fitting it with a polynomial. */
    skycoeffs = astCalloc( nbols * (skyorder+1), sizeof(*skycoeffs) );
    skycoeffsvar = astCalloc( nbols * (skyorder+1), sizeof(*skycoeffs) );

    /* temp memory to hold the coefficients for a single bolometer */
    coeff = astCalloc( skyorder + 1, sizeof(*coeff) );
    coeffvar = astCalloc( skyorder + 1, sizeof(*coeffvar) );

    /* check status after memory allocation */
    if (*status == SAI__OK) {
      double * ddata = NULL;
      double * dindices = NULL;
      dim_t extras = 0;    /* extra space required */
      const dim_t szfit = 30; /* number of points to use for ref heat extrapolation */
      double * before_heat = NULL;
      double * after_heat = NULL;
      double * heatmeas = NULL;

      /* get the key based on the reference heater integer */
      sprintf( heatstr, "%d", heatref );

      /* and hence we can get all the relevant indices */
      astMapGet1I( heatmap, heatstr, maxfound, &nind, indices );

      /* see whether we need to anchor the data at the ends. We don't
       need to be super accurate, just something to constrain the fit. */
      if (heatbounds[0] != heatref) {
        /* we are not starting at the reference heater but we can assume we
         are close enough that a linear fit on the first N measurements in the
         ramp will be a reasonable approximation. We'll need the first N heater
         measurements. */
        before_heat = astCalloc( szfit, sizeof(*before_heat) );
        extras += meas_per_heat;
        for (i=0; i<szfit; i++) {
          before_heat[i] = (hdr->allState)[i].sc2_heat;
        }
      }

      if (heatbounds[3] != heatref ) {
        /* we are not ending at the reference heater but we can assume we
         are close enough that a linear fit on the first N measurements in the
         ramp will be a reasonable approximation. We'll need the first N heater
         measurements. */
        after_heat = astCalloc( szfit, sizeof(*after_heat) );
        extras += meas_per_heat;
        for (i=0;i<szfit; i++) {
          after_heat[i] = (hdr->allState)[nframes-1-i].sc2_heat;
        }
      }

      /* and _DOUBLE versions because smf_fit_poly1d takes doubles */
      ddata = astCalloc( maxfound + extras, sizeof(*ddata) );
      dindices = astCalloc( maxfound + extras, sizeof(*dindices) );

      if (before_heat || after_heat) heatmeas = astCalloc( szfit, sizeof(*heatmeas) );

      if (*status == SAI__OK) {
        for (bol = 0; bol < nbols; bol++) {
          dim_t idx;
          int64_t nused;
          dim_t ndata = nind;

          /* copy over the relevant data for this bolometer */
          for (idx = 0; idx < ndata; idx++) {
            dim_t slice = indices[idx];
            ddata[idx] = ffdata[ bol * bstride + slice*tstride ];
            dindices[idx] = slice;
          }

          /* Calculate any extrema for anchoring the fit */
          if (before_heat) {
            int nstepsoffset;
            int stepsize;
            int deltaheat;
            double result = smf__calc_refheat_meas( ffdata, bol*bstride,
                                                    tstride, nframes, before_heat, heatmeas,
                                                    szfit, heatref, 1, status );
            /* push the result onto the array for fitting. Making sure we give it
               equal weight by duplicating it. The coordinate must refer to the start
               of the time stream in indices and so be negative. */
            stepsize = heatbounds[1] - heatbounds[0];
            deltaheat = heatref - before_heat[0];
            nstepsoffset = (abs(deltaheat / stepsize) -1 ) * (int)meas_per_heat;
            for (i = 0; i < meas_per_heat; i++) {
              ddata[ndata] = result;
              dindices[ndata] = -1.0 - nstepsoffset - i;
              ndata++;
            }
          }

          if (after_heat) {
            int nstepsoffset;
            int stepsize;
            int deltaheat;
            double result = smf__calc_refheat_meas( ffdata, bol*bstride,
                                                    tstride, nframes, after_heat, heatmeas,
                                                    szfit, heatref, 0, status );
            /* push the result onto the array for fitting.
               Need to convert the heater ref value to an indices for fitting.
             */
            stepsize = heatbounds[1] - heatbounds[0];
            deltaheat = heatref - after_heat[0];
            nstepsoffset = (abs(deltaheat / stepsize) -1 ) * (int)meas_per_heat;
            for (i = 0; i < meas_per_heat; i++) {
              ddata[ndata] = result;
              dindices[ndata] = nframes - 1.0 + nstepsoffset + i;
              ndata++;
            }
          }

          msgOutiff( MSG__DEBUG20, "",
                     "Calculating sky background at reference heater for bolometer %zd",
                     status, bol);
          if (ndata > 1) {
            smf_fit_poly1d( skyorder, ndata, 0, 0, dindices, ddata, NULL, NULL,
                            coeff, coeffvar, NULL, &nused, status );

          } else {
            coeff[0] = ddata[0];
            coeffvar[0] = 0.0;
            for ( idx = 1; idx <= skyorder; idx++) {
              coeff[idx] = 0.0;
              coeffvar[idx] = 0.0;
            }
          }

          /* copy coefficients into array */
          for (idx = 0; idx <= skyorder; idx++) {
            skycoeffs[ bol + idx*nbols ] = coeff[idx];
            skycoeffsvar[ bol + idx*nbols ] = coeffvar[idx];
          }
        }
      }

      if (ddata) ddata = astFree( ddata );
      if (dindices) dindices = astFree( dindices );
      if (before_heat) before_heat = astFree( before_heat );
      if (after_heat) after_heat = astFree( after_heat );
      if (heatmeas) heatmeas = astFree( heatmeas );

      /* for each heater value we now need to calculate the measured signal */
      for ( i = 0; i < nheat; i++) {

        /* get the key based on this heater integer */
        sprintf( heatstr, "%d", heatval[i] );

        /* and hence we can get all the relevant indices */
        astMapGet1I( heatmap, heatstr, maxfound, &nind, indices );

        /* Copy state from the first entry */
        if (*status == SAI__OK) memcpy( &(outstate[i]), &(instate[indices[0]]), sizeof(*outstate));

        /* now we need to loop over each bolometer to calculate the statistics. */
        for ( bol = 0; bol < nbols; bol++) {
          double mean = VAL__BADD;
          double sigma = VAL__BADD;
          dim_t ngood = 0;
          dim_t idx;
          double skyoffset = 0.0;

          /* Get the polynomial data */
          for (idx = 0; idx <= skyorder; idx++) {
            coeff[idx] = skycoeffs[ bol + idx*nbols ];
            coeffvar[idx] = skycoeffsvar[ bol + idx * nbols ];
          }

          /* Obtain the measurements for that bolometer */
          for (idx = 0; idx < (dim_t)nind; idx++ ) {
            dim_t slice = indices[idx];
            int thisdata = ffdata[ bol*bstride + slice*tstride ];

            if (thisdata != VAL__BADI) {
              /* calculate the reference value */
              double poly = 0.0;
              EVALPOLY( poly, slice, skyorder, coeff );

              /* This section can be uncommented to help debug any issues with sky removal.
                 Just pick a bolometer number and grep for VALUES in the output. Then load
                 into topcat */
              /*
              if (bol == 334) {
                printf( "VALUES %zu %zu %d %d %g %g\n", slice, idx, heatval[i],idata[idx], poly, (double)idata[idx] - poly);
              }
              */

              if (poly != VAL__BADD) thisdata -= (int)poly;

              idata[ngood] = thisdata;
              ngood++;
            }
          }

          /* We need to get statistics but smf_stats1I won't give us values
             if we do not have enough data points. We do an estimate by hand */

          if ( ngood < SMF__MINSTATSAMP ) {

            /* Calculate the mean but for sigma we put in the min/max difference
               just to have an estimate that is large for down weighting */
            double minval = VAL__BADD;
            double maxval = VAL__BADD;
            double isum = 0.0;

            for (idx=0; idx < (dim_t)ngood; idx++) {
              if (idata[idx] != VAL__BADI) {
                if (minval == VAL__BADD) {
                  minval = idata[idx];
                } else if (minval > idata[idx]) {
                  minval = idata[idx];
                }
                if (maxval == VAL__BADD) {
                  maxval = idata[idx];
                } else if (maxval < idata[idx]) {
                  maxval = idata[idx];
                }
                isum += idata[idx];
              }
            }
            if (ngood > 1) {
              mean = isum / (int)ngood;
              sigma = (maxval - minval) / 2.0;
            } else if (ngood == 1) {
              /* Make up a number for sigma to stop the fit going crazy */
              mean = isum;
              sigma = 0.1 * isum;
            } else {
              mean = VAL__BADD;
              sigma = VAL__BADD;
            }

          } else {
            /* Calculate properly */
            smf_stats1I( idata, 1, ngood, NULL, 0, 0, &mean, &sigma, NULL,
                         &ngood, status );

          }

          /* Need to put the sky signal back into the data to ensure that the
             sky offset is correctly accounted for. Use the value from the
             polynomial from the last frame */
          EVALPOLY(skyoffset, (nframes-1), skyorder, coeff );

          /* store the answer */
          idx = bol + i*nbols;
          if (sigma == VAL__BADD || sigma == 0.0) {
            bolval[ idx ] = VAL__BADD;
            bolvalvar[ idx ] = VAL__BADD;
          } else {

            bolval[ idx ] = mean + skyoffset;
            bolvalvar[ idx ] = sigma * sigma;
          }
        }

      }
    }

    if (idata) idata = astFree( idata );
    if (indices) indices = astFree( indices );
    if (coeff) coeff = astFree( coeff );
    if (coeffvar) coeffvar = astFree( coeffvar );
    if (skycoeffs) skycoeffs = astFree( skycoeffs );
    if (skycoeffsvar) skycoeffsvar = astFree( skycoeffsvar );

  }

  /* Create a smfDA struct to store the heater settings. */
  if (*status == SAI__OK) {
    smfDA * da = NULL;
    double * dheatval = astMalloc( nheat*sizeof(*dheatval) );

    for (i = 0; i < nheat; i++) {
      dheatval[i] = heatval[i];
    }

    da = smf_construct_smfDA( NULL, NULL, NULL, NULL,
                              SMF__FLATMETH_NULL, 0, VAL__BADD,
                              dheatval, nheat, status );

    (*bolvald)->da = da;
  }

  /* Store the ramp filename in a smfFile so that we know where the flatfield
     data came from */
  if (fflat->file && *status == SAI__OK) {
    (*bolvald)->file = smf_construct_smfFile( (*bolvald)->file, NDF__NOID, 0, 0,
                                              fflat->file->name, status );
  }

  if (heatval) heatval = astFree( heatval );
  heatmap = astAnnul( heatmap );

}

static
int smf__sort_ints ( const void * a, const void * b ) {
  const int * ia;
  const int * ib;
  int temp;
  ia = a;
  ib = b;
  temp = *ia - *ib;
  if ( temp > 0 ) {
    return 1;
  } else if ( temp < 0 ) {
    return -1;
  } else {
    return 0;
  }
}

/* Calculate the reference heater value by extrapolation */

double smf__calc_refheat_meas ( int indata[], dim_t boloffset, dim_t tstride, dim_t nframes,
                                double heatdata[], double buffer[], dim_t nmeas, int heatref,
                                int forward, int *status ) {
  double result = VAL__BADD;
  int64_t nused;
  double coeff[2];
  dim_t i;

  if (*status != SAI__OK) return result;

  for (i = 0; i<nmeas; i++) {
    dim_t htindex;
    if (forward) {
      htindex = i;
    } else {
      htindex = nframes - 1 - i;
    }
    buffer[i] = indata[ boloffset + htindex * tstride ];
  }
  smf_fit_poly1d( 1, nmeas, 0, 0, heatdata, buffer, NULL, NULL,
                  coeff, NULL, NULL, &nused, status );

  /* Evaluate the result and push it onto the array for fitting */
  EVALPOLY( result, heatref, 1, coeff );
  if (result != 0.0)
    msgOutiff(MSG__DEBUG20, "", "Extrapolated heater value of %g\n",status, result);
  return result;
}
