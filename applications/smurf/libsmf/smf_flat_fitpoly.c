/*
*+
*  Name:
*     smf_flat_fitpoly

*  Purpose:
*     Calculate POLYNOMIAL flatfield solution

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     void smf_flat_fitpoly ( const smfData * powval, const smfData * bolval,
*                             double snrmin, dim_t order, smfData ** coeffs,
*                             smfData ** polyfit, int *status );

*  Arguments:
*     powval = const smfData * (Given)
*        Resistance input powers. 1 dimensional.
*     bolval = const smfData * (Given)
*        Response of each bolometer to powval. Dimensioned as number of
*        number of bolometers times dimension of powval.
*     snrmin = double (Given)
*        Minimum acceptable signal-to-noise ratio for the gradient.
*        Below this value the fit will be treated as bad and the bolometer
*        will be disabled. Only used when doing a linear fit.
*     order = dim_t (Given)
*        Order of fit. Must lie between 1 (linear) and 3 (cubic).
*     coeffs = smfData ** (Returned)
*        smfData of coefficients. Will be returned with 6 entries even if order
*        less than 3 is requested. The first two coefficients refernce heater
*        and bolometer values.
*        elements even if an order less than 3 is requested.
*     polyfit = smfData ** (Returned)
*        This is the polynomial expansion for each powval coordinate for direct
*        comparison with "bolval". Will only be created for first order fits.
*        Can be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     For each bolometer calculate the best fit polynomial of bolometer response
*     versus input heater power in a form suitable for storing as a POLYNOMIAL
*     flatfield solution.

*  Authors:
*     BDK: Dennis Kelly (UKATC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     COBA: Coskun Oba (UoL)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2005-08-18 (BDK):
*        Original version written for the simulator (latterly called sc2sim_fitheat)
*     2010-02-04 (TIMJ):
*        Rewritten in SMURF based upon logic from sc2sim_fitheat.
*     2010-04-09 (TIMJ):
*        Add snr filter for fit in linear case.
*        Use correct power values in the presence of bad data.
*     2010-04-14 (TIMJ):
*        Use correlation coefficient to flag fits where the data doesn't
*        really look like the fitted function. The issue is that the error
*        bars are large enough in some cases that a curvy flatfield can be
*        fit by a straight line with a reasonable signal-to-noise.
*     2010-09-17 (COBA):
*        Updated smf_construct_smfData which now contains smfFts
*     2013-08-16 (DSB):
*        Use a nearby good value as the reference value if the central value
*        is bad, rather than rejecting the whole bolometer.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2010,2013 Science and Technology Facilities Council.
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

#include <stdint.h>

#include "smf_typ.h"
#include "smf.h"
#include "smurf_par.h"
#include "smf_err.h"

#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"

#include "gsl/gsl_fit.h"

void smf_flat_fitpoly ( const smfData * powvald, const smfData * bolvald,
                        double snrmin, dim_t order, smfData **coeffs,
                        smfData **polyfit, int *status ) {

  dim_t bol;               /* bolometer index */
  double * bolval = NULL;   /* Pointer to bolvald smfData */
  double * bolvar = NULL;   /* Pointer to bolvald variance */
  const double CLIP = 3.0;  /* Sigma clipping for polynomial fit */
  double * coptr = NULL;    /* pointer to coefficients data array */
  double * corrs = NULL;    /* correlation coefficients for each bolometer */
  double corr_thresh = 0.0; /* threshold for correlation coefficient thresholding */
  double * goodht = NULL;   /* Heater values for good measurements */
  dim_t * goodidx = NULL;  /* Indices of good measurements */
  double * ht = NULL;       /* Heater values corrected for reference */
  dim_t iref;              /* Index of reference measurement */
  dim_t ireflo;            /* Lowest allowed index for reference measurement */
  dim_t j;
  const dim_t NCOEFF = 6;  /* Max number of coefficients per bolometer */
  dim_t nbol = 0;          /* Number of bolometers */
  dim_t ncorr = 0;         /* Number of bolometers flagged due to bad correlation coefficient */
  dim_t nheat = 0;         /* Number of measurements */
  dim_t nsnr = 0;          /* Number of bolometers flagged by SNR limit */
  double *poly = NULL;      /* polynomial expansion of each fit */
  double *polybol = NULL;   /* polynomial expansion for all bolometers */
  double * powval = NULL;   /* Pointer to powvald smfData */
  double refheat = 0.0;     /* Reference heater setting */
  double * scan = NULL;     /* corrected bol values for a single bolometer */
  double * scanvar = NULL;  /* Variance on corrected bol (if bolvar) */
  double * scoeff = NULL;   /* Coefficients for a single scan */
  double * scoeffvar = NULL;/* Error in Coefficients for a single scan */

  *coeffs = NULL;

  if (*status != SAI__OK) return;

  if (order < 1 || order > 3) {
    *status = SAI__ERROR;
    errRepf( "", "Polynomial flatfield must be of order 1, 2 or 3 not %zd",
            status, order);
    return;
  }

  /* Get information from smfData */
  nheat = (powvald->dims)[0];
  nbol = (bolvald->dims)[0] * (bolvald->dims)[1];
  powval = (powvald->pntr)[0];
  bolval = (bolvald->pntr)[0];
  bolvar = (bolvald->pntr)[1];

  /* do not allow a fit if we have fewer than order + 6 measurements
     since we want to have some reliability */
  if ( nheat < order + 6 ) {
    *status = SAI__ERROR;
    errRepf( "", "Too few heater measurements to calculate reliable fit. %zd required, got %zd.",
             status, order + 6, nheat);
    return;
  }


  /* Create the smfData for the result. Flatfield of type POLYNOMIAL
     requires the third dimension to be 6 so we initialise all to 0.0 */
  coptr = astCalloc( NCOEFF * nbol, sizeof(*coptr) );
  {
    void * pntr[2];
    dim_t dims[3];
    dim_t lbnd[3];
    pntr[0] = coptr;
    pntr[1] = NULL;
    dims[0] = (bolvald->dims)[0];
    dims[1] = (bolvald->dims)[1];
    dims[2] = NCOEFF;
    lbnd[0] = (bolvald->lbnd)[0];
    lbnd[1] = (bolvald->lbnd)[1];
    lbnd[2] = 1;

    *coeffs = smf_construct_smfData( NULL, NULL, NULL, NULL, NULL, SMF__DOUBLE,
                                     pntr, NULL, SMF__QFAM_TSERIES, NULL, 0, 1,
                                     dims, lbnd, 3, 0, 0, NULL, NULL, status);
  }

  /* Get some work space for the fits */
  scan = astCalloc( nheat, sizeof(*scan) );
  if (bolvar) scanvar = astCalloc( nheat, sizeof(*scanvar) );
  ht = astCalloc( nheat, sizeof(*ht) );
  goodht = astCalloc( nheat, sizeof(*goodht) );
  scoeff = astMalloc( (order + 1)*sizeof(*scoeff) );
  scoeffvar = astMalloc( (order + 1)*sizeof(*scoeffvar) );
  goodidx = astCalloc( nheat, sizeof(*goodidx) );
  corrs = astMalloc( nbol*sizeof(*corrs) );

  /* Assume that we have monotonically increasing heater settings and so
     pick a value from the middle as a reference. Ramps may well have an
     issue here. */
  refheat = powval[nheat/2];
  for ( j=0; j<nheat; j++ ) {
    ht[j] = powval[j] - refheat;
  }

  /* space for the calculated polynomial */
  if (polyfit && order == 1) polybol = astMalloc( (nheat*nbol)*sizeof(*polybol) );
  poly = astMalloc( nheat*sizeof(*poly) );

  /* Now loop over each bolometer and extract the measurements */
  for (bol=0; bol<nbol; bol++) {

    /* Find a reference value. Use the central value if good, otherwise
       use the previous good value so long as it is not too far away from
       the centre. */
    iref = nheat/2;

    ireflo = iref - 5;
    if( ireflo < nheat/4 ) ireflo = nheat/4;

    double refval = bolval[nbol*iref+bol];

    while( refval == VAL__BADD && iref > ireflo ) {
       iref--;
       refval = bolval[nbol*iref+bol];
    }

    double refvar = 0.0;
    dim_t i;
    dim_t nrgood = 0;

    /* initialise the correlation coefficients array */
    if (corrs) corrs[bol] = VAL__BADD;

    if (bolvar) {
      refvar = bolvar[nbol*iref+bol];
    }

    /* If refval is bad then this means that there were no good measurements
       close to the centre of the array. Chances are it's a dodgy ramp so
       we ignore it. */
    if (refval != VAL__BADD) {
      for (i=0; i<nheat; i++) {
        if (bolval[nbol*i+bol] != VAL__BADD && bolvar[nbol*i+bol] != VAL__BADD) {
          scan[nrgood] = bolval[nbol*i+bol] - refval;
          goodht[nrgood] = ht[i];
          if (scanvar) {
            if (refvar != VAL__BADD) {
              scanvar[nrgood] = bolvar[nbol*i+bol] + refvar;
            } else {
              scanvar[nrgood] = VAL__BADD;
            }
          }
          goodidx[nrgood] = i;
          nrgood++;
        }
      }
    }

    /* fit the polynomial. Offset the coefficients array to account for the
       two values we add for flatfield. */
    if (nrgood > 0) {
      int badcoeffs = 0;
      int64_t nused;

      /* initialise scoeff to bad */
      for (i=0; i< order+1; i++) {
        scoeff[i] = VAL__BADD;
        scoeffvar[i] = VAL__BADD;
      }

      /* Note that we want a polynomial that fits the heater values given
         the current measurements. And not the current measurements given the heater.
         Note that the polynomial fitting routine does not support variance on the x-
         values. We can invert the special case of a linear fit. */
      if (order == 1) {
        double c0;
        double c1;
        double gain;
        double offset;
        double corr;
        int usevar = 0;

        /* if all the variances are identical we do an unweighted fit.
         This is mainly for TABLE data after writing to flat files where
         we will not have any variance */
        refvar = scanvar[0];
        for (i=0; i<nrgood; i++) {
          double mydiff = fabs(scanvar[i] - refvar);
          if (mydiff > 1e-12) {
            usevar = 1;
            break;
          }
        }

        smf_fit_poly1d( order, nrgood, CLIP, 0, goodht, scan, (usevar ? scanvar : NULL),
                        NULL, scoeff, scoeffvar, poly, &nused, status);

        /* and calculate the fitted polynomial */
        if (polybol) {
          for (i=0; i<nheat;i++) {
            polybol[i*nbol+bol] = VAL__BADD;
          }
          for (i=0; i<nrgood; i++) {
            dim_t idx = goodidx[i];
            if (!isnan(poly[i]) && poly[i] != VAL__BADD) polybol[idx*nbol+bol] = poly[i] + refval;
          }
        }

        c0 = (isnan(scoeff[0]) ? VAL__BADD : scoeff[0] );
        c1 = (isnan(scoeff[0]) ? VAL__BADD : scoeff[1] );

        /* Check signal to noise of gradient */
        if (scoeffvar && c1 != VAL__BADD) {
          double snr = fabs(c1) / sqrt( scoeffvar[1] );
          msgOutiff( MSG__DEBUG20, "", "SNR of flatfield fit for bolometer %zd = %g\n", status, bol, snr );
          if (snr < snrmin ) {
            c0 = VAL__BADD;
            c1 = VAL__BADD;
            nsnr++;
          }
        }

        /* Now compare the fit to the data and get a correlation
           coefficient to decide whether the data really do look like
           the fit. We need this because sometimes a nice curvy line
           gets a high signal-to-noise linear fit. Ignore non-linear
           for the moment. Only do this test if the SNR test has
           passed. */

        if (c0 != VAL__BADD && c1 != VAL__BADD) {
          smf_templateFit1D( scan, NULL, NULL, NULL, 0, 0, nrgood, 1, poly, 0,
                             0, &gain, &offset, &corr, status );
          msgOutiff(MSG__DEBUG20, "",
                    " Template: Bol %zd Gain = %g offset = %g corr = %g",
                    status, bol, gain, offset, corr );
          corrs[bol] = corr;
        }

        if (c0 != VAL__BADD && c1 != VAL__BADD) {
          /* simple inverse of straight line fit */
          scoeff[0] = -1.0 * c0 / c1;
          scoeff[1] = 1.0 / c1;
        }

      } else {
        /* disable variance and fit the other way */
        smf_fit_poly1d( order, nrgood, CLIP, 0, scan, goodht, NULL, NULL, scoeff,
                        NULL, NULL, &nused, status);
      }


      /* copy the result into coptr */
      for (i=0; i<=order; i++) {
        double val = scoeff[i];
        if (isnan(val)) {
          val = VAL__BADD;
          badcoeffs = 1;
          break;
        }
        coptr[(i+2)*nbol+bol] = val;
      }
      /* if any of the coefficients are bad blank everything */
      if (badcoeffs) {
        for (i=0; i<NCOEFF;i++) {
          coptr[i*nbol+bol] = VAL__BADD;
        }
      } else {
        coptr[bol] = refheat;
        coptr[nbol+bol] = refval;
        /* set any remaining coefficients to 0
         Offset is the number of coefficients (order+1) + 2 for the two reference values */
        for (i=order+3; i < NCOEFF; i++) {
          coptr[i*nbol+bol] = 0.0;
        }
      }

    } else {

      /* set everything to bad */
      for (i=0; i<NCOEFF; i++) {
        coptr[i*nbol+bol] = VAL__BADD;
      }

      if (polybol) {
        for (i=0; i<nheat; i++) {
          polybol[i*nbol+bol] = VAL__BADD;
        }
      }
    }

  }

  /* Analyze correlation coefficients */
  if (corrs && *status == SAI__OK) {
    double csig = VAL__BADD;
    double cmean = VAL__BADD;
    dim_t ngood = 0;
    double corr_bigtol = 3.0;
    double corr_smalltol = 0.75;
    double delta_mean = 0.0;

    smf_stats1D( corrs, 1, nbol, NULL, 0, 0, &cmean, &csig, NULL, &ngood,
                 status );

    if (*status == SMF__INSMP) {
      /* it has all gone horribly wrong. Let someone else report the bad news */
      errAnnul( status );
    } else {
      msgOutiff( MSG__DEBUG20, "", "Fit Correlation coefficients = %g +/- %g (%zd)\n",
                 status, cmean, csig, ngood);

      /* Now loop over the bolometers and throw out the ones on the lower end of the
         scale. If the mean is within 0.5 sigma of 1.0 we use a "mean - 1 sigma" threshold,
         else we use a "mean - 3 sigma" threshold. We do this because the case where the
         distribution is jammed up against 1.0 is decidedly non-gaussian. */
      delta_mean = ( 1.0 - cmean ) / csig;
      if ( delta_mean < 0.5 ) {
        corr_thresh = corr_smalltol;
      } else {
        corr_thresh = corr_bigtol;
      }
      corr_thresh = cmean - ( csig * corr_thresh );
      for ( bol = 0; bol < nbol; bol++ ) {
        if (corrs[bol] != VAL__BADD && corrs[bol] < corr_thresh ) {
          dim_t i;
          ncorr++;
          /* blank that bolometer */
          for (i=0; i<NCOEFF;i++) {
            coptr[i*nbol+bol] = VAL__BADD;
          }
        }
      }
    }
  }


  msgOutiff( MSG__VERB, "", "Flagged %zd bolometers with gradients failing SNR > %g",
             status, nsnr, snrmin);
  if (ncorr > 0) {
    msgOutiff( MSG__VERB, "", "Flagged %zd bolometers with fit correlation coefficients < %g",
               status, ncorr, corr_thresh );
  }


  if (polybol) {
    if (polyfit) {
      void *pntr[2];
      pntr[0] = polybol;
      pntr[1] = NULL;
      *polyfit = smf_construct_smfData( NULL, NULL, NULL, NULL, NULL,
                                        SMF__DOUBLE, pntr, NULL,
                                        SMF__QFAM_TSERIES, NULL, 0, 1,
                                        bolvald->dims, bolvald->lbnd, 3, 0, 0,
                                        NULL, NULL, status );
      if (*status != SAI__OK && ! *polyfit) polybol = astFree( polybol );
    } else {
      polybol = astFree( polybol );
    }
  }

  if (poly) poly = astFree( poly );
  if (scan) scan = astFree( scan );
  if (scanvar) scanvar = astFree( scanvar );
  if (scoeff) scoeff = astFree( scoeff );
  if (scoeffvar) scoeff = astFree( scoeffvar );
  if (ht) ht = astFree( ht );
  if (goodht) goodht = astFree( goodht );
  if (goodidx) goodidx = astFree( goodidx );
  if (corrs) corrs = astFree( corrs );

  if (*status != SAI__OK) {
    if (*coeffs) smf_close_file( NULL, coeffs, status );
    if (*polyfit) smf_close_file( NULL, polyfit, status );
  }

}
