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
*                             double snrmin, size_t order, smfData ** coeffs,
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
*     order = size_t (Given)
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
*     {enter_new_authors_here}

*  History:
*     2005-08-18 (BDK):
*        Original version written for the simulator (latterly called sc2sim_fitheat)
*     2010-02-04 (TIMJ):
*        Rewritten in SMURF based upon logic from sc2sim_fitheat.
*     2010-04-09 (TIMJ):
*        Add snr filter for fit in linear case.
*        Use correct power values in the presence of bad data.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "smf_typ.h"
#include "smf.h"
#include "smurf_par.h"

#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"

#include "gsl/gsl_fit.h"

void smf_flat_fitpoly ( const smfData * powvald, const smfData * bolvald,
                        double snrmin, size_t order, smfData **coeffs,
                        smfData **polyfit, int *status ) {

  size_t bol;               /* bolometer index */
  double * bolval = NULL;   /* Pointer to bolvald smfData */
  double * bolvar = NULL;   /* Pointer to bolvald variance */
  const double CLIP = 3.0;  /* Sigma clipping for polynomial fit */
  double * coptr = NULL;    /* pointer to coefficients data array */
  double * goodht = NULL;   /* Heater values for good measurements */
  size_t * goodidx = NULL;     /* Indices of good measurements */
  double * ht = NULL;       /* Heater values corrected for reference */
  size_t j;
  const size_t NCOEFF = 6;  /* Max number of coefficients per bolometer */
  size_t nbol = 0;          /* Number of bolometers */
  size_t nheat = 0;         /* Number of measurements */
  size_t nsnr = 0;          /* Number of bolometers flagged by SNR limit */
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
    errRepf( "", "Too few measurements to calculate reliable fit. %zd required.",
             status, order + 6);
    return;
  }


  /* Create the smfData for the result. Flatfield of type POLYNOMIAL
     requires the third dimension to be 6 so we initialise all to 0.0 */
  coptr = smf_malloc( NCOEFF * nbol, sizeof(*coptr), 1, status );
  {
    void * pntr[3];
    dim_t dims[3];
    int lbnd[3];
    pntr[0] = coptr;
    pntr[1] = NULL;
    pntr[2] = NULL;
    dims[0] = (bolvald->dims)[0];
    dims[1] = (bolvald->dims)[1];
    dims[2] = NCOEFF;
    lbnd[0] = (bolvald->lbnd)[0];
    lbnd[1] = (bolvald->lbnd)[1];
    lbnd[2] = 1;

    *coeffs = smf_construct_smfData( NULL, NULL, NULL, NULL, SMF__DOUBLE,
                                     pntr, 1, dims, lbnd, 3, 0, 0, NULL,
                                     NULL, status);
  }

  /* Get some work space for the fits */
  scan = smf_malloc ( nheat, sizeof(*scan), 1, status );
  if (bolvar) scanvar = smf_malloc ( nheat, sizeof(*scanvar), 1, status );
  ht = smf_malloc ( nheat, sizeof(*ht), 1, status );
  goodht = smf_malloc ( nheat, sizeof(*goodht), 1, status );
  scoeff = smf_malloc( order + 1, sizeof(*scoeff), 0, status );
  scoeffvar = smf_malloc( order + 1, sizeof(*scoeffvar), 0, status );
  goodidx = smf_malloc( nheat, sizeof(*goodidx), 1, status );

  /* Assume that we have monotonically increasing heater settings and so
     pick a value from the middle as a reference. Ramps may well have an
     issue here. */
  refheat = powval[nheat/2];
  for ( j=0; j<nheat; j++ ) {
    ht[j] = powval[j] - refheat;
  }

  /* space for the calculated polynomial */
  if (polyfit && order == 1) polybol = smf_malloc( nheat*nbol, sizeof(*polybol), 0, status );
  poly = smf_malloc( nheat, sizeof(*poly), 0, status );

  /* Now loop over each bolometer and extract the measurements */
  for (bol=0; bol<nbol; bol++) {
    const double refval = bolval[nbol*(nheat/2)+bol];
    double refvar = 0.0;
    size_t i;
    size_t nrgood = 0;

    if (bolvar) {
      refvar = bolvar[nbol*(nheat/2)+bol];
    }

    for (i=0; i<nheat; i++) {
      if (bolval[nbol*i+bol] != VAL__BADD && bolvar[nbol*i+bol] != VAL__BADD) {
        scan[nrgood] = bolval[nbol*i+bol] - refval;
        goodht[nrgood] = ht[i];
        if (scanvar) {
          scanvar[nrgood] = bolvar[nbol*i+bol] + refvar;
        }
        goodidx[nrgood] = i;
        nrgood++;
      }
    }

    /* fit the polynomial. Offset the coefficients array to account for the
       two values we add for flatfield. */
    if (nrgood > 0) {
      int badcoeffs = 0;
      size_t nused;

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

        smf_fit_poly1d( order, nrgood, CLIP, goodht, scan, scanvar, scoeff, scoeffvar, poly, &nused, status);

        /* and calculate the fitted polynomial */
        if (polybol) {
          for (i=0; i<nheat;i++) {
            polybol[i*nbol+bol] = VAL__BADD;
          }
          for (i=0; i<nrgood; i++) {
            size_t idx = goodidx[i];
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

        if (c0 != VAL__BADD && c1 != VAL__BADD) {
          /* simple inverse of straight line fit */
          scoeff[0] = -1.0 * c0 / c1;
          scoeff[1] = 1.0 / c1;
        }

      } else {
        /* disable variance and fit the other way */
        smf_fit_poly1d( order, nrgood, CLIP, scan, goodht, NULL, scoeff, NULL, NULL, &nused, status);
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

  msgOutiff( MSG__VERB, "", "Flagged %zd bolometers with gradients failing SNR > %g",
             status, nsnr, snrmin);

  if (polybol) {
    if (polyfit) {
      void *pntr[3];
      pntr[0] = polybol;
      pntr[1] = NULL;
      pntr[2] = NULL;
      *polyfit = smf_construct_smfData( NULL, NULL, NULL, NULL, SMF__DOUBLE,
                                        pntr, 1, bolvald->dims, bolvald->lbnd, 3, 0, 0, NULL,
                                        NULL, status );
      if (*status != SAI__OK && ! *polyfit) polybol = smf_free( polybol, status );
    } else {
      polybol = smf_free( polybol, status );
    }
  }

  if (poly) poly = smf_free( poly, status );
  if (scan) scan = smf_free( scan, status );
  if (scanvar) scanvar = smf_free( scanvar, status );
  if (scoeff) scoeff = smf_free( scoeff, status );
  if (scoeffvar) scoeff = smf_free( scoeffvar, status );
  if (ht) ht = smf_free( ht, status );
  if (goodht) goodht = smf_free( goodht, status );
  if (goodidx) goodidx = smf_free( goodidx, status );

  if (*status != SAI__OK) {
    if (*coeffs) smf_close_file( coeffs, status );
    if (*polyfit) smf_close_file( polyfit, status );
  }

}
