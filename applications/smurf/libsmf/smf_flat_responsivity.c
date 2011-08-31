/*
*+
*  Name:
*     smf_flat_responsivity

*  Purpose:
*     Calculate power relations for all bolometers

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     size_t smf_flat_responsivity ( smf_flatmeth method, smfData *respmap, double snrmin,
*                                    size_t order, const smfData * powval, const smfData * bolval,
*                                    smfData ** polyfit, int *status );

*  Arguments:
*     method = smf_flatmeth (Given)
*        Type of flatfield being presented in bolval and powval. Can
*        be SMF__FLATMETH_POLYNOMIAL or SMF__FLATMETH_TABLE.
*     respmap = smfData * (Given & Returned)
*        smfData to receive the responsivity map. Must be _DOUBLE.
*        Will be assumed to be the correct size as compared to bolval.
*     snrmin = double (Given)
*        Minimum acceptable signal-to-noise ratio for a responsivity fit.
*        Below this value the fit will be treated as bad and the bolometer
*        will be disabled. Only used when fitting TABLE data.
*     order = size_t (Given)
*        If the data are being fitted this is the order of polynomial
*        to use. Ignored if method is POLYNOMIAL.
*     powval = const smfData * (Given)
*        Resistance input powers. Is the number of heater measurements.
*     bolval = const smfData * (Given)
*        Response of each bolometer to powval. Dimensioned as number of
*        number of bolometers (size of respmap) times number of heater
*        measurements (size of powval).
*     polyfit = smfData ** (Returned)
*        If a polynomial is being fitted this is the polynomial expansion
*        for each powval coordinate for direct comparison with "bolval".
*        Can be NULL. Will be ignored if method is POLYNOMIAL. Will not
*        be returned if the function sets status to bad.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     For each bolometer calculate the current in amps equivalent to its data
*     numbers and so estimate the responsivity in Amps/Watt for each step in
*     power. Assume the response for a good bolometer should be nearly linear,
*     and so analyse the set of responsivities to determine the mean and
*     evaluate the quality. Write the results to the supplied smfData.

*  Returned Value:
*     size_t = number of good responsivities.

*  Notes:
*     - powval and bolval are calculated by smf_flat_standardpow.
*     - "raw2current" converts raw (10 or 20 kHz) data numbers to
*       current through a TES
*     - The MCE firmware low-pass filters the raw data and subsamples
*       down to 200 Hz. This filtering is the reason for the "mcepass"
*       factor included in raw2current returned by smf_raw2current.
*     - The bolometers are nominally supposed to have a responsivity
*       of -1.0e6 Amps/Watt.
*     - Does not attempt to "pre-condition" TABLE data. Assumption
*       is that CALCFLAT has already done this.

*  Authors:
*     BDK: Dennis Kelly (UKATC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     COBA: Coskun Oba (UoL)
*     EC: Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2007-08-31 (BDK):
*        Original version
*     2007-09-04 (BDK):
*        Adjust conversion factor to amps.
*     2008-08-27 (TIMJ):
*        Rewrite for SMURF from sc2flat.c
*     2008-09-03 (TIMJ):
*        Use GSL to fit a gradient directly rather than trying to calculate
*        stats on the differences.
*     2009-10-08 (TIMJ):
*        WSH requests that responsivities are returned as positive numbers.
*        Add SNR argument.
*     2010-01-28 (TIMJ):
*        Switch to a smfData API
*     2010-01-29 (TIMJ):
*        Use shared 1d polynomial fitting routine.
*     2010-02-02 (TIMJ):
*        Add poly smfData and support cubic fits.
*     2010-02-03 (TIMJ):
*        Add method argument and handle POLYNOMIAL flatfield solutions.
*     2010-03-05 (TIMJ):
*        Change type of method to an enum
*     2010-09-17 (COBA):
*        Updated smf_construct_smfData which now contains smfFts
*     2011-04-15 (TIMJ):
*        Lower MINRESP by an order of magnitude.
*     2011-06-08 (EC):
*        Add raw2current to API
*     2011-06-09 (EC):
*        Don't need raw2current because the respmap header has what we need
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2011 Science and Technology Facilities Council.
*     Copyright (C) 2011 University of British Columbia
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

size_t smf_flat_responsivity ( smf_flatmeth method, smfData *respmap, double snrmin,
                               size_t order, const smfData * powvald, const smfData * bolvald,
                               smfData ** polyfit, int *status ) {

  size_t bol;                  /* Bolometer offset into array */
  double * bolv = NULL;        /* Temp space for bol values */
  double * bolvv = NULL;       /* Temp space for bol variance values */
  double * coeffs = NULL;      /* Polynomial coefficients of 1d fit */
  size_t * goodidx = NULL;     /* Indices of good measurements */
  int istable = 0;             /* Is this table mode? */
  size_t k;                    /* loop counter */
  size_t nbol;                 /* number of bolometers */
  size_t nheat;                /* number of heater measurements */
  size_t ngood = 0;            /* number of valid responsivities */
  size_t nrgood = 0;           /* number of good responsivities for bolo */
  double *poly = NULL;         /* polynomial expansion of each fit */
  double *polybol = NULL;      /* polynomial expansion for all bolometers */
  double *powv = NULL;         /* Temp space for power values */
  double raw2current=VAL__BADD;/* Conversion from DAC --> current units */
  double *respdata = NULL;     /* responsivity data */
  double *respvar = NULL;      /* responsivity variance */
  double * varcoeffs = NULL;   /* variance in polynomial coefficients of 1d fit */

  double * powval = NULL;      /* pointer to data in smfData */
  double * bolval = NULL;      /* pointer to data in smfData */
  double * bolvalvar = NULL;   /* pointer to variance in smfData bolvald */

  const int usevar = 1;
  const double MINRESP = 0.1e6;/* Minimum "in specification" responsivity A/W */
  const double MAXRESP = 5.0e8;/* Maximum "in specification" responsivity A/W */

  if (*status != SAI__OK) return ngood;

  if (!smf_dtype_check_fatal(respmap, NULL, SMF__DOUBLE, status)) return ngood;
  if (!smf_dtype_check_fatal(powvald, NULL, SMF__DOUBLE, status)) return ngood;
  if (!smf_dtype_check_fatal(bolvald, NULL, SMF__DOUBLE, status)) return ngood;

  /* Calculate a boolean for table mode */
  istable = ( method == SMF__FLATMETH_TABLE ? 1 : 0 );

  /* Extract relevant information from the smfData */
  respdata = (respmap->pntr)[0];
  respvar  = (respmap->pntr)[1];
  raw2current = smf_raw2current( respmap->hdr, status );

  powval = (powvald->pntr)[0];
  bolval = (bolvald->pntr)[0];
  bolvalvar = (bolvald->pntr)[1];

  nheat = (powvald->dims)[0];
  nbol = (respmap->dims)[0] * (respmap->dims)[1];

  /* The flatfield mode makes a difference here. If we are fitting
     in TABLE mode then we fit and expand the polynomial and take
     the gradient. If we are in polynomial mode the polynomial
     is actually the inverse of the polynomial we would be generating
     in TABLE mode. Simplest to have two separate blocks. */

  if (istable) {

    /* Space for fit data */
    bolv = astMalloc( nheat*sizeof(*bolv) );
    if (usevar && bolvalvar) bolvv = astMalloc( nheat*sizeof(*bolvv) );
    powv = astMalloc( nheat*sizeof(*powv) );

    /* Polynomial expansion */
    if (polyfit) polybol = astMalloc( (nheat*nbol)*sizeof(*polybol) );
    poly = astMalloc( nheat*sizeof(*poly) );

    /* prefil polynomial with bad */
    if (polybol) {
      for (k=0; k < nheat*nbol; k++) {
        polybol[k] = VAL__BADD;
      }
    }

    /* some memory for good indices */
    goodidx = astCalloc( nheat, sizeof(*goodidx) );

    /* coefficients */
    coeffs = astCalloc( order+1, sizeof(*coeffs) );
    varcoeffs = astCalloc( order+1, sizeof(*varcoeffs) );

    /* dim1 must change slower than dim0 */
    for (bol=0; bol < nbol; bol++) {

      /* perform a fit - responsivity is the gradient */
      nrgood = 0;
      for (k = 0; k < nheat; k++) {
        if ( bolval[k*nbol+bol] != VAL__BADD &&
             powval[k] != VAL__BADD) {
          bolv[nrgood] = raw2current * bolval[k*nbol+bol];
          powv[nrgood] = powval[k];
          if (bolvv) {
            if  (bolvalvar[k*nbol+bol] != VAL__BADD) {
              bolvv[nrgood] = bolvalvar[k*nbol+bol] * pow(raw2current,2);
            } else {
              bolvv[nrgood] = VAL__BADD;
            }
          }
          goodidx[nrgood] = k;
          nrgood++;
        }
      }

      if (nrgood > 3) {
        double resp = 0.0;
        double varresp = 0.0;
        double snr = VAL__BADD;
        size_t nused;

        /* Now fit a polynomial */
        smf_fit_poly1d( order, nrgood, 5.0, powv, bolv, bolvv, NULL, coeffs,
                        varcoeffs, poly, &nused, status );

        /* we take the responsivity to be the gradient at the middle heater setting */
        for (k=1; k<order+1; k++) {
          /* standard differential of a polynomial */
          double xterm = k * pow( powval[nheat/2], k-1 );
          if (coeffs[k] == VAL__BADD) {
            resp = VAL__BADD;
            if (varcoeffs) varresp = VAL__BADD;
            break;
          }
          resp += coeffs[k] * xterm;
          if (varcoeffs) varresp += pow(xterm, 2) * varcoeffs[k];
        }

        /* Wayne wants a positive responsivity. Dennis wants it to be
           properly negative. */
        if (resp != VAL__BADD) resp = fabs( resp );

        /* Calculate the signal to noise ratio. */
        if (resp != VAL__BADD && varresp != VAL__BADD) snr = resp / sqrt( varresp );

        /* Nominal responsivity is -1.0e6 but we allow a bigger range
           through */
        if ( resp == VAL__BADD || resp > MAXRESP || resp < MINRESP
             || snr < snrmin ) {
          respdata[bol] = VAL__BADD;
          if (respvar) respvar[bol] = VAL__BADD;
        } else {
          respdata[bol] = resp;
          if (respvar) respvar[bol] = varresp;
          ngood++;
        }

        /* copy out the polynomial expansion */
        if (polybol) {
          for (k=0; k<nrgood; k++) {
            size_t idx = goodidx[k];
            polybol[idx*nbol+bol] = poly[k] / (raw2current);
          }
        }

      } else {
        respdata[bol] = VAL__BADD;
        if (respvar) respvar[bol] = VAL__BADD;

        if (polybol) {
          for (k=0; k<nheat; k++) {
            polybol[k*nbol+bol] = VAL__BADD;
          }
        }

      }

    }
  } else {
    /* Polynomial fit of  POWER = f( DAC units ) so we calculate the gradient
     for the reference value (stored in coefficient [1]) and reciprocate it. We do
     not expand the polynomial in this branch. */
    size_t ncoeffs = (bolvald->dims)[2];
    size_t coffset = 2;

    for (bol=0; bol < nbol; bol++) {

      if ( bolval[1*nbol+bol] != VAL__BADD ) {
        double refbol  = bolval[1*nbol+bol];
        double resp = 0.0;

        /* need the gradient at x=refbol */
        for (k=1; k<ncoeffs-coffset; k++) {
          /* standard differential of a polynomial:
             grad = c[1] x^0 + 2 c[2] x^1 + 3 c[3] x^3
           */
          double xterm = k * pow( refbol, k-1 );
          resp += bolval[(k+coffset)*nbol+bol] * xterm;
        }

        /* need to invert and take the absolute value */
        resp = 1.0 / fabs(resp);

        /* That gradient is DAC/W and we want A/W */
        resp *= raw2current;

        /* can not do a signal-to-noise clip */
        if ( resp > MAXRESP || resp < MINRESP ) {
          respdata[bol] = VAL__BADD;
          if (respvar) respvar[bol] = VAL__BADD;
        } else {
          respdata[bol] = resp;
          if (respvar) respvar[bol] = 0.0;
          ngood++;
        }

      } else {

        respdata[bol] = VAL__BADD;
        if (respvar) respvar[bol] = VAL__BADD;

      }

    }

  }

  if (polybol) {
    if (polyfit && istable) {
      void *pntr[2];
      pntr[0] = polybol;
      pntr[1] = NULL;
      *polyfit = smf_construct_smfData( NULL, NULL, NULL, NULL, NULL, SMF__DOUBLE,
                                        pntr, NULL, SMF__QFAM_TSERIES, NULL, 1,
                                        bolvald->dims, bolvald->lbnd, 3, 0, 0, NULL,
                                        NULL, status );
      if (*status != SAI__OK && ! *polyfit) astFree( polybol );
    } else {
      astFree( polybol );
    }
  }

  if (goodidx) astFree( goodidx );
  if (coeffs) astFree( coeffs );
  if (varcoeffs) astFree( varcoeffs );
  if (poly) astFree( poly );
  if (bolv) astFree( bolv );
  if (bolvv) astFree( bolvv );
  if (powv) astFree( powv );

  if (*status != SAI__OK) {
    if (*polyfit) smf_close_file( polyfit, status );
  }

  return ngood;
}
