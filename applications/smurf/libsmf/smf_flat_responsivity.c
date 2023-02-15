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
*     dim_t smf_flat_responsivity ( smf_flatmeth method, smfData *respmap, double snrmin,
*                                    dim_t order, const smfData * powval, const smfData * bolval,
*                                    double refres, smfData ** polyfit, int *status );

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
*     order = dim_t (Given)
*        If the data are being fitted this is the order of polynomial
*        to use. Ignored if method is POLYNOMIAL.
*     powval = const smfData * (Given)
*        Resistance input powers. Is the number of heater measurements.
*     bolval = const smfData * (Given)
*        Response of each bolometer to powval. Dimensioned as number of
*        number of bolometers (size of respmap) times number of heater
*        measurements (size of powval).
*     refres = double (Given)
*        Reference resistance. Whilst the value is not used directly the
*        heater efficiencies are only corrected if the reference resistance
*        is defined to some value other than VAL__BADD.
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
*     dim_t = number of good responsivities.

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
*     2011-09-02 (TIMJ):
*        Simplify logic with TABLE mode. Now just pass TABLE data to the
*        polynomial fitting flatfield routine and then handle the responsivity
*        calculation in the same place.
*     2011-09-07 (TIMJ):
*        Now reads heater efficiency data directly when calculating responsivity.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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

dim_t smf_flat_responsivity ( smf_flatmeth method, smfData *respmap, double snrmin,
                               dim_t order, const smfData * powvald, const smfData * bolvald,
                               double refres, smfData ** polyfit, int *status ) {

  dim_t bol;                  /* Bolometer offset into array */
  double * bolval = NULL;      /* pointer to data in smfData */
  const dim_t coffset = 2;    /* Offset into POLYNOMIAL for coefficients */
  smfData * heateff = NULL;    /* Heater efficiency smfData */
  double * heateffdata = NULL; /* Pointer to efficiency data */
  dim_t k;                    /* loop counter */
  dim_t nbol;                 /* number of bolometers */
  dim_t ncoeffs = 0;
  dim_t ngood = 0;            /* number of valid responsivities */
  double raw2current=VAL__BADD;/* Conversion from DAC --> current units */
  double *respdata = NULL;     /* responsivity data */
  double *respvar = NULL;      /* responsivity variance */
  smfData * tabbolval = NULL;  /* POLYNOMIAL bolval derived from TABLE data */

  const double MINRESP = 0.1e6;/* Minimum "in specification" responsivity A/W */
  const double MAXRESP = 5.0e8;/* Maximum "in specification" responsivity A/W */

  if (*status != SAI__OK) return ngood;

  if (!smf_dtype_check_fatal(respmap, NULL, SMF__DOUBLE, status)) return ngood;
  if (!smf_dtype_check_fatal(powvald, NULL, SMF__DOUBLE, status)) return ngood;
  if (!smf_dtype_check_fatal(bolvald, NULL, SMF__DOUBLE, status)) return ngood;

  /* Extract relevant information from the smfData */
  respdata = (respmap->pntr)[0];
  respvar  = (respmap->pntr)[1];
  raw2current = smf_raw2current( respmap->hdr, status );

  nbol = (respmap->dims)[0] * (respmap->dims)[1];

  /* The flatfield mode makes a difference here. If we are fitting
     in TABLE mode we first have to fit the data with a polynomial.
     We do that using the same code that we would have used to
     calculate the POLYNOMIAL result. Then we can continue with
     shared code and free the additional polynomial fit results at
     the end. */

  if (method == SMF__FLATMETH_TABLE) {
    /* Generate a polynomial fit of the TABLE data. Note that this
       routine fits current as a function of heater so for order>1
       the polynomial is not inverted. */
    smf_flat_fitpoly( powvald, bolvald, snrmin, order, &tabbolval,
                      polyfit, status );

    bolval = (tabbolval->pntr)[0];
    ncoeffs = (tabbolval->dims)[2];

  } else {

    bolval = (bolvald->pntr)[0];
    ncoeffs = (bolvald->dims)[2];

  }

  /* Get the heater efficiency file if we are going to be using it */
  if (refres != VAL__BADD) {
    smf_flat_params( respmap, "RESIST", NULL, NULL, NULL, NULL, NULL,
                     NULL, NULL, NULL, NULL, NULL, &heateff, status );

    if (heateff) heateffdata = (heateff->pntr)[0];
  }

  /* Polynomial fit of  POWER = f( DAC units ) so we calculate the gradient
     for the reference value (stored in coefficient [1]) and reciprocate it. We do
     not expand the polynomial in this branch. */

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

      /* Correct by the heater efficiency */
      if (heateffdata) resp *= heateffdata[bol];

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

  if (tabbolval) smf_close_file(NULL, &tabbolval, status );
  if (heateff) smf_close_file(NULL, &heateff, status);
  if (*status != SAI__OK) {
    if (polyfit && *polyfit) smf_close_file( NULL, polyfit, status );
  }

  return ngood;
}
