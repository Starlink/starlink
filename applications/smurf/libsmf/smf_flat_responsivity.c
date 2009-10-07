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
*     size_t smf_flat_responsivity ( smfData *respmap, size_t nheat,
*                                  const double powval[], const double bolval[],
*                                  int *status );

*  Arguments:
*     respmap = smfData * (Given & Returned)
*        smfData to receive the responsivity map. Must be _DOUBLE.
*        Will be assumed to be the correct size as compared to bolval.
*     nheat = size_t (Given)
*        Number of measurements. 3rd dimension of bolval. Size of powval.
*     powval = const double [] (Given)
*        Resistance input powers. Must be nheat elements.
*     bolval = const double [] (Given)
*        Response of each bolometer to powval. Dimensioned as number of
*        number of bolometers (size of respmap) time nheat.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     For each bolometer calculate the current in amps equivalent to its data
*     numbers and so estimate the responsivity in Amps/Watt for each step in
*     power. Assume the response for a good bolometer should be nearly linear,
*     and so analyse the set of responsivities to determine the mean and
*     evaluate the quality. Write the results to a text file.

*  Returned Value:
*     size_t = number of good responsivities.

*  Notes:
*     - powval and bolval are calculated by smf_flat_standardpow.
*     - "raw2current" converts raw (10 or 20 kHz) data numbers to
*       current through a TES
*     - The MCE firmware low-pass filters the raw data and subsamples
*       down to 200 Hz. This filtering is the reason for the "mcepass"
*       factor.
*     - The bolometers are nominally supposed to have a responsivity
*       of -1.0e6 Amps/Watt.

*  Authors:
*     BDK: Dennis Kelly (UKATC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
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
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2008 Science and Technology Facilities Council.
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

size_t smf_flat_responsivity ( smfData *respmap, size_t nheat,
                               const double powval[], const double bolval[],
                               int *status ) {

  size_t bol;                  /* Bolometer offset into array */
  double * bolv = NULL;        /* Temp space for bol values */
  size_t k;                    /* loop counter */
  size_t nbol;                 /* number of bolometers */
  size_t ngood = 0;            /* number of valid responsivities */
  int nrgood;                  /* number of good responsivities for bolo */
  double *powv = NULL;         /* Temp space for power values */
  double *respdata = NULL;     /* responsivity data */
  double *resps = NULL;        /* responsivities for a bolometer at each step */
  double *respvar = NULL;      /* responsivity variance */

  if (*status != SAI__OK) return ngood;

  if (!smf_dtype_check_fatal(respmap, NULL, SMF__DOUBLE, status)) return ngood;

  respdata = (respmap->pntr)[0];
  respvar  = (respmap->pntr)[1];

  /* Responsivities */
  resps = smf_malloc( nheat, sizeof(*resps), 0, status );

  /* Space for fit data */
  bolv = smf_malloc( nheat, sizeof(*bolv), 0, status );
  powv = smf_malloc( nheat, sizeof(*powv), 0, status );

  nbol = (respmap->dims)[0] * (respmap->dims)[1];

  /* dim1 must change slower than dim0 */
  for (bol=0; bol < nbol; bol++) {

    /* perform a fit - responsivity is the gradient */
    nrgood = 0;
    for (k = 0; k < nheat; k++) {
      if ( bolval[k*nbol+bol] != VAL__BADD &&
           powval[k] != VAL__BADD) {
        bolv[k] = RAW2CURRENT * bolval[k*nbol+bol];
        powv[k] = powval[k];
        nrgood++;
      }
    }

    if (nrgood > 3) {
      double c0, c1, cov00, cov01, cov11, sumsq;
      double snr;

      /* we have not propagated variance from standardpow so we can not
         weight the fit */
      gsl_fit_linear( powv, 1, bolv, 1, nrgood, &c0, &c1, &cov00, &cov01,
                      &cov11, &sumsq);

      snr = fabs(c1) / sqrt( cov11 );

      /* Nominal responsivity is -1.0e6 but we allow a bigger range
         through */
      if ( fabs(c1) > 5.0e6 || fabs(c1) < 0.1e6 || snr < 25.0 ) {
        respdata[bol] = VAL__BADD;
        respvar[bol] = VAL__BADD;
      } else {
        respdata[bol] = c1;
        respvar[bol] = cov11;
        ngood++;
      }

    } else {
      respdata[bol] = VAL__BADD;
      respvar[bol] = VAL__BADD;
    }

  }

  if (resps) smf_free( resps, status );
  if (bolv) smf_free( bolv, status );
  if (powv) smf_free( powv, status );

  return ngood;
}
