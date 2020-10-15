/*
*+
*  Name:
*     smf_cso2filt_coeff

*  Purpose:
*     Obtain coefficients required to convert CSO scale tau to filter tau

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_cso2filt_coeff( const smfHead *hdr, AstKeyMap *extpars,
*                         dim_t nvals, double coeffs[3], dim_t *ncoeff, int * status );

*  Arguments:
*     hdr = const smfHead* (Given)
*        Header struct from data struct
*     extpars = AstKeyMap * (Given)
*        AST keymap containing the parameters required to convert
*        the tau (on the CSO scale) to the current filter. Must
*        contain the "taurelation" key which itself will be a keymap
*        containing the parameters for the specific filter.
*     nvals = dim_t (Given)
*        Number of elements in coeff[] array.
*     coeff = double [3] (Given & Returned)
*        Array to receive coefficients. Should be sized to at least 3 elements.
*     ncoeff = dim_t * (Returned)
*        Number of coefficients stored in coeff[] array.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine returns the coefficients required to convert a tau in
*     CSO scale to the relevant filter scale. Should be used in conjunction
*     with smf_cso2filt_applycoeff if the correction is to be applied in a loop.

*  Notes:
*     - The tau relation is formulated as:
*          tau_filt = a ( tau_cso + b + c sqrt(tau_cso) )
*       and the keymap should contain an entry named "taurelation_FILT"
*       containing "a", "b" and "c".
*     - The routine will not attempt to guess a tau relation.
*     - Use smf_cso2filt_applycoeff to apply the coefficients.
*     - Use smf_cso2filt_tau to retrieve coefficients and apply them in
*       a single call.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2013-05-12 (TIMJ):
*        Initial version derived from smf_cso2filt_tau
*     2021-02-04 (GSB)
*        Increase number of coefficients to 3 and default any unspecified.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2013 Science and Technology Facilities Council.
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
#include "sae_par.h"
#include "prm_par.h"
#include "mers.h"

/* SMURF includes */
#include "smf.h"

static const double default_coeffs[] = {1.0, 0.0, 0.0};
static const size_t n_default_coeffs = sizeof(default_coeffs) / sizeof(*default_coeffs);

void smf_cso2filt_coeff( const smfHead *hdr,  AstKeyMap * extpars,
                         dim_t nvals, double coeffs[], dim_t *ncoeff, int *status) {
  char filter[81];         /* somewhere for the filter name */
  AstKeyMap * taumap = NULL; /* tau relation keymap */

  int ast_nvals;
  int ast_ncoeff;

  *ncoeff = 0;

  if (*status != SAI__OK) return;

  /* Get the filter name */
  smf_fits_getS( hdr, "FILTER", filter, sizeof(filter), status);

  /* The supplied keymap is the "ext" entry from the config file so we first need
     to get the "taurelation" keymap */
  astMapGet0A( extpars, "TAURELATION", &taumap );


  /* Now look for the filter */
  ast_nvals = (int) nvals;
  if (astMapGet1D( taumap, filter, ast_nvals, &ast_ncoeff, coeffs )) {
    *ncoeff = ast_ncoeff;

    if (nvals > n_default_coeffs) nvals = n_default_coeffs;
    for (; nvals > *ncoeff; ++ *ncoeff) coeffs[*ncoeff] = default_coeffs[*ncoeff];

  } else {
    if (*status == SAI__OK) *status = SAI__ERROR;
    errRepf( "", "Unable to obtain tau scaling for filter '%s'",
             status, filter );
  }

  return;
}
