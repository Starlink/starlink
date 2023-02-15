/*
*+
*  Name:
*     smf_cso2filt_tau

*  Purpose:
*     Convert tau referenced to the CSO to a filter tau value

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     double smf_cso2filt_tau( const smfHead *hdr, double csotau,
*                              AstKeyMap * extpars, int * status );

*  Arguments:
*     hdr = const smfHead* (Given)
*        Header struct from data struct
*     csotau = double (Given)
*        225GHz tau value. Will be obtained from the header if it
*        is VAL__BADD.
*     extpars = AstKeyMap * (Given)
*        AST keymap containing the parameters required to convert
*        the tau (on the CSO scale) to the current filter. Must
*        contain the "taurelation" key which itself will be a keymap
*        containing the parameters for the specific filter.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine returns the optical depth for the given SCUBA-2
*     filter based on the supplied CSO tau value.

*  Notes:
*     - Returns a value of VAL__BADD if status is set bad on entry
*     - The tau relation is formulated as:
*          tau_filt = a ( tau_cso + b + c sqrt(tau_cso) )
*       and the keymap should contain an entry named "taurelation_FILT"
*       containing "a", "b" and "c".
*     - The routine will not attempt to guess a tau relation.
*     - Since "b" is usually negative, the calculation is skipped if the
*       CSO tau is zero. The routine returns a tau of 0.0.
*     - If this routine is to be called in a loop, consider calling
*       smf_cso2filt_coeff once outside the loop and then calling smf_cso2filt_applycoeff
*       inside the loop.

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-12-16 (TIMJ):
*        Initial version
*     2010-06-02 (TIMJ):
*        Add external extinction parameters. No longer calls smf_scale_tau.
*     2011-02-18 (AGG):
*        Trap CSO tau of 0 and return 0
*     2013-05-12 (TIMJ):
*        Recast in terms of coefficient retrieval and application.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008,2010,2013 Science and Technology Facilities Council.
*     Copyright (C) 2011 the University of British Columbia.
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

double smf_cso2filt_tau( const smfHead *hdr, double csotau, AstKeyMap * extpars, int *status) {
  double coeffs[3];        /* Tau relation coefficients */
  dim_t nvals;             /* Number of coefficients */
  double tau = VAL__BADD;  /* return filter tau */

  if (*status != SAI__OK) return tau;

  /* do we need to get our own value */
  if (csotau == VAL__BADD) {
    csotau = smf_calc_meantau( hdr, status );
  }

  /* Trap a CSO tau of 0 and just return 0 */
  if (csotau == 0.0) return 0.0;

  /* Get the coefficients */
  smf_cso2filt_coeff( hdr, extpars, 3, coeffs, &nvals, status );

  /* Now apply them */
  tau = smf_cso2filt_applycoeff( csotau, coeffs, status );

  return tau;
}
