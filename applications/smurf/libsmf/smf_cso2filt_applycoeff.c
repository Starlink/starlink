/*
*+
*  Name:
*     smf_cso2filt_applycoeff

*  Purpose:
*     Convert tau to filter scale using suppled coefficients

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     double smf_cso2filt_applycoeff( double csotau, const double coeffs[2],
*                                     int *status );

*  Arguments:
*     csotau = double (Given)
*        225GHz tau value. Returns VAL__BADD if it is bad. Returns zero if it
*        is zero.
*     coeffs = double[2] (Given)
*        Conversion Coefficients obtained by calling smf_cso2filt_coeff().
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine returns the optical depth for the given SCUBA-2
*     filter based on the supplied CSO tau value, using the supplied
*     conversion coefficients.

*  Notes:
*     - Returns a value of VAL__BADD if status is set bad on entry
*     - The tau relation is formulated as:
*          tau_filt = a ( tau_cso + b )
*     - Since "b" is usually negative, the calculation is skipped if the
*       CSO tau is zero. The routine returns a tau of 0.0.
*     - Coefficients should be obtained by calling smf_cso2filt_coeff

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2013-05-12 (TIMJ):
*        Initial version, derived from smf_cso2filt_tau
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

double smf_cso2filt_applycoeff( double csotau, const double coeffs[2],
                                int *status ) {
  double tau = VAL__BADD;  /* return filter tau */

  if (*status != SAI__OK) return tau;

  if (csotau == VAL__BADD) {
    return VAL__BADD;
  }

  tau = coeffs[0] * ( csotau + coeffs[1] );

  return tau;
}
