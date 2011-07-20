/*
*+
*  Name:
*     smf_wvm225_to_cso225

*  Purpose:
*     Correct the WVM 225 GHz calibration to CSO scale

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     cso = smf_wvm225_to_cso225( double wvm225, int * status );

*  Arguments:
*     wvm225 = double (Given)
*        Uncorrected WVM 225GHz tau.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     csotau = double
*        225 GHz tau on CSO scale.

*  Description:
*     This routine returns the 225 GHz tau on the CSO scale based given
*     a mis-calibrated WVM 225 GHz opacity that would have been calculated
*     using pwv2tau().

*  Notes:
*     - Returns a value of VAL__BADD if status is set bad on entry or if the
*       supplied tau is VAL__BADD.
*     - The correction factor may well turn out to be date dependent.
*     - This routine may disappear if the pwv2tau code is fixed.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2011-07-20 (TIMJ):
*        Initial version. Extracted from smf_calc_wvm
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
#include "prm_par.h"

/* SMURF includes */
#include "smf.h"

double smf_wvm225_to_cso225( double wvm225, int * status ) {
  double csotau;

  if (*status != SAI__OK) return VAL__BADD;
  if (wvm225 == VAL__BADD) return VAL__BADD;

  /* Corrections from RPT on July 19th 2011 */
  if (wvm225 <= 0.1) {
    csotau = wvm225 - (0.2857 * wvm225) + 0.0111;
  } else {
    csotau = wvm225 - 0.0175;
  }

  return csotau;
}
