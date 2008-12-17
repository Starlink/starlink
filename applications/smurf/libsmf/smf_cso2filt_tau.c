/*
*+
*  Name:
*     smf_cso2filt_tau

*  Purpose:
*     Convert CSO tau value to filter tau value

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     double smf_cso2filt_tau( const smfHead *hdr, double csotau,
*                              int * status );

*  Arguments:
*     hdr = const smfHead* (Given)
*        Header struct from data struct
*     csotau = double (Given)
*        225GHz tau value. Will be obtained from the header if it
*        is VAL__BADD.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine returns the optical depth for the given SCUBA-2
*     filter based on the supplied CSO tau value.

*  Notes:
*     - Returns a value of VAL__BADD if status is set bad on entry
*     - Use smf_scale_tau() to do the scaling.

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-12-16 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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

/* Starlink includes */
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "smf.h"

double smf_cso2filt_tau( const smfHead *hdr, double csotau, int *status) {
  double tau = VAL__BADD;  /* return filter tau */
  char filter[81];         /* somewhere for the filter name */

  if (*status != SAI__OK) return tau;

  /* do we need to get our own value */
  if (csotau == VAL__BADD) {
    csotau = smf_calc_meantau( hdr, status );
  }

  smf_fits_getS( hdr, "FILTER", filter, sizeof(filter), status);
  tau = smf_scale_tau( csotau, filter, status);

  return tau;
}
