/*
*+
*  Name:
*     smf_find_meantau

*  Purpose:
*     Determine the mean tau reading from the header

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     tau = smf_calc_meantau( const smfHead * hdr, int *status );

*  Arguments:
*     hdr = const smfHead * (Given)
*        Header from which to obtain the FITS information
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function reads the FITS header to determine the mean tau
*     in CSO units.

*  Return Value:
*     tau = double
*        Tau in CSO units.
     
*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-07-28 (TIMJ):
*        Initial version.

*  Notes:
*     - May calculate the mean from two values or may read a single
*       mean header value.
*     - could be expanded in the future to handle scaling from CSO
*       to filter tau.

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

#include <stdio.h>

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "star/one.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_calc_meantau"

double smf_calc_meantau ( const smfHead * hdr, int *status ) {
  double tau1;         /* Tau at start */
  double tau2;         /* Tau at end of file */
  double retval;

  retval = 0.0;
  if (*status != SAI__OK) return retval;

  smf_fits_getD( hdr, "WVMTAUST", &tau1, status );
  smf_fits_getD( hdr, "WVMTAUEN", &tau2, status );
  if (*status == SAI__OK) {
    retval = (tau1 + tau2) / 2.0;
  }
  return retval;
}
