/*
*+
*  Name:
*     smf_scale_tau

*  Purpose:
*     Optical depth scaling function

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_scale_tau( double tauwvm, int filter, int *status) {

*  Arguments:
*     tauwvm = double (Given)
*        WVM tau
*     filter = int (Given)
*        SUBSYSNR keyword from FITS header (temporary)
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine returns the optical depth for the given SCUBA-2
*     filter based on the tau from the WVM. Currently the SCUBA
*     scaling numbers are used assuming that the WVM tau is actually
*     the 225 GHz zenith optical depth. 

*  Notes:
*     At the moment it uses the SUBSYSNR FITS header keyword to
*     determine the appropriate wavelength. In the future this will be
*     changed to the SUBARRY string.

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-01-10 (AGG):
*        Initial test version
*     2006-01-24 (AGG):
*        Change floats to doubles
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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

#include "smf.h"
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"
#include "smurf_par.h"
#include "smurf_typ.h"

double smf_scale_tau( const double tauwvm, const int filter, int *status ) {

  double tau;
  double a = 0;
  double b = 0;

  /* Note these are tau_CSO to SCUBA tau_filter conversions.... */
  if ( filter >= 1 && filter <= 4 ) {
    /* Long wave */
    a = 4.02;
    b = 0.001;
  } else if ( filter >= 5 && filter <= 8 ) {
    /* Short wave */
    a = 26.2;
    b = 0.014;
  } else {
    /* Error.... */
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSeti("FILT", filter);
      errRep("", "Unknown filter, ^FILT", status);
    }
  }

  tau = a * ( tauwvm - b );
  
  return tau;
}
