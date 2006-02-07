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
*     smf_scale_tau( double tauwvm, char *filter, int *status) {

*  Arguments:
*     tauwvm = double (Given)
*        WVM tau
*     filter = char* (Given)
*        FILTER keyword from FITS header
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine returns the optical depth for the given SCUBA-2
*     filter based on the tau from the WVM. Currently the SCUBA
*     scaling numbers are used assuming that the WVM tau is actually
*     the 225 GHz zenith optical depth. 

*  Notes:

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-01-10 (AGG):
*        Initial test version
*     2006-01-24 (AGG):
*        Change floats to doubles
*     2006-02-03 (AGG):
*        API change: filter is now a string
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
#include <string.h>

#include "smf.h"
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"
#include "smurf_par.h"
#include "smurf_typ.h"

double smf_scale_tau( const double tauwvm, const char *filter, int *status ) {

  double tau;
  double a = 0;
  double b = 0;

  /* Note these are tau_CSO to SCUBA tau_filter conversions.... */
  /* Also note that we are assuming that the filter names start with
     850 or 450. This may or may not be true... */
  if ( strncmp( filter, "850", 3) == 0 ) {
    /* Long wave */
    a = 4.02;
    b = 0.001;
  } else if ( strncmp( filter, "450", 3) == 0 ) {
    /* Short wave */
    a = 26.2;
    b = 0.014;
  } else {
    /* Error.... */
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSetc("FILT", filter);
      errRep("", "Unknown filter, ^FILT", status);
    }
  }

  tau = a * ( tauwvm - b );
  
  return tau;
}
