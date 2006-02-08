/*
*+
*  Name:
*     smf_calc_wvm

*  Purpose:
*     Function to calculate the optical depth from the raw WVM data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_calc_wvm( const smfHead *hdr, int *status) {

*  Arguments:
*     hdr = smfHead* (Given)
*        Header struct from data struct
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine returns the optical depth for the given SCUBA-2
*     filter based on the raw temperature info from the WVM. 

*  Notes:
*     - Returns a value of VAL__BADD if status is set bad on entry
*     - See also smf_scale_tau.c for scaling between filters/wavelengths

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-02-03 (AGG):
*        Initial test version
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

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smurf_typ.h"

/* WVM includes */
#include "wvm/wvmCal.h"
#include "wvm/wvmTau.h"

double smf_calc_wvm( const smfHead *hdr, int *status ) {

  double airmass;           /* Airmass of current observation */
  sc2head *sc2head;         /* sc2head struct containing TCS info */
  double tau;               /* Zenith tau at current wavelength */
  float wvm[3];             /* WVM temperature in the 3 channels */

  float pwv;
  float tau0;
  float twater;

  double tamb;
  double tau225;

  char filter[80];

  if ( *status != SAI__OK) return VAL__BADD;

  /* Store TCS info */
  sc2head = hdr->sc2head;

  /* Retrieve WVM brightness temperatures and the airmass from the
     header */
  wvm[0] = sc2head->wvm_t12;
  wvm[1] = sc2head->wvm_t42;
  wvm[2] = sc2head->wvm_t78;
  airmass = sc2head->tcs_airmass;

  /* Retrieve the ambient temperature */
  /* FUTURE: interpolate to current timeslice */
  smf_fits_getD( hdr, "ATSTART", &tamb, status );
  smf_fits_getS( hdr, "FILTER", filter, 81, status);

  wvmOpt( airmass, tamb, wvm, &pwv, &tau0, &twater);

  tau225 = pwv2tau( airmass, pwv );

  tau = smf_scale_tau( tau225, filter, status);

  /*  printf("tau = %g\n",tau);*/

  return tau;
}
