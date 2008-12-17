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
*     hdr = const smfHead* (Given)
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
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-02-03 (AGG):
*        Initial test version
*     2006-07-26 (TIMJ):
*        sc2head no longer used. Use JCMTState instead.
*     2008-04-29 (AGG):
*        Code tidy, add status check
*     2008-12-16 (TIMJ):
*        - Ambient temperature should be in kelvin
*        - PWV should be converted to zenith value before calculating tau.
*        - use smf_cso2filt_tau
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council.  Copyright (C) 2006-2008 University of British
*     Columbia.  All Rights Reserved.

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

  /* Local variables */
  double airmass;           /* Airmass of current observation */
  float pwv;                /* Precipitable water vapour in mm */
  const JCMTState *state = NULL; /* STATE struct containing TCS info */
  double tau;               /* Zenith tau at current wavelength */
  double tamb;              /* Effective ambient temperature */
  float tau0;               /* Optical depth along line of sight */
  double tau225 = 0.0;      /* 225 GHz zenith optical depth */
  float twater;             /* Effective temperature of water vapour */
  float wvm[3];             /* WVM temperature in the 3 channels */

  /* Routine */
  if ( *status != SAI__OK) return VAL__BADD;

  /* Store TCS info */
  state = hdr->state;

  /* Retrieve WVM brightness temperatures and the airmass from the
     header */
  wvm[0] = state->wvm_t12;
  wvm[1] = state->wvm_t42;
  wvm[2] = state->wvm_t78;
  airmass = state->tcs_airmass;

  /* Retrieve the ambient temperature and convert to kelvin */
  /* FUTURE: interpolate to current timeslice */
  smf_fits_getD( hdr, "ATSTART", &tamb, status );
  tamb += 273.15;

  if ( *status == SAI__OK ) {

    /* Get the pwv for this airmass */
    wvmOpt( (float)airmass, (float)tamb, wvm, &pwv, &tau0, &twater);
    
    /* Convert to zenith pwv */
    pwv /= airmass;

    /* convert zenith pwv to zenith tau */
    tau225 = pwv2tau( airmass, pwv );
  }

  /* Scale from CSO to filter tau */
  tau = smf_cso2filt_tau( hdr, tau225, status );

  /*  printf("A = %g tau225 = %g tau = %g\n",airmass,tau225,tau);*/

  return tau;
}
