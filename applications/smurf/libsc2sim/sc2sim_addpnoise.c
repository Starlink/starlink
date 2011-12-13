/*
 *+
 *  Name:
 *     sc2sim_addpnoise

 *  Purpose:
 *     Calculate photon noise and add to a flux.

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_addpnoise( double flux_0, double sig_0, double integ_time,
 *                       double *flux, int *status ) {

 *  Arguments:
 *     flux_0 = double (Given)
 *        Reference power per pixel in pW
 *     sig_0 = double (Given)
 *        NEP at reference power in pW/sqrt(Hz)
 *     integ_time = double (Given)
 *        Effective integration time in sec
 *     flux = double* (Given and Returned)
 *        Flux value in pW
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Calculate the photon equivalent of the given flux allowing the
 *     corresponding photon noise sigma to be calculated. Use this to
 *     scale a generated random number.

 *  Authors:
 *     B.D.Kelly (ROE)
 *     {enter_new_authors_here}

 *  History :
 *     2001-07-26 (BDK):
 *        Original
 *     2002-08-19 (BDK):
 *        C version
 *     2003-05-22 (BDK):
 *        Multiply photon noise by 1.5
 *     2004-01-23 (BDK):
 *        Use dsim_getsigma
 *     2006-07-19 (JB):
 *        Split from dsim.c
 *     2006-09-22 (JB):
 *        Removed dream.h requirements
 *     2007-06-29 (EC):
 *        Removed physical model for noise and replaced with simple scaling
 *        from reference power/noise

 *  Copyright:
 *     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
 *     Council. University of British Columbia. All Rights Reserved.

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

/* Standard includes */
#include <math.h>

/* SC2SIM includes */
#include "sc2sim.h"

void sc2sim_addpnoise( double flux_0, double sig_0, double integ_time,
                       double *flux, int *status ) {

  double err;                  /* error offset */
  double sigma;                /* NEP in pW */

  /* Check status */
  if ( !StatusOkP(status) ) return;

  /* Calculate the dispersion due to photon noise */
  sc2sim_getsigma( flux_0, sig_0, *flux, &sigma, status );

  /* Calculate a random number and scale it to the required sigma */
  err = sc2sim_drand( sigma );

  /* err is already measured /sqrt(Hz) so no factor of 2 needed */
  *flux = *flux + err/sqrt(integ_time);

}
