/*
 *+
 *  Name:
 *     sc2sim_response

 *  Purpose:
 *     Return coefficients of average bolometer response

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_response ( double lambda, int ncoeffs, double coeffs[],
 *                       int *status )

 *  Arguments:
 *     lambda = double (Given)
 *        Wavelength in metres
 *     ncoeffs = int (Given)
 *        Number of coefficients to be returned
 *     coeffs = double[] (Returned)
 *        Array t ohold returned coefficients
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Fill the given array with a list of polynomial coefficients
 *     representing the bolometer response curve.
 *     The coefficients represent a numerical model of the bolometer
 *     response by Damian Audley which he fitted to a 5th order polynomial
 *     so that
 *     I = a0 + a1*P +a2*P^2 + a3*P^3 + a4*P^4 + a5*P^5
 *     where I is the current in amps and P is the input power in picowatts.
 *
 *     This is valid for powers between 0 and 50 pW.

 *  Authors:
 *     B.D.Kelly (ROE)
 *     {enter_new_authors_here}

 *  History :
 *     2001-07-19 (BDK):
 *        Original
 *     2002-08-21 (BDK)
 *        C version
 *     2005-02-16 (BDK)
 *        Allow choice of wavelength
 *     2005-04-15 (BDK)
 *        Put in coefficients for 450
 *     2006-07-21 (JB):
 *        Split from dsim.c

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

/* SC2SIM includes */
#include "sc2sim.h"

void sc2sim_response
(
 double lambda,     /* wavelength in metres (given) */
 int ncoeffs __attribute__((unused)), /* number of coefficients to be returned (given) */
 double coeffs[],   /* array to hold returned coefficients (returned) */
 int *status        /* global status (given and returned) */
 )

{
  /* Check status */
  if ( !StatusOkP(status) ) return;

  /* Original 850 values used for DREAM simulations */
  coeffs[0] =  5.88628682e-05;
  coeffs[1] = -2.01609623e-06;
  coeffs[2] = 9.42948262e-08;
  coeffs[3] = -6.2155352e-09;
  coeffs[4] = 1.60394822e-10;
  coeffs[5] = -1.32057874e-12;

  /* 850 Values provided by Damian in Nov 2001 but not added until Feb 2005 */
  if ( lambda > 0.8e-3 ) {

    coeffs[0] =   7.12511028E-05;
    coeffs[1] =  -2.15770388E-06;
    coeffs[2] =  -8.91768082E-08;
    coeffs[3] =   1.04025402E-08;
    coeffs[4] =  -4.69516093E-10;
    coeffs[5] =   7.43680464E-12;

  } else {

    /* 450 values provided by Damian in April 2005 */
    coeffs[0] =   1.19403481E-04;
    coeffs[1] =  -6.28742218E-07;
    coeffs[2] =   2.92078918E-09;
    coeffs[3] =  -2.06985387E-11;
    coeffs[4] =   7.09562826E-14;
    coeffs[5] =  -7.57326701E-17;

  }

}
