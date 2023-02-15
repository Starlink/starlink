/*
 *+
 *  Name:
 *     sc2sim_ptoi

 *  Purpose:
 *     Convert input pW to bolometer current

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_ptoi ( double flux, int ncoeffs, double coeffs[],
 *                   double pzero, double *current, int *status )

 *  Arguments:
 *     flux = double (Given)
 *        Input flux in pW
 *     ncoeffs = int (Given)
 *        Numbe rof coefficients describing response curve
 *     coeffs = double[] (Given)
 *        Array to hold response curve coefficients
 *     pzero = calibratio noffset in pW
 *        Calibration offset in pW
 *     current = double* (Returned)
 *        Signal from bolometer in amps
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Given a flux in pW, apply the bolometer response curve with the
 *     bolometer-specific shift.
 *     The coefficients represent a numerical model of the bolometer
 *     response by Damian Audley which he fitted to a 5th order polynomial
 *     so that
 *     I = a0 + a1*P +a2*P^2 + a3*P^3 + a4*P^4 + a5*P^5
 *     where I is the current in amps and P is the input power in picowatts.
 *
 *     This is valid for powers between 0 and 50 pW at 850.

 *  Authors:
 *     B.D.Kelly (ROE)
 *     {enter_new_authors_here}

 *  History :
 *     2001-07-26 (BDK):
 *        Original
 *     2002-08-21 (BDK)
 *        C version
 *     2005-08-11 (BDK)
 *        Trap obviously out of range values
 *     2006-07-21 (JB):
 *        Split from dsim.c
 *     2007-07-04 (EC):
 *        Removed hard limits on input power from 0--200 pW
 *     2007-07-11 (EC):
 *        Huge (or tiny) input powers are clipped to the range 0--200 pW

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

void sc2sim_ptoi( double flux, int ncoeffs __attribute__((unused)), double coeffs[],
                  double pzero, double *current, int *status ) {
  /* Local variables */
  double p;

  /* Check status */
  if ( !StatusOkP(status) ) return;

  p = flux + pzero;

  /* clip values that have ridiculous ranges */
  if( p < 0.0 ) p = 0;
  if( p > 200.0 ) p = 200.;

  *current = coeffs[0] + coeffs[1] * p + coeffs[2] * p*p +
    coeffs[3] * p*p*p + coeffs[4] * p*p*p*p + coeffs[5] * p*p*p*p*p;
}
