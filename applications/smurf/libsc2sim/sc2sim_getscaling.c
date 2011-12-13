/*
 *+
 *  Name:
 *     sc2sim_getscaling.c

 *  Purpose:
 *     Get parameters for scaling data to integers

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_getscaling ( int ncoeffs, double coeffs[], double targetpow,
 *                         double photonsigma, double *digmean,
 *                         double *digscale, double *digcurrent, int *status )

 *  Arguments:
 *     ncoeffs = int (Given)
 *        Number of coefficients describing response curve
 *     coeffs = double[] (Given)
 *        Array to hold response curve coefficients
 *     targetpow = double (Given)
 *        Target power level in pW
 *     photonsigma = double (Given)
 *        Photon noise level
 *     digmean = double* (Returned)
 *        Mean digitised level
 *     digscale = double* (Returned)
 *        Digitisation scale factor (Returned)
 *     digcurrent = double* (Returned)
 *        Current in amps at digmean
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     The simulated bolometer current is to be "digitised" using the formula
 *
 *     digval = int ( 0.5 + ( current - digcurrent ) * digscale + digmean )
 *
 *     where the three digitisation parameters are chosen so that the
 *     current corresponding to the target power level falls in the middle
 *     of a 24-bit range, and one digitisation level is one-sixth of the
 *     photon noise dispersion (this assumes the digitisation can encode
 *     instrumental noise, and the latter is about 1/3 of photon noise).

 *  Authors:
 *     B.D.Kelly (ROE)
 *     {enter_new_authors_here}

 *  History :
 *     2004-09-23 (BDK):
 *        Original
 *     2005-06-10 (BDK):
 *        Return fixes values as experiment to guarantee consistent
 *        flat-fielding
 *     2006-07-20 (JB):
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

/* Standard includes */
#include <math.h>

/* SC2SIM includes */
#include "sc2sim.h"

void sc2sim_getscaling
(
 int ncoeffs,          /* number of coefficients describing response curve
                          (given) */
 double coeffs[],      /* array to hold response curve coefficients (given) */
 double targetpow,     /* target power level in pW (given) */
 double photonsigma,   /* photon noise level (given) */
 double *digmean,      /* mean digitised level (returned) */
 double *digscale,     /* digitisation scale factor (returned) */
 double *digcurrent,   /* current in amps at digmean (returned) */
 int *status           /* global status (given and returned) */
 )

{
  /* Local variables */
  double currentsigma;       /* photon noise current equivalent */
  double meanplus;           /* current matching targetpow plus noise */

  /* Check status */
  if ( !StatusOkP(status) ) return;

  sc2sim_ptoi ( targetpow, ncoeffs, coeffs, 0.0, digcurrent, status );

  sc2sim_ptoi ( targetpow+photonsigma, ncoeffs, coeffs, 0.0, &meanplus,
                status );

  currentsigma = fabs ( meanplus - (*digcurrent) );

  *digscale = 6.0 / currentsigma;
  *digmean = (double) 8388608;

  /* replace by fixed values */

  *digcurrent = 1.333301e-05;

  /* experiment
   *digscale = 4.655870e+10;
   */

  *digscale = 4.655870e+11;

}

