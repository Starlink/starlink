/*
 *+
 *  Name:
 *     sc2sim_digitise

 *  Purpose:
 *     Convert array of currents to integers

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_digitise ( int nvals, double current[], double digmain,
 *                       double digscale, double digcurrent,
 *                       int digits[], int *status )

 *  Arguments:
 *     nvals = int (Given)
 *        Number of values
 *     current = double[] (Given)
 *        Signal values in amps
 *     digmean = double (Given)
 *        Mean digitised level
 *     digscale = double (Given)
 *        Digitisation scale factor
 *     digcurrent - double (Given)
 *        Current in amps at digmean
 *     digits = int[] (Returned)
 *        Digitised currents
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
 *     B.D.Kelly (bdk@roe.ac.uk)
 *     {enter_new_authors_here}

 *  History :
 *     2004-09-23 (BDK):
 *        Original
 *     2005-02-18 (BDK):
 *        Don't allow negative values
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

/* SC2SIM includes */
#include "sc2sim.h"

void sc2sim_digitise
(
 int nvals,            /* number of values (given) */
 double current[],     /* signal values in amps (given) */
 double digmean,       /* mean digitised level (given) */
 double digscale,      /* digitisation scale factor (given) */
 double digcurrent,    /* current in amps at digmean (given) */
 int digits[],         /* digitised currents (returned) */
 int *status           /* global status (given and returned) */
 )

{
  /* Local variables */
  int j;                     /* loop counter */

  /* Check status */
  if ( !StatusOkP(status) ) return;

  for ( j=0; j<nvals; j++ ) {
    digits[j] = (int) ( 0.5 + ( current[j] - digcurrent ) * digscale +
                        digmean );
    if ( digits[j] < 0 ) digits[j] = 0;
  }

}

