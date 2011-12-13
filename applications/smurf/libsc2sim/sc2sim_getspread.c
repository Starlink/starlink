/*
 *+
 *  Name:
 *     sc2sim_getspread.c

 *  Purpose:
 *     Return scattered characteristics of bolometers

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_getspread ( int numbols, double pzero[],
 *                        double heater[], int *status )

 *  Arguments:
 *     numbols = int (Given)
 *        Number of bolometers
 *     pzero = double[] (Returned)
 *        Array to hold response curve offsets of
 *        bolometers in pW
 *     heater = double[] (Returned)
 *        Array to hold heater factors of bolometers
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Use scaled random numbers for the response curve offsets.
 *     The heater factors are 1.0 with a 5% random spread, simulating a
 *     spread in the heater resistors of each bolometer.

 *  Authors:
 *     B.D.Kelly (ROE)
 *     {enter_new_authors_here}

 *  History :
 *     2005-05-11 (BDK):
 *        Original
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

void sc2sim_getspread
(
 int numbols,             /* Number of bolometers (given) */
 double pzero[],          /* Array to hold response curve offsets
                             of bolometers in pW (returned) */
 double heater[],         /* Array to hold heater factors of bolometers
                             (returned) */
 int *status              /* global status (given and returned) */
 )

{
  /* Local variables */
  int bol;                          /* bolometer counter */

  /* Check status */
  if ( !StatusOkP(status) ) return;

  /* Produce some power offsets, zero mean, sigma = 1pW */
  for (bol=0; bol<numbols; bol++ ) {
    pzero[bol] = sc2sim_drand ( 1.0 );
  }

  /* Produce some heater input ratios, mean=1.0, sigma=0.05 */
  for (bol=0; bol<numbols; bol++ ) {
    heater[bol] = 1.0 + sc2sim_drand ( 0.05 );
  }

}
