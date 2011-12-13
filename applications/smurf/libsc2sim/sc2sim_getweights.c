/*
 *+
 *  Name:
 *     sc2sim_getweights.c

 *  Purpose:
 *     Return weights for smoothing by impulse response

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_getweights ( double decay, double samptime, int nweights,
 double weights[], int *status )

 *  Arguments:
 *     decay = double (Given)
 *        Time constant in millisec
 *     samptime = double (Given)
 *        Sampling time in millisec
 *     nweights = int (Given)
 *        Number of values to be returned
 *     weights = double[] (Returned)
 *        Array to hold returned values
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     A set of values suitable to act as a colvolving kernel to
 *     match the impulse response is generated.

 *  Authors:
 *     B.D.Kelly (ROE)
 *     {enter_new_authors_here}

 *  History :
 *     2001-08-21 (BDK):
 *        Original
 *     2002-08-20 (BDK)
 *        C version
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

void sc2sim_getweights
(
 double decay,      /* time constant in millisec (given) */
 double samptime,   /* sampling time in millisec (given) */
 int nweights,      /* number of values to be returned (given) */
 double weights[],  /* array to hold returned values (returned) */
 int *status        /* global status (given and returned) */
 )

{
  /* Local variables */
  int j;
  double pos;
  double sum;

  /* Check status */
  if ( !StatusOkP(status) ) return;

  sum = 0.0;
  for ( j=0; j<nweights; j++ ) {
    pos = ( samptime * (double) ( nweights - j - 1 ) ) / decay;
    weights[j] = 1.0 / exp(pos);
    sum = sum + weights[j];
  }

  for ( j=0; j<nweights; j++ ) {
    weights[j] = weights[j] / sum;
  }

}
