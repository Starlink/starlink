/*
 *+
 *  Name:
 *     sc2sim_calctrans

 *  Purpose:
 *     Calculate sky zenith % atmospheric transmission

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_calctrans ( double lambda, double *trans, double tauCSO,
 *                        int *status )

 *  Arguments:
 *     lambda = double (Given)
 *        Wavelength in metres
 *     trans = double* (Returned)
 *        % Atmospheric transmission
 *     tauCSO = double (Given)
 *        CSO optical depth
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Calculate the atmospheric transmission equivalent to the given
 *     optical depth for this wavelength band.  This is essentially the
 *     opposite of dsim_calctau.


 *  Authors:
 *     J.Balfour (UBC)
 *     {enter_new_authors_here}

 *  History :
 *     2006-06-19 (JB):
 *        Original
 *     2006-07-20 (JB):
 *        Split from dsim.c
 *     2007-07-13 (AGG):
 *        Trap cases when tauCSO is out of range. Shouldn't happen of
 *        course...

 *  Copyright:
 *     Copyright (C) 2005-2007 Particle Physics and Astronomy Research
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

void sc2sim_calctrans
(
 double lambda,        /* wavelength in metres (given) */
 double *trans,        /* % atmospheric transmission (returned) */
 double tauCSO,        /* CSO optical depth (given) */
 int *status           /* global status (given and returned) */
 )

{
  /* Check status */
  if ( !StatusOkP(status) ) return;

  if ( fabs ( lambda - 0.45e-3 ) < 0.1e-3 ) {
    if ( tauCSO > 0.014) {
      *trans = 100.0 / exp ( 26.2 * ( tauCSO - 0.014 ) );
    } else {
      *trans = 100.0;
    }
  } else {
    if ( tauCSO > 0.001 ) {
      *trans = 100 / exp ( 4.02 * ( tauCSO - 0.001 ) );
    } else {
      *trans = 100.0;
    }
  }
}

