/*
 *+
 *  Name:
 *     sc2sim_getsigma.c

 *  Purpose:
 *     Calculate photon noise (NEP) in pW/sqrt(Hz)

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_getsigma ( double flux_0, double sig_0,
 *                       double flux, double *sig, int *status )

 *  Arguments:
 *     flux_0 = double (Given)
 *        Reference power per pixel in pW
 *     sig_0 = double (Given)
 *        NEP at reference power in pW/sqrt(Hz)
 *     flux = double (Given)
 *        Power per pixel in pW
 *     sig = double* (Returned)
 *        NEP in pW/sqrt(hz)
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Calculate photon noise

 *  Authors:
 *     J.Balfour (UBC)
 *     E.Chapin (UBC)
 *     A.G. Gibb (UBC)
 *     {enter_new_authors_here}

 *  History :
 *     2006-07-20 (JB):
 *        Split from dsim.c
 *     2007-06-29 (EC):
 *        Removed physical model for noise and replaced with simple scaling
 *        from reference power/noise
 *     2008-04-28 (AGG):
 *        Change return type to void
 *     {enter_further_changes_here}

 *  Copyright:
 *     Copyright (C) 2006-2008 University of British Columbia. All
 *     Rights Reserved.

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

void sc2sim_getsigma( double flux_0, double sig_0, double flux, double *sig,
                      int *status ) {
  /* Check status */
  if ( !StatusOkP(status) ) return;

  /* Assume NEP follows Poisson statistics and scale from a reference */
  *sig = sig_0*sqrt( flux/flux_0 );
}
