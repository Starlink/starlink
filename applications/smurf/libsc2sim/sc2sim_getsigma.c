/*
*+
*  Name:
*     sc2sim_getsigma.c

*  Purpose:
*     Calculate photon noise

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2sim_getsigma ( double lambda, double bandGHx, double aomega, 
*                       double flux, double *sigma, int *status )

*  Arguments:
*     lambda = double (Given)
*        Wavelength in metres
*     bandGHz = double (Given)
*        Bandwidth in GHz
*     aomega = double (Given)
*        Geometrical factor
*     flux = double (Given)
*        Sky power per pixel in pW
*     sigma = double* (Returned)
*        Photon noise in pW
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     Calculate photon noise

*  Authors:
*     {enter_new_authors_here}

*  History :
*     2006-07-20 (JB):
*        Split from dsim.c

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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

/* Standard includes */
#include <math.h>

/* SC2SIM includes */
#include "sc2sim.h"

void sc2sim_getsigma
( 
double lambda,         /* wavelength in metres (given) */
double bandGHz,        /* bandwidth in GHz (given) */
double aomega,         /* geometrical factor (given) */
double flux,           /* sky power per pixel in pW (given) */
double *sigma,         /* photon noise in pW (returned) */
int *status            /* global status (given and returned) */
)

{
   /* Local variables */
   double coherence;   /* spatial coherence of the beam */

   /* Check status */
   if ( !StatusOkP(status) ) return;

   coherence = ( 1.0 + exp ( -aomega ) ) / ( 2.0 + aomega );

   *sigma = 1.0e12 * sqrt ( 2.0 * flux * 1.0e-12 * H * C / lambda 
        + flux * flux * 1.0e-24 * coherence / ( bandGHz * 1.0e9 ) );

}
