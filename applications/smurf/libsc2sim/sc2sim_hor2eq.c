/*
*+
*  Name:
*     sc2sim_hor2eq.c

*  Purpose:
*     Get telescope position and orientation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2sim_hor2eq ( double az, double el, double lst, double *ra, 
*                     double *dec, int *status )

*  Arguments:
*     az = double (Given)
*        Azimuth in radians
*     el = double (Given)
*        Elevation in radians
*     lst = double (Given)
*        Local sidereal time in radians
*     ra = double* (Returned)
*        Right Ascension in radians
*     dec = double* (Returned)
*        Declination in radians
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     Use slalib algorithms to get from horizontal to equatorial coordinates.

*  Authors:
*     E.Chapin (UBC)
*     {enter_new_authors_here}

*  History :
*     2006-01-10 (EC):
*        Original
*     2006-07-21 (JB):
*        Split from dsim.c
*     2006-07-08 (EC)
*        Replace cut-and-pasted slaDh2e with library call 

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

/* Starlink includes */
#include "star/slalib.h"

/* SC2SIM includes */
#include "sc2sim.h"

void sc2sim_hor2eq
( 
double az,          /* Azimuth in radians (given) */
double el,          /* Elevation in radians (given) */
double lst,         /* local sidereal time in radians (given) */
double *ra,         /* Right Ascension in radians (returned) */
double *dec,        /* Declination in radians (returned) */
int *status         /* global status (given and returned) */
)

{
   /* Local variables */
   double phi, ha;

   /* Check status */
   if ( !StatusOkP(status) ) return;

   /* JCMT is 19:49:33 N */
   phi = ( 19.0 + (49.0/60.0) + (33.0/3600.0) ) / AST__DR2D;

   slaDh2e( az, el, phi, &ha, dec );

   *ra = lst - ha;

}//sc2sim_hor2eq
