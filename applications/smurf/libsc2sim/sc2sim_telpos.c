/*
*+
*  Name:
*     sc2sim_telpos

*  Purpose:
*     Get telescope position and orientation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2sim_telpos ( double ra, double dec, double lst, double *az, 
*                     double *el, double *p, int *status )

*  Arguments:
*     ra = double (Given)
*        Right Ascension in radians
*     dec = double (Given)
*        Declination in radians
*     lst = double (Given)
*        Local sidereal time in radians
*     az = double* (Returned)
*        Azimuth in radians
*     el = double* (Returned)
*        Elevation in radians
*     p = double* (Returned)
*        Parallactic angle in radians
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     Use slalib algorithms to get from equatorial to horizontal coordinates.

*  Authors:
*     B.D.Kelly (ROE)
*     {enter_new_authors_here}

*  History :
*     2005-05-12 (BDK):
*        Original
*     2006-07-21 (JB):
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

/* Starlink includes */
#include "ast.h"

/* SC2SIM includes */
#include "sc2sim.h"

void sc2sim_telpos
( 
double ra,           /* Right Ascension in radians (given) */
double dec,          /* Declination in radians (given) */
double lst,          /* local sidereal time in radians (given) */
double *az,          /* Azimuth in radians (returned) */
double *el,          /* Elevation in radians (returned) */
double *p,           /* Parallactic angle in radians (returned) */
int *status          /* global status (given and returned) */
)

{
   /* Local variables */
   double cosp;      /* intermediate result */
   double phi;       /* latitude of telescope in radians */
   double ha;        /* hour angle in radians */
   double r;         /* intermediate result */
   double sinp;      /* intermediate result */
   double x;         /* cartesian coordinates */
   double y;         /* cartesian coordinates */
   double z;         /* cartesian coordinates */
  
    /* Check status */
   if ( !StatusOkP(status) ) return;

   /* JCMT is 19:49:33 N */
   phi = ( 19.0 + (49.0/60.0) + (33.0/3600.0) ) / AST__DR2D;
   ha = lst - ra;

   /* Equivalent to slaDe2h ( ha, dec, phi, az, el );  */

   /* Az,El as x,y,z */
   x = - cos(ha) * cos(dec) * sin(phi) + sin(dec) * cos(phi);
   y = - sin(ha) * cos(dec);
   z = cos(ha) * cos(dec) * cos(phi) + sin(dec) * sin(phi);

   /* To spherical */
   r = sqrt ( x*x + y*y );

   if ( r < 1.0e-20 ) {
      *az = 0.0;
   } else {
      *az = atan2 ( y, x );
   }//if-else

   if ( *az < 0.0 ) {
      *az += 2.0 * AST__DPI;
   }//if

   *el = atan2 ( z, r );

   /* *p = slaPa ( ha, dec, phi ); */

   sinp = cos ( phi ) * sin ( ha );
   cosp = sin ( phi ) * cos ( dec) - cos ( phi ) * sin ( dec) * cos ( ha );

   if ( sinp != 0.0 || cosp != 0.0 ) {
      *p = atan2 ( sinp, cosp );
   } else {
      *p = 0.0;
   }//if-else

}//sc2sim_telpos
