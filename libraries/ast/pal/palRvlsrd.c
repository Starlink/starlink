/*
*+
*  Name:
*     palRvlsrd

*  Purpose:
*     Velocity component in a given direction due to the Sun's motion
*     with respect to the dynamical Local Standard of Rest.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     double palRvlsrd( double r2000, double d2000 )

*  Arguments:
*     r2000 = double (Given)
*        J2000.0 mean RA (radians)
*     d2000 = double (Given)
*        J2000.0 mean Dec (radians)

*  Returned Value:
*     Component of "peculiar" solar motion in direction R2000,D2000 (km/s).

*  Description:
*     This function returns the velocity component in a given direction
*     due to the Sun's motion with respect to the dynamical Local Standard
*     of Rest. The result is +ve when the Sun is receding from the given
*     point on the sky.

*  Notes:
*     - The Local Standard of Rest used here is the "dynamical" LSR,
*     a point in the vicinity of the Sun which is in a circular orbit
*     around the Galactic centre.  The Sun's motion with respect to the
*     dynamical LSR is called the "peculiar" solar motion.
*     - There is another type of LSR, called a "kinematical" LSR.  A
*     kinematical LSR is the mean standard of rest of specified star
*     catalogues or stellar populations, and several slightly different
*     kinematical LSRs are in use.  The Sun's motion with respect to an
*     agreed kinematical LSR is known as the "standard" solar motion.
*     To obtain a radial velocity correction with respect to an adopted
*     kinematical LSR use the routine sla_RVLSRK.

*  Reference:
*     - Delhaye (1965), in "Stars and Stellar Systems", vol 5, p73.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2012-02-16 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.
*     All Rights Reserved.

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
*    You should have received a copy of the GNU General Public License
*    along with this program; if not, write to the Free Software
*    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
*    USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "pal.h"
#include "sofa.h"

double palRvlsrd( double r2000, double d2000 ){

/* Local Variables: */
   double vb[ 3 ];

/*
*  Peculiar solar motion from Delhaye 1965: in Galactic Cartesian
*  coordinates (+9,+12,+7) km/s.  This corresponds to about 16.6 km/s
*  towards Galactic coordinates L2 = 53 deg, B2 = +25 deg, or RA,Dec
*  17 49 58.7 +28 07 04 J2000.
*
*  The solar motion is expressed here in the form of a J2000.0
*  equatorial Cartesian vector:
*
*      VA(1) = X = -SPEED*COS(RA)*COS(DEC)
*      VA(2) = Y = -SPEED*SIN(RA)*COS(DEC)
*      VA(3) = Z = -SPEED*SIN(DEC)
*/

   double va[ 3 ] = { +0.63823, +14.58542, -7.80116 };

/* Convert given J2000 RA,Dec to x,y,z. */
   iauS2c( r2000, d2000, vb );

/* Compute dot product with Solar motion vector. */
   return iauPdp( va, vb );
}
