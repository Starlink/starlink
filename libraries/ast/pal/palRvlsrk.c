/*
*+
*  Name:
*     palRvlsrk

*  Purpose:
*     Velocity component in a given direction due to the Sun's motion
*     with respect to an adopted kinematic Local Standard of Rest.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     double palRvlsrk( double r2000, double d2000 )

*  Arguments:
*     r2000 = double (Given)
*        J2000.0 mean RA (radians)
*     d2000 = double (Given)
*        J2000.0 mean Dec (radians)

*  Returned Value:
*     Component of "standard" solar motion in direction R2000,D2000 (km/s).

*  Description:
*     This function returns the velocity component in a given direction
*     due to the Sun's motion with respect to an adopted kinematic
*     Local Standard of Rest. The result is +ve when the Sun is receding
*     from the given point on the sky.

*  Notes:
*     - The Local Standard of Rest used here is one of several
*     "kinematical" LSRs in common use.  A kinematical LSR is the mean
*     standard of rest of specified star catalogues or stellar
*     populations.  The Sun's motion with respect to a kinematical LSR
*     is known as the "standard" solar motion.
*     - There is another sort of LSR, the "dynamical" LSR, which is a
*     point in the vicinity of the Sun which is in a circular orbit
*     around the Galactic centre.  The Sun's motion with respect to
*     the dynamical LSR is called the "peculiar" solar motion.  To
*     obtain a radial velocity correction with respect to the
*     dynamical LSR use the routine sla_RVLSRD.

*  Reference:
*     - Delhaye (1965), in "Stars and Stellar Systems", vol 5, p73.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2012-02-16 (DSB):
*        Initial version.
*        Adapted with permission from the Fortran SLALIB library.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 1995 Rutherford Appleton Laboratory
*     Copyright (C) 2012 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "pal.h"
#include "pal1sofa.h"

double palRvlsrk( double r2000, double d2000 ){

/* Local Variables: */
   double vb[ 3 ];

/*
*  Standard solar motion (from Methods of Experimental Physics, ed Meeks,
*  vol 12, part C, sec 6.1.5.2, p281):
*
*  20 km/s towards RA 18h Dec +30d (1900).
*
*  The solar motion is expressed here in the form of a J2000.0
*  equatorial Cartesian vector:
*
*      VA(1) = X = -SPEED*COS(RA)*COS(DEC)
*      VA(2) = Y = -SPEED*SIN(RA)*COS(DEC)
*      VA(3) = Z = -SPEED*SIN(DEC)
*/

   double va[ 3 ] = { -0.29000, +17.31726, -10.00141 };

/* Convert given J2000 RA,Dec to x,y,z. */
   eraS2c( r2000, d2000, vb );

/* Compute dot product with Solar motion vector. */
   return eraPdp( va, vb );
}
