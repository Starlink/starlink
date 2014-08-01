/*
*+
*  Name:
*     palRvlg

*  Purpose:
*     Velocity component in a given direction due to Galactic rotation
*     and motion of the local group.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     double palRvlg( double r2000, double d2000 )

*  Arguments:
*     r2000 = double (Given)
*        J2000.0 mean RA (radians)
*     d2000 = double (Given)
*        J2000.0 mean Dec (radians)

*  Returned Value:
*     Component of SOLAR motion in direction R2000,D2000 (km/s).

*  Description:
*     This function returns the velocity component in a given
*     direction due to the combination of the rotation of the
*     Galaxy and the motion of the Galaxy relative to the mean
*     motion of the local group. The result is +ve when the Sun
*     is receding from the given point on the sky.
*
*  Reference:
*     - IAU Trans 1976, 168, p201.

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

double palRvlg( double r2000, double d2000 ){

/* Local Variables: */
   double vb[ 3 ];

/*
*
*  Solar velocity due to Galactic rotation and translation
*
*  Speed = 300 km/s
*
*  Apex  = L2,B2  90deg, 0deg
*        = RA,Dec  21 12 01.1  +48 19 47  J2000.0
*
*  This is expressed in the form of a J2000.0 x,y,z vector:
*
*      VA(1) = X = -SPEED*COS(RA)*COS(DEC)
*      VA(2) = Y = -SPEED*SIN(RA)*COS(DEC)
*      VA(3) = Z = -SPEED*SIN(DEC)
*/

   double va[ 3 ] = { -148.23284, +133.44888, -224.09467 };

/* Convert given J2000 RA,Dec to x,y,z. */
   eraS2c( r2000, d2000, vb );

/* Compute dot product with Solar motion vector. */
   return eraPdp( va, vb );
}
