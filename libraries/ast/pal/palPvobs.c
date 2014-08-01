/*
*+
*  Name:
*     palPvobs

*  Purpose:
*     Position and velocity of an observing station.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     palPvobs( double p, double h, double stl, double pv[6] )

*  Arguments:
*     p = double (Given)
*        Latitude (geodetic, radians).
*     h = double (Given)
*        Height above reference spheroid (geodetic, metres).
*     stl = double (Given)
*        Local apparent sidereal time (radians).
*     pv = double[ 6 ] (Returned)
*        position/velocity 6-vector (AU, AU/s, true equator
*                                    and equinox of date).

*  Description:
*     Returns the position and velocity of an observing station.

*  Notes:
*     - The WGS84 reference ellipsoid is used.

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
#include "palmac.h"
#include "palmac.h"
#include "pal1sofa.h"

void palPvobs( double p, double h, double stl, double pv[6] ){

/* Local Variables: */
   double xyz[3], z, r, s, c, v;

/* Geodetic to geocentric conversion (WGS84 reference ellipsoid). */
   eraGd2gc( ERFA_WGS84, 0.0, p, h, xyz );

/* Convert from metres to AU */
   r = xyz[ 0 ]/ERFA_DAU;
   z = xyz[ 2 ]/ERFA_DAU;

/* Functions of ST. */
   s = sin( stl );
   c = cos( stl );

/* Speed. */
   v = PAL__SR*r;

/* Position. */
   pv[ 0 ] = r*c;
   pv[ 1 ] = r*s;
   pv[ 2 ] = z;

/* Velocity. */
   pv[ 3 ] = -v*s;
   pv[ 4 ] = v*c;
   pv[ 5 ] = 0.0;

}


