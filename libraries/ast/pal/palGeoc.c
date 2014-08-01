/*
*+
*  Name:
*     palGeoc

*  Purpose:
*     Convert geodetic position to geocentric

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palGeoc( double p, double h, double * r, double *z );

*  Arguments:
*     p = double (Given)
*       latitude (radians)
*     h = double (Given)
*       height above reference spheroid (geodetic, metres)
*     r = double * (Returned)
*       distance from Earth axis (AU)
*     z = double * (Returned)
*       distance from plane of Earth equator (AU)

*  Description:
*     Convert geodetic position to geocentric.

*  Authors:
*     PTW: Patrick T. Wallace
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - Geocentric latitude can be obtained by evaluating atan2(z,r)
*     - Uses WGS84 reference ellipsoid and calls eraGd2gc

*  History:
*     2012-03-01 (TIMJ):
*        Initial version moved from palOne2One
*        Adapted with permission from the Fortran SLALIB library.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2004 Patrick T. Wallace
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

void palGeoc ( double p, double h, double *r, double *z ) {
  double xyz[3];
  const double elong = 0.0;   /* Use zero longitude */
  const double AU = 1.49597870E11;
  /* WGS84 looks to be the closest match */
  eraGd2gc( ERFA_WGS84, elong, p, h, xyz );
  *r = xyz[0] / (AU * cos(elong) );
  *z = xyz[2] / AU;
}
