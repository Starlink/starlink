/*
*+
*  Name:
*     palDtp2s

*  Purpose:
*     Tangent plane to spherical coordinates

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     palDtp2s( double xi, double eta, double raz, double decz,
*               double *ra, double *dec);

*  Arguments:
*     xi = double (Given)
*        First rectangular coordinate on tangent plane (radians)
*     eta = double (Given)
*        Second rectangular coordinate on tangent plane (radians)
*     raz = double (Given)
*        RA spherical coordinate of tangent point (radians)
*     decz = double (Given)
*        Dec spherical coordinate of tangent point (radians)
*     ra = double * (Returned)
*        RA spherical coordinate of point to be projected (radians)
*     dec = double * (Returned)
*        Dec spherical coordinate of point to be projected (radians)

*  Description:
*     Transform tangent plane coordinates into spherical.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2012-02-08 (TIMJ):
*        Initial version with documentation taken from Fortran SLA
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

#include <math.h>

void
palDtp2s ( double xi, double eta, double raz, double decz,
           double *ra, double *dec ) {

  double cdecz;
  double denom;
  double sdecz;
  double d;

  sdecz = sin(decz);
  cdecz = cos(decz);
  denom = cdecz - eta * sdecz;
  d = atan2(xi, denom) + raz;
  *ra = eraAnp(d);
  *dec = atan2(sdecz + eta * cdecz, sqrt(xi * xi + denom * denom));

  return;
}
