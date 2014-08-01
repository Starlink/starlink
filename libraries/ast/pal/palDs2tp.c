/*
*+
*  Name:
*     palDs2tp

*  Purpose:
*     Spherical to tangent plane projection

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     palDs2tp( double ra, double dec, double raz, double decz,
*               double *xi, double *eta, int *j );

*  Arguments:
*     ra = double (Given)
*        RA spherical coordinate of point to be projected (radians)
*     dec = double (Given)
*        Dec spherical coordinate of point to be projected (radians)
*     raz = double (Given)
*        RA spherical coordinate of tangent point (radians)
*     decz = double (Given)
*        Dec spherical coordinate of tangent point (radians)
*     xi = double * (Returned)
*        First rectangular coordinate on tangent plane (radians)
*     eta = double * (Returned)
*        Second rectangular coordinate on tangent plane (radians)
*     j = int * (Returned)
*        status: 0 = OK, star on tangent plane
*                1 = error, star too far from axis
*                2 = error, antistar on tangent plane
*                3 = error, antistar too far from axis

*  Description:
*     Projection of spherical coordinates onto tangent plane:
*     "gnomonic" projection - "standard coordinates"

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
*     Copyright (C) 1996 Rutherford Appleton Laboratory
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
#include <math.h>

void
palDs2tp ( double ra, double dec, double raz, double decz,
           double *xi, double *eta, int *j ) {

  const double TINY = 1.0e-6;

  double cdec;
  double sdec;
  double radif;
  double cdecz;
  double denom;
  double sdecz;
  double cradif;
  double sradif;

  /*  Trig functions */
  sdecz = sin(decz);
  sdec = sin(dec);
  cdecz = cos(decz);
  cdec = cos(dec);
  radif = ra - raz;
  sradif = sin(radif);
  cradif = cos(radif);

  /*  Reciprocal of star vector length to tangent plane */
  denom = sdec * sdecz + cdec * cdecz * cradif;

  /*  Handle vectors too far from axis */
  if (denom > TINY) {
    *j = 0;
  } else if (denom >= 0.) {
    *j = 1;
    denom = TINY;
  } else if (denom > -TINY) {
    *j = 2;
    denom = -TINY;
  } else {
    *j = 3;
  }

  /*  Compute tangent plane coordinates (even in dubious cases) */
  *xi = cdec * sradif / denom;
  *eta = (sdec * cdecz - cdec * sdecz * cradif) / denom;

  return;
}
