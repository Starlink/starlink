/*
*+
*  Name:
*     palDtps2c

*  Purpose:
*     Determine RA,Dec of tangent point from coordinates

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     palDtps2c( double xi, double eta, double ra, double dec,
*                double * raz1, double decz1,
*                double * raz2, double decz2, int *n);

*  Arguments:
*     xi = double (Given)
*        First rectangular coordinate on tangent plane (radians)
*     eta = double (Given)
*        Second rectangular coordinate on tangent plane (radians)
*     ra = double (Given)
*        RA spherical coordinate of star (radians)
*     dec = double (Given)
*        Dec spherical coordinate of star (radians)
*     raz1 = double * (Returned)
*        RA spherical coordinate of tangent point, solution 1 (radians)
*     decz1 = double * (Returned)
*        Dec spherical coordinate of tangent point, solution 1 (radians)
*     raz2 = double * (Returned)
*        RA spherical coordinate of tangent point, solution 2 (radians)
*     decz2 = double * (Returned)
*        Dec spherical coordinate of tangent point, solution 2 (radians)
*     n = int * (Returned)
*        number of solutions: 0 = no solutions returned (note 2)
*                             1 = only the first solution is useful (note 3)
*                             2 = both solutions are useful (note 3)


*  Description:
*     From the tangent plane coordinates of a star of known RA,Dec,
*     determine the RA,Dec of the tangent point.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - The RAZ1 and RAZ2 values are returned in the range 0-2pi.
*     - Cases where there is no solution can only arise near the poles.
*       For example, it is clearly impossible for a star at the pole
*       itself to have a non-zero XI value, and hence it is
*       meaningless to ask where the tangent point would have to be
*       to bring about this combination of XI and DEC.
*     - Also near the poles, cases can arise where there are two useful
*       solutions.  The argument N indicates whether the second of the
*       two solutions returned is useful.  N=1 indicates only one useful
*       solution, the usual case;  under these circumstances, the second
*       solution corresponds to the "over-the-pole" case, and this is
*       reflected in the values of RAZ2 and DECZ2 which are returned.
*     - The DECZ1 and DECZ2 values are returned in the range +/-pi, but
*       in the usual, non-pole-crossing, case, the range is +/-pi/2.
*     - This routine is the spherical equivalent of the routine sla_DTPV2C.

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
palDtps2c( double xi, double eta, double ra, double dec,
           double * raz1, double * decz1,
           double * raz2, double * decz2, int *n) {

  double x2;
  double y2;
  double sd;
  double cd;
  double sdf;
  double r2;

  x2 = xi * xi;
  y2 = eta * eta;
  sd = sin(dec);
  cd = cos(dec);
  sdf = sd * sqrt(x2 + 1. + y2);
  r2 = cd * cd * (y2 + 1.) - sd * sd * x2;
  if (r2 >= 0.) {
    double r;
    double s;
    double c;

    r = sqrt(r2);
    s = sdf - eta * r;
    c = sdf * eta + r;
    if (xi == 0. && r == 0.) {
      r = 1.;
    }
    *raz1 = eraAnp(ra - atan2(xi, r));
    *decz1 = atan2(s, c);
    r = -r;
    s = sdf - eta * r;
    c = sdf * eta + r;
    *raz2 = eraAnp(ra - atan2(xi, r));
    *decz2 = atan2(s, c);
    if (fabs(sdf) < 1.) {
      *n = 1;
    } else {
      *n = 2;
    }
  } else {
    *n = 0;
  }
  return;
}
