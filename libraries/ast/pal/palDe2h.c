/*
*+
*  Name:
*     palDe2h

*  Purpose:
*     Equatorial to horizon coordinates: HA,Dec to Az,E

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     palDe2h( double ha, double dec, double phi, double * az, double * el );

*  Arguments:
*     ha = double * (Given)
*        Hour angle (radians)
*     dec = double * (Given)
*        Declination (radians)
*     phi = double (Given)
*        Observatory latitude (radians)
*     az = double * (Returned)
*        Azimuth (radians)
*     el = double * (Returned)
*        Elevation (radians)

*  Description:
*     Convert equatorial to horizon coordinates.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - All the arguments are angles in radians.
*     - Azimuth is returned in the range 0-2pi;  north is zero,
*       and east is +pi/2.  Elevation is returned in the range
*       +/-pi/2.
*     - The latitude must be geodetic.  In critical applications,
*       corrections for polar motion should be applied.
*     - In some applications it will be important to specify the
*       correct type of hour angle and declination in order to
*       produce the required type of azimuth and elevation.  In
*       particular, it may be important to distinguish between
*       elevation as affected by refraction, which would
*       require the "observed" HA,Dec, and the elevation
*       in vacuo, which would require the "topocentric" HA,Dec.
*       If the effects of diurnal aberration can be neglected, the
*       "apparent" HA,Dec may be used instead of the topocentric
*       HA,Dec.
*     - No range checking of arguments is carried out.
*     - In applications which involve many such calculations, rather
*       than calling the present routine it will be more efficient to
*       use inline code, having previously computed fixed terms such
*       as sine and cosine of latitude, and (for tracking a star)
*       sine and cosine of declination.

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
#include "palmac.h"
#include <math.h>

void
palDe2h ( double ha, double dec, double phi, double *az, double *el) {

  double sh;
  double ch;
  double sd;
  double cd;
  double sp;
  double cp;

  double a;

  double x;
  double y;
  double z;
  double r;

  /*  Useful trig functions */
  sh = sin(ha);
  ch = cos(ha);
  sd = sin(dec);
  cd = cos(dec);
  sp = sin(phi);
  cp = cos(phi);

  /*  Az,El as x,y,z */
  x = -ch * cd * sp + sd * cp;
  y = -sh * cd;
  z = ch * cd * cp + sd * sp;

  /*  To spherical */
  r = sqrt(x * x + y * y);
  if (r == 0.) {
    a = 0.;
  } else {
    a = atan2(y, x);
  }
  if (a < 0.) {
    a += PAL__D2PI;
  }
  *az = a;
  *el = atan2(z, r);

  return;
}
