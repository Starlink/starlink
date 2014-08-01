/*
*+
*  Name:
*     palDh2e

*  Purpose:
*     Horizon to equatorial coordinates: Az,El to HA,Dec

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     palDh2e( double az, double el, double phi, double * ha, double * dec );

*  Arguments:
*     az = double (Given)
*        Azimuth (radians)
*     el = double (Given)
*        Elevation (radians)
*     phi = double (Given)
*        Observatory latitude (radians)
*     ha = double * (Returned)
*        Hour angle (radians)
*     dec = double * (Returned)
*        Declination (radians)

*  Description:
*     Convert horizon to equatorial coordinates.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - All the arguments are angles in radians.
*     - The sign convention for azimuth is north zero, east +pi/2.
*     - HA is returned in the range +/-pi.  Declination is returned
*       in the range +/-pi/2.
*     - The latitude is (in principle) geodetic.  In critical
*       applications, corrections for polar motion should be applied.
*     - In some applications it will be important to specify the
*       correct type of elevation in order to produce the required
*       type of HA,Dec.  In particular, it may be important to
*       distinguish between the elevation as affected by refraction,
*       which will yield the "observed" HA,Dec, and the elevation
*       in vacuo, which will yield the "topocentric" HA,Dec.  If the
*       effects of diurnal aberration can be neglected, the
*       topocentric HA,Dec may be used as an approximation to the
*       "apparent" HA,Dec.
*     - No range checking of arguments is done.
*     - In applications which involve many such calculations, rather
*       than calling the present routine it will be more efficient to
*       use inline code, having previously computed fixed terms such
*       as sine and cosine of latitude.

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
palDh2e ( double az, double el, double phi, double *ha, double *dec) {

  double sa;
  double ca;
  double se;
  double ce;
  double sp;
  double cp;

  double x;
  double y;
  double z;
  double r;

  /*  Useful trig functions */
  sa = sin(az);
  ca = cos(az);
  se = sin(el);
  ce = cos(el);
  sp = sin(phi);
  cp = cos(phi);

  /*  HA,Dec as x,y,z */
  x = -ca * ce * sp + se * cp;
  y = -sa * ce;
  z = ca * ce * cp + se * sp;

  /*  To HA,Dec */
  r = sqrt(x * x + y * y);
  if (r == 0.) {
    *ha = 0.;
  } else {
    *ha = atan2(y, x);
  }
  *dec = atan2(z, r);

  return;
}
