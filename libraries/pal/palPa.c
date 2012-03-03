/*
*+
*  Name:
*     palPa

*  Purpose:
*     HA, Dec to Parallactic Angle

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     double palPa( double ha, double dec, double phi );

*  Arguments:
*     ha = double (Given)
*        Hour angle in radians (Geocentric apparent)
*     dec = double (Given)
*        Declination in radians (Geocentric apparent)
*     phi = double (Given)
*        Observatory latitude in radians (geodetic)

*  Returned Value:
*     palPa = double
*        Parallactic angle in the range -pi to +pi.

*  Description:
*     Converts HA, Dec to Parallactic Angle.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - The parallactic angle at a point in the sky is the position
*       angle of the vertical, i.e. the angle between the direction to
*       the pole and to the zenith.  In precise applications care must
*       be taken only to use geocentric apparent HA,Dec and to consider
*       separately the effects of atmospheric refraction and telescope
*       mount errors.
*     - At the pole a zero result is returned.

*  History:
*     2012-03-02 (TIMJ):
*        Initial version
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "pal.h"

#include <math.h>

double palPa( double ha, double dec, double phi ) {
  double cp, sqsz, cqsz;

  cp = cos(phi);
  sqsz = cp * sin(ha);
  cqsz = sin(phi) * cos(dec) - cp * sin(dec) * cos(ha);
  if (sqsz == 0.0 && cqsz == 0.0) cqsz = 1.0;
  return atan2( sqsz, cqsz );
}
