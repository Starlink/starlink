/*
*+
*  Name:
*     palAirmas

*  Purpose:
*     Air mass at given zenith distance

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     double palAirmas( double zd );

*  Arguments:
*     zd = double (Given)
*        Observed zenith distance (radians)

*  Description:
*     Calculates the airmass at the observed zenith distance.

*  Authors:
*     PTW: Patrick Wallace (STFC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - The "observed" zenith distance referred to above means "as
*       affected by refraction".
*     - Uses Hardie's (1962) polynomial fit to Bemporad's data for
*       the relative air mass, X, in units of thickness at the zenith
*       as tabulated by Schoenberg (1929). This is adequate for all
*       normal needs as it is accurate to better than 0.1% up to X =
*       6.8 and better than 1% up to X = 10. Bemporad's tabulated
*       values are unlikely to be trustworthy to such accuracy
*       because of variations in density, pressure and other
*       conditions in the atmosphere from those assumed in his work.
*     - The sign of the ZD is ignored.
*     - At zenith distances greater than about ZD = 87 degrees the
*       air mass is held constant to avoid arithmetic overflows.

*  See Also:
*     - Hardie, R.H., 1962, in "Astronomical Techniques"
*         ed. W.A. Hiltner, University of Chicago Press, p180.
*     - Schoenberg, E., 1929, Hdb. d. Ap.,
*         Berlin, Julius Springer, 2, 268.

*  History:
*     2012-03-02 (TIMJ):
*        Initial version from the SLA/F version including documentation.
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
#include "palmac.h"

double palAirmas ( double zd ) {
  double seczm1;
  double airmass;

  /* Have maximum zenith distance of 87 deg */
  const double MAXZD = 87.0 * PAL__DD2R;

  zd = fabs(zd);
  zd = ( zd > MAXZD ? MAXZD : zd );

  seczm1 = (1.0 / cos(zd)) - 1.0;
  airmass = 1.0 + seczm1*(0.9981833 - seczm1*(0.002875 + 0.0008083*seczm1));
  return airmass;
}
