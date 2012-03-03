/*
*+
*  Name:
*     palPreces

*  Purpose:
*     Precession - either FK4 or FK5 as required.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palPreces ( const char sys[3], double ep0, double ep1,
*                      double *ra, double *dc );

*  Arguments:
*     sys = const char [3] (Given)
*        Precession to be applied: FK4 or FK5. Case insensitive.
*     ep0 = double (Given)
*        Starting epoch.
*     ep1 = double (Given)
*        Ending epoch
*     ra = double * (Given & Returned)
*        On input the RA mean equator & equinox at epoch ep0. On exit
*        the RA mean equator & equinox of epoch ep1.
*     dec = double * (Given & Returned)
*        On input the dec mean equator & equinox at epoch ep0. On exit
*        the dec mean equator & equinox of epoch ep1.

*  Description:
*     Precess coordinates using the appropriate system and epochs.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - Uses palPrec for FK5 data and palPrebn for FK4 data.
*     - The epochs are Besselian if SYSTEM='FK4' and Julian if 'FK5'.
*        For example, to precess coordinates in the old system from
*        equinox 1900.0 to 1950.0 the call would be:
*             palPreces( "FK4", 1900.0, 1950.0, &ra, &dc );
*     - This routine will NOT correctly convert between the old and
*       the new systems - for example conversion from B1950 to J2000.
*       For these purposes see palFk425, palFk524, palFk45z and
*       palFk54z.
*     - If an invalid SYSTEM is supplied, values of -99D0,-99D0 will
*       be returned for both RA and DC.

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
#include "sofa.h"

#include <string.h>

void palPreces ( const char sys[3], double ep0, double ep1,
                 double *ra, double *dc ) {

  double pm[3][3];
  double v1[3];
  double v2[3];

  /* Generate appropriate precession matrix */
  if ( strncasecmp( "FK4", sys, 3 ) == 0 ) {
    palPrebn( ep0, ep1, pm );
  } else if (strncasecmp( "FK5", sys, 3 ) == 0 ) {
    palPrec( ep0, ep1, pm );
  } else {
    *ra = -99.0;
    *dc = -99.0;
    return;
  }

  /* Convert RA,Dec to x,y,z */
  iauS2c( *ra, *dc, v1 );

  /* Precess */
  iauRxp( pm, v1, v2 );

  /* Back to RA,Dec */
  iauC2s( v2, ra, dc );
  *ra = iauAnp( *ra );
}
