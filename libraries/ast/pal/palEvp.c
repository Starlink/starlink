/*
*+
*  Name:
*     palEvp

*  Purpose:
*     Returns the barycentric and heliocentric velocity and position of the
*     Earth.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palEvp( double date, double deqx, double dvb[3], double dpb[3],
*                  double dvh[3], double dph[3] )

*  Arguments:
*     date = double (Given)
*        TDB (loosely ET) as a Modified Julian Date (JD-2400000.5)
*     deqx = double (Given)
*        Julian epoch (e.g. 2000.0) of mean equator and equinox of the
*        vectors returned.  If deqx <= 0.0, all vectors are referred to the
*        mean equator and equinox (FK5) of epoch date.
*     dvb = double[3] (Returned)
*        Barycentric velocity (AU/s, AU)
*     dpb = double[3] (Returned)
*        Barycentric position (AU/s, AU)
*     dvh = double[3] (Returned)
*        heliocentric velocity (AU/s, AU)
*     dph = double[3] (Returned)
*        Heliocentric position (AU/s, AU)

*  Description:
*     Returns the barycentric and heliocentric velocity and position of the
*     Earth at a given epoch, given with respect to a specified equinox.
*     For information about accuracy, see the function iauEpv00.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     {enter_new_authors_here}

*  History:
*     2012-02-13 (PTW):
*        Initial version.
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
*    You should have received a copy of the GNU General Public License
*    along with this program; if not, write to the Free Software
*    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
*    USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "pal.h"
#include "palmac.h"
#include "sofa.h"

void palEvp( double date, double deqx, double dvb[3], double dpb[3],
             double dvh[3], double dph[3] ){

/* Local Variables; */
   int i;
   double pvh[2][3], pvb[2][3], d1, d2, r[3][3];

/* BCRS PV-vectors. */
   iauEpv00 ( 2400000.5, date, pvh, pvb );

/* Was precession to another equinox requested? */
   if ( deqx > 0.0 ) {

/* Yes: compute precession matrix from J2000.0 to deqx. */
      iauEpj2jd ( deqx, &d1, &d2 );
      iauPmat06 ( d1, d2, r );

/* Rotate the PV-vectors. */
      iauRxpv ( r, pvh, pvh );
      iauRxpv ( r, pvb, pvb );
   }

/* Return the required vectors. */
   for ( i = 0; i < 3; i++ ) {
      dvh[i] = pvh[1][i] / PAL__SPD;
      dvb[i] = pvb[1][i] / PAL__SPD;
      dph[i] = pvh[0][i];
      dpb[i] = pvb[0][i];
   }
}
