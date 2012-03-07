/*
*+
*  Name:
*     palPlanet

*  Purpose:
*     Approximate heliocentric position and velocity of major planet

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palPlanet ( double date, int np, double pv[6], int *j );

*  Arguments:
*     date = double (Given)
*        TDB Modified Julian Date (JD-2400000.5).
*     np = int (Given)
*        planet (1=Mercury, 2=Venus, 3=EMB, 4=Mars,
*                5=Jupiter, 6=Saturn, 7=Uranus, 8=Neptune)
*     pv = double [6] (Returned)
*        heliocentric x,y,z,xdot,ydot,zdot, J2000, equatorial triad
*        in units AU and AU/s.
*     j = int * (Returned)
*        -2 = solution didn't converge.
*        -1 = illegal np (1-8)
*         0 = OK
*        +1 = warning: year outside 1000-3000

*  Description:
*     Calculates the approximate heliocentric position and velocity of
*     the specified major planet.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - See SOFA iauPlan94 for details
*     - Note that Pluto is supported in SLA/F but not in this routine
*     - Status -2 is equivalent to iauPlan94 status +2.
*     - Note that velocity units here match the SLA/F documentation.

*  History:
*     2012-03-07 (TIMJ):
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
#include "palmac.h"
#include "sofa.h"

void palPlanet ( double date, int np, double pv[6], int *j ) {
  double iaupv[2][3];

  *j = iauPlan94( PAL__MJD0, date, np, iaupv );

  /* Convert the outputs to the correct form and also correct AU/d
     to AU/s */
  pv[0] = iaupv[0][0];
  pv[1] = iaupv[0][1];
  pv[2] = iaupv[0][2];
  pv[3] = iaupv[1][0] / PAL__SPD;
  pv[4] = iaupv[1][1] / PAL__SPD;
  pv[5] = iaupv[1][2] / PAL__SPD;

  /* SLA compatibility for status */
  if (*j == 2) *j = -2;

}
