/*
*+
*  Name:
*     palPv2ue

*  Purpose:
*     Universal elements to position and velocity.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palPv2ue( const double pv[6], double date, double pmass,
*                    double u[13], int * jstat );

*  Arguments:
*     pv = double [6] (Given)
*        Heliocentric x,y,z,xdot,ydot,zdot of date, (AU,AU/s; Note 1)
*     date = double (Given)
*        Date (TT modified Julian Date = JD-2400000.5)
*     pmass = double (Given)
*        Mass of the planet (Sun=1; note 2)
*     u = double [13] (Returned)
*        Universal orbital elements (Note 3)
*
*            (1)  combined mass (M+m)
*            (2)  total energy of the orbit (alpha)
*            (3)  reference (osculating) epoch (t0)
*          (4-6)  position at reference epoch (r0)
*          (7-9)  velocity at reference epoch (v0)
*           (10)  heliocentric distance at reference epoch
*           (11)  r0.v0
*           (12)  date (t)
*           (13)  universal eccentric anomaly (psi) of date, approx
*     jstat = int * (Returned)
*        status: 0 = OK
*               -1 = illegal PMASS
*               -2 = too close to Sun
*               -3 = too slow

*  Description:
*     Construct a universal element set based on an instantaneous position
*     and velocity.


*  Authors:
*     PTW: Pat Wallace (STFC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - The PV 6-vector can be with respect to any chosen inertial frame,
*       and the resulting universal-element set will be with respect to
*       the same frame.  A common choice will be mean equator and ecliptic
*       of epoch J2000.
*     - The mass, PMASS, is important only for the larger planets.  For
*       most purposes (e.g. asteroids) use 0D0.  Values less than zero
*       are illegal.
*     - The "universal" elements are those which define the orbit for the
*       purposes of the method of universal variables (see reference).
*       They consist of the combined mass of the two bodies, an epoch,
*       and the position and velocity vectors (arbitrary reference frame)
*       at that epoch.  The parameter set used here includes also various
*       quantities that can, in fact, be derived from the other
*       information.  This approach is taken to avoiding unnecessary
*       computation and loss of accuracy.  The supplementary quantities
*       are (i) alpha, which is proportional to the total energy of the
*       orbit, (ii) the heliocentric distance at epoch, (iii) the
*       outwards component of the velocity at the given epoch, (iv) an
*       estimate of psi, the "universal eccentric anomaly" at a given
*       date and (v) that date.
*     - Reference:  Everhart, E. & Pitkin, E.T., Am.J.Phys. 51, 712, 1983.

*  History:
*     2012-03-09 (TIMJ):
*        Initial version from the SLA/F implementation.
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

#include <math.h>

#include "pal.h"
#include "palmac.h"

void palPv2ue( const double pv[6], double date, double pmass,
               double u[13], int * jstat ) {

  /*  Gaussian gravitational constant (exact) */
  const double GCON = 0.01720209895;

  /*  Canonical days to seconds */
  const double CD2S = GCON / PAL__SPD;

  /*  Minimum allowed distance (AU) and speed (AU per canonical day) */
  const double RMIN = 1e-3;
  const double VMIN = 1e-3;

  double T0,CM,X,Y,Z,XD,YD,ZD,R,V2,V,ALPHA,RDV;

  /*  Reference epoch. */
  T0 = date;

  /*  Combined mass (mu=M+m). */
  if (pmass < 0.0 ) { /* Negative planet mass */
    *jstat = -1;
    return;
  }
  CM = 1.0+pmass;

  /*  Unpack the state vector, expressing velocity in AU per canonical day. */
  X = pv[0];
  Y = pv[1];
  Z = pv[2];
  XD = pv[3]/CD2S;
  YD = pv[4]/CD2S;
  ZD = pv[5]/CD2S;

  /*  Heliocentric distance, and speed. */
  R = sqrt(X*X+Y*Y+Z*Z);
  V2 = XD*XD+YD*YD+ZD*ZD;
  V = sqrt(V2);

  /*  Reject unreasonably small values. */
  if (R < RMIN) { /* Too close */
    *jstat = -2;
    return;
  }
  if (V < VMIN) { /* Too slow */
    *jstat = -3;
    return;
  }

  /*  Total energy of the orbit. */
  ALPHA = V2-2.0*CM/R;

  /*  Outward component of velocity. */
  RDV = X*XD+Y*YD+Z*ZD;

  /*  Construct the universal-element set. */
  u[0] = CM;
  u[1] = ALPHA;
  u[2] = T0;
  u[3] = X;
  u[4] = Y;
  u[5] = Z;
  u[6] = XD;
  u[7] = YD;
  u[8] = ZD;
  u[9] = R;
  u[10] = RDV;
  u[11] = T0;
  u[12] = 0.0;

  *jstat = 0;
  return;
}
