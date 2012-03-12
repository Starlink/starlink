/*
*+
*  Name:
*     palEl2ue

*  Purpose:
*     Transform conventional elements into "universal" form

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palEl2ue ( double date, int jform, double epoch, double orbinc,
*                     double anode, double perih, double aorq, double e,
*                     double aorl, double dm, double u[13], int *jstat );

*  Arguments:
*     date = double (Given)
*        Epoch (TT MJD) of osculation (Note 3)
*     jform = int (Given)
*        Element set actually returned (1-3; Note 6)
*     epoch = double (Given)
*        Epoch of elements (TT MJD)
*     orbinc = double (Given)
*        inclination (radians)
*     anode = double (Given)
*        longitude of the ascending node (radians)
*     perih = double (Given)
*        longitude or argument of perihelion (radians)
*     aorq = double (Given)
*        mean distance or perihelion distance (AU)
*     e = double (Given)
*        eccentricity
*     aorl = double (Given)
*        mean anomaly or longitude (radians, JFORM=1,2 only)
*     dm = double (Given)
*        daily motion (radians, JFORM=1 only)
*     u = double [13] (Returned)
*        Universal orbital elements (Note 1)
*            (0)  combined mass (M+m)
*            (1)  total energy of the orbit (alpha)
*            (2)  reference (osculating) epoch (t0)
*          (3-5)  position at reference epoch (r0)
*          (6-8)  velocity at reference epoch (v0)
*            (9)  heliocentric distance at reference epoch
*           (10)  r0.v0
*           (11)  date (t)
*           (12)  universal eccentric anomaly (psi) of date, approx
*     jstat = int * (Returned)
*        status:  0 = OK
*                -1 = illegal JFORM
*                -2 = illegal E
*                -3 = illegal AORQ
*                -4 = illegal DM
*                -5 = numerical error

*  Description:
*      Transform conventional osculating elements into "universal" form.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
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
*     - The companion routine is palUe2pv.  This takes the set of numbers
*       that the present routine outputs and uses them to derive the
*       object's position and velocity.  A single prediction requires one
*       call to the present routine followed by one call to palUe2pv;
*       for convenience, the two calls are packaged as the routine
*       palPlanel.  Multiple predictions may be made by again calling the
*       present routine once, but then calling palUe2pv multiple times,
*       which is faster than multiple calls to palPlanel.
*     - DATE is the epoch of osculation.  It is in the TT timescale
*       (formerly Ephemeris Time, ET) and is a Modified Julian Date
*       (JD-2400000.5).
*     - The supplied orbital elements are with respect to the J2000
*       ecliptic and equinox.  The position and velocity parameters
*       returned in the array U are with respect to the mean equator and
*       equinox of epoch J2000, and are for the perihelion prior to the
*       specified epoch.
*     - The universal elements returned in the array U are in canonical
*       units (solar masses, AU and canonical days).
*     - Three different element-format options are available:
*
*       Option JFORM=1, suitable for the major planets:
*
*       EPOCH  = epoch of elements (TT MJD)
*       ORBINC = inclination i (radians)
*       ANODE  = longitude of the ascending node, big omega (radians)
*       PERIH  = longitude of perihelion, curly pi (radians)
*       AORQ   = mean distance, a (AU)
*       E      = eccentricity, e (range 0 to <1)
*       AORL   = mean longitude L (radians)
*       DM     = daily motion (radians)
*
*       Option JFORM=2, suitable for minor planets:
*
*       EPOCH  = epoch of elements (TT MJD)
*       ORBINC = inclination i (radians)
*       ANODE  = longitude of the ascending node, big omega (radians)
*       PERIH  = argument of perihelion, little omega (radians)
*       AORQ   = mean distance, a (AU)
*       E      = eccentricity, e (range 0 to <1)
*       AORL   = mean anomaly M (radians)
*
*       Option JFORM=3, suitable for comets:
*
*       EPOCH  = epoch of perihelion (TT MJD)
*       ORBINC = inclination i (radians)
*       ANODE  = longitude of the ascending node, big omega (radians)
*       PERIH  = argument of perihelion, little omega (radians)
*       AORQ   = perihelion distance, q (AU)
*       E      = eccentricity, e (range 0 to 10)
*
*     - Unused elements (DM for JFORM=2, AORL and DM for JFORM=3) are
*       not accessed.
*     - The algorithm was originally adapted from the EPHSLA program of
*       D.H.P.Jones (private communication, 1996).  The method is based
*       on Stumpff's Universal Variables.
*
*  See Also:
*     Everhart & Pitkin, Am.J.Phys. 51, 712 (1983).

*  History:
*     2012-03-12 (TIMJ):
*        Initial version taken directly from SLA/F.
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

void palEl2ue ( double date, int jform, double epoch, double orbinc,
                double anode, double perih, double aorq, double e,
                double aorl, double dm, double u[13], int *jstat ) {

  /*  Sin and cos of J2000 mean obliquity (IAU 1976) */
  const double SE=0.3977771559319137;
  const double CE=0.9174820620691818;

  int J;

  double PHT,ARGPH,Q,W,CM,ALPHA,PHS,SW,CW,SI,CI,SO,CO,
    X,Y,Z,PX,PY,PZ,VX,VY,VZ,DT,FC,FP,PSI,
    UL[13],PV[6];

  /*  Validate arguments. */
  if (jform < 1 || jform > 3) {
    *jstat = -1;
    return;
  }
  if (e < 0.0 || e > 10.0 || (e >= 1.0 && jform != 3)) {
    *jstat = -2;
    return;
  }
  if (aorq <= 0.0) {
    *jstat = -3;
    return;
  }
  if (jform == 1 && dm <= 0.0) {
    *jstat = -4;
    return;
  }

  /*
   *  Transform elements into standard form:
   *
   *  PHT   = epoch of perihelion passage
   *  ARGPH = argument of perihelion (little omega)
   *  Q     = perihelion distance (q)
   *  CM    = combined mass, M+m (mu)
   */

  if (jform == 1) {

    /*     Major planet. */
    PHT = epoch-(aorl-perih)/dm;
    ARGPH = perih-anode;
    Q = aorq*(1.0-e);
    W = dm/PAL__GCON;
    CM = W*W*aorq*aorq*aorq;

  } else if (jform == 2) {

    /*     Minor planet. */
    PHT = epoch-aorl*sqrt(aorq*aorq*aorq)/PAL__GCON;
    ARGPH = perih;
    Q = aorq*(1.0-e);
    CM = 1.0;

  } else {

    /*     Comet. */
    PHT = epoch;
    ARGPH = perih;
    Q = aorq;
    CM = 1.0;

  }

  /*  The universal variable alpha.  This is proportional to the total
   *  energy of the orbit:  -ve for an ellipse, zero for a parabola,
   *  +ve for a hyperbola. */

  ALPHA = CM*(e-1.0)/Q;

  /*  Speed at perihelion. */

  PHS = sqrt(ALPHA+2.0*CM/Q);

  /*  In a Cartesian coordinate system which has the x-axis pointing
   *  to perihelion and the z-axis normal to the orbit (such that the
   *  object orbits counter-clockwise as seen from +ve z), the
   *  perihelion position and velocity vectors are:
   *
   *    position   [Q,0,0]
   *    velocity   [0,PHS,0]
   *
   *  To express the results in J2000 equatorial coordinates we make a
   *  series of four rotations of the Cartesian axes:
   *
   *           axis      Euler angle
   *
   *     1      z        argument of perihelion (little omega)
   *     2      x        inclination (i)
   *     3      z        longitude of the ascending node (big omega)
   *     4      x        J2000 obliquity (epsilon)
   *
   *  In each case the rotation is clockwise as seen from the +ve end of
   *  the axis concerned.
   */

  /*  Functions of the Euler angles. */
  SW = sin(ARGPH);
  CW = cos(ARGPH);
  SI = sin(orbinc);
  CI = cos(orbinc);
  SO = sin(anode);
  CO = cos(anode);

  /*  Position at perihelion (AU). */
  X = Q*CW;
  Y = Q*SW;
  Z = Y*SI;
  Y = Y*CI;
  PX = X*CO-Y*SO;
  Y = X*SO+Y*CO;
  PY = Y*CE-Z*SE;
  PZ = Y*SE+Z*CE;

  /*  Velocity at perihelion (AU per canonical day). */
  X = -PHS*SW;
  Y = PHS*CW;
  Z = Y*SI;
  Y = Y*CI;
  VX = X*CO-Y*SO;
  Y = X*SO+Y*CO;
  VY = Y*CE-Z*SE;
  VZ = Y*SE+Z*CE;

  /*  Time from perihelion to date (in Canonical Days: a canonical day
   *  is 58.1324409... days, defined as 1/PAL__GCON). */

  DT = (date-PHT)*PAL__GCON;

  /*  First approximation to the Universal Eccentric Anomaly, PSI,
   *  based on the circle (FC) and parabola (FP) values. */

  FC = DT/Q;
  W = pow(3.0*DT+sqrt(9.0*DT*DT+8.0*Q*Q*Q), 1.0/3.0);
  FP = W-2.0*Q/W;
  PSI = (1.0-e)*FC+e*FP;

  /*  Assemble local copy of element set. */
  UL[0] = CM;
  UL[1] = ALPHA;
  UL[2] = PHT;
  UL[3] = PX;
  UL[4] = PY;
  UL[5] = PZ;
  UL[6] = VX;
  UL[7] = VY;
  UL[8] = VZ;
  UL[9] = Q;
  UL[10] = 0.0;
  UL[11] = date;
  UL[12] = PSI;

  /*  Predict position+velocity at epoch of osculation. */
  palUe2pv( date, UL, PV, &J );
  if (J != 0) {
    *jstat = -5;
    return;
  }

  /*  Convert back to universal elements. */
  palPv2ue( PV, date, CM-1.0, u, &J );
  if (J != 0) {
    *jstat = -5;
    return;
  }

  /*  OK exit. */
  *jstat = 0;

}
