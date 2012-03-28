/*
*+
*  Name:
*     palPv2el

*  Purpose:
*     Position velocity to heliocentirc osculating elements

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palPv2el ( const double pv[6], double date, double pmass, int jformr,
*                     int *jform, double *epoch, double *orbinc,
*                     double *anode, double *perih, double *aorq, double *e,
*                     double *aorl, double *dm, int *jstat );

*  Arguments:
*     pv = const double [6] (Given)
*        Heliocentric x,y,z,xdot,ydot,zdot of date,
*        J2000 equatorial triad (AU,AU/s; Note 1)
*     date = double (Given)
*        Date (TT Modified Julian Date = JD-2400000.5)
*     pmass = double (Given)
*        Mass of the planet (Sun=1; Note 2)
*     jformr = int (Given)
*        Requested element set (1-3; Note 3)
*     jform = int * (Returned)
*        Element set actually returned (1-3; Note 4)
*     epoch = double * (Returned)
*        Epoch of elements (TT MJD)
*     orbinc = double * (Returned)
*        inclination (radians)
*     anode = double * (Returned)
*        longitude of the ascending node (radians)
*     perih = double * (Returned)
*        longitude or argument of perihelion (radians)
*     aorq = double * (Returned)
*        mean distance or perihelion distance (AU)
*     e = double * (Returned)
*        eccentricity
*     aorl = double * (Returned)
*        mean anomaly or longitude (radians, JFORM=1,2 only)
*     dm = double * (Returned)
*        daily motion (radians, JFORM=1 only)
*     jstat = int * (Returned)
*        status:  0 = OK
*               - -1 = illegal PMASS
*               - -2 = illegal JFORMR
*               - -3 = position/velocity out of range

*  Description:
*     Heliocentric osculating elements obtained from instantaneous position
*     and velocity.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - The PV 6-vector is with respect to the mean equator and equinox of
*       epoch J2000.  The orbital elements produced are with respect to
*       the J2000 ecliptic and mean equinox.
*     - The mass, PMASS, is important only for the larger planets.  For
*       most purposes (e.g. asteroids) use 0D0.  Values less than zero
*       are illegal.
*     - Three different element-format options are supported:
*
*       Option JFORM=1, suitable for the major planets:
*
*       EPOCH  = epoch of elements (TT MJD)
*       ORBINC = inclination i (radians)
*       ANODE  = longitude of the ascending node, big omega (radians)
*       PERIH  = longitude of perihelion, curly pi (radians)
*       AORQ   = mean distance, a (AU)
*       E      = eccentricity, e
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
*       E      = eccentricity, e
*       AORL   = mean anomaly M (radians)
*
*       Option JFORM=3, suitable for comets:
*
*       EPOCH  = epoch of perihelion (TT MJD)
*       ORBINC = inclination i (radians)
*       ANODE  = longitude of the ascending node, big omega (radians)
*       PERIH  = argument of perihelion, little omega (radians)
*       AORQ   = perihelion distance, q (AU)
*       E      = eccentricity, e
*
*     - It may not be possible to generate elements in the form
*       requested through JFORMR.  The caller is notified of the form
*       of elements actually returned by means of the JFORM argument:

*        JFORMR   JFORM     meaning
*
*          1        1       OK - elements are in the requested format
*          1        2       never happens
*          1        3       orbit not elliptical
*
*          2        1       never happens
*          2        2       OK - elements are in the requested format
*          2        3       orbit not elliptical
*
*          3        1       never happens
*          3        2       never happens
*          3        3       OK - elements are in the requested format
*
*     - The arguments returned for each value of JFORM (cf Note 5: JFORM
*       may not be the same as JFORMR) are as follows:
*
*         JFORM         1              2              3
*         EPOCH         t0             t0             T
*         ORBINC        i              i              i
*         ANODE         Omega          Omega          Omega
*         PERIH         curly pi       omega          omega
*         AORQ          a              a              q
*         E             e              e              e
*         AORL          L              M              -
*         DM            n              -              -
*
*       where:
*
*         t0           is the epoch of the elements (MJD, TT)
*         T              "    epoch of perihelion (MJD, TT)
*         i              "    inclination (radians)
*         Omega          "    longitude of the ascending node (radians)
*         curly pi       "    longitude of perihelion (radians)
*         omega          "    argument of perihelion (radians)
*         a              "    mean distance (AU)
*         q              "    perihelion distance (AU)
*         e              "    eccentricity
*         L              "    longitude (radians, 0-2pi)
*         M              "    mean anomaly (radians, 0-2pi)
*         n              "    daily motion (radians)
*         -             means no value is set
*
*     - At very small inclinations, the longitude of the ascending node
*       ANODE becomes indeterminate and under some circumstances may be
*       set arbitrarily to zero.  Similarly, if the orbit is close to
*       circular, the true anomaly becomes indeterminate and under some
*       circumstances may be set arbitrarily to zero.  In such cases,
*       the other elements are automatically adjusted to compensate,
*       and so the elements remain a valid description of the orbit.
*     - The osculating epoch for the returned elements is the argument
*       DATE.
*
*     - Reference:  Sterne, Theodore E., "An Introduction to Celestial
*                   Mechanics", Interscience Publishers, 1960

*  History:
*     2012-03-09 (TIMJ):
*        Initial version converted from SLA/F.
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

#include "sofa.h"
#include "pal.h"
#include "palmac.h"

void palPv2el ( const double pv[6], double date, double pmass, int jformr,
                int *jform, double *epoch, double *orbinc,
                double *anode, double *perih, double *aorq, double *e,
                double *aorl, double *dm, int *jstat ) {

  /*  Sin and cos of J2000 mean obliquity (IAU 1976) */
  const double SE = 0.3977771559319137;
  const double CE = 0.9174820620691818;

  /*  Minimum allowed distance (AU) and speed (AU/day) */
  const double RMIN = 1e-3;
  const double VMIN = 1e-8;

  /*  How close to unity the eccentricity has to be to call it a parabola */
  const double PARAB = 1.0e-8;

  double X,Y,Z,XD,YD,ZD,R,V2,V,RDV,GMU,HX,HY,HZ,
    HX2PY2,H2,H,OI,BIGOM,AR,ECC,S,C,AT,U,OM,
    GAR3,EM1,EP1,HAT,SHAT,CHAT,AE,AM,DN,PL,
    EL,Q,TP,THAT,THHF,F;

  int JF;

  /*  Validate arguments PMASS and JFORMR.*/
  if (pmass < 0.0) {
    *jstat = -1;
    return;
  }
  if (jformr < 1 || jformr > 3) {
    *jstat = -2;
    return;
  }

  /*  Provisionally assume the elements will be in the chosen form. */
  JF = jformr;

  /*  Rotate the position from equatorial to ecliptic coordinates. */
  X = pv[0];
  Y = pv[1]*CE+pv[2]*SE;
  Z = -pv[1]*SE+pv[2]*CE;

  /*  Rotate the velocity similarly, scaling to AU/day. */
  XD = PAL__SPD*pv[3];
  YD = PAL__SPD*(pv[4]*CE+pv[5]*SE);
  ZD = PAL__SPD*(-pv[4]*SE+pv[5]*CE);

  /*  Distance and speed. */
  R = sqrt(X*X+Y*Y+Z*Z);
  V2 = XD*XD+YD*YD+ZD*ZD;
  V = sqrt(V2);

  /*  Reject unreasonably small values. */
  if (R < RMIN || V < VMIN) {
    *jstat = -3;
    return;
  }

  /*  R dot V. */
  RDV = X*XD+Y*YD+Z*ZD;

  /*  Mu. */
  GMU = (1.0+pmass)*PAL__GCON*PAL__GCON;

  /*  Vector angular momentum per unit reduced mass. */
  HX = Y*ZD-Z*YD;
  HY = Z*XD-X*ZD;
  HZ = X*YD-Y*XD;

  /*  Areal constant. */
  HX2PY2 = HX*HX+HY*HY;
  H2 = HX2PY2+HZ*HZ;
  H = sqrt(H2);

  /*  Inclination. */
  OI = atan2(sqrt(HX2PY2),HZ);

  /*  Longitude of ascending node. */
  if (HX != 0.0 || HY != 0.0) {
    BIGOM = atan2(HX,-HY);
  } else {
    BIGOM=0.0;
  }

  /*  Reciprocal of mean distance etc. */
  AR = 2.0/R-V2/GMU;

  /*  Eccentricity. */
  ECC = sqrt(DMAX(1.0-AR*H2/GMU,0.0));

  /*  True anomaly. */
  S = H*RDV;
  C = H2-R*GMU;
  if (S != 0.0 || C != 0.0) {
    AT = atan2(S,C);
  } else {
    AT = 0.0;
  }

  /*  Argument of the latitude. */
  S = sin(BIGOM);
  C = cos(BIGOM);
  U = atan2((-X*S+Y*C)*cos(OI)+Z*sin(OI),X*C+Y*S);

  /*  Argument of perihelion. */
  OM = U-AT;

  /*  Capture near-parabolic cases. */
  if (fabs(ECC-1.0) < PARAB) ECC=1.0;

  /*  Comply with JFORMR = 1 or 2 only if orbit is elliptical. */
  if (ECC > 1.0) JF=3;

  /*  Functions. */
  GAR3 = GMU*AR*AR*AR;
  EM1 = ECC-1.0;
  EP1 = ECC+1.0;
  HAT = AT/2.0;
  SHAT = sin(HAT);
  CHAT = cos(HAT);

  /*  Variable initializations to avoid compiler warnings. */
  AM = 0.0;
  DN = 0.0;
  PL = 0.0;
  EL = 0.0;
  Q = 0.0;
  TP = 0.0;

  /*  Ellipse? */
  if (ECC < 1.0 ) {

    /*     Eccentric anomaly. */
    AE = 2.0*atan2(sqrt(-EM1)*SHAT,sqrt(EP1)*CHAT);

    /*     Mean anomaly. */
    AM = AE-ECC*sin(AE);

    /*     Daily motion. */
    DN = sqrt(GAR3);
  }

  /*  "Major planet" element set? */
  if (JF == 1) {

    /*     Longitude of perihelion. */
    PL = BIGOM+OM;

    /*     Longitude at epoch. */
    EL = PL+AM;
  }

  /*  "Comet" element set? */
  if (JF == 3) {

    /*     Perihelion distance. */
    Q = H2/(GMU*EP1);

    /*     Ellipse, parabola, hyperbola? */
    if (ECC < 1.0) {

      /*        Ellipse: epoch of perihelion. */
      TP = date-AM/DN;

    } else {

      /*        Parabola or hyperbola: evaluate tan ( ( true anomaly ) / 2 ) */
      THAT = SHAT/CHAT;
      if (ECC == 1.0) {

        /*           Parabola: epoch of perihelion. */
        TP = date-THAT*(1.0+THAT*THAT/3.0)*H*H2/(2.0*GMU*GMU);

      } else {

        /*           Hyperbola: epoch of perihelion. */
        THHF = sqrt(EM1/EP1)*THAT;
        F = log(1.0+THHF)-log(1.0-THHF);
        TP = date-(ECC*sinh(F)-F)/sqrt(-GAR3);
      }
    }
  }

  /*  Return the appropriate set of elements. */
  *jform = JF;
  *orbinc = OI;
  *anode = iauAnp(BIGOM);
  *e = ECC;
  if (JF == 1) {
    *perih = iauAnp(PL);
    *aorl = iauAnp(EL);
    *dm = DN;
  } else {
    *perih = iauAnp(OM);
    if (JF == 2) *aorl = iauAnp(AM);
  }
  if (JF != 3) {
    *epoch = date;
    *aorq = 1.0/AR;
  } else {
    *epoch = TP;
    *aorq = Q;
  }
  *jstat = 0;

}
