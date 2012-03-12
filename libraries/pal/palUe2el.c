/*
*+
*  Name:
*     palUe2el

*  Purpose:
*     Universal elements to heliocentric osculating elements

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palUe2el ( const double u[13], int jformr,
*                     int *jform, double *epoch, double *orbinc,
*                     double *anode, double *perih, double *aorq, double *e,
*                     double *aorl, double *dm, int *jstat );

*  Arguments:
*     u = const double [13] (Given)
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
*                -1 = illegal combined mass
*                -2 = illegal JFORMR
*                -3 = position/velocity out of range

*  Description:
*     Transform universal elements into conventional heliocentric
*     osculating elements.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - The "universal" elements are those which define the orbit for the
*       purposes of the method of universal variables (see reference 2).
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
*     - The universal elements are with respect to the mean equator and
*       equinox of epoch J2000.  The orbital elements produced are with
*       respect to the J2000 ecliptic and mean equinox.
*     - Three different element-format options are supported:
*
*        Option JFORM=1, suitable for the major planets:
*
*        EPOCH  = epoch of elements (TT MJD)
*        ORBINC = inclination i (radians)
*        ANODE  = longitude of the ascending node, big omega (radians)
*        PERIH  = longitude of perihelion, curly pi (radians)
*        AORQ   = mean distance, a (AU)
*        E      = eccentricity, e
*        AORL   = mean longitude L (radians)
*        DM     = daily motion (radians)
*
*        Option JFORM=2, suitable for minor planets:
*
*        EPOCH  = epoch of elements (TT MJD)
*        ORBINC = inclination i (radians)
*        ANODE  = longitude of the ascending node, big omega (radians)
*        PERIH  = argument of perihelion, little omega (radians)
*        AORQ   = mean distance, a (AU)
*        E      = eccentricity, e
*        AORL   = mean anomaly M (radians)
*
*        Option JFORM=3, suitable for comets:
*
*        EPOCH  = epoch of perihelion (TT MJD)
*        ORBINC = inclination i (radians)
*        ANODE  = longitude of the ascending node, big omega (radians)
*        PERIH  = argument of perihelion, little omega (radians)
*        AORQ   = perihelion distance, q (AU)
*        E      = eccentricity, e
*
*     - It may not be possible to generate elements in the form
*       requested through JFORMR.  The caller is notified of the form
*       of elements actually returned by means of the JFORM argument:
*
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
*     - The arguments returned for each value of JFORM (cf Note 6: JFORM
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
*     where:
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

*   See Also:
*     - Sterne, Theodore E., "An Introduction to Celestial Mechanics",
*       Interscience Publishers Inc., 1960.  Section 6.7, p199.
*     - Everhart, E. & Pitkin, E.T., Am.J.Phys. 51, 712, 1983.

*  History:
*     2012-03-09 (TIMJ):
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

void palUe2el ( const double u[], int jformr,
                int *jform, double *epoch, double *orbinc,
                double *anode, double *perih, double *aorq, double *e,
                double *aorl, double *dm, int *jstat ) {

  /*  Canonical days to seconds */
  const double CD2S = PAL__GCON / PAL__SPD;

  int i;
  double pmass, date, pv[6];

  /* Unpack the universal elements */
  pmass = u[0] - 1.0;
  date = u[2];
  for (i=0; i<3; i++) {
    pv[i] = u[i+3];
    pv[i+3] = u[i+6] * CD2S;
  }

  /* Convert the position and velocity etc into conventional elements */
  palPv2el( pv, date, pmass, jformr, jform, epoch, orbinc, anode,
            perih, aorq, e, aorl, dm, jstat );
}
