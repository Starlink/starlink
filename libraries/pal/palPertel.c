/*
*+
*  Name:
*     palPertel

*  Purpose:
*     Update elements by applying planetary perturbations

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palPertel (int jform, double date0, double date1,
*                     double epoch0, double orbi0, double anode0,
*                     double perih0, double aorq0, double e0, double am0,
*                     double *epoch1, double *orbi1, double *anode1,
*                     double *perih1, double *aorq1, double *e1, double *am1,
*                     int *jstat );

*  Arguments:
*     jform = int (Given)
*        Element set actually returned (1-3; Note 6)
*     date0 = double (Given)
*        Date of osculation (TT MJD) for the given elements.
*     date1 = double (Given)
*        Date of osculation (TT MJD) for the updated elements.
*     epoch0 = double (Given)
*        Epoch of elements (TT MJD)
*     orbi0 = double (Given)
*        inclination (radians)
*     anode0 = double (Given)
*        longitude of the ascending node (radians)
*     perih0 = double (Given)
*        longitude or argument of perihelion (radians)
*     aorq0 = double (Given)
*        mean distance or perihelion distance (AU)
*     e0 = double (Given)
*        eccentricity
*     am0 = double (Given)
*        mean anomaly (radians, JFORM=2 only)
*     epoch1 = double * (Returned)
*        Epoch of elements (TT MJD)
*     orbi1 = double * (Returned)
*        inclination (radians)
*     anode1 = double * (Returned)
*        longitude of the ascending node (radians)
*     perih1 = double * (Returned)
*        longitude or argument of perihelion (radians)
*     aorq1 = double * (Returned)
*        mean distance or perihelion distance (AU)
*     e1 = double * (Returned)
*        eccentricity
*     am1 = double * (Returned)
*        mean anomaly (radians, JFORM=2 only)
*     jstat = int * (Returned)
*        status:  +102 = warning, distant epoch
*                 +101 = warning, large timespan ( > 100 years)
*            +1 to +10 = coincident with planet (Note 6)
*                    0 = OK
*                   -1 = illegal JFORM
*                   -2 = illegal E0
*                   -3 = illegal AORQ0
*                   -4 = internal error
*                   -5 = numerical error

*  Description:
*     Update the osculating orbital elements of an asteroid or comet by
*     applying planetary perturbations.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - Two different element-format options are available:
*
*       Option JFORM=2, suitable for minor planets:
*
*       EPOCH   = epoch of elements (TT MJD)
*       ORBI    = inclination i (radians)
*       ANODE   = longitude of the ascending node, big omega (radians)
*       PERIH   = argument of perihelion, little omega (radians)
*       AORQ    = mean distance, a (AU)
*       E       = eccentricity, e
*       AM      = mean anomaly M (radians)
*
*       Option JFORM=3, suitable for comets:
*
*       EPOCH   = epoch of perihelion (TT MJD)
*       ORBI    = inclination i (radians)
*       ANODE   = longitude of the ascending node, big omega (radians)
*       PERIH   = argument of perihelion, little omega (radians)
*       AORQ    = perihelion distance, q (AU)
*       E       = eccentricity, e
*
*     - DATE0, DATE1, EPOCH0 and EPOCH1 are all instants of time in
*       the TT timescale (formerly Ephemeris Time, ET), expressed
*       as Modified Julian Dates (JD-2400000.5).
*
*       DATE0 is the instant at which the given (i.e. unperturbed)
*       osculating elements are correct.
*
*       DATE1 is the specified instant at which the updated osculating
*       elements are correct.
*
*       EPOCH0 and EPOCH1 will be the same as DATE0 and DATE1
*       (respectively) for the JFORM=2 case, normally used for minor
*       planets.  For the JFORM=3 case, the two epochs will refer to
*       perihelion passage and so will not, in general, be the same as
*       DATE0 and/or DATE1 though they may be similar to one another.
*     - The elements are with respect to the J2000 ecliptic and equinox.
*     - Unused elements (AM0 and AM1 for JFORM=3) are not accessed.
*     - See the palPertue routine for details of the algorithm used.
*     - This routine is not intended to be used for major planets, which
*       is why JFORM=1 is not available and why there is no opportunity
*       to specify either the longitude of perihelion or the daily
*       motion.  However, if JFORM=2 elements are somehow obtained for a
*       major planet and supplied to the routine, sensible results will,
*       in fact, be produced.  This happens because the sla_PERTUE routine
*       that is called to perform the calculations checks the separation
*       between the body and each of the planets and interprets a
*       suspiciously small value (0.001 AU) as an attempt to apply it to
*       the planet concerned.  If this condition is detected, the
*       contribution from that planet is ignored, and the status is set to
*       the planet number (1-10 = Mercury, Venus, EMB, Mars, Jupiter,
*       Saturn, Uranus, Neptune, Earth, Moon) as a warning.
*
*  See Also:
*     - Sterne, Theodore E., "An Introduction to Celestial Mechanics",
*       Interscience Publishers Inc., 1960.  Section 6.7, p199.

*  History:
*     2012-03-12 (TIMJ):
*        Initial version direct conversion of SLA/F.
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

void palPertel (int jform, double date0, double date1,
                double epoch0, double orbi0, double anode0,
                double perih0, double aorq0, double e0, double am0,
                double *epoch1, double *orbi1, double *anode1,
                double *perih1, double *aorq1, double *e1, double *am1,
                int *jstat ) {

  double u[13], dm;
  int j, jf;

  /*  Check that the elements are either minor-planet or comet format. */
  if (jform < 2 || jform > 3) {
    *jstat = -1;
    return;
  } else {

    /*     Provisionally set the status to OK. */
    *jstat = 0;
  }

  /*  Transform the elements from conventional to universal form. */
  palEl2ue(date0,jform,epoch0,orbi0,anode0,perih0,
           aorq0,e0,am0,0.0,u,&j);
  if (j != 0) {
    *jstat = j;
    return;
  }

  /*  Update the universal elements. */
  palPertue(date1,u,&j);
  if (j > 0) {
    *jstat = j;
  } else if (j < 0) {
    *jstat = -5;
    return;
  }

  /*  Transform from universal to conventional elements. */
  palUe2el(u, jform, &jf, epoch1, orbi1, anode1, perih1,
           aorq1, e1, am1, &dm, &j);
  if (jf != jform || j != 0) *jstat = -5;
}
