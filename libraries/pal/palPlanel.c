/*
*+
*  Name:
*     palPlanel

*  Purpose:
*     Transform conventional elements into position and velocity

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palPlanel ( double date, int jform, double epoch, double orbinc,
*                      double anode, double perih, double aorq, double e,
*                      double aorl, double dm, double pv[6], int *jstat );

*  Arguments:
*     date = double (Given)
*        Epoch (TT MJD) of osculation (Note 1)
*     jform = int (Given)
*        Element set actually returned (1-3; Note 3)
*     epoch = double (Given)
*        Epoch of elements (TT MJD) (Note 4)
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
*     Heliocentric position and velocity of a planet, asteroid or comet,
*     starting from orbital elements.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - DATE is the instant for which the prediction is required.  It is
*       in the TT timescale (formerly Ephemeris Time, ET) and is a
*       Modified Julian Date (JD-2400000.5).
*     - The elements are with respect to the J2000 ecliptic and equinox.
*     - A choice of three different element-set options is available:
*
*       Option JFORM = 1, suitable for the major planets:
*
*         EPOCH  = epoch of elements (TT MJD)
*         ORBINC = inclination i (radians)
*         ANODE  = longitude of the ascending node, big omega (radians)
*         PERIH  = longitude of perihelion, curly pi (radians)
*         AORQ   = mean distance, a (AU)
*         E      = eccentricity, e (range 0 to <1)
*         AORL   = mean longitude L (radians)
*         DM     = daily motion (radians)
*
*       Option JFORM = 2, suitable for minor planets:
*
*         EPOCH  = epoch of elements (TT MJD)
*         ORBINC = inclination i (radians)
*         ANODE  = longitude of the ascending node, big omega (radians)
*         PERIH  = argument of perihelion, little omega (radians)
*         AORQ   = mean distance, a (AU)
*         E      = eccentricity, e (range 0 to <1)
*         AORL   = mean anomaly M (radians)
*
*       Option JFORM = 3, suitable for comets:
*
*         EPOCH  = epoch of elements and perihelion (TT MJD)
*         ORBINC = inclination i (radians)
*         ANODE  = longitude of the ascending node, big omega (radians)
*         PERIH  = argument of perihelion, little omega (radians)
*         AORQ   = perihelion distance, q (AU)
*         E      = eccentricity, e (range 0 to 10)
*
*       Unused arguments (DM for JFORM=2, AORL and DM for JFORM=3) are not
*       accessed.
*     - Each of the three element sets defines an unperturbed heliocentric
*       orbit.  For a given epoch of observation, the position of the body
*       in its orbit can be predicted from these elements, which are
*       called "osculating elements", using standard two-body analytical
*       solutions.  However, due to planetary perturbations, a given set
*       of osculating elements remains usable for only as long as the
*       unperturbed orbit that it describes is an adequate approximation
*       to reality.  Attached to such a set of elements is a date called
*       the "osculating epoch", at which the elements are, momentarily,
*       a perfect representation of the instantaneous position and
*       velocity of the body.
*
*       Therefore, for any given problem there are up to three different
*       epochs in play, and it is vital to distinguish clearly between
*       them:
*
*       . The epoch of observation:  the moment in time for which the
*         position of the body is to be predicted.
*
*       . The epoch defining the position of the body:  the moment in time
*         at which, in the absence of purturbations, the specified
*         position (mean longitude, mean anomaly, or perihelion) is
*         reached.
*
*       . The osculating epoch:  the moment in time at which the given
*         elements are correct.
*
*       For the major-planet and minor-planet cases it is usual to make
*       the epoch that defines the position of the body the same as the
*       epoch of osculation.  Thus, only two different epochs are
*       involved:  the epoch of the elements and the epoch of observation.
*
*       For comets, the epoch of perihelion fixes the position in the
*       orbit and in general a different epoch of osculation will be
*       chosen.  Thus, all three types of epoch are involved.
*
*       For the present routine:
*
*       . The epoch of observation is the argument DATE.
*
*       . The epoch defining the position of the body is the argument
*         EPOCH.
*
*       . The osculating epoch is not used and is assumed to be close
*         enough to the epoch of observation to deliver adequate accuracy.
*         If not, a preliminary call to sla_PERTEL may be used to update
*         the element-set (and its associated osculating epoch) by
*         applying planetary perturbations.
*     - The reference frame for the result is with respect to the mean
*       equator and equinox of epoch J2000.
*     - The algorithm was originally adapted from the EPHSLA program of
*       D.H.P.Jones (private communication, 1996).  The method is based
*       on Stumpff's Universal Variables.

*  See Also:
*     Everhart, E. & Pitkin, E.T., Am.J.Phys. 51, 712, 1983.

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

#include "pal.h"


void palPlanel ( double date, int jform, double epoch, double orbinc,
		 double anode, double perih, double aorq, double e,
		 double aorl, double dm, double pv[6], int *jstat ) {

  int j;
  double u[13];

  /*  Validate elements and convert to "universal variables" parameters. */
  palEl2ue( date, jform, epoch, orbinc, anode, perih, aorq, e, aorl,
	    dm, u, &j );

  /* Determine the position and velocity */
  if (j == 0) {
    palUe2pv( date, u, pv, &j);
    if (j != 0) j = -5;
  }

  /* Wrap up */
  *jstat = j;

}
