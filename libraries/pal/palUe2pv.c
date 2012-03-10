/*
*+
*  Name:
*     palUe2pv

*  Purpose:
*     Heliocentric position and velocity of a planet, asteroid or comet, from universal elements

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palUe2pv( double date, double u[13], double pv[6], int *jstat );

*  Arguments:
*     date = double (Given)
*        TT Modified Julian date (JD-2400000.5).
*     u = double [13] (Given & Returned)
*        Universal orbital elements (updated, see note 1)
*        given    (1)   combined mass (M+m)
*          "      (2)   total energy of the orbit (alpha)
*          "      (3)   reference (osculating) epoch (t0)
*          "    (4-6)   position at reference epoch (r0)
*          "    (7-9)   velocity at reference epoch (v0)
*          "     (10)   heliocentric distance at reference epoch
*          "     (11)   r0.v0
*       returned (12)   date (t)
*          "     (13)   universal eccentric anomaly (psi) of date
*     jstat = int * (Returned)
*       status:  0 = OK
*               -1 = radius vector zero
*               -2 = failed to converge

*  Description:
*     Heliocentric position and velocity of a planet, asteroid or comet,
*     starting from orbital elements in the "universal variables" form.

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
*     - The companion routine is palEl2ue.  This takes the conventional
*       orbital elements and transforms them into the set of numbers
*       needed by the present routine.  A single prediction requires one
*       one call to palEl2ue followed by one call to the present routine;
*       for convenience, the two calls are packaged as the routine
*       sla_PLANEL.  Multiple predictions may be made by again
*       calling palEl2ue once, but then calling the present routine
*       multiple times, which is faster than multiple calls to palPlanel.
*     - It is not obligatory to use palEl2ue to obtain the parameters.
*       However, it should be noted that because palEl2ue performs its
*       own validation, no checks on the contents of the array U are made
*       by the present routine.
      - DATE is the instant for which the prediction is required.  It is
*       in the TT timescale (formerly Ephemeris Time, ET) and is a
*       Modified Julian Date (JD-2400000.5).
      - The universal elements supplied in the array U are in canonical
*       units (solar masses, AU and canonical days).  The position and
*       velocity are not sensitive to the choice of reference frame.  The
*       palEl2ue routine in fact produces coordinates with respect to the
*       J2000 equator and equinox.
*     - The algorithm was originally adapted from the EPHSLA program of
*       D.H.P.Jones (private communication, 1996).  The method is based
*       on Stumpff's Universal Variables.
*     - Reference:  Everhart, E. & Pitkin, E.T., Am.J.Phys. 51, 712, 1983.

*  History:
*     2012-03-09 (TIMJ):
*        Initial version cloned from SLA/F.
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

void palUe2pv( double date, double u[13], double pv[6], int *jstat ) {

  /*  Gaussian gravitational constant (exact) */
  double const GCON = 0.01720209895;

  /*  Canonical days to seconds */
  const double CD2S = GCON / PAL__SPD;

  /*  Test value for solution and maximum number of iterations */
  const double TEST = 1e-13;
  const int NITMAX = 25;

  int I, NIT, N;
  double CM,ALPHA,T0,P0[3],V0[3],R0,SIGMA0,T,PSI,DT,W,
    TOL,PSJ,PSJ2,BETA,S0,S1,S2,S3,
    FF,R,FLAST,PLAST,F,G,FD,GD;

  /*  Unpack the parameters. */
  CM = u[0];
  ALPHA = u[1];
  T0 = u[2];
  for (I=0; I<3; I++) {
    P0[I] = u[I+3];
    V0[I] = u[I+6];
  }
  R0 = u[9];
  SIGMA0 = u[10];
  T = u[11];
  PSI = u[12];

  /*  Approximately update the universal eccentric anomaly. */
  PSI = PSI+(date-T)*GCON/R0;

  /*  Time from reference epoch to date (in Canonical Days: a canonical
   *  day is 58.1324409... days, defined as 1/GCON). */
  DT = (date-T0)*GCON;

  /*  Refine the universal eccentric anomaly, psi. */
  NIT = 1;
  W = 1.0;
  TOL = 0.0;
  while (fabs(W) >= TOL) {

    /*     Form half angles until BETA small enough. */
    N = 0;
    PSJ = PSI;
    PSJ2 = PSJ*PSJ;
    BETA = ALPHA*PSJ2;
    while (fabs(BETA) > 0.7) {
      N = N+1;
      BETA = BETA/4.0;
      PSJ = PSJ/2.0;
      PSJ2 = PSJ2/4.0;
    }

    /*     Calculate Universal Variables S0,S1,S2,S3 by nested series. */
    S3 = PSJ*PSJ2*((((((BETA/210.0+1.0)
                       *BETA/156.0+1.0)
                      *BETA/110.0+1.0)
                     *BETA/72.0+1.0)
                    *BETA/42.0+1.0)
                   *BETA/20.0+1.0)/6.0;
    S2 = PSJ2*((((((BETA/182.0+1.0)
                   *BETA/132.0+1.0)
                  *BETA/90.0+1.0)
                 *BETA/56.0+1.0)
                *BETA/30.0+1.0)
               *BETA/12.0+1.0)/2.0;
    S1 = PSJ+ALPHA*S3;
    S0 = 1.0+ALPHA*S2;

    /*     Undo the angle-halving. */
    TOL = TEST;
    while (N > 0) {
      S3 = 2.0*(S0*S3+PSJ*S2);
      S2 = 2.0*S1*S1;
      S1 = 2.0*S0*S1;
      S0 = 2.0*S0*S0-1.0;
      PSJ = PSJ+PSJ;
      TOL += TOL;
      N--;
    }

    /*     Values of F and F' corresponding to the current value of psi. */
    FF = R0*S1+SIGMA0*S2+CM*S3-DT;
    R = R0*S0+SIGMA0*S1+CM*S2;

    /*     If first iteration, create dummy "last F". */
    if ( NIT == 1) FLAST = FF;

    /*     Check for sign change. */
    if ( FF*FLAST < 0.0 ) {

      /*        Sign change:  get psi adjustment using secant method. */
      W = FF*(PLAST-PSI)/(FLAST-FF);
    } else {

      /*        No sign change:  use Newton-Raphson method instead. */
      if (R == 0.0) {
        /* Null radius vector */
        *jstat = -1;
        return;
      }
      W = FF/R;
    }

    /*     Save the last psi and F values. */
    PLAST = PSI;
    FLAST = FF;

    /*     Apply the Newton-Raphson or secant adjustment to psi. */
    PSI = PSI-W;

    /*     Next iteration, unless too many already. */
    if (NIT > NITMAX) {
      *jstat = -2; /* Failed to converge */
      return;
    }
    NIT++;
  }

  /*  Project the position and velocity vectors (scaling velocity to AU/s). */
  W = CM*S2;
  F = 1.0-W/R0;
  G = DT-CM*S3;
  FD = -CM*S1/(R0*R);
  GD = 1.0-W/R;
  for (I=0; I<3; I++) {
    pv[I] = P0[I]*F+V0[I]*G;
    pv[I+3] = CD2S*(P0[I]*FD+V0[I]*GD);
  }

  /*  Update the parameters to allow speedy prediction of PSI next time. */
  u[11] = date;
  u[12] = PSI;

  /*  OK exit. */
  *jstat = 0;
  return;
}
