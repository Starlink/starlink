/*
*+
*  Name:
*     palPertue

*  Purpose:
*     Update the universal elements by applying planetary perturbations

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palPertue( double date, double u[13], int *jstat );

*  Arguments:
*     date = double (Given)
*        Final epoch (TT MJD) for the update elements.
*     u = const double [13] (Given & Returned)
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
*        status:
*                   +102 = warning, distant epoch
*                   +101 = warning, large timespan ( > 100 years)
*              +1 to +10 = coincident with major planet (Note 5)
*                      0 = OK
*                     -1 = numerical error

*  Description:
*     Update the universal elements of an asteroid or comet by applying
*     planetary perturbations.

*  Authors:
*     PTW: Pat Wallace (STFC)
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
*     - The universal elements are with respect to the J2000 equator and
*       equinox.
*     - The epochs DATE, U(3) and U(12) are all Modified Julian Dates
*       (JD-2400000.5).
*     - The algorithm is a simplified form of Encke's method.  It takes as
*       a basis the unperturbed motion of the body, and numerically
*       integrates the perturbing accelerations from the major planets.
*       The expression used is essentially Sterne's 6.7-2 (reference 1).
*       Everhart and Pitkin (reference 2) suggest rectifying the orbit at
*       each integration step by propagating the new perturbed position
*       and velocity as the new universal variables.  In the present
*       routine the orbit is rectified less frequently than this, in order
*       to gain a slight speed advantage.  However, the rectification is
*       done directly in terms of position and velocity, as suggested by
*       Everhart and Pitkin, bypassing the use of conventional orbital
*       elements.
*
*       The f(q) part of the full Encke method is not used.  The purpose
*       of this part is to avoid subtracting two nearly equal quantities
*       when calculating the "indirect member", which takes account of the
*       small change in the Sun's attraction due to the slightly displaced
*       position of the perturbed body.  A simpler, direct calculation in
*       double precision proves to be faster and not significantly less
*       accurate.
*
*       Apart from employing a variable timestep, and occasionally
*       "rectifying the orbit" to keep the indirect member small, the
*       integration is done in a fairly straightforward way.  The
*       acceleration estimated for the middle of the timestep is assumed
*       to apply throughout that timestep;  it is also used in the
*       extrapolation of the perturbations to the middle of the next
*       timestep, to predict the new disturbed position.  There is no
*       iteration within a timestep.
*
*       Measures are taken to reach a compromise between execution time
*       and accuracy.  The starting-point is the goal of achieving
*       arcsecond accuracy for ordinary minor planets over a ten-year
*       timespan.  This goal dictates how large the timesteps can be,
*       which in turn dictates how frequently the unperturbed motion has
*       to be recalculated from the osculating elements.
*
*       Within predetermined limits, the timestep for the numerical
*       integration is varied in length in inverse proportion to the
*       magnitude of the net acceleration on the body from the major
*       planets.
*
*       The numerical integration requires estimates of the major-planet
*       motions.  Approximate positions for the major planets (Pluto
*       alone is omitted) are obtained from the routine palPlanet.  Two
*       levels of interpolation are used, to enhance speed without
*       significantly degrading accuracy.  At a low frequency, the routine
*       palPlanet is called to generate updated position+velocity "state
*       vectors".  The only task remaining to be carried out at the full
*       frequency (i.e. at each integration step) is to use the state
*       vectors to extrapolate the planetary positions.  In place of a
*       strictly linear extrapolation, some allowance is made for the
*       curvature of the orbit by scaling back the radius vector as the
*       linear extrapolation goes off at a tangent.
*
*       Various other approximations are made.  For example, perturbations
*       by Pluto and the minor planets are neglected and relativistic
*       effects are not taken into account.
*
*       In the interests of simplicity, the background calculations for
*       the major planets are carried out en masse.  The mean elements and
*       state vectors for all the planets are refreshed at the same time,
*       without regard for orbit curvature, mass or proximity.
*
*       The Earth-Moon system is treated as a single body when the body is
*       distant but as separate bodies when closer to the EMB than the
*       parameter RNE, which incurs a time penalty but improves accuracy
*       for near-Earth objects.
*
*     - This routine is not intended to be used for major planets.
*       However, if major-planet elements are supplied, sensible results
*       will, in fact, be produced.  This happens because the routine
*       checks the separation between the body and each of the planets and
*       interprets a suspiciously small value (0.001 AU) as an attempt to
*       apply the routine to the planet concerned.  If this condition is
*       detected, the contribution from that planet is ignored, and the
*       status is set to the planet number (1-10 = Mercury, Venus, EMB,
*       Mars, Jupiter, Saturn, Uranus, Neptune, Earth, Moon) as a warning.

*  See Also:
*     - Sterne, Theodore E., "An Introduction to Celestial Mechanics",
*       Interscience Publishers Inc., 1960.  Section 6.7, p199.
*     - Everhart, E. & Pitkin, E.T., Am.J.Phys. 51, 712, 1983.

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

#include <math.h>

#include "pal.h"
#include "palmac.h"
#include "sofa.h"

/* Only needed in one place */
#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))

void palPertue( double date, double u[13], int *jstat ) {

  /*  Distance from EMB at which Earth and Moon are treated separately */
  const double RNE=1.0;

  /*  Coincidence with major planet distance */
  const double COINC=0.0001;

  /*  Coefficient relating timestep to perturbing force */
  const double TSC=1e-4;

  /*  Minimum and maximum timestep (days) */
  const double TSMIN = 0.01;
  const double TSMAX = 10.0;

  /*  Age limit for major-planet state vector (days) */
  const double AGEPMO=5.0;

  /*  Age limit for major-planet mean elements (days) */
  const double AGEPEL=50.0;

  /*  Margin for error when deciding whether to renew the planetary data */
  const double TINY=1e-6;

  /*  Age limit for the body's osculating elements (before rectification) */
  const double AGEBEL=100.0;

  /*  Gaussian gravitational constant squared */
  const double GCON2 = PAL__GCON * PAL__GCON;

  /*  The final epoch */
  double TFINAL;

  /*  The body's current universal elements */
  double UL[13];

  /*  Current reference epoch */
  double T0;

  /*  Timespan from latest orbit rectification to final epoch (days) */
  double TSPAN;

  /*  Time left to go before integration is complete */
  double TLEFT;

  /*  Time direction flag: +1=forwards, -1=backwards */
  double FB;

  /*  First-time flag */
  int FIRST = 0;

  /*
   *  The current perturbations
   */

  /*  Epoch (days relative to current reference epoch) */
  double RTN;
  /*  Position (AU) */
  double PERP[3];
  /*  Velocity (AU/d) */
  double PERV[3];
  /*  Acceleration (AU/d/d) */
  double PERA[3];

  /*  Length of current timestep (days), and half that */
  double TS,HTS;

  /*  Epoch of middle of timestep */
  double T;

  /*  Epoch of planetary mean elements */
  double TPEL = 0.0;

  /*  Planet number (1=Mercury, 2=Venus, 3=EMB...8=Neptune) */
  int NP;

  /*  Planetary universal orbital elements */
  double UP[8][13];

  /*  Epoch of planetary state vectors */
  double TPMO = 0.0;

  /*  State vectors for the major planets (AU,AU/s) */
  double PVIN[8][6];

  /*  Earth velocity and position vectors (AU,AU/s) */
  double VB[3],PB[3],VH[3],PE[3];

  /*  Moon geocentric state vector (AU,AU/s) and position part */
  double PVM[6],PM[3];

  /*  Date to J2000 de-precession matrix */
  double PMAT[3][3];

  /*
   *  Correction terms for extrapolated major planet vectors
   */

  /*  Sun-to-planet distances squared multiplied by 3 */
  double R2X3[8];
  /*  Sunward acceleration terms, G/2R^3 */
  double GC[8];
  /*  Tangential-to-circular correction factor */
  double FC;
  /*  Radial correction factor due to Sunwards acceleration */
  double FG;

  /*  The body's unperturbed and perturbed state vectors (AU,AU/s) */
  double PV0[6],PV[6];

  /*  The body's perturbed and unperturbed heliocentric distances (AU) cubed */
  double R03,R3;

  /*  The perturbating accelerations, indirect and direct */
  double FI[3],FD[3];

  /*  Sun-to-planet vector, and distance cubed */
  double RHO[3],RHO3;

  /*  Body-to-planet vector, and distance cubed */
  double DELTA[3],DELTA3;

  /*  Miscellaneous */
  int I,J;
  double R2,W,DT,DT2,R,FT;
  int NE;

  /*  Planetary inverse masses, Mercury through Neptune then Earth and Moon */
  const double AMAS[10] = {
    6023600., 408523.5, 328900.5, 3098710.,
    1047.355, 3498.5, 22869., 19314.,
    332946.038, 27068709.
  };

  /*  Preset the status to OK. */
  *jstat = 0;

  /*  Copy the final epoch. */
  TFINAL = date;

  /*  Copy the elements (which will be periodically updated). */
  for (I=0; I<13; I++) {
    UL[I] = u[I];
  }

/*  Initialize the working reference epoch. */
  T0=UL[2];

  /*  Total timespan (days) and hence time left. */
  TSPAN = TFINAL-T0;
  TLEFT = TSPAN;

  /*  Warn if excessive. */
  if (fabs(TSPAN) > 36525.0) *jstat=101;

  /*  Time direction: +1 for forwards, -1 for backwards. */
  FB = copysign(1.0,TSPAN);

  /*  Initialize relative epoch for start of current timestep. */
  RTN = 0.0;

  /*  Reset the perturbations (position, velocity, acceleration). */
  for (I=0; I<3; I++) {
    PERP[I] = 0.0;
    PERV[I] = 0.0;
    PERA[I] = 0.0;
  }

  /*  Set "first iteration" flag. */
  FIRST = 1;

  /*  Step through the time left. */
  while (FB*TLEFT > 0.0) {

    /*     Magnitude of current acceleration due to planetary attractions. */
    if (FIRST) {
      TS = TSMIN;
    } else {
      R2 = 0.0;
      for (I=0; I<3; I++) {
        W = FD[I];
        R2 = R2+W*W;
      }
      W = sqrt(R2);

      /*        Use the acceleration to decide how big a timestep can be tolerated. */
      if (W != 0.0) {
        TS = MIN(TSMAX,MAX(TSMIN,TSC/W));
      } else {
        TS = TSMAX;
      }
    }
    TS = TS*FB;

    /*     Override if final epoch is imminent. */
    TLEFT = TSPAN-RTN;
    if (fabs(TS) > fabs(TLEFT)) TS=TLEFT;

    /*     Epoch of middle of timestep. */
    HTS = TS/2.0;
    T = T0+RTN+HTS;

    /*     Is it time to recompute the major-planet elements? */
    if (FIRST || fabs(T-TPEL)-AGEPEL >= TINY) {

      /*        Yes: go forward in time by just under the maximum allowed. */
      TPEL = T+FB*AGEPEL;

      /*        Compute the state vector for the new epoch. */
      for (NP=1; NP<=8; NP++) {
        palPlanet(TPEL,NP,PV,&J);

        /*           Warning if remote epoch, abort if error. */
        if (J == 1) {
          *jstat = 102;
        } else if (J != 0) {
          goto ABORT;
        }

        /*           Transform the vector into universal elements. */
        palPv2ue(PV,TPEL,0.0,&(UP[NP-1][0]),&J);
        if (J != 0) goto ABORT;
      }
    }

    /*     Is it time to recompute the major-planet motions? */
    if (FIRST || fabs(T-TPMO)-AGEPMO >= TINY) {

      /*        Yes: look ahead. */
      TPMO = T+FB*AGEPMO;

      /*        Compute the motions of each planet (AU,AU/d). */
      for (NP=1; NP<=8; NP++) {

        /*           The planet's position and velocity (AU,AU/s). */
        palUe2pv(TPMO,&(UP[NP-1][0]),&(PVIN[NP-1][0]),&J);
        if (J != 0) goto ABORT;

        /*           Scale velocity to AU/d. */
        for (J=3; J<6; J++) {
          PVIN[NP-1][J] = PVIN[NP-1][J]*PAL__SPD;
        }

        /*           Precompute also the extrapolation correction terms. */
        R2 = 0.0;
        for (I=0; I<3; I++) {
          W = PVIN[NP-1][I];
          R2 = R2+W*W;
        }
        R2X3[NP-1] = R2*3.0;
        GC[NP-1] = GCON2/(2.0*R2*sqrt(R2));
      }
    }

    /*     Reset the first-time flag. */
    FIRST = 0;

    /*     Unperturbed motion of the body at middle of timestep (AU,AU/s). */
    palUe2pv(T,UL,PV0,&J);
    if (J != 0) goto ABORT;

    /*     Perturbed position of the body (AU) and heliocentric distance cubed. */
    R2 = 0.0;
    for (I=0; I<3; I++) {
      W = PV0[I]+PERP[I]+(PERV[I]+PERA[I]*HTS/2.0)*HTS;
      PV[I] = W;
      R2 = R2+W*W;
    }
    R3 = R2*sqrt(R2);

    /*     The body's unperturbed heliocentric distance cubed. */
    R2 = 0.0;
    for (I=0; I<3; I++) {
      W = PV0[I];
      R2 = R2+W*W;
    }
    R03 = R2*sqrt(R2);

    /*     Compute indirect and initialize direct parts of the perturbation. */
    for (I=0; I<3; I++) {
      FI[I] = PV0[I]/R03-PV[I]/R3;
      FD[I] = 0.0;
    }

    /*     Ready to compute the direct planetary effects. */

    /*     Reset the "near-Earth" flag. */
    NE = 0;

    /*     Interval from state-vector epoch to middle of current timestep. */
    DT = T-TPMO;
    DT2 = DT*DT;

    /*     Planet by planet, including separate Earth and Moon. */
    for (NP=1; NP<10; NP++) {

      /*        Which perturbing body? */
      if (NP <= 8) {

        /*           Planet: compute the extrapolation in longitude (squared). */
        R2 = 0.0;
        for (J=3; J<6; J++) {
          W = PVIN[NP-1][J]*DT;
          R2 = R2+W*W;
        }

        /*           Hence the tangential-to-circular correction factor. */
        FC = 1.0+R2/R2X3[NP-1];

        /*           The radial correction factor due to the inwards acceleration. */
        FG = 1.0-GC[NP-1]*DT2;

        /*           Planet's position. */
        for (I=0; I<3; I++) {
          RHO[I] = FG*(PVIN[NP-1][I]+FC*PVIN[NP-1][I+3]*DT);
        }

      } else if (NE) {

        /*           Near-Earth and either Earth or Moon. */

        if (NP == 9) {

          /*              Earth: position. */
          palEpv(T,PE,VH,PB,VB);
          for (I=0; I<3; I++) {
            RHO[I] = PE[I];
          }

        } else {

          /*              Moon: position. */
          palPrec(palEpj(T),2000.0,PMAT);
          palDmoon(T,PVM);
          iauRxp(PMAT,PVM,PM);
          for (I=0; I<3; I++) {
            RHO[I] = PM[I]+PE[I];
          }
        }
      }

      /*        Proceed unless Earth or Moon and not the near-Earth case. */
      if (NP <= 8 || NE) {

        /*           Heliocentric distance cubed. */
        R2 = 0.0;
        for (I=0; I<3; I++) {
          W = RHO[I];
          R2 = R2+W*W;
        }
        R = sqrt(R2);
        RHO3 = R2*R;

        /*           Body-to-planet vector, and distance. */
        R2 = 0.0;
        for (I=0; I<3; I++) {
          W = RHO[I]-PV[I];
          DELTA[I] = W;
          R2 = R2+W*W;
        }
        R = sqrt(R2);

        /*           If this is the EMB, set the near-Earth flag appropriately. */
        if (NP == 3 && R < RNE) NE = 1;

        /*           Proceed unless EMB and this is the near-Earth case. */
        if ( ! (NE && NP == 3) ) {

          /*              If too close, ignore this planet and set a warning. */
          if (R < COINC) {
            *jstat = NP;

          } else {

            /*                 Accumulate "direct" part of perturbation acceleration. */
            DELTA3 = R2*R;
            W = AMAS[NP-1];
            for (I=0; I<3; I++) {
              FD[I] = FD[I]+(DELTA[I]/DELTA3-RHO[I]/RHO3)/W;
            }
          }
        }
      }
    }

    /*     Update the perturbations to the end of the timestep. */
    RTN += TS;
    for (I=0; I<3; I++) {
      W = (FI[I]+FD[I])*GCON2;
      FT = W*TS;
      PERP[I] = PERP[I]+(PERV[I]+FT/2.0)*TS;
      PERV[I] = PERV[I]+FT;
      PERA[I] = W;
    }

    /*     Time still to go. */
    TLEFT = TSPAN-RTN;

    /*     Is it either time to rectify the orbit or the last time through? */
    if (fabs(RTN) >= AGEBEL || FB*TLEFT <= 0.0) {

      /*        Yes: update to the end of the current timestep. */
      T0 += RTN;
      RTN = 0.0;

      /*        The body's unperturbed motion (AU,AU/s). */
      palUe2pv(T0,UL,PV0,&J);
      if (J != 0) goto ABORT;

      /*        Add and re-initialize the perturbations. */
      for (I=0; I<3; I++) {
        J = I+3;
        PV[I] = PV0[I]+PERP[I];
        PV[J] = PV0[J]+PERV[I]/PAL__SPD;
        PERP[I] = 0.0;
        PERV[I] = 0.0;
        PERA[I] = FD[I]*GCON2;
      }

      /*        Use the position and velocity to set up new universal elements. */
      palPv2ue(PV,T0,0.0,UL,&J);
      if (J != 0) goto ABORT;

      /*        Adjust the timespan and time left. */
      TSPAN = TFINAL-T0;
      TLEFT = TSPAN;
    }

    /*     Next timestep. */
  }

  /*  Return the updated universal-element set. */
  for (I=0; I<13; I++) {
    u[I] = UL[I];
  }

  /*  Finished. */
  return;

  /*  Miscellaneous numerical error. */
 ABORT:
  *jstat = -1;
  return;
}
