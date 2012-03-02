/*
*+
*  Name:
*     palMapqk

*  Purpose:
*     Quick mean to apparent place

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palMapqk ( double rm, double dm, double pr, double pd,
*                     double px, double rv, double amprms[21],
*                     double *ra, double *da );

*  Arguments:
*     rm = double (Given)
*        Mean RA (radians)
*     dm = double (Given)
*        Mean declination (radians)
*     pr = double (Given)
*        RA proper motion, changes per Julian year (radians)
*     pd = double (Given)
*        Dec proper motion, changes per Julian year (radians)
*     px = double (Given)
*        Parallax (arcsec)
*     rv = double (Given)
*        Radial velocity (km/s, +ve if receding)
*     amprms = double [21] (Given)
*        Star-independent mean-to-apparent parameters (see palMappa).
*     ra = double * (Returned)
*        Apparent RA (radians)
*     dec = double * (Returned)
*        Apparent dec (radians)

*  Description:
*     Quick mean to apparent place:  transform a star RA,Dec from
*     mean place to geocentric apparent place, given the
*     star-independent parameters.
*
*     Use of this routine is appropriate when efficiency is important
*     and where many star positions, all referred to the same equator
*     and equinox, are to be transformed for one epoch.  The
*     star-independent parameters can be obtained by calling the
*     palMappa routine.
*
*     If the parallax and proper motions are zero the palMapqkz
*     routine can be used instead.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - The reference frames and timescales used are post IAU 2006.

*  History:
*     2012-03-01 (TIMJ):
*        Initial version with documentation from SLA/F
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

void palMapqk ( double rm, double dm, double pr, double pd,
                double px, double rv, double amprms[21],
                double *ra, double *da ) {

/* local constants */
   const double VF = 0.21094502; /* Km/s to AU/year */

/* Local Variables: */
   int i;
   double ab1, abv[3], p[3], w, p1dv, p2[3], p3[3];
   double pmt, gr2e, eb[3], q[3], pxr, em[3];
   double pde, pdep1, p1[3], ehn[3], pn[3];

/* Unpack scalar and vector parameters. */
   pmt = amprms[0];
   gr2e = amprms[7];
   ab1 = amprms[11];
   for( i = 0; i < 3; i++ ) {
      eb[i] = amprms[i+1];
      ehn[i] = amprms[i+4];
      abv[i] = amprms[i+8];
   }

/* Spherical to x,y,z. */
   iauS2c( rm, dm, q);

 /* Space motion (radians per year) */
   pxr = px * PAL__DAS2R;
   w = VF * rv * pxr;
   em[0] = -pr * q[1] - pd * cos(rm) * sin(dm) + w * q[0];
   em[1] =  pr * q[0] - pd * sin(rm) * sin(dm) + w * q[1];
   em[2] =              pd * cos(dm)           + w * q[2];

/* Geocentric direction of star (normalised) */
   for( i = 0; i < 3; i++ ) {
      p[i] = q[i] + pmt * em[i] - pxr * eb[i];
   }
   iauPn( p, &w, pn );

/* Light deflection (restrained within the Sun's disc) */
   pde = iauPdp( pn, ehn );
   pdep1 = pde + 1.0;
   w = gr2e / ( pdep1 > 1.0e-5 ? pdep1 : 1.0e-5 );
   for( i = 0; i < 3; i++) {
      p1[i] = pn[i] + w * ( ehn[i] - pde * pn[i] );
   }

/* Aberration (normalisation omitted). */
   p1dv = iauPdp( p, abv );
   w = 1.0 + p1dv / ( ab1 + 1.0 );
   for( i = 0; i < 3; i++ ) {
      p2[i] = ( ab1 * p1[i] ) + ( w * abv[i] );
   }

/* Precession and nutation. */
   iauRxp( (double(*)[3]) &amprms[12], p2, p3 );

/* Geocentric apparent RA,dec. */
   iauC2s( p3, ra, da );
   *ra = iauAnp( *ra );

}
