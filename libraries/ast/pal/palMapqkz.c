/*
*+
*  Name:
*     palMapqkz

*  Purpose:
*     Quick mean to apparent place.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palMapqkz( double rm, double dm, double amprms[21],
*                     double *ra, double *da )

*  Arguments:
*     rm = double (Given)
*        Mean RA (radians).
*     dm = double (Given)
*        Mean Dec (radians).
*     amprms = double[21] (Given)
*        Star-independent mean-to-apparent parameters (see palMappa):
*        (0-3)    not used
*        (4-6)    not used
*        (7)      not used
*        (8-10)   abv: barycentric Earth velocity in units of c
*        (11)     sqrt(1-v**2) where v=modulus(abv)
*        (12-20)  precession/nutation (3,3) matrix
*     ra = double * (Returned)
*        Apparent RA (radians).
*     da = double * (Returned)
*        Apparent Dec (radians).

*  Description:
*     Quick mean to apparent place:  transform a star RA,dec from
*     mean place to geocentric apparent place, given the
*     star-independent parameters, and assuming zero parallax
*     and proper motion.
*
*     Use of this function is appropriate when efficiency is important
*     and where many star positions, all with parallax and proper
*     motion either zero or already allowed for, and all referred to
*     the same equator and equinox, are to be transformed for one
*     epoch.  The star-independent parameters can be obtained by
*     calling the palMappa function.
*
*     The corresponding function for the case of non-zero parallax
*     and proper motion is palMapqk.
*
*     The reference systems and timescales used are IAU 2006.
*
*     Strictly speaking, the function is not valid for solar-system
*     sources, though the error will usually be extremely small.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     {enter_new_authors_here}

*  History:
*     2012-02-13 (PTW):
*        Initial version.
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
*    You should have received a copy of the GNU General Public License
*    along with this program; if not, write to the Free Software
*    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
*    USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "pal.h"
#include "sofa.h"

void palMapqkz ( double rm, double dm, double amprms[21], double *ra,
                 double *da ){

/* Local Variables: */
   int i;
   double ab1, abv[3], p[3], w, p1dv, p1dvp1, p2[3], p3[3];

/* Unpack scalar and vector parameters. */
   ab1 = amprms[11];
   for( i = 0; i < 3; i++ ) {
      abv[i] = amprms[i+8];
   }

/* Spherical to x,y,z. */
   iauS2c( rm, dm, p );

/* Aberration. */
   p1dv = iauPdp( p, abv );
   p1dvp1 = p1dv + 1.0;
   w = 1.0 + p1dv / ( ab1 + 1.0 );
   for( i = 0; i < 3; i++ ) {
      p2[i] = ( ( ab1 * p[i] ) + ( w * abv[i] ) ) / p1dvp1;
   }

/* Precession and nutation. */
   iauRxp( (double(*)[3]) &amprms[12], p2, p3 );

/* Geocentric apparent RA,dec. */
   iauC2s( p3, ra, da );
   *ra = iauAnp( *ra );
}
