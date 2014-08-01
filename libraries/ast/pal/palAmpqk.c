/*
*+
*  Name:
*     palAmpqk

*  Purpose:
*     Convert star RA,Dec from geocentric apparent to mean place.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palAmpqk ( double ra, double da, double amprms[21],
*                     double *rm, double *dm )

*  Arguments:
*     ra = double (Given)
*        Apparent RA (radians).
*     da = double (Given)
*        Apparent Dec (radians).
*     amprms = double[21] (Given)
*        Star-independent mean-to-apparent parameters (see palMappa):
*        (0)      time interval for proper motion (Julian years)
*        (1-3)    barycentric position of the Earth (AU)
*        (4-6)    not used
*        (7)      not used
*        (8-10)   abv: barycentric Earth velocity in units of c
*        (11)     sqrt(1-v*v) where v=modulus(abv)
*        (12-20)  precession/nutation (3,3) matrix
*     rm = double (Returned)
*        Mean RA (radians).
*     dm = double (Returned)
*        Mean Dec (radians).

*  Description:
*     Convert star RA,Dec from geocentric apparent to mean place. The "mean"
*     coordinate system is in fact close to ICRS. Use of this function
*     is appropriate when efficiency is important and where many star
*     positions are all to be transformed for one epoch and equinox.  The
*     star-independent parameters can be obtained by calling the palMappa
*     function.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     {enter_new_authors_here}

*  History:
*     2012-02-13 (PTW):
*        Initial version.
*        Adapted with permission from the Fortran SLALIB library.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2000 Rutherford Appleton Laboratory
*     Copyright (C) 2012 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "pal.h"
#include "pal1sofa.h"

void palAmpqk ( double ra, double da, double amprms[21], double *rm,
                double *dm ){

/* Local Variables: */
   double ab1;                 /* sqrt(1-v*v) where v=modulus of Earth vel */
   double abv[3];              /* Earth velocity wrt SSB (c, FK5) */
   double p1[3], p2[3], p3[3]; /* work vectors */
   double ab1p1, p1dv, p1dvp1, w;
   int i, j;

/* Unpack some of the parameters */
   ab1  = amprms[11];
   for( i = 0; i < 3; i++ ) {
      abv[i] = amprms[i + 8];
   }

/* Apparent RA,Dec to Cartesian */
   eraS2c( ra, da, p3 );

/* Precession and nutation */
   eraTrxp( (double(*)[3]) &amprms[12], p3, p2 );

/* Aberration */
   ab1p1 = ab1 + 1.0;
   for( i = 0; i < 3; i++ ) {
      p1[i] = p2[i];
   }
   for( j = 0; j < 2; j++ ) {
      p1dv = eraPdp( p1, abv );
      p1dvp1 = 1.0 + p1dv;
      w = 1.0 + p1dv / ab1p1;
      for( i = 0; i < 3; i++ ) {
         p1[i] = ( p1dvp1 * p2[i] - w * abv[i] ) / ab1;
      }
      eraPn( p1, &w, p3 );
      for( i = 0; i < 3; i++ ) {
         p1[i] = p3[i];
      }
   }

/* Mean RA,Dec */
   eraC2s( p1, rm, dm );
   *rm = eraAnp( *rm );
}
