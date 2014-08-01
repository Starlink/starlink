/*
*+
*  Name:
*     palFk45z

*  Purpose:
*     Convert B1950.0 FK4 star data to J2000.0 FK5 assuming zero
*     proper motion in the FK5 frame

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     palFk45z( double r1950, double d1950, double bepoch, double *r2000,
*               double *d2000 )

*  Arguments:
*     r1950 = double (Given)
*        B1950.0 FK4 RA at epoch (radians).
*     d1950 = double (Given)
*        B1950.0 FK4 Dec at epoch (radians).
*     bepoch = double (Given)
*        Besselian epoch (e.g. 1979.3)
*     r2000 = double (Returned)
*        J2000.0 FK5 RA (Radians).
*     d2000 = double (Returned)
*        J2000.0 FK5 Dec(Radians).

*  Description:
*     Convert B1950.0 FK4 star data to J2000.0 FK5 assuming zero
*     proper motion in the FK5 frame (double precision)
*
*     This function converts stars from the Bessel-Newcomb, FK4
*     system to the IAU 1976, FK5, Fricke system, in such a
*     way that the FK5 proper motion is zero.  Because such a star
*     has, in general, a non-zero proper motion in the FK4 system,
*     the routine requires the epoch at which the position in the
*     FK4 system was determined.
*
*     The method is from Appendix 2 of Ref 1, but using the constants
*     of Ref 4.

*  Notes:
*     - The epoch BEPOCH is strictly speaking Besselian, but if a
*     Julian epoch is supplied the result will be affected only to
*     a negligible extent.
*
*     - Conversion from Besselian epoch 1950.0 to Julian epoch 2000.0
*     only is provided for.  Conversions involving other epochs will
*     require use of the appropriate precession, proper motion, and
*     E-terms routines before and/or after palFk45z is called.
*
*     - In the FK4 catalogue the proper motions of stars within 10
*     degrees of the poles do not embody the differential E-term effect
*     and should, strictly speaking, be handled in a different manner
*     from stars outside these regions. However, given the general lack
*     of homogeneity of the star data available for routine astrometry,
*     the difficulties of handling positions that may have been
*     determined from astrometric fields spanning the polar and non-polar
*     regions, the likelihood that the differential E-terms effect was not
*     taken into account when allowing for proper motion in past
*     astrometry, and the undesirability of a discontinuity in the
*     algorithm, the decision has been made in this routine to include the
*     effect of differential E-terms on the proper motions for all stars,
*     whether polar or not.  At epoch 2000, and measuring on the sky rather
*     than in terms of dRA, the errors resulting from this simplification
*     are less than 1 milliarcsecond in position and 1 milliarcsecond per
*     century in proper motion.
*
*  References:
*     - Aoki,S., et al, 1983.  Astron.Astrophys., 128, 263.
*     - Smith, C.A. et al, 1989.  "The transformation of astrometric
*       catalog systems to the equinox J2000.0".  Astron.J. 97, 265.
*     - Yallop, B.D. et al, 1989.  "Transformation of mean star places
*       from FK4 B1950.0 to FK5 J2000.0 using matrices in 6-space".
*       Astron.J. 97, 274.
*     - Seidelmann, P.K. (ed), 1992.  "Explanatory Supplement to
*       the Astronomical Almanac", ISBN 0-935702-68-7.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2012-02-10 (DSB):
*        Initial version with documentation taken from Fortran SLA
*        Adapted with permission from the Fortran SLALIB library.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 1998 Rutherford Appleton Laboratory
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
#include "palmac.h"
#include "pal1sofa.h"

void palFk45z( double r1950, double d1950, double bepoch, double *r2000,
               double *d2000 ){

/* Local Variables: */
   double w;
   int i;
   int j;
   double r0[3], a1[3], v1[3], v2[6]; /* Position and position+velocity vectors */


/* CANONICAL CONSTANTS  (see references) */

/* Vector A. */
   double a[3] = { -1.62557E-6, -0.31919E-6, -0.13843E-6 };

/* Vectors Adot. */
   double ad[3] = { 1.245E-3, -1.580E-3, -0.659E-3 };

/* Matrix M (only half of which is needed here). */
   double em[6][3] = { {0.9999256782, -0.0111820611, -0.0048579477},
                       {0.0111820610, 0.9999374784, -0.0000271765},
                       {0.0048579479, -0.0000271474, 0.9999881997},
                       {-0.000551, -0.238565, 0.435739},
                       {0.238514, -0.002667, -0.008541},
                       {-0.435623, 0.012254, 0.002117} };


/* Spherical to Cartesian. */
   eraS2c( r1950, d1950, r0 );

/* Adjust vector A to give zero proper motion in FK5. */
   w = ( bepoch - 1950.0 )/PAL__PMF;
   for( i = 0; i < 3; i++ ) {
      a1[ i ] = a[ i ] + w*ad[ i ];
   }

/* Remove e-terms. */
   w = r0[ 0 ]*a1[ 0 ] + r0[ 1 ]*a1[ 1 ] + r0[ 2 ]*a1[ 2 ];
   for( i = 0; i < 3; i++ ) {
      v1[ i ] = r0[ i ] - a1[ i ] + w*r0[ i ];
   }

/* Convert position vector to Fricke system. */
   for( i = 0; i < 6; i++ ) {
      w = 0.0;
      for( j = 0; j < 3; j++ ) {
         w += em[ i ][ j ]*v1[ j ];
      }
      v2[ i ] = w;
   }

/* Allow for fictitious proper motion in FK4. */
   w = ( palEpj( palEpb2d( bepoch ) ) - 2000.0 )/PAL__PMF;
   for( i = 0; i < 3; i++ ) {
      v2[ i ] += w*v2[ i + 3 ];
   }

/* Revert to spherical coordinates. */
   eraC2s( v2, &w, d2000 );
   *r2000 = eraAnp( w );
}


