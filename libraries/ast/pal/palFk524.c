/*
*+
*  Name:
*     palFk524

*  Purpose:
*     Convert J2000.0 FK5 star data to B1950.0 FK4.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     palFk524( double r2000, double d2000, double dr2000, double dd2000,
*               double p2000, double v2000, double *r1950, double *d1950,
*               double *dr1950, double *dd1950, double *p1950, double *v1950 )

*  Arguments:
*     r2000 = double (Given)
*        J2000.0 FK5 RA (radians).
*     d2000 = double (Given)
*        J2000.0 FK5 Dec (radians).
*     dr2000 = double (Given)
*        J2000.0 FK5 RA proper motion (rad/Jul.yr)
*     dd2000 = double (Given)
*        J2000.0 FK5 Dec proper motion (rad/Jul.yr)
*     p2000 = double (Given)
*        J2000.0 FK5 parallax (arcsec)
*     v2000 = double (Given)
*         J2000.0 FK5 radial velocity (km/s, +ve = moving away)
*     r2000 = double * (Returned)
*        B1950.0 FK4 RA (radians).
*     d2000 = double * (Returned)
*        B1950.0 FK4 Dec (radians).
*     dr2000 = double * (Returned)
*        B1950.0 FK4 RA proper motion (rad/Jul.yr)
*     dd2000 = double * (Returned)
*        B1950.0 FK4 Dec proper motion (rad/Jul.yr)
*     p2000 = double * (Returned)
*        B1950.0 FK4 parallax (arcsec)
*     v2000 = double * (Returned)q
*         B1950.0 FK4 radial velocity (km/s, +ve = moving away)

*  Description:
*     This function converts stars from the IAU 1976, FK5, Fricke
*     system, to the Bessel-Newcomb, FK4 system.  The precepts
*     of Smith et al (Ref 1) are followed, using the implementation
*     by Yallop et al (Ref 2) of a matrix method due to Standish.
*     Kinoshita's development of Andoyer's post-Newcomb precession is
*     used.  The numerical constants from Seidelmann et al (Ref 3) are
*     used canonically.

*  Notes:
*     - The proper motions in RA are dRA/dt rather than
*     cos(Dec)*dRA/dt, and are per year rather than per century.
*     - Note that conversion from Julian epoch 2000.0 to Besselian
*     epoch 1950.0 only is provided for.  Conversions involving
*     other epochs will require use of the appropriate precession,
*     proper motion, and E-terms routines before and/or after
*     FK524 is called.
*     - In the FK4 catalogue the proper motions of stars within
*     10 degrees of the poles do not embody the differential
*     E-term effect and should, strictly speaking, be handled
*     in a different manner from stars outside these regions.
*     However, given the general lack of homogeneity of the star
*     data available for routine astrometry, the difficulties of
*     handling positions that may have been determined from
*     astrometric fields spanning the polar and non-polar regions,
*     the likelihood that the differential E-terms effect was not
*     taken into account when allowing for proper motion in past
*     astrometry, and the undesirability of a discontinuity in
*     the algorithm, the decision has been made in this routine to
*     include the effect of differential E-terms on the proper
*     motions for all stars, whether polar or not.  At epoch 2000,
*     and measuring on the sky rather than in terms of dRA, the
*     errors resulting from this simplification are less than
*     1 milliarcsecond in position and 1 milliarcsecond per
*     century in proper motion.
*
*  References:
*     1  - Smith, C.A. et al, 1989.  "The transformation of astrometric
*        catalog systems to the equinox J2000.0".  Astron.J. 97, 265.
*     2  - Yallop, B.D. et al, 1989.  "Transformation of mean star places
*        from FK4 B1950.0 to FK5 J2000.0 using matrices in 6-space".
*        Astron.J. 97, 274.
*     3  - Seidelmann, P.K. (ed), 1992.  "Explanatory Supplement to
*        the Astronomical Almanac", ISBN 0-935702-68-7.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2012-02-13 (DSB):
*        Initial version with documentation taken from Fortran SLA
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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
*     USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "pal.h"
#include "palmac.h"
#include "math.h"

void palFk524( double r2000, double d2000, double dr2000, double dd2000,
               double p2000, double v2000, double *r1950, double *d1950,
               double *dr1950, double *dd1950, double *p1950, double *v1950 ){

/* Local Variables; */
   double r, d, ur, ud, px, rv;
   double sr, cr, sd, cd, x, y, z, w;
   double v1[ 6 ], v2[ 6 ];
   double xd, yd, zd;
   double rxyz, wd, rxysq, rxy;
   int i, j;

/* Small number to avoid arithmetic problems. */
   static const double tiny = 1.0-30;

/* Canonical constants (see references). Constant vector and matrix. */
   double a[ 6 ] = { -1.62557E-6,  -0.31919E-6, -0.13843E-6,
                     +1.245E-3,    -1.580E-3,   -0.659E-3 };
   double emi[ 6 ][ 6 ] = {
                 { 0.9999256795,      0.0111814828,      0.0048590039,
                  -0.00000242389840, -0.00000002710544, -0.00000001177742},
                 {-0.0111814828,      0.9999374849,     -0.0000271771,
                   0.00000002710544, -0.00000242392702,  0.00000000006585 },
                 {-0.0048590040,     -0.0000271557,      0.9999881946,
                   0.00000001177742,  0.00000000006585, -0.00000242404995 },
                 {-0.000551,          0.238509,         -0.435614,
                   0.99990432,        0.01118145,        0.00485852 },
                 {-0.238560,         -0.002667,          0.012254,
                  -0.01118145,        0.99991613,       -0.00002717},
                 { 0.435730,         -0.008541,          0.002117,
                  -0.00485852,       -0.00002716,        0.99996684 } };

/* Pick up J2000 data (units radians and arcsec/JC). */
   r = r2000;
   d = d2000;
   ur = dr2000*PAL__PMF;
   ud = dd2000*PAL__PMF;
   px = p2000;
   rv = v2000;

/* Spherical to Cartesian. */
   sr = sin( r );
   cr = cos( r );
   sd = sin( d );
   cd = cos( d );

   x = cr*cd;
   y = sr*cd;
   z =    sd;

   w = PAL__VF*rv*px;

   v1[ 0 ] = x;
   v1[ 1 ] = y;
   v1[ 2 ] = z;

   v1[ 3 ] = -ur*y - cr*sd*ud + w*x;
   v1[ 4 ] =  ur*x - sr*sd*ud + w*y;
   v1[ 5 ] =            cd*ud + w*z;

/* Convert position+velocity vector to BN system. */
   for( i = 0; i < 6; i++ ) {
      w = 0.0;
      for( j = 0; j < 6; j++ ) {
         w += emi[ i ][ j ]*v1[ j ];
      }
      v2[ i ] = w;
   }

/* Position vector components and magnitude. */
   x = v2[ 0 ];
   y = v2[ 1 ];
   z = v2[ 2 ];
   rxyz = sqrt( x*x + y*y + z*z );

/* Apply E-terms to position. */
   w = x*a[ 0 ] + y*a[ 1 ] + z*a[ 2 ];
   x += a[ 0 ]*rxyz - w*x;
   y += a[ 1 ]*rxyz - w*y;
   z += a[ 2 ]*rxyz - w*z;

/* Recompute magnitude. */
   rxyz = sqrt( x*x + y*y + z*z );

/* Apply E-terms to both position and velocity. */
   x = v2[ 0 ];
   y = v2[ 1 ];
   z = v2[ 2 ];
   w = x*a[ 0 ] + y*a[ 1 ] + z*a[ 2 ];
   wd = x*a[ 3 ] + y*a[ 4 ] + z*a[ 5 ];
   x += a[ 0 ]*rxyz - w*x;
   y += a[ 1 ]*rxyz - w*y;
   z += a[ 2 ]*rxyz - w*z;
   xd = v2[ 3 ] + a[ 3 ]*rxyz - wd*x;
   yd = v2[ 4 ] + a[ 4 ]*rxyz - wd*y;
   zd = v2[ 5 ] + a[ 5 ]*rxyz - wd*z;

/* Convert to spherical. */
   rxysq = x*x + y*y;
   rxy = sqrt( rxysq );

   if( x == 0.0 && y == 0.0 ) {
      r = 0.0;
   } else {
      r = atan2( y, x );
      if( r <  0.0 ) r += PAL__D2PI;
   }
   d = atan2( z, rxy );

   if( rxy > tiny ) {
      ur = ( x*yd - y*xd )/rxysq;
      ud = ( zd*rxysq - z*( x*xd + y*yd ) )/( ( rxysq + z*z )*rxy );
   }

/* Radial velocity and parallax. */
   if( px > tiny ) {
      rv = ( x*xd + y*yd + z*zd )/( px*PAL__VF*rxyz );
      px /= rxyz;
   }

/* Return results. */
   *r1950 = r;
   *d1950 = d;
   *dr1950 = ur/PAL__PMF;
   *dd1950 = ud/PAL__PMF;
   *p1950 = px;
   *v1950 = rv;
}
