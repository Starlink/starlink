/*
*+
*  Name:
*     smf_geod

*  Purpose:
*     Convert a terrestrial Cartesian (x,y,z) position to geodetic lat/long

*  Synopsis:
*     #include "smf.h"
*     void smf_geod( const double pos[3],double *phi, double *h,double *lambda )

*  Description:
*     This function converts a position supplied as terrestrial Cartesian
*     (x,y,z) values into geodetic longitude, latitude and height above the
*     reference spheroid. The (x,y,z) system has origin at the centre of
*     the earth, Z axis going through the north pole, X axis at
*     (long,lat)=(0,0), and Y axis at (long,lat) = (E90,0).
*
*     The inverse of this function is smf_terr.
*
*     The algorithm is due to Borkowski, and is described in the
*     Explanatory Supplement to the Astronomical Almanac (p206).

*  Parameters:
*     const pos
*        Array holding the (x,y,z) values, in metres.
*     phi
*        Pointer at a location at which to return the geodetic latitude,
*        in radians.
*     h
*        Pointer at a location at which to return the height above the
*        reference spheroid (geodetic, metres).
*     lambda
*        Pointer at a location at which to return the geodetic longitude,
*        in radians, positive east.

*  Authors:
*     DSB: David Berry (JAC, UCLan)

*  History:
*     2-NOV-2006 (DSB):
*        Initial version (copied from ast/fitschan.c).
*     7-JUL-2008 (TIMJ):
*        Use const.

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*-
*/

/* System includes */
#include <math.h>

/* SMURF includes */
#include "smurf_par.h"
#include "smf.h"

/* Constants */
#define FUNC_NAME "smf_geod"
#define FL  1.0/298.257  /*  Reference spheroid flattening factor */
#define A0  6378140.0    /*  Earth equatorial radius (metres) */

void smf_geod( const double pos[3], double *phi, double *h, double *lambda ){

/* Local Variables... */
   double r, e, f, p, q, d, n, g, t, rp, rd, sn, b0, boa, ab2oa;

/* Initialise */
   *phi = 0.0;
   *h = 0.0;
   *lambda = 0.0;

/* Check the global status. */
   if( !astOK ) return;

/* Earth polar radius (metres) */
   b0 = A0*( 1.0 - FL );

/* Useful functions */
   boa = b0/A0;
   ab2oa = ( A0*A0 - b0*b0)/A0;

/* To obtain the proper sign and polynomial solution, the sign of b is
   set to that of z. Note the sign of z. */
   if( pos[ 2 ] > 0.0 ) {
      sn = 1.0;
   } else {
      sn = -1.0;
   }

/* If the supplied position is on the polar axis, the returned values are
   trivial. We check this case because it corresponds to a singularity in
   the main algorithm. */
   r = sqrt( pos[ 0 ]*pos[ 0 ] + pos[ 1 ]*pos[ 1 ] );
   if( r == 0 ) {
      *lambda = 0.0;
      *phi = M_PI_2;
      *h = pos[ 2 ] - sn*b0;

   } else {

/* The longitude is simple. */
      *lambda = atan2( pos[ 1 ], pos[ 0 ] );

/* The equator is also a singularity in the main algorithm. If the
   supplied point is on the equator, the answers are trivial. */
      if( pos[ 2 ] == 0.0 ) {
         *phi = 0.0;
         *h = r - A0;

/* For all other cases, use the main Borkowski algorithm. */
      } else {
         e = ( sn*boa*pos[ 2 ] - ab2oa )/r;
         f = ( sn*boa*pos[ 2 ] + ab2oa )/r;
         p = 4.0*( e*f + 1.0 )/3.0;
         q = 2.0*( e*e - f*f );
         d = p*p*p + q*q;

         if( d < 0.0 ) {
            rp = sqrt( -p );
            n = 2.0*rp*cos( acos( q/(p*rp) )/3.0 );
         } else {
            rd = sqrt( d );
            n = pow( ( rd - q ), 1.0/3.0 ) - pow( (rd + q ), 1.0/3.0 );
         }

         g = 0.5* ( sqrt( e*e + n ) + e );
         t = sqrt( g*g + ( f - n*g )/( 2*g - e ) ) - g;

         *phi = atan( A0*( 1.0 - t*t  )/( 2.0*sn*b0*t ) );
         *h = ( r - A0*t )*cos( *phi ) + ( pos[ 2 ] - sn*b0 )*sin( *phi );

      }
   }
}

