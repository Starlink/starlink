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
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  Notes:
*     This routine is a thin wrapper around the ERFA function. It exists
*     simply to enforce the use of WGS84 reference ellipsoid.

*  History:
*     2-NOV-2006 (DSB):
*        Initial version (copied from ast/fitschan.c).
*     7-JUL-2008 (TIMJ):
*        Use const.
*     2012-03-06 (TIMJ):
*        Now uses iauGc2gd with WGS84 reference ellipsoid.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
#include "erfa.h"
#include "erfam.h"

/* Constants */
#define FUNC_NAME "smf_geod"

void smf_geod( const double pos[3], double *phi, double *h, double *lambda ){
  /* ERFA does not like to const */
  double lpos[3];

  lpos[0] = pos[0];
  lpos[1] = pos[1];
  lpos[2] = pos[2];

  eraGc2gd( ERFA_WGS84, lpos, lambda, phi, h );
}

