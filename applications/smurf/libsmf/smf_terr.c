/*
*+
*  Name:
*     smf_terr

*  Purpose:
*     Convert geodetic lat/long to a terrestrial Cartesian (x,y,z) position

*  Synopsis:
*     #include "smf.h"
*     void smf_terr( double phi, double h, double lambda, double pos[3] )


*  Description:
*     This function converts a geodetic longitude, latitude and height
*     above the reference spheroid into a terrestrial Cartesian
*     position (x,y,z). The (x,y,z) system has origin at the centre of
*     the earth, Z axis going through the north pole, X axis at
*     (long,lat)=(0,0), and Y axis at (long,lat) = (E90,0).
*     This function is the inverse of smf_geod.

*  Parameters:
*     phi
*        Geodetic latitude in radians.
*     h
*        Height above the reference spheroid (geodetic, metres).
*     lambda
*        Geodetic longitude, in radians, positive east.
*     pos
*        Array holding the returned (x,y,z) values, in metres.

*  Notes:
*     This routine is a thin wrapper around the ERFA function. It exists
*     simply to enforce the use of WGS84 reference ellipsoid.

*  Authors:
*     EC: Ed Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     23-MAY-2007 (EC):
*        Initial version (copied from ast/fitschan.c).
*     2012-03-06 (TIMJ):
*        Replace with call to SOFA iauGd2gc using WGS84 reference ellipsoid.

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

/* Starlink includes */
#include "erfa.h"
#include "erfam.h"

/* SMURF includes */
#include "smf.h"

#define FUNC_NAME "smf_terr"

void smf_terr( double phi, double h, double lambda, double pos[3] ) {

  /* Calculate cartesian coordinates in metres */
  eraGd2gc( ERFA_WGS84, lambda, phi, h, pos );

}

