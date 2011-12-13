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

*  Authors:
*     EC: Ed Chapin (UBC)

*  History:
*     23-MAY-2007 (EC):
*        Initial version (copied from ast/fitschan.c).

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

/* Starlink includes */
#include "ast.h"
#include "star/slalib.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smf.h"

#define FUNC_NAME "smf_terr"

void smf_terr( double phi, double h, double lambda, double pos[3] ) {

  /* Local Variables... */
  double r, z;

  /* Check the global status. */
  if( !astOK ) return;

  /* Calculate cartesian coordinates in metres */
  slaGeoc( phi, h, &r, &z );
  r *= AST__AU; /* covert AU to metres */
  pos[0] = r*cos( lambda );
  pos[1] = r*sin( lambda );
  pos[2] = z*AST__AU;

}

