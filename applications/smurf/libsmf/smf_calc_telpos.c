/*
*+
*  Name:
*     smf_calc_telpos

*  Purpose:
*     Low-level routine that calculates the geodetic W lon/lat/altitude
*     of a telescope given its name, or geocentric (x,y,z)
*     coordinates.

*  Invocation:
*     smf_calc_telpos( const double obsgeo[3], const char telName[],
*                      double telpos[3],
*                      int *status );

*  Language:
*     ANSI C

*  Description:
*     If the geocentric coordinates are given, use those to calculate
*     telpos. Otherwise try to determine telpos from the telescope name.

*  Arguments:
*     obsgeo = const double[3] (Given)
*        Geocentric (x,y,z) coordinates of the telescope or NULL.
*     telName = const char[] (Given)
*        Name of the telescope (ignored if obsgeo non-NULL)
*     telpos = double[3] (Returned)
*        The geodetic West longitude/latitude/altitude (deg/deg/m)
*     status = int* (Given & Returned)
*        Inherited status.

*  Notes:
*     -  The conversion from obsgeo has not yet been implemented.

*  Authors:
*     EC: Edward Chapin (UBC)
*     DSB: David Berry (JAC, UCLan)

*  History:
*     6-SEPT-2006 (EC):
*        Original version.
*     2-NOV-2006 (DSB):
*        Added support for calculation based on supplied obsgeo values.
*     7-JUL-2008 (TIMJ):
*        Use const.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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

#include "string.h"

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "star/slalib.h"
#include "star/one.h"

/* SMURF includes */
#include "smf.h"
#include "smf_err.h"
#include "smurf_par.h"


#define FUNC_NAME "smf_calc_telpos"

void smf_calc_telpos( const double obsgeo[3], const char telName[],
                      double telpos[3],
                      int *status ) {

  /* Local variables */

  double height;        /* Height above sea-level (metres) */
  double lat;           /* Latitude (rad) */
  double lon;           /* West Longitude (rad) */
  char retname[41];     /* Returned name of the telescope */
  char name[11];      /* name for supplied buffer */

  if (*status != SAI__OK) return;

  /* Calculate telpos from obsgeo if given. We could have used a
     FitsChan to do this conversion (this keeping all the geocentric-geodetic
     conversion code in one place), if it were not for the fact that
     the observatory altitude is required in addition to the longitude
     and latitude (FitsChan does not generate an altitude value). */
  if( obsgeo != NULL && obsgeo[ 0 ] != AST__BAD &&
                        obsgeo[ 1 ] != AST__BAD &&
                        obsgeo[ 2 ] != AST__BAD ) {
     smf_geod( obsgeo, &lat, &height, &lon );

  /* smf_geod returns longitude +ve east, so negate it to get +ve west. */
     lon = -lon;

  /* Otherwise, calculate telpos from telName */
  } else if( telName != NULL ) {
    /* Note that slaObs does not promise constness */
    one_strlcpy( name, telName, 11, status );
    slaObs( 0, name, retname, &lon, &lat, &height );

    if( retname[0] == '?' ) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Telescope name was not recognized.", status );
    }

  /* Otherwise, report an error. */
  } else {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "Telescope name not given, can't calculate telpos.",
            status );
  }

  /* Store values in telpos */
  if( *status == SAI__OK ) {
    telpos[0] = lon*DR2D;
    telpos[1] = lat*DR2D;
    telpos[2] = height;
  }

}
