/*
*+
*  Name:
*     smf_calc_telpos

*  Purpose:
*     Low-level routine that calculates the geodetic W lon/lat/altitude
*     of a telescope given its name, or geocentric (x,y,z)
*     coordinates.

*  Invocation:
*     smf_calc_telpos( double obsgeo[3], char telName[], double telpos[3],
*                      int *status );

*  Language:
*     ANSI C

*  Description:
*     If the geocentric coordinates are given, use those to calculate 
*     telpos. Otherwise try to determine telpos from the telescope name.

*  Arguments:
*     obsgeo = double[3] (Given)
*        Geocentric (x,y,z) coordinates of the telescope or NULL.
*     telName = char[] (Given)
*        Name of the telescope (ignored if obsgeo non-NULL)
*     telpos = double[3] (Returned)
*        The geodetic West longitude/latitude/altitude (deg/deg/m)
*     status = int* (Given & Returned)
*        Inherited status.

*  Notes:
*     -  The conversion from obsgeo has not yet been implemented.

*  Authors:
*     EC: Edward Chapin (UBC)

*  History:
*     6-SEPT-2006 (EC):
*        Original version.

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*-
*/

#include "string.h"

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "star/slalib.h"

/* SMURF includes */
#include "smf.h"
#include "smf_err.h"
#include "smurf_par.h"


#define FUNC_NAME "smf_calc_telpos"

void smf_calc_telpos( double obsgeo[3], char telName[], double telpos[3],
		      int *status ) {

  /* Local variables */

  double height;        /* Height above sea-level (metres) */
  double lat;           /* Latitude (rad) */
  double lon;           /* West Longitude (rad) */ 
  char retname[41];     /* Returned name of the telescope */
  

  if (*status != SAI__OK) return;

  /* Calculate telpos from obsgeo if given */
  if( obsgeo != NULL ) {
    msgOutif( MSG__VERB, FUNC_NAME, "Calculation of telescope position from OBSGEO keywords not implemented.", status );
  }

  /* Calculate telpos from telName */
  if( telName != NULL ) {
    slaObs( 0, telName, retname, &lon, &lat, &height );

    if( retname[0] == '?' ) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Telescope name was not recognized.", status );
    }
    
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
