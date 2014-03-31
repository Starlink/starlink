/*
*+
*  Name:
*     smf_offsets

*  Purpose:
*     Convert an absolute sky position into an offset sky position.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_offsets( double baselon, double baselat, double *lon, double *lat,
*                  int *status )

*  Arguments:
*     lon0 = double (Given)
*        The longitude at the base position, in radians.
*     lat0 = double (Given)
*        The latitude at the base position, in radians.
*     lon = double * (Given and Returned)
*        On entry, the longitude to convert, in radians. On exit, the
*        converted longitude.
*     lat = double * (Given and Returned)
*        On entry, the latitude to convert, in radians. On exit, the
*        converted latitude.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function converts a supplied absolute (longitude,latitude)
*     position into the corresponding longitude and latitude offsets away
*     from a specified base position. North in the offset system is
*     parallel to north in the absolute system.

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     31-MAR-2014 (DSB):
*        Initial version.
*     {enter_further_changes_here}


*  Copyright:
*     Copyright (C) 2014 Science and Technology Facilities Council.
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

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include <stdio.h>

/* Starlink includes */
#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "star/pal.h"

/* SMURF includes */
#include "libsmf/smf.h"

void smf_offsets( double baselon, double baselat, double *lon, double *lat,
                  int *status ){

/* Local Variables */
   double vmod;
   double vp[ 3 ] = { 0.0, 0.0, 1.0 };
   double vq[ 3 ];
   double vx[ 3 ];
   double vy[ 3 ];
   double vz[ 3 ];

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Check all input are good. */
   if( baselon != VAL__BADD && baselat != VAL__BADD &&
       *lon != VAL__BADD && *lat != VAL__BADD ) {

/* Convert the base point into a 3-vector of unit length. This vector defines
   the X axis in the 3D Cartesian coord system corresponding to offset
   coords. */
      palDcs2c( baselon, baselat, vx );

/* The Y axis in the 3D offset coord system is perpendicular to the above
   X axis and also perpendicular to the radius vector to the standard north
   pole (vp). That is, it is parallel to the cross product of the 2 vectors.*/
      palDvxv( vp, vx, vy );

/* Normalize the y vector. */
      palDvn( vy, vy, &vmod );

/* Report an error if the modulus of the vector is zero.*/
      if( vmod == 0.0 ) {
         *status = SAI__ERROR;
         errRep( "", "smf_mapbounds: The moving target is exactly at the "
                 "north pole.", status );

/* If OK, form the Z axis as the cross product of the x and y axes. */
      } else {
         palDvxv( vx, vy, vz );

/* Convert the point to be converted into a 3-vector of unit length. */
         palDcs2c( *lon, *lat, vp );

/* Get the components of this vector parallel to the new X, Y and Z axes. */
         vq[ 0 ] = palDvdv( vp, vx );
         vq[ 1 ] = palDvdv( vp, vy );
         vq[ 2 ] = palDvdv( vp, vz );

/* Convert to angular lon lat values in the offset system. */
         palDcc2s( vq, lon, lat );
      }

/* Return bad values if any input values were bad. */
   } else {
      *lon = VAL__BADD;
      *lat = VAL__BADD;
   }

}
