/*============================================================================
*
*   WCSLIB - an implementation of the FITS WCS proposal.
*   Copyright (C) 1995, Mark Calabretta
*
*   This library is free software; you can redistribute it and/or modify it
*   under the terms of the GNU Library General Public License as published
*   by the Free Software Foundation; either version 2 of the License, or (at
*   your option) any later version.
*
*   This library is distributed in the hope that it will be useful, but
*   WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library
*   General Public License for more details.
*
*   You should have received a copy of the GNU Library General Public License
*   along with this library; if not, write to the Free Software Foundation,
*   Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*
*   Correspondence concerning WCSLIB may be directed to:
*      Internet email: mcalabre@atnf.csiro.au
*      Postal address: Dr. Mark Calabretta,
*                      Australia Telescope National Facility,
*                      P.O. Box 76,
*                      Epping, NSW, 2121,
*                      AUSTRALIA
*
*=============================================================================
*
*   C routines for the spherical coordinate transformations used by the FITS
*   "World Coordinate System" (WCS) convention.
*
*   Summary of routines
*   -------------------
*   The spherical coordinate transformations are implemented via separate
*   functions for the transformation in each direction.
*
*   Forward transformation; sphfwd()
*   --------------------------------
*   Transform celestial coordinates to the native coordinates of a projection.
*
*   Given:
*      lng,lat  double   Celestial longitude and latitude, in degrees.
*      eul[5]   double   Euler angles for the transformation:
*                          0: Celestial longitude of the native pole, in
*                             degrees.
*                          1: Celestial colatitude of the native pole, or
*                             native colatitude of the celestial pole, in
*                             degrees.
*                          2: Native longitude of the celestial pole, in
*                             degrees.
*                          3: cos(eul[1])
*                          4: sin(eul[1])
*
*   Returned:
*      phi,     double   Longitude and latitude in the native coordinate
*      theta             system of the projection, in degrees.
*
*   Function return value:
*               int      Error status
*                           0: Success.
*
*   Reverse transformation; sphrev()
*   --------------------------------
*   Transform native coordinates of a projection to celestial coordinates.
*
*   Given:
*      phi,     double   Longitude and latitude in the native coordinate
*      theta             system of the projection, in degrees.
*      eul[5]   double   Euler angles for the transformation:
*                          0: Celestial longitude of the native pole, in
*                             degrees.
*                          1: Celestial colatitude of the native pole, or
*                             native colatitude of the celestial pole, in
*                             degrees.
*                          2: Native longitude of the celestial pole, in
*                             degrees.
*                          3: cos(eul[1])
*                          4: sin(eul[1])
*
*   Returned:
*      lng,lat  double   Celestial longitude and latitude, in degrees.
*
*   Function return value:
*               int      Error status
*                           0: Success.
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*   $Id: sph.c,v 2.1 1995/11/09 03:17:43 mcalabre Exp $
*===========================================================================*/

#include "wcstrig.h"

#ifndef __STDC__
#ifndef const
#define const
#endif
#endif

#ifdef COPYSIGN
#define wcs_copysign(X, Y) ((Y) < 0.0 ? -fabs(X) : fabs(X))
#endif

const double tol = 1.0e-5;

int sphfwd (lng, lat, eul, phi, theta)

const double lat, lng, eul[5];
double *phi, *theta;

{
   double coslat, coslng, dlng, dphi, sinlat, sinlng, x, y, z;

   coslat = wcs_cosd(lat);
   sinlat = wcs_sind(lat);

   dlng = lng - eul[0];
   coslng = wcs_cosd(dlng);
   sinlng = wcs_sind(dlng);

   /* Compute the native longitude. */
   x = sinlat*eul[4] - coslat*eul[3]*coslng;
   if (fabs(x) < tol) {
      /* Rearrange formula to reduce roundoff errors. */
      x = -wcs_cosd(lat+eul[1]) + coslat*eul[3]*(1.0 - coslng);
   }
   y = -coslat*sinlng;
   if (x != 0.0 || y != 0.0) {
      dphi = wcs_atan2d(y, x);
   } else {
      /* Change of origin of longitude. */
      dphi = dlng - 180.0;
   }
   *phi = eul[2] + dphi;

   /* Normalize the native longitude. */
   if (*phi > 180.0) {
      *phi -= 360.0;
   } else if (*phi < -180.0) {
      *phi += 360.0;
   }

   /* Compute the native latitude. */
   if (fmod(dlng,180.0) == 0.0) {
      *theta = lat + coslng*eul[1];
      if (*theta >  90.0) *theta =  180.0 - *theta;
      if (*theta < -90.0) *theta = -180.0 - *theta;
   } else {
      z = sinlat*eul[3] + coslat*eul[4]*coslng;
      if (fabs(z) > 0.99) {
         /* Use an alternative formula for greater numerical accuracy. */
         *theta = wcs_copysign(wcs_acosd(sqrt(x*x+y*y)), z);
      } else {
         *theta = wcs_asind(z);
      }
   }

   return 0;
}

/*-----------------------------------------------------------------------*/

int sphrev (phi, theta, eul, lng, lat)

const double phi, theta, eul[5];
double *lng, *lat;

{
   double cosphi, costhe, dlng, dphi, sinphi, sinthe, x, y, z;

   costhe = wcs_cosd(theta);
   sinthe = wcs_sind(theta);

   dphi = phi - eul[2];
   cosphi = wcs_cosd(dphi);
   sinphi = wcs_sind(dphi);

   /* Compute the celestial longitude. */
   x = sinthe*eul[4] - costhe*eul[3]*cosphi;
   if (fabs(x) < tol) {
      /* Rearrange formula to reduce roundoff errors. */
      x = -wcs_cosd(theta+eul[1]) + costhe*eul[3]*(1.0 - cosphi);
   }
   y = -costhe*sinphi;
   if (x != 0.0 || y != 0.0) {
      dlng = wcs_atan2d(y, x);
   } else {
      /* Change of origin of longitude. */
      dlng = dphi + 180.0;
   }
   *lng = eul[0] + dlng;

   /* Normalize the celestial longitude. */
   if (eul[0] >= 0.0) {
      if (*lng < 0.0) *lng += 360.0;
   } else {
      if (*lng > 0.0) *lng -= 360.0;
   }

   if (*lng > 360.0) {
      *lng -= 360.0;
   } else if (*lng < -360.0) {
      *lng += 360.0;
   }

   /* Compute the celestial latitude. */
   if (fmod(dphi,180.0) == 0.0) {
      *lat = theta + cosphi*eul[1];
      if (*lat >  90.0) *lat =  180.0 - *lat;
      if (*lat < -90.0) *lat = -180.0 - *lat;
   } else {
      z = sinthe*eul[3] + costhe*eul[4]*cosphi;
      if (fabs(z) > 0.99) {
         /* Use an alternative formula for greater numerical accuracy. */
         *lat = wcs_copysign(wcs_acosd(sqrt(x*x+y*y)), z);
      } else {
         *lat = wcs_asind(z);
      }
   }

   return 0;
}
