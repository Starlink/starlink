/*
*+
*  Name:
*     palRverot

*  Purpose:
*     Velocity component in a given direction due to Earth rotation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     double palRverot ( double phi, double ra, double da, double st );

*  Arguments:
*     phi = double (Given)
*        latitude of observing station (geodetic) (radians)
*     ra = double (Given)
*        apparent RA (radians)
*     da = double (Given)
*        apparent Dec (radians)
*     st = double (Given)
8        Local apparent sidereal time.

*  Returned Value:
*     palRverot = double
*        Component of Earth rotation in direction RA,DA (km/s).
*        The result is +ve when the observatory is receding from the
*        given point on the sky.

*  Description:
*     Calculate the velocity component in a given direction due to Earth
*     rotation.
*
*     The simple algorithm used assumes a spherical Earth, of
*     a radius chosen to give results accurate to about 0.0005 km/s
*     for observing stations at typical latitudes and heights.  For
*     applications requiring greater precision, use the routine
*     palPvobs.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2012-03-02 (TIMJ):
*        Initial version
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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "pal.h"

#include <math.h>

double palRverot ( double phi, double ra, double da, double st ) {

  /*  Nominal mean sidereal speed of Earth equator in km/s (the actual
   *  value is about 0.4651) */
  const double espeed = 0.4655;
  return espeed * cos(phi) * sin(st-ra) * cos(da);
}
