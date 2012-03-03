/*
*+
*  Name:
*     palEqecl

*  Purpose:
*     Transform from J2000.0 equatorial coordinates to ecliptic coordinates

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palEqecl( double dr, double dd, double date,
*                    double *dl, double *db);

*  Arguments:
*     dr = double (Given)
*        J2000.0 mean RA (radians)
*     dd = double (Given)
*        J2000.0 mean Dec (Radians)
*     date = double (Given)
*        TT as Modified Julian Date (JD-2400000.5). The difference
*        between TT and TDB is of the order of a millisecond or two
*        (i.e. about 0.02 arc-seconds).
*     dl = double * (Returned)
*        Ecliptic longitude (mean of date, IAU 1980 theory, radians)
*     db = double * (Returned)
*        Ecliptic latitude (mean of date, IAU 1980 theory, radians)

*  Description:
*     Transform from J2000.0 equatorial coordinates to ecliptic coordinates.

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
#include "sofa.h"

void palEqecl ( double dr, double dd, double date, double *dl, double *db ) {
  double v1[3], v2[3];
  double rmat[3][3];

  /* Spherical to Cartesian */
  iauS2c( dr, dd, v1 );

  /* Mean J2000 to mean of date */
  palPrec( 2000.0, palEpj(date), rmat );
  iauRxp( rmat, v1, v2 );

  /* Equatorial to ecliptic */
  palEcmat( date, rmat );
  iauRxp( rmat, v2, v1 );

  /* Cartesian to spherical */
  iauC2s( v1, dl, db );
}
