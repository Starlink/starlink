/*
*+
*  Name:
*     palEqgal

*  Purpose:
*     Convert from J2000.0 equatorial coordinates to Galactic

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palEqgal ( double dr, double dd, double *dl, double *db );

*  Arguments:
*     dr = double (Given)
*       J2000.0 RA (radians)
*     dd = double (Given)
*       J2000.0 Dec (radians
*     dl = double * (Returned)
*       Galactic longitude (radians).
*     db = double * (Returned)
*       Galactic latitude (radians).

*  Description:
*     Transformation from J2000.0 equatorial coordinates
*     to IAU 1958 galactic coordinates.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     The equatorial coordinates are J2000.0.  Use the routine
*     palGe50 if conversion to B1950.0 'FK4' coordinates is
*     required.

*  See Also:
*     Blaauw et al, Mon.Not.R.Astron.Soc.,121,123 (1960)

*  History:
*     2012-02-12(TIMJ):
*        Initial version with documentation taken from Fortran SLA
*        Adapted with permission from the Fortran SLALIB library.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 1998 Rutherford Appleton Laboratory
*     Copyright (C) 2012 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "pal.h"
#include "pal1sofa.h"

void palEqgal ( double dr, double dd, double *dl, double *db ) {

  double v1[3];
  double v2[3];

/*
*  L2,B2 system of galactic coordinates
*
*  P = 192.25       RA of galactic north pole (mean B1950.0)
*  Q =  62.6        inclination of galactic to mean B1950.0 equator
*  R =  33          longitude of ascending node
*
*  P,Q,R are degrees
*
*  Equatorial to galactic rotation matrix (J2000.0), obtained by
*  applying the standard FK4 to FK5 transformation, for zero proper
*  motion in FK5, to the columns of the B1950 equatorial to
*  galactic rotation matrix:
*/
  double rmat[3][3] = {
    { -0.054875539726,-0.873437108010,-0.483834985808 },
    { +0.494109453312,-0.444829589425,+0.746982251810 },
    { -0.867666135858,-0.198076386122,+0.455983795705 }
  };

  /* Spherical to Cartesian */
  eraS2c( dr, dd, v1 );

  /* Equatorial to Galactic */
  eraRxp( rmat, v1, v2 );

  /* Cartesian to spherical */
  eraC2s( v2, dl, db );

  /* Express in conventional ranges */
  *dl = eraAnp( *dl );
  *db = eraAnpm( *db );

}
