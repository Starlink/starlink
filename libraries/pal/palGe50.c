/*
*+
*  Name:
*     palGe50

*  Purpose:
*     Transform Galactic Coordinate to B1950 FK4

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     palGe50( double dl, double db, double *dr, double *dd );

*  Arguments:
*     dl = double (Given)
*        Galactic longitude (radians)
*     db = double (Given)
*        Galactic latitude (radians)
*     dr = double * (Returned)
*        B9150.0 FK4 RA.
*     dd = double * (Returned)
*        B1950.0 FK4 Dec.

*  Description:
*     Transformation from IAU 1958 galactic coordinates to
*     B1950.0 'FK4' equatorial coordinates.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - The equatorial coordinates are B1950.0 'FK4'. Use the routine
*     palGaleq if conversion to J2000.0 coordinates is required.

*  See Also:
*     - Blaauw et al, Mon.Not.R.Astron.Soc.,121,123 (1960)

*  History:
*     2012-03-23 (TIMJ):
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

void palGe50 ( double dl, double db, double * dr, double * dd ) {

/*
 *  L2,B2 system of galactic coordinates
 *
 *  P = 192.25       RA of galactic north pole (mean B1950.0)
 *  Q =  62.6        inclination of galactic to mean B1950.0 equator
 *  R =  33          longitude of ascending node
 *
 *  P,Q,R are degrees
 *
 *
 *  Equatorial to galactic rotation matrix
 *
 *  The Euler angles are P, Q, 90-R, about the z then y then
 *  z axes.
 *
 *         +CP.CQ.SR-SP.CR     +SP.CQ.SR+CP.CR     -SQ.SR
 *
 *         -CP.CQ.CR-SP.SR     -SP.CQ.CR+CP.SR     +SQ.CR
 *
 *         +CP.SQ              +SP.SQ              +CQ
 *
 */

  double rmat[3][3] = {
    { -0.066988739415,-0.872755765852,-0.483538914632 },
    { +0.492728466075,-0.450346958020,+0.744584633283 },
    { -0.867600811151,-0.188374601723,+0.460199784784 }
  };

  double v1[3], v2[3], r, d, re, de;

  /* Spherical to cartesian */
  iauS2c( dl, db, v1 );

  /* Rotate to mean B1950.0 */
  iauTrxp( rmat, v1, v2 );

  /* Cartesian to spherical */
  iauC2s( v2, &r, &d );

  /* Introduce E-terms */
  palAddet( r, d, 1950.0, &re, &de );

  /* Express in conventional ranges */
  *dr = iauAnp( re );
  *dd = iauAnpm( de );

}
