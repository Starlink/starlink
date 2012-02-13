/*
*+
*  Name:
*     palSupgal

*  Purpose:
*     Convert from supergalactic to galactic coordinates

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palSupgal ( double dsl, double dsb, double *dl, double *db );

*  Arguments:
*     dsl = double (Given)
*       Supergalactic longitude.
*     dsb = double (Given)
*       Supergalactic latitude.
*     dl = double * (Returned)
*       Galactic longitude.
*     db = double * (Returned)
*       Galactic latitude.

*  Description:
*     Transformation from de Vaucouleurs supergalactic coordinates
*     to IAU 1958 galactic coordinates

*  Authors:
*     PTW: Pat Wallace (STFC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  See Also:
*     - de Vaucouleurs, de Vaucouleurs, & Corwin, Second Reference
*       Catalogue of Bright Galaxies, U. Texas, page 8.
*     - Systems & Applied Sciences Corp., Documentation for the
*       machine-readable version of the above catalogue,
*       Contract NAS 5-26490.
*
*    (These two references give different values for the galactic
*     longitude of the supergalactic origin.  Both are wrong;  the
*     correct value is L2=137.37.)

*  History:
*     2012-02-12(TIMJ):
*        Initial version with documentation taken from Fortran SLA
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
*     USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "pal.h"
#include "sofa.h"

void palSupgal ( double dsl, double dsb, double *dl, double *db ) {

  double v1[3];
  double v2[3];

/*
*  System of supergalactic coordinates:
*
*    SGL   SGB        L2     B2      (deg)
*     -    +90      47.37  +6.32
*     0     0         -      0
*
*  Galactic to supergalactic rotation matrix:
*/
  double rmat[3][3] = {
    { -0.735742574804,+0.677261296414,+0.000000000000 },
    { -0.074553778365,-0.080991471307,+0.993922590400 },
    { +0.673145302109,+0.731271165817,+0.110081262225 }
  };

  /* Spherical to Cartesian */
  iauS2c( dsl, dsb, v1 );

  /* Supergalactic to galactic */
  iauTrxp( rmat, v1, v2 );

  /* Cartesian to spherical */
  iauC2s( v2, dl, db );

  /* Express in conventional ranges */
  *dl = iauAnp( *dl );
  *db = iauAnpm( *db );

}
