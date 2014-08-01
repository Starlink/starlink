/*
*+
*  Name:
*     palGalsup

*  Purpose:
*     Convert from galactic to supergalactic coordinates

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palGalsup ( double dl, double db, double *dsl, double *dsb );

*  Arguments:
*     dl = double (Given)
*       Galactic longitude.
*     db = double (Given)
*       Galactic latitude.
*     dsl = double * (Returned)
*       Supergalactic longitude.
*     dsb = double * (Returned)
*       Supergalactic latitude.

*  Description:
*     Transformation from IAU 1958 galactic coordinates to
*     de Vaucouleurs supergalactic coordinates.

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
*        Adapted with permission from the Fortran SLALIB library.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 1999 Rutherford Appleton Laboratory
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

void palGalsup ( double dl, double db, double *dsl, double *dsb ) {

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
  eraS2c( dl, db, v1 );

  /* Galactic to Supergalactic */
  eraRxp( rmat, v1, v2 );

  /* Cartesian to spherical */
  eraC2s( v2, dsl, dsb );

  /* Express in conventional ranges */
  *dsl = eraAnp( *dsl );
  *dsb = eraAnpm( *dsb );

}
