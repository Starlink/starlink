/*
*+
*  Name:
*     palSubet

*  Purpose:
*     Remove the E-terms from a pre IAU 1976 catalogue RA,Dec

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palSubet ( double rc, double dc, double eq,
*                     double *rm, double *dm );

*  Arguments:
*     rc = double (Given)
*        RA with E-terms included (radians)
*     dc = double (Given)
*        Dec with E-terms included (radians)
*     eq = double (Given)
*        Besselian epoch of mean equator and equinox
*     rm = double * (Returned)
*        RA without E-terms (radians)
*     dm = double * (Returned)
*        Dec without E-terms (radians)

*  Description:
*     Remove the E-terms (elliptic component of annual aberration)
*     from a pre IAU 1976 catalogue RA,Dec to give a mean place.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     Most star positions from pre-1984 optical catalogues (or
*     derived from astrometry using such stars) embody the
*     E-terms.  This routine converts such a position to a
*     formal mean place (allowing, for example, comparison with a
*     pulsar timing position).

*  See Also:
*     Explanatory Supplement to the Astronomical Ephemeris,
*     section 2D, page 48.

*  History:
*     2012-02-12(TIMJ):
*        Initial version with documentation taken from Fortran SLA
*        Adapted with permission from the Fortran SLALIB library.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 1995 Rutherford Appleton Laboratory
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

void palSubet ( double rc, double dc, double eq, double *rm, double *dm ) {
  double a[3];   /* The E-terms */
  double v[3];
  double f;
  int i;

  /* Note the preference for IAU routines */

  /* Retrieve the E-terms */
  palEtrms( eq, a );

  /* Spherical to Cartesian */
  eraS2c( rc, dc, v );

  /* Include the E-terms */
  f = 1.0 + eraPdp( v, a );
  for (i=0; i<3; i++) {
    v[i] = f*v[i] - a[i];
  }

  /* Cartesian to spherical */
  eraC2s( v, rm, dm );

  /* Bring RA into conventional range */
  *rm = eraAnp( *rm );

}
