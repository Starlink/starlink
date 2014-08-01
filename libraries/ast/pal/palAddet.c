/*
*+
*  Name:
*     palAddet

*  Purpose:
*     Add the E-terms to a pre IAU 1976 mean place

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palAddet ( double rm, double dm, double eq,
*                     double *rc, double *dc );

*  Arguments:
*     rm = double (Given)
*        RA without E-terms (radians)
*     dm = double (Given)
*        Dec without E-terms (radians)
*     eq = double (Given)
*        Besselian epoch of mean equator and equinox
*     rc = double * (Returned)
*        RA with E-terms included (radians)
*     dc = double * (Returned)
*        Dec with E-terms included (radians)

*  Description:
*     Add the E-terms (elliptic component of annual aberration)
*     to a pre IAU 1976 mean place to conform to the old
*     catalogue convention.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     Most star positions from pre-1984 optical catalogues (or
*     derived from astrometry using such stars) embody the
*     E-terms.  If it is necessary to convert a formal mean
*     place (for example a pulsar timing position) to one
*     consistent with such a star catalogue, then the RA,Dec
*     should be adjusted using this routine.

*  See Also:
*     Explanatory Supplement to the Astronomical Ephemeris,
*     section 2D, page 48.

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

void palAddet ( double rm, double dm, double eq, double *rc, double *dc ) {
  double a[3];   /* The E-terms */
  double v[3];
  int i;

  /* Note the preference for IAU routines */

  /* Retrieve the E-terms */
  palEtrms( eq, a );

  /* Spherical to Cartesian */
  eraS2c( rm, dm, v );

  /* Include the E-terms */
  for (i=0; i<3; i++) {
    v[i] += a[i];
  }

  /* Cartesian to spherical */
  eraC2s( v, rc, dc );

  /* Bring RA into conventional range */
  *rc = eraAnp( *rc );

}
