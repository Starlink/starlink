/*
*+
*  Name:
*     palMappa

*  Purpose:
*     Compute parameters needed by palAmpqk and palMapqk.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palMappa( double eq, double date, double amprms[21] )

*  Arguments:
*     eq = double (Given)
*        epoch of mean equinox to be used (Julian)
*     date = double (Given)
*        TDB (JD-2400000.5)
*     amprms =   double[21]  (Returned)
*        star-independent mean-to-apparent parameters:
*        - (0)      time interval for proper motion (Julian years)
*        - (1-3)    barycentric position of the Earth (AU)
*        - (4-6)    heliocentric direction of the Earth (unit vector)
*        - (7)      (grav rad Sun)*2/(Sun-Earth distance)
*        - (8-10)   abv: barycentric Earth velocity in units of c
*        - (11)     sqrt(1-v**2) where v=modulus(abv)
*        - (12-20)  precession/nutation (3,3) matrix

*  Description:
*     Compute star-independent parameters in preparation for
*     transformations between mean place and geocentric apparent place.
*
*     The parameters produced by this function are required in the
*     parallax, aberration, and nutation/bias/precession parts of the
*     mean/apparent transformations.
*
*     The reference systems and timescales used are IAU 2006.

*  Notes:
*     - For date, the distinction between the required TDB and TT
*     is always negligible.  Moreover, for all but the most
*     critical applications UTC is adequate.
*     - The vector amprms(1-3) is referred to the mean equinox and
*     equator of epoch eq.
*     - The parameters amprms produced by this function are used by
*     palAmpqk, palMapqk and palMapqkz.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     {enter_new_authors_here}

*  History:
*     2012-02-13 (PTW):
*        Initial version.
*        Adapted with permission from the Fortran SLALIB library.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2003 Rutherford Appleton Laboratory
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
#include "palmac.h"
#include "pal1sofa.h"

#include <string.h>

void palMappa( double eq, double date, double amprms[21] ){

/* Local constants */

/*  Gravitational radius of the Sun x 2 (2*mu/c**2, AU) */
  const double GR2 = 2.0 * 9.87063e-9;

/* Local Variables; */
   int i;
   double ebd[ 3 ], ehd[ 3 ], eh[ 3 ], e, vn[ 3 ], vm;

/* Initialise so that unsused values are returned holding zero */
   memset( amprms, 0, 21*sizeof( *amprms ) );

/* Time interval for proper motion correction. */
   amprms[ 0 ] = eraEpj( PAL__MJD0, date ) - eq;

/* Get Earth barycentric and heliocentric position and velocity. */
   palEvp( date, eq, ebd, &amprms[ 1 ], ehd, eh );

/* Heliocentric direction of Earth (normalized) and modulus. */
   eraPn( eh, &e, &amprms[ 4 ] );

/* Light deflection parameter */
   amprms[7] = GR2 / e;

/* Aberration parameters. */
   for( i = 0; i < 3; i++ ) {
      amprms[ i + 8 ] = ebd[ i ]*PAL__CR;
   }
   eraPn( &amprms[8], &vm, vn );
   amprms[ 11 ] = sqrt( 1.0 - vm*vm );

/* NPB matrix. */
   palPrenut( eq, date, (double(*)[ 3 ]) &amprms[ 12 ] );
}
