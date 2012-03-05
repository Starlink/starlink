/*
*+
*  Name:
*     palNutc

*  Purpose:
*     Calculate nutation longitude & obliquoty components

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palNutc( double date, double * dpsi, double *deps, double *eps0 );

*  Arguments:
*     date = double (Given)
*        TT as modified Julian date (JD-2400000.5)
*     dpsi = double * (Returned)
*        Nutation in longitude
*     deps = double * (Returned)
*        Nutation in obliquity
*     eps0 = double * (Returned)
*        Mean obliquity.

*  Description:
*     Calculates the longitude * obliquity components and mean obliquity
*     using the SOFA library.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - Calls iauObl06 and iauNut06a and therefore uses the IAU 206
*       precession/nutation model.
*     - Note the change from SLA/F regarding the date. TT is used
*       rather than TDB.

*  History:
*     2012-03-05 (TIMJ):
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
#include "palmac.h"
#include "sofa.h"

void palNutc( double date, double * dpsi, double *deps, double *eps0 ) {
  iauNut06a( PAL__MJD0, date, dpsi, deps );
  *eps0 = iauObl06( PAL__MJD0, date );
}
