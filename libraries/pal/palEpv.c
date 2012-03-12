/*
*+
*  Name:
*     palEpv

*  Purpose:
*     Earth position and velocity with respect to the BCRS

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palEpv( double date, double ph[3], double vh[3],
*                  double pb[3], double vb[3] );

*  Arguments:
*     date = double (Given)
*        Date, TDB Modified Julian Date (JD-2400000.5)
*     ph = double [3] (Returned)
*        Heliocentric Earth position (AU)
*     vh = double [3] (Returned)
*        Heliocentric Earth velocity (AU/day)
*     pb = double [3] (Returned)
*        Barycentric Earth position (AU)
*     vb = double [3] (Returned)
*        Barycentric Earth velocity (AU/day)

*  Description:
*     Earth position and velocity, heliocentric and barycentric, with
*     respect to the Barycentric Celestial Reference System.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - See iauEpv00 for details on accuracy
*     - Note that the status argument from iauEpv00 is ignored

*  History:
*     2012-03-12 (TIMJ):
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

#include "palmac.h"
#include "pal.h"
#include "sofa.h"

void palEpv( double date, double ph[3], double vh[3],
             double pb[3], double vb[3] ) {

  int i;
  double pvh[2][3];
  double pvb[2][3];

  iauEpv00( PAL__MJD0, date, pvh, pvb );

  /* Copy into output arrays */
  for (i=0; i<3; i++) {
    ph[i] = pvh[0][i];
    vh[i] = pvh[1][i];
    pb[i] = pvb[0][i];
    vb[i] = pvb[1][i];
  }

}
