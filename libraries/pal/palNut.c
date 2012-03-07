/*
*+
*  Name:
*     palNut

*  Purpose:
*     Form the matrix of nutation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palNut( double date, double rmatn[3][3] );

*  Arguments:
*     date = double (Given)
*        TT as modified Julian date (JD-2400000.5)
*     rmatn = double [3][3] (Returned)
*        Nutation matrix in the sense v(true)=rmatn * v(mean)
*        where v(true) is the star vector relative to the
*        true equator and equinox of date and v(mean) is the
*        star vector relative to the mean equator and equinox
*        of date.

*  Description:
*     Form the matrix of nutation for a given date using
*     the IAU 2006 nutation model and palDeuler.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - Uses iauNut06a via palNutc
*     - The distinction between TDB and TT is negligible. For all but
*       the most critical applications UTC is adequate.

*  History:
*     2012-03-07 (TIMJ):
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

#include <pal.h>

void palNut( double date, double rmatn[3][3]) {
  double dpsi, deps, eps0;

  /* Nutation component and mean obliquity */
  palNutc( date, &dpsi, &deps, &eps0 );

  /* Rotation matrix */
  palDeuler( "XZX", eps0, -dpsi, -(eps0+deps), rmatn );

}
