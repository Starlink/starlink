/*
*+
*  Name:
*     palDtt

*  Purpose:
*     Return offset between UTC and TT

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     dtt = palDtt( double utc );

*  Arguments:
*     utc = double (Given)
*        UTC date as a modified JD (JD-2400000.5)

*  Returned Value:
*     dtt = double
*        TT-UTC in seconds

*  Description:
*     Increment to be applied to Coordinated Universal Time UTC to give
*     Terrestrial Time TT (formerly Ephemeris Time ET)

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - Consider a comprehensive upgrade to use the time transformations in SOFA's time
*       cookbook:  http://www.iausofa.org/sofa_ts_c.pdf.
*     - See iauDat for a description of error conditions when calling this function
*       with a time outside of the UTC range. This behaviour differs from slaDtt.

*  History:
*     2012-02-08 (TIMJ):
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
*    You should have received a copy of the GNU General Public License
*    along with this program; if not, write to the Free Software
*    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
*    USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "pal.h"

double palDtt( double utc ) {
  return 32.184 + palDat( utc );
}
