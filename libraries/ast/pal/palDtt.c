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
*     PTW: Patrick T. Wallace
*     {enter_new_authors_here}

*  Notes:
*     - Consider a comprehensive upgrade to use the time transformations in SOFA's time
*       cookbook:  http://www.iausofa.org/sofa_ts_c.pdf.
*     - See eraDat for a description of error conditions when calling this function
*       with a time outside of the UTC range. This behaviour differs from slaDtt.

*  History:
*     2012-02-08 (TIMJ):
*        Initial version
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

double palDtt( double utc ) {
  return 32.184 + palDat( utc );
}
