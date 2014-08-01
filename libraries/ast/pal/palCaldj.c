/*
*+
*  Name:
*     palCaldj

*  Purpose:
*     Gregorian Calendar to Modified Julian Date

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palCaldj ( int iy, int im, int id, double *djm, int *j );

*  Arguments:
*     iy = int (Given)
*        Year in the Gregorian calendar
*     im = int (Given)
*        Month in the Gergorian calendar
*     id = int (Given)
*        Day in the Gregorian calendar
*     djm = double * (Returned)
*        Modified Julian Date (JD-2400000.5) for 0 hrs
*     j = status (Returned)
*       0 = OK. See eraCal2jd for other values.

*  Description:
*     Modified Julian Date to Gregorian Calendar with special
*     behaviour for 2-digit years relating to 1950 to 2049.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2012-02-11 (TIMJ):
*        Initial version with documentation taken from Fortran SLA
*        Adapted with permission from the Fortran SLALIB library.
*     {enter_further_changes_here}

*  Notes:
*     - Uses eraCal2jd
*     - Unlike eraCal2jd this routine treats the years 0-100 as
*       referring to the end of the 20th Century and beginning of
*       the 21st Century. If this behaviour is not acceptable
*       use the SOFA/ERFA routine directly or palCldj.
*       Acceptable years are 00-49, interpreted as 2000-2049,
*                            50-99,     "       "  1950-1999,
*                            all others, interpreted literally.
*     - Unlike SLA this routine will work with negative years.


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
#include "palmac.h"
#include "pal1sofa.h"

void palCaldj ( int iy, int im, int id, double *djm, int *j ) {
  int adj = 0;   /* Year adjustment */
  double djm0;

  if (iy >= 0 && iy <= 49) {
    adj = 2000;
  } else if (iy >= 50 && iy <= 99) {
    adj = 1900;
  }
  iy += adj;

  *j = eraCal2jd( iy, im, id, &djm0, djm );
}
