/*
*+
*  Name:
*     palDjcal

*  Purpose:
*     Modified Julian Date to Gregorian Calendar

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palDjcal ( int ndp, double djm, int iymdf[4], int *j );

*  Arguments:
*     ndp = int (Given)
*        Number of decimal places of days in fraction.
*     djm = double (Given)
*        Modified Julian Date (JD-2400000.5)
*     iymdf[4] = int[] (Returned)
*       Year, month, day, fraction in Gregorian calendar.
*     j = status (Returned)
*       0 = OK. See iauJd2cal for other values.

*  Description:
*     Modified Julian Date to Gregorian Calendar, expressed
*     in a form convenient for formatting messages (namely
*     rounded to a specified precision, and with the fields
*     stored in a single array)

*  Authors:
*     PTW: Pat Wallace (STFC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2012-02-10 (TIMJ):
*        Initial version with documentation taken from Fortran SLA
*     {enter_further_changes_here}

*  Notes:
*     - Uses iauJd2cal

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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
*     USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include <math.h>

#include "pal.h"
#include "palmac.h"
#include "sofa.h"

void palDjcal ( int ndp, double djm, int iymdf[4], int *j ) {
  double frac = 0.0;
  double nfd;

  *j = iauJd2cal( PAL__MJD0, djm, &(iymdf[0]),
		  &(iymdf[1]), &(iymdf[2]),
		  &frac);

  /* Convert ndp to a power of 10 */
  nfd = pow( 10., (double)ndp );

  /* Multiply the fraction */
  frac *= nfd;

  /* and now we want to round to the nearest integer */
  iymdf[3] = (int)DNINT(frac);

}
