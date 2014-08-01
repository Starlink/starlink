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
*       0 = OK. See eraJd2cal for other values.

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
*        Adapted with permission from the Fortran SLALIB library.
*     {enter_further_changes_here}

*  Notes:
*     - Uses eraJd2cal

*  Copyright:
*     Copyright (C) 2004 Patrick T. Wallace
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

#include <math.h>

#include "pal.h"
#include "palmac.h"
#include "pal1sofa.h"

void palDjcal ( int ndp, double djm, int iymdf[4], int *j ) {
  double frac = 0.0;
  double nfd;

  *j = eraJd2cal( PAL__MJD0, djm, &(iymdf[0]),
		  &(iymdf[1]), &(iymdf[2]),
		  &frac);

  /* Convert ndp to a power of 10 */
  nfd = pow( 10., (double)ndp );

  /* Multiply the fraction */
  frac *= nfd;

  /* and now we want to round to the nearest integer */
  iymdf[3] = (int)DNINT(frac);

}
