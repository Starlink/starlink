/*
*+
*  Name:
*     palEcmat

*  Purpose:
*     Form the equatorial to ecliptic rotation matrix - IAU 2006
*     precession model.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     palEcmat( double date, double rmat[3][3] )

*  Arguments:
*     date = double (Given)
*        TT as Modified Julian Date (JD-2400000.5). The difference
*        between TT and TDB is of the order of a millisecond or two
*        (i.e. about 0.02 arc-seconds).
*     rmat = double[3][3] (Returned)
*        Rotation matrix

*  Description:
*     The equatorial to ecliptic rotation matrix is found and returned.
*     The matrix is in the sense   V(ecl)  =  RMAT * V(equ);  the
*     equator, equinox and ecliptic are mean of date.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2012-02-10 (DSB):
*        Initial version with documentation taken from Fortran SLA
*        Adapted with permission from the Fortran SLALIB library.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 1996 Rutherford Appleton Laboratory
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

void palEcmat( double date, double rmat[3][3] ) {

/* Mean obliquity (the angle between the ecliptic and mean equator of
   date). */
   double eps0 = eraObl06( PAL__MJD0, date );

/* Matrix */
   palDeuler( "X", eps0, 0.0, 0.0, rmat );

}
