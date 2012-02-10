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
#include "palmac.h"
#include "sofa.h"

void palEcmat( double date, double rmat[3][3] ) {

/* Mean obliquity (the angle between the ecliptic and mean equator of
   date). */
   double eps0 = iauObl06( PAL__MJD0, date );

/* Matrix */
   palDeuler( "X", eps0, 0.0, 0.0, rmat );

}
