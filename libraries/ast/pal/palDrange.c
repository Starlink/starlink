/*
*+
*  Name:
*     palDrange

*  Purpose:
*     Normalize angle into range +/- pi

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     palDrange( double angle )

*  Arguments:
*     angle = double (Given)
*        The angle in radians.

*  Description:
*     The result is "angle" expressed in the range +/- pi. If the
*     supplied value for "angle" is equal to +/- pi, it is returned
*     unchanged.

*  Authors:
*     DSB: David S Berry (JAC, Hawaii)
*     PTW: Patrick T. Wallace
*     {enter_new_authors_here}

*  History:
*     2012-05-09 (DSB):
*        Initial version with documentation taken from Fortran SLA
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
#include "palmac.h"
#include <math.h>

double palDrange( double angle ){
   double result = fmod( angle, PAL__D2PI );
   if( result > PAL__DPI ) {
      result -= PAL__D2PI;
   } else if( result < -PAL__DPI ) {
      result += PAL__D2PI;
   }
   return result;
}

