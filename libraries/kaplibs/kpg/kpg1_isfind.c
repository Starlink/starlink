/*
 *+
*  Name:
*     KPG1_ISFIND

*  Purpose:
*     A Fortran callable function to check if a value is finite.

*  Language:
*     C

*  Invocation:
*     RESULT = KPG1_ISFIND( VAL )

*  Description:
*     The routine uses the finite() system call to check the
*     supplied value. If it is NaN of Inf a .FALSE. value is
*     returned, otherwise a .TRUE. value is returned.

*  Arguments:
*     VAL = DOUBLE PRECISION (Given)
*        The value to check.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-DEC-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
#include <math.h>
#include "f77.h"

F77_LOGICAL_FUNCTION(kpg1_isfind)( DOUBLE(val) )
{
   GENPTR_DOUBLE(val)
   return isfinite( *val ) ? F77_TRUE : F77_FALSE;
}
