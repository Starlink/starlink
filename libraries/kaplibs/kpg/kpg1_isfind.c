/*
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
   return finite( *val ) ? F77_TRUE : F77_FALSE;
}
