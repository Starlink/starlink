/*
*  Name:
*     KPG1_ISFINR

*  Purpose:
*     A Fortran callable function to check if a value is finite.

*  Language:
*     C

*  Invocation:
*     RESULT = KPG1_ISFINR( VAL )

*  Description:
*     The routine uses the finite() system call to check the 
*     supplied value. If it is NaN of Inf a .FALSE. value is 
*     returned, otherwise a .TRUE. value is returned.

*  Arguments:
*     VAL = REAL (Given)
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

F77_LOGICAL_FUNCTION(kpg1_isfinr)( REAL(val) )
{
   GENPTR_REAL(val)
   return finite( (double) *val ) ? F77_TRUE : F77_FALSE;
}
