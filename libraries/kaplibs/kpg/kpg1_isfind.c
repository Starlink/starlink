/*
*  Name:
*     KPG1_ISFIND

*  Purpose:
*     A Fortran callable function to check if a value is finite.

*  Language:
*     C

*  Invocation:
*     CALL KPG1_ISFIND( VAL, FINITE )

*  Description:
*     The routine uses the finite() system call to check the 
*     supplied value. If it is NaN of Inf a .FALSE. value is 
*     returned, otherwise a .TRUE. value is returned.

*  Arguments:
*     VAL = DOUBLE PRECISION (Given)
*        The value to check.
*     FINITE = LOGICAL (Returned)
*        Is VAL a finite value?

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-JUN-1998 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*- 
*/
#include <math.h>
#include "f77.h"

F77_SUBROUTINE(kpg1_isfind)( DOUBLE(val), LOGICAL(finite) )
{
   GENPTR_DOUBLE(val)
   GENPTR_LOGICAL(finite)
   *finite = finite( *val )?F77_TRUE:F77_FALSE;
}
