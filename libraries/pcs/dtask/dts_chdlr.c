/*
*+
*  Name:
*     DTASK_CHDLR

*  Purpose:
*     C Version AST handler for timed reschedules

*  Language:
*     C

*  Invocation:
*     Invoked by timer completion
*     interrupts.

*  Description:
*     Is called by the C ATIMER system and  in turn calls the Fortran
*     DTASK_ASTHDLR routine.

*  Arguments:
*     ASTPARM=INTEGER (given)
*           The timer id

*  Algorithm:
*     Call the Fortran handler with its argument a  pointer to the
*     argument of this routine.

*  Authors:
*     AJC: A.J.Chipperfield (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     28-JUN-1994 (AJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
#include "f77.h"

extern void F77_EXTERNAL_NAME(dtask_asthdlr)( INTEGER(id) );

F77_SUBROUTINE(dtask_chdlr)( int id )
{
F77_CALL(dtask_asthdlr)( INTEGER_ARG(&id) );
}
