/*+  DTASK_CHDLR - C Version AST handler for timed reschedules
      SUBROUTINE DTASK_CHDLR ( ID )
*    Description :
*     Is called by the C ATIMER system and  in turn calls the Fortran
*     DTASK_ASTHDLR routine.
*    Language:
*     C
*    Invocation :
*     Invoked by timer completion
*     interrupts.
*    Parameters :
*     ASTPARM=INTEGER (given)
*           The timer id
*    Method :
*     Call the Fortran handler with its argument a  pointer to the
*     argument of this routine.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     AJC: A.J.Chipperfield (STARLINK, RAL)
*    History :
*     date:  changes (institution::username)
*     28-JUN-1994 (AJC):
*        Original version.
*    endhistory
*/
#include "f77.h"

extern void F77_EXTERNAL_NAME(dtask_asthdlr)( INTEGER(id) );

F77_SUBROUTINE(dtask_chdlr)( int id )
{
F77_CALL(dtask_asthdlr)( INTEGER_ARG(&id) );
}
