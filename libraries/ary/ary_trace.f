      SUBROUTINE ARY_TRACE( NEWFLG, OLDFLG )
*+
*  Name:
*     ARY_TRACE

*  Purpose:
*     Set the internal ARY_ system error-tracing flag.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_TRACE( NEWFLG, OLDFLG )

*  Description:
*     The routine sets an internal flag in the ARY_ system which
*     enables or disables error-tracing messages. If this flag is set
*     to .TRUE., then any error occurring within the ARY_ system will
*     be accompanied by error messages indicating which internal
*     routines have exited prematurely as a result. If the flag is set
*     to .FALSE., this internal diagnostic information will not appear
*     and only standard error messages will be produced.

*  Arguments:
*     NEWFLG = LOGICAL (Given)
*        The new value to be set for the error-tracing flag.
*     OLDFLG = LOGICAL (Returned)
*        The previous value of the flag.

*  Notes:
*     -  By default, the error tracing flag is set to .FALSE., so
*     no internal diagnostic information will be produced.

*  Algorithm:
*     -  Return the previous value of the error tracing flag.
*     -  Set the new value.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-NOV-1989 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'ARY_TCB'          ! ARY_ Error Tracing Control Block
*        TCB_ETFLG = LOGICAL (Read and Write)
*           Error tracing flag.

*  Arguments Given:
      LOGICAL NEWFLG

*  Arguments Returned:
      LOGICAL OLDFLG

*.

*  Return the previous value of the error tracing flag.
      OLDFLG = TCB_ETFLG

*  Set the new value.
      TCB_ETFLG = NEWFLG

      END
