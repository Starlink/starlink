      SUBROUTINE ERR_OUT( PARAM, TEXT, STATUS )
*+
*  Name:
*     ERR_OUT

*  Purpose:
*     Report an error message and deliver it to the user.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR_OUT( PARAM, TEXT, STATUS )

*  Description:
*     The message is added to the error table at the current error context. 
*     The contents of the error table at this context are then flushed, i.e. 
*     output to the user. After output, the error table context is annulled, 
*     and the values associated with any existing message tokens left 
*     undefined. 

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Expression containing the message parameter name.
*     TEXT = CHARACTER * ( * ) (Given)
*        Expression containing the error message text.
*     STATUS = INTEGER (Given and Returned)
*        The global status. On normal completion Status is returned 
*        set to SAI__OK.

*  Implementation Notes:
*     -  It exists purely to provide compatibility with the existing ERR_
*     library.
*     -  This subroutine is not documented and should not be used in
*     any new code.

*  Algorithm:
*     -  Use ERR_REP and then ERR_FLUSH.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     SLW: Sid Wright (UCL)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     17-APR-1983 (SLW):
*        Changed ERR_FLUSH call.
*     7-AUG-1989 (RFWS):
*        Converted to new prologue layout and added comments.
*     12-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     31-MAY-1991 (PCTR):
*        Return SAI__OK regardless of the success of ERR_FLUSH.
*     14-MAY-1993 (PCTR):
*        Removed STATUS = SAI__OK assignment on exit so that errors on 
*        message delivery can be detected by the application.
*     25-JAN-1996 (AJC):
*        Change argument ERROR to PARAM in code
*         and MSG to PARAM in prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) TEXT

*  Status:
      INTEGER STATUS

*.
 
*  Report the new error.
      CALL ERR_REP( PARAM, TEXT, STATUS )

*  Flush the error table.
      CALL ERR_FLUSH( STATUS )

      END
