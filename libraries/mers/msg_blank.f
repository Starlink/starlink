      SUBROUTINE MSG_BLANK( STATUS )
*+
*  Name:
*     MSG_BLANK

*  Purpose:
*     Output a blank line.

*  Language:
*    Starlink Fortran 77

*  Invocation:
*     CALL MSG_BLANK( STATUS )

*  Description:
*     A blank line is output to the user. If the status argument is not
*     set to SAI__OK on entry, no action is taken. If an output error
*     occurs, an error report is made and the status argument returned
*     set to MSG__OPTER.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Use MSG1_PRINT to send a print message to the user interface.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-SEP-1990 (PCTR):
*        Original version.
*     24-JAN-1991 (PCTR):
*        Changed to use MSG1_PRINT (i.e. environment independent).
*     26-AUG-1992 (PCTR):
*        Output the blank line conditionally, assuming MSG__NORM to be
*        the priority.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Declarations:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants
      INCLUDE 'MSG_PAR'                 ! MSG_ public constants

*  Status:
      INTEGER STATUS

*.

*  Check the inherited global status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Mark a new error reporting context to ensure that no existing message 
*  tokens are annulled.
      CALL EMS_MARK

*  Call MSG_OUTIF with the delivery priority set to MSG__NORM.
      CALL MSG_OUTIF( MSG__NORM, 'MSG_BLANK', ' ', STATUS )

*  Release the current error reporting context.
      CALL EMS_RLSE

      END
