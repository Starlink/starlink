      SUBROUTINE KPG1_REPRT( MESS, QUIET, LOG, FD, STATUS )
*+
*  Name:
*     KPG1_REPRT

*  Purpose:
*     Reports a MSG message to user and also optionally write it to a file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_REPRT( MESS, QUIET, LOG, FD, STATUS )

*  Description:
*     This routine dislays the supplied message using MSG_OUT (unless
*     QUIET is .TRUE), and (if LOG is .TRUE.) writes out the same text 
*     to the file identified by FD.

*  Arguments:
*     MESS = CHARACTER * ( * ) (Given)
*        The message, which may contain MSG tokens.
*     QUIET = LOGICAL (Given)
*        Supress screen output?
*     LOG = LOGICAL (Given)
*        Write the text to  a file?
*     FD = INTEGER (Given)
*        An FIO identifier for a file. If LOG is .TRUE., then the message
*        is written to this file. 
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-AUG-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER MESS*(*)
      LOGICAL QUIET
      LOGICAL LOG
      INTEGER FD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER OPSTR*255        ! Buffer for message text
      INTEGER OPLEN              ! Length of message text

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Show the message to the user.
      IF( .NOT. QUIET ) CALL MSG_OUT( ' ', MESS, STATUS )

*  If writing to the file...
      IF( LOG ) THEN 

*  Get the expanded text of the message.
         CALL MSG_LOAD( ' ', MESS, OPSTR, OPLEN, STATUS )

*  Write it to the file.
         CALL FIO_WRITE( FD, OPSTR( : OPLEN ), STATUS )

      END IF

      END
