      SUBROUTINE USR_MTSHOW( STATUS )
*+
*  Name:
*     SUBROUTINE USR_MTSHOW

*  Description:
*     The current position of the tape on drive specified by
*     the %DRIVE parameter is printed.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_MTSHOW( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     Get tape descriptor. Use utility routine to get and print tape
*     position.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     SLW: Sid Wright (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-JAN-82 (JRG):
*       IUEDR Vn. 1.0
*     19-AUG-82 (SLW):
*       Revise tape open and close calls.
*     04-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     19-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS     ! Global status.

*  Local Variables:
      INTEGER TP         ! Tape descriptor.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Access tape drive.
      CALL MT_OPEN( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERRPAR( 'DRIVE\\' )
         CALL ERROUT( ': tape drive not available\\', STATUS )
         GO TO 999
      END IF

*   Find and print tape position information.
      CALL TAPE_SHOP( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      END IF

 999  CONTINUE

      END
