      SUBROUTINE USR_MTSKIPEOV( STATUS )
*+
*  Name:
*     SUBROUTINE USR_MTSKIPEOV

*  Description:
*     An end-of-volume (EOV) marker (two consecutive tape marks)
*     is skipped over in the forward direction on drive %DRIVE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_MTSKIPEOV( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     Get channel to tape drive and jump over EOV.

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

*  Global Variables:
      INCLUDE 'CMTAPE'

*  Status:
      INTEGER STATUS     ! Global status.

*  Local Variables:
      INTEGER ISTAT      ! Local status.
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

*   Jump over EOV condition.
      ISTAT = SAI__OK
      CALL MAG_JEOV( TCHAN, ISTAT )
      IF ( ISTAT .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( ISTAT )
         CALL ERROUT( '\\', STATUS )
      END IF

 999  CONTINUE

      END
