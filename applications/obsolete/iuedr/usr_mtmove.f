      SUBROUTINE USR_MTMOVE( STATUS )
*+
*  Name:
*     SUBROUTINE USR_MTMOVE

*  Description:
*     The tape on the tape drive specified by the %DRIVE parameter
*     is moved to position specified by the %FILE and %BLOCK
*     parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_MTMOVE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     Get tape descriptor.
*     Get required file and block number (relative to start of file)
*     from parameter system.
*     Use MT library routine MTMOVE.

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
*     30-SEP-94 (MJC):
*       IUEDR Vn. 3.1-6
*     06-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*       MT_ calls replaced by MAG_ calls.
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
      INTEGER STATUS  ! Global status.

*  Local Variables:
      INTEGER ACTVAL  ! Parameter value count.
      INTEGER FILE    ! File number.
      INTEGER ISTAT   ! Local status.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      ISTAT = SAI__OK

*   Access tape drive.
      CALL MT_OPEN( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERRPAR( 'DRIVE\\' )
         CALL ERROUT( ': tape drive not available\\', STATUS )
         GO TO 999
      END IF

*   FILE parameter.
      CALL RDPARI( 'FILE\\', .FALSE., 1, FILE, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'FILE\\', STATUS )
         GO TO 999

      ELSE IF ( FILE .LE. 0 ) THEN
         CALL ERRPAR( 'FILE\\' )
         CALL ERROUT( ': illegal value\\', STATUS )
         GO TO 999
      END IF

*   Move to requested position on tape.
      CALL MAG_MOVE( TCHAN, FILE, .TRUE., 1, ISTAT )
      IF ( ISTAT .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( ISTAT )
         CALL ERR_OUT( '\\', STATUS )
         GO TO 999
      END IF

 999  CONTINUE

      END
