      SUBROUTINE USR_MTSKIPF( STATUS )
*+
*  Name:
*     SUBROUTINE USR_MTSKIPF

*  Description:
*     %NSKIP tape marks are skipped along the tape on drive specified
*     by the %DRIVE parameter. %NSKIP can be negative.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_MTSKIPF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     Get channel to tape drive and skip tape marks.

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
*     06-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*       MT_ calls replced with MAG_ calls.
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
      INTEGER ISTAT   ! Status.
      INTEGER NSKIP   ! Number of files to be skipped.
      INTEGER TP      ! Tape descriptor.
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

*   NSKIP parameter.
      CALL RDPARI( 'NSKIP\\', .FALSE., 1, NSKIP, ACTVAL, STATUS )
      CALL WRPARI( 'NSKIP\\', 1, 1, ISTAT )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'NSKIP\\', STATUS )
         GO TO 999

      ELSE IF ( ISTAT .NE. 0 ) THEN
         CALL ERRPAR( 'NSKIP\\' )
         CALL ERROUT( ': parameter write error\\', STATUS )
         GO TO 999
      END IF

*   Look for non-zero skip.
      IF ( NSKIP .NE. 0 ) THEN
         ISTAT = SAI__OK
         CALL MAG_SKIP( TCHAN, NSKIP, ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( ISTAT )
            CALL ERROUT( '\\', STATUS )
            GO TO 999
         END IF
      END IF

 999  CONTINUE

      END
