      SUBROUTINE USR_NEWABS( STATUS )
*+
*  Name:
*     SUBROUTINE USR_NEWABS

*  Description:
*     The contents of CMABS are read from a specified file, and the
*     Calibration marked as needing update.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_NEWABS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     04-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     13-OCT-94 (MJC):
*       IUEDR Vn. 3.1-7
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

*  Local Constants:
      INTEGER FILENAMESIZE ! Maximum length of file name.
      PARAMETER (FILENAMESIZE = 81)

*  Status:
      INTEGER STATUS  ! Global status.

*  Global Variables:
      INCLUDE 'CMABS'

*  Local Variables:
      BYTE FILE( FILENAMESIZE ) ! File name.

      INTEGER ACTVAL  ! Parameter value count.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get Calibration.
      CALL DASSOC( '\\', '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999
      END IF

*   ABSFILE.
      CALL RDPARC( 'ABSFILE\\', .FALSE., FILENAMESIZE, FILE, ACTVAL,
     :             STATUS )
      IF ( STATUS .NE. SAI__OK) THEN
         CALL PARFER( 'ABSFILE\\', STATUS )
         GO TO 999

      ELSE
         CALL CNPAR( 'ABSFILE\\', STATUS )
         IF ( STATUS .NE. SAI__OK) THEN
            CALL PCANER( 'ABSFILE\\', STATUS )
            GO TO 999
         END IF
      END IF

*   Whatever happens, update will be needed.
      CALL MODCAL

*   Read file.
      NOABS = .TRUE.
      CALL RFABS( FILE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: replacing absolute calibration\\',
     :                STATUS )
         GO TO 999

      ELSE
         CALL RECAL( STATUS )
         IF ( STATUS .NE. SAI__OK)
     :      CALL ERROUT( 'Error: whilst recalibrating\\', STATUS )
      END IF

 999  CONTINUE

      END
