      SUBROUTINE USR_NEWDISP( STATUS )
*+
*  Name:
*     SUBROUTINE USR_NEWDISP

*  Description:
*    The contents of CMDISP are read from a specified file, and the
*    Calibration marked as needing update.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_NEWDISP( STATUS )

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
*       IUEDR VN. 2.0
*     18-JAN-95 (MJC):
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

*  Global Variables:
      INCLUDE 'CMDISP'

*  Local Variables:
      BYTE FILE( 81 )    ! File name.

      INTEGER ACTVAL     ! Parameter value count.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get Calibration.
      CALL DASSOC( '\\', '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999
      END IF

*   DISPFILE.
      CALL RDPARC( 'DISPFILE\\', .FALSE., 81, FILE, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'DISPFILE\\', STATUS )
         GO TO 999

      ELSE
         CALL CNPAR( 'DISPFILE\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'DISPFILE\\', STATUS )
            GO TO 999
         END IF
      END IF

*   Whatever happens, update will be needed.
      CALL MODCAL

*   Read file.
      NODISP = .TRUE.
      CALL RFDISP( FILE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: replacing dispersion constants\\',
     :                STATUS )
         GO TO 999
      END IF

 999  CONTINUE

      END
