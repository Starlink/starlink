      SUBROUTINE USR_NEWRIP( STATUS )
*+
*  Name:
*     SUBROUTINE USR_NEWRIP

*  Description:
*     The contents of CMRIP are read from a specified file, and the
*     Calibration marked as needing update.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_NEWRIP( STATUS )

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
*     05-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
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
      INTEGER STATUS   ! Global status.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMRIP'

*  Local Variables:
      BYTE FILE( 81 )  ! File name.

      INTEGER ACTVAL   ! Parameter value count.
      INTEGER IRES     ! Resolution index.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get Calibration.
      CALL DASSOC( '\\', '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999
      END IF

*   Check that resolution is HIRES.
      CALL IUE_RESN( RESOL, IRES, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: resolution invalid\\', STATUS )
         GO TO 999

      ELSE IF ( IRES .NE. 2 ) THEN
         CALL ERROUT( 'Error: not HIRES\\', STATUS )
         GO TO 999
      END IF

*   RIPFILE.
      CALL RDPARC( 'RIPFILE\\', .FALSE., 81, FILE, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'RIPFILE\\', STATUS )
         GO TO 999

      ELSE
         CALL CNPAR( 'RIPFILE\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'RIPFILE\\', STATUS )
            GO TO 999
         END IF
      END IF

*   Whatever happens, update will be needed.
      CALL MODCAL

*   Read file.
      NORIP = .TRUE.
      CALL RFRIP( FILE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: replacing absolute calibration\\',
     :                STATUS )

      ELSE
         CALL RECAL( STATUS )
         IF ( STATUS .NE. SAI__OK )
     :      CALL ERROUT( 'Error: recalibrating\\', STATUS )
      END IF

 999  CONTINUE

      END
