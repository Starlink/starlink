      SUBROUTINE USR_SAVE( STATUS )
*+
*  Name:
*     SUBROUTINE USR_SAVE

*  Purpose:
*     Force file updates.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_SAVE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*      Any revailing data changes are made permanent by writing the
*      appropriate files.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     13-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     13-SEP-94 (MJC):
*       IUEDR Vn. 3.1-3
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
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get Calibration.
      CALL DASSOC( '\\', '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )

      ELSE
         CALL FRDSN( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: updating files\\', STATUS )
         END IF
      END IF

      END
