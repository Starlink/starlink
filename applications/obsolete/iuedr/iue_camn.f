      SUBROUTINE IUE_CAMN( CAMERA, ICAM, STATUS )
*+
*  Name:
*     SUBROUTINE IUE_CAMN

*  Description:
*     The camera name (LWP, LWR, SWP) is used to get camera
*     number (1, 2, 3).
*     So the SWR camera is not allowed.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IUE_CAMN( CAMERA, ICAM, STATUS )

*  Arguments:
*     CAMERA = BYTE ( 16 ) (Given)
*        The name of the selected camera.
*     ICAM = INTEGER (Returned)
*        Camera index.
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
*     04-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     18-SEP-94 (MJC):
*       IUEDR Vn. 3.1-8
*     19-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Defintions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      BYTE CAMERA( 16 )  ! Camera name.

*  Arguments Returned:
      INTEGER ICAM       ! Camera index.

*  Status:
      INTEGER STATUS     ! Global status.

*  External References:
      LOGICAL STR_SIMLR  ! Caseless string equality.

*  Local Constants:
      INTEGER ERR        ! Error code.
      PARAMETER ( ERR = -3 )
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( STR_SIMLR( 'SWP\\', CAMERA ) ) THEN
         ICAM = 3

      ELSE IF ( STR_SIMLR( 'LWR\\', CAMERA ) ) THEN
         ICAM = 2

      ELSE IF ( STR_SIMLR( 'LWP\\', CAMERA ) ) THEN
         ICAM = 1

      ELSE
         STATUS = ERR
      END IF

      END
