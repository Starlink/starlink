      SUBROUTINE RDSPEC( STATUS )
*+
*  Name:
*     SUBROUTINE RDSPEC

*  Purpose:
*    The "current" spectrum as indicated by APERTURE (LORES) or
*    ORDER (HIRES) is accessed if possible.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDSPEC( STATUS )

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
*     07-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     26-JUL-94 (MJC):
*       IUEDR Vn. 3.1-2
*     07-FEB-95 (MJC):
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
      INCLUDE 'CMHEAD'

*  Status:
      INTEGER STATUS     ! Global status.

*  Local Variables:
      INTEGER IRES       ! Resolution index.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get Calibration and Spectrum (raw).
      CALL DASSOC( 'S\\', 'T\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999
      END IF

      CALL IUE_RESN( RESOL, IRES, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: resolution invalid\\', STATUS )

      ELSE IF ( IRES .EQ. 1 ) THEN
         CALL RDLO( STATUS )

      ELSE IF ( IRES. EQ. 2 ) THEN
         CALL RDHI( STATUS )
      END IF

 999  CONTINUE

      END
