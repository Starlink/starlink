      SUBROUTINE MT_FIX( STATUS )
*+
*  Name:
*     SUBROUTINE MT_FIX

*  Purpose:
*     Try hard to fix it so that tape file position is known.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MT_FIX( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     DMILLS: Dave Mills (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     23-OCT-81 (JRG):
*       AT4 version.
*     31-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*     02-JUN-89 (PCTR):
*       IUEDR Vn. 2.1
*       Conversion to SGP/16 style.
*     01-OCT-92 (DMILLS):
*       IUEDR Vn. 3.0
*       ADAMised version to run on multiple hardware platforms
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
      INCLUDE 'MAG_ERR'

*  Global Variables:
      INCLUDE 'CMTAPE'

*  Status:
      INTEGER STATUS     ! Global status.

*  Local Variables:
      INTEGER FILE
      INTEGER BLOCK

      LOGICAL START
      LOGICAL MOVED
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get tape position information.
      CALL MAG_POS( TCHAN, FILE, START, BLOCK, MOVED, STATUS )
      IF ( FILE .EQ. 0 ) THEN
         CALL MAG_REW( TCHAN, STATUS )
      END IF

 999  CONTINUE

      END
