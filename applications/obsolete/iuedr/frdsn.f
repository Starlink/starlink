      SUBROUTINE FRDSN( STATUS )
*+
*  Name:
*     SUBROUTINE FRDSN

*  Purpose:
*     If there is a current dataset, and this has any unwritten components,
*     then these are wriiten to disk.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FRDSN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (Starlink)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     22-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*     06-JUL-94 (MJC):
*       IUEDR Vn. 3.1-1
*     26-APR-95 (MJC):
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
      INCLUDE 'CMFILE'

*  Status:
      INTEGER STATUS      ! Global status.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( .NOT. NODSN ) THEN
         CALL WRIMG( DSNAME, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERRSTR( DSNAME )
            CALL ERROUT( ': update error\\', STATUS )
         END IF
      END IF

      END
