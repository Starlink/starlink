      SUBROUTINE USR_CULIMITS( STATUS )
*+
*  Name:
*     SUBROUTINE USR_CULIMITS

*  Description:
*     Open display zone and measure coordinate limits.  Then write
*     appropriate display limit parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_CULIMITS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-AUG-82 (JRG):
*       IUEDR Vn. 1.0
*     09-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*     12-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
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
      INTEGER STATUS        ! Global status.

*  External References:
      LOGICAL STR_SIMLR     ! Caseless string equality.

*  Global Variables:
      INCLUDE 'CMGRAF'
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Open graphics.
      CALL GRF_OPCURS( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: graphics cursor unavailable\\', STATUS )

      ELSE IF ( STR_SIMLR( 'S\\', XLAB ) .AND.
     :          STR_SIMLR( 'L\\', YLAB ) ) THEN
         CALL SLCULM( STATUS )

      ELSE
         CALL XYCULM( STATUS )
      END IF

      END
