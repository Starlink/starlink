      SUBROUTINE USR_CURSOR( STATUS )
*+
*  Name:
*     SUBROUTINE USR_CURSOR

*  Description:
*     Make a measurement with the graphics cursor on current graph.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_CURSOR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     Open display zone and measure coordinates in value coordinates.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-AUG-82 (JRG):
*       IUEDR Vn. 1.0
*     08-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*     29-JUN-94 (MJC):
*       IUEDR Vn. 3.1-1
*       Added inherited status check, minor tidy of code.
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

*  Global Variables:
      INCLUDE 'CMGRAF'

*  Status:
      INTEGER STATUS     ! Global status.

*  External References:
      LOGICAL STR_SIMLR  ! Caseless string equality.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Open graphics.
      CALL GRF_OPCURS( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: graphics cursor unavailable\\', STATUS )

      ELSE IF ( STR_SIMLR( 'S\\', XLAB ) .AND.
     :          STR_SIMLR( 'L\\', YLAB ) ) THEN
         CALL SLCURS( STATUS )

      ELSE
         CALL XYCURS( STATUS )
      END IF

      END
