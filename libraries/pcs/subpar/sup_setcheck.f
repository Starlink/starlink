
      SUBROUTINE SUBPAR_SETCHECK ( FLAG, STATUS )
*+
*  Name:
*     SUBPAR_SETCHECK

*  Purpose:
*     set flag for controlling NEEDS checks.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_SETCHECK ( FLAG, STATUS )

*  Description:
*     Sets whether the NEEDS lists for actions are checked for validity
*     before ACT is called.

*  Arguments:
*     FLAG=LOGICAL (given)
*        .TRUE. => the task fixed part validates the needs list of
*        the requested action before calling ACT.
*     STATUS=INTEGER

*  Algorithm:
*     Copy FLAG into the common block variable CHECKNEEDS.

*  Authors:
*     BDK: B D Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     06-MAR-1986 (BDK):
*        Original
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'


*  Arguments Given:
      LOGICAL FLAG       ! .TRUE. => the task fixed part validates the
                         ! needs list of the requested action before
                         ! calling ACT.

*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      CHECKNEEDS = FLAG

      END
