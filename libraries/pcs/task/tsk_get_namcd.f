      SUBROUTINE TASK_GET_NAMECODE ( NAMECODE, STATUS )
*+
*  Name:
*     TASK_GET_NAMECODE

*  Purpose:
*     Get current action name code

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_GET_NAMECODE ( NAMECODE, STATUS)

*  Description:
*     Gets current action namecode. This simply involves copying the namecode
*     from COMMON.

*  Arguments:
*     NAMECODE=INTEGER (returned)
*           The action name code
*     STATUS=INTEGER

*  Algorithm:
*     Copy information from COMMON.

*  Authors:
*     W.F.Lupton (AAOEPP::WFL)
*     {enter_new_authors_here}

*  History:
*     29-APR-1989 (AAOEPP::WFL):
*        Original
*     23-APR-1991 (REVAD::BDK):
*        Rearrange INCLUDE files
*     06-MAY-1991 (REVAD::BDK):
*        Remove ADAMDEFNS
*     04-OCT-1992 (RLVAD::AJC):
*        Add PAR_PAR for porting
*     24-AUG-1993 (RLVAD::AJC):
*        Use SUBPAR_SYS not PAR_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'MESSYS_LEN'
      INCLUDE 'TASK_PAR'

*  Arguments Returned:
      INTEGER NAMECODE    ! the action name code

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'TASK_CMN'
*.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Simply copy the value from COMMON.
*
      NAMECODE = CURACTNAMECODE

      END
