      SUBROUTINE TASK_PUT_DELAY ( DELAY, STATUS )
*+
*  Name:
*     TASK_PUT_DELAY

*  Purpose:
*     Set delay before next entry for current action

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_PUT_DELAY ( DELAY, STATUS )

*  Description:
*     Sets the delay in milliseconds before the next entry for the current
*     action. This simply involves copying to COMMON.

*  Arguments:
*     DELAY=INTEGER (given)
*           The delay before the next action entry in milliseconds. Depending
*           on the status returned from ACT, this may act as as a time-out
*           period. -1 means infinity.
*     STATUS=INTEGER

*  Algorithm:
*     Copy information to COMMON.

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

*  Arguments Given:
      INTEGER DELAY   ! the delay before the next action entry

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'TASK_CMN'
*.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Simply copy the delay to COMMON.
*
      CURACTDELAY = DELAY

      END
