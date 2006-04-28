      SUBROUTINE TASK_INIT_MESSINFO ( STATUS )
*+
*  Name:
*     TASK_INIT_MESSINFO

*  Purpose:
*     Initialise list of active subsidiary actions

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_INIT_MESSINFO ( STATUS )

*  Description:
*     Initialises the list of active subsidiary actions. This simply involves
*     clearing the action pointers to zero, since the first zero action
*     pointer indicates the entry past the last used one.

*  Arguments:
*     STATUS=INTEGER

*  Algorithm:
*     Clear all action pointers to zero.

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

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'TASK_CMN'

*  Local Variables:
      INTEGER I           ! counter
*.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Cycle through the list clearing all action pointers to zero (end of list).
*
      DO I = 1, TASK__MAXSUB
         MESACTPTR(I) = 0
      ENDDO

      END
