      SUBROUTINE SUBPAR_TERMFACE ( UFACE, STATUS )
*+
*  Name:
*     SUBPAR_TERMFACE

*  Purpose:
*     set flag saying whether task connected to terminal.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_TERMFACE ( UFACE, STATUS )

*  Description:
*     Set a flag in the parameter system common blocks saying whether
*     the task is a 'normal' task, or whether, instead, it is being run
*     from a terminal. In the latter case, all output and prompting goes
*     directly to the terminal instead of being sent to another task.

*  Arguments:
*     UFACE=LOGICAL (given)
*        .TRUE. => attached to terminal
*        .FALSE. => controlled by another task
*     STATUS=INTEGER

*  Algorithm:
*     Set the COMMON variable RUNFACE.

*  Authors:
*     BDK: B D Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     11-NOV-1985 (BDK):
*        Original
*     05-MAY-1987 (BDK):
*        Make RUNFACE an integer
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
      LOGICAL UFACE           ! .TRUE. => attached to terminal
                              ! .FALSE. => controlled by another task


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( UFACE ) THEN
         RUNFACE = SUBPAR__TERM
      ELSE
         RUNFACE = SUBPAR__TASK
      ENDIF

      END
