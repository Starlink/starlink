      SUBROUTINE TASK_PUT_REQUEST ( REQUEST, STATUS )
*+
*  Name:
*     TASK_PUT_REQUEST

*  Purpose:
*     Set REQUEST for action be the fixed-part

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_PUT_REQUEST ( REQUEST, STATUS )

*  Description:
*     Sets the REQUEST for interpretation by the fixed-part.
*     This simply involves copying to COMMON.

*  Arguments:
*     REQUEST=INTEGER (given)
*           The REQUEST from the application for action by the 
*           fixed-part.
*     STATUS=INTEGER

*  Algorithm:
*     Copy information to COMMON.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     22-AUG-1991 (REVAD::BDK):
*        Original
*     09-OCT-1992 (RLVAD::AJC):
*        Add PAR_PAR
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
      INTEGER REQUEST   ! the REQUEST for the fixed-part

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'TASK_CMN'
*.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Simply copy the REQUEST to COMMON.
*
      CURACTREQUEST = REQUEST

      END
