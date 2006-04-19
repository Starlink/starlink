      SUBROUTINE PARSECON_PAREND ( STATUS )
*+
*  Name:
*     PARSECON_PAREND

*  Purpose:
*     on ENDPARAMETER clear name.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_PAREND ( STATUS )

*  Description:
*     Clear the parameter name from the error report common block.

*  Arguments:
*     STATUS=INTEGER

*  Algorithm:
*     Set PRNAME to blank.

*  Authors:
*     A.J.Chipperfield
*     A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16.08.1990: Original (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'
      INCLUDE 'PARSECON3_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      PRNAME = ' '

      END
