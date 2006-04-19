      SUBROUTINE PARSECON_SETPROM ( TEXT, STATUS )
*+
*  Name:
*     PARSECON_SETPROM

*  Purpose:
*     Stores prompt string for a parameter.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_SETPROM ( TEXT, STATUS )

*  Description:
*     The prompt-string for the parameter currently being defined is
*     stored.

*  Arguments:
*     TEXT=CHARACTER*(*) (given)
*        text of the prompt
*     STATUS=INTEGER

*  Algorithm:
*     Remove superfluous quotes from the prompt-string and store it in
*     common.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     02.10.1984 Original (REVAD::BDK)
*     17.10.1990 Remove unused declaration (RLVAD::AJC)
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


*  Arguments Given:
      CHARACTER*(*) TEXT              ! prompt-string


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Remove surrounding quotes, and simplify escaped quotes.
*
      CALL STRING_STRIPQUOT ( TEXT, PARPROM(PARPTR), STATUS )

      END
