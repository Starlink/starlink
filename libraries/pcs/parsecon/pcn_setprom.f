*+  PARSECON_SETPROM - Stores prompt string for a parameter
      SUBROUTINE PARSECON_SETPROM ( TEXT, STATUS )
*    Description :
*     The prompt-string for the parameter currently being defined is 
*     stored.
*    Invocation :
*     CALL PARSECON_SETPROM ( TEXT, STATUS )
*    Parameters :
*     TEXT=CHARACTER*(*) (given)
*           text of the prompt
*     STATUS=INTEGER
*    Method :
*     Remove superfluous quotes from the prompt-string and store it in 
*     common.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*    History :
*     02.10.1984 Original (REVAD::BDK)
*     17.10.1990 Remove unused declaration (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*    Import :
      CHARACTER*(*) TEXT              ! prompt-string

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'SUBPAR_CMN'

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Remove surrounding quotes, and simplify escaped quotes.
*
      CALL STRING_STRIPQUOT ( TEXT, PARPROM(PARPTR), STATUS )

      END
