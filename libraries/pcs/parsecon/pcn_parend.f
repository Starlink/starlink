*+  PARSECON_PAREND - on ENDPARAMETER clear name
      SUBROUTINE PARSECON_PAREND ( STATUS )
*    Description :
*     Clear the parameter name from the error report common block.
*    Invocation :
*     CALL PARSECON_PAREND ( STATUS )
*    Parameters :
*     STATUS=INTEGER
*    Method :
*     Set PRNAME to blank.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     A.J.Chipperfield
*     A J Chipperfield (STARLINK)
*    History :
*     16.08.1990: Original (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'SUBPAR_CMN'
      INCLUDE 'PARSECON3_CMN'

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      PRNAME = ' '

      END
