*+  PARSECON_ACTEND - on ENDACTION check for ACTKEY clashes
      SUBROUTINE PARSECON_ACTEND ( STATUS )
*    Description :
*     Check the list of action keywords for a clash
*     and remove the action name from the error report common block.
*    Invocation :
*     CALL PARSECON_ACTEND ( STATUS )
*    Parameters :
*     STATUS=INTEGER
*    Method :
*     The list of action keywords is searched sequentially
*     and set ACNAME to blank.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*    History :
*     26.05.1987:  Original (REVAD::BDK)
*     16.08.1990:  Reset ACNAME for error reports (RLVAD::AJC)
*     24.02.1992:  Report errors (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PARSECON_ERR'

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'SUBPAR_CMN'
      INCLUDE 'PARSECON3_CMN'

*    Local variables :
      INTEGER NAMECODE                  ! counter for searching
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO NAMECODE = 1, ACTPTR-1

         IF ( ACTKEY(ACTPTR) .EQ. ACTKEY(NAMECODE) ) THEN
           STATUS = PARSE__OLDACTKEY
           CALL EMS_REP( 'PCN_ACTEND1',
     :     'PARSECON: Action Keyword multiply defined', STATUS )
         ENDIF

      ENDDO

      ACNAME = ' '

      END
