*+  PARSECON_TABENT - Look-up parsing state-table
      SUBROUTINE PARSECON_TABENT ( STATE, TOKTYPE, ACTCODE, 
     :  NEWSTATE, STATUS )
*    Description :
*     Look-up parsing state-table
*    Invocation :
*     CALL PARSECON_TABENT ( STATE, TOKTYPE; ACTCODE, 
*    :  NEWSTATE )
*    Parameters :
*     STATE=INTEGER (given)
*           current parsing state
*     TOKTYPE=INTEGER (given)
*           type of current token
*     ACTCODE=INTEGER (returned)
*           action code 
*     NEWSTATE=INTEGER (returned)
*           new parsing state
*     STATUS=INTEGER
*    Method :
*   Look up (action code, new state) for (state, token type).
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     W.F.Lupton (RGO)
*    History :
*     18.09.1984:  VAX version (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'

*    Import :
      INTEGER STATE           ! current parsing state

      INTEGER TOKTYPE         ! type of current token

*    Export :
      INTEGER ACTCODE         ! action code (0 = none)

      INTEGER NEWSTATE        ! new parsing state

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'PARSECON_CMN'

*-

      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Look-up the two values corresponding to the given combination of 
*   parse-state and token-type. An invalid combination will result in
*          ACTCODE = ERROR
*          NEWSTATE = FACEGOT
*
      ACTCODE = ACTTAB(STATE,TOKTYPE)
      NEWSTATE = STATETAB(STATE,TOKTYPE)

      END
