*+  PARSECON_TABSET - puts values into state-table for interface parsing
      SUBROUTINE PARSECON_TABSET ( STATE, TOKTYPE, ACTCODE, NEWSTATE )
*    Description :
*     The given action-code and new parse state are loaded into the 
*     state table at the location corresponding to the given state and 
*     token-type.
*    Invocation :
*     CALL PARSECON_TABSET ( STATE, TOKTYPE, ACTCODE, NEWSTATE )
*     STATE=INTEGER (given)
*           current parse-state
*     TOKTYPE=INTEGER (given)
*           type of token read from interface file
*     ACTCODE=INTEGER (given)
*           code for action to be taken
*     NEWSTATE=INTEGER (given)
*           new parse state
*    Method :
*     There are a pair of 2-d byte arrays held in a common block.
*     One holds action codes (ACTTAB) the other parse states 
*     (STATETAB). This routine simply sets the given action code and 
*     state code into the (STATE,TOKTYPE) elements of each of the two 
*     arrays. The arrays have previously been filled with values 
*     corresponding to error conditions, and so PARSECON_TABSET is only 
*     called to fill-in valid conditions.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     17.09.1984:  Original (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'

*    Import :
      INTEGER STATE          ! current parse-state

      INTEGER TOKTYPE        ! token-type

      INTEGER ACTCODE        ! code for action to be taken

      INTEGER NEWSTATE       ! new parse-state

*    Global variables :
      INCLUDE 'PARSECON_CMN'

*-

      ACTTAB(STATE,TOKTYPE) = ACTCODE
      STATETAB(STATE,TOKTYPE) = NEWSTATE

      END
