*+  PARSECON_ACTRES - Set flag indicating RANGE or IN limit on action
      SUBROUTINE PARSECON_ACTRES ( FLAG, STATUS )
*    Description :
*     Sets the flag indicating whether the NEEDS parameter being defined 
*     for the current action has a RANGE or an IN constraint.
*    Invocation :
*     CALL PARSECON_ACTRES ( FLAG, STATUS )
*    Parameters :
*     FLAG=LOGICAL (given)
*           .TRUE. => RANGE constraint on the value of the parameter 
*           required for the action currently being defined.
*     STATUS=INTEGER
*    Method :
*     Set the value into the common-block variable.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*    History :
*     02.10.1984:  Original (REVAD::BDK)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*    Import :
      LOGICAL FLAG                 ! value to be set

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'SUBPAR_CMN'

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      NEEDCONT(NEEDPTR) = FLAG

      END
