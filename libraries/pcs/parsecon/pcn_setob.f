*+  PARSECON_SETOB - Set OBEY flag
      SUBROUTINE PARSECON_SETOB ( FLAG, STATUS )
*    Description :
*     Sets the flag indicating whether OBEY is valid for the current 
*     action
*    Invocation :
*     CALL PARSECON_SETOB ( FLAG, STATUS )
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*     FLAG=LOGICAL (given)
*           .TRUE. => OBEY is valid for the current action.
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

      MAYOB(ACTPTR) = FLAG

      END
