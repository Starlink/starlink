*+  PARSECON_SETMON - set-up for parsing monolith IFL
      SUBROUTINE PARSECON_SETMON ( NAME, STATUS )
*    Description :
*     Handle MONOLITH statement
*    Invocation :
*     CALL PARSECON_SETMON ( NAME, STATUS )
*    Parameters :
*     NAME=CHARACTER*(*) (given)
*           name of the monolith
*     STATUS=INTEGER
*    Method :
*     Set the flag which indicates that the interface file is the 
*     declaration for a monolith.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*    History :
*     23.08.1985:  Original (REVAD::BDK)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*    Import :
      CHARACTER*(*) NAME               ! name of the monolith

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'SUBPAR_CMN'
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      MONOLITH = .TRUE.
      FACENAME = NAME

      END
