*+  PARSECON_MESTEXT - Stores text for a message parameter
      SUBROUTINE PARSECON_MESTEXT ( TEXT, STATUS )
*    Description :
*     The parameter currently being defined is a message parameter.
*     Set-up the parameter as a message, and store the message text.
*    Invocation :
*     CALL PARSECON_MESTEXT ( TEXT, STATUS )
*    Parameters :
*     TEXT=CHARACTER*(*) (given)
*           text of the message
*     STATUS=INTEGER
*    Method :
*     Mark the most-recently defined parameter for READ access, set 
*     VPATH to INTERNAL, set the type to character, and put the text 
*     string into the static default.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*    History :
*     02.10.1984
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PAR'

*    Import :
      CHARACTER*(*) TEXT              ! text of message

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'SUBPAR_CMN'

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Set type
*
      PARTYPE(PARPTR) = SUBPAR__CHAR
*
*   Set text as static default
*
      CALL PARSECON_SETDEF ( TEXT, STATUS )
*
*   Mark parameter for READ access and INTERNAL VPATH
*
      PARWRITE(PARPTR) = .FALSE.
      PARVPATH(1,PARPTR) = SUBPAR__INTERNAL

      END
