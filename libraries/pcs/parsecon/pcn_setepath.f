*+  PARSECON_SETEPATH - Set EPATH
      SUBROUTINE PARSECON_SETEPATH ( NAME, STATUS )
*    Description :
*     Sets search-path for execution module into common block.
*    Invocation :
*     CALL PARSECON_SETEPATH ( NAME, STATUS )
*    Parameters :
*     NAME=CHARACTER*(*) (given)
*           EPATH string
*     STATUS=INTEGER
*    Method :
*     Put the given string into the common-block variable.
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
      CHARACTER*(*) NAME                 ! string to be inserted

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'SUBPAR_CMN'

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      EXEPATH = NAME

      END
