*+  PARSECON_STFACE - Action on INTERFACE field
      SUBROUTINE PARSECON_STFACE ( STATUS )
*    Description :
*     Performs the ENDINTERFACE action if the previous ENDINTERFACE is
*     missing
*    Invocation :
*     CALL PARSECON_STFACE ( STATUS )
*    Parameters :
*     STATUS=INTEGER
*    Method :
*     Looks at ACNAME. If it is set, the ENDINTERFACE must have been missing
*     so a message is reported and the ENDINTERFACE action routine is called.
*     This checks for correct POSITION numbering.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     A J Chipperfield (RLVAD::AJC)
*     A J Chipperfield (STARLINK)
*    History :
*     25.02.1992:  Original (RLVAD::AJC)
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

*    Local Variables
      INTEGER ISTAT               ! Local status
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( ACNAME .NE. ' ' ) THEN
*   The ENDINTERFACE must have been missing
         ISTAT = PARSE__MISEND
         CALL EMS_REP ( 'PCN_STFACE1',
     :   'PARSECON: Missing ENDINTERFACE', ISTAT )
         
         CALL PARSECON_FACEND ( STATUS )
*   Ensure that some error is reported
         IF ( STATUS .EQ. SAI__OK ) STATUS = PARSE__MISEND

      ENDIF

      END
