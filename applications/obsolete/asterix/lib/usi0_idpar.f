*+  USI0_IDPAR - Identifies the parameter associated with an identifier
      SUBROUTINE USI0_IDPAR( ID, PAR, STATUS )
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER			ID			! Parameter identifier
*    Export:
      CHARACTER*(*)          PAR       ! Parameter name
*    Status :
      INTEGER                STATUS
*    Local variables :
      LOGICAL FOUND
*-

      IF (STATUS.EQ.SAI__OK) THEN
        PAR=' '
        CALL ADI_THERE( ID, '.USI_PAR', FOUND, STATUS )
        IF ( FOUND ) THEN
          CALL ADI_CGET0C(  ID, '.USI_PAR', PAR, STATUS )
        END IF
        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT( 'USI0_IDPAR', STATUS )
        ENDIF
      ENDIF
      END
