*+  USI0_FNDADI - Find ADI identifier associated with named parameter
      SUBROUTINE USI0_FNDADI( PAR, ID, STATUS )
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
      INCLUDE 'ADI_PAR'
*    Import :
      CHARACTER*(*)          PAR       ! Parameter name
*    Export :
      INTEGER			ID			! Parameter identifier
*    Status :
      INTEGER                STATUS
*    Local variables :
      INTEGER PSID
*-

*  Check inherited global status
      IF (STATUS.NE.SAI__OK) RETURN

*  Initialise
      ID = ADI__NULLID

*  Locate parameter storage
      CALL USI0_FNDPSL( PAR, .FALSE., PSID, STATUS )

*  Found it?
      IF ( STATUS .EQ. SAI__OK ) THEN
        CALL ADI_CGET0I( PSID, 'ID', ID, STATUS )
        CALL ADI_ERASE( PSID, STATUS )
      ELSE
        CALL ERR_ANNUL( STATUS )
      END IF

      END
