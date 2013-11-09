*+  USI0_STOREI - Stores ADI identifier in internal common
      SUBROUTINE USI0_STOREI( PAR, ID, IO, ISTEMP, STATUS )
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
      CHARACTER*(*)          PAR       ! Parameter name
      INTEGER			ID			! Parameter identifier
      CHARACTER*1            IO	       ! input or output
      LOGICAL			ISTEMP
*    Export :

*    Status :
      INTEGER                STATUS
*    Local variables :
      INTEGER			PSID			! Parameter storage
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate parameter slot
      CALL USI0_FNDPSL( PAR, .TRUE., PSID, STATUS )

*  Store data
      CALL ADI_CPUT0C( PSID, 'IO', IO, STATUS )
      CALL ADI_CPUT0I( PSID, 'ID', ID, STATUS )
      CALL ADI_CPUT0L( PSID, 'TEMP', ISTEMP, STATUS )

*  Release parameter slot
      CALL ADI_ERASE( PSID, STATUS )

*  Mark identifier as belonging to USI
      CALL ADI_CPUT0C( ID, '.USI_PAR', PAR, STATUS )

*  Report any problems
      IF (STATUS.NE.SAI__OK) THEN
        CALL AST_REXIT( 'USI0_STOREI', STATUS )
      END IF

      END
