*+  USI0_UPDID - Updates the identifier associated with a parameter
      SUBROUTINE USI0_UPDID( PAR, ID, STATUS )
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
*    Status :
      INTEGER                STATUS
*    Local variables :
      INTEGER			PSID			! Parameter storage
*-

      IF (STATUS.EQ.SAI__OK) THEN

*    Locate parameter storage
        CALL USI0_FNDPSL( PAR, .FALSE., PSID, STATUS )

*    Found ok?
        IF ( STATUS .EQ. SAI__OK ) THEN

*      Update identifier
          CALL ADI_CPUT0I( PSID, 'ID', ID, STATUS )
          CALL ADI_ERASE( PSID, STATUS )

*      Make link from identifier to USI
          CALL ADI_CPUT0C( ID, '.USI_PAR', PAR, STATUS )

        ELSE
          STATUS=SAI__ERROR
          CALL ERR_REP(' ', 'Parameter ^P has not be associated by USI',
     ;                  STATUS )
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT( 'USI0_UPDID', STATUS )
        ENDIF
      ENDIF
      END
