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
      INCLUDE 'ADI_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USI_CMN'
*    Import :
      CHARACTER*(*)          PAR       ! Parameter name
      INTEGER			ID			! Parameter identifier
*    Status :
      INTEGER                STATUS
*    Local variables :
      INTEGER I
      LOGICAL FOUND
*-

      IF (STATUS.EQ.SAI__OK) THEN
        I=1
        FOUND = .FALSE.
*  scan list to find first empty slot
        DO WHILE (.NOT.SPARE.AND.I.LE.USI__NMAX)
          IF ( DS(I).USED .AND. (PAR.EQ.DS(I).PAR) ) THEN
            FOUND=.TRUE.
          ELSE
            I=I+1
          ENDIF
        ENDDO

        IF ( FOUND ) THEN
          DS(I).ADI_ID = ID
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
