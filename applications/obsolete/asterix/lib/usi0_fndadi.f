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
      INCLUDE 'DAT_PAR'
      INCLUDE 'USI_CMN'
*    Import :
      CHARACTER*(*)          PAR       ! Parameter name
*    Export :
      INTEGER			ID			! Parameter identifier
*    Status :
      INTEGER                STATUS
*    Local variables :
      INTEGER I
      LOGICAL FOUND
*-

      IF (STATUS.EQ.SAI__OK) THEN
        I=1
        FOUND=.FALSE.
*  scan list to find first empty slot
        DO WHILE ( (STATUS.EQ.SAI__OK) .AND. .NOT.FOUND .AND.
     :          (I.LE.USI__NMAX))
          IF ( DS(I).USED ) THEN
            IF ( PAR .EQ. DS(I).PAR ) THEN
              IF ( DS(I).ADI_ID .NE. ADI__NULLID ) THEN
                ID = DS(I).ADI_ID
                FOUND = .TRUE.
              ELSE
                STATUS = SAI__ERROR
                CALL ERR_REP( ' ', 'Cannot access ADI identifier '/
     :              /'when ADI association not performed', STATUS )
              END IF
            ELSE
              I = I + 1
            END IF
          ELSE
            I = I + 1
          END IF
        END DO

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT('USI0_FNDADI',STATUS)
        ENDIF
      ENDIF
      END
