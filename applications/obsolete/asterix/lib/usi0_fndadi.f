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
*-

*  Check inherited global status
      IF (STATUS.NE.SAI__OK) RETURN

      ID = ADI__NULLID

      I=1
      FOUND=.FALSE.

*  Scan list to find first empty slot
      DO WHILE ( (ID.EQ.ADI__NULLID .AND. (I.LE.USI__NMAX))

*    Slot in use?
        IF ( DS(I).USED ) THEN

*      Parameter matches?
          IF ( PAR .EQ. DS(I).PAR ) THEN
            ID = DS(I).ADI_ID
          ELSE
            I = I + 1
          END IF

        END IF

      END DO

      END
