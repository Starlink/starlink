*+  AXIS_RNGIDX - Returns output pixel for irregularly binned axis
      SUBROUTINE AXIS_RNGIDX( NRAD, RANGE, DEC, IN, INDEX, VALID )
*    Description :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     21 Feb 91 : Original
*
*    Type Definitions :
*
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER                   NRAD             ! # range pairs in RANGE
      REAL                      RANGE(*)         ! Irregular bins
      LOGICAL                   DEC              ! Axis decreasing?
      REAL                      IN               ! Event value
*
*    Export :
*
      LOGICAL                   VALID            ! Is output pixel valid
      INTEGER                   INDEX            ! Index
*
*    Local variables :
*
      INTEGER                   J                ! Current search position
      INTEGER                   INC              ! Increment for J
*-

      INDEX = 0
      VALID = .FALSE.
      IF ( .NOT. DEC ) THEN
        IF ( RANGE(1) .LE. IN .AND. IN .LT. RANGE(NRAD*2) ) THEN

*        Start J off at the RANGE index which is half way through array
          J     = 1 + 2* ( NRAD/2 - 1 )
          INC   = J

*        Binary search
          DO WHILE ( INDEX .EQ. 0 )
            IF ( IN .LT. RANGE(J) ) THEN
              J = J - INC
              IF ( INC .GT. 2 ) INC = INC / 2
            ELSE IF (IN .GE. RANGE(J+1)) THEN
              J = J + INC
              IF ( INC .GT. 2 ) INC = INC / 2
            ELSE
              INDEX = 1 + ((J - 1) / 2)
              VALID = .TRUE.
            END IF
          END DO

        END IF

      ELSE
        IF ( RANGE(1) .GE. IN .AND. IN .GT. RANGE(NRAD*2) ) THEN

*        Start J off at the RANGE index which is half way through array
          J     = 1 + 2* ( NRAD/2 - 1 )
          INC   = J

*        Binary search
          DO WHILE ( INDEX .EQ. 0 )
            IF ( IN .GT. RANGE(J) ) THEN
              J = J - INC
              IF ( INC .GT. 2 ) INC = INC / 2
            ELSE IF ( IN .LE. RANGE(J+1) ) THEN
              J = J + INC
              IF ( INC .GT. 2 ) INC = INC / 2
            ELSE
              INDEX = 1 + ((J - 1) / 2)
              VALID = .TRUE.
            END IF
          END DO

        END IF
      END IF

      END
