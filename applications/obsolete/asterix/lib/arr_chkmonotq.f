*+  ARR_CHKMONOTQ - Checks whether array has monotonically changing values
      SUBROUTINE ARR_CHKMONOTQ( N, X, Q, MASK, MONOT, STATUS )
*
*    Description :
*
*      Scans an array with quality to see if all the values are increasing or
*      decreasing monotonically.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     30 Jul 93 : Original (DJA)
*      1 Mar 94 : Quality handling updated (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Status definition :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER N
      REAL X(*)
      BYTE Q(*)
      BYTE MASK
*
*    Export :
*
      LOGICAL 			MONOT			! Whether monotonic
*
*    Functions :
*
      BYTE			BIT_ANDUB
*
*    Local variables :
*
      INTEGER I,FSIGN,J
*-
      IF (STATUS.EQ.SAI__OK) THEN
        MONOT = .TRUE.
        IF (N.GT.2) THEN

*        Find first good point
          J = 1
          DO WHILE ( (J.LE.N) .AND.
     :               (BIT_ANDUB(Q(J),MASK).NE.QUAL__GOOD) )
            J = J + 1
          END DO

*        At least one good point!
          IF ( J .LE. N ) THEN

*          Find the next one
            I = J
            DO WHILE ( (I.LE.N) .AND.
     :                 (BIT_ANDUB(Q(I),MASK).NE.QUAL__GOOD) )
              I = I + 1
            END DO

*          At least two good points!
            IF ( I .LE. N ) THEN

*            Direction of increase of 1st two good points
              FSIGN = SIGN(1.0,X(I)-X(J))

*            Look at rest of points. J stores index of last good point
              I = J + 1
              DO WHILE ( (I.LE.N) .AND. MONOT )
                IF ( BIT_ANDUB(Q(I),MASK) .EQ. QUAL__GOOD ) THEN
                  MONOT = (SIGN(1.0,X(I)-X(J)).EQ.FSIGN)
                  J = I
                END IF
                I = I + 1
              END DO

            END IF

          END IF

        END IF
      END IF

      END
