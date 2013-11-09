*+  ARR_CHKMONOT - Checks whether array has monotonically changing values
      SUBROUTINE ARR_CHKMONOT( N, X, MONOT, STATUS )
*
*    Description :
*
*      Scans an array to see if all the values are increasing or
*      decreasing monotonically.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     30 Jul 93 : Original (DJA)
*     22 Mar 94 : Corrected (RJV)
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status definition :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER N
      REAL X(N)
*
*    Export :
*
      LOGICAL MONOT		! whether monotonic
*
*    Local variables :
*
      INTEGER I,FSIGN
*-
      IF (STATUS.EQ.SAI__OK) THEN
        MONOT = .TRUE.
        IF (N.GT.2) THEN
          FSIGN = SIGN(1.0,X(2)-X(1))
          I = 2
          DO WHILE ( (I.LE.N) .AND. MONOT )
            MONOT = (SIGN(1.0,X(I)-X(I-1)).EQ.FSIGN)
            I = I + 1
          END DO
        END IF
      END IF

      END
