*+  ARR_CHKREG - Checks whether array is regularly spaced
      SUBROUTINE ARR_CHKREG(X,N,REG,BASE,SCALE,STATUS)
*    Description :
*      Checks whether an array is regularly spaced and if so
*      returns the appropriate values for storing it as a
*      spaced array
*    Authors :
*           (BHVAD::RJV)
*           (BHVAD::DJA)
*
*    History :
*
*      18 Mar 88 : Original
*      28 Nov 89 : Now compares delta_x / spacing, rather than comparing
*                  delta_x absolutely. (DJA)
*       7 Feb 92 : compares the least significant of 6 sig.figs. and allows
*                  a tolerance of 1 (RJV)
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status definition :
      INTEGER STATUS
*    Import :
      INTEGER N
      REAL X(N)
*    Import/Export :
*    Export :
      LOGICAL REG		! whether regularly spaced
      REAL BASE			! base point if regular
      REAL SCALE		! increment  "    "
*    Local variables :
      REAL DX
      REAL FACT
      INTEGER I,J
*-
      IF (STATUS.EQ.SAI__OK) THEN
        IF (N.GT.1) THEN
          SCALE=(X(N)-X(1))/REAL(N-1)
          IF ( SCALE .EQ. 0.0 ) THEN
             REG=.FALSE.
          ELSE
             BASE=X(1)
             FACT=10.0**(4-INT(LOG10(ABS(SCALE))))
             I=2
             REG=.TRUE.
          END IF
          DO WHILE (REG.AND.I.LT.N)
            J=I+1
            DX = X(J) - X(I)
            IF ( DX .NE. 0.0 ) THEN
               REG = ( ABS(DX-SCALE)*FACT .LE. 1.0 )
            ELSE
               REG = .FALSE.
            END IF
            I=J
          ENDDO
          IF (.NOT.REG) THEN
            BASE=0.0
            SCALE=0.0
          ENDIF
        ELSE
          REG=.TRUE.
          BASE=X(1)
          SCALE=0.0
        ENDIF
      ENDIF
      END
