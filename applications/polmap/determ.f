      DOUBLE PRECISION FUNCTION DETERM(ARRAY,NORDER)
C+
C
C Double precision function:
C
C    D E T E R M
C
C
C Author: Bevington
C
C Parameters:
C
C ARRAY (<), NORDER (<)
C
C History:
C
C   Unknown
C
C
C
C
C
C
C
C-

C
C     DECLARE EVERYTHING TO BE REAL*8
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ARRAY(20,20)
      DETERM  =  1.0D0
      DO K  =  1, NORDER
C
C     INTERCHANGE COLUMNS IF DIAGONAL ELEMENT IS ZERO
C
        IF (ARRAY(K,K).NE.0) GOTO 100
        DO J  =  K, NORDER
          IF (ARRAY(K,J).NE.0) GOTO 50
        ENDDO
        DETERM  =  0.0D0
        GOTO 200
   50   CONTINUE
        DO I  =  K, NORDER
          SAVE  =  ARRAY(I,J)
          ARRAY(I,J)  =  ARRAY(I,K)
          ARRAY(I,K)  =  SAVE
        ENDDO
        DETERM  =  -DETERM
C
C     SUBTRACT ROW K FROM THE LOWER ROWS TO GET A DIAGONAL MATRIX
C
  100   CONTINUE
        DETERM  =  DETERM*ARRAY(K,K)
        IF (K.LT.NORDER) THEN
          K1  =  K + 1
          DO I  =  K1, NORDER
            DO J  =  K1, NORDER
              ARRAY(I,J)  =  ARRAY(I,J) -
     &                       ARRAY(I,K)*ARRAY(K,J)/ARRAY(K,K)
            ENDDO
          ENDDO
        ENDIF
      ENDDO
  200 CONTINUE
      RETURN
      END
