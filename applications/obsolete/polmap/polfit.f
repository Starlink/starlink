      SUBROUTINE POLFIT(X,Y,SIGMAY,NPTS,NTERMS,MODE,A,CHISQR)
C+
C
C Subroutine: 
C
C      P O L F I T
C
C
C Author: Bevington
C
C Parameters: 
C
C X (<), Y (<), SIGMAY (<), NPTS (<), NTERMS (<), MODE (<),A (>),
C CHISQR (>)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C
C
C  Bevingtons polynomial least-sqaures fitting routine
C
C
C
C
C-
C
C     DECLARE EVERYTHING TO BE REAL*8
C
      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'array_size.inc'
C
C     SET UP ARRAYS
C
      DIMENSION X(*), Y(*), SIGMAY(*), A(*)
      DIMENSION ARRAY(20,20), SUMX(MAXPTS), SUMY(MAXPTS)
C
C     ACCUMULATE WEIGHTED SUMS
C
      NMAX  =  2*NTERMS - 1
      DO N  =  1, NMAX
        SUMX(N)  =  0.0D0
      ENDDO
      DO J  =  1, NTERMS
        SUMY(J)  =  0.0D0
      ENDDO
      CHISQ  =  0.0D0
      DO I  =  1, NPTS
        XI  =  X(I)
        YI  =  Y(I)
        IF (MODE.LT.0) THEN
          IF (YI.LT.0) THEN
            WEIGHT  =  -1.0D0/YI
          ELSEIF (YI.EQ.0) THEN
            GOTO 50
          ELSE
            WEIGHT  =  1.0D0/YI
          ENDIF
        ELSEIF (MODE.EQ.0) THEN
          GOTO 50
        ELSE
          WEIGHT  =  1.0D0/SIGMAY(I)**2
        ENDIF
        GOTO 100
   50   CONTINUE
        WEIGHT  =  1.0D0
  100   CONTINUE
        XTERM  =  WEIGHT
        DO N  =  1, NMAX
          SUMX(N)  =  SUMX(N) + XTERM
          XTERM  =  XTERM*XI
        ENDDO
        YTERM  =  WEIGHT*YI
        DO N  =  1, NTERMS
          SUMY(N)  =  SUMY(N) + YTERM
          YTERM  =  YTERM*XI
        ENDDO
        CHISQ  =  CHISQ + WEIGHT*YI**2
      ENDDO
C
C     CONSTRUCT MATRICES AND CALCULATE COEFFICIENTS
C
      DO J  =  1, NTERMS
        DO K  =  1, NTERMS
          N  =  J + K - 1
          ARRAY(J,K)  =  SUMX(N)
        ENDDO
      ENDDO
      DELTA  =  DETERM(ARRAY,NTERMS)
      IF (DELTA.NE.0) THEN
        DO L  =  1, NTERMS
          DO J  =  1, NTERMS
            DO K  =  1, NTERMS
              N  =  J + K - 1
              ARRAY(J,K)  =  SUMX(N)
            ENDDO
            ARRAY(J,L)  =  SUMY(J)
          ENDDO
          A(L)  =  DETERM(ARRAY,NTERMS)/DELTA
        ENDDO
C
C     CALCULATE CHI SQUARE
C
        DO J  =  1, NTERMS
          CHISQ  =  CHISQ - 2.0D0*A(J)*SUMY(J)
          DO K  =  1, NTERMS
            N  =  J + K - 1
            CHISQ  =  CHISQ + A(J)*A(K)*SUMX(N)
          ENDDO
        ENDDO
        FREE  =  DFLOAT(NPTS-NTERMS)
        CHISQR  =  CHISQ/FREE
      ELSE
        CHISQR  =  0.0D0
        DO J  =  1, NTERMS
          A(J)  =  0.0D0
        ENDDO
      ENDIF
      RETURN
      END
