      SUBROUTINE SMATIN (ARRAY,ORD,DET)
C
C  Subroutine inverts a symmetric matrix and calculates its determinant.
C
C  Imports:
C     ARRAY == input matrix, replaced by its inverse on exit.
C     ORD == degree of matrix, unchanged on exit.
C
C  Exports:
C     DET   == determinant of input matrix.
C
      IMPLICIT NONE
      INTEGER I,J,K,L,ORD
      INTEGER IK(100), JK(100)
      DOUBLE PRECISION ARRAY(ORD,ORD)
      DOUBLE PRECISION AMAX,DET,SAVE
      DO 3, I=1,100
        IK(I) = 0
        JK(I) = 0
    3 CONTINUE
      SAVE = 0.0D+00
   10 DET =1.0D+00
   11 DO 100, K=1,ORD
C
C  Find largest element ARRAY(I,J) in rest of matrix.
C
        AMAX = 0.0D+00
   21   DO 30, I=K,ORD
          DO 30, J=K,ORD
   23       IF (DABS(AMAX) - DABS(ARRAY(I,J))) 24,24,30
   24       AMAX = ARRAY(I,J)
            IK(K) = I
            JK(K) = J
   30     CONTINUE
C
C  Interchange rows and columns to put AMAX in ARRAY(K,K).
C
   31     IF (AMAX) 41,32,41
   32     DET = 0.0D+00
          GO TO 140
   41     I = IK(K)
          IF (I-K) 21,51,43
   43     DO 50, J=1,ORD
            SAVE = ARRAY(K,J)
            ARRAY(K,J) = ARRAY(I,J)
   50       ARRAY(I,J) = -SAVE
   51       J = JK(K)
            IF (J-K) 21,61,53
   53       DO 60, I=1,ORD
              SAVE = ARRAY(I,K)
              ARRAY(I,K) = ARRAY(I,J)
              ARRAY(I,J) = -SAVE
   60       CONTINUE
C
C  Accumulate elements of inverse matrix.
C
   61       DO 70, I=1,ORD
              IF (I-K) 63,70,63
   63         ARRAY(I,K) = -ARRAY(I,K)/AMAX
   70       CONTINUE
   71       DO 80, I=1,ORD
            DO 80, J=1,ORD
              IF (I-K) 74,80,74
   74         IF (J-K) 75,80,75
   75         ARRAY(I,J) = ARRAY(I,J) + ARRAY(I,K)*ARRAY(K,J)
   80       CONTINUE
   81       DO 90, J=1,ORD
              IF (J-K) 83,90,83
   83         ARRAY(K,J) = ARRAY(K,J)/AMAX
   90       CONTINUE
            ARRAY(K,K) =1.0D+00/AMAX
  100       DET = DET*AMAX
C
C  Restore ordering of matrix.
C
  101       DO 130, L=1,ORD
              K = ORD - L + 1
              J = IK(K)
              IF (J-K) 111,111,105
  105         DO 110, I=1,ORD
                SAVE = ARRAY(I,K)
                ARRAY(I,K) = -ARRAY(I,J)
                ARRAY(I,J) = SAVE
  110         CONTINUE
  111         I =JK(K)
              IF (I-K) 130,130,113
  113         DO 120, J=1,ORD
                SAVE = ARRAY(K,J)
                ARRAY(K,J) = -ARRAY(I,J)
                ARRAY(I,J) = SAVE
  120         CONTINUE
  130       CONTINUE
  140       RETURN
            END
