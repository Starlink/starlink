C+
      SUBROUTINE CHUNKWRITE(ARRAY,NELM,I,DATA,ND,K,N,ISTAT)
C
C     Write a set of contiguous elements from array DATA into
C     (data mapped) AARAY.
C     NELM = total length (dimension) of ARRAY, and first element
C     of ARRAY written into is the I'th.
C     ND = dimension of input DATA array, N is the number of elements
C     to be transferred, and the first element of DATA transferred is
C     the K'th (ie K-1 elements at the beginning of DATA are ignored).
C     ISTAT = 0 indicates a successful transfer.
C+
      DIMENSION ARRAY(NELM),DATA(ND)
      INTEGER ISTAT
      MAX=I+N-1
      ISTAT=0
      IF(I.LT.1.OR.MAX.GT.NELM)THEN
        ISTAT=1
        RETURN
      END IF
      MAXD=K+N-1
      IF(K.LT.1 .OR. MAXD.GT.ND)THEN
        ISTAT=2
        RETURN
      END IF
      DO J=1,N
        ARRAY(I+J-1)=DATA(J+K-1)
      END DO
      RETURN
      END
