C+
      SUBROUTINE CHUNK(ARRAY,NELM,RESULT,I,N,ISTAT)
C
C     Extract N contiguous elements from a real (data mapped) ARRAY, of
C     total length NELM.  First element extracted is I (I=1 gets first
C     element of input array); output to array RESULT, length N.
C     ISTAT = 0 indicates successful transfer.
C+
      DIMENSION ARRAY(NELM),RESULT(N)
      INTEGER ISTAT
      MAX=I+N-1
      ISTAT=0
      IF(I.LT.1.OR.MAX.GT.NELM)THEN
        ISTAT=1
        RETURN
      END IF
      DO J=1,N
      L=I+J-1
      RESULT(J)=ARRAY(L)
      END DO
      RETURN
      END
