C
      SUBROUTINE MAPSPECE(NX,XVAL,ZVAL,EVAL,IXST,NNX,XVALS,ZVALS,
     :                    ERRORS,STATUS)
C
C     M A P S P E C E
C
C     Unmaps the X Z and error data from virtual memory into arrays
C     XVALS, ZVALS and ERRORS over the range of X values IXST to IXST
C     + NXX - 1
C
C     Parameters -  (">" input, "<" output )
C
C     (>) NX      (Integer) Number of values in XVAL
C     (>) XVAL    (Real array) X values of whole spectrum
C     (>) ZVAL    (Real array) Z values of whole input spectrum
C     (>) EVAL    (Real array) Error values for whole input spectrum
C     (>) IXST    (Integer) Start channel of output X array in XVAL
C     (>) NNX     (Integer) Number of X values in output array
C     (<) XVALS   (Real array) X values of unmapped data
C     (<) ZVALS   (Real array) Z values of unmapped data
C     (<) ERRORS  (Real array) Error values of unmapped data
C     (<) STATUS  (Integer) Not equal to 0 if an error occurs in unmapping
C
C                                           JRW / AAO February 1987
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,IXST,NNX,STATUS
      REAL XVAL(NX),ZVAL(NX),EVAL(NX),XVALS(NNX),ZVALS(NNX),
     :ERRORS(NNX)
C
C     Local parameters
C
      INTEGER I

      IF ((NNX+IXST-1).GT.NX) THEN
        STATUS=1
        GO TO 99
      END IF

      DO I=1,NNX,1
        XVALS(I)=XVAL(I+IXST-1)
        ZVALS(I)=ZVAL(I+IXST-1)
        ERRORS(I)=EVAL(I+IXST-1)
      END DO

      STATUS=0
99    END
