C+
      SUBROUTINE GEN_MULCAD(IN,N,CONST,OUT)
C
C     G E N _ M U L C A D
C
C     Multiplies a double precision array by a constant.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) IN     (Double precision array IN(N)) The input array
C                (Note that IN may be multiply dimensioned
C                in the calling program.  It is treated as
C                1D here for efficiency and generality.)
C     (>) N      (Integer) The number of elements of IN.
C     (>) CONST  (Double precision) The constant by which all the
C                elements of IN are to be multiplied.
C     (<) OUT    (Double precision array OUT(N)) The result of the
C                multiplication.  Note that IN and OUT may
C                be the same array.
C
C     Subroutines / functions used - None
C
C                                         KS / AAO 23rd Sept 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER N
      DOUBLE PRECISION IN(N),CONST,OUT(N)
C
C     Local variable
C
      INTEGER I
C
C     Perform operation
C
      DO I=1,N
         OUT(I)=IN(I)*CONST
      END DO
C
      END
