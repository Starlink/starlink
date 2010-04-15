C+
      SUBROUTINE GEN_MULCAF(IN,N,CONST,OUT)
C
C     G E N _ M U L C A F
C
C     Multiplies a real array by a real constant.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) IN     (Real array IN(N)) The input array
C                (Note that IN may be multiply dimensioned
C                in the calling program.  It is treated as
C                1D here for efficiency and generality.)
C     (>) N      (Integer) The number of elements of IN.
C     (>) CONST  (Real) The constant by which all the
C                elements of IN are to be multiplied.
C     (<) OUT    (Real array OUT(N)) The result of the
C                multiplication.  Note that IN and OUT may
C                be the same array.
C
C     Subroutines / functions used - None
C
C                                         KS / CIT 1st March 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER N
      REAL IN(N),CONST,OUT(N)
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
