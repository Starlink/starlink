C+
      SUBROUTINE GEN_SORTF (ARRAY,N)
C
C     G E N _ S O R T F
C
C     Sorts a real array. Note: a more efficient Macro version
C     will eventually replace this routine.
C
C     Parameters -   (">" input, "!" modified)
C
C     (!) ARRAY    (Real array ARRAY(N)) Passed as the data to be
C                  sorted, returned as the sorted data.
C     (>) N        (Integer) The number of elements of ARRAY.
C
C                                         KS / CIT 22nd Feb 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER N
      REAL ARRAY(N)
C
C     Local variables
C
      INTEGER I,J
      REAL TEMP
C
      DO I=1,N-1
         DO J=I+1,N
            IF (ARRAY(I).GT.ARRAY(J)) THEN
               TEMP=ARRAY(J)
               ARRAY(J)=ARRAY(I)
               ARRAY(I)=TEMP
            END IF
         END DO
      END DO
C
      END
