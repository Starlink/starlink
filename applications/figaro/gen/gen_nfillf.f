C+
      SUBROUTINE GEN_NFILLF(NV,ARRAY)
C
C     G E N _ N F I L L F
C
C     Fills up the first NV elements of ARRAY with the numbers
C     1 through NV.
C
C     Parameters     (">" input, "<" output)
C
C     (>) NV      (Integer) Number of elements to be filled.
C     (<) ARRAY   (Real array ARRAY(NV)) Array to be filled.
C
C                                         KS / CIT 2nd Jan 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NV
      REAL ARRAY(NV)
C
C     Local variables
C
      INTEGER I
C
      DO I=1,NV
         ARRAY(I)=I
      END DO
C
      END
