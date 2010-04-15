C
      SUBROUTINE MAPSPECG(NX,GAUFT,GAUFTS,STATUS)
C
C     M A P S P E C G
C
C     Maps the Z data for the continuum and gaussian fit into virtual memory
C
C     Parameters -  (">" input, "<" output )
C
C     (>) NX      (Integer) Number of values in XVAL
C     (>) GAUFT   (Real array) Z values of whole input spectrum
C     (<) GAUFTS  (Real array) Z values of mapped data
C     (<) STATUS  (Integer) Not equal to 0 if an error occurs in unmapping
C
C                                           JRW / AAO February 1987
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,STATUS
      REAL GAUFT(NX),GAUFTS(NX)
C
C     Local parameters
C
      INTEGER I

      DO I=1,NX,1
        GAUFTS(I)=GAUFT(I)
      END DO

      STATUS=0
99    END
