C
      SUBROUTINE GAUF3(GX,GXVALS,GZVALS,GAUSUM,GPOS,GPK)
C
C     G A U F 3
C
C     Determines the positon of the peak of the next profile
C     to fit by subtracting the previous sum of Gaussians
C     ( GAUSUM ). The position of the peak ( GPOS ) and height
C     ( GPK ) above continuum are returned
C
C     Parameters -  (">" input, "<" output )
C
C     (>) GX      (Integer) Number of values in array GXVALS
C     (>) GXVALS  (Real array) X values of the line profile
C     (>) GZVALS  (Real array) Y values of the line profile
C     (>) GAUSUM  (Real array) The Y values of the Gaussian+continuum fit
C     (<) GPOS    (Real) The position of the new peak
C     (<) GPK     (Real) The height of the new peak
C
C                                             JRW / AAO  February 1987
C
C     Modified:
C       Original
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER GX
      REAL GXVALS(GX),GZVALS(GX),GAUSUM(GX),GPOS,GPK
C
C     Local variables
C
      INTEGER I,NI
      REAL DIF(4096),MFF

      DO I=1,GX
        DIF(I)=GZVALS(I)-GAUSUM(I)
      END DO

      MFF=-1.E36

      DO I=1,GX
        IF (ABS(DIF(I)).GT.MFF) THEN
          MFF=ABS(DIF(I))
          NI=I
        END IF
      END DO

      GPOS=GXVALS(NI)
      GPK=DIF(NI)

99    END
