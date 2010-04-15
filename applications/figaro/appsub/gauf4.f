C
      SUBROUTINE GAUF4(GX,GXVALS,ICST,NX,CX,IGST,CONVALS,GN,GINFP,
     :GINFH,GINFW,GAUFS)
C
C      G A U F 4
C
C      Puts the profile points for each fitting Gaussian
C      on the continuum into successive rows of GAUFS
C
C     Parameters -  (">" input, "<" output )
C
C     (>) GX      (Integer) Number of values in array GXVALS
C     (>) GXVALS  (Real array) X values of the line profile
C     (>) ICST    (Integer) Left edge of fitted continuum in spectrum
C     (>) NX      (Integer) Length of array CONVALS
C     (>) CX      (Integer) Number of values in fitted continuum
C     (>) IGST    (Integer) Left edge of line extent in spectrum
C     (>) CONVALS (Real array) Y values of continuum points
C     (>) GN      (Integer) Total number of Gaussians
C     (>) GINFP   (Real array) The X positions of the Gaussian peaks
C     (>) GINFH   (Real array) The peak heights of the Gaussian peaks
C     (>) GINFW   (Real array) The sigma widths of the Gaussian peaks
C     (<) GAUFS   (Real array) The Y values of each Gaussians+continuum
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
      INTEGER GX,IGST,NX,CX,ICST,GN
      REAL GXVALS(GX),CONVALS(NX),GINFP(GN),GINFH(GN),GINFW(GN),
     :GAUFS(GX,GN)
C
C     Local variables
C
      INTEGER I,J
      REAL TT,YAA,YG(4096)

      DO I=1,GX
        YG(I)=0.0
      END DO

      IF (GN.EQ.0) THEN
        GO TO 99
      END IF
C
C     Copy the sum of each Gaussian on the continuum into GAUFS
C
      DO J=1,GN
        DO I=1,GX
          TT=0.5*(((GXVALS(I)-GINFP(J))/GINFW(J))**2.)
          IF (TT.GT.20.) THEN
            YAA=0.0
            GO TO 5
          END IF
          IF (TT.LT.1.E-20) THEN
            YAA=GINFH(J)
            GO TO 5
          END IF
4         YAA=GINFH(J)*EXP(-1.*(TT))
5         GAUFS(I,J)=YAA+CONVALS(I+IGST-1)
        END DO
      END DO

99    END
