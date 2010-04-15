C
      SUBROUTINE FWHMIT(GX,GXVALS,WID,GZVALS,IP,FWHM)
C
C     F W H M I T
C
C     The FWHM of the profile GZVALS, whose peak is at channel IP is
C     returned. The difference in wavelenghts of the two positions
C     where the signal is below half that of the peak is taken as the
C     FWHM ( minus wavelength extent of one channel ).
C
C     Parameters - (">" input, "<" output )
C
C     (>) GX      (Integer) Number of values in arrays GXVALS and GZVALS
C     (>) GXVALS  (Real array) The X values of the data points
C     (>) WID     (Real) Width of X channels
C     (>) GZVALS  (Real array) The Y values of the data points
C     (>) IP      (Integer) The position of the peak in the array
C     (<) FWHM    (Real) The FWHM of the line
C
C                                               JRW / AAO February 1987
C
C     Modified:
C       Original
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER GX,IP
      REAL GXVALS(GX),GZVALS(GX),WID,FWHM
C
C     Local variables
C
      INTEGER I
      REAL RIG,LEF,HH

      HH=0.5*ABS(GZVALS(IP))
C
C     From the line peak move leftwards testing points for value <
C     half of peak height
C
      I=IP-1
20    IF (I.LE.1) THEN
       LEF=GXVALS(1)
       GO TO 50
      END IF
      IF (ABS(GZVALS(I)).LT.HH) THEN
       LEF=GXVALS(I)
       GO TO 50
      END IF
      I=I-1
      GO TO 20
C
C     Now from the line peak move rightwards testing points for
C     value < half of peak height
C
50    I=IP+1
52    IF (I.GE.GX) THEN
       RIG=GXVALS(GX)
       GO TO 80
      END IF
      IF (ABS(GZVALS(I)).LT.HH) THEN
       RIG=GXVALS(I)
       GO TO 80
      END IF
      I=I+1
      GO TO 52

80    FWHM=RIG-LEF-WID
      IF (FWHM.LT.1.3*WID) THEN
       FWHM=1.3*WID
      END IF

99    END
