C*GRLIN3 -- draw a thick line (multiple strokes)
C+
      SUBROUTINE GRLIN3 (X0,Y0,X1,Y1)
C
C GRPCKG: draw a heavy line from (X0,Y0) to (X1,Y1) by making multiple
C strokes.  In order to simulate a thick pen, the line drawn has
C circular, rather than square, end points.  If this is not done,
C thick letters and other figures have an abnormal and unpleasant
C appearance.
C
C Vocabulary:
C
C LINEWT: the number of strokes required to draw the line; if
C       this is odd, one stroke will lie along the requested vector.
C       The nominal line thickness is (LINEWT-1)*0.005 in.
C RSQURD: the square of the semi-line thickness.
C (DX,DY): the vector length of the line.
C (VX,VY): a vector of length 1 pixel in the direction of the line.
C (VY,-VX): a vector of length 1 pixel perpendicular to (VX,VY).
C OFF: the offset parallel to (VY,-VX) of the K'th stroke.
C (VXK,VYK): the vector increment of the K'th stroke to allow for the
C       semi-circular terminal on the line.
C (PXK,PYK): the vector offset of the K'th stroke perpendicular to the
C       line vector.
C--
C (1-Feb-1983)
C 23-Nov-1994 - change algorithm so that the unit of line-width is
C               0.005 inch instead of 1 pixel [TJP].
C March 1995 - added ABS to prevent domain error in SQRT (CTD)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  K,LINEWT
      REAL     DX,DY, HK, OFF, PXK,PYK, RSQURD, VLEN,VX,VY,VXK,VYK
      REAL     X0,X1,Y0,Y1
      REAL     XS0,XS1, YS0,YS1, SPIX,SPIY
      LOGICAL  VIS
C
C Determine number of strokes and line thickness.
C
      LINEWT = GRWIDT(GRCIDE)
      RSQURD = ((LINEWT-1)**2)*0.25
C
C Determine the vectors (VX,VY), (VY,-VX). If the line-length is zero,
C pretend it is a very short horizontal line.
C
      DX = X1 - X0
      DY = Y1 - Y0
      VLEN = SQRT(DX**2 + DY**2)
      SPIX = GRPXPI(GRCIDE)*0.005
      SPIY = GRPYPI(GRCIDE)*0.005
C
      IF (VLEN .EQ. 0.0) THEN
          VX = SPIX
          VY = 0.0
      ELSE
          VX = DX/VLEN*SPIX
          VY = DY/VLEN*SPIY
      END IF
C
C Draw LINEWT strokes. We have to clip again in case thickening the
C line has taken us outside the window.
C
      OFF = (LINEWT-1)*0.5
      DO 10 K=1,LINEWT
          PXK = VY*OFF
          PYK = -(VX*OFF)
          HK  = SQRT(ABS(RSQURD - OFF**2))
          VXK = VX*HK
          VYK = VY*HK
          XS1 = X1+PXK+VXK
          YS1 = Y1+PYK+VYK
          XS0 = X0+PXK-VXK
          YS0 = Y0+PYK-VYK
          CALL GRCLPL(XS1,YS1,XS0,YS0,VIS)
          IF (VIS) CALL GRLIN2(XS1, YS1, XS0, YS0)
          OFF = OFF - 1.0
   10 CONTINUE
      END
