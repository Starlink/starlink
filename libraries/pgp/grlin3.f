      SUBROUTINE GRLIN3 (X0,Y0,X1,Y1)
*+
*   - - - - - - - -
*     G R L I N 3      (GKS emulation of GRPCKG)
*   - - - - - - - -
*
*   Draw a heavy line from (X0,Y0) to (X1,Y1) by making multiple
*   strokes.  In order to simulate a thick pen, the line drawn has
*   circular, rather than square, end points.  If this is not done,
*   thick letters and other figures have an abnormal and unpleasant
*   appearance.
*
*   Vocabulary:
*
*   LINEWT:    the number of strokes required to draw the line; if
*              this is odd, one stroke will lie along the requested vector.
*              The nominal line thickness is (LINEWT-1) pixels.
*   RSQURD:    the square of the semi-line thickness.
*   (DX,DY):   the vector length of the line.
*   (VX,VY):   a vector of length 1 pixel in the direction of the line.
*   (VY,-VX):  a vector of length 1 pixel perpendicular to (VX,VY).
*   OFF:       the offset parallel to (VY,-VX) of the K'th stroke.
*   (VXK,VYK): the vector increment of the K'th stroke to allow for the
*              semi-circular terminal on the line.
*   (PXK,PYK): the vector offset of the K'th stroke perpendicular to the
*              line vector.
*   PIXRAT:    Aspect ratio of device pixels.
*   (XSP,YSP): Size in x and y of a "nominal" pixel; on devices with
*              non-square pixels and nominal pixel is a square with
*              sides equal to the shorter side of real pixels
*
*   Given
*      X0       r       Start position x coordinate
*      Y0       r       Start position y coordinate
*      X1       r       Start position x coordinate
*      Y1       r       Start position y coordinate
*
*   Read from COMMON
*      GRCIDE   i       Current device id
*      GRWIDT   i()     Line width
*      GRXPIN   r()     Device resolution
*      GRYPIN   r()        "       "
*
*   D.L.Terrett  Starlink  Aug 1987  (After T.J.Pearson)
*+
      IMPLICIT NONE
      INCLUDE 'grecom.inc'

      REAL X0, Y0, X1, Y1

      INTEGER K, LINEWT
      REAL XS0, XS1, YS0, YS1
      REAL DX, DY, HK, OFF, PXK, PYK, RSQURD, VLEN, VX, VY, VXK, VYK
      REAL XSP, YSP, PIXRAT, STEP

*   Find the size of "nominal" pixels
      PIXRAT = GRXPIN(GRCIDE)/GRYPIN(GRCIDE)
      IF (PIXRAT.EQ.1.0) THEN
         XSP = 1.0
         YSP = 1.0
      ELSE IF (PIXRAT.GT.1.0) THEN
         XSP = 1.0/PIXRAT
         YSP = 1.0
      ELSE
         XSP = 1.0
         YSP = PIXRAT
      END IF

*   Determine number of strokes and line thickness.
      LINEWT = GRWIDT(GRCIDE)
      STEP = MIN(XSP,YSP)
      RSQURD = ((REAL(LINEWT-1)*STEP)**2)*0.25

*   Determine the vectors (VX,VY), (VY,-VX). If the line-length is zero,
*   pretend it is a very short horizontal line.
      DX = X1 - X0
      DY = Y1 - Y0
      VLEN = SQRT(DX**2 + DY**2)

      IF (VLEN .EQ. 0.0) THEN
          VX = XSP
          VY = 0.0
      ELSE
          VX = DX/VLEN*XSP
          VY = DY/VLEN*YSP
      END IF

*   Draw LINEWT strokes separated by the size of a nominal pixel
      DO 10 K=1,LINEWT
         OFF = (REAL(LINEWT) * 0.5 - REAL(K) + 0.5) * STEP
         PXK = VY*OFF*PIXRAT
         PYK = -VX*OFF/PIXRAT
         HK  = SQRT(ABS(RSQURD - OFF**2))
         VXK = VX*HK
         VYK = VY*HK
         XS1 = X1+PXK+VXK
         YS1 = Y1+PYK+VYK
         XS0 = X0+PXK-VXK
         YS0 = Y0+PYK-VYK
         CALL GRLIN2(XS1, YS1, XS0, YS0)
   10 CONTINUE

      END
