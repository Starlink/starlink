C*GRXRGB -- convert HLS color to RGB color
C+
      SUBROUTINE GRXRGB (H,L,S,R,G,B)
C
C GRPCKG: Convert a color specified in the HLS color model to one in
C the RGB model.  This is a support routine: no graphics I/O occurs.
C The inverse transformation is accomplished with routine GRXHLS.
C Reference: SIGGRAPH Status Report of the Graphic Standards Planning
C Committee, Computer Graphics, Vol.13, No.3, Association for
C Computing Machinery, New York, NY, 1979.
C
C Arguments:
C
C H,L,S (real, input): hue (0 to 360), lightness (0 to 1.0), and
C       saturation (0 to 1.0).
C R,G,B (real, output): red, green, blue color coordinates, each in the
C       range 0.0 to 1.0.
C--
C  2-Jul-1984 - new routine [TJP].
C 29-Sep-1994 - take H module 360 [TJP].
C 26-Nov-1996 - force results to be in range (avoid rounding error
C               problems on some machines) [TJP].
C-----------------------------------------------------------------------
      REAL     H,L,S, R,G,B, MA, MI, HM
C
      HM = MOD(H, 360.0)
      IF (HM.LT.0.0) HM = HM+360.0
      IF (L.LE.0.5) THEN
          MA = L*(1.0+S)
      ELSE
          MA = L + S - L*S
      END IF
      MI = 2.0*L-MA
C
C R component
C
      IF (HM.LT.60.0) THEN
          R = MI + (MA-MI)*HM/60.0
      ELSE IF (HM.LT.180.0) THEN
          R = MA
      ELSE IF (HM.LT.240.0) THEN
          R = MI + (MA-MI)*(240.0-HM)/60.0
      ELSE
          R = MI
      END IF
C
C G component
C
      IF (HM.LT.120.0) THEN
          G = MI
      ELSE IF (HM.LT.180.0) THEN
          G = MI + (MA-MI)*(HM-120.0)/60.0
      ELSE IF (HM.LT.300.0) THEN
          G = MA
      ELSE
          G = MI + (MA-MI)*(360.0-HM)/60.0
      END IF
C
C B component
C
      IF (HM.LT.60.0 .OR. HM.GE.300.0) THEN
          B = MA
      ELSE IF (HM.LT.120.0) THEN
          B = MI + (MA-MI)*(120.0-HM)/60.0
      ELSE IF (HM.LT.240.0) THEN
          B = MI
      ELSE
          B = MI + (MA-MI)*(HM-240.0)/60.0
      END IF
C
      R = MIN(1.0, MAX(0.0,R))
      G = MIN(1.0, MAX(0.0,G))
      B = MIN(1.0, MAX(0.0,B))
C
      END
