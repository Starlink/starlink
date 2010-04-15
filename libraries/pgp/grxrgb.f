      SUBROUTINE GRXRGB (H,L,S,R,G,B)
C-----------------------------------------------------------------------
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
C
C  2-Jul-1984 - new routine [TJP].
C-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL     H,L,S, R,G,B, MA, MI
C
      IF (L.LE.0.5) THEN
          MA = L*(1.+S)
      ELSE
          MA = L + S - L*S
      END IF
      MI = 2*L-MA
      IF (H.LT.60.0) THEN
          R = MI + (MA-MI)*H/60.0
      ELSE IF (H.LT.180.0) THEN
          R = MA
      ELSE IF (H.LT.240.0) THEN
          R = MI + (MA-MI)*(240.0-H)/60.0
      ELSE
          R = MI
      END IF
      IF (H.LT.120.0) THEN
          G = MI
      ELSE IF (H.LT.180.0) THEN
          G = MI + (MA-MI)*(H-120.0)/60.0
      ELSE IF (H.LT.300.0) THEN
          G = MA
      ELSE
          G = MI + (MA-MI)*(360.0-H)/60.0
      END IF
      IF (H.LT.60.0 .OR. H.GE.300.0) THEN
          B = MA
      ELSE IF (H.LT.120.0) THEN
          B = MI + (MA-MI)*(120.0-H)/60.0
      ELSE IF (H.LT.240.0) THEN
          B = MI
      ELSE
          B = MI + (MA-MI)*(H-240.0)/60.0
      END IF
      END
