      SUBROUTINE GRXHLS (R,G,B,H,L,S)
C-----------------------------------------------------------------------
C GRPCKG: Convert a color specified in the RGB color model to one in
C the HLS model.  This is a support routine: no graphics I/O occurs.
C The inverse transformation is accomplished with routine GRXRGB.
C Reference: SIGGRAPH Status Report of the Graphic Standards Planning
C Committee, Computer Graphics, Vol.13, No.3, Association for
C Computing Machinery, New York, NY, 1979.
C
C Arguments:
C
C R,G,B (real, input): red, green, blue color coordinates, each in the
C       range 0.0 to 1.0. Input outside this range causes HLS = (0,1,0)
C       [white] to be returned.
C H,L,S (real, output): hue (0 to 360), lightness (0 to 1.0), and
C       saturation (0 to 1.0).
C
C  2-Jul-1984 - new routine [TJP].
C-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL     R,G,B, H,L,S, MA, MI, RR, GG, BB, D
C
      H = 0.0
      L = 1.0
      S = 0.0
      MA = MAX(R,G,B)
      MI = MIN(R,G,B)
      IF (MA.GT.1.0 .OR. MI.LT.0.0) RETURN
      RR = (MA-R)
      GG = (MA-G)
      BB = (MA-B)
      L = 0.5*(MA+MI)
      IF (MA.EQ.MI) THEN
         S = 0.0
         H = 0.0
      ELSE
         D = MA-MI
         IF (L.LE.0.5) THEN
            S = D/(MA+MI)
         ELSE
            S = D/(2.0-MA-MI)
         END IF
         IF (R.EQ.MA) THEN
         H = (2.0*D+BB-GG)
         ELSE IF (G.EQ.MA) THEN
            H = (4.0*D+RR-BB)
*  (B.EQ.MA)
         ELSE
            H = (6.0*D+GG-RR)
         END IF
         H = H*60.0/D
      END IF
      END
