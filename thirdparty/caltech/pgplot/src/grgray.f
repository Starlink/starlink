C*GRGRAY -- gray-scale map of a 2D data array
C+
      SUBROUTINE GRGRAY (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   FG, BG, PA, MININD, MAXIND, MODE)
      INTEGER IDIM, JDIM, I1, I2, J1, J2, MININD, MAXIND, MODE
      REAL    A(IDIM,JDIM)
      REAL    FG, BG
      REAL    PA(6)
C
C This is a device-dependent support routine for PGGRAY.
C
C Draw gray-scale map of an array in current window. Array
C values between FG and BG are shaded in gray levels determined
C by linear interpolation. FG may be either less than or greater
C than BG.  Array values outside the range FG to BG are
C shaded black or white as appropriate.
C
C GRGRAY uses GRIMG0 on devices with enough color indices available.
C Note that it changes the color table to gray-scale.
C Otherwise in does a random dither with GRIMG3.
C
C Arguments:
C  A      (input)  : the array to be plotted.
C  IDIM   (input)  : the first dimension of array A.
C  JDIM   (input)  : the second dimension of array A.
C  I1, I2 (input)  : the inclusive range of the first index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the second
C                    index (J) to be plotted.
C  FG     (input)  : the array value which is to appear in
C                    foreground color.
C  BG     (input)  : the array value which is to appear in
C                    background color.
C  PA     (input)  : transformation matrix between array grid and
C                    device coordinates (see GRCONT).
C  MODE   (input)  : transfer function.
C--
C 12-Dec-1986 - Speed up plotting [J. Biretta].
C  3-Apr-1987 - Add special code for /PS, /VPS, /GR.
C  2-Sep-1987 - Adapted from PGGRAY [TJP].
C  1-Dec-1988 - Put random-number generator inline [TJP].
C  3-Apr-1989 - Use "line of pixels" primitive where available [TJP].
C  6-Sep-1989 - Changes for standard Fortran-77 [TJP].
C 19-Jan-1990 - Add special code for /CPS, /VCPS [DLM]
C  3-Sep-1992 - Add special code for NULL device [TJP].
C 25-Nov-1992 - Add special code for /NEXT [AFT].
C 17-Mar-1994 - Scale in device coordinates [TJP].
C 31-Aug-1994 - use GRIMG0 when appropriate [TJP].
C  7-Sep-1994 - speed up random dither [TJP].
C  8-Feb-1995 - use color ramp based on color indices 0 and 1 [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER I
      REAL    A0, A1, CR0, CG0, CB0, CR1, CG1, CB1
      INTRINSIC REAL
C-----------------------------------------------------------------------
C
C N.B. Arguments are assumed to be valid (checked by PGGRAY).
C
C Use GRIMG0 if this an appropriate device; first initialize the
C color table to a linear ramp between the colors assigned to color
C indices 0 and 1.
C
      IF (GRGCAP(GRCIDE)(7:7).NE.'N' .AND. MAXIND-MININD .GT. 15) THEN
         CALL GRQCR(0, CR0, CG0, CB0)
         CALL GRQCR(1, CR1, CG1, CB1)
         DO 5 I=MININD,MAXIND
            A0 = REAL(I-MININD)/REAL(MAXIND-MININD)
            A1 = 1.0 - A0
            CALL GRSCR(I, A0*CR0+A1*CR1, A0*CG0+A1*CG1, A0*CB0+A1*CB1)
 5       CONTINUE
         CALL GRIMG0(A, IDIM, JDIM, I1, I2, J1, J2,
     :               FG, BG, PA, MININD, MAXIND, MODE)
         RETURN
C
C Otherwise use random dither in current color index.
C
      ELSE
         CALL GRIMG3(A, IDIM, JDIM, I1, I2, J1, J2,
     :               FG, BG, PA, MODE)
      END IF
C-----------------------------------------------------------------------
      END
