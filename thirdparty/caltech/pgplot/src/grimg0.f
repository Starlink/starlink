C*GRIMG0 -- color image of a 2D data array
C+
      SUBROUTINE GRIMG0 (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   A1, A2, PA, MININD, MAXIND, MODE)
      INTEGER IDIM, JDIM, I1, I2, J1, J2, MININD, MAXIND, MODE
      REAL    A(IDIM,JDIM), A1, A2, PA(6)
C
C This is a support routine for PGIMAG.
C
C Arguments:
C  A      (input)  : the array to be plotted.
C  IDIM   (input)  : the first dimension of array A.
C  JDIM   (input)  : the second dimension of array A.
C  I1, I2 (input)  : the inclusive range of the first index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the second
C                    index (J) to be plotted.
C  A1     (input)  : the array value which is to appear in color
C                    index MININD.
C  A2     (input)  : the array value which is to appear in color
C                    index MAXIND.
C  PA     (input)  : transformation matrix between array grid and
C                    device coordinates.
C  MININD (input)  : minimum color index to use.
C  MAXIND (input)  : maximum color index to use.
C  MODE   (input)  : =0 for linear, =1 for logarithmic, =2 for
C                    square-root mapping of array values to color
C                    indices.
C--
C  7-Sep-1994 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      CHARACTER C
C-----------------------------------------------------------------------
C
C Switch on type of device support.
C
      C = GRGCAP(GRCIDE)(7:7)
      IF (C.EQ.'Q') THEN
C         -- Image-primitive devices
          CALL GRIMG1(A, IDIM, JDIM, I1, I2, J1, J2, A1, A2, PA,
     :                MININD, MAXIND, MODE)
      ELSE IF (C.EQ.'P') THEN
C         -- Pixel-primitive devices         
          CALL GRIMG2(A, IDIM, JDIM, I1, I2, J1, J2, A1, A2, PA,
     :                MININD, MAXIND, MODE)
      ELSE IF (C.EQ.'N') THEN
C         -- Other devices
          CALL GRWARN(
     :     'images cannot be displayed on the selected device')
      ELSE
C         -- Unknown device code
          CALL GRWARN('unexpected error in routine GRIMG0')
      END IF
C-----------------------------------------------------------------------
      END
