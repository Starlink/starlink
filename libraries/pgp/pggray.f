C*PGGRAY -- gray-scale map of a 2D data array
C%void cpggray(const float *a, int idim, int jdim, int i1, int i2, \
C% int j1, int j2, float fg, float bg, const float *tr);
C+
      SUBROUTINE PGGRAY (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   FG, BG, TR)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      REAL    A(IDIM,JDIM), FG, BG, TR(6)
C
C Draw gray-scale map of an array in current window. The subsection
C of the array A defined by indices (I1:I2, J1:J2) is mapped onto
C the view surface world-coordinate system by the transformation
C matrix TR. The resulting quadrilateral region is clipped at the edge
C of the window and shaded with the shade at each point determined
C by the corresponding array value.  The shade is a number in the
C range 0 to 1 obtained by linear interpolation between the background
C level (BG) and the foreground level (FG), i.e.,
C
C   shade = [A(i,j) - BG] / [FG - BG]
C
C The background level BG can be either less than or greater than the
C foreground level FG.  Points in the array that are outside the range
C BG to FG are assigned shade 0 or 1 as appropriate.
C
C PGGRAY uses two different algorithms, depending how many color
C indices are available in the color index range specified for images.
C (This range is set with routine PGSCIR, and the current or default
C range can be queried by calling routine PGQCIR).
C
C If 16 or more color indices are available, PGGRAY first assigns
C color representations to these color indices to give a linear ramp
C between the background color (color index 0) and the foreground color
C (color index 1), and then calls PGIMAG to draw the image using these
C color indices. In this mode, the shaded region is "opaque": every
C pixel is assigned a color.
C
C If less than 16 color indices are available, PGGRAY uses only
C color index 1, and uses  a "dithering" algorithm to fill in pixels,
C with the shade (computed as above) determining the faction of pixels
C that are filled. In this mode the shaded region is "transparent" and
C allows previously-drawn graphics to show through.
C
C The transformation matrix TR is used to calculate the world
C coordinates of the center of the "cell" that represents each
C array element. The world coordinates of the center of the cell
C corresponding to array element A(I,J) are given by:
C
C          X = TR(1) + TR(2)*I + TR(3)*J
C          Y = TR(4) + TR(5)*I + TR(6)*J
C
C Usually TR(3) and TR(5) are zero -- unless the coordinate
C transformation involves a rotation or shear.  The corners of the
C quadrilateral region that is shaded by PGGRAY are given by
C applying this transformation to (I1-0.5,J1-0.5), (I2+0.5, J2+0.5).
C
C Arguments:
C  A      (input)  : the array to be plotted.
C  IDIM   (input)  : the first dimension of array A.
C  JDIM   (input)  : the second dimension of array A.
C  I1, I2 (input)  : the inclusive range of the first index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the second
C                    index (J) to be plotted.
C  FG     (input)  : the array value which is to appear with the
C                    foreground color (corresponding to color index 1).
C  BG     (input)  : the array value which is to appear with the
C                    background color (corresponding to color index 0).
C  TR     (input)  : transformation matrix between array grid and
C                    world coordinates.
C--
C  2-Sep-1987: remove device-dependent code to routine GRGRAY (TJP).
C  7-Jun-1988: change documentation and argument names (TJP).
C 31-May-1989: allow 1-pixel wide arrays to be plotted (TJP).
C 17-Mar-1994: pass PG scaling info to lower routines (TJP).
C 15-Sep-1994: use PGITF attribute (TJP).
C  8-Feb-1995: use color ramp based on current foreground and background
C              colors (TJP).
C  6-May-1996: allow multiple devives (TJP).
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      REAL PA(6)
      LOGICAL PGNOTO
C
C Check inputs.
C
      IF (PGNOTO('PGGRAY')) RETURN
      IF (I1.LT.1 .OR. I2.GT.IDIM .OR. I1.GT.I2 .OR.
     1    J1.LT.1 .OR. J2.GT.JDIM .OR. J1.GT.J2) THEN
          CALL GRWARN('PGGRAY: invalid range I1:I2, J1:J2')
      ELSE IF (FG.EQ.BG) THEN
          CALL GRWARN('PGGRAY: foreground level = background level')
      ELSE
C
C Call lower-level routine to do the work.
C
          CALL PGBBUF
          CALL PGSAVE
          CALL PGSCI(1)
          PA(1) = TR(1)*PGXSCL(PGID) + PGXORG(PGID)
          PA(2) = TR(2)*PGXSCL(PGID)
          PA(3) = TR(3)*PGXSCL(PGID)
          PA(4) = TR(4)*PGYSCL(PGID) + PGYORG(PGID)
          PA(5) = TR(5)*PGYSCL(PGID)
          PA(6) = TR(6)*PGYSCL(PGID)
          CALL GRGRAY(A, IDIM, JDIM, I1, I2, J1, J2, FG, BG, PA,
     :                PGMNCI(PGID), PGMXCI(PGID), PGITF(PGID))
          CALL PGEBUF
          CALL PGUNSA
      END IF
C-----------------------------------------------------------------------
      END

