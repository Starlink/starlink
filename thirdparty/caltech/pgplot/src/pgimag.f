C*PGIMAG -- color image from a 2D data array
C%void cpgimag(const float *a, int idim, int jdim, int i1, int i2, \
C% int j1, int j2, float a1, float a2, const float *tr);
C+
      SUBROUTINE PGIMAG (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   A1, A2, TR)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      REAL    A(IDIM,JDIM), A1, A2, TR(6)
C
C Draw a color image of an array in current window. The subsection
C of the array A defined by indices (I1:I2, J1:J2) is mapped onto
C the view surface world-coordinate system by the transformation
C matrix TR. The resulting quadrilateral region is clipped at the edge
C of the window. Each element of the array is represented in the image
C by a small quadrilateral, which is filled with a color specified by
C the corresponding array value.
C
C The subroutine uses color indices in the range C1 to C2, which can
C be specified by calling PGSCIR before PGIMAG. The default values
C for C1 and C2 are device-dependent; these values can be determined by
C calling PGQCIR. Note that color representations should be assigned to
C color indices C1 to C2 by calling PGSCR before calling PGIMAG. On some
C devices (but not all), the color representation can be changed after
C the call to PGIMAG by calling PGSCR again.
C
C Array values in the range A1 to A2 are mapped on to the range of
C color indices C1 to C2, with array values <= A1 being given color
C index C1 and values >= A2 being given color index C2. The mapping
C function for intermediate array values can be specified by
C calling routine PGSITF before PGIMAG; the default is linear.
C
C On devices which have no available color indices (C1 > C2),
C PGIMAG will return without doing anything. On devices with only
C one color index (C1=C2), all array values map to the same color
C which is rather uninteresting. An image is always "opaque",
C i.e., it obscures all graphical elements previously drawn in
C the region.
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
C quadrilateral region that is shaded by PGIMAG are given by
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
C  A1     (input)  : the array value which is to appear with shade C1.
C  A2     (input)  : the array value which is to appear with shade C2.
C  TR     (input)  : transformation matrix between array grid and
C                    world coordinates.
C--
C 15-Sep-1994: new routine [TJP].
C 21-Jun-1995: minor change to header comments [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      REAL PA(6)
      LOGICAL PGNOTO
C
C Check inputs.
C
      IF (PGNOTO('PGIMAG')) RETURN
      IF (I1.LT.1 .OR. I2.GT.IDIM .OR. I1.GT.I2 .OR.
     1    J1.LT.1 .OR. J2.GT.JDIM .OR. J1.GT.J2) THEN
          CALL GRWARN('PGIMAG: invalid range I1:I2, J1:J2')
      ELSE IF (A1.EQ.A2) THEN
          CALL GRWARN('PGIMAG: foreground level = background level')
      ELSE IF (PGMNCI(PGID).GT.PGMXCI(PGID)) THEN
          CALL GRWARN('PGIMAG: not enough colors available')
      ELSE
C
C Call lower-level routine to do the work.
C
          CALL PGBBUF
          PA(1) = TR(1)*PGXSCL(PGID) + PGXORG(PGID)
          PA(2) = TR(2)*PGXSCL(PGID)
          PA(3) = TR(3)*PGXSCL(PGID)
          PA(4) = TR(4)*PGYSCL(PGID) + PGYORG(PGID)
          PA(5) = TR(5)*PGYSCL(PGID)
          PA(6) = TR(6)*PGYSCL(PGID)
          CALL GRIMG0(A, IDIM, JDIM, I1, I2, J1, J2, A1, A2, PA,
     :                PGMNCI(PGID), PGMXCI(PGID), PGITF(PGID))
          CALL PGEBUF
      END IF
C-----------------------------------------------------------------------
      END
