C*PGCONT -- contour map of a 2D data array (contour-following)
C%void cpgcont(const float *a, int idim, int jdim, int i1, int i2, \
C% int j1, int j2, const float *c, int nc, const float *tr);
C+
      SUBROUTINE PGCONT (A, IDIM, JDIM, I1, I2, J1, J2, C, NC, TR)
      INTEGER IDIM, JDIM, I1, J1, I2, J2, NC
      REAL A(IDIM,JDIM), C(*), TR(6)
C
C Draw a contour map of an array.  The map is truncated if
C necessary at the boundaries of the viewport.  Each contour line
C is drawn with the current line attributes (color index, style, and
C width); except that if argument NC is positive (see below), the line
C style is set by PGCONT to 1 (solid) for positive contours or 2
C (dashed) for negative contours.
C
C Arguments:
C  A      (input) : data array.
C  IDIM   (input) : first dimension of A.
C  JDIM   (input) : second dimension of A.
C  I1, I2 (input) : range of first index to be contoured (inclusive).
C  J1, J2 (input) : range of second index to be contoured (inclusive).
C  C      (input) : array of NC contour levels; dimension at least NC.
C  NC     (input) : +/- number of contour levels (less than or equal
C                   to dimension of C). If NC is positive, it is the
C                   number of contour levels, and the line-style is
C                   chosen automatically as described above. If NC is
C                   negative, it is minus the number of contour
C                   levels, and the current setting of line-style is
C                   used for all the contours.
C  TR     (input) : array defining a transformation between the I,J
C                   grid of the array and the world coordinates.
C                   The world coordinates of the array point A(I,J)
C                   are given by:
C                     X = TR(1) + TR(2)*I + TR(3)*J
C                     Y = TR(4) + TR(5)*I + TR(6)*J
C                   Usually TR(3) and TR(5) are zero - unless the
C                   coordinate transformation involves a rotation or
C                   shear.
C--
C (7-Feb-1983)
C (24-Aug-1984) Revised to add the option of not automatically
C       setting the line-style. Sorry about the ugly way this is
C       done (negative NC); this is the least incompatible way of doing
C       it (TJP).
C (21-Sep-1989) Changed to call PGCONX instead of duplicating the code
C       [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      INTEGER  I
      LOGICAL  PGNOTO
      EXTERNAL PGCP
C
      IF (PGNOTO('PGCONT')) RETURN
C
C Save TRANS matrix.
C
      DO 10 I=1,6
          TRANS(I) = TR(I)
   10 CONTINUE
C
C Use PGCONX with external function PGCP, which applies the TRANS
C scaling.
C
      CALL PGCONX (A, IDIM, JDIM, I1, I2, J1, J2, C, NC, PGCP)
C
      END
