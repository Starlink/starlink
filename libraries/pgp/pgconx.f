C*PGCONX -- contour map of a 2D data array (non rectangular)
C+
      SUBROUTINE PGCONX (A, IDIM, JDIM, I1, I2, J1, J2, C, NC, PLOT)
      INTEGER  IDIM, JDIM, I1, J1, I2, J2, NC
      REAL     A(IDIM,JDIM), C(*)
      EXTERNAL PLOT
C
C Draw a contour map of an array using a user-supplied plotting
C routine.  This routine should be used instead of PGCONT when the
C data are defined on a non-rectangular grid.  PGCONT permits only
C a linear transformation between the (I,J) grid of the array
C and the world coordinate system (x,y), but PGCONX permits any
C transformation to be used, the transformation being defined by a
C user-supplied subroutine. The nature of the contouring algorithm,
C however, dictates that the transformation should maintain the
C rectangular topology of the grid, although grid-points may be
C allowed to coalesce.  As an example of a deformed rectangular
C grid, consider data given on the polar grid theta=0.1n(pi/2),
C for n=0,1,...,10, and r=0.25m, for m=0,1,..,4. This grid
C contains 55 points, of which 11 are coincident at the origin.
C The input array for PGCONX should be dimensioned (11,5), and
C data values should be provided for all 55 elements.  PGCONX can
C also be used for special applications in which the height of the
C contour affects its appearance, e.g., stereoscopic views.
C
C The map is truncated if necessary at the boundaries of the viewport.
C Each contour line is drawn with the current line attributes (color
C index, style, and width); except that if argument NC is positive
C (see below), the line style is set by PGCONX to 1 (solid) for
C positive contours or 2 (dashed) for negative contours. Attributes
C for the contour lines can also be set in the user-supplied
C subroutine, if desired.
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
C  PLOT   (input) : the address (name) of a subroutine supplied by
C                   the user, which will be called by PGCONX to do
C                   the actual plotting. This must be declared
C                   EXTERNAL in the program unit calling PGCONX.
C
C The subroutine PLOT will be called with four arguments:
C      CALL PLOT(VISBLE,X,Y,Z)
C where X,Y (input) are real variables corresponding to
C I,J indices of the array A. If  VISBLE (input, integer) is 1,
C PLOT should draw a visible line from the current pen
C position to the world coordinate point corresponding to (X,Y);
C if it is 0, it should move the pen to (X,Y). Z is the value
C of the current contour level, and may be used by PLOT if desired.
C Example:
C       SUBROUTINE PLOT (VISBLE,X,Y,Z)
C       REAL X, Y, Z, XWORLD, YWORLD
C       INTEGER VISBLE
C       XWORLD = X*COS(Y) ! this is the user-defined
C       YWORLD = X*SIN(Y) ! transformation
C       IF (VISBLE.EQ.0) THEN
C           CALL PGMOVE (XWORLD, YWORLD)
C       ELSE
C           CALL PGDRAW (XWORLD, YWORLD)
C       END IF
C       END
C--
C 14-Nov-1985 - new routine [TJP].
C 12-Sep-1989 - correct documentation error [TJP].
C 22-Apr-1990 - corrected bug in panelling algorithm [TJP].
C 13-Dec-1990 - make errors non-fatal [TJP].
C-----------------------------------------------------------------------
      INTEGER  MAXEMX,MAXEMY
      PARAMETER (MAXEMX=100)
      PARAMETER (MAXEMY=100)
      INTEGER  I
      INTEGER  NNX,NNY, KX,KY, KI,KJ, IA,IB, JA,JB, LS, PX, PY
      LOGICAL  STYLE, PGNOTO
C
C Check arguments.
C
      IF (PGNOTO('PGCONX')) RETURN
      IF (I1.LT.1 .OR. I2.GT.IDIM .OR. I1.GE.I2 .OR.
     1    J1.LT.1 .OR. J2.GT.JDIM .OR. J1.GE.J2) THEN
          CALL GRWARN('PGCONX: invalid range I1:I2, J1:J2')
          RETURN
      END IF
      IF (NC.EQ.0) RETURN
      STYLE = NC.GT.0
      CALL PGQLS(LS)
      CALL PGBBUF
C
C Divide arrays into panels not exceeding MAXEMX by MAXEMY for
C contouring by PGCNSC.
C
CD    write (*,*) 'PGCONX window:',i1,i2,j1,j2
      NNX = I2-I1+1
      NNY = J2-J1+1
      KX = MAX(1,(NNX+MAXEMX-2)/(MAXEMX-1))
      KY = MAX(1,(NNY+MAXEMY-2)/(MAXEMY-1))
      PX = (NNX+KX-1)/KX
      PY = (NNY+KY-1)/KY
      DO 60 KI=1,KX
          IA = I1 + (KI-1)*PX
          IB = MIN(I2, IA + PX)
          DO 50 KJ=1,KY
              JA = J1 + (KJ-1)*PY
              JB = MIN(J2, JA + PY)
C
C             Draw the contours in one panel.
C
CD            write (*,*) 'PGCONX panel:',ia,ib,ja,jb
              IF (STYLE) CALL PGSLS(1)
              DO 40 I=1,ABS(NC)
                  IF (STYLE.AND.(C(I).LT.0.0)) CALL PGSLS(2)
                  CALL PGCNSC(A,IDIM,JDIM,IA,IB,JA,JB,C(I),PLOT)
                  IF (STYLE) CALL PGSLS(1)
   40         CONTINUE
   50     CONTINUE
   60 CONTINUE
C
      CALL PGSLS(LS)
      CALL PGEBUF
      END
