C*GRCLPL -- clip line against clipping rectangle
C+
      SUBROUTINE GRCLPL (X0,Y0,X1,Y1,VIS)
C
C GRPCKG (internal routine): Change the end-points of the line (X0,Y0)
C (X1,Y1) to clip the line at the window boundary.  The algorithm is
C that of Cohen and Sutherland (ref: Newman & Sproull).
C
C Arguments:
C
C X0, Y0 (input/output, real): device coordinates of starting point
C       of line.
C X1, Y1 (input/output, real): device coordinates of end point of line.
C VIS (output, logical): .TRUE. if line lies wholly or partially
C       within the clipping rectangle; .FALSE. if it lies entirely
C       outside the rectangle.
C--
C 13-Jul-1984 - [TJP].
C 20-Jun-1985 - [TJP] - revise clipping algorithm.
C 28-Jun-1991 - [TJP] - use IAND().
C 12-Jun-1992 - [TJP] - clip exactly on the boundary.
C
C Caution: IAND is a non-standard intrinsic function to do bitwise AND
C of two integers. If it is not supported by your Fortran compiler, you
C will need to modify this routine or supply an IAND function.
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      LOGICAL  VIS
      INTEGER  C0,C1,C
      REAL     XMIN,XMAX,YMIN,YMAX
      REAL     X,Y, X0,Y0, X1,Y1
      INTEGER IAND
C
      XMIN = GRXMIN(GRCIDE)
      YMIN = GRYMIN(GRCIDE)
      XMAX = GRXMAX(GRCIDE)
      YMAX = GRYMAX(GRCIDE)
      CALL GRCLIP(X0,Y0,XMIN,XMAX,YMIN,YMAX,C0)
      CALL GRCLIP(X1,Y1,XMIN,XMAX,YMIN,YMAX,C1)
   10 IF (C0.NE.0 .OR. C1.NE.0) THEN
          IF (IAND(C0,C1).NE.0) THEN
C             ! line is invisible
              VIS = .FALSE.
              RETURN
          END IF
          C = C0
          IF (C.EQ.0) C = C1
          IF (IAND(C,1).NE.0) THEN
C             ! crosses XMIN
              Y = Y0 + (Y1-Y0)*(XMIN-X0)/(X1-X0)
              X = XMIN
          ELSE IF (IAND(C,2).NE.0) THEN
C             ! crosses XMAX
              Y = Y0 + (Y1-Y0)*(XMAX-X0)/(X1-X0)
              X = XMAX
          ELSE IF (IAND(C,4).NE.0) THEN
C             ! crosses YMIN
              X = X0 + (X1-X0)*(YMIN-Y0)/(Y1-Y0)
              Y = YMIN
          ELSE IF (IAND(C,8).NE.0) THEN
C             ! crosses YMAX
              X = X0 + (X1-X0)*(YMAX-Y0)/(Y1-Y0)
              Y = YMAX
          END IF
          IF (C.EQ.C0) THEN
              X0 = X
              Y0 = Y
              CALL GRCLIP(X,Y,XMIN,XMAX,YMIN,YMAX,C0)
          ELSE
              X1 = X
              Y1 = Y
              CALL GRCLIP(X,Y,XMIN,XMAX,YMIN,YMAX,C1)
          END IF
      GOTO 10
      END IF
      VIS = .TRUE.
      END
