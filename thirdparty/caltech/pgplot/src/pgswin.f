C*PGSWIN -- set window
C%void cpgswin(float x1, float x2, float y1, float y2);
C+
      SUBROUTINE PGSWIN (X1, X2, Y1, Y2)
      REAL X1, X2, Y1, Y2
C
C Change the window in world coordinate space that is to be mapped on
C to the viewport.  Usually PGSWIN is called automatically by PGENV,
C but it may be called directly by the user.
C
C Arguments:
C  X1     (input)  : the x-coordinate of the bottom left corner
C                    of the viewport.
C  X2     (input)  : the x-coordinate of the top right corner
C                    of the viewport (note X2 may be less than X1).
C  Y1     (input)  : the y-coordinate of the bottom left corner
C                    of the viewport.
C  Y2     (input)  : the y-coordinate of the top right corner
C                    of the viewport (note Y2 may be less than Y1).
C--
C 15-Nov-95: check arguments to prevent divide-by-zero [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSWIN')) RETURN
C
C If invalid arguments are specified, issue warning and leave window
C unchanged.
C
      IF (X1.EQ.X2) THEN
         CALL GRWARN('invalid x limits in PGSWIN: X1 = X2.')
      ELSE IF (Y1.EQ.Y2) THEN
         CALL GRWARN('invalid y limits in PGSWIN: Y1 = Y2.')
      ELSE
         PGXBLC(PGID) = X1
         PGXTRC(PGID) = X2
         PGYBLC(PGID) = Y1
         PGYTRC(PGID) = Y2
         CALL PGVW
      END IF
      END
