      PROGRAM PGDEM5
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT. This programs shows the use of
C routine PGOLIN to allow the user to draw polygons on the screen.
C As each polygon is completed, it is filled in using routine PGPOLY.
C The user positions the cursor to define the vertices of the polygon.
C He types 'A' to add a vertex at the current cursor position, 'D' to
C delete the nearest vertex, or 'X' to signal that the polygon is 
C complete. Two 'X's in succession terminates the program.
C-----------------------------------------------------------------------
      INTEGER PGBEG
      INTEGER MAXPT, NPT, COL
      PARAMETER (MAXPT=50)
      REAL X(MAXPT),Y(MAXPT)
      INTEGER WHICH
C
  100 FORMAT(' Demonstration of PGPLOT cursor routines',
     1       ' PGLCUR and PGOLIN.'/
     2       ' These routines allow you to draw polygons on the',
     3       ' screen, using the'/
     4       ' cursor to mark the vertices.'/)
  110 FORMAT(/' PGLCUR outlines the polygon as you draw it, PGOLIN',
     1       ' just marks the'/
     2       ' vertices. Which routine do you want to use? Type 1',
     3       ' for PGLCUR, 2 for'/
     4       ' PGOLIN:')
  120 FORMAT(' Use the cursor to choose the vertices of the polygon'/
     1       '   Type A to add a vertex at the cursor position'/
     2       '   Type D to delete the last vertex'/
     3       '   Type X to close the polygon and shade it'/
     4       '   (Type X again to exit from the program)')
C
      WRITE (6, 100)
      WRITE (6, 110)
      READ (5, *, ERR=10, END=10) WHICH
 10   IF (WHICH.LT.1 .OR. WHICH.GT.2) WHICH = 1
      WRITE (6, 120)
C
C Open device for graphics.
C
      IF (PGBEG(0,'?',1,1) .NE. 1) STOP
C
C Clear the screen. Draw a frame at the physical extremities of the
C view surface using full-screen viewport and standard window.
C
      CALL PGPAGE
      CALL PGSVP(0.0,1.0,0.0,1.0)
      CALL PGSWIN(0.0,1.0,0.0,1.0)
      CALL PGBOX('BC',0.1,5,'BC',0.1,5)
      COL = 0
C
C Increment the color index and then call PGOLIN to allow the user
C to draw a polygon.
C
   20 COL = COL+1
      CALL PGSCI(COL)
      NPT = 0
      IF (WHICH.EQ.1) CALL PGLCUR(MAXPT,NPT,X,Y)
      IF (WHICH.EQ.2) CALL PGOLIN(MAXPT,NPT,X,Y,-1)
C
C Fill the interior of the polygon in the current color. If less
C than three vertices were supplied, that is a signal to terminate
C the program.  Otherwise, go back and draw another polygon.
C
      IF (NPT.GE.3) THEN
           CALL PGPOLY(NPT,X,Y)
           GOTO 20
       END IF 
C
C Close the device and exit.
C
      CALL PGEND
      END
