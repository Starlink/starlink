      PROGRAM PGDEM6
C-----------------------------------------------------------------------
C Test program for PGPLOT: test of Cursor
C-----------------------------------------------------------------------
      INTEGER PGBEG, PGBAND
      INTEGER JUNK, MODE
      CHARACTER*1 CH
      REAL X,Y
C
      WRITE(*,*) '   This program demonstrates the use of routine',
     :             ' PGBAND. It'
      WRITE(*,*) '   requires a graphics device with a cursor.',
     :             ' Position the cursor'
      WRITE(*,*) '   anywhere in the window. Press any key (or a mouse',
     :             ' button if'
      WRITE(*,*) '   the device has a mouse supported by PGPLOT); the',
     :             ' program'
      WRITE(*,*) '   draws a marker at the current cursor position and',
     :             ' reports the'
      WRITE(*,*) '   current cursor position [bottom left corner is',
     :             ' (0,0); top'
      WRITE(*,*) '   right is (1,1)] and the ASCII code of the key',
     :             ' that you'
      WRITE(*,*) '   pressed. To exit from the program, type a slash',
     :             ' (/), ctrl-D,'
      WRITE(*,*) '   or ctrl-Z. The + key toggles between normal',
     :             ' cursor, cross-hair,'
      WRITE(*,*) '   and other cursor modes (on supporting devices).'
C
C Open device for graphics.
C
      IF (PGBEG(0,'?',1,1) .NE. 1) STOP
C
C Clear the screen. Draw a frame at the physical extremities of the
C plot, using full-screen viewport and standard window.
C
      CALL PGPAGE
      CALL PGSVP(0.0,1.0,0.0,1.0)
      CALL PGSWIN(0.0,1.0,0.0,1.0)
      CALL PGBOX('bcts',0.1,5,'bcts',0.1,5)
C
C Loop to read and display cursor position. Initial position for cursor
C is center of viewport. 
C
      X = 0.5
      Y = 0.5
      MODE = 0
 10   CONTINUE
          JUNK = PGBAND(MODE, 1, X, Y, X,Y,CH)
          WRITE (*, '(2F8.3,I4)') X,Y,ICHAR(CH)
C         Check for exit
          IF (CH.EQ.'/' .OR. CH.EQ.CHAR(0) .OR. CH.EQ.CHAR(4) .OR.
     :        CH.EQ.CHAR(26)) GOTO 20
C         Check for switch of cursor type.
          IF (CH.EQ.'+') THEN
             MODE = MOD(MODE+1,8)
             WRITE (*,*) 'Cursor mode:', MODE
             GOTO 10
          END IF
          CALL PGPT1(X, Y, ICHAR(CH))
      GOTO 10
C
C Close the device and exit.
C
 20   CALL PGEND
      END
