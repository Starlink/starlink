C*PGBAND -- read cursor position, with anchor
C%int cpgband(int mode, int posn, float xref, float yref, float *x,\
C%            float *y, char *ch_scalar);
C+
      INTEGER FUNCTION PGBAND (MODE, POSN, XREF, YREF, X, Y, CH)
      INTEGER MODE, POSN
      REAL XREF, YREF, X, Y
      CHARACTER*(*) CH
C
C Read the cursor position and a character typed by the user.
C The position is returned in world coordinates.  PGBAND positions
C the cursor at the position specified (if POSN=1), allows the user to
C move the cursor using the mouse or arrow keys or whatever is available
C on the device. When he has positioned the cursor, the user types a
C single character on the keyboard; PGBAND then returns this
C character and the new cursor position (in world coordinates).
C
C Some interactive devices offer a selection of cursor types,
C implemented as thin lines that move with the cursor, but without
C erasing underlying graphics. Of these types, some extend between
C a stationary anchor-point at XREF,YREF, and the position of the
C cursor, while others simply follow the cursor without changing shape
C or size. The cursor type is specified with one of the following MODE
C values. Cursor types that are not supported by a given device, are
C treated as MODE=0.
C
C -- If MODE=0, the anchor point is ignored and the routine behaves
C like PGCURS.
C -- If MODE=1, a straight line is drawn joining the anchor point 
C and the cursor position.
C -- If MODE=2, a hollow rectangle is extended as the cursor is moved,
C with one vertex at the anchor point and the opposite vertex at the
C current cursor position; the edges of the rectangle are horizontal
C and vertical.
C -- If MODE=3, two horizontal lines are extended across the width of
C the display, one drawn through the anchor point and the other
C through the moving cursor position. This could be used to select
C a Y-axis range when one end of the range is known.
C -- If MODE=4, two vertical lines are extended over the height of
C the display, one drawn through the anchor point and the other
C through the moving cursor position. This could be used to select an
C X-axis range when one end of the range is known.
C -- If MODE=5, a horizontal line is extended through the cursor
C position over the width of the display. This could be used to select
C an X-axis value such as the start of an X-axis range. The anchor point
C is ignored.
C -- If MODE=6, a vertical line is extended through the cursor
C position over the height of the display. This could be used to select
C a Y-axis value such as the start of a Y-axis range. The anchor point
C is ignored.
C -- If MODE=7, a cross-hair, centered on the cursor, is extended over
C the width and height of the display. The anchor point is ignored.
C
C Returns:
C  PGBAND          : 1 if the call was successful; 0 if the device
C                    has no cursor or some other error occurs.
C Arguments:
C  MODE   (input)  : display mode (0, 1, ..7: see above).
C  POSN   (input)  : if POSN=1, PGBAND attempts to place the cursor
C                    at point (X,Y); if POSN=0, it leaves the cursor
C                    at its current position. (On some devices this
C                    request may be ignored.)
C  XREF   (input)  : the world x-coordinate of the anchor point.
C  YREF   (input)  : the world y-coordinate of the anchor point.
C  X      (in/out) : the world x-coordinate of the cursor.
C  Y      (in/out) : the world y-coordinate of the cursor.
C  CH     (output) : the character typed by the user; if the device has
C                    no cursor or if some other error occurs, the value
C                    CHAR(0) [ASCII NUL character] is returned.
C
C Note: The cursor coordinates (X,Y) may be changed by PGBAND even if
C the device has no cursor or if the user does not move the cursor.
C Under these circumstances, the position returned in (X,Y) is that of
C the pixel nearest to the requested position.
C--
C 7-Sep-1994 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE      'pgplot.inc'
      INTEGER      GRCURS, I, J, IREF, JREF
      LOGICAL      PGNOTO
C
      IF (PGNOTO('PGBAND')) THEN
          CH = CHAR(0)
          PGBAND = 0
          RETURN
      END IF
      IF (MODE.LT.0 .OR. MODE.GT.7) CALL GRWARN(
     :     'Invalid MODE argument in PGBAND')
      IF (POSN.LT.0 .OR. POSN.GT.1) CALL GRWARN(
     :     'Invalid POSN argument in PGBAND')
C
      I = NINT(PGXORG(PGID) + X*PGXSCL(PGID))
      J = NINT(PGYORG(PGID) + Y*PGYSCL(PGID))
      IREF = NINT(PGXORG(PGID) + XREF*PGXSCL(PGID))
      JREF = NINT(PGYORG(PGID) + YREF*PGYSCL(PGID))
      PGBAND = GRCURS(PGID,I,J,IREF,JREF,MODE,POSN,CH)
      X = (I - PGXORG(PGID))/PGXSCL(PGID)
      Y = (J - PGYORG(PGID))/PGYSCL(PGID)
      CALL GRTERM
      END
