C*PGCURS -- read cursor position
C%int cpgcurs(float *x, float *y, char *ch_scalar);
C+
      INTEGER FUNCTION PGCURS (X, Y, CH)
      REAL X, Y
      CHARACTER*(*) CH
C
C Read the cursor position and a character typed by the user.
C The position is returned in world coordinates.  PGCURS positions
C the cursor at the position specified, allows the user to move the
C cursor using the joystick or arrow keys or whatever is available on
C the device. When he has positioned the cursor, the user types a
C single character on the keyboard; PGCURS then returns this
C character and the new cursor position (in world coordinates).
C
C Returns:
C  PGCURS         : 1 if the call was successful; 0 if the device
C                    has no cursor or some other error occurs.
C Arguments:
C  X      (in/out) : the world x-coordinate of the cursor.
C  Y      (in/out) : the world y-coordinate of the cursor.
C  CH     (output) : the character typed by the user; if the device has
C                    no cursor or if some other error occurs, the value
C                    CHAR(0) [ASCII NUL character] is returned.
C
C Note: The cursor coordinates (X,Y) may be changed by PGCURS even if
C the device has no cursor or if the user does not move the cursor.
C Under these circumstances, the position returned in (X,Y) is that of
C the pixel nearest to the requested position.
C--
C  7-Sep-1994 - changed to use PGBAND [TJP].
C-----------------------------------------------------------------------
      INTEGER PGBAND
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGCURS')) THEN
         CH = CHAR(0)
         PGCURS = 0
      ELSE
         PGCURS = PGBAND(0, 1, 0.0, 0.0, X, Y, CH)
      END IF
      END
