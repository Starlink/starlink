C*PGQVP -- inquire viewport size and position
C%void cpgqvp(int units, float *x1, float *x2, float *y1, float *y2);
C+
      SUBROUTINE PGQVP (UNITS, X1, X2, Y1, Y2)
      INTEGER UNITS
      REAL    X1, X2, Y1, Y2
C
C Inquiry routine to determine the current viewport setting.
C The values returned may be normalized device coordinates, inches, mm,
C or pixels, depending on the value of the input parameter CFLAG.
C
C Arguments:
C  UNITS  (input)  : used to specify the units of the output parameters:
C                    UNITS = 0 : normalized device coordinates
C                    UNITS = 1 : inches
C                    UNITS = 2 : millimeters
C                    UNITS = 3 : pixels
C                    Other values give an error message, and are
C                    treated as 0.
C  X1     (output) : the x-coordinate of the bottom left corner of the
C                    viewport.
C  X2     (output) : the x-coordinate of the top right corner of the
C                    viewport.
C  Y1     (output) : the y-coordinate of the bottom left corner of the
C                    viewport.
C  Y2     (output) : the y-coordinate of the top right corner of the
C                    viewport.
C--
C 26-Sep-1985 - new routine (TJP).
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      REAL SX, SY
C
      IF (UNITS.EQ.0) THEN
          SX = PGXSZ(PGID)
          SY = PGYSZ(PGID)
      ELSE IF (UNITS.EQ.1) THEN
          SX = PGXPIN(PGID)
          SY = PGYPIN(PGID)
      ELSE IF (UNITS.EQ.2) THEN
          SX = (PGXPIN(PGID)/25.4)
          SY = (PGYPIN(PGID)/25.4)
      ELSE IF (UNITS.EQ.3) THEN
          SX = 1.0
          SY = 1.0
      ELSE
          CALL GRWARN(
     1        'Illegal value for parameter UNITS in routine PGQVP')
          SX = PGXSZ(PGID)
          SY = PGYSZ(PGID)
      END IF
      X1 = PGXVP(PGID)/SX
      X2 = (PGXVP(PGID)+PGXLEN(PGID))/SX
      Y1 = PGYVP(PGID)/SY
      Y2 = (PGYVP(PGID)+PGYLEN(PGID))/SY
      END
