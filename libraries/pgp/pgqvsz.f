C*PGQVSZ -- find the window defined by the full view surface
C%void cpgqvsz(int units, float *x1, float *x2, float *y1, float *y2);
C+
      SUBROUTINE PGQVSZ (UNITS, X1, X2, Y1, Y2)
      INTEGER UNITS
      REAL X1, X2, Y1, Y2
C
C Return the window, in a variety of units, defined by the full
C device view surface (0 -> 1 in normalized device coordinates).
C
C Input:
C   UNITS    0,1,2,3 for output in normalized device coords,
C            inches, mm, or absolute device units (dots)
C Output
C   X1,X2    X window
C   Y1,Y2    Y window
C
C--
C 28-Aug-92 - new routine (Neil Killeen)
C  2-Dec-92 - changed to avoid resetting the viewport (TJP).
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
     1        'Illegal value for parameter UNITS in routine PGQVSZ')
          SX = PGXSZ(PGID)
          SY = PGYSZ(PGID)
      END IF
      X1 = 0.0
      X2 = PGXSZ(PGID)/SX
      Y1 = 0.0
      Y2 = PGYSZ(PGID)/SY
      END
