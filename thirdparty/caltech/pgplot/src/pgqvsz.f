C*PGQVSZ -- inquire size of view surface
C%void cpgqvsz(int units, float *x1, float *x2, float *y1, float *y2);
C+
      SUBROUTINE PGQVSZ (UNITS, X1, X2, Y1, Y2)
      INTEGER UNITS
      REAL X1, X2, Y1, Y2
C
C This routine returns the dimensions of the view surface (the maximum
C plottable area) of the currently selected graphics device, in 
C a variety of units. The size of the view surface is device-dependent
C and is established when the graphics device is opened. On some 
C devices, it can be changed by calling PGPAP before starting a new
C page with PGPAGE. On some devices, the size can be changed (e.g.,
C by a workstation window manager) outside PGPLOT, and PGPLOT detects
C the change when PGPAGE is used. Call this routine after PGPAGE to 
C find the current size.
C
C Note 1: the width and the height of the view surface in normalized
C device coordinates are both always equal to 1.0.
C
C Note 2: when the device is divided into panels (see PGSUBP), the
C view surface is a single panel.
C
C Arguments:
C  UNITS  (input)  : 0,1,2,3 for output in normalized device coords, 
C                    inches, mm, or device units (pixels)
C  X1     (output) : always returns 0.0
C  X2     (output) : width of view surface
C  Y1     (output) : always returns 0.0
C  Y2     (output) : height of view surface
C--
C 28-Aug-1992 - new routine [Neil Killeen].
C  2-Dec-1992 - changed to avoid resetting the viewport [TJP].
C 26-Feb-1997 - revised description [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
      REAL SX, SY
C
      IF (PGNOTO('PGQVSZ')) THEN
         X1 = 0.0
         X2 = 0.0
         Y1 = 0.0
         Y2 = 0.0
         RETURN
      END IF
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
