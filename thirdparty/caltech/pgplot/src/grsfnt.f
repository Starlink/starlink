C*GRSFNT -- set text font
C+
      SUBROUTINE GRSFNT (IF)
      INTEGER IF
C
C GRPCKG: Set the font for subsequent text plotting.
C The default font is 1 ("Normal" font); others available are 2
C ("Roman"), 3 ("Italic"), and 4 ("Script").
C
C Argument:
C  IF (input): the font number to be used for subsequent
C       text plotting on the current device (in range 1-4).
C--
C 19-Mar-1983 - [TJP].
C  4-Jun-1984 - add code for GMFILE device [TJP].
C 15-Dec-1988 - change name [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER    I
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRSFNT - no graphics device is active.')
          RETURN
      END IF
C
C Set software font index.
C
      IF (IF.LT.1 .OR. IF.GT.4) THEN
          CALL GRWARN('Illegal font selected: font 1 used.')
          I = 1
      ELSE
          I = IF
      END IF
C
C Ignore request if no change is to be made.
C
      IF (IF.EQ.GRCFNT(GRCIDE)) RETURN
C
C Save font setting.
C
      GRCFNT(GRCIDE) = I
C
      END
