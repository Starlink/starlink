C*PGQCF -- inquire character font
C%void cpgqcf(int *font);
C+
      SUBROUTINE PGQCF (FONT)
      INTEGER  FONT
C
C Query the current Character Font (set by routine PGSCF).
C
C Argument:
C  FONT   (output)   : the current font number (in range 1-4).
C--
C  5-Nov-1985 - new routine [TJP].
C 25-OCT-1993 - changed name of argument [TJP].
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGQCF')) THEN
         FONT = 1
      ELSE
         CALL GRQFNT(FONT)
      END IF
      END
