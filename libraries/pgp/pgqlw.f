C*PGQLW -- inquire line width
C%void cpgqlw(int *lw);
C+
      SUBROUTINE PGQLW (LW)
      INTEGER  LW
C
C Query the current Line-Width attribute (set by routine PGSLW).
C
C Argument:
C  LW     (output)  : the line-width (in range 1-201).
C--
C  5-Nov-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C     
      IF (PGNOTO('PGQLW')) THEN
         LW = 1
      ELSE
         CALL GRQLW(LW)
      END IF
      END
