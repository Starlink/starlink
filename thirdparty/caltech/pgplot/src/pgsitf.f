C*PGSITF -- set image transfer function
C%void cpgsitf(int itf);
C+
      SUBROUTINE PGSITF (ITF)
      INTEGER  ITF
C
C Set the Image Transfer Function for subsequent images drawn by
C PGIMAG, PGGRAY, or PGWEDG. The Image Transfer Function is used
C to map array values into the available range of color indices
C specified with routine PGSCIR or (for PGGRAY on some devices)
C into dot density.
C
C Argument:
C  ITF    (input)  : type of transfer function:
C                      ITF = 0 : linear
C                      ITF = 1 : logarithmic
C                      ITF = 2 : square-root
C--
C 15-Sep-1994 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSITF')) RETURN
      IF (ITF.LT.0 .OR. ITF.GT.2) THEN
          PGITF(PGID) = 0
          CALL GRWARN('PGSITF: argument must be 0, 1, or 2')
      ELSE
          PGITF(PGID) = ITF
      END IF
      END
