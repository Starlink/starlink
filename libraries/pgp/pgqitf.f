C*PGQITF -- inquire image transfer function
C%void cpgqitf(int *itf);
C+
      SUBROUTINE PGQITF (ITF)
      INTEGER  ITF
C
C Return the Image Transfer Function as set by default or by a previous
C call to PGSITF. The Image Transfer Function is used by routines
C PGIMAG, PGGRAY, and PGWEDG.
C
C Argument:
C  ITF    (output) : type of transfer function (see PGSITF)
C--
C 15-Sep-1994 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGQITF')) THEN
          ITF = 0
      ELSE
          ITF = PGITF(PGID)
      END IF
      END
