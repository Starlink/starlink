C-----------------------------------------------------------------------

      SUBROUTINE DELETE_LAST_PLOT (JPLOT, IPSEQ, IFAIL)

C  Routine to remove last plot from file ( saves redoing whole thing )

      LOGICAL ICONT

      INTEGER*4 PLOT_UNIT
      COMMON /PLTDEV/ PLOT_UNIT

C  Go...

      IFAIL=0

C  If no plot file open, give up now

      IF(JPLOT.EQ.0)   RETURN

C  Backspace over last plot and mark end of file

      DO J=1,5
        BACKSPACE(PLOT_UNIT)
      END DO
      ICONT=.FALSE.
      WRITE(PLOT_UNIT) ICONT

C  Reset plot counter.

      JPLOT=JPLOT-1

      RETURN
      END


