C--------------------------------------------------------------------------

      SUBROUTINE NEW_PLOT (XSCALE,BUF,IFAIL)

C   Plot current spectrum on Anadex, VT240 or Retrographics type terminal

      REAL*4    XSCALE(1),BUF(1),BOX(4)
      INTEGER   IFAIL
      LOGICAL   REPEAT,FINISH

      INCLUDE   'FLAGCOMM'
      INCLUDE   'NOKEEP'

C  If plot open (e.g. on printer) get rid of it.

      IF (JPLOT.NE.0)   CALL CLOSE_PLOT (JPLOT, IPSEQ, IDEV)

C  Add current spectrum to plot file

      CALL PLTBUF (XSCALE, IFAIL)
      IF (IFAIL.NE.0)   RETURN

C  Then if plotting interactively plot on selected device

      REPEAT = .TRUE.
      FINISH = .FALSE.

      IF (TERMINAL) THEN

C   Then initialize plot device and plot

        CALL ALLOCATE_DEVICE (IDEV, IFAIL)
        IF (IFAIL.NE.0) THEN
          RETURN
        END IF

        DO WHILE (REPEAT)
          CALL PLOT_FILE (XSCALE, BUF, IFAIL)
          IF (IFAIL.NE.0) RETURN
          REPEAT = .FALSE.
          IF (INTERACTIVE) THEN
            CALL IVTOPT (IDEV, ' L R B T N E Q H C D ? S',
     &                   BOX(1), BOX(3), REPEAT)
          END IF
        END DO

        CALL ENDPLOT (IDEV, INTERACTIVE, .FALSE.)

      END IF

      RETURN
      END
