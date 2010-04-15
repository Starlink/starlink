*  History:
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused INEW, IANS
C-----------------------------------------------------------------------------

      SUBROUTINE SEE_PLOT (XSCALE, BUF, IFAIL)

C  Send (unclosed) plot to nominated device

      IMPLICIT   NONE

      REAL       XSCALE(*)
      REAL       BUF(*)
      INTEGER    IFAIL

      LOGICAL    REPEAT,SEE_TERMINAL
      INTEGER    IOLD, IVT
      INTEGER    IDEV1
      REAL       BOX(4)

      INCLUDE   'FLAGCOMM'
      INCLUDE   'NOKEEP'

      INTEGER   IVTOPT

      IF (JPLOT.EQ.0)  THEN
        IFAIL = 7
        RETURN
      END IF

C  Determine the plot device for quick look

      IOLD = -1
      CALL ASK_PLOT_DEVICE (IOLD, IDEV1, SEE_TERMINAL)

      IF (IDEV1.LE.0) THEN
        PRINT *,'Unknown plot device - abandoning'
        RETURN
      END IF

C   Then initialize plot device and plot

      CALL ALLOCATE_DEVICE (IDEV1, IFAIL)
      IF (IFAIL.NE.0) THEN
        RETURN
      END IF

C  Plot on chosen device

      REPEAT = .TRUE.
      DO WHILE (REPEAT)
        CALL PLOT_FILE (XSCALE, BUF, IFAIL)
        IF (IFAIL.NE.0) RETURN
        IF (SEE_TERMINAL .AND. INTERACTIVE) THEN
          IVT=IVTOPT(IDEV, ' L R B T N E Q H C D ? S',
     &               BOX(1), BOX(3), REPEAT)
        ELSE
          REPEAT=.FALSE.
        END IF
      END DO

      CALL ENDPLOT (IDEV1, INTERACTIVE, .TRUE.)

      RETURN
      END

