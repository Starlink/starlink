C History:
C   01-Aug-1995 (rpt):
C    ICOLOR support added.
C   17-Sep-1995 (rp):
C    Put ICOLOR into FLAGCOMM
C   20-Jul-2000 (ajc):
C    Change TYPE * to PRINT *
C    Unused in PLOT_SPECTRUM: FINISH
C--------------------------------------------------------------------------

      SUBROUTINE PLOT_SPECTRUM (XSCALE, BUF, IFAIL)

C  Plot new spectrum on same set of axes as last plot.

      IMPLICIT  NONE

C     Formal parameters:

      REAL      XSCALE(1)
      REAL      BUF(1)
      INTEGER   IFAIL

C     Include files:

      INCLUDE   'NOKEEP'
      INCLUDE   'FLAGCOMM'

C     Local variables:

      REAL      BOX(4)
      INTEGER   ISTAT
      INTEGER   IWEIGHT
      LOGICAL   REPEAT

C     Functions

      INTEGER   ICHECK

C  Ok, go...

      IF(ICHECK(1,IFAIL).NE.1)   RETURN

C     Check that a plot file is open

      IF(JPLOT.EQ.0) THEN
        IFAIL=7
        RETURN
      ENDIF

C     Find line weight

      CALL GEN_GETI4 ('Line weight for plot?',
     &                 LWEIGHT, 'I2', IWEIGHT, ISTAT)

      CALL GEN_GETI4 ('Color for plot [1-15]?',
     &                 ICOLOR, 'I3', ICOLOR, ISTAT)
      IF (ABS(ICOLOR).lt.1  .or. ABS(ICOLOR).gt.15) THEN
        PRINT *, 'Colour must be in range [1--15]; using 1'
        ICOLOR = 1
      END IF

C     Add current spectrum to plot file

      CALL QPLOT (XSCALE, IWEIGHT, ICOLOR, IFAIL)
      IF (IFAIL.NE.0)   RETURN

C     Output plot to device

      IF (TERMINAL)THEN

C       Initialize plot device and plot

        CALL ALLOCATE_DEVICE (IDEV, IFAIL)
        IF (IFAIL.NE.0) THEN
          RETURN
        END IF

        REPEAT = .TRUE.
        DO WHILE (REPEAT)
          CALL PLOT_FILE (XSCALE, BUF, IFAIL)
          IF (IFAIL.NE.0) RETURN
          REPEAT = .FALSE.
          IF (INTERACTIVE) THEN
            CALL IVTOPT(IDEV,' L R B T N E Q H C D ? S',
     &                  BOX(1),BOX(3),REPEAT)
          END IF
        END DO

        CALL ENDPLOT (IDEV, INTERACTIVE, .FALSE.)

      END IF

      RETURN
      END


