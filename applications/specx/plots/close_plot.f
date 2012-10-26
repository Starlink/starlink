*  History:
*     19 Nov 1993 (hme):
*        Replace LIB$FREE_LUN with FIO_PUNIT.
*     09 Jan 1994 (rp):
*        Replace FIO_PUNIT with IFREELUN
*     15 Jan 1994 (rp):
*        Parameterize array sizes from SPECX_PARS
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
C-----------------------------------------------------------------------

      SUBROUTINE CLOSE_PLOT (JPLOT, IPSEQ, IDEV)

C   Routine to close off plot file for either batch or on-line plotting

      CHARACTER  FILNAM*40, SEQUENCE*3, TTNAME*4

      INCLUDE  'SPECX_PARS'
      INCLUDE  'CNF_PAR'

      INTEGER*4 PLOT_UNIT
      COMMON /PLTDEV/ PLOT_UNIT

*     Local variables

      INTEGER STATUS

*     Functions

      INTEGER IFREELUN

      DATA TTNAME /'    '/

*  Ok, go...

C  If there is no open plot file give up now!

      IF(JPLOT.EQ.0)   RETURN

C  If device is not a terminal, then plot the file on chosen device

      IF (IDEV.GE.20) THEN

        PRINT *,'Disposing of existing plot file...'

C       Initialize plot device and plot

        CALL ALLOCATE_DEVICE (IDEV, IFAIL)
        IF (IFAIL.NE.0) THEN
          RETURN
        END IF

        ISTAT = IGETVM (4*LSPMAX, .TRUE., 'CLOSE_PLOT', IXPTR)
        ISTAT = IGETVM (4*LSPMAX, .TRUE., 'CLOSE_PLOT', IYPTR)
        CALL PLOT_FILE (%VAL(CNF_PVAL(IXPTR)), %VAL(CNF_PVAL(IYPTR)), 
     :                  IFAIL)
        ISTAT = IFREEVM (IXPTR)
        ISTAT = IFREEVM (IYPTR)

        CALL SXGDEVEND

      END IF

C  Reset JPLOT for next plot and close off data file (PLOT.nnn)

      JPLOT = 0
      CLOSE  (PLOT_UNIT)
      STATUS = IFREELUN (PLOT_UNIT)

C  Dispose of plot

      WRITE (SEQUENCE,'(I3.3)') IPSEQ
      FILNAM = 'PLOT.'//SEQUENCE
      CALL DEL_PLOT (FILNAM)

      RETURN
      END
