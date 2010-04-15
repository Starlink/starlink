*  History:
*     19 Nov 1993 (hme):
*        Replace LIB${GET|FREE}_LUN with FIO_{G|P}UNIT.
*        Use PSX_GETENV to translate SPECXDIR.
*     30 Nov 1993 (hme):
*        Disuse SPECXDIR, use current working directory instead.
*     31 Dec 1993 (rp)
*        Insert changes from V6.3
*     09 Jan 1994 (rp):
*        Replace FIO_{G|P}UNIT with I{GET|FREE}LUN
*     01-Aug-1995 (rpt):
*        ICOLOR support added.
*     20-Sep-2000 (ajc):
*        Unused ID, VELOFF
*-----------------------------------------------------------------------

      SUBROUTINE PLOT_FILE (XSCALE, DATA, IERR)

C  This is a routine to plot spectra reduced by SPECX on arbitrary output device.
C  It takes an arbitrary number of (X & Y) arrays stored in a sequential
C  unformatted file and plots each set using SXG interface library routines.

      IMPLICIT  NONE

C  Formal parameters

      REAL      XSCALE(*)
      REAL      DATA(*)
      INTEGER   IERR

C  Include files

      INCLUDE 'FLAGCOMM'
      INCLUDE 'NEWXY'
      INCLUDE 'SPECX_PARS'
      INCLUDE 'PLOTPAR1'

C  Local variables

      LOGICAL   ICONT, IOPEN, SHOWHD, TOPSCAL
      INTEGER   ERRCODE, IERR1
      INTEGER   I
      INTEGER   IAXIS
      INTEGER   ITIP1, IPEN1, IWEIGHT
      INTEGER   NQ, J
      INTEGER   NDAT
      INTEGER   STATUS
      REAL      A0, A1
      REAL      BADVAL
      REAL      PLOTS(11)
      REAL      XSIZED, YSIZED
      REAL*8    OSCFREQ, DOPPFAC
      CHARACTER PLOT_HEADER(5)*80
      CHARACTER FNO*3, PLTNAM*40, ICHAR1*76

      REAL      XW1
      REAL      YW1
      REAL      XMARGIN
      REAL      YMARGIN

C   Functions

      INTEGER   GEN_ILEN
      INTEGER   IGETLUN
      INTEGER   IFREELUN

C   Common blocks

      INTEGER   PLOT_UNIT
      COMMON    /PLTDEV/ PLOT_UNIT

      INTEGER   NPTS2(NQMAX)
      INTEGER   NQUAD2
      INTEGER   NMASK2
      INTEGER   MASK2(NQMAX)
      REAL      XFAC2(NQMAX)
      CHARACTER XTITLE2*80, YTITLE2*34
      COMMON    /FREQ2/  NPTS2,  NQUAD2,  MASK2, XFAC2,
     &                   NMASK2, XTITLE2, YTITLE2

      EQUIVALENCE (PLOTS(1),XLEN1)

      DATA      XW1     /25.0/
      DATA      YW1     /50.0/
      DATA      XMARGIN /30./
      DATA      YMARGIN /70./

      IF (IERR.NE.0) RETURN

C   Open file containing data

      INQUIRE (PLOT_UNIT, OPENED=IOPEN)
      IF (IOPEN) THEN
        REWIND (PLOT_UNIT)
      ELSE
        WRITE (FNO,'(I3.3)') IPSEQ
        PLTNAM = 'PLOT.'//FNO
        STATUS = IGETLUN (PLOT_UNIT, 'plot_file', .FALSE.)
        OPEN (PLOT_UNIT,
     &        FILE   =  PLTNAM(:GEN_ILEN(PLTNAM)),
     &        ACCESS = 'SEQUENTIAL',
     &        FORM   = 'UNFORMATTED',
     &        STATUS = 'OLD',
     &        IOSTAT =  ERRCODE)
        IF (ERRCODE.NE.0)   THEN
          CALL GEN_ERMSG (ERRCODE)
          RETURN
        END IF
      END IF

C  Read plot header,update for new scales if necessary

      READ (PLOT_UNIT, IOSTAT=IERR1) PLOTS, IDEV1
      IF (IERR1.NE.0) GO TO 99
      READ (PLOT_UNIT, IOSTAT=IERR1) XTITLE2, YTITLE2
      IF (IERR1.NE.0) GO TO 99

      XRANGE(1) = XST1
      XRANGE(2) = XEND1
      YRANGE(1) = YST1
      YRANGE(2) = YEND1

      IF (CHANGE_SCALES) THEN
        XST1    = XXST
        YST1    = YYST
        XEND1   = XXEND
        YEND1   = YYEND
        NINTSX1 = NX
        NINTSY1 = NY
      END IF

C   Write out the header

      READ (PLOT_UNIT) SHOWHD

      IF (SHOWHD) THEN
        DO I = 1,5
          READ (PLOT_UNIT) PLOT_HEADER(I)
        END DO
      END IF

C   Set plot dimensions if AUTO

      IF (XLEN1.EQ.0.0 .OR. YLEN1.EQ.0.0) THEN
        CALL SXGDEVINFO (XSIZED, YSIZED)
        XLEN1 = XSIZED - XMARGIN
        YLEN1 = YSIZED - YMARGIN
      END IF

      IAXIS = 0
      IF (QUICK_PLOT) IAXIS = 1
      READ(PLOT_UNIT) ICONT
      DO WHILE (ICONT)
        READ(PLOT_UNIT) NPTS2, NQUAD2, ICHAR1, ITIP1, IPEN1, MASK2,
     &                  NMASK2, XFAC2, IWEIGHT, ICOLOR, BADVAL, TOPSCAL,
     &                  OSCFREQ, DOPPFAC

        NDAT = 0
        DO NQ = 1,NQUAD2
          NDAT = NDAT+NPTS2(NQ)
        END DO

        READ (PLOT_UNIT) (XSCALE(J),J=1,NDAT)
        READ (PLOT_UNIT) (DATA(J),J=1,NDAT)

        CALL DRAW_PLOT (IAXIS, DATA, XSCALE, XW1, YW1, ITIP1,
     &                  IPEN1, IWEIGHT, ICOLOR, BADVAL)
        READ(PLOT_UNIT) ICONT
        IAXIS = 2
      END DO

C     Draw axes etc if required

      TOPSCAL = TOPSCAL .AND. OSCFREQ.NE.0.D0
      IF (TOPSCAL) THEN
        A0 = 2.*OSCFREQ/DOPPFAC
        A1 = -1.0
      END IF

      IF (.NOT.QUICK_PLOT) THEN
        CALL DRAW_AXES (PLOT_HEADER, SHOWHD, XTITLE2, YTITLE2,
     &                  XW1, YW1, XLEN1, YLEN1, CHARHT1, LWEIGHT,
     &                  1, TOPSCAL, A0, A1)
      END IF

* ----------------

   99 IF(.not.IOPEN) THEN
        CLOSE (PLOT_UNIT)
        STATUS = IFREELUN (PLOT_UNIT)
      END IF

      RETURN
      END

C-----------------------------------------------------------------------
