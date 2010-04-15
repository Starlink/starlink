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
*     17-Sep-1995 (rp):
*        Rationalize LCOLOR/ICOLOR and include files, test ICOLOR in range
*     07-Oct-1995 (rp):
*        Modify Y-axis autoscaling to exclude out-of-X-range values.
*     20 July 2000 (ajc):
*        Missing commas in FORMAT
*        Change TYPE * to PRINT *
*        Invalid X specifier
*        Unused in PLTBUF: VELDAT, IFREELUN
*               in PLTHEAD: I
*     07 Mar 2002 (rpt):
*        Change test USB/LSB to look at sign iffreq
C-------------------------------------------------------------------------

      SUBROUTINE PLTBUF (XSCALE, IFAIL)

C   Routine to write data in X-register of the stack, and the corresponding
C   X values, to a plot file. Also writes the other parameters of the plot
C   such as scales, titles etc.

      IMPLICIT  NONE

C     Formal parameters:

      REAL*4    XSCALE(*)
      INTEGER   IFAIL

C     Include files/common blocks

      INCLUDE   'NEWXY'
      INCLUDE   'NOKEEP'
      INCLUDE   'STACKCOMM'
      INCLUDE   'FLAGCOMM'

      INTEGER    PLOT_UNIT
      COMMON /PLTDEV/ PLOT_UNIT

C     Local variables:

      INTEGER   ITIP
      INTEGER   ISTAT
      INTEGER   IERR
      INTEGER   IWEIGHT
      INTEGER   ILYN, ILYU
      INTEGER   STATUS
      LOGICAL   ICONT
      LOGICAL   TOPSCAL
      REAL      EPS
      REAL      XP1, XP2
      REAL      YP1, YP2
      CHARACTER PLTNAM*40
      CHARACTER XTITLE*80
      CHARACTER YTITLE*34

C     Functions:

      INTEGER    GEN_ILEN
      INTEGER    IGETLUN

C  Ok, go...

C     Set plot parameters (line weight, title, histogram or no...)

      CALL GEN_GETI4 ('Line weight for plot?',
     &                 LWEIGHT, 'I2', IWEIGHT, ISTAT)

      CALL GEN_GETI4 ('Color for plot [1-15]?',
     &                 ICOLOR, 'I3', ICOLOR, ISTAT)
      IF (ABS(ICOLOR).lt.1 .or. ABS(ICOLOR).gt.15) THEN
        PRINT *, 'Colour must be in range [1--15]; using 1'
        ICOLOR = 1
      END IF

      PLTNAM = ITITLE

      IF (HISTOGRAM) THEN
        ITIP = 1
      ELSE
        ITIP = 0
      END IF

C   Form a title

      WRITE (ICHAR,1110) LSCAN, ITIME, IDATE, PLTNAM
 1110 FORMAT ('Scan ',I4,' ',A8,' ',A9,'  ',A40,'    *.')

C   Increment plot counter (old plot closed in calling routine).

      IPSEQ = IPSEQ+1
      IF (IPSEQ.GE.1000) IPSEQ=1

C   Open the plot file

      STATUS = 0
      STATUS = IGETLUN (PLOT_UNIT, 'pltbuf', .FALSE.)
      WRITE (FILNAM,'(''PLOT.''I3.3)') IPSEQ
      OPEN  (PLOT_UNIT,
     &       FILE   =  FILNAM,
     &       STATUS = 'UNKNOWN',
     &       ACCESS = 'SEQUENTIAL',
     &       FORM   = 'UNFORMATTED',
     &       IOSTAT =  IERR)
      IF(IERR.NE.0)   THEN
        CALL GEN_ERMSG (IERR)
        IFAIL = 40
        PRINT *,'Error opening plot file: file=', filnam
        RETURN
      END IF

      WRITE (6,'(/'' Plot opened; sequence no. '',I3.3)') IPSEQ

C   Set up the X-axis

      CALL SETXNEW (XSCALE, IFAIL)
      IF (IFAIL.NE.0) RETURN

C   X- and Y-axis labelling

      CALL MAKE_XTITLE (XTITLE)

      ILYN    = GEN_ILEN (YAXIS_NAME)
      ILYU    = GEN_ILEN (YAXIS_UNITS)
      YTITLE  = YAXIS_NAME(:ILYN) // '  (' // YAXIS_UNITS(:ILYU) // ')'

CD    PRINT *, ' -- pltbuf --'
CD    PRINT *, '    xtitle = ', xtitle
CD    PRINT *, '    ytitle = ', ytitle

C   Set up plot limits

      EPS = .0001*(XEND-XST)
      XP1 = XST  + EPS
      XP2 = XEND - EPS

      EPS = .0001*(YEND-YST)
      YP1 = YST  + EPS
      YP2 = YEND - EPS

      IF (ISETSC)   THEN                                     ! Automatic scaling
        IF (FREEX) CALL RANGEXY   (XSCALE, NQUAD, XP1, XP2, NINTSX,
     &                             1.0, BADPIX_VAL, IFAIL)
        IF (FREEY) CALL RANGEXY2D (XSCALE, XP1, XP2,
     &                             DATA,   NQUAD, YP1, YP2, NINTSY,
     &                             2.0, BADPIX_VAL, IFAIL)
      ELSE                                                   ! Manual scaling
        CHANGE_SCALES = .FALSE.
        IF (XP1.EQ.XP2 .OR. YP1.EQ.YP2) IFAIL = 41
      END IF

      IF (IFAIL.NE.0) RETURN

C   Write the plot description and first set of XY pairs to the file

      IPEN  = 0
      ICONT = .FALSE.
      WRITE (PLOT_UNIT) XLEN,   YLEN, CHARHT, ITICKX, ITICKY, XP1, XP2,
     &                  NINTSX, YP1,  YP2,    NINTSY, IDEV
      WRITE (PLOT_UNIT) XTITLE, YTITLE

      IF (SHOW_HEADER) THEN
        WRITE (PLOT_UNIT) .TRUE.
        CALL PLTHEAD (PLOT_UNIT)
      ELSE
        WRITE (PLOT_UNIT) .FALSE.
      END IF

      WRITE (PLOT_UNIT) ICONT

      TOPSCAL = (NXS.EQ.2) .AND. ABS_FREQ
      CALL PLTDAT (NPTS, NQUAD, ITIP, IWEIGHT, ICOLOR,
     &             JPLOT, TOPSCAL, XSCALE, DATA)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE PLTHEAD (PLOT_UNIT)

*  Routine to write header information out to the plot file

      IMPLICIT  NONE

*     Formal parameter:

      INTEGER*4 PLOT_UNIT

*     Include files

      INCLUDE 'STACKCOMM'
      INCLUDE 'FLAGCOMM'

*     Functions

      INTEGER*4 NTOT

*     Local variables

      INTEGER*4 IERR
      INTEGER*4 NQ
      REAL*4    XCEN
      REAL*4    XQCEN
      REAL*4    XNTT
      REAL*8    DFREST
      REAL*8    DFCEN
      REAL*8    DFINC
      CHARACTER IUT*4
      CHARACTER SB*3
      CHARACTER RASTRING*12
      CHARACTER DECSTRING*12
      CHARACTER PLOT_HEADER*80

*  Ok, go...

      IUT = ' '
      IF (IUTFLG.EQ.1)   IUT = '(UT)'
      WRITE  (PLOT_HEADER, 10) LSCAN, ITITLE, IDATE, ITIME, IUT
   10 FORMAT ('Scan ',I4,'  ',A26, '      Obs''d ',A9,' at ',A8,A4)
      WRITE  (PLOT_UNIT) PLOT_HEADER

      CALL DEG_TO_STRING (RA/15.0, RASTRING)
      CALL DEG_TO_STRING (DEC,     DECSTRING)
      WRITE  (PLOT_HEADER, 15, IOSTAT=IERR) RASTRING, DECSTRING,
     &                                      DRA, DDEC
   15 FORMAT ('Map centre: ',A12,' ',A12,'; ',
     &            'Offset(R,D): (',F8.1,1X,F8.1,') arcsec')
      WRITE  (PLOT_UNIT) PLOT_HEADER

      XNTT = FLOAT(INTT)/1000.
      SB = 'USB'
C      IF (JFCEN(1)/1E+06 .LT. LOFREQ(1)) SB = 'LSB'
      IF (IFFREQ(1) .LT. 0) SB = 'LSB'
      WRITE  (PLOT_HEADER, 20, IOSTAT=IERR) XNTT, EL, VLSR, SB
   20 FORMAT ('Int''n time: ',F8.2,' sec; ',
     &        'Elevation: ',F4.1,' deg; ',
     &        'Vlsr: ',F7.1,' km/s  ',A3)
      WRITE  (PLOT_UNIT) PLOT_HEADER

      WRITE  (PLOT_HEADER, 30)
   30 FORMAT ('Quad.  #pts. Cent.Ch  Rest Freq(GHz) Obs.Freq(GHz)',
     &          ' Inc.freq(MHz)   Tsys(K)')
      WRITE  (PLOT_UNIT) PLOT_HEADER

      NQ     = 1

      IF (FCEN(NQ).EQ.0.0) THEN
        DFREST = DFLOAT (JFREST(NQ))/1.E6
      ELSE
        DFREST = FCEN(NQ)
      END IF

      XQCEN  = FLOAT(NPTS(NQ)+1)/2.
      XCEN   = XQCEN+NTOT(NQ-1)
      DFCEN  = DFLOAT (JFCEN(NQ))/1.E6
      DFINC  = DFLOAT (JFINC(NQ))/1.E6

      WRITE (PLOT_HEADER, 40, IOSTAT=IERR) NQ, NPTS(NQ), XCEN,
     &                            DFREST, DFCEN, DFINC, TSYS(NQ)
   40 FORMAT (3X,I2,1X,I5,2X,F6.1,3X,F10.4,3X,F10.4,4X,G12.5,2X,G12.5)

      WRITE  (PLOT_UNIT) PLOT_HEADER

      RETURN
      END

*-----------------------------------------------------------------------
