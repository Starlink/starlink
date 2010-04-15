*    History:
*     01-Aug-1995 (rpt):
*        ICOLOR support added.
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused PLOTS, DOQUAD
C-----------------------------------------------------------------------

      SUBROUTINE PLOTXY (XSCALE, BUF, NCH, IFAIL)

C   Subroutine to plot data in DATA array against XSCALE for use in
C   finding baseline fit regions, etc.

      IMPLICIT NONE

C     Formal parameters:
      REAL     XSCALE(*)
      REAL     BUF(*)
      INTEGER  NCH
      INTEGER  IFAIL

C     Local variables:
      REAL      A0, A1
      REAL      XW1
      REAL      YW1
      REAL      YMARGIN
      REAL      XMARGIN
      REAL      XSIZED, YSIZED
      INTEGER   IAXIS
      INTEGER   ITIP
      INTEGER   ILYN
      INTEGER   ILYU
      LOGICAL   TOPSCAL
      CHARACTER PLOT_HEADER(5)*80

      INTEGER   NMASK1
      INTEGER   NPTS1
      INTEGER   NQUAD1
      INTEGER   MASK1
      REAL      EPS
      REAL      XFAC1

      CHARACTER        XTITLE1*80, YTITLE1*34
      COMMON /FREQ2/   NPTS1(8), NQUAD1,  MASK1(8), XFAC1(8),
     &                 NMASK1,   XTITLE1, YTITLE1

C     Include files:

      INCLUDE 'FLAGCOMM'
      INCLUDE 'PLOTPAR1'
      INCLUDE 'NEWXY'
      INCLUDE 'DOPPLER'

      REAL*4           PLOTS1(11)
      EQUIVALENCE      (PLOTS1,XLEN1)

C     Functions:
      INTEGER  GEN_ILEN

      DATA      XW1       /25.0/
      DATA      YW1       /50.0/
      DATA      YMARGIN   /70./
      DATA      XMARGIN   /30./
      DATA      PLOT_HEADER  /5*' '/
C  Ok, go...

      IFAIL=0

C   Copy relevant parameters to PLOTPAR1 and FREQ2

      XLEN1   = XLEN
      YLEN1   = YLEN
      CHARHT1 = CHARHT
      ITICKX1 = ITICKX
      ITICKY1 = ITICKY

      EPS = .0001*(XEND-XST)
      XST1    = XST + EPS
      XEND1   = XEND - EPS
      NINTSX1 = NINTSX

      EPS = .0001*(YEND-YST)
      YST1    = YST + EPS
      YEND1   = YEND - EPS
      NINTSY1 = NINTSY

      NQUAD1  = 1
      NPTS1(1)= NCH
      MASK1(1)= 1
      NQUAD1  = 1
      NMASK1  = NMASK

      CALL MAKE_XTITLE (XTITLE1)

      ILYN    = GEN_ILEN (YAXIS_NAME)
      ILYU    = GEN_ILEN (YAXIS_UNITS)
      YTITLE1 = YAXIS_NAME(:ILYN) // '  (' // YAXIS_UNITS(:ILYU) // ')'

C   Set up X-axis limits and plot limits if required,
C   else change axes if change in effect

      IF (.NOT.CHANGE_SCALES) THEN
        IF (ISETSC) THEN
          IF (FREEX) CALL SETRANGE (NCH, XSCALE, XST1, XEND1, NINTSX1,
     &                              1.0, BADPIX_VAL, IFAIL)
          IF (IFAIL.NE.0) THEN
            PRINT *, 'Error setting X-scale automatically'
            RETURN
          END IF

          IF (FREEY) CALL SETRANGE2D (NCH, XSCALE, BUF, BADPIX_VAL,
     &               XST1, XEND1, 1.3, NINTSY1, YST1, YEND1, IFAIL)
          IF (IFAIL.NE.0) THEN
            PRINT *, 'Error setting Y-scale automatically'
            RETURN
          END IF
        END IF

      ELSE
        XST1    = XXST
        XEND1   = XXEND
        YST1    = YYST
        YEND1   = YYEND
        NINTSX1 = NX
        NINTSY1 = NY
      END IF

C   Set plot dimensions if AUTO

      IF (XLEN1.EQ.0.0 .OR. YLEN1.EQ.0.0) THEN
        CALL SXGDEVINFO (XSIZED, YSIZED)
        XLEN1 = XSIZED - XMARGIN
        YLEN1 = YSIZED - YMARGIN
      END IF

C   Plot

      IPEN  = 0

      ITIP  = 0
      IF (HISTOGRAM)    ITIP = 1

      PLOT_HEADER(1)= 'Current contents of buffer'

      IAXIS = 0
      IF (QUICK_PLOT) IAXIS = 1
      CALL DRAW_PLOT (IAXIS, BUF, XSCALE, XW1, YW1, ITIP,
     &                IPEN, 1, ICOLOR, BADPIX_VAL)

      TOPSCAL = (NXS.EQ.2) .and. ABS_FREQ .and. OSCFREQ.ne.0.D0
      IF (TOPSCAL) THEN
        A0 = +2.0 * OSCFREQ/DOPPFAC
        A1 = -1.0
      END IF

      CALL DRAW_AXES (PLOT_HEADER, .TRUE., XTITLE1, YTITLE1,
     &                XW1, YW1, XLEN1, YLEN1, CHARHT1, 1, 1,
     &                TOPSCAL, A0, A1)

      RETURN
      END


