*  History:
*     01-Aug-1995 (rpt):
*       ICOLOR support added.
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Declare RANGE external for Linux
C-----------------------------------------------------------------------

      SUBROUTINE DRAW_PLOT (IAXIS, DATA, XSCALE, XW1, YW1,
     &                      ITIP, IPEN, IWEIGHT, ICOLOR, BADPIX_VAL)

      IMPLICIT   NONE

C  Arguments:
C
C       IAXIS      : = 0 for new axes, plotted
C                      1 for new axes, not plotted
C                      2 to use old axes.
C       DATA       : Array holding y-values of data pairs
C       XSCALE     : Array holding x-values of points in data array
C       ITIP       : = 1 for histogram
C                      0 for line plot
C       IPEN       : No of pen to be used for plot
C       IWEIGHT    : Line weight for plot
C       ICOLOR     : Color Index (0 (black), 1 (white) - 15)
C                      if ICOLOR is negative, its absolute
C                      value is the start color of a color
C                      cycle over quadrants
C       BADPIX_VAL : Magic value for plots.

      EXTERNAL RANGE

      INTEGER   IAXIS
      REAL      DATA(*)
      REAL      XSCALE(*)
      REAL      XW1, YW1
      INTEGER   ITIP
      INTEGER   IPEN
      INTEGER   IWEIGHT
      INTEGER   ICOLOR
      REAL      BADPIX_VAL

C     Common blocks/global variables

      INCLUDE  'PLOTPAR1'
      INCLUDE  'SPECX_PARS'

      INTEGER   NPTS
      INTEGER   NQUAD
      INTEGER   MASK
      REAL      XFAC
      INTEGER   NMASK
      CHARACTER XTITLE*80
      CHARACTER YTITLE*34

      COMMON /FREQ2/ NPTS(8), NQUAD,  MASK(8), XFAC(8),
     &               NMASK,   XTITLE, YTITLE

C     Local variables:

      INTEGER   I1,     I2
      INTEGER   ICOL
      INTEGER   J
      INTEGER   NPPTS
      INTEGER   NQ
      INTEGER   NQ1,    NQ2
      INTEGER   NST
      INTEGER   NDAT
      REAL      XPLOT(LSPMAX+1), YPLOT(LSPMAX+1)
      REAL      XMIN,   XMAX
      REAL      YMIN,   YMAX
      REAL      XXMIN,  XXMAX
      REAL      YZERO(2)

      DATA YZERO /2*0.0/

C  Ok, go...

C     Define Axes

      CALL SXGVWINDOW (XW1, XW1+XLEN1, YW1, YW1+YLEN1)
      CALL SXGLIMITS  (XST1, XEND1, YST1, YEND1)

C     Draw in zero level

      IF(IAXIS.EQ.0 .OR. IAXIS.EQ.1)   THEN
        CALL SXGLWEIGHT (1)
        CALL SXGLTYPE   (1)
        NQ1=NQUAD                                  ! All this nonsense is necessary
        NQ2=1                                      ! because NPTS may not be the
        DO NQ=1,NQUAD                              ! same for the plot and the
          IF(MASK(NQ).EQ.1) NQ2=NQ                 ! current data
          IF(MASK(NQUAD+1-NQ).EQ.1) NQ1=NQUAD+1-NQ !
        END DO                                     !
        NST=0                                      !
        NDAT=0                                     !
        DO NQ=1,NQ2                                !
          IF(MASK(NQ).EQ.1) THEN                   !
            NDAT=NDAT+NPTS(NQ)                     !
          ELSE                                     !
            NST=NST+NPTS(NQ)                       !
          END IF                                   !
        END DO                                     !
        CALL RANGE  (XSCALE(NST+1), NDAT, XPLOT(1), XPLOT(2))
        CALL SXGCONNECT (XPLOT,YZERO,2)
      END IF

C     Select Pen

      ICOL = ABS(ICOLOR)
      CALL SXGLTYPE   (IPEN)
      CALL SXGLWEIGHT (IWEIGHT)
      CALL SXGSCI     (ICOL)

C     Establish windowing limits for data

      XMIN = MIN (XST1, XEND1)
      XMAX = MAX (XST1, XEND1)
      YMIN = MIN (YST1, YEND1)
      YMAX = MAX (YST1, YEND1)

      XXMIN = XMIN - 50.*(XMAX-XMIN)
      XXMAX = XMAX + 50.*(XMAX-XMIN)

C     Do plots,each quadrant.....

      NST = 0
      DO NQ = 1,NQUAD
        IF (MASK(NQ).NE.0)   THEN

*          PRINT *, 'Quadrant # = ', NQ

C         Find first good point in data

          I1 = 1

          DO WHILE (I1.LE.NPTS(NQ) .AND. DATA(NST+I1).EQ.BADPIX_VAL)
            I1 = I1 + 1
          END DO

C         Slice out successive stretches of good data, make into
C         histogram if wanted, and plot

          DO WHILE (I1.LE.NPTS(NQ))

            I2 = I1
            DO WHILE (I2.LT.NPTS(NQ) .AND. DATA(NST+I2+1).NE.BADPIX_VAL)
              I2 = I2 + 1
            END DO

            NPPTS = I2+1-I1

*           PRINT *, 'Data stretch: ', I1, ' to ', I2,
*    &              '(length=', NPPTS, ')'

C           Copy data (with window that preserves magic values)

            IF (NPPTS.GE.1) THEN

              DO J = 1, NPPTS
                XPLOT(J) = MAX (MIN (XSCALE(NST+I1+J-1),XXMAX), XXMIN)
                YPLOT(J) = MAX (MIN (DATA(NST+I1+J-1),  YMAX),  YMIN)
                IF (DATA(NST+I1+J-1) .EQ. BADPIX_VAL) THEN
                  YPLOT(J) = BADPIX_VAL
                END IF
              END DO

              IF (ITIP.NE.0)   THEN
                IF (NPPTS.GT.2048)   THEN
                  PRINT *,'***  Too many points for histogram ***'
                ELSE
                  CALL MKHIS (XPLOT, YPLOT, NPPTS, XFAC(NQ))
                END IF
              END IF

C             Plot and go back for more

*             PRINT *, 'Final value of NPPTS = ', NPPTS

              CALL SXGCONNECT (XPLOT, YPLOT, NPPTS)
            END IF

C           Find start of next good data stretch

            I1 = I2 + 1
            DO WHILE (I1.LE.NPTS(NQ) .AND. DATA(NST+I1).EQ.BADPIX_VAL)
              I1 = I1 + 1
            END DO

          END DO
        END IF
        NST = NST + NPTS(NQ)
        IF (ICOLOR .LT. 0) THEN
          ICOL = 1 + MOD(ICOL,15)
          CALL SXGSCI(ICOL)
        ENDIF
      END DO

      RETURN
      END

