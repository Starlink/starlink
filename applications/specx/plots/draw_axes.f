C History:
C   01-Aug-1995 (rpt):
C    ICOLOR support added.
C
C-----------------------------------------------------------------------

      SUBROUTINE DRAW_AXES (PLOT_HEADER, SHOWHD,
     &                      XTITLE, YTITLE, XW1, YW1, XLEN1, YLEN1,
     &                      CHARHT1, IWEIGHT, ICOLOR, TOPSCAL, A0, A1)

      IMPLICIT   NONE


C   Arguments:
C
C  PLOT_HEADER ; Character array containing titles for plot
C  SHOWHD;       Logical, TRUE if full plot header is to be plotted
C  XTITLE;       Character variable holding 6-char title for x-axis
C  YTITLE;       Character variable, title for Y-axis
C  XW1;          Real, x offset to plot box in mm from left edge of display
C  YW1;          Real, y offset to plot box in mm from bottom on screen
C  XLEN1;        Real, length of x axis in mm
C  YLEN1;        Real, length of y axis in mm
C  CHARHT1;      Real, approx character height in mm
C  IWEIGHT;      Integer, weight of line to use for box, lettering etc
C  ICOLOR;       Color Index (0 (black), 1 (white) - 15)
C                   If color negative for color cycle (see draw_plot)
C                   it is ignored
C  TOPSCAL;      Logical, true if top scale bar different from bottom
C  A0, A1;       If TOPSCAL, linear transformation from bottom scale
C                -- thus x(top) = A0 + A1*x(bottom)

C     Formal parameters:

      CHARACTER   PLOT_HEADER(5)*80
      LOGICAL     SHOWHD
      CHARACTER   XTITLE*(*)
      CHARACTER   YTITLE*(*)
      REAL        XW1
      REAL        YW1
      REAL        XLEN1
      REAL        YLEN1
      REAL        CHARHT1
      INTEGER     IWEIGHT
      INTEGER     ICOLOR
      LOGICAL     TOPSCAL
      REAL        A0, A1

C     Local variables

      INTEGER     I
      REAL        STT1

C     Functions

      INTEGER     GEN_ILEN

C  Ok, go...

C     Draw the box:

      CALL SXGFONT    (1)
      CALL SXGEXPAND  (CHARHT1/3.)
      CALL SXGTICKSIZE(0., 0., 0., 0.)
      CALL SXGLTYPE   (0)
      CALL SXGLWEIGHT (IWEIGHT)
      IF ( ICOLOR .GE. 0 ) THEN
         CALL SXGSCI  (ICOLOR)
      ELSE
         CALL SXGSCI  (1)
      ENDIF
      CALL SXGBOX2    (1, 2, TOPSCAL, A0, A1)
      CALL SXGXLABEL  (XTITLE)
      CALL SXGYLABEL  (YTITLE)
      IF (TOPSCAL) CALL SXGXLABEL2 ('Image sideband frequency')

C  Scan header information

      STT1 = CHARHT1/1.8

      CALL SXGFONT    (1)
      CALL SXGEXPAND  (CHARHT1/3.)
      IF (SHOWHD) THEN
        DO I = 1, 5
          CALL SXGVLABEL(0.0, STT1*(3*(5-I)+1), PLOT_HEADER(I))
        END DO
      END IF

C  Plot ID

      CALL SXGEXPAND (1.0)
      IF (SHOWHD.AND..NOT.TOPSCAL) CALL SXGPLOTID (' ','Plot date')
      CALL SXGFONT   (1)

      RETURN
      END

C-----------------------------------------------------------------------

