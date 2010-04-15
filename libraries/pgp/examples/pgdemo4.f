      PROGRAM PGDEM4
C-----------------------------------------------------------------------
C Test program for PGPLOT: test of imaging routine PGGRAY (with
C PGCONT). This program serves to both demonstrate and test PGGRAY.
C It computes an (arbitrary) function on a 2D array, and uses both
C PGGRAY and PGCONT to display it. An irregular transformation (TR
C matrix) is used, to test (a) that the routine works when the array
C pixels are not aligned with the device pixels, and (b) that the
C image is clipped correctly at the edge of the viewport. The program
C also draws the bounding quadrilateral of the contour map. The contours
C should end on this quadrilateral, but note that the grayscale image
C extends one half pixel beyond this quadrilateral (if the subarray to
C be displayed contains N pixels in one dimension, the width of the
C image is N units, while the width of the contour map is N-1 pixels).
C-----------------------------------------------------------------------
      INTEGER PGBEG
      INTEGER   MXI, MXJ
      PARAMETER (MXI=40, MXJ=40)
      INTEGER I,J
      REAL F(MXI,MXJ)
      REAL ANGLE,FMIN,FMAX,ALEV,RADIUS,TR(6)
C
C Open device for graphics.
C
      IF (PGBEG(0,'?',1,1) .NE. 1) STOP
C
C Compute a suitable function.
C
      FMIN = F(1,1)
      FMAX = F(1,1)
      DO 20 I=1,MXI
          DO 10 J=1,MXJ
              F(I,J) = COS(0.6*SQRT(I*80./MXI)-16.0*J/(3.*MXJ))*
     :                 COS(16.0*I/(3.*MXI))+(I/FLOAT(MXI)-J/FLOAT(MXJ))
              FMIN = MIN(F(I,J),FMIN)
              FMAX = MAX(F(I,J),FMAX)
   10     CONTINUE
   20 CONTINUE
C
C Compute the coordinate transformation matrix.
C ANGLE is the image rotation angle in radians.
C
      ANGLE=-120./57.29578
      RADIUS= 40.*SQRT(2.)
      TR(1) =-(40./MXI)*COS(ANGLE)+(40./MXJ)*SIN(ANGLE)-
     :         RADIUS*COS(ANGLE+45/57.29578)
      TR(2) = 80.*COS(ANGLE)/MXI
      TR(3) =-80.*SIN(ANGLE)/MXJ
      TR(4) =-(40./MXI)*SIN(ANGLE)-(40./MXJ)*COS(ANGLE)-
     :         RADIUS*SIN(ANGLE+45/57.29578)
      TR(5) = 80.*SIN(ANGLE)/MXI
      TR(6) = 80.*COS(ANGLE)/MXJ
C-----------------------------------------------------------------------
C PGGRAY
C-----------------------------------------------------------------------
C Clear the screen. Set up window and viewport.
C
      CALL PGPAGE
      CALL SETVP
      CALL PGWNAD(-40., 40.,-40., 40.)
      CALL PGSCI(1)
C
C Draw the map with PGGRAY.
C
      CALL PGGRAY(F,MXI,MXJ,1,MXI,1,MXJ,FMAX,FMIN,TR)
C
C Overlay contours in red.
C
      CALL PGSCI(2)
      DO 30 I=1,21
          ALEV = FMIN + (I-1)*(FMAX-FMIN)/20.0
          IF (MOD(I,5).EQ.0) THEN
              CALL PGSLW(3)
          ELSE
              CALL PGSLW(1)
          END IF
          IF (I.LT.10) THEN
              CALL PGSLS(2)
          ELSE
              CALL PGSLS(1)
          END IF
          CALL PGCONT(F,MXI,MXJ,1,MXI,1,MXJ,ALEV,-1,TR)
   30 CONTINUE
      CALL PGSLS(1)
      CALL PGSLW(1)
C
C Annotate the plot.
C
      CALL PGSCI(2)
      CALL OUTLIN(1,MXI,1,MXJ,TR)
      CALL PGSCI(5)
      CALL PGMTXT('t',1.0,0.0,0.0,'Routines PGGRAY, PGCONT, PGWEDG')
      CALL PGBOX('bcnts',0.0,0,'bcnts',0.0,0)
C
C Draw a wedge.
C
      CALL PGSCH(0.8)
      CALL PGWEDG('BG', 3.0, 4.0, FMAX, FMIN, 'Elevation')
      CALL PGSCH(1.0)
C-----------------------------------------------------------------------
C PGIMAG
C-----------------------------------------------------------------------
C Clear the screen. Set up window and viewport.
C
      CALL PGPAGE
      CALL SETVP
      CALL PGWNAD(-40., 40.,-40., 40.)
      CALL PGSCI(1)
C
C Set up the color map.
C
      CALL PALETT(2)
C
C Draw the map with PGIMAG.
C
      CALL PGIMAG(F,MXI,MXJ,1,MXI,1,MXJ,FMIN,FMAX,TR)
C
C Overlay contours in white.
C
      CALL PGSCI(1)
      DO 40 I=1,21
          ALEV = FMIN + (I-1)*(FMAX-FMIN)/20.0
          IF (MOD(I,5).EQ.0) THEN
              CALL PGSLW(3)
          ELSE
              CALL PGSLW(1)
          END IF
          IF (I.LT.10) THEN
              CALL PGSLS(2)
          ELSE
              CALL PGSLS(1)
          END IF
          CALL PGCONT(F,MXI,MXJ,1,MXI,1,MXJ,ALEV,-1,TR)
   40 CONTINUE
      CALL PGSLS(1)
      CALL PGSLW(1)
C
C Annotate the plot.
C
      CALL PGSCI(2)
      CALL OUTLIN(1,MXI,1,MXJ,TR)
      CALL PGSCI(5)
      CALL PGMTXT('t',1.0,0.0,0.0,'Routines PGIMAG, PGCONT, PGWEDG')
      CALL PGBOX('bcnts',0.0,0,'bcnts',0.0,0)
C
C Draw a wedge.
C
      CALL PGSCH(0.8)
      CALL PGWEDG('BI', 3.0, 4.0, FMIN, FMAX, 'Elevation')
      CALL PGSCH(1.0)
C-----------------------------------------------------------------------
C Close the device and exit.
C
      CALL PGEND
      END

      SUBROUTINE OUTLIN(I1,I2,J1,J2,TR)
      INTEGER I1,I2,J1,J2
      REAL TR(6)
C-----------------------------------------------------------------------
C Draw the enclosing rectangle of the subarray to be contoured,
C applying the transformation TR.
C
C For a contour map, the corners are (I1,J1) and (I2,J2); for
C a gray-scale map, they are (I1-0.5,J1-0.5), (I2+0.5, J2+0.5).
C-----------------------------------------------------------------------
      INTEGER K
      REAL XW(5), YW(5), T
C
      XW(1) = I1
      YW(1) = J1
      XW(2) = I1
      YW(2) = J2
      XW(3) = I2
      YW(3) = J2
      XW(4) = I2
      YW(4) = J1
      XW(5) = I1
      YW(5) = J1
      DO 10 K=1,5
          T = XW(K)
          XW(K) = TR(1) + TR(2)*T + TR(3)*YW(K)
          YW(K) = TR(4) + TR(5)*T + TR(6)*YW(K)
   10 CONTINUE
      CALL PGLINE(5,XW,YW)
      END

      SUBROUTINE PALETT(TYPE)
C-----------------------------------------------------------------------
C Set a "palette" of colors in the range of color indices used by
C PGIMAG.
C-----------------------------------------------------------------------
      INTEGER TYPE
C
      REAL GL(2), GR(2), GG(2), GB(2)
      REAL RL(9), RR(9), RG(9), RB(9)
      REAL HL(5), HR(5), HG(5), HB(5)
      REAL WL(10), WR(10), WG(10), WB(10)
      REAL AL(20), AR(20), AG(20), AB(20)
      REAL TL(4), TR(4), TG(4), TB(4)
C
      DATA GL /0.0, 1.0/
      DATA GR /0.0, 1.0/
      DATA GG /0.0, 1.0/
      DATA GB /0.0, 1.0/
C
      DATA RL /-0.5, 0.0, 0.17, 0.33, 0.50, 0.67, 0.83, 1.0, 1.7/
      DATA RR / 0.0, 0.0,  0.0,  0.0,  0.6,  1.0,  1.0, 1.0, 1.0/
      DATA RG / 0.0, 0.0,  0.0,  1.0,  1.0,  1.0,  0.6, 0.0, 1.0/
      DATA RB / 0.0, 0.3,  0.8,  1.0,  0.3,  0.0,  0.0, 0.0, 1.0/
C
      DATA HL /0.0, 0.2, 0.4, 0.6, 1.0/
      DATA HR /0.0, 0.5, 1.0, 1.0, 1.0/
      DATA HG /0.0, 0.0, 0.5, 1.0, 1.0/
      DATA HB /0.0, 0.0, 0.0, 0.3, 1.0/
C
      DATA WL /0.0, 0.5, 0.5, 0.7, 0.7, 0.85, 0.85, 0.95, 0.95, 1.0/
      DATA WR /0.0, 1.0, 0.0, 0.0, 0.3,  0.8,  0.3,  1.0,  1.0, 1.0/
      DATA WG /0.0, 0.5, 0.4, 1.0, 0.0,  0.0,  0.2,  0.7,  1.0, 1.0/
      DATA WB /0.0, 0.0, 0.0, 0.0, 0.4,  1.0,  0.0,  0.0, 0.95, 1.0/
C
      DATA AL /0.0, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.4, 0.5,
     :         0.5, 0.6, 0.6, 0.7, 0.7, 0.8, 0.8, 0.9, 0.9, 1.0/
      DATA AR /0.0, 0.0, 0.3, 0.3, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0,
     :         0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0/
      DATA AG /0.0, 0.0, 0.3, 0.3, 0.0, 0.0, 0.0, 0.0, 0.8, 0.8,
     :         0.6, 0.6, 1.0, 1.0, 1.0, 1.0, 0.8, 0.8, 0.0, 0.0/
      DATA AB /0.0, 0.0, 0.3, 0.3, 0.7, 0.7, 0.7, 0.7, 0.9, 0.9,
     :         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0/
C
      DATA TL /0.0, 0.5, 0.5, 1.0/
      DATA TR /0.2, 0.6, 0.6, 1.0/
      DATA TG /0.0, 0.0, 0.5, 1.0/
      DATA TB /1.0, 0.0, 0.0, 0.0/
C
      IF (TYPE.EQ.1) THEN
C        -- gray scale
         CALL PGCTAB(GL, GR, GG, GB, 2, 1.0, 0.5)
      ELSE IF (TYPE.EQ.2) THEN
C        -- rainbow
         CALL PGCTAB(RL, RR, RG, RB, 9, 1.0, 0.5)
      ELSE IF (TYPE.EQ.3) THEN
C        -- heat
         CALL PGCTAB(HL, HR, HG, HB, 5, 1.0, 0.5)
      ELSE IF (TYPE.EQ.4) THEN
C        -- weird IRAF
         CALL PGCTAB(WL, WR, WG, WB, 10, 1.0, 0.5)
      ELSE IF (TYPE.EQ.5) THEN
C        -- AIPS
         CALL PGCTAB(AL, AR, AG, AB, 20, 1.0, 0.5)
      ELSE IF (TYPE.EQ.6) THEN
C        -- TJP
         CALL PGCTAB(TL, TR, TG, TB, 4, 1.0, 0.5)
      END IF
      END

      SUBROUTINE SETVP
C-----------------------------------------------------------------------
C Set the viewport, allowing margins around the edge for annotation.
C (This is similar in effect to PGVSTD, but has different margins.)
C The routine determines the view-surface size and allocates margins
C as fractions of the minimum of width and height.
C-----------------------------------------------------------------------
      REAL D, VPX1, VPX2, VPY1, VPY2
C
      CALL PGSVP(0.0, 1.0, 0.0, 1.0)
      CALL PGQVP(1, VPX1, VPX2, VPY1, VPY2)
      D = MIN(VPX2-VPX1, VPY2-VPY1)/40.0
      VPX1 = VPX1 + 5.0*D
      VPX2 = VPX2 - 2.0*D
      VPY1 = VPY1 + 8.0*D
      VPY2 = VPY2 - 2.0*D
      CALL PGVSIZ(VPX1, VPX2, VPY1, VPY2)
      END
