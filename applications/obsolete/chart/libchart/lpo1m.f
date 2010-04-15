      SUBROUTINE LPO1M(X,Y, STATUS )

C
C      To calculate guide stars for the 1 metre camera
C      input values of X,Y from CHART are in units of minutes of
C      arc. ThE factor 2.326 converts them to mm at the f/8 focus.
C      The radial minimum is governed by the prism blocking the
C      on-axis beam and the radial maximum by the size of the
C      useful field. This is not a circle hence the slightly
C      different values given for XYMAX. The axis is given by
C      X=100 and Y=100. This ensures that guide star coordinates
C      do not go negative within the range of the prism.
C
C      R.W.Argyle, RGO  1984 January
C
*   Arguments:
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   History:
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
C

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      DOUBLE PRECISION X,Y
      DATA RADMIN,RADMAX,XYMAX1,XYMAX2/16.0,96.0,186.0,196.0/
      DATA XYMIN/116.0/
      XS= REAL( X )
      YS= REAL( Y )
      X1=2.326*XS+100.0
      Y1=2.326*YS+100.0
      X100=X1-100.0
      Y100=Y1-100.0
      R=SQRT(X100*X100+Y100*Y100)
C
C      CHECK QUADRANT
C
      IF(X1.LT.0.0.OR.Y1.LT.0.0) GO TO 100
      IF(X1.GT.100.0.AND.Y1.GT.100.0) GO TO 10
      IF(X1.LT.100.0.AND.Y1.LT.100.0) GO TO 20
      IF(X1.GT.100.0.AND.Y1.LT.100.0) GO TO 40
      GO TO 30
C
C      QUADRANT O DEGREES
C
   10 IF(X1.LT.XYMIN.AND.Y1.LT.XYMIN) GO TO 100
      IF(X1.GT.XYMAX2.OR.Y1.GT.XYMAX1) GO TO 100
      X100=X1-100.0
      Y100=Y1-100.0
      R=SQRT(X100*X100+Y100*Y100)
      IF(R.LT.RADMIN.OR.R.GT.RADMAX) GO TO 100
      IPA=0
      XF=X1
      YF=Y1
      GO TO 200
C
C      QUADRANT 180 DEGREES
C
  20  X3=200.0-X1
      Y3=200.0-Y1
      Y100=Y3-100.0
      X100=X3-100.0
      R=SQRT(X100*X100+Y100*Y100)
      IF(R.LT.RADMIN.OR.R.GT.RADMAX) GO TO 100
      IF(X3.LT.XYMIN.AND.Y3.LT.XYMIN) GO TO 100
      IF(X3.GT.XYMAX2.OR.Y3.GT.XYMAX1) GO TO 100
      IPA=180
      XF=X3
      YF=Y3
      GO TO 200
C
C      QUADRANT 90 DEGREES
C
   30 X2=Y1
      Y2=200.0-X1
      X100=X2-100.0
      Y100=Y2-100.0
      R=SQRT(X100*X100+Y100*Y100)
      IF(R.LT.RADMIN.OR.R.GT.RADMAX) GO TO 100
      IF(X2.LT.XYMIN.AND.Y2.LT.XYMIN) GO TO 100
      IF(X2.GT.XYMAX1.OR.Y2.GT.XYMAX2) GO TO 100
      IPA=90
      XF=X2
      YF=Y2
      GO TO 200
C
C      QUADRANT 270 DEGREES
C
   40 X4=200.0-Y1
      Y4=X1
      X100=X4-100.0
      Y100=Y4-100.0
      R=SQRT(X100*X100+Y100*Y100)
      IF(R.LT.RADMIN.OR.R.GT.RADMAX) GO TO 100
      IF(X4.LT.XYMIN.AND.Y4.LT.XYMIN) GO TO 100
      IF(X4.GT.XYMAX1.OR.Y4.GT.XYMAX2) GO TO 100
      IPA=270
      XF=X4
      YF=Y4
      GO TO 200
 200  IX=NINT(XF)
      IY=NINT(YF)
      WRITE(7,910) IX,IY,IPA
  910 FORMAT('+',71X,2(I4,2X),I3)
      GO TO 300
  100 WRITE(7,900)
  900 FORMAT('+',72X,'-',5X,'-')
  300 RETURN
      END
