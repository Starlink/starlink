C# IL>=a, OL>=0
      SUBROUTINE GKXPXC(TH,TW,WD1,WDN,XHTVEC,YHTVEC,XWDVEC,YWDVEC,
     :                     DH,XTX,YTX)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Find text extent for text drawn in char precision.
*
*  MAINTENANCE LOG
*  ---------------
*     09/11/83    FY  Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP TH     total height (base unit = 1)
*     INP TW     total width
*     INP WD1    width of first character
*     INP WDN    width of last character
*     INP XHTVEC transformed height vector
*     INP YHTVEC transformed height vector
*     INP XWDVEC transformed width vector
*     INP YWDVEC transformed width vector
*     INP DH     device height
*     OUT XTX    pseudo text extent to be returned
*     OUT YTX    pseudo text extent to be returned
*
      REAL TH, TW, WD1, WDN, XHTVEC, YHTVEC, XWDVEC, YWDVEC, DH,
     :     XTX(4), YTX(4)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkxfd.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*  LOCALS
*  ------
*     XVHT       inverse of htvec
*     YVHT       inverse of htvec
*     XVWD       inverse of wdvec
*     YVWD       inverse of wdvec
*     XFCBOX(4)  coordinates of first char. box
*     YFCBOX(4)  coordinates of first char. box
*     XLCBOX(4)  coordinates of last char. box
*     YLCBOX(4)  coordinates of last char. box
*     XTRN       translation(shift) vector
*     YTRN       translation(shift) vector
*     XLCC       x coordinate of last char. centre
*     YLCC       y coordinate of last char. centre
*     DELTA
*
      REAL XVHT,YVHT,XVWD,YVWD,XFCBOX(4),YFCBOX(4),XLCBOX(4),YLCBOX(4),
     :     XTRN,YTRN,XLCC,YLCC,DELTA
*
*---------------------------------------------------------------------



* calculate inverse of height and width vector, modified by device
* height.

      DELTA = XWDVEC*YHTVEC - XHTVEC*YWDVEC
      XVWD = DH*YHTVEC/DELTA
      YVWD = -DH*YWDVEC/DELTA
      XVHT = -DH*XHTVEC/DELTA
      YVHT = DH*XWDVEC/DELTA

* obtain char. extent of first and last char.
      CALL GKXPCX(KWTXP(KWKIX),WD1,XFCBOX,YFCBOX)
      CALL GKXPCX(KWTXP(KWKIX),WDN,XLCBOX,YLCBOX)

* NOTE: first char. centre is always 0,0
* find centre of last char.
      IF (KWTXP(KWKIX).EQ.GRIGHT) THEN
        XLCC = TW - (WD1+WDN)*QWCHXP(KWKIX)/2.0
        YLCC = 0.0
      ELSEIF (KWTXP(KWKIX).EQ.GLEFT) THEN
        XLCC = (WD1+WDN)*QWCHXP(KWKIX)/2.0 - TW
        YLCC = 0.0
      ELSEIF (KWTXP(KWKIX).EQ.GUP) THEN
        XLCC = 0.0
        YLCC = TH - 1.0 - QFCAP - QFBASE
      ELSEIF (KWTXP(KWKIX).EQ.GDOWN) THEN
        XLCC = 0.0
        YLCC = 1.0 + QFCAP + QFBASE - TH
      ENDIF

* inverse transform first char. box

      XTRN = 0.0
      YTRN = 0.0
      CALL GKXTFB(XVWD,YVWD,XVHT,YVHT,XTRN,YTRN,XFCBOX,YFCBOX)

* inverse transform last char. box
      XTRN = XLCC
      YTRN = YLCC
      CALL GKXTFB(XVWD,YVWD,XVHT,YVHT,XTRN,YTRN,XLCBOX,YLCBOX)

* obtain pseudo text extent
      XTX(1) = AMIN1(XFCBOX(1),XFCBOX(2),XFCBOX(3),XFCBOX(4),
     :               XLCBOX(1),XLCBOX(2),XLCBOX(3),XLCBOX(4))
      XTX(2) = AMAX1(XFCBOX(1),XFCBOX(2),XFCBOX(3),XFCBOX(4),
     :               XLCBOX(1),XLCBOX(2),XLCBOX(3),XLCBOX(4))
      XTX(3) = XTX(2)
      XTX(4) = XTX(1)
      YTX(1) = AMIN1(YFCBOX(1),YFCBOX(2),YFCBOX(3),YFCBOX(4),
     :               YLCBOX(1),YLCBOX(2),YLCBOX(3),YLCBOX(4))
      YTX(2) = YTX(1)
      YTX(3) = AMAX1(YFCBOX(1),YFCBOX(2),YFCBOX(3),YFCBOX(4),
     :               YLCBOX(1),YLCBOX(2),YLCBOX(3),YLCBOX(4))
      YTX(4) = YTX(3)

      END
