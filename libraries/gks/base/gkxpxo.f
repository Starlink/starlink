C# IL>=a, OL>=0
      SUBROUTINE GKXPXO(TH,TW,THWD1,THWDN,WD1,WDN,WDM,
     :                     RNHT,RNBASE,RNCAP,CHXP,XTX,YTX)
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
*     calculate text extent of text in stroke precision
*
*  MAINTENANCE LOG
*  ---------------
*     16/03/83    FY  Original version stabilized
*     20/04/83    FY  split arrays with mixed x & y points
*     26/04/83    FY  change path,lccx,lccy to ip,xlcc,ylcc
*     29/04/83    FY  'I' specified twice
*     12/01/84   RSK  Replaced GRIGHT with KWTXP(KWKIX), in GKXPCX
*                     calls
*     19/01/84    PB  obtained font details from parameters
*     01/02/84    PB  more font details from parameters
*
*  ARGUMENTS
*  ---------
*     INP   TH     total height
*     INP   TW     total width
*     INP   WD1    width of first character
*     INP   WDN    width of last character
*     INP   WDM    width of widest character
*     INP   THWD1  Theoretical width of first character
*     INP   THWDN  Theoretical width of last character
*     INP   RNHT   normalised font height
*     INP   RNBASE normalised bottom to base
*     INP   RNCAP  normalised cap to top
*     INP   CHXP   hardware expansion factor
*     OUT   XTX    text extent
*     OUT   YTX    text extent
*
      REAL TH, TW, WD1, WDN, WDM, RNHT, RNBASE, RNCAP
      REAL THWD1, THWDN,CHXP, XTX(4), YTX(4)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkxfd.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*  LOCALS
*  ------
*     XFCBOX   extent of 1st character
*     YFCBOX   extent of 1st character
*     XLCBOX   extent of last character
*     YLCBOX   extent of last character
*     XLCC     x- centre of last character
*     YLCC     y- centre of first character
*
      INTEGER I
      REAL  XFCBOX(4), YFCBOX(4), XLCBOX(4), YLCBOX(4), XLCC, YLCC
*
*---------------------------------------------------------------------



* obtain extent of 1st and last char.
      CALL GKXPCX(KWTXP(KWKIX),WD1,WDM,RNHT,RNBASE,RNCAP,CHXP,
     :               XFCBOX,YFCBOX)
      CALL GKXPCX(KWTXP(KWKIX),WDN,WDM,RNHT,RNBASE,RNCAP,CHXP,
     :               XLCBOX,YLCBOX)

* find centre of last char.
      IF (KWTXP(KWKIX).EQ.GRIGHT) THEN
        XLCC = TW - (THWD1+THWDN)*QWCHXP(KWKIX)/2.0
        YLCC = 0.0
      ELSEIF (KWTXP(KWKIX).EQ.GLEFT) THEN
        XLCC = (THWD1+THWDN)*QWCHXP(KWKIX)/2.0 - TW
        YLCC = 0.0
      ELSEIF (KWTXP(KWKIX).EQ.GUP) THEN
        XLCC = 0.0
        YLCC = TH - QFHT - QFCAP - QFBASE
      ELSEIF (KWTXP(KWKIX).EQ.GDOWN) THEN
        XLCC = 0.0
        YLCC = QFHT + QFCAP + QFBASE - TH
      ENDIF

* translate last char. extent, not needed for first
* char because first char. centre is at (0,0)

      DO 70 I=1,4
        XLCBOX(I) = XLCBOX(I) + XLCC
        YLCBOX(I) = YLCBOX(I) + YLCC
   70 CONTINUE


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
