C# IL>=a, OL>=0
      SUBROUTINE GKXPCX(IP,WC9,WCM,RNHT,RNBASE,RNCAP,CHXP,XTX,YTX)
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
*     obtain char extent
*
*  MAINTENANCE LOG
*  ---------------
*     10/03/83    FY  Original version stabilized
*     20/04/83    FY  split arrays with mixed x & y points
*     27/04/83    FY  change path to ip
*     29/04/83    FY  WC changed to wc9, (wc is in par file)
*     19/01/84    PB  obtained font details from parameters
*     01/02/84    PB  obtained char exp factor & max width
*                     from parameters
*
*  ARGUMENTS
*  ---------
*     INP IP   path
*     INP WC9     width of current char.
*     INP WCM     width of widest char.
*     INP RNHT    normalised height of font
*     INP RNBASE  normalised bottom to base
*     INP RNCAP   normalised cap to top
*     INP CHXP    hardware char exp factor
*     OUT XTX    char. extent
*     OUT YTX
*
      REAL WC9,WCM,RNHT,RNBASE,RNCAP,XTX(4),YTX(4),CHXP
      INTEGER IP
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
*     HT     height
*     WD     width
*
      REAL HT,WD
*
*---------------------------------------------------------------------


      HT = RNHT
      IF (IP .LE. GLEFT) THEN
        WD = WC9
      ELSE
        WD = WCM
      ENDIF
      WD = WD * CHXP

      XTX(1) = -WD/2.0
      YTX(1) = -(HT/2.0 + RNBASE)
      XTX(2) = WD/2.0
      YTX(2) = -(HT/2.0 + RNBASE)
      XTX(3) = WD/2.0
      YTX(3) = HT/2.0 + RNCAP
      XTX(4) = -WD/2.0
      YTX(4) = HT/2.0 + RNCAP

      END
