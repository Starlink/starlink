C# IL>=a, OL>=0
      SUBROUTINE GKXTHW(TEXT,NC,TH,TW)
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
*     get pseudo total height and width assuming font nominal height be 1.0
*
*  MAINTENANCE LOG
*  ---------------
*     10/03/83    FY  Original version stabilized
*     05/05/83    FY  replace path,cap,aoff by ip,dcap,ioff
*     01/02/84    PB  use QFHT for font height
*
*  ARGUMENTS
*  ---------
*     INP TEXT   integer array of char. codes
*     INP NC     number of char.
*     OUT TH     total height
*     OUT TW     total width
*
      REAL TH, TW
      INTEGER NC, TEXT(NC)
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
*     HT     nominal height
*
       INTEGER I
       REAL HT
*
*---------------------------------------------------------------------


      TW = 0.0
      HT = QFHT

      IF (KWTXP(KWKIX) .LE. GLEFT) THEN
* right or left
        DO 100 I = 1,NC
          TW = TW + QFWIDS(TEXT(I)-KBEGIN)
  100   CONTINUE
        TW = TW*QWCHXP(KWKIX) + (NC-1)*QWCHSP(KWKIX)
        TH = HT + QFCAP + QFBASE
      ELSE
        TW = QFWDMX * QWCHXP(KWKIX)
        TH = (HT+QFCAP+QFBASE)*NC + QWCHSP(KWKIX)*(NC-1)
      ENDIF

      END
