C# IL>=a, OL>=0
      SUBROUTINE GKXPCC(WP,WC,XC,YC)
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
*     obtain pseudo character centre
*
*  MAINTENANCE LOG
*  ---------------
*     10/03/83    FY  Original version stabilized
*     20/04/83    FY  rename variables
*     01/02/84    PB  use QFHT for font height
*
*  ARGUMENTS
*  ---------
*     INP WP     width of previous char.
*     INP WC     width of current char.
*     OUT XC     x component of char. centre
*     OUT YC     y component of char. centre
*
      REAL WP,WC,XC,YC
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
      REAL HT
*
*---------------------------------------------------------------------


      HT = QFHT
      IF (KWTXP(KWKIX).EQ.GRIGHT) THEN
        XC = (WP+WC)*QWCHXP(KWKIX)/2.0 + QWCHSP(KWKIX)
        YC = 0.0
      ELSEIF (KWTXP(KWKIX).EQ.GLEFT) THEN
        XC = -( (WP+WC)*QWCHXP(KWKIX)/2.0 + QWCHSP(KWKIX))
        YC = 0.0
      ELSEIF (KWTXP(KWKIX).EQ.GUP) THEN
        XC = 0.0
        YC = HT + QFCAP + QFBASE + QWCHSP(KWKIX)
      ELSEIF (KWTXP(KWKIX).EQ.GDOWN) THEN
        XC = 0.0
        YC = -(HT + QFCAP + QFBASE + QWCHSP(KWKIX))
      ENDIF

      END
