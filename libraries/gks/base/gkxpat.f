C# IL>=a, OL>=0
      SUBROUTINE GKXPAT(TH,TW,PAX,PAY,ATX,ATY)
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
*     locate concatenation point
*
*  MAINTENANCE LOG
*  ---------------
*     10/03/83    FY  Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP TH     height of extent
*     INP TW     width of extent
*     INP PAX    x component of aligning point
*     INP PAY    y component of aligning point
*     OUT ATX    x component of concatenation point
*     OUT ATY    y component of concatenation point
*
      REAL TH,TW,PAX,PAY,ATX,ATY
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*---------------------------------------------------------------------


* it can be assumed that neither alignment is normal this implies that
* such cases should be resolved prior calling this routine.

      IF (KWTXP(KWKIX) .LE. GLEFT) THEN
        ATY = PAY
        IF (KWHTXA(KWKIX) .EQ. GALEFT) THEN
          ATX = PAX + TW + QWCHSP(KWKIX)
        ELSEIF (KWHTXA(KWKIX) .EQ. GARITE) THEN
          ATX = PAX - TW - QWCHSP(KWKIX)
        ELSE
          ATX = PAX
        ENDIF
      ELSE
        ATX = PAX
        IF (KWVTXA(KWKIX) .LE. GACAP) THEN
          ATY = PAY - TH - QWCHSP(KWKIX)
        ELSEIF (KWVTXA(KWKIX) .GT. GAHALF) THEN
          ATY = PAY + TH + QWCHSP(KWKIX)
        ELSE
          ATY = PAY
        ENDIF
      ENDIF

      END
