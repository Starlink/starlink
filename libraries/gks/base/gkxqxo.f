C# IL>=a, OL>=0
      SUBROUTINE GKXQXO(NC, ITXT, XTX, YTX)
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
*     Inquire text entent for text in Stroke precision
*
*  MAINTENANCE LOG
*  ---------------
*     09/11/83    FY  Original version stabilized
*     30/11/83    AS  Check KERROR after call to GKXFD
*     19/01/84    PB  pass font details to lower routines
*                     as parameters
*     01/02/84    PB  pass more font details to lower routines
*                     as parameters
*
*  ARGUMENTS
*  ---------
*     INP NC      number of entries in ITXT
*     INP ITXT    integer arrary of character codes
*     OUT XTX     x points of extent
*     OUT YTX     y points of extent
*
      INTEGER NC, ITXT(NC)
      REAL XTX(4), YTX(4)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkxfd.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     WD1     width of 1st char.
*     WDN     width of last char.
*     X       temp. var. for x coordinate
*     Y       temp. var. for y coordinate
*     RNYJ    normalized yadj
*     XTRN    translation vector
*     YTRN
*     PAX     x comp. of pseudo align. point
*     PAY     y comp. of pseudo align. point
*     ATX     concatenation point (x)
*     ATY     concatenation point (y)
*
      REAL WD1, WDN, X, Y, RNYJ, TH, TW, XTRN, YTRN, PAX, PAY, ATX, ATY
*
*---------------------------------------------------------------------


* get stroke font details
      IF (KWTXFN(KWKIX).NE.KURFON) CALL GKXFD
      IF (KERROR.NE.0) GOTO 999

      RNYJ = QFYADJ/FLOAT(KFHGT)

* obtain character width of first and last character

      WDN = QFWIDS(ITXT(NC)-KBEGIN)
      WD1 = QFWIDS(ITXT(1)-KBEGIN)

* get total height and width
      CALL GKXTHW(ITXT, NC, TH, TW)

* get pseudo text extent
      CALL GKXPXO(TH, TW, WD1, WDN, WD1, WDN,QFWDMX,
     :               1.0, QFBASE, QFCAP,QWCHXP(KWKIX),XTX,YTX)

* obtain text alignment
      CALL GKXPAL(XTX, YTX, RNYJ, QFBASE, QFCAP, X, Y)
      PAX = X
      PAY = Y
      XTRN = 0.0
      YTRN = 0.0
      CALL GKXTFP(QWR3, QWR4, QWR5, QWR6, XTRN, YTRN, X, Y)

* obtain concatenation point
      CALL GKXPAT(TH, TW, PAX, PAY, ATX, ATY)

* setup translation vector for first char.
      XTRN = QWR1 - X
      YTRN = QWR2 - Y

* transform text extent
      CALL GKXTFB(QWR3, QWR4, QWR5, QWR6, XTRN, YTRN, XTX, YTX)

* transform concatenation point
      CALL GKXTFP(QWR3, QWR4, QWR5, QWR6, XTRN, YTRN, ATX, ATY)

      QWR7 = ATX
      QWR8 = ATY

  999 CONTINUE
      END
