C# IL>= a, OL>= 0
         SUBROUTINE GKXQXC(NC, ITXT, XBVCH, YBVCH, XTX, YTX,
     :                        XHWFD)
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
*     Inquire text extent and concatenation point for text in CHAR
*     precision.
*
*  MAINTENANCE LOG
*  ---------------
*     08/11/83    FY  Original version stabilized
*     30/11/83    AS  Check KERROR after call to GKXFD
*     19/01/84    PB  added font details as parameters
*     25/01/84    PB  use hardware details if no hershy equivalent
*     01/02/84    PB  String precision or SW not available
*                     set QFHT to normalised char height

*
*  ARGUMENTS
*  ---------
*     INP NC       number of entries in ITXT
*     INP ITXT     integer array of character codes
*     INP XHWFD   Routine to obtain Hardware text details
*     INP XBVCH    baseline vector of hardware char
*     INP YBVCH    baseline vector of hardware char
*     OUT XTX      x points of text extent
*     OUT YTX      y points of text extent
*
         INTEGER NC, ITXT(NC)
         REAL XBVCH,YBVCH, XTX(4), YTX(4)
         EXTERNAL XHWFD
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkxfd.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     ATX,
*      ATY
*     PAX,    text alignment
*      PAY
*     TH      Total height of text
*     TW      Total width of text
*     RNHT    normalised character height
*     RNBASE  normalised base
*     RNCAP   normalised cap
*     RNWD1   normalised width of 1st character
*     RNWDN   normalised width of last character
*     RWD     array of hardware character widths in DC
*     RWDMX   width of widest char in DC
*     RNWDM   normalised width of widest character
*     WD1   Theoretical width of first character
*     WDN   Theoretical width of last character
*     X,Y     temporary vars. denoting position coordinate
*     XTRN    translation vector
*     ,YTRN
*     HT       nominal height of hershy character
*     FLAG    true if hershy font available
*
      INTEGER I

      REAL    ATX, ATY, PAX, PAY, TH, TW, HT,
     :        QCHHX,QCHHY, RWDMX, WD1, WDN, RNWDM
      REAL    RNHT, RNBASE, RNCAP, RNWD1, RNWDN
      REAL    X, Y, XTRN, YTRN, RWD(95)

      LOGICAL FLAG
*
*---------------------------------------------------------------------


* obtain hardware data and normalise.

      CALL XHWFD(KWTXFI(KWKIX),RNHT,RWDMX,
     :           RNBASE,RNCAP,RWD)
         QCHHX = QWCHHX(KWKIX)
         QCHHY = QWCHHY(KWKIX)
         HT = SQRT(QCHHX*QCHHX + QCHHY*QCHHY)

      RNHT = RNHT/HT
      RNBASE = RNBASE/HT
      RNCAP = RNCAP/HT
      RNWD1 = RWD(ITXT(1)-KBEGIN)/HT
      RNWDN = RWD(ITXT(NC)-KBEGIN)/HT
      RNWDM = RWDMX/HT

* check if have corresponding hershy font
* and set up font details

      FLAG = .FALSE.
      IF (KWTXPR(KWKIX) .NE. GSTRP) THEN
        IF (KWTXFN(KWKIX).NE.KURFON) CALL GKXFD
        DO 1 I=1,KFNTMX
          IF (KWTXFN(KWKIX).EQ.KHFONT(I)) FLAG = .TRUE.
    1   CONTINUE
      ENDIF
      IF(.NOT.FLAG) THEN
* no hershy font so use hardware details

        KURFON = -1
        QFHT = RNHT
        QFWDMX = RWDMX/HT
        QFYADJ = 0.0
        QFCAP = RNCAP
        QFBASE = RNBASE
        DO 4 I=1,KCHTOT
          QFWIDS(I) = RWD(I)/HT
    4   CONTINUE
      ENDIF


* get total height and width
      CALL GKXTHW(ITXT, NC, TH, TW)

* get pseudo text extent
      WD1 = QFWIDS(ITXT(1)-KBEGIN)
      WDN = QFWIDS(ITXT(NC)-KBEGIN)
*     HW char exp factor of 1.0 since already in HW widths
      CALL GKXPXO(TH,TW, WD1, WDN, RNWD1, RNWDN,RNWDM,
     :               RNHT, RNBASE, RNCAP,1.0, XTX, YTX)

* obtain text alignment
      CALL GKXPAL(XTX, YTX, 0.0 ,RNBASE,RNCAP, X, Y)
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

* compensate for some of the rotational deficiencies
* of hardware characters

* if text upside down
* and HW text not

      IF ((QWR6.LT.0).AND.(XBVCH.GT.0)) THEN
        YTX(1) = YTX(1) + RNBASE
        YTX(2) = YTX(2) + RNBASE
        YTX(3) = YTX(3) + RNBASE
        YTX(4) = YTX(4) + RNBASE
      ENDIF


* if >45 degree rotation
* and hardware rotation less

*     QWR6 = y of ht vector
*     QWR5 = x of ht vector
      IF ((ABS(QWR6).LT.2*ABS(QWR5)).AND.(ABS(XBVCH).GT.2*ABS(YBVCH)))
     :THEN
          XTX(1) = XTX(1) - RNBASE
          XTX(4) = XTX(4) - RNBASE
          XTX(2) = XTX(2) + RNBASE
          XTX(3) = XTX(3) + RNBASE
      ENDIF

* transform text extent
      CALL GKXTFB(QWR3, QWR4, QWR5, QWR6, XTRN, YTRN, XTX, YTX)

* transform concatenation point
      CALL GKXTFP(QWR3, QWR4, QWR5, QWR6, XTRN, YTRN, ATX, ATY)

      QWR7 = ATX
      QWR8 = ATY
  999 CONTINUE
      END
