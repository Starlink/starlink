C# IL>= a, OL>= 0
      SUBROUTINE GKXDWC(NC, ITXT, XBVCH, YBVCH, XHWFD, XHWCHR)
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
*     A utility driver to draw text in char. precision.
*
*  MAINTENANCE LOG
*  ---------------
*     09/11/83  FY    Original version stabilized
*     30/11/83  AS    Check KERROR after calling GKXFD
*     19/01/84  PB    Use further hardware characteristics
*     01/02/84  PB    String precision or SW not available
*                     set QFHT to normalised char height
*                     more parameters to gkxpcx & gkxpxo
*     03/04/84  MGC   Suppress partially clipped characters
*     04/04/84  MGC   Rotate box with HT not RHT
*     19/01/87  RMK   IS conversion. Changed "-1" to KNIL.
*     10/12/91  KEVP  Removed unused variable (C91).
*
*  ARGUMENTS
*  ---------
*     INP NC      number of entries in ITXT
*     INP ITXT    integer array of character codes
*     INP XBVCH   Baseline vector of char extent
*     INP YBVCH   Baseline vector of char extent
*     INP XHWFD   Routine to obtain Hardware text details
*     INP XHWCHR  Routine to perform output of Hardware text
*
         INTEGER NC, ITXT(NC)
         REAL XBVCH, YBVCH
         EXTERNAL XHWFD, XHWCHR
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
*     I       loop index
*     IGCLP   clip status of the whole string
*     LCLIP   clip status of individual character
*     HT      floating version of hershy height
*     RHT     hardware character height in DC
*     RNHT    normalised hardware character height
*     RNBASE  normalised hardware character baseline
*     RNCAP   normalised hardware character capline
*     RNWD1   normalised width of 1st character
*     RNWDM   normalised width of widest character
*     RWD     array of hardware character widths in DC
*     RWDMX   width of widest char in DC
*     WDC     width of current character
*     RNWDN   normalised width of last character
*     WDP     width of previous character
*     WD1   Theoretical width of first character
*     WDN   Theoretical width of last character
*     X       temporary variables denoting position coordinate
*     ,Y
*     XCBOX   character extent
*     ,YCBOX
*     XD      array of positions in DC (required for transformation)
*     ,YD
*     XTRN    translation vector
*     ,YTRN
*     XTX     text extent
*     ,YTX
*     XW      array of positions in WC (required for transformation)
*     ,YW
*     FLAG    true if hershy font available
*
      INTEGER IGCLP, LCLIP, I, J

      REAL    BCOS, BSIN, BX, BY, DIST, TH, TW,  WDC,
     :        WDP, X, Y, XCBOX(4), YCBOX(4), XD(2), YD(2),
     :        XTRN, YTRN, XTX(4), YTX(4), XW(2), YW(2),
     :        QCHHX, QCHHY, RWDMX, RHT, WD1, WDN, RNWDM
      REAL    HT, RNHT, RNBASE, RNCAP, RNWD1, RNWDN, RWD(95)

      LOGICAL FLAG
*
*---------------------------------------------------------------------


* copy data from Common to local variables

      XW(1)  = QWR1
      YW(1)  = QWR2
      CALL GKTWD(1, XW, YW, XD, YD)

* obtain hardware data and normalise.
      CALL XHWFD(KWTXFI(KWKIX),RHT,RWDMX,
     :           RNBASE,RNCAP,RWD)
         QCHHX = QWCHHX(KWKIX)
         QCHHY = QWCHHY(KWKIX)
         HT = SQRT(QCHHX*QCHHX + QCHHY*QCHHY)
      RNHT = RHT/HT
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

         KURFON = KNIL
        QFHT = RNHT
        QFWDMX = RWDMX/HT
        QFYADJ = 0.0
        QFCAP = RNCAP
        QFBASE = RNBASE
        DO 4 I=1,KCHTOT
          QFWIDS(I) = RWD(I)/HT
    4   CONTINUE
      ENDIF

* get total height and width of text

      CALL GKXTHW(ITXT, NC, TH, TW)

* obtain pseudo text extent

      WD1 = QFWIDS(ITXT(1)-KBEGIN)
      WDN = QFWIDS(ITXT(NC)-KBEGIN)
*     HW char exp factor of 1.0 since already in HW widths
      CALL GKXPXO(TH, TW, WD1, WDN, RNWD1, RNWDN,RNWDM,
     :               RNHT, RNBASE, RNCAP,1.0, XTX, YTX)
* obtain text alignment

      CALL GKXPAL(XTX, YTX, 0.0, RNBASE, RNCAP, X, Y)
      XTRN = 0.0
      YTRN = 0.0
      CALL GKXTFP(QWCHWX(KWKIX), QWCHWY(KWKIX), QWCHHX(KWKIX),
     :               QWCHHY(KWKIX), XTRN, YTRN, X, Y)

* setup translation vector for first char.

      XTRN = XD(1) - X
      YTRN = YD(1) - Y

* transform text extent

      CALL GKXTFB(QWCHWX(KWKIX), QWCHWY(KWKIX), QWCHHX(KWKIX),
     :               QWCHHY(KWKIX), XTRN, YTRN, XTX, YTX)

* obtain global clip status : results are
*            0 : no clip
*            4 : totally clipped,
* other values : partially clipped.

      CALL GKXBCL(XTX, YTX, QWCLXL(KWKIX), QWCLXR(KWKIX),
     :               QWCLYB(KWKIX), QWCLYT(KWKIX), IGCLP)

      IF (IGCLP .NE. 4) THEN

* initailize for loop to draw all characters in text
* if there is at least one corner within clip rect.

        WDC = QFWIDS(ITXT(1)-KBEGIN)

        DO 200 I=1,NC

* Except for first time, calculate next char. Centre and setup
* new translation vector.

          IF (I .NE. 1) THEN
            WDP = WDC
            WDC = QFWIDS(ITXT(I)-KBEGIN)
            CALL GKXPCC(WDP, WDC, X, Y)
            CALL GKXTFP(QWCHWX(KWKIX), QWCHWY(KWKIX),QWCHHX(KWKIX),
     :                     QWCHHY(KWKIX), XTRN, YTRN, X, Y)
            XTRN = X
            YTRN = Y
          ENDIF

* unless the whole text extent is inside clip rectangle otherwise
* we have to test individual character extent is not clipped.

          IF (IGCLP .EQ. 0) THEN
            LCLIP = 0
          ELSE

* construct character extent.

            CALL GKXPCX(GRIGHT, WDC,RNWDM, RNHT, RNBASE, RNCAP,
     :                      1.0,XCBOX, YCBOX)

* translate and rotate box, do not use general text
* transformation matrix.

            DIST = SQRT(XBVCH*XBVCH + YBVCH*YBVCH)
            BSIN = YBVCH / DIST
            BCOS = XBVCH / DIST

            DO 100 J = 1,4
              BX = (XCBOX(J)*BCOS - YCBOX(J)*BSIN)*HT + XTRN
              BY = (XCBOX(J)*BSIN + YCBOX(J)*BCOS)*HT + YTRN
              XCBOX(J) = BX
              YCBOX(J) = BY
  100       CONTINUE

* finally test whether box is clipped or not

            CALL GKXBCL(XCBOX, YCBOX, QWCLXL(KWKIX), QWCLXR(KWKIX),
     :                     QWCLYB(KWKIX), QWCLYT(KWKIX), LCLIP)
          ENDIF

* draw character only if box not clipped

          IF (LCLIP.EQ.0) CALL XHWCHR(KWTXFI(KWKIX),ITXT(I),XTRN,YTRN)

  200   CONTINUE

      ENDIF
  999 CONTINUE

      END
