C# IL>=a, OL>=0
      SUBROUTINE GKDTXB
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
*     Handle text attribute settings and derivations.
*
*  MAINTENANCE LOG
*  ---------------
*     02/11/83  AS    Hack FY's PERQ workstation independent version
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gaspct.par'
      INCLUDE '../include/gkwdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gktxb.cmn'
      INCLUDE '../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
      INTEGER I
*
*---------------------------------------------------------------------


* Find text bundle for this index and workstation
      DO 10 I=1,KMXTXB
        IF (KTXI(I,KWKIX) .EQ. KITXI) GOTO 20
        IF (KTXI(I,KWKIX) .EQ. KNIL) GOTO 15
   10 CONTINUE
* Not found: use bundle 1
   15 CONTINUE
      I = 1
   20 CONTINUE

* Fetch font and precision either from bundle or communication area
* depending on aspect flag.
      IF (KITXAF(KTXFNA) .EQ. GBUNDL) THEN
        KWTXFN(KWKIX) = KTXFN(I,KWKIX)
        KWTXPR(KWKIX) = KTXPR(I,KWKIX)
      ELSE
        KWTXFN(KWKIX) = KITXFN
        KWTXPR(KWKIX) = KITXPR
      ENDIF

* Fetch character expansion factor
      IF (KITXAF(KCHXPA) .EQ. GBUNDL) THEN
        QWCHXP(KWKIX) = QCHXP(I,KWKIX)
      ELSE
        QWCHXP(KWKIX) = QICHXP
      ENDIF

* Similarly for character spacing
      IF (KITXAF(KCHSPA) .EQ. GBUNDL) THEN
        QWCHSP(KWKIX) = QCHSP(I,KWKIX)
      ELSE
        QWCHSP(KWKIX) = QICHSP
      ENDIF

* Similarly for colour index
      IF (KITXAF(KTXCIA) .EQ. GBUNDL) THEN
        KWTXCI(KWKIX) = KTXCI(I,KWKIX)
      ELSE
        KWTXCI(KWKIX) = KITXCI
      ENDIF

* Transfer text path
      KWTXP(KWKIX) = KITXP

* Transfer text alignment and resolve 'normal'
      KWHTXA(KWKIX) = KIHTXA
      KWVTXA(KWKIX) = KIVTXA
      IF (KIHTXA .EQ. GAHNOR) THEN
        IF (KWTXP(KWKIX).EQ.GRIGHT) THEN
          KWHTXA(KWKIX) = GALEFT
        ELSEIF (KWTXP(KWKIX).EQ.GLEFT) THEN
          KWHTXA(KWKIX) = GARITE
        ELSE
          KWHTXA(KWKIX) = GACENT
        ENDIF
      ENDIF
      IF (KIVTXA .EQ. GAVNOR) THEN
        IF (KWTXP(KWKIX).EQ.GDOWN) THEN
          KWVTXA(KWKIX) = GATOP
        ELSE
          KWVTXA(KWKIX) = GABASE
        ENDIF
      ENDIF

* Transform height and width vectors
      CALL GKTWDV(QICHWX,QICHWY,QWCHWX(KWKIX),QWCHWY(KWKIX))
      CALL GKTWDV(QICHHX,QICHHY,QWCHHX(KWKIX),QWCHHY(KWKIX))

* Let font index be the font number, workstation can reset it.
      KWTXFI(KWKIX) = KWTXFN(KWKIX)

      END
