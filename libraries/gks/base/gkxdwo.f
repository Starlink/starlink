C# IL>= a, OL>= 0
      SUBROUTINE GKXDWO(NC,ITXT,XDRWLN)
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
*     A driver for drawing text in stroke precision
*
*  MAINTENANCE LOG
*  ---------------
*     07/11/83  FY    Original version stabilized
*     30/11/83  AS    Check KERROR after calling GKXFD
*     12/01/84  RSK   Replaced KWTXP(KWKIX) with GRIGHT, in GKXPCX
*                     calls
*     19/01/84  PB    Passed hershey details down to lower level
*                     routines as parameters
*     01/02/84  PB    Passed more hershey details down to lower level
*                     routines as parameters
*     19/01/87  KEVP  Modified routine to enable it to perform
*                     polylines of more than 2 points in drawing
*                     hershey characters (S24).
*     22/04/87  KEVP  Use stack for polylines
*     15/06/87  RMK   Added check of KERROR after GKLUMP call and
*                     moved GKSTDA call inside LCLIP.NE.4 block.
*     04/11/87  RMK   Changed setting of READY to ensure that GKLCLP
*                     isn't called with an empty stroke (S293).
*     22/07/88  KEVP  Replaced IFIX with machine independent INT.
*     22/07/90  PLP   Removed unused local variables XLAST, YLAST.
*
*  ARGUMENTS
*  ---------
*     INP NC      number of entries in ITXT
*     INP ITXT    integer array of character codes
*     INP XDRWLN  Line drawing routine
*
         INTEGER NC, ITXT(NC)
         EXTERNAL XDRWLN
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkxfd.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     DONE    indicate end of stroke defn.
*     HT      nominal font height ( cap to base distance )
*     I       loop index
*     IGCLP   clip status of the whole string
*     IOFF    offset of stack allocation in stack
*     ISX
*     ,ISY    Hershey coordinates
*     LCLIP   clip status of individual char.
*     MFRAC   maximum fraction of stack used (1/MFRAC)
*     MXCONT  maximum number of points in a continuous stroke
*     MXNP    maximum number of points for device polyline
*     NCONT   number of points counted in continuous stroke
*     NP      number of points for device Polyline
*     READY   indicate polyline ready for drawing
*     RNYJ    normalized YADJ
*     WD1     width of 1st char.
*     WDC     width of current char.
*     WDN     width of last character
*     WDP     width of previous char.
*     X,Y     temporary vars. denoting character displacement vector
*     XCBOX   char. extent
*     ,YCBOX
*     XD,YD   array of positions in DC (reqd' for tran. rout.)
*     XS,YS   current font stroke
*     XTRN    translation vector
*     ,YTRN
*     XTX     text extent
*     ,YTX
*     XW,YW   array of positions in WC (reqd' for tran. rout.)
*     XI,YI   integerised translation vector
*
      LOGICAL DONE, READY

      INTEGER IGCLP, IS, LCLIP, I, ISX(KMXSTK), ISY(KMXSTK),
     :        NP, IOFF, MFRAC, MXCONT, MXNP, NCONT

      REAL HT, RNYJ, TH, TW, WD1, WDC, WDN, WDP, X, Y,
     :     XCBOX(4), YCBOX(4), XD(2), YD(2), XS, YS,
     :     XTRN, YTRN, XTX(4),YTX(4), XW(2), YW(2), XI, YI

*
*  STACK USAGE
*  -----------
*     2*MXNP  REAL   used to store polyline arrays

*---------------------------------------------------------------------


*  Set Maximum Fraction of Stack to be used
*           ( this is 1/MFRAC )

      MFRAC = 4
*     ie, up to a quarter of the stack may be used


* copy data from Common to local variables

      XW(1)  = QWR1
      YW(1)  = QWR2
      CALL GKTWD(1, XW, YW, XD, YD)

* get stroke font details

      IF (KWTXFN(KWKIX).NE.KURFON) CALL GKXFD
      IF (KERROR.NE.0) GOTO 999

      HT = FLOAT(KFHGT)
      RNYJ = QFYADJ/HT

* obtain character width of first and last character

      WD1 = QFWIDS(ITXT(1)-KBEGIN)
      WDN = QFWIDS(ITXT(NC)-KBEGIN)

* get total height and width

      CALL GKXTHW(ITXT, NC, TH, TW)

* obtain untransformed text extent

      CALL GKXPXO(TH, TW, WD1, WDN, WD1, WDN,QFWDMX,
     :               1.0, QFBASE, QFCAP,QWCHXP(KWKIX), XTX, YTX)

* obtain text alignment

      CALL GKXPAL(XTX, YTX, RNYJ, QFBASE, QFCAP, X, Y)
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

* test clipping, results are
*            0 : not clipped
*            4 : totally clipped
* other values : partially clipped

      CALL GKXBCL(XTX, YTX, QWCLXL(KWKIX), QWCLXR(KWKIX),
     :               QWCLYB(KWKIX), QWCLYT(KWKIX), IGCLP)
      IF (IGCLP .EQ. 4) GOTO 999

* if not completely clipped, setup loop to draw chars.

      WDC = WD1

      DO 200 I = 1, NC

* Except for first time find the character jump and
* increment translation vector by transformed jump.

        IF (I .NE. 1) THEN
          WDP = WDC
          WDC = QFWIDS(ITXT(I)-KBEGIN)
          CALL GKXPCC(WDP, WDC, X, Y)
          CALL GKXTFP(QWCHWX(KWKIX), QWCHWY(KWKIX), QWCHHX(KWKIX),
     :                   QWCHHY(KWKIX), XTRN, YTRN, X, Y)
          XTRN = X
          YTRN = Y
        ENDIF

* integerise translation vector

      XI = FLOAT(INT(XTRN)) + 0.5
      YI = FLOAT(INT(YTRN)) + 0.5

* unless no part of text extent is clipped, otherwise has to test
* individual char. extent to see whether it is clipped or not

        IF (IGCLP .EQ. 0) THEN
          LCLIP = 0
        ELSE
* obtain char. extent and transform it
          CALL GKXPCX(GRIGHT, WDC,QFWDMX, 1.0 ,QFBASE, QFCAP,
     :                   QWCHXP(KWKIX),XCBOX, YCBOX)
          CALL GKXTFB(QWCHWX(KWKIX), QWCHWY(KWKIX), QWCHHX(KWKIX),
     :                   QWCHHY(KWKIX), XTRN, YTRN, XCBOX, YCBOX)
* test clip
          CALL GKXBCL(XCBOX, YCBOX, QWCLXL(KWKIX), QWCLXR(KWKIX),
     :                   QWCLYB(KWKIX), QWCLYT(KWKIX), LCLIP)
        ENDIF

* char. not drawn if completely clipped

        IF (LCLIP .NE. 4) THEN

* get stroke definition

          CALL GKXSS(ITXT(I), ISX, ISY)
          IS = 1
          READY = .FALSE.
          NP = 0
          DONE = .FALSE.

*         Allocate Stack Space for a Contiguous part of the
*         the Stroke definition.
*
*         This will be for the maximum contiguous stroke
*         in the stroke definition,
*         provided its does not exceed 1/MFRAC of the stack space.

          NCONT = 1
          MXCONT = 2
          DO 95 IS=1,KMXSTK
            IF (ISX(IS) .EQ. KFMARK ) THEN
              IF( NCONT .GT. MXCONT ) MXCONT = NCONT
              IF( ISY(IS) .EQ. KFMARK ) GOTO 96
              NCONT = 0
            ENDIF
            NCONT = NCONT + 1
 95       CONTINUE
 96       CALL GKLUMP ( KREALS, 2, MXCONT, 2, MFRAC, MXNP, IOFF )
          IF (KERROR.NE.0) GOTO 999
          IS = 1

* repeat (until end of stroke definitions)
  100     CONTINUE

          IF (ISX(IS) .NE. KFMARK) THEN
            XS = ISX(IS)*QWCHXP(KWKIX) / HT
            YS = (ISY(IS) + QFYADJ) / HT
* transform point
            CALL GKXTFP(QWCHWX(KWKIX), QWCHWY(KWKIX), QWCHHX(KWKIX),
     :                     QWCHHY(KWKIX), XI, YI, XS, YS)
            NP = NP + 1
            QSTACK(IOFF+NP-1) = XS
            QSTACK(IOFF+NP+MXNP-1) = YS
*
*  If NP reaches MXNP  end polyline preparation
            IF( NP .EQ. MXNP ) THEN
              READY = .TRUE.
              IS = IS - 1
            ENDIF
* come here if ISX(IS) = KFMARK
          ELSEIF (ISY(IS) .EQ. 0) THEN
            READY = .TRUE.
          ELSEIF (ISY(IS) .EQ. KFMARK) THEN
            DONE = .TRUE.
            READY = NP.NE.0
          ENDIF
* clip and draw.

          IF( READY ) THEN
            CALL GKLCLP(NP, QSTACK(IOFF), QSTACK(IOFF+MXNP),
     :                       .FALSE., 1.0,
     :                       QWCLXL(KWKIX), QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                       QWCLYT(KWKIX), XDRWLN)
            READY = .FALSE.
            NP = 0
          ENDIF
* update stroke pointer
          IS = IS + 1

          IF (.NOT. DONE) GOTO 100
* Until end of stroke definition

* Release Stack Space

          CALL GKSTDA ( KREALS, IOFF )

        ENDIF

  200 CONTINUE

  999 CONTINUE
      END
