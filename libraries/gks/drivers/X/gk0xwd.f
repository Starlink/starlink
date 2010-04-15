      SUBROUTINE GK0XWD(IENT, NID, IDAT, NRD, RX, RY, NCD, STR)

*-----------------------------------------------------------------------
*
*  RAL GKS SYSTEM
*
*  Type of routine:  Workstation Driver
*  Authors:          TAW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Workstation driver to use XLib X11 release 2 and ww.
*
*  MAINTENANCE LOG
*  ---------------
*     20/09/88  TAW   Copied from ../sun/gk9swd.f and modified for XLib.
*     30/10/88  TAW   Implemented gk0xln.
*     25/11/88  TAW   Implemented colour, gk0xwc and gk0xcc.
*     30/11/88  TAW   Implemented hardware text routines gk0xxc and gk0xsf.
*     30/01/89  TAW   Implemented rasterop and fill area, gk0xfa_ & gk0xro_.
*     30/01/89  TAW   Changed polymarker and text output to save current line
*                     style and restore it after primitive and took out code
*                     to set them in the set bundle table entry.
*     23/08/89  RMK   Changed pattern index check in set fill area attributes
*                     to allow for user-defined patterns as well as
*                     predefined ones (S79).
*     31/01/90  RMK   Changed close wkstn entry to INQUIRE and CLOSE
*                     both files (S353).
*
*  ARGUMENTS
*  ---------
*     INP IENT  - Entrypoint code.
*     INP NID   - Size of array IDAT.
*     I/0 IDAT  - Integer data passed to or from workstation.
*     INP NRD   - Size of arrays RX and RY.
*     I/O RX    - Real X-coordinate data passed to or from workstation.
*     I/O RY    - Real Y-coordinate data passed to or from workstation.
*     INP NCD   - Size of character array.
*     I/O STR   - Character array.
*
      INTEGER IENT, NID, IDAT(NID), NRD, NCD
      REAL RX(NRD), RY(NRD)
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*  Read    /GKYFLS/  KDBFLS
*          /GKYHP/   KHPXR
*          /GKYWCA/  KCVIS, KNIR, KNRR, KWI1..3, KWKIX, KWKTYP, QWR1..3
*          /GKYWDT/  QNMMKS
*          /GKYWKD/  KWFAIS, KWLNTY, KWMKTY, KWTXPR, QWCHHX, QWCHHY,
*                    QWCHWX, QWCHWY, QWCLXL, QWCLXR, QWCLYB, QWCLYT,
*                    QWMKSZ
*          /GKYWSL/  KCTBPT, KNFAUP, KSSGPT
*          /GKYXFD/  KHFONT
*  Modify  /GKYERR/  KERROR
*          /GKYHP/   QHP
*          /GKYSTK/  QSTACK
*          /GKYWCA/  KRGN, KSGRQ, KWDONE, KWI1, KWRGN
*          /GKYWDT/  KPCI
*          /GKYWKD/  KWCID, KWFACI, KWFASI, KWIO, KWKDAT, KWPLCI, KWPMCI
*                    KWTXCI, KWTXFN, QWKDAT
*          /GKYWSL/  KCID, KDFM, KDSMT, KIMRGM
*
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkerr.par'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwdt.par'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkfls.par'
      INCLUDE '../../include/gkfls.cmn'
      INCLUDE '../../include/gkxfd.cmn'
*
*  EXTERNAL SUBROUTINES
*  --------------------
*     GK0XLN  Solid / styled line output routine.
*     GK0XRO  Raster output routine.
*     GK0XXF  Text details routine.
*     GK0XXC  Text output routine.
*     GK0XSF  Hardware font setting.
*     GK0XCM  Set X colourmap entry.
*     GK0XWC  Set WW foreground colour.
*     GK0XFA  Fill area routine.
*
      EXTERNAL GK0XLN, GK0XRO, GK0XXF, GK0XXC, GK0XSF, GK0XCM, GK0XWC
     :         GK0XFA
*
*  EXTERNAL FUNCTIONS
*  ------------------
*     FPUTC   UNIX library function for single byte output.
*     GKAN1   Converts integer ASCII code to native character.
*     GK0XRL  Request locator input.
*     GK0XRV  Request valuator input.
*     GK0XRC  Request choice input.
*     GK0XRT  Request string input.
*     GK0XRK  Request stroke input.
*
      INTEGER FPUTC, GK0XRL, GK0XRV, GK0XRC, GK0XRT, GK0XRK
      CHARACTER*1 GKAN1
*
*  LOCALS
*  ------
*     ICREAT  Function code for GK0XWW() - create window.
*     ISELCT  Function code for GK0XWW() - select window.
*     IDELET  Function code for GK0XWW() - delete window.
*     IUPDAT  Function code for GK0XWW() - update window.
*     ICOSET  Function code for GK0XCM() - set WW colour index.
*     ICOGET  Function code for GK0XCM() - get WW colour index.
*             - ICOSET and ICOGET equal WW's COSET and COGET.
*     ICHSIZ  Index in KWKDAT for index of raster font loaded.
*     ILEFT   Index in KWKDAT for display update area left bound.
*     IRIGHT  Index in KWKDAT for display update area right bound.
*     ITOP    Index in KWKDAT for display update area top bound.
*     IBOTT   Index in KWKDAT for display update area bottom bound.
*     ILNTY   Index in KWKDAT for line type to be drawn by GK0XLN.
*     ILNWD   Index on KWKDAT for line width to be drawn by GK0XLN.
*     IQPAX   Index in QWKDAT of X coordinate for INQUIRE PIXEL ARRAY.
*     IQPAY   Index in QWKDAT of Y coordinate for INQUIRE PIXEL ARRAY.
*     ICHHT   Index in QWKDAT for char. height of raster font loaded.
*     ICHWD   Index in QWKDAT for char. width of raster font loaded.
*     ICHCT   Index in QWKDAT for cap to top line distance of raster
*             font loaded.
*     ICHBB   Index in QWKDAT for base to bottom line distance of
*             raster font loaded.
*     ICHGS   Index in QWKDAT for offset of GKS baseline from SunView
*             baseline for the raster font loaded.
*     ICHUNK  Size of stack used for splitting polyline and polymarker.
*     MNFAHS  Minimum hatch style index.
*     MXFAHS  Maximum hatch style index.
*     MNLNTY  Minmum line type.
*     MXLNTY  Maximum line type.
*     MNPMTY  Minmum polymarker type.
*     MXPMTY  Maximum polymarker type.
*     PATLEN  Length of pattern for line types (value irrelevant).
*     I       Integer used as temporary / subsript / loop control.
*     N       Integer used as temporary count.
*     IOFF    Offset for stack.
*     IOS     Return status from Fortran I/O system and FPUTC (see above).
*     ISOP1   Status return for INQUIRE().
*     ISOP2   Status return for INQUIRE().
*     IPREC   Precision for each font (STRING, CHAR, STROKE).
*     INTA    Integer array with multiple uses:
*             - setting representations
*             - integer data return from GKQWDT()
*             - temporary storage for polyline attributes when used by text
*               and marker primitive.
*     REALA   Real array with multiple uses:
*             - testing colour representation
*             - real data return from GKQWDT()
*
*     Function code parameters
      INTEGER    ICOSET,    ICOGET
      PARAMETER (ICOSET= 2, ICOGET= 1)
      INTEGER   ICREAT,     ISELCT,     IDELET,     IUPDAT
      PARAMETER(ICREAT = 1, ISELCT = 2, IDELET = 3, IUPDAT = 4)

*     Integer workspace offset parameters
      INTEGER   ICHSIZ,     ILEFT,     IRIGHT,     ITOP,     IBOTT
      PARAMETER(ICHSIZ = 1, ILEFT = 2, IRIGHT = 3, ITOP = 4, IBOTT = 5)
      INTEGER   ILNTY,     ILNWD
      PARAMETER(ILNTY = 6, ILNWD = 7)

*     Real workspace offset parameters
      INTEGER   IQPAX,     IQPAY,     ICHHT,     ICHWD,     ICHCT
      PARAMETER(IQPAX = 1, IQPAY = 2, ICHHT = 3, ICHWD = 4, ICHCT = 5)
      INTEGER   ICHBB,     ICHGS
      PARAMETER(ICHBB = 6, ICHGS = 7)

*     Other parameters
      INTEGER   ICHUNK
      PARAMETER(ICHUNK = 1024)
      REAL      PATLEN
      PARAMETER(PATLEN = 0.0)
      INTEGER   MNFAHS,      MNLNTY,     MNPMTY
      PARAMETER(MNFAHS = -10, MNLNTY = 0, MNPMTY = 0)
      INTEGER   MXFAHS,      MXLNTY,     MXPMTY
      PARAMETER(MXFAHS = -1, MXLNTY = 5, MXPMTY = 5)

*        Backslash character, doubled for UNIX compilers.
      CHARACTER*1 BS
      PARAMETER( BS = '\\' )

*     Local variables
      INTEGER I, IOFF, IOS, N
      INTEGER IPREC(KFNTMX)
      INTEGER INTA(19)

      LOGICAL ISOP1, ISOP2

      REAL REALA(12)
*
*  STACK USAGE
*  -----------
*     POLYLINE and POLYMARKER for transformations.
*
*  HEAP USAGE
*  ----------
*     SET COLOUR REPRESENTATION for colour table.
*
*  STREAMS USED
*  ------------
*     (KCID(KWKIX) + 1) for MESSAGE.
*
*  ERRORS
*  ------
*
*        26  Specified workstation cannot be opened
*        93  Colour index is invalid
*       102  Generalised drawing primitive identifier is invalid
*       104  At least one open workstation is not able to generate the
*            specified generalized drawing primitive
*     -1042  Error while trying to open internal segment store
*
*  COMMENTS
*  --------
*     Both the ww window management toolkit and Xlib are used to
*     perform input and output for this driver.  As opening windows
*     isn't amongst GKIOOP()s functions,  this driver also manages
*     its own connections and is hence
*
*                             SYSTEM DEPENDANT
*
*-----------------------------------------------------------------------

* Conditional GOTO on entrypoint code.

      GOTO (       10,  20,  30,  40,  50,  60,  70,  80,9999,
     :       100, 110, 120, 130, 140, 150, 160, 170, 180, 190,
     :       200, 210, 220, 230, 240, 250, 260, 270, 280,9999,
     :      9999, 310, 320, 330,9999,9999,9999,9999,9999,9999,
     :      9999, 410, 410, 410, 410, 410, 410, 410, 410, 410,
     :       410, 410,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999, 610, 620, 630, 640, 650, 660, 670, 680, 690,
     :       700, 710, 720, 730, 740, 750, 760, 770, 780, 790,
     :       800, 810,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999, 910, 920, 930 ) IENT

      GOTO (1111,1210,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1111,1111,1111,1111,1111,1111,1111,1111,1380,1111,
     :      1111,1111,1111,1111,1111,1111,1460,1111,1111,1111,
     :      1111,9999,1111,9999,9999,9999,9999,9999,9999,9999,
     :      9999,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1111,1111,1111,1111,1111,1111,1111,1111,1111,1111,
     :      1111,1111,1111,1111,1111,1850,1111,1870,1880,1111,
     :      1111,1111,1111,1111,1111,1111,1111,9999,1111 ) IENT - 119

      GOTO 9999

* Open Workstation.
   10 CONTINUE
*     Set up workstation description table and workstation state list.
      CALL GKIWSL(KWKIX, KWKTYP)
      IF (KERROR.NE.0) GOTO 9999
*     Initialise state for workstation utilities.
      KCID(KWKIX) = KWI1
      KDFM(KWKIX) = GASAP
      KWIO(KWKIX) = GYES
      KIMRGM(KWKIX) = GSUPPD
*     Here we should call GKIOOP() to establish connections.  Instead
*     we do it locally as GKIOOP() can't handle SunOS requirements.
      IF (KCID(KWKIX).GT.KMNIO.AND.KCID(KWKIX).LT.KMXIO) THEN
*       Check the status of both streams.
      INQUIRE(UNIT = KCID(KWKIX), OPENED = ISOP1)
      INQUIRE(UNIT = (KCID(KWKIX) + 1), OPENED = ISOP2)
      IF((.NOT.ISOP1).AND.(.NOT.ISOP2)) THEN
*       Not using standard streams,  so open the output stream to
*       /dev/tty for messages.
      OPEN(UNIT = KCID(KWKIX) + 1, FILE = '/dev/tty')
      ELSE IF (KCID(KWKIX).NE.5.AND.(ISOP1.OR.ISOP2)) THEN
*         One or both streams open and it isn't streams 5 and/or 6.
        KERROR = 26
        GOTO 9999
      ENDIF
      ELSE
*       The output channel will collide with the runtime files
*       or the input channel will collide with stderr.
      KERROR = 21
      GOTO 9999
      ENDIF
*     Now create the workstation.
      CALL GK0XWW(ICREAT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
*     Initialise workspace variables.  Raster font size is set to an
*     impossible value so the first font raster selection will force a
*     load,  and the screen update area to an impossible rectangle so the
*     first primitive will always reset the update area.
      KWKDAT(ICHSIZ,KWKIX) = KNIL
      KWKDAT(ILEFT,KWKIX) = KDSRX(KWKIX) - 1
      KWKDAT(IRIGHT,KWKIX) = 0
      KWKDAT(ITOP,KWKIX) = KDSRY(KWKIX) - 1
      KWKDAT(IBOTT,KWKIX) = 0
*     Initialise colour table.
      DO 11, I = 0, KPCI(KWKIX) - 1
      CALL GK0XCM(ICOSET, I,
     :              QHP(KHPXR(KCTBPT(1,KWKIX))+I),
     :              QHP(KHPXR(KCTBPT(2,KWKIX))+I),
     :              QHP(KHPXR(KCTBPT(3,KWKIX))+I))
   11 CONTINUE
*     Set up workstation category and return.
      KWI1 = GOUTIN
      GOTO 9999

* Close workstation.
   20 CONTINUE
      IF (KWI1.EQ.1) THEN
        KWDONE = KRFUSE
      ELSE
        CALL GK0XWW(IDELET, KWCID(KWKIX))
        IF (KERROR.NE.0) GO TO 9999
        INQUIRE(UNIT = KCID(KWKIX), OPENED = ISOP1)
        INQUIRE(UNIT = (KCID(KWKIX)+1), OPENED = ISOP2)
        IF (ISOP1.AND.KCID(KWKIX).NE.5)
     :    CLOSE(UNIT = KCID(KWKIX), IOSTAT = IOS)
        IF (ISOP2.AND.(KCID(KWKIX)+1).NE.6)
     :    CLOSE(UNIT = (KCID(KWKIX)+1), IOSTAT = IOS)
        IF (IOS.NE.0) THEN
          KERROR = 25
          GOTO 9999
        ENDIF
        CALL GKCWSL(KWKIX)
        CALL GKSLDL(KSSGPT(KWKIX))
      ENDIF
      GOTO 9999

* Clear workstation.
   30 CONTINUE
      IF (KWI1.EQ.2) THEN
        KWDONE = KRFUSE
        CALL GKSLDL(KSSGPT(KWKIX))
      ENDIF
      GOTO 9999

* Redraw all segments on workstation.
   40 CONTINUE
      KWDONE = KRFUSE
      GOTO 9999

* Update workstation.
   50 CONTINUE
      KWDONE = KRFUSE
      GOTO 9999

* Set deferral state.
   60 CONTINUE
      KDFM(KWKIX) = KWI1
      KIMRGM(KWKIX) = KWI2
      IF (KWI1.EQ.GASAP) THEN
        KWIO(KWKIX) = GYES
*       Update window
      CALL GK0XWW(IUPDAT, KWCID(KWKIX))
      ELSE
        KWIO(KWKIX) = GNO
      ENDIF
      IF (KIMRGM(KWKIX).EQ.GALLOW.AND.KNFAUP(KWKIX).EQ.GYES.AND.
     :    KDSMT(KWKIX).EQ.GNEMPT) THEN
        KWRGN(KWKIX) = .TRUE.
        KRGN = .TRUE.
      ENDIF
      GOTO 9999

* Do deferred output actions.
   70 CONTINUE
*     Update window
      CALL GK0XWW(IUPDAT, KWCID(KWKIX))
      KWI1 = KNFAUP(KWKIX)
      GOTO 9999

* Clear display surface.
   80 CONTINUE
      IF (KWI1.EQ.GALWAY.OR.KDSMT(KWKIX).EQ.GNEMPT) THEN
        CALL GK0XWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
        CALL GK0XCD
      ENDIF
      CALL GKWCLD
      GOTO 9999

* Message.
  100 CONTINUE
      CALL FLUSH(KCID(KWKIX) + 1)
      DO 101, I = 1, NID
        IOS = FPUTC(KCID(KWKIX) + 1, GKAN1(IDAT(I)))
  101 CONTINUE
      IOS = FPUTC(KCID(KWKIX) + 1, BS\\'n')
      CALL FLUSH(KCID(KWKIX) + 1)
      GOTO 9999

* Escape [Not Implemented].
  110 CONTINUE
      KERROR = 180
      GOTO 9999

* Polyline.
  120 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
      CALL GKSTAL(KREALS, ICHUNK * 2, IOFF)
      IF (KERROR.EQ.0) THEN
        CALL GK0XWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
*       Set up device colour.
        CALL GK0XWC(KWPLCI(KWKIX))
*       Output line a chunk at a time. Use the line utility.
*       Transform world coordinates to device coordinates and
*       output clipped line a chunk at a time.
        N = ICHUNK
        DO 121 I = 1, NRD, ICHUNK - 1
          IF (NRD - I.LT.ICHUNK - 1) N = NRD - I + 1
          IF (N.EQ.1) GOTO 121
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF + ICHUNK))
*         Output clipped line.
        CALL GKLCLP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),.FALSE.,PATLEN,
     :                QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                QWCLYT(KWKIX),GK0XLN)
  121   CONTINUE
        CALL GKSTDA(KREALS,IOFF)
      ENDIF
      GOTO 8888

* Polymarker.
  130 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
*     Transform polymarker points and output them a chunk at a time.
      CALL GKSTAL(KREALS,ICHUNK * 2,IOFF)
      IF (KERROR.EQ.0) THEN
        CALL GK0XWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
*       Set up device colour, linestyle and width.
        CALL GK0XWC(KWPMCI(KWKIX))
        INTA(1) = KWKDAT(ILNTY,KWKIX)
        KWKDAT(ILNTY,KWKIX) = 1
        INTA(2) = KWKDAT(ILNWD,KWKIX)
        KWKDAT(ILNWD,KWKIX) = 0
        N = ICHUNK
        DO 131 I = 1, NRD, ICHUNK
          IF (NRD - I.LT.ICHUNK) N = NRD - I + 1
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF + ICHUNK))
*         Could change this later to draw first marker
*         and copy the others from it.
          CALL GKMTYP(N,QSTACK(IOFF),QSTACK(IOFF + ICHUNK),
     :                   KWMKTY(KWKIX),QNMMKS(KWKIX),QWMKSZ(KWKIX),
     :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK0XLN)
  131   CONTINUE
        CALL GKSTDA(KREALS,IOFF)
        KWKDAT(ILNTY,KWKIX) = INTA(1)
        KWKDAT(ILNWD,KWKIX) = INTA(2)
      ENDIF
      GOTO 8888

* Text.
  140 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
*     Select display surface, set up colour and solid unbroken line style.
      CALL GK0XWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
      CALL GK0XWC(KWTXCI(KWKIX))
      INTA(1) = KWKDAT(ILNTY,KWKIX)
      KWKDAT(ILNTY,KWKIX) = 1
      INTA(2) = KWKDAT(ILNWD,KWKIX)
      KWKDAT(ILNWD,KWKIX) = 0
*     Output text.
      IF(KWTXPR(KWKIX).EQ.GSTRKP) THEN
        CALL GKXDWO(NID, IDAT, GK0XLN)
      ELSE
        CALL GKXDCS(NID, IDAT, QWCHWX(KWKIX), QWCHWY(KWKIX),
     :              GK0XXF, GK0XXC)
      ENDIF
      KWKDAT(ILNTY,KWKIX) = INTA(1)
      KWKDAT(ILNWD,KWKIX) = INTA(2)
      GOTO 8888

* Fill Area.
  150 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
      CALL GK0XWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
*     Set up device colour.
      CALL GK0XWC(KWFACI(KWKIX))
*     Draw fill area.
      CALL GK0XFA(NRD, RX, RY)
      GOTO 8888

* Cell Array.
  160 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
      CALL GK0XWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
      CALL GKCELL(NID,IDAT,GK0XRO)
      GOTO 8888

* GDP.
  170 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
*     Check the GDP identifier
      IF (KWI1.EQ.0) THEN
        KERROR = 102
        GOTO 9999
      ELSE IF (KWI1.LT.-4.OR.KWI1.GT.-1) THEN
        KERROR = 104
        GOTO 9999
      ELSE
*       Select workstation
      CALL GK0XWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
*       Set up solid line.
      IF (KWI1.EQ.-1) THEN
*         Arc - use polyline colour index
        CALL GK0XWC(KWPLCI(KWKIX))
        CALL GKCRCS(KWI1,NRD,RX,RY,1,.TRUE.,PATLEN,GK0XLN,GK0XRO)
      ELSE
*         Filled chord, pie or circle - use fill area colour index
        CALL GK0XWC(KWFACI(KWKIX))
        CALL GKCRCS(KWI1,NRD,RX,RY,1,.TRUE.,PATLEN,GK0XLN,GK0XRO)
      ENDIF
      ENDIF
      GOTO 8888

* Set Polyline Attributes.
  180 CONTINUE
      CALL GKDPLB
*     Need to check because individual settings won't have been checked.
      IF (KWLNTY(KWKIX).LT.MNLNTY.OR.KWLNTY(KWKIX).GT.MXLNTY)
     :  KWLNTY(KWKIX) = 1
      IF (KWPLCI(KWKIX).GE.KPCI(KWKIX)) KWPLCI(KWKIX) = 1
*     Set up workspace variables for polyline device external.
      KWKDAT(ILNTY,KWKIX) = KWLNTY(KWKIX)
      KWKDAT(ILNWD,KWKIX) = MIN(KLNWD,INT(QWLNWD(KWKIX)))
      GOTO 9999

* Set Polymarker Attributes.
  190 CONTINUE
      CALL GKDPMB
*     Need to check because individual settings won't have been checked.
      IF (KWMKTY(KWKIX).LT.MNPMTY.OR.KWMKTY(KWKIX).GT.MXPMTY)
     :  KWMKTY(KWKIX) = 3
      IF (KWPMCI(KWKIX).GE.KPCI(KWKIX)) KWPMCI(KWKIX) = 1
      GOTO 9999

* Set Text Attributes.
  200 CONTINUE
      CALL GKDTXB
*     Need to check because individual settings won't have been checked.
      IF (KWTXCI(KWKIX).GE.KPCI(KWKIX)) KWTXCI(KWKIX) = 1
*     If font 1 and not stroke precision, calculate device-dependent
*     attributes:
      IF (KWTXFN(KWKIX).EQ.1.AND.KWTXPR(KWKIX).NE.GSTRKP) THEN
*       Find and load the raster font with the height nearest the
*       magnitude of the character height vector.
        CALL GK0XSF(SQRT(QWCHHX(KWKIX)**2 + QWCHHY(KWKIX)**2))
      ELSE
*       Ensure stroke precision is being used.
      KWTXPR(KWKIX) = GSTRKP
      ENDIF
      GOTO 9999

* Set Fill Area Attributes.
  210 CONTINUE
      CALL GKDFAB
*     Need to check because individual settings won't have been checked.
      IF (KWFAIS(KWKIX).EQ.GHATCH.AND.
     :    (KWFASI(KWKIX).LT.MNFAHS.OR.KWFASI(KWKIX).GT.MXFAHS))
     :  KWFASI(KWKIX) = -1
      IF (KWFAIS(KWKIX).EQ.GPATTR) THEN
*       Check whether pattern repres has been set (either predefined or
*       set by the user) - if not, use pattern index 1
        CALL GKDRGE(KPABPT(KWKIX),KWFASI(KWKIX),3,0,INTA,REALA)
        IF (KERROR.NE.0) THEN
          KWFASI(KWKIX) = 1
          KERROR = 0
        ENDIF
      ENDIF
      IF (KWFACI(KWKIX).GE.KPCI(KWKIX)) KWFACI(KWKIX) = 1
      GOTO 9999

* Set Pick Identifier [Not Implemented].
  220 CONTINUE
      GOTO 9999

* Set Polyline Representation.
  230 CONTINUE
      INTA(1) = 5
      CALL GKSRPL(1,INTA,.FALSE.)
      GOTO 9999

* Set Polymarker Representation.
  240 CONTINUE
      CALL GKSRPM(0,INTA,.FALSE.)
      GOTO 9999

* Set Text Representation.
  250 CONTINUE
      IF (KWI3.EQ.GSTRKP) THEN
*       Stroke Precision.  Make sure that fonts are available.
        IF (KDBFLS.EQ.KFLNA) THEN
          KERROR = -1042
          GOTO 9999
        ENDIF
        IF (KDBFLS.EQ.KFLCL) CALL GKXON
        IF (KERROR.NE.0) GOTO 9999
        DO 255 I = 1, KFNTMX
          IPREC(I) = GSTRKP
  255   CONTINUE
        CALL GKSRTX(KFNTMX,KHFONT,IPREC,.FALSE.)
      ELSE
*       String or Char Precision.
        IPREC(1)=KWI3
        INTA(1)=1
        CALL GKSRTX(1,INTA,IPREC,.FALSE.)
      ENDIF
      GOTO 9999

* Set Fill Area Representation.
  260 CONTINUE
      CALL GKSRFA(.FALSE.)
      GOTO 9999

* Set Pattern Representation.
  270 CONTINUE
      CALL GKSRPA(NID,IDAT)
      GOTO 9999

* Set Colour Representation.
  280 CONTINUE
*     See if colour index is valid and set up GKS colour table.
      IF (KWI1.GE.0.AND.KWI1.LT.KPCI(KWKIX)) THEN
        QHP(KHPXR(KCTBPT(1,KWKIX))+KWI1) = QWR1
        QHP(KHPXR(KCTBPT(2,KWKIX))+KWI1) = QWR2
        QHP(KHPXR(KCTBPT(3,KWKIX))+KWI1) = QWR3
*       Select workstation display surface.
        CALL GK0XWW(ISELCT,KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
*       Set the colour representation.
        CALL GK0XCM(ICOSET, KWI1, QWR1, QWR2, QWR3)
      ELSE
        KERROR = 93
      ENDIF
      GOTO 9999

* Normalization Transformation.
  310 CONTINUE
      CALL GKWKC4
      GOTO 9999

* Set Workstation Window.
  320 CONTINUE
      CALL GKSWKW
      GOTO 9999

* Set Workstation Viewport.
  330 CONTINUE
      CALL GKSWKV
      GOTO 9999

* Segment Entrypoints.
  410 CONTINUE
      CALL GKSGWK(IENT,.FALSE.)
      GOTO 9999

* Initialise Locator.
  610 CONTINUE
      CALL GKINIP(GLOCAT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999

* Initialise Stroke.
  620 CONTINUE
      CALL GKINIP(GSTROK,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999

* Initialise Valuator.
  630 CONTINUE
      CALL GKINIP(GVALUA,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999

* Initialise Choice.
  640 CONTINUE
      CALL GKINIP(GCHOIC,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999

* Initialise Pick.
  650 CONTINUE
      CALL GKINIP(GPICK,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999

* Initialise String.
  660 CONTINUE
      CALL GKINIP(GSTRIN,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999

* Set Input Operating Modes.
  670 CONTINUE
      GOTO 9999

* Set Input Modes.
  680 CONTINUE
      CALL GKSIPM
      GOTO 9999

* Request Locator.
  690 CONTINUE
      CALL GK0XWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
      KWI1 = GK0XRL(1, RX(1), RY(1))
      GOTO 9999

* Request Stroke.
  700 CONTINUE
      CALL GK0XWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
      KWI1 = GK0XRK(KNRR,NRD,RX,RY)
      GOTO 9999

* Request Valuator.
  710 CONTINUE
      CALL GK0XWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
      KWI1 = GK0XRV(QWR1)
      GOTO 9999

* Request Choice.
  720 CONTINUE
      CALL GK0XWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
      KWI1 = GK0XRC(KWI2)
      GOTO 9999

* Request Pick [Not Implemented].
  730 CONTINUE
      GOTO 9999

* Request String.
  740 CONTINUE
      CALL GK0XWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
      KWI1 = GK0XRT(NID,STR,KNIR)
      CALL GKNTOA(KNIR,STR,IDAT)
      GOTO 9999

* Sample Locator [Not Implemented].
  750 CONTINUE
      GOTO 9999

* Sample Stroke [Not Implemented].
  760 CONTINUE
      GOTO 9999

* Sample Valuator [Not Implemented].
  770 CONTINUE
      GOTO 9999

* Sample Choice [Not Implemented].
  780 CONTINUE
      GOTO 9999

* Sample Pick [Not Implemented].
  790 CONTINUE
      GOTO 9999

* Sample String [Not Implemented].
  800 CONTINUE
      GOTO 9999

* Flush Device Events.
  810 CONTINUE
      GOTO 9999

* Write Item To GKSM.
  910 CONTINUE
      KERROR = 32
      GOTO 9999

* Get Item Type From GKSM.
  920 CONTINUE
      KERROR = 34
      GOTO 9999

* Read Item From GKSM.
  930 CONTINUE
      KERROR = 34
      GOTO 9999

* Inquire Everything.
 1111 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999

* Inquire Pixel Array.
 1210 CONTINUE
      CALL GK0XWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
      CALL GKTWD(1,QWRA(1),QWRA(2),QWKDAT(IQPAX,KWKIX),
     :  QWKDAT(IQPAY,KWKIX))
      CALL GK0XPA(NINT(QWKDAT(IQPAX,KWKIX)),NINT(QWKDAT(IQPAY,KWKIX)),
     :  IDAT)
      GOTO 9999

* Inquire Text Facilities.
 1790 CONTINUE
*     Allow for string and character precision font 1 explicitly
      IF (KWI1.GT.KFNTMX+2) THEN
        KERROR=2002
        KWI1=KFNTMX+2
        GOTO 9999
      ENDIF
      IF (KWI1.GT.KFNTMX) THEN
*       String or character precision font
        KWI2 = 1
      IF (KWI1.EQ.KFNTMX+1) THEN
        KWI3 = GSTRP
      ELSE
        KWI3 = GCHARP
      ENDIF
      ELSE
*       Stroke precision font - first make sure that fonts are available
      IF (KDBFLS.EQ.KFLNA) THEN
          KERROR=-1009
          GOTO 9999
      ENDIF
*       Open fonts database if necessary
      IF (KDBFLS.EQ.KFLCL) CALL GKXON
      IF (KERROR.NE.0) GOTO 9999
      KWI2 = KHFONT(KWI1)
      KWI3 = 2
      ENDIF
      KWI1 = KFNTMX+2
      IF (KWKIX.NE.KNIL) THEN
*       A suitable workstation is open,  so use available state
        KWI4 = KCHH(KWKIX)
        KWI5 = KCHXPF(KWKIX)
        KWI6 = KPTXI(KWKIX)
        QWR1 = QMNCHH(KWKIX)
        QWR2 = QMXCHH(KWKIX)
        QWR3 = QMNCHX(KWKIX)
        QWR4 = QMXCHX(KWKIX)
      ELSE
*       Interrogate the WDT file
        CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
        IF (KERROR.EQ.0) THEN
          KWI4 = INTA(5)
          KWI5 = INTA(6)
          KWI6 = INTA(10)
          QWR1 = REALA(9)
          QWR2 = REALA(10)
          QWR3 = REALA(11)
          QWR4 = REALA(12)
        ENDIF
      ENDIF
      GOTO 9999

* Inquire Text Extent.
 1380 CONTINUE
*     Stroke Precision.
      IF (KWTXPR(KWKIX).EQ.GSTRKP) THEN
        CALL GKXQXO(NID,IDAT,RX,RY)
*     String and Char Precision.
      ELSE
*       Baseline vector from WS Set Text Attributes entry.
        CALL GKXQXC (NID,IDAT,QWCHWX(KWKIX),QWCHWY(KWKIX),
     :                  RX,RY,GK0XXF)

      ENDIF
      GOTO 9999

* Inquire Set Member of Segment Names on Workstation.
 1460 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KSGRQ = KSGRQ + 1
      GOTO 9999

* Inquire Maximum Display Surface Size.
 1720 CONTINUE
*     This must be read direct from the WDT because the state list items
*     get modified if the application changes the display space on OPEN
*     WORKSTATION.
      CALL GKQWDT(KWKTYP, KWDT, KNONE, 19, 12, INTA, REALA)
      IF (KERROR.NE.0) GOTO 9999
      KWI1 = GOTHU
      KWI2 = INTA(1)
      KWI3 = INTA(2)
      QWR1 = REALA(1)
      QWR2 = REALA(2)
      GOTO 9999

* Inquire Colour Facilities.
 1850 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KWI1 = KWI3
      IF (KWI1.GT.2) KWI2 = GCOLOR
      GOTO 9999

* Inquire List Element of Available GDPs.
 1870 CONTINUE
      IF (KWI1.GE.1.AND.KWI1.LE.4) THEN
        KWI2 = -KWI1
      ELSE
        KERROR = 2002
        KWI2 = KNIL
      ENDIF
      KWI4 = 4
      GOTO 9999


* Inquire GDP.
 1880 CONTINUE
      KNIR = 1
      IDAT(2) = KNIL
      IDAT(3) = KNIL
      IDAT(4) = KNIL
      IF (KWI1.EQ.-1) THEN
      IDAT(1) = GPLATT
      ELSE IF (KWI1.LE.-2.AND.KWI1.GE.-4) THEN
        IDAT(1) = GFAATT
      ELSE
      KNIR = 0
      IDAT(1) = KNIL
        KERROR = 41
      ENDIF
      GOTO 9999

* Update display usage information and update window if necessary.
 8888 CONTINUE
      KDSMT(KWKIX) = GNEMPT
      IF (KWIO(KWKIX).EQ.GYES) THEN
      CALL GK0XWW(IUPDAT, KWCID(KWKIX))
      ENDIF

* Exit.
 9999 CONTINUE
      RETURN

      END
