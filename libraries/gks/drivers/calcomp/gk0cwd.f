C# IL>=a, OL>=0
      SUBROUTINE GK0CWD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*
* --------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:             MGC
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     CalComp Model 81 Plotter
*
*  MAINTENANCE LOG
*  ---------------
*     00/99/83  MGC   Original version stabilized
*     24/01/84  RSK   Changed use of GKIOB to use of GKIOBO for
*                     bufferd byte output.
*     07/03/84  MGC   Char and string precision text
*     30/03/84  MGC   Add GK0COP routine
*     27/04/84  MGC   Add entrypoint 1460 (I192)
*     08/05/84  RSK   GDP entrypoint should return KERROR=104 for unknown
*                     GDP indentifier or KERROR=102 for invalid GDP identifier
*                     (I206), while Inquire GDP entrypoint should return
*                     KERROR=41 for unknown GDP indentifier (I206).
*     11/05/84  MGC   Correct wdt inquires (I212)
*     09/07/84  RMK   Changed GKIOBO calls to go through new routines
*                     GK0CPB and GK0CPA for handshaking.
*                     GDP entrypoint: added G to parameters in final GKCIRC.
*                     At 8888, changed KSDSMT to KDSMT.
*     12/07/84  RMK   Removed flush buffer from start of each output function.
*                     Made flush buffer at 8888 conditional.
*     23/07/84  RMK   Added Pen Up to end of each Output Function.
*     18/09/84  RMK   Choice of sheet or roll paper made dependent on
*                     workstation type. Removed Calcomp-specific common block.
*      8/10/84  RMK   Changed default deferral mode and wkstn immediate output.
*                     Corrected set deferral state entry.
*     26/11/84  RMK   Added error 38 for all input-related entries.
*                     Miscellaneous changes to bring in line with Sigma.
*      4/12/84  RMK   Changed Cell Array entry to use GKCASM, to allow
*                     for transformations. Removed 8th pen - white.
*     16/05/85  RMK   Use utility to produce standard RAL 5th linestyle (S131).
*                     Removed declarations of variables which are not used.
*     30/06/86  RMK   Changed to use new interface to GKIOOP/GKIOCL, and new
*                     utilities GKFILS/GKCRCS. Tightened up error checking in
*                     Inquire Fill Area Facils entry. Removed use of CHAR
*                     function - changed to GKAN1 (S103).
*     09/07/86  RMK   Added include file GKFLS.PAR.
*     05/06/87  RMK   Removed use of GARC in Inquire GDP entry.
*     24/06/87  PLP   Converted driver to 7.4.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*     06/07/87  RMK   Changed Inquire GDP to use IS Fortran binding names
*                     for GDP attributes.
*     16/11/87  RMK   Corrected error checking in GDP entry (S275).
*     09/12/88  DBG   Corrected Inquire fill area facilities entry (S408).
*     12/12/88  NMH   Corrected Inquire list element of available
*                     generalized drawing primitives. Check element requested
*                     is within range 1 to 4 (S409).
*     25/05/90  KEVP  Made hardware fonts 2 to 6 negative. Changes at
*                     data statement of ITXF and in the entry points
*                     KSTXA=20 and KSTXR=25 - IS conversion (S309).
*     24/10/90  KEVP  Rewrote inQuire Fill Area Facilities entry point
*                     to ensure data is passed even if the list element
*                     is out of range (C55 & C29).
*     25/10/90  KEVP  Ensured that inQuire TeXt Facilities entry point
*                     passes data even if there is an error (C55).
*     29/10/90  KEVP  Made it call GKPXAD to return correct values for
*                     inQuire PiXel Array Dimensions (C57).
*
*  ARGUMENTS
*  ---------
*     INP IENT  - Entrypoint code
*     INP NID   - Size of array IDAT
*     I/0 IDAT  - Integer data passed to or from workstation
*     INP NRD   - Size of arrays RX and RY
*     I/O RX    - Real X-coordinate data passed to or from workstation
*     I/O RY    - Real Y-coordinate data passed to or from workstation
*     INP NCD   - Size of character array
*     I/O STR   - Character array
*
      INTEGER IENT, NID, IDAT(NID), NRD, NCD
      REAL RX(NRD), RY(NRD)
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkfls.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gks.par'
      INCLUDE '../../include/gkwdt.par'
      INCLUDE '../../include/gkwke.par'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkfab.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkplb.cmn'
      INCLUDE '../../include/gkpmb.cmn'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gktxb.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkxfd.cmn'
*
*  LOCALS
*  ------
*
*     Parameters
*     ----------
*
*     standard chunk for polyline,polymarker
      INTEGER    ICHUNK
      PARAMETER (ICHUNK=200)

*     index for comms line speed ???
      INTEGER    IBAUD
      PARAMETER (IBAUD=1)

*     no of text font and precision pairs
      INTEGER   INTXFP
      PARAMETER (INTXFP=10)

*     pi radians
      REAL      PI
      PARAMETER(PI=3.14159)

*
*     Variables
*     ---------
*
*     text
      INTEGER ICHA,ICHH,ICHW,ICHX
      INTEGER IHF(1),IHP(1),ITXF(INTXFP),ITXP(INTXFP)
      REAL RCHHX,RCHHY,RCHWX,RCHWY,RCHRX,RCHRY

*     miscellaneous variables
      INTEGER IOFF, I, N, INKWI2
      INTEGER INTA(19)
      REAL REALA(12)
      LOGICAL FLAG
*
*  EXTERNALS
*  ---------
*
      EXTERNAL GK0CLN, GK0CRO, GK0CXC, GK0CXF
*
*  EXTERNAL FUNCTION DEFINITIONS
*  -----------------------------
*
      CHARACTER*1 GKAN1
*
*  STACK USAGE
*  -----------
*     2*ICHUNK REAL     polyline, polymarker transformed points
*
*  ERRORS
*  ------
*       32   CC81 workstation is not of category MO
*       34   CC81 workstation is not of category MI
*       38   CC81 workstation has no input devices
*       41   CC81 workstation is not able to generate
*            the specified generalized drawing primitive
*       64   Specified linetype is not supported on CC81
*       70   Specified marker type is not supported on CC81
*       76   Text font not supported for specified precision
*       83   Fill area interior style not supported (PATTERN)
*       86   Specified HATCH style is not supported on CC81
*       93   Colour index invaild
*      102   Generalized drawing primitive identifier is invalid
*      104   CC81 is not able to generate the specified gdp
*      180   Specified escape function is not supported
*     2002   List element or set member not available
*    -1004   No room in bundle table
*
*  COMMENTS
*  --------
*
*     Font        Precision     Hardware      Hershey
*     1            STRING        [1]           none
*                  CHAR          [1]           default (see note below)
*                  STROKE        none          default
*     -2..-6]      STRING        [1..5]        none
*                  CHAR          [1..5]        KHFONT(1)
*                  STROKE        none          default
*     KHFONT([1..KFNTMX])
*                  STRING        [1]           none
*                  CHAR          [1]           KHFONT([1..KFNTMX])
*                  STROKE        none          KHFONT([1..KFNTMX])
*     other        STRING        [1]           none
*                  CHAR          [1]           none
*                  STROKE        none          default
*
*  The table above indicates which hardware font and,or
*  hershey font is appropriate for each font,precision pair.
*  The default hershey font is KHFONT(1), but for Font 1 char
*  precision this is currently not the case: due to an unresolved
*  deficiency in utility GKXDWC - so for 'default' read 'none'.
*
*  There is currently no guarantee that the hershey data base is set
*  up prior to ws entry; so for the present the (temporary) list of
*  font nos IHFONT is used in place of KHFONT.
*
* --------------------------------------------------------------------

*     temporary hershey font list
      INTEGER IHFONT(15)
      DATA IHFONT /-101,-102,-103,-104,-105,-106,-107,
     :             -108,-109,-110,-111,-112,-113,-114,-115/

*     Hardware fonts
      DATA ITXF /-2,-2,-3,-3,
     :           -4,-4,-5,-5,-6,-6/

*     Hardware precisions
      DATA ITXP /GSTRP,GCHARP,GSTRP,GCHARP,
     :           GSTRP,GCHARP,GSTRP,GCHARP,GSTRP,GCHARP/


* --------------------------------------------------------------------
* Conditional GOTO on entrypoint code
* --------------------------------------------------------------------
*     WS Entry : 1-119
      GOTO (       10,  20,  30,  40,  50,  60,  70,  80,9999,
     :       100, 110, 120, 130, 140, 150, 160, 170, 180, 190,
     :       200, 210, 220, 230, 240, 250, 260, 270, 280,9999,
     :      9999, 310, 320, 330,9999,9999,9999,9999,9999,9999,
     :      9999, 410, 410, 410, 410, 410, 410, 410, 410, 410,
     :       410, 410,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999, 610, 610, 610, 610, 610, 610, 670, 680, 690,
     :       690, 690, 690, 690, 690, 750, 750, 750, 750, 750,
     :       750, 810,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999, 910, 920, 930) IENT
*     WS Entry : 120-206
      GOTO (1200,1111,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1111,1111,1111,1111,1111,1111,1111,1370,1380,1111,
     :      1111,1111,1420,1111,1440,1111,1460,1470,1470,1470,
     :      1470,1470,1470,1111,9999,9999,9999,9999,9999,9999,
     :      9999,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1700,1710,1111,1111,1111,1111,1111,1111,1111,1790,
     :      1111,1810,1111,1111,1840,1850,1111,1870,1880,1111,
     :      1111,1111,1920,1930,1930,1930,1930,1930,1930) IENT-119
      GOTO 9999



*===============================================================
*                 CONTROL FUNCTIONS
*===============================================================
*
* ---------------------------------------------------------------
* Open workstation
* ---------------------------------------------------------------
 10   CONTINUE
*     data expected
*     KWI1   connection identifier
*     KWKTYP
*     data returned
*     KWI1   workstation category (IO)
*     KERROR error response or zero

*     set up workstation state list and WDT
      DO 12 I=1,6
        INTA(I) = 0
12    CONTINUE
      CALL GKIWSL(KWKIX,KWKTYP)
      IF (KERROR.NE.0) GOTO 9999
*     connection identifier
      KCID(KWKIX) = KWI1
*     deferral mode
      KDFM(KWKIX) = GBNIG
*     ws immediate output - no
      KWIO(KWKIX)=GNO
*     implicit regeneration - suppressed
      KIMRGM(KWKIX) = GSUPPD
*     ask operating system to make a connection
      CALL GKIOOP(KFWKT,KCID(KWKIX),KWCID(KWKIX))
      IF (KERROR.EQ.0) THEN
        KWKDAT(IBAUD,KWKIX)=1200
*       initialise device
        CALL GK0CID
      ENDIF
*     set segment list pointer
      KSSGPT(KWKIX) = KNIL
*     no input devices
      KWI1 = GOUTPT
      GOTO 9999

* --------------------------------------------------------------
* Close workstation
* --------------------------------------------------------------
 20   CONTINUE
*     Data expected
*     KWI1 : first/second pass

*     first pass
      IF(KWI1.EQ.1) THEN
*       indicate second pass expected
        KWDONE=KRFUSE
*     second pass
      ELSE
*       picture clear
        CALL GK0CCL(.TRUE.)
*       finish with device
        CALL GK0CIE
*       disconnect ws
        CALL GKIOCL(KFWKT,KCID(KWKIX),KWCID(KWKIX))
        CALL GKCWSL(KWKIX)
        CALL GKSLDL(KSSGPT(KWKIX))
      ENDIF
      GOTO 9999

* --------------------------------------------------------------
* Clear workstation (F/E announcement before clr surface)
* --------------------------------------------------------------
 30   CONTINUE
*     Data expected:
*     KWI1 : first/second pass
*     KWI2 : conditional - Clear Control Flag (ignored)


*     second pass
      IF (KWI1 .EQ. 2) THEN
*        delete ws segments
         CALL GKSLDL(KSSGPT(KWKIX))
*        indicate clear surface entry expected
         KWDONE = KRFUSE
      ENDIF
      GOTO 9999

* --------------------------------------------------------------
* Redraw all segments on workstation
* --------------------------------------------------------------
 40   CONTINUE
      KWDONE = KRFUSE
      GOTO 9999

* --------------------------------------------------------------
* Update workstation (F/E announcement before close wk)
* --------------------------------------------------------------
50    CONTINUE
*     Data expected:
*     KWI1 : GPERFO    - Perform update?

      KWDONE = KRFUSE
      GOTO 9999

* --------------------------------------------------------------
* Set deferral state
* --------------------------------------------------------------
 60   CONTINUE
*     Data expected:
*     KWI1 : New deferral mode (ASAP,BNIG,BNIL,ASTI)
*     KWI2 : New implicit regeneration mode (SUPPRESSED,ALLOWED)

      KDFM(KWKIX) = KWI1
      KIMRGM(KWKIX) = KWI2
      IF (KWI1.EQ.GASAP) THEN
        KWIO(KWKIX) = GYES
        CALL GK0CPA(KIOSN,1,KDAT)
      ELSE
        KWIO(KWKIX) = GNO
      ENDIF
      IF (KIMRGM(KWKIX).EQ.GALLOW.AND.KNFAUP(KWKIX).EQ.GYES
     :    .AND.KDSMT(KWKIX).EQ.GNEMPT) THEN
         KWRGN(KWKIX) = .TRUE.
         KRGN         = .TRUE.
      ENDIF
      GOTO 9999

* --------------------------------------------------------------
* Do deferred output actions
* --------------------------------------------------------------
 70   CONTINUE
*     Data returned:
*     KWI1 : new frame action necessary at update

      CALL GK0CPA(KIOSN,1,KDAT)
      KWI1 = KNFAUP(KWKIX)
      GOTO 9999

* --------------------------------------------------------------
* Clear display surface
* --------------------------------------------------------------
 80   CONTINUE
*     Data expected:
*     KWI1 : conditional - Clear Control Flag (COND,ALWAYS)

      IF(KWI1.EQ.GALWAY.OR.KDSMT(KWKIX).EQ.GNEMPT)
     :  CALL GK0CCL(.FALSE.)
*     reset current wk window and viewport
      CALL GKWCLD
      GOTO 9999

* --------------------------------------------------------------
* Message
* --------------------------------------------------------------
 100  CONTINUE
*     Flush buffer first
*     (Should really have a system utility to output message)
      CALL GK0COP
      IF (NID.GE.1)  WRITE(KERRFL,101) (GKAN1(IDAT(I)),I=1,NID)
  101                FORMAT(80A1)
      GOTO 9999

* --------------------------------------------------------------
* Escape
* --------------------------------------------------------------
 110  CONTINUE
*     Data expected:
*     KWI1 : function identifier
*
      KERROR = 180
      GOTO 9999

*===============================================================
*                OUTPUT FUNCTIONS
*===============================================================

* --------------------------------------------------------------
* Polyline
* --------------------------------------------------------------
 120  CONTINUE
      IF (KCVIS.EQ.GINVIS)  GOTO 9999
*     no action if drawing in Background colour
      IF(KWPLCI(KWKIX).EQ.0) GOTO 9999
*     get enough stack space for a chunk of transformed points
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.NE.0) GOTO 9999
*     select pen from polyline colour index
      CALL GK0CSP(KWPLCI(KWKIX),KCTBPT(1,KWKIX),
     :               KCTBPT(2,KWKIX),KCTBPT(3,KWKIX))
*     Use hardware for linestyles 1 to 4, and the utility for style 5
      FLAG = .FALSE.
      IF (KWLNTY(KWKIX).EQ.5) THEN
         FLAG = .TRUE.
      ELSE
         CALL GK0CSL(KWLNTY(KWKIX))
      ENDIF
*     output in manageable polyline sections
      N = ICHUNK
      DO 122 I=1,NRD,ICHUNK-1
        IF (NRD-I.LT.ICHUNK-1) N = NRD-I + 1
        IF (N.EQ.1) GOTO 122
        CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
        CALL GKLCLP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),FLAG,75.0,
     :                 QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                 QWCLYT(KWKIX),GK0CLN)
122   CONTINUE
      CALL GKSTDA(KREALS,IOFF)
*     pen up (H)
      CALL GK0CPB(72)
      GOTO 8888

* --------------------------------------------------------------
* Polymarker
* --------------------------------------------------------------
 130  CONTINUE
      IF (KCVIS.EQ.GINVIS)  GOTO 9999
*     no action if drawing in Background colour
      IF(KWPMCI(KWKIX).EQ.0) GOTO 9999
*     get enough stack space for a chunk of transformed points
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.NE.0) GOTO 9999
*     select pen from polymarker colour index
      CALL GK0CSP(KWPMCI(KWKIX),KCTBPT(1,KWKIX),
     :               KCTBPT(2,KWKIX),KCTBPT(3,KWKIX))
*     set linetype solid - all marker types simulated
      CALL GK0CSL(1)
*     output in manageable polymarker sections
      N = ICHUNK
      DO 132 I=1,NRD,ICHUNK
        IF (NRD-I.LT.ICHUNK) N = NRD-I + 1
        CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
        CALL GKMTYP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),
     :                 KWMKTY(KWKIX),QNMMKS(KWKIX),QWMKSZ(KWKIX),
     :                 QWCLXL(KWKIX),QWCLYB(KWKIX),
     :                 QWCLXR(KWKIX),QWCLYT(KWKIX),GK0CLN)
132   CONTINUE
      CALL GKSTDA(KREALS,IOFF)
*     pen up (H)
      CALL GK0CPB(72)
      GOTO 8888

* --------------------------------------------------------------
* Text
* --------------------------------------------------------------
 140  CONTINUE
      IF(KCVIS.EQ.GINVIS) GOTO 9999
*     no action if drawing in Background colour
      IF(KWTXCI(KWKIX).EQ.0) GOTO 9999
*     select pen from text colour index
      CALL GK0CSP(KWTXCI(KWKIX),KCTBPT(1,KWKIX),
     :               KCTBPT(2,KWKIX),KCTBPT(3,KWKIX))
*  -- stroke precision
      IF(KWTXPR(KWKIX).EQ.GSTRKP) THEN
        CALL GK0CSL(1)
        CALL GKXDWO(NID,IDAT,GK0CLN)
*  -- string or char precision
      ELSE
        CALL GKXDWC(NID,IDAT,
     :                 QWCHRX(KWKIX),QWCHRY(KWKIX),
     :                 GK0CXF,GK0CXC)
      ENDIF
*     pen up (H)
      CALL GK0CPB(72)
      GOTO 8888

* --------------------------------------------------------------
* Fill area
* --------------------------------------------------------------
 150  CONTINUE
      IF (KCVIS.EQ.GINVIS)  GOTO 9999
*      no action if solid or pattern
      IF(KWFAIS(KWKIX).EQ.GPATTR .OR.
     :   KWFAIS(KWKIX).EQ.GSOLID) GOTO 9999
*     for hollow,hatch no action if Background colour
      IF(KWFACI(KWKIX).EQ.0) GOTO 9999
*     select pen from fill area colour index
      CALL GK0CSP(KWFACI(KWKIX),KCTBPT(1,KWKIX),
     :               KCTBPT(2,KWKIX),KCTBPT(3,KWKIX))
*     set linetype solid
      CALL GK0CSL(1)
*     fill area -  GK0CLN for hollow,hatch
      CALL GKFILS(NRD,RX,RY,1,GK0CLN,GK0CRO)
*     pen up (H)
      CALL GK0CPB(72)
      GOTO 8888

* --------------------------------------------------------------
* Cell array
* --------------------------------------------------------------
 160  CONTINUE
      IF (KCVIS.EQ.GINVIS)  GOTO 9999
*     select pen from default colour index
      CALL GK0CSP(1,KCTBPT(1,KWKIX),
     :               KCTBPT(2,KWKIX),KCTBPT(3,KWKIX))
*     set linetype solid
      CALL GK0CSL(1)
*     use utility to perform minimal simulation
      CALL GKCASM(GK0CLN)
*     pen up (H)
      CALL GK0CPB(72)
      GOTO 8888

* --------------------------------------------------------------
* GDP
* --------------------------------------------------------------
 170  CONTINUE
      IF (KCVIS.EQ.GINVIS)  GOTO 9999
* First, check GDP identifier
      IF (KWI1.EQ.0) THEN
        KERROR = 102
        GOTO 9999
      ELSE IF (KWI1.LT.-4 .OR. KWI1.GT.-1) THEN
        KERROR = 104
        GOTO 9999
      ELSE IF (KWI1.EQ.-1) THEN
* Arc
*     -- Set line colour and width
        CALL GK0CSP(KWPLCI(KWKIX),
     :                 KCTBPT(1,KWKIX),KCTBPT(2,KWKIX),KCTBPT(3,KWKIX))
        CALL GK0CSL(KWLNTY(KWKIX))
        CALL GKCRCS(KWI1,NRD,RX,RY,1,.FALSE.,20.0,GK0CLN,GK0CRO)
*     pen up (H)
        CALL GK0CPB(72)
      ELSE
* Filled chord, pie, circle
*     -- Set line colour and width
        IF(KWFAIS(KWKIX).NE.GPATTR .AND. KWFACI(KWKIX).EQ.0) GOTO 9999
        IF(KWFAIS(KWKIX).NE.GPATTR) THEN
          CALL GK0CSP(KWFACI(KWKIX),
     :              KCTBPT(1,KWKIX),KCTBPT(2,KWKIX),KCTBPT(3,KWKIX))
          CALL GK0CSL(1)
          CALL GKCRCS(KWI1,NRD,RX,RY,1,.FALSE.,20.0,GK0CLN,GK0CRO)
        ENDIF
*     pen up (H)
        CALL GK0CPB(72)
      ENDIF
      GOTO 8888


*===============================================================
*                WORKSTATION ATTRIBUTES (REALISED)
*===============================================================

* --------------------------------------------------------------
* Set polyline attributes
* --------------------------------------------------------------
 180  CONTINUE
      CALL GKDPLB
*     reset workstation dependent (realised) values
      QWLNWD(KWKIX) = 1.0
      IF(KWLNTY(KWKIX).GT.5) KWLNTY(KWKIX) = 3
      IF(KWPLCI(KWKIX).GE.KPCI(KWKIX)) KWPLCI(KWKIX) = 1
      GOTO 9999

* --------------------------------------------------------------
* Set polymarker attributes
* --------------------------------------------------------------
 190  CONTINUE
      CALL GKDPMB
*     reset workstation dependent (realised) values
      IF(KWMKTY(KWKIX).GT.5) KWMKTY(KWKIX) = 3
      IF(KWPMCI(KWKIX).GE.KPCI(KWKIX)) KWPMCI(KWKIX) = 1
      GOTO 9999

* --------------------------------------------------------------
* Set  text attributes
* --------------------------------------------------------------
 200  CONTINUE
*     note GKDTXB sets KWTXFI from KWTXFN
      CALL GKDTXB
      IF(KWTXCI(KWKIX).GE.KPCI(KWKIX)) KWTXCI(KWKIX) = 1
*     done if stroke precision
      IF(KWTXPR(KWKIX).EQ.GSTRKP) GOTO 9999
*     default string,char hardware font
      ICHX=1
*     identify if hardware font required
      IF(KWTXFI(KWKIX).GE.-6 .AND. KWTXFI(KWKIX).LE.-2) THEN
*       hershey font for char precision
        KWTXFN(KWKIX)=IHFONT(1)
*       hardware font for string,char precision
        ICHX=KWTXFI(KWKIX)-1
      ENDIF
*     character height
      RCHHX=QWCHHX(KWKIX)
      RCHHY=QWCHHY(KWKIX)
      ICHH=IFIX(SQRT(RCHHX*RCHHX+RCHHY*RCHHY))
*     character width
      RCHWX=QWCHWX(KWKIX)
      RCHWY=QWCHWY(KWKIX)
      ICHW=IFIX(SQRT(RCHWX*RCHWX+RCHWY*RCHWY) * QWCHXP(KWKIX))
*     character rotation
      RCHRX=QWCHWX(KWKIX)
      RCHRY=QWCHWY(KWKIX)
      ICHA=MOD(NINT(180.0*ATAN2(RCHRY,RCHRX)/PI)+360,360)
*     char dimensions multiple of 6 units
      ICHH=(ICHH/6)*6
      ICHW=(ICHW/6)*6
*     min units 6 ; max units 1500
      ICHH=MAX(6,ICHH)
      ICHW=MAX(6,ICHW)
      ICHH=MIN(1500,ICHH)
      ICHW=MIN(1500,ICHW)
*     realised values - used by GK0CXC,GK0CXF
      KWTXFI(KWKIX)=ICHX
      KWCHHT(KWKIX)=ICHH
      KWCHWD(KWKIX)=ICHW
      QWCHRX(KWKIX)=RCHRX
      QWCHRY(KWKIX)=RCHRY
*     now prime the device
      CALL GK0CXP(ICHX,ICHH,ICHW,ICHA)
      GOTO 9999

* --------------------------------------------------------------
* Set fill area attributes
* --------------------------------------------------------------
 210  CONTINUE
      CALL GKDFAB
*     realise pattern with hollow,and current fill area colour
      IF(KWFAIS(KWKIX).EQ.GPATTR) KWFAIS(KWKIX) = GHOLLO
*     realise solid with hollow
      IF(KWFAIS(KWKIX).EQ.GSOLID) KWFAIS(KWKIX) = GHOLLO
      IF(KWFAIS(KWKIX).EQ.GHATCH .AND. (KWFASI(KWKIX).LT.-10.OR.
     :   KWFASI(KWKIX).GT.-1)) KWFASI(KWKIX) = -1
      IF(KWFACI(KWKIX).GE.KPCI(KWKIX)) KWFACI(KWKIX) = 1
      GOTO 9999

* --------------------------------------------------------------
* Set pick identifier
* --------------------------------------------------------------
220   CONTINUE
      GOTO 9999


*===============================================================
*                WORKSTATION ATTRIBUTES (REPRESENTATIONS)
*===============================================================

* --------------------------------------------------------------
* Set polyline representation
* --------------------------------------------------------------
 230  CONTINUE
*     Data expected:
*     KWI1 : polyline index
*     KWI2 : linetype
*     QWR1 : linewidth scale factor
*     KWI3 : polyline colour index

      INTA(1) = 5
      CALL GKSRPL(1,INTA,.FALSE.)
      GOTO 9999

* --------------------------------------------------------------
* Set polymarker representation
* --------------------------------------------------------------
 240  CONTINUE
*     Data expected:
*     KWI1   : polymarker index
*     KWI2   : markertype
*     QWR1   : marker scale factor
*     KWI3   : polymarker colour index

      CALL GKSRPM(0,INTA,.FALSE.)
      GOTO 9999

* --------------------------------------------------------------
* Set text representation
* --------------------------------------------------------------
 250  CONTINUE
*     Data expected:
*     KWI1   : text index
*     KWI2   : font
*     KWI3   : precision  (STRING,CHAR,STROKE)
*     QWR1   : character expansion factor
*     QWR2   : character spacing factor
*     KWI4   : text colour index
*     Data returned:
*     KERROR : error indicator

*     hardware fonts
      IF(KWI2.GE.-6 .AND. KWI2.LE.-2) THEN
        CALL GKSRTX(INTXFP,ITXF,ITXP,.FALSE.)
*     font 1 special case (but see inquiry)
      ELSEIF(KWI2.EQ.1) THEN
        IHF(1)=1
        IHP(1)=KWI3
        CALL GKSRTX(1,IHF,IHP,.FALSE.)
*     hershey stroke fonts
      ELSE
        DO 252 I=1,KFNTMX
         IHF(1)=IHFONT(I)
         IHP(1)=GSTRKP
         IF(KWI2.EQ.IHF(1)) GOTO 254
 252    CONTINUE
        KERROR=76
 254    CONTINUE
        IF(KERROR.EQ.0) CALL GKSRTX(1,IHF,IHP,.FALSE.)
      ENDIF
      GOTO 9999

* --------------------------------------------------------------
* Set fill area representation
* --------------------------------------------------------------
 260  CONTINUE
*     Data expected:
*     KWI1   : Fill Area Index
*     KWI2   : Fill Area Interior Style
*     KWI3   : Fill Area Style Index
*     KWI4   : Fill Area Colour Index

*     interior styles hollow,hatch but not solid,pattern
      IF (KWI2.NE.GHOLLO .AND. KWI2.NE.GHATCH) THEN
        KERROR = 83
      ELSE
        CALL GKSRFA(.FALSE.)
      ENDIF
      GOTO 9999

* --------------------------------------------------------------
* Set pattern representation
* --------------------------------------------------------------
 270  CONTINUE
*     Data expected:
*     KWI1   : Pattern Index
*     KWI2   : Pattern X dimension
*     KWI3   : Pattern Y dimension
*     KWI4   : First dimension of colour index array

*     pattern not supported
      KERROR = 90
      GOTO 9999

* --------------------------------------------------------------
* Set colour representation
* --------------------------------------------------------------
 280  CONTINUE
*     Data expected:
*     KWI1   : Colour Index
*     QWR1   : RED component
*     QWR2   : GREEN component
*     QWR3   : BLUE component

* Colour table bundles are stored on the HEAP; bundles can be
* modified but new ones may not be added; the total no of bundle
* entries (set by WDT) is found from KPCI(KWKIX).
      IF (KWI1.LT.0 .OR. KWI1.GE.KPCI(KWKIX)) THEN
        KERROR = 93
      ELSE
        QHP(KHPXR(KCTBPT(1,KWKIX))+KWI1) = QWR1
        QHP(KHPXR(KCTBPT(2,KWKIX))+KWI1) = QWR2
        QHP(KHPXR(KCTBPT(3,KWKIX))+KWI1) = QWR3
      ENDIF
      GOTO 9999


*===============================================================
*                TRANSFORMATIONS
*===============================================================

* --------------------------------------------------------------
* Normalisation transformation
* --------------------------------------------------------------
 310  CONTINUE
      CALL GKWKC4
      GOTO 9999

* --------------------------------------------------------------
* Set workstation window
* --------------------------------------------------------------
320   CONTINUE
*     Data expected:
*     QWR1-4 : workstation window XL, XR, YB, YT
*     Data returned:
*     KRGN & KWRGN : .TRUE. if regeneration needed

*     set up requested window
      CALL GKSWKW
      GOTO 9999

* --------------------------------------------------------------
* Set workstation viewport
* --------------------------------------------------------------
330   CONTINUE
*     Data expected:
*     QWR1-4 : workstation viewport XL, XR, YB, YT
*     Data returned:
*     KRGN & KWRGN : .TRUE. if regeneration needed

* Set up requested viewport
      CALL GKSWKV
      GOTO 9999

* --------------------------------------------------------------
* Segment entrypoints (410-510)
* --------------------------------------------------------------
 410  CONTINUE
      CALL GKSGWK(IENT,.FALSE.)
      GOTO 9999


*===============================================================
*                INPUT FUNCTIONS - INITIALISATION
*===============================================================

* --------------------------------------------------------------
* Initialise locator/stroke/valuator/choice/pick/string
* --------------------------------------------------------------
 610  CONTINUE
* Calcomp has no input devices
      KERROR = 38
      GOTO 9999


*===============================================================
*                INPUT FUNCTIONS - SET MODE
*===============================================================
* --------------------------------------------------------------
* Set input operating modes
* --------------------------------------------------------------
670   CONTINUE
      GOTO 9999
* --------------------------------------------------------------
* Set input mode
* --------------------------------------------------------------
680   CONTINUE
      KERROR = 38
      GOTO 9999


*===============================================================
*                INPUT FUNCTIONS - REQUEST
*===============================================================

* --------------------------------------------------------------
* Request locator/stroke/valuator/choice/pick/string
* --------------------------------------------------------------
 690  CONTINUE
* Calcomp has no input devices
      KERROR = 38
      GOTO 9999


*===============================================================
*                INPUT FUNCTIONS - SAMPLE
*===============================================================

* --------------------------------------------------------------
* Sample locator/stroke/valuator/choice/pick/string
* --------------------------------------------------------------
 750  CONTINUE
* Calcomp has no input devices
      KERROR = 38
      GOTO 9999


*===============================================================
*                INPUT FUNCTIONS - EVENT
*===============================================================

* --------------------------------------------------------------
* Flush device events
* --------------------------------------------------------------
 810  CONTINUE
      GOTO 9999


*===============================================================
*                METAFILE FUNCTIONS
*===============================================================

* --------------------------------------------------------------
* Write item to GKSM
* --------------------------------------------------------------
 910  CONTINUE
      KERROR = 32
      GOTO 9999

* --------------------------------------------------------------
* Get item type from GKSM
* --------------------------------------------------------------
  920 CONTINUE
      KERROR = 34
      GOTO 9999
* --------------------------------------------------------------
* Read item from GKSM
* --------------------------------------------------------------
 930  CONTINUE
      KERROR = 34
      GOTO 9999


*===============================================================
*             INQUIRE EVERYTHING
*===============================================================
 1111 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999


*===============================================================
*             INQUIRE PIXEL ARRAY DIMENSIONS
*===============================================================
 1200 CONTINUE
      CALL GKPXAD
      GOTO 9999

*===============================================================
*             INQUIRY FUNCTIONS - WS STATE LIST
*===============================================================

* --------------------------------------------------------------
* Inquire text representation
* --------------------------------------------------------------
 1370 CONTINUE
*     Input data :
*     KWI1   : text index
*     KWI2   : type of returned value ( GSET or GREALIzed )
*
*     Data returned:
*     KERROR : error indicator
*     KWI1   : text font
*     KWI2   : text precision
*     KWI3   : text colour index
*     QWR1   : character expansion factor
*     QWR2   : character spacing

      INKWI2 = KWI2
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      IF(INKWI2.EQ.GREALI) THEN
*       hardware fonts
        IF(KWI1.GE.2 .AND. KWI1.LE.6) THEN
          DO 1372 I=1,INTXFP
            IF(KWI1.EQ.ITXF(I) .AND. KWI2.EQ.ITXP(I)) GOTO 1379
 1372     CONTINUE
        ENDIF
*       hershey stroke fonts
        IF(KWI2.EQ.GSTRKP) THEN
          DO 1374 I=1,KFNTMX
            IF(KWI1.EQ.IHFONT(I)) GOTO 1379
 1374     CONTINUE
        ENDIF
*       default font
        KWI1=1
 1379   CONTINUE
        IF(KWI3.GE.KPCI(KWKIX)) KWI3 = 1
      ENDIF
      GOTO 9999

* --------------------------------------------------------------
* Inquire text extent
* --------------------------------------------------------------
 1380 CONTINUE
*     Input data :
*     NID    : length of string
*     IDAT   : integer array character codes for string
*     QWR1   : x-text position
*     QWR2   : y-text position
*     QWR3   : x-width vector
*     QWR4   : y-width vector
*     QWR5   : x-height vector
*     QWR6   : y-height vector
*     QWCHRX(KWKIX),QWCHRY(KWKIX) : baseline vector
*
*     Data returned:
*     KERROR : error indicator
*     RX(1-4): x-text extent
*     RY(1-4): y-text extent
*     QWR7   : x-concatenation point
*     QWR8   : y-concatenation point

*     stroke precision
      IF (KWTXPR(KWKIX) .EQ. GSTRKP) THEN
        CALL GKXQXO(NID,IDAT,RX,RY)
*     string and char precision
      ELSE
*       baseline vector from ws Set text attributes entry
        CALL GKXQXC (NID,IDAT,QWCHRX(KWKIX),QWCHRY(KWKIX),
     :                  RX,RY,GK0CXF)

      ENDIF
      GOTO 9999


* --------------------------------------------------------------
* Inquire pattern representation
* --------------------------------------------------------------
 1420 CONTINUE
*     Input data :
*     KWI1   : pattern index
*     KWI2   : type of returned values ( "GSET" or "GREALI" )
*     KWI3   : N maximum pattern array dimension
*     KWI4   : M maximum pattern array dimension
*
*     Data returned:
*     KERROR : error indicator
*     KWI1   : N pattern array dimension
*     KWI2   : M pattern array dimension
*     IDAT   : pattern array

*     pattern not supported
      KERROR = 90
      GOTO 9999

* --------------------------------------------------------------
* Inquire colour representation
* --------------------------------------------------------------
 1440 CONTINUE
*     Input data :
*     KWI1 : colour table index
*     KWI2 : type of returned values ( "GSET" or "GREALI" )
*
*     Data returned:
*     KERROR : error indicator
*     QWR1   : colour (red intensity)
*     QWR2   : colour (green intensity)
*     QWR3   : colour (blue intensity)

      INKWI2=KWI2
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      IF (KWI2.EQ.GREALI) THEN
         DO 1442 I=1,3
          IF(QWRA(I).GT.0.5) THEN
            QWRA(I)=1.0
          ELSE
            QWRA(I)=0.0
          ENDIF
 1442   CONTINUE
      ENDIF
      GOTO 9999

* --------------------------------------------------------------
* Inquire Set Members of Segment Names on Workstation
* --------------------------------------------------------------
 1460 CONTINUE
*     KWI1    : list element requested
*
*     Data returned:
*     KERROR : error indicator
*     KWI1   : no of segment names
*     KWI2   : Nth member of set of stored segments for this
*                workstation
*
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KSGRQ = KSGRQ + 1
      GOTO 9999

* --------------------------------------------------------------
* Inquire locator/stroke/valuator/choice/pick/string device state
* --------------------------------------------------------------
 1470 CONTINUE
* Calcomp has no input devices
      KERROR = 38
      GOTO 9999


*===============================================================
*             INQUIRY FUNCTIONS - WS DESC TABLE
*===============================================================

* --------------------------------------------------------------
* Inquire Workstation Category
* --------------------------------------------------------------
 1700 CONTINUE
*     Data returned:
*     KWI1   : Workstation Category

      KWI1 = GOUTPT
      GOTO 9999

* --------------------------------------------------------------
* Inquire workstation classification
* --------------------------------------------------------------
 1710 CONTINUE
*     Data returned:
*     KWI1   : Workstation Classification

      KWI1 = GVECTR
      GOTO 9999

* --------------------------------------------------------------
* Inquire text facilities
* --------------------------------------------------------------
 1790 CONTINUE
*     Data sent:
*     KWI1   : list element requested
*     Data returned:
*     KERROR : error indicator
*     KWI1   : number of text font and precision pairs
*     KWI2   : Nth element of list of text fonts
*     KWI3   : Nth element of list of text precisions
*     KWI4   : number of available character heights
*     QWR1   : minimum character height
*     QWR2   : maximum character height
*     KWI5   : number of available character expansion factors
*     QWR3   : minimum character expansion factor
*     QWR4   : maximum character expansion factor
*     KWI6   : number of predefined text indices

*     no of text font and precision pairs
      N=INTXFP+1+KFNTMX
      IF (KWI1.LE.0 .AND. KWI1.GT.N) THEN
        KERROR = 2002
      ELSE
*       precisions guaranteed by ws
        IF(KWI1.GE.1 .AND.KWI1.LE.INTXFP) THEN
          KWI2 = ITXF(KWI1)
          KWI3 = ITXP(KWI1)
*       stroke precisions
        ELSE
          I=INTXFP+1
          KWI3=GSTRKP
*         stroke font 1 special case
          IF(KWI1.EQ.I) THEN
            KWI2=1
          ELSEIF(KWI1.GT.I .AND. KWI1.LE.N) THEN
            KWI2=IHFONT(KWI1-I)
          ENDIF
        ENDIF
      ENDIF
      KWI1 = N
      IF(KWKIX.NE.KNIL) THEN
        KWI4 = KCHH(KWKIX)
        KWI5 = KCHXPF(KWKIX)
        KWI6 = KPTXI(KWKIX)
        QWR1 = QMNCHH(KWKIX)
        QWR2 = QMXCHH(KWKIX)
        QWR3 = QMNCHX(KWKIX)
        QWR4 = QMXCHX(KWKIX)
      ELSE
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

* ---------------------------------------------------------------
* Inquire Fill Area facilities
* ---------------------------------------------------------------
 1810 CONTINUE
*     Data sent:
*     KWI1   : list element of interior styles requested
*     KWI2   : list element of hatch styles requested
*     Data returned:
*     KERROR : error indicator
*     KWI1   : number of available fill area interior styles
*     KWI2   : Nth element of list of available fill area interior styles
*     KWI3   : number of available hatch styles
*     KWI4   : Nth element of list of available hatch styles
*     KWI5   : number of predefined fill area indices

*     This workstation supports only HOLLOW and HATCHed fill.
*     Call standard inquiry with list element shifted,
*     as necessary,  to exclude solid to patterned fill.
      IF(KWI1 .GE. 2) KWI1 = KWI1+2
      CALL GKQWK (IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
*     Reset number of avaible fill-styles to exclude patterned fill.
      KWI1 = 2
      GOTO 9999

* ---------------------------------------------------------------
* Inquire predefined pattern representation
* ---------------------------------------------------------------
1840  CONTINUE
      KERROR = 90
      GOTO 9999

* ---------------------------------------------------------------
* Inquire Colour facilities
* ---------------------------------------------------------------
 1850 CONTINUE
*     Data returned:
*     KERROR : error indicator
*     KWI1   : number of available colours
*     KWI2   : colour available
*     KWI3   : number of predefined colour indices

      IF (KWKIX.NE.KNIL) THEN
        KWI3 = KPCI(KWKIX)
      ELSE
        CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
        IF (KERROR.EQ.0) THEN
          KWI3 = INTA(13)
        ENDIF
      ENDIF
*     KWI1 should be set to 9 when the 8th pen is added
      KWI1 = 8
      KWI2 = GCOLOR
      GOTO 9999

* ---------------------------------------------------------------
* Inquire List Element of Available GDPs
* ---------------------------------------------------------------
 1870 CONTINUE
*     Input data :
*     KWI1    : list element requested
*
*     Data returned:
*     KERROR : error indicator
*     KWI1   : no of available Generalised Drawing Primitives
*     KWI2   : Nth element of list of available GDP's

      IF (KWI1.GE.1 .AND. KWI1.LE.4) THEN
        KWI2 = -KWI1
      ELSE
        KERROR = 2002
        KWI2 = KNIL
      ENDIF
      KWI1 = 4
      GOTO 9999

* ---------------------------------------------------------------
* Inquire Generalised Drawing Primitive
* ---------------------------------------------------------------
 1880 CONTINUE
*     Input data :
*     KWI1    : GDP identifier
*
*     Data returned:
*     KERROR : error indicator
*     IDAT   : number of sets of attributes used

      KNIR = 0
      IDAT(1) = KNIL
      IDAT(2) = KNIL
      IDAT(3) = KNIL
      IDAT(4) = KNIL
      IF (KWI1.LE.-1 .AND. KWI1.GE.-4) THEN
         KNIR = 1
         IDAT(1) = GFAATT
*        Arc
         IF (KWI1.EQ.-1)  IDAT(1) = GPLATT
      ELSE
        KERROR = 41
      ENDIF
      GOTO 9999

* --------------------------------------------------------------
* Inquire number of available logical input devices
* --------------------------------------------------------------
 1920 CONTINUE
* Calcomp has no input devices
      KERROR = 38
      GOTO 9999

* --------------------------------------------------------------
* Inquire default locator/stroke/valuator/choice/pick/string device data
* --------------------------------------------------------------
 1930 CONTINUE
* Calcomp has no input devices
      KERROR = 38
      GOTO 9999

* --------------------------------------------------------------
* Common end path for output primitives
* --------------------------------------------------------------
 8888 CONTINUE
*     output makes paper not empty
      KDSMT(KWKIX) = GNEMPT
*     flush buffer if necessary
      IF(KWIO(KWKIX).EQ.GYES) CALL GK0COP

* --------------------------------------------------------------
* Return
* --------------------------------------------------------------
 9999 CONTINUE
      RETURN
      END
