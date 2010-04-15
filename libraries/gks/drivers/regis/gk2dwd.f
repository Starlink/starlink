      SUBROUTINE GK2DWD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)

*---------------------------------------------------------------------
*
*
*  Type of routine:    W/S
*  Author:             GGT
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To output graphical data on Digital VT240 and VT241 Terminal
*
*  MAINTENANCE LOG
*  ---------------
*
*     13/01/84  AS    Original version stabilized
*     19/12/85  GGT/YEE Adapted from T4010 to ACW
*     13/02/87  GGT   Adapted for VT241
*     21/01/88   AA   Added GKMC.PAR now needed by GKHP.PAR
*     22/01/88   AA   IS conversion. Error number changes
*     22/01/88   AA   Initialised variables(NSIGS,INKWI2)
*     18/04/88   AA   Changed to use GKFILS instead of GKFILL
*     26/05/88   AA   Changed request choice entry to expect 6
*     	   	      integers in GKRQIP
*     31/10/89  DLT   Eliminate private common
*     01/11/89  DLT   Complete conversion to IS
*     06/12/91  DLT   Add no clear escape
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
      INTEGER IFILSC
      PARAMETER(IFILSC=1)

*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkinp.par'
      INCLUDE '../../include/gkfls.par'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkfls.cmn'
      INCLUDE '../../include/gkxfd.cmn'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkerr.cmn'
*
*  EXTERNALS
*  ---------
*
      EXTERNAL GK2DLN, GK2DXF, GK2DXC, GK2DCS, GK2DGI
*
*  LOCALS
*  ------
*     ICHUNK Number of points in each chunk of output primitive
*     ICTLZ  ASCII code for ctrl/Z
*     INTXFP No. of font/precision pairs
*     IOFF   Stack offset for output primitive chunks
*     ITXF   Hardware fonts
*     ITXP   Hardware precisions
*     I      Various
*     N      Various
*     ICODE  Key hit returned with cursor position
*     NSEE   Number of character we have room for both in echo area and
*            input buffer
*     INTA   Local integer array with multiple uses
*              - receiving input device state (size 10 max)
*              - receiving WDT info for Inq Text Facil (size 19)
*     IPREC  Stores available text precisions
*     REALA  Local real array with multiple uses
*              - receiving input device state (size 7 max)
*              - receiving WDT info for Inq Text Facil (size 12)
*     XDC,YDC  2-element arrays used to hold single locator value in DC
*            or part of stroke in DC (previous and current point)
*     FLAG   Used to indicate whether linetype simulation is required.
*     VALSTR String of characters representing string input
*
      INTEGER ICHUNK, ICTLZ, INTXFP,NSIGS, INKWI2
      INTEGER ILEFT,ITOP,IFUNC
      PARAMETER ( ICHUNK=200, ICTLZ=26, INTXFP=2)
      INTEGER IOFF, I, N, NLEFT, ICODE, IC, INKWI1
      INTEGER INTA(19),IPREC(KFNTMX),ITXP(INTXFP),ITXF(INTXFP)
      REAL REALA(12), XDC(2),YDC(2)
      LOGICAL FLAG, LCECHO
      CHARACTER VALSTR*80, DIGITS*9
      CHARACTER DOT*1,CHRSET*13
*
*  Offsets in KWKDAT workspace
*
      INTEGER MODE, CHARHI,CHARWI,ICX,ICY, ICPEN
      PARAMETER (MODE = 1, CHARHI = 2, CHARWI = 3, ICX = 4, ICY = 5,
     :           ICPEN = 6)
      INTEGER ICHFLG, ICHSTA, ICHNUM
      PARAMETER (ICHFLG=KMXWKI, ICHSTA=KMXWKI-1, ICHNUM=KMXWKI-2)
      INTEGER ILCFLG, ILCSTA, ILCX, ILCY
      PARAMETER (ILCFLG=KMXWKI-3, ILCSTA=KMXWKI-4)
      PARAMETER (ILCX=KMXWKR, ILCY=KMXWKR-1)

*  Starlink no screen clear escape
      INTEGER INOCLR
      SAVE INOCLR
      DATA INOCLR/GNO/

      DATA DIGITS/'123456789'/,DOT/'.'/
      DATA ITXF /1,1/,
     :     ITXP /GSTRP,GCHARP/

*
*  STACK USAGE
*  -----------
*     POLYLINE and POLYMARKER for transformations
*
*  ERRORS
*  ------
*      32   Specified workstation is not of category MO
*      34   Specified workstation is not of category MI
*      77   Fill area interior style not supported
*      83   Pattern not supported
*      86   Colour index is invalid
*     104   Cannot generate GDP
*     180   Specified function is not supported
*
*---------------------------------------------------------------------
*

*
* Conditional GOTO on entrypoint code

      GOTO (       10,  20,  30,  40,  50,  60,  70,  80,9999,
     :       100, 110, 120, 130, 140, 150, 160, 170, 180, 190,
     :       200, 210, 220, 230, 240, 250, 260, 270, 280,9999,
     :      9999, 310, 320, 330,9999,9999,9999,9999,9999,9999,
     :      9999, 410, 410, 410, 410, 410, 410, 410, 410, 410,
     :       410, 410,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999, 610, 620, 630, 640, 650, 660, 670, 680, 690,
     :       700, 710, 720, 730, 740, 750, 760, 770, 780, 790,
     :       800, 810,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999, 910, 920, 930) IENT

      GOTO (1111,1111,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1111,1111,1111,1330,1111,1111,1111,1370,1380,1111,
     :      1111,1410,1420,1111,1440,1111,1111,1111,1111,1111,
     :      1111,1510,1111,9999,9999,9999,9999,9999,9999,9999,
     :      9999,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1111,1710,1111,1111,1111,1111,1111,1111,1111,1790,
     :      1111,1810,1111,1111,1840,1850,1111,1111,1111,1111,
     :      1111,1111,1111,1111,1111,1111,1960,1970,1111) IENT-119

      GOTO 9999


* Open workstation
   10 CONTINUE
* Set up workstation state list and workstation description table
      CALL GKIWSL(KWKIX,KWKTYP)
      IF (KERROR.EQ.0) THEN
        KCID(KWKIX) = KWI1
        KDFM(KWKIX) = GBNIG
        KWIO(KWKIX) = GNO
        KIMRGM(KWKIX) = GSUPPD
* Ask operating system to make a connection
        CALL GKIOOP(KFWKT,KCID(KWKIX),KWCID(KWKIX))
        IF (KERROR.EQ.0) THEN

* Initialise flags used by escapes -1 and -2
         KWKDAT(ICHFLG, KWKIX) = KNIL
         KWKDAT(ILCFLG, KWKIX) = KNIL
*
* initialise the workstation
*
         CALL GK2DIN(KWKTYP, INOCLR)
*
*	Clear the screen
*
      IF (INOCLR.EQ.GNO) CALL GKIOCO(KIOPB,'S(E)',NLEFT)
*
*	Flush the buffer
*
      CALL GKIOCO(KIOSN,' ',NLEFT)
*
* Initialise variables for linetype simulation
          QWOLDX(KWKIX) = -99.0
          QWOLDY(KWKIX) = -99.0
        ENDIF
      ENDIF
      KWI1 = GOUTIN
      GOTO 9999



* Close workstation
   20 CONTINUE
      IF( KWI1.EQ.1 ) THEN
        KWDONE=KRFUSE
      ELSE
* Reset the graphics and text colours to default
        CALL GK2DSP(0)
        CALL GK2DLT(1)
        CALL GKIOCO(KIOSN,' ',NLEFT)
*
        CALL GKIOCL(KWKTYP,KCID(KWKIX),KWCID(KWKIX))
        CALL GKCWSL(KWKIX)
        CALL GKSLDL(KSSGPT(KWKIX))
      ENDIF
      GOTO 9999


* Clear workstation
   30 CONTINUE
      IF( KWI1.EQ.2 ) THEN
        KWDONE=KRFUSE
        CALL GKSLDL(KSSGPT(KWKIX))
      ENDIF
      GOTO 9999



* Redraw all segments on workstation
   40 CONTINUE
      KWDONE = KRFUSE
      GOTO 9999



* Update workstation
   50 CONTINUE
      KWDONE = KRFUSE
      GOTO 9999



* Set deferral state
   60 CONTINUE
      KDFM(KWKIX) = KWI1
      KIMRGM(KWKIX) = KWI2
      IF (KWI1.EQ.GASAP) THEN
        KWIO(KWKIX) = GYES
        CALL GKIOCO(KIOSN,' ',NLEFT)
      ELSE
        KWIO(KWKIX) = GNO
      ENDIF
      IF (KIMRGM(KWKIX).EQ.GALLOW .AND. KNFAUP(KWKIX).EQ.GYES .AND.
     :    KDSMT(KWKIX).EQ.GNEMPT) THEN
        KWRGN(KWKIX) = .TRUE.
        KRGN = .TRUE.
      ENDIF
      GOTO 9999



* Do deferred output actions
   70 CONTINUE
      CALL GKIOCO(KIOSN,' ',NLEFT)
      KWI1 = KNFAUP(KWKIX)
      GOTO 9999



* Clear display surface
   80 CONTINUE
      IF (KWI1.EQ.GALWAY.OR.KDSMT(KWKIX).EQ.GNEMPT) CALL GK2DCL
      CALL GKWCLD

      GOTO 9999


* Message
  100 CONTINUE
      GOTO 9999


* Escape
  110 CONTINUE
*    Starlink no screen clear escape
      IF (KWI1.EQ.-3) THEN
         INOCLR = KWI2
         GOTO 9999
      END IF

      CALL GKESC
      GOTO 9999


* Polyline
  120 CONTINUE
      IF ( KCVIS.EQ.GINVIS) GO TO 9999
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN
         FLAG = .FALSE.

* Select linetype
         CALL GK2DLT(KWLNTY(KWKIX))

* Select pen colour
        CALL GK2DSP(KWPLCI(KWKIX))
        N = ICHUNK
        DO 122 I=1,NRD,ICHUNK-1
          IF (NRD-I.LT.ICHUNK-1) N = NRD-I + 1
          IF (N.EQ.1) GOTO 122
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
          CALL GKLCLP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),FLAG,0.0,
     :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK2DLN)
  122   CONTINUE
        CALL GKSTDA(KREALS,IOFF)

*    Restore linetype
         IF (KWLNTY(KWKIX).NE.1) CALL GK2DLT(1)

      ENDIF

      GOTO 8888



* Polymarker
  130 CONTINUE
      IF ( KCVIS.EQ.GINVIS) GO TO 9999
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN
* Select pen colour
        CALL GK2DSP(KWPMCI(KWKIX))
        N = ICHUNK
        DO 132 I=1,NRD,ICHUNK
          IF (NRD-I.LT.ICHUNK) N = NRD-I + 1
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
          CALL GKMTYP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),
     :                   KWMKTY(KWKIX),QNMMKS(KWKIX),QWMKSZ(KWKIX),
     :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK2DLN)
  132   CONTINUE
        CALL GKSTDA(KREALS,IOFF)
      ENDIF
      GOTO 8888



* Text
  140 CONTINUE
      IF ( KCVIS.EQ.GINVIS) GO TO 9999
* Select pen colour
      CALL GK2DSP(KWTXCI(KWKIX))
      IF(KWTXPR(KWKIX) .EQ. GSTRKP) THEN
         CALL GKXDWO(NID,IDAT,GK2DLN)
      ELSE
         CALL GKXDWC(NID,IDAT,QWCHRX(KWKIX),QWCHRY(KWKIX),
     :               GK2DXF,GK2DXC)
      ENDIF
      GOTO 8888


* Fill area
  150 CONTINUE
      IF ( KCVIS.EQ.GINVIS) GO TO 9999
* Select pen colour
      CALL GK2DSP(KWFACI(KWKIX))
      CALL GKFILS(NRD,RX,RY,IFILSC,GK2DLN,GK2DLN)
      GOTO 8888


* Cell array ... do minimal simulation
  160 CONTINUE
      IF ( KCVIS.EQ.GINVIS) GO TO 9999
* Select pen colour - use default colour
      CALL GK2DSP(1)
      CALL GKCASM(GK2DLN)
      GOTO 8888


* GDP
  170 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
* First, check GDP identifier
      IF (KWI1.EQ.0) THEN
        KERROR = 102
        GOTO 9999
      ELSE IF (KWI1.LT.-4.OR.KWI1.GT.-1) THEN
        KERROR = 104
        GOTO 9999
      ELSE IF (KWI1.EQ.-1) THEN
* Arc
*          (Arc GDP uses Polyline attributes)
        CALL GK2DSP(KWPLCI(KWKIX))
        CALL GKCRCS(KWI1,NRD,RX,RY,1,.FALSE.,1.0,GK2DLN, GK2DLN)
      ELSE
* Filled chord, pie or circle
*   (Chord, Pie Or Circle GDP's use Fill Area attr.)
        CALL GK2DSP(KWFACI(KWKIX))
        CALL GKCRCS(KWI1,NRD,RX,RY,IFILSC,.FALSE.,1.0,
     :       GK2DLN,GK2DLN)
      ENDIF
      GOTO 8888


* Set polyline attributes
  180 CONTINUE
      CALL GKDPLB
* Need to check because individual settings won't have been checked.
      IF (KWLNTY(KWKIX).LT.0.OR.KWLNTY(KWKIX).GT.5) KWLNTY(KWKIX) = 1
      IF (KWPLCI(KWKIX).GE.KPCI(KWKIX)) KWPLCI(KWKIX) = 1
      GOTO 9999


* Set polymarker attributes
  190 CONTINUE
      CALL GKDPMB
* Need to check because individual settings won't have been checked.
      IF (KWMKTY(KWKIX).LT.0.OR.KWMKTY(KWKIX).GT.5) KWMKTY(KWKIX) = 3
      IF (KWPMCI(KWKIX).GE.KPCI(KWKIX)) KWPMCI(KWKIX) = 1
      GOTO 9999


* Set text attributes
  200 CONTINUE
      CALL GKDTXB
* Need to check because individual settings won't have been checked.
      IF (KWTXCI(KWKIX).GE.KPCI(KWKIX)) KWTXCI(KWKIX) = 1
*     done if stroke precision
      IF(KWTXPR(KWKIX).EQ.GSTRKP) GOTO 9999
      KWTXFN(KWKIX)=1
*     character rotation
      QWCHRX(KWKIX)=QWCHWX(KWKIX)
      QWCHRY(KWKIX)=QWCHWY(KWKIX)
      GOTO 9999

* Set fill area attributes
  210 CONTINUE
      CALL GKDFAB
* Need to check because individual settings won't have been checked.
      IF (KWFAIS(KWKIX).EQ.GPATTR) KWFAIS(KWKIX) = GHOLLO
      IF (KWFAIS(KWKIX).EQ.GHATCH) THEN
         IF (KWFASI(KWKIX).GT.-1 .OR. KWFASI(KWKIX).LT.-10)
     :      KWFASI(KWKIX) = -1
      END IF
      IF (KWFACI(KWKIX).GE.KPCI(KWKIX)) KWFACI(KWKIX) = 1
      GOTO 9999



* Set pick identifier
  220 CONTINUE
      GOTO 9999



* Set polyline representation
  230 CONTINUE
      INTA(1) = 5
      CALL GKSRPL(1,INTA,.TRUE.)
      GOTO 9999



* Set polymarker representation
  240 CONTINUE
      CALL GKSRPM(0,INTA,.TRUE.)
      GOTO 9999



* Set text representation
  250 CONTINUE
      IF( KWI3.EQ.GSTRKP ) THEN

*       Stroke Precision
*       Make sure that fonts are available
        IF( KDBFLS.EQ.KFLNA ) THEN
          KERROR=-1042
          GOTO 9999
        ENDIF
        IF( KDBFLS.EQ.KFLCL ) CALL GKXON
        IF( KERROR.NE.0 ) GOTO 9999
        DO 255 I=1,KFNTMX
          IPREC(I) = GSTRKP
  255   CONTINUE
        CALL GKSRTX(KFNTMX,KHFONT,IPREC,.FALSE.)
      ELSE

*       String or Char precision
        IPREC(1)=KWI3
        INTA(1)=1
        CALL GKSRTX(1,INTA,IPREC,.FALSE.)
      ENDIF
      GOTO 9999



* Set fill area representation
  260 CONTINUE
      IF (KWI2.NE.GPATTR) THEN
        CALL GKSRFA(.TRUE.)
      ELSE
        KERROR = 83
      ENDIF
      GOTO 9999



* Set pattern representation
  270 CONTINUE
* Pattern not supported
      KERROR = 90
      GOTO 9999



* Set colour representation
  280 CONTINUE
* See if colour index valid
      IF (KWI1.GE.0 .AND. KWI1.LT.KPCI(KWKIX)) THEN
        QHP(KHPXR(KCTBPT(1,KWKIX))+KWI1) = QWR1
        QHP(KHPXR(KCTBPT(2,KWKIX))+KWI1) = QWR2
        QHP(KHPXR(KCTBPT(3,KWKIX))+KWI1) = QWR3
        CALL GK2DSC(KWI1)
      ELSE
        KERROR = 93
      ENDIF
      GOTO 9999



* Normalisation transformation
  310 CONTINUE
      CALL GKWKC4
      GOTO 9999



* Set workstation window
  320 CONTINUE
      CALL GKSWKW
      GOTO 9999



* Set workstation viewport
  330 CONTINUE
      CALL GKSWKV
      GOTO 9999



* Segment entrypoints *
  410 CONTINUE
      CALL GKSGWK(IENT,.FALSE.)
      GOTO 9999



* Initialise locator
  610 CONTINUE
      GOTO 9999



* Initialise stroke
  620 CONTINUE
      GOTO 9999



* Initialise valuator
  630 CONTINUE
      GOTO 9999



* Initialise choice
  640 CONTINUE
      GOTO 9999



* Initialise pick
  650 CONTINUE
      GOTO 9999



* Initialise string
  660 CONTINUE
      GOTO 9999



* Set input operating modes
  670 CONTINUE
      GOTO 9999



* Set input mode
  680 CONTINUE
      CALL GKSIPM
      GOTO 9999



* Request locator
  690 CONTINUE

      IF (KWKDAT(ILCFLG,KWKIX).NE.KNIL .AND. KWI1.EQ.1) THEN
*        Escape has been used to associate choice device 2 with
*        locator device 1.
*        If no stored data, need to request operator action
         IF (KWKDAT(ILCFLG,KWKIX) .EQ. GNO) CALL GKRQES(GK2DCS)
*        Pass locator data back
         KWI1 = KWKDAT(ILCSTA, KWKIX)
         RX(1) = QWKDAT(ILCX, KWKIX)
         RY(1) = QWKDAT(ILCY, KWKIX)
*        Clear stored locator data flag
         KWKDAT(ILCFLG, KWKIX) = GNO
      ELSE
*        Normal locator input
         CALL GKRQLC(GK2DCS, RX(1), RY(1))
      ENDIF
      GOTO 9999


* Request stroke
  700 CONTINUE
*
*
*   First get stroke device information
*
      CALL GKRQIP(GSTROK,KWI1,10,10,INTA,REALA)
*
*   Set echoing according to echo switch
*
      IF( INTA(KIPE).EQ.GECHO ) THEN
         LCECHO = .TRUE.
      ELSE
         LCECHO = .FALSE.
      ENDIF
*
      I = 1
  702 CONTINUE
*
*   Read key hit and cursor.
*
      CALL GK2DCS(LCECHO,ICODE, XDC(2),YDC(2))

*         Branch on key hit (ICODE).
*         If break (ctrl/Z), then we quit with status=none
*         If normal terminator (ICODE=13), then finish stroke, not
*         accepting the point.
*         Otherwise inspect the point returned and
*            if OK go and get next one
*            if not OK try again (without stepping on)
      IF (ICODE.EQ.ICTLZ) THEN
         KWI1=GNONE
         GOTO 709
      ELSE IF (ICODE.EQ.13) THEN
         GOTO 706
      ELSE
         CALL GKTDN(1,XDC(2),YDC(2),RX(I),RY(I))
         IF (KERROR.NE.0) THEN
            KERROR=0
            GOTO 702
         ENDIF
         IF (I.GT.1 .AND. LCECHO) THEN
            CALL GK2DLN(2,XDC,YDC)
         ELSE
            CALL GK2DDR(INT(XDC(2)),INT(YDC(2)),0)
         ENDIF
         XDC(1)=XDC(2)
         YDC(1)=YDC(2)
      ENDIF
*
      I = I + 1
      IF (I.LE.NRD) GOTO 702
  705 CONTINUE

*   Here, stroke is full. Set KNRR to be maxm # of points
*   Mark stroke as valid
      KNRR=NRD
      GOTO 707

*   Here, came out of loop on receiving line terminator.
*   Point indexed by I-1 is the last we received.
  706 KNRR=I-1

*   Here, KNRR (number of points in stroke) is set. Now set status to be
  707 KWI1=GOK
*   Drop to 709

  709 CONTINUE
*
*
      GOTO 9999



* Request valuator
  710 CONTINUE
*   First get valuator device information
      CALL GKRQIP(GVALUA,KWI1,4,7,INTA,REALA)

*   Display prompt in echo area
      CALL GK2DPD(REALA,'Real value [0.0,1.0]: ')

*   Set echoing according to echo switch
      IF (INTA(KIPE).EQ.GECHO ) THEN
         IFUNC=KIOEN
      ELSE
         IFUNC=KIONN
      ENDIF
*
*
*
*   Iinitialised status = none , i.e. break function is triggered
      KWI1=GNONE
      VALSTR=' '
      CALL GKIOCO(KIOSN,' ',NLEFT)
      CALL GKIOCI(IFUNC,0,' ',VALSTR,NSIGS)
      IF (VALSTR.EQ.CHAR(ICTLZ)) GOTO 715
      IF (NSIGS.EQ.0) GOTO 715
*
*   Trap illegal characters
*
      KWI1=2
      IC=NSIGS
      IF (.NOT.(INDEX(VALSTR(1:IC),'1').GE.1.OR.
     *          INDEX(VALSTR(1:IC),'2').GE.1.OR.
     *          INDEX(VALSTR(1:IC),'3').GE.1.OR.
     *          INDEX(VALSTR(1:IC),'4').GE.1.OR.
     *          INDEX(VALSTR(1:IC),'5').GE.1.OR.
     *          INDEX(VALSTR(1:IC),'6').GE.1.OR.
     *          INDEX(VALSTR(1:IC),'7').GE.1.OR.
     *          INDEX(VALSTR(1:IC),'8').GE.1.OR.
     *          INDEX(VALSTR(1:IC),'9').GE.1.OR.
     *          INDEX(VALSTR(1:IC),'0').GE.1))  GOTO 715
*

      CHRSET='1234567890-+.'

      DO 718 I=1,IC
         IF (INDEX(CHRSET,VALSTR(I:I)).LE.0) GOTO 715
718   CONTINUE

*
*   Otherwise interpret the result.
      READ(VALSTR,'(F7.0)') QWR1
*
      KWI1=GOK
*
* Clear the echo area and reset autowrap
*
  715 ILEFT =INT(REALA(KIPEXL)/KWKDAT(CHARWI,KWKIX))
      ITOP  =INT(REALA(KIPEYB)/KWKDAT(CHARHI,KWKIX))
*
      WRITE(*,744)CHAR(27)
*
      WRITE(*,745)CHAR(27),ITOP,ILEFT,CHAR(27)
*
      GOTO 9999



* Request choice
  720 CONTINUE
      IF (KWKDAT(ICHFLG,KWKIX) .NE. KNIL .AND. KWI1.EQ.2) THEN
*        Escape has been used to associate locator device 1 with
*        choice device 2.
*        If no stored choice data, need to request operator action
         IF (KWKDAT(ICHFLG,KWKIX) .EQ. GNO) CALL GKRQES(GK2DCS)
*        Pass choice data back
         KWI1 = KWKDAT(ICHSTA, KWKIX)
         KWI2 = KWKDAT(ICHNUM, KWKIX)
*        Clear stored choice data flag
         KWKDAT(ICHFLG, KWKIX) = GNO
      ELSE
*      Normal choice input
         CALL GKRQCH(KWI1,KWI2,GK2DGI)
      ENDIF
      GOTO 9999



* Request pick
  730 CONTINUE
      GOTO 9999



* Request string
  740 CONTINUE
*   Get string device information
      CALL GKRQIP(GSTRIN,KWI1,9,4,INTA,REALA)

*   Display prompt in echo area
      CALL GK2DPD(REALA, 'String: ')
*
*   Set echoing according to echo switch
      IF (INTA(KIPE).EQ.GECHO ) THEN
         IFUNC=KIOEN
      ELSE
         IFUNC=KIONN
      ENDIF
*
      CALL GKIOCO(KIOSN,' ',NLEFT)
      CALL GKIOCI(IFUNC,0,' ',VALSTR,NSIGS)
*
* If there are any characters then convert to integer
*
      IF (NSIGS.EQ.0) THEN
         KNIR=0
         KWI1=GNONE
      ELSE
         DO 743 I=1,NSIGS
          IDAT(I)=ICHAR(VALSTR(I:I))
  743    CONTINUE
         KNIR=NSIGS
         KWI1=GOK
      ENDIF
*
* Clear the echo area and reset autowrap
*
      ILEFT =INT(REALA(KIPEXL)/KWKDAT(CHARWI,KWKIX))
      ITOP  =INT(REALA(KIPEYB)/KWKDAT(CHARHI,KWKIX))
*
      WRITE(*,744)CHAR(27)
  744 FORMAT(1H+,A,'[?7h',$)
*
      WRITE(*,745)CHAR(27),ITOP,ILEFT,CHAR(27)
  745 FORMAT(1H+,A,'[',I2.2,';',I3.3,'H',A,'[K')

      GOTO 9999


* Sample locator
  750 CONTINUE
      GOTO 9999



* Sample stroke
  760 CONTINUE
      GOTO 9999



* Sample valuator
  770 CONTINUE
      GOTO 9999



* Sample choice
  780 CONTINUE
      GOTO 9999



* Sample pick
  790 CONTINUE
      GOTO 9999



* Sample string
  800 CONTINUE
      GOTO 9999



* Flush device events
  810 CONTINUE
      GOTO 9999



* Write item to GKSM
  910 CONTINUE
      KERROR = 32
      GOTO 9999



* Get item type from GKSM
  920 CONTINUE
      KERROR = 34
      GOTO 9999



* Read item from GKSM
  930 CONTINUE
      KERROR = 34
      GOTO 9999



* Inquire everything *
 1111 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999



* Inquire polyline representation
 1330 CONTINUE
      IF (KWI2.EQ.GSET) THEN
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
        QWR1 = 1.0
      ENDIF
      GOTO 9999



* Inquire text representation
C THIS WILL CHANGE CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 1370 CONTINUE
      IF (KWI2.EQ.GSET) THEN
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
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
     :                  RX,RY,GK2DXF)

      ENDIF
      GOTO 9999




* Inquire list of pattern indices
 1410 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999



* Inquire pattern representation
 1420 CONTINUE
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
*
* Inquire pick device state
 1510 CONTINUE
      GOTO 9999



* Inquire workstation classification
 1710 CONTINUE
      KWI1 = GRASTR
      GOTO 9999



* Inquire text facilities ... on entry KWI1 specifies list element reque
* Allow for string and char precision font (number 1) explicitly
 1790 CONTINUE
      IF( KWI1.GT.KFNTMX+2 ) THEN
        KERROR=2002
        KWI1=KFNTMX+2
        GOTO 9999
      ENDIF
      IF( KWI1.GT.KFNTMX ) THEN

*       String or Char precision font
          IF( KWI1.EQ.KFNTMX+1 ) KWI3 = GSTRP
          IF( KWI1.EQ.KFNTMX+2 ) KWI3 = GCHARP
          KWI2 = 1
      ELSE

*       Stroke precision font
*       Make sure that fonts are available
          IF( KDBFLS.EQ.KFLNA ) THEN
            KERROR=-1042
            GOTO 9999
          ENDIF
          IF( KDBFLS.EQ.KFLCL ) CALL GKXON
          IF( KERROR.NE.0 ) GOTO 9999
          KWI2 = KHFONT(KWI1)
          KWI3 = 2
      ENDIF
      KWI1 = KFNTMX+2
      IF (KWKIX.NE.KNIL) THEN
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

* Inquire fill area facilities
 1810 CONTINUE
* Only 3 interior styles supported on this workstation
* Check for valid list element of interior styles
      IF (KWI1.LT.1.OR.KWI1.GT.3) THEN
        KERROR = 2002
* If have interior style hatch, check hatch style element is valid
      ELSE IF (KWI1.EQ.3.AND. (KWI2.LT.1.OR.KWI2.GT.10)) THEN
        KERROR = 2002
      ELSE
        INKWI1 = KWI1
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
        IF (KERROR.EQ.0) THEN
          KWI1 = 3
          IF (INKWI1.EQ.1) THEN
            KWI2 = GHOLLO
          ELSE IF (INKWI1.EQ.2) THEN
            KWI2 = GSOLID
          ELSE
            KWI2 = GHATCH
          ENDIF
        ENDIF
      ENDIF
      GOTO 9999

* Inquire predefined pattern representation
 1840 CONTINUE
      KERROR = 90
      GOTO 9999



* Inquire colour facilities
 1850 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KWI2 = GCOLOR
      KWI1 = 4
      GOTO 9999

*
* Inquire default choice device data
 1960 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KWI4=9
      GOTO 9999



* Inquire default pick device data
 1970 CONTINUE
      GOTO 9999



*   Here after all output primitives to sort out buffering
 8888 CONTINUE
      KDSMT(KWKIX) = GNEMPT
      IF (KWIO(KWKIX).EQ.GYES) CALL GKIOBO(KIOSN,1,KDAT,NLEFT)

 9999 CONTINUE

      END
