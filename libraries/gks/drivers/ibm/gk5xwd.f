C# IL>=a, OL>=0
      SUBROUTINE GK5XWD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:             GMC
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*
*
* Driver routine for Xerox 4050
*
*  MAINTENANCE LOG
*  ---------------
*
*     11/05/88  DSG  IS conversion.
*     12/05/88  DSG  GK0TXS declared EXTERNAL
*                    GKMC parameter file included (needed by revised
*                    GKHP parameter file).
*     01/07/88  DSG  GK5XCS: VECBUF(15) and (16) set for EPIC 3.1
*     08/09/88  DSG  GK5XIO: page size changed from 10.75 by 8.25 to
*                    11.31 by 8.0
*     12/09/88  DSG  GK5XCL: anchor point moved from IYPXLS-400 to
*                    IYPXLS-100
*                    GK5XIO: MXLINE changed from -MXPXLX to -IYPXLS
*     15/09/88  DSG  GK5XIO: NSCAN reduced from 150 to the Epic 3.1
*                    value of 144.
*                    GK5XCL: anchor point to IYPXLS-1
*     25/07/90  PLP  Removed unused locals.
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
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkinp.par'
      INCLUDE '../../include/gkfls.par'
      INCLUDE '../../include/gkmc.par'
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
      EXTERNAL GK5XLN,GK5XRO
      EXTERNAL GK0TXF
* If TEXT uses CHAR precision, the second EXTERNAL statement will be nee
*    EXTERNAL GK5XLN,GK5XRO,GK5XXF,GK5XXC
*
*  LOCALS
*  ------
*     IBAUD  Offset in KWKDAT of baud rate for this terminal
*     INKWI1 Holds initial value of KWI1 in inquire fill area facils
*     ICHUNK Number of points in each chunk of output primitive
*     ICTLZ  ASCII code for ctrl/Z
*     INTXFP No. of font/precision pairs
*     IOFF   Stack offset for output primitive chunks
*     ITXF   Hardware fonts
*     ITXP   Hardware precisions
*     I      Various
*     N      Various
*     NLEFT  Returned by output buffering routine (amount of buffer left
*     INTA   Local integer array with multiple uses
*              - receiving input device state (size 10 max)
*              - receiving WDT info for Inq Text Facil (size 19)
*     IPREC  Stores available text precisions
*     REALA  Local real array with multiple uses
*              - receiving input device state (size 7 max)
*              - receiving WDT info for Inq Text Facil (size 12)
*     FLAG   Used to indicate whether linetype simulation is required.
*     IFILSC Fill area scale factor to pass to utility
*
      INTEGER    IBAUD, ICHUNK, ICTLZ, INTXFP, IFILSC
      PARAMETER (IBAUD=1, ICHUNK=200, ICTLZ=26, INTXFP=2, IFILSC=1)
      INTEGER INKWI1, IOFF, I, N, NLEFT
      INTEGER INTA(19),IPREC(KFNTMX),ITXP(INTXFP),ITXF(INTXFP)
      REAL REALA(12)
      LOGICAL FLAG
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

* Open file to Xerox
          CALL GK5XIO
* Initialise variables for linetype simulation
          QWOLDX(KWKIX) = -99.0
          QWOLDY(KWKIX) = -99.0
      ENDIF
      KWI1 = GOUTPT
      GOTO 9999



* Close workstation
   20 CONTINUE
      IF( KWI1.EQ.1 ) THEN
        KWDONE=KRFUSE
      ELSE
*       CALL GKIOBO(KIOSN,1,KDAT,NLEFT)
*       CALL GKIOCL(KWKTYP,KCID(KWKIX),KWCID(KWKIX))
*       CALL GKCWSL(KWKIX)
*       CALL GKSLDL(KSSGPT(KWKIX))
        CALL GK5XCS
      ENDIF
      GOTO 9999



* Clear workstation
   30 CONTINUE
*     WRITE(*,*) 'CLEAR WORKSTATION entry point'
*     WRITE(*,*) 'KWI1, KWI2 = ', kwi1, kwi2
      IF( KWI1.EQ.2 ) THEN
*       indicate clear surface entry expected
        KWDONE=KRFUSE
*       delete workstation segments
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

*     TRIED THIS, BUT IT DID NOT WORK:
*     Previous line commented out and next two lines inserted
*     in an attempt to get round 'graphic off page' with GUWK.
*        KWDONE=KACEPT
*        CALL GK5XCL
      GOTO 9999



* Set deferral state
   60 CONTINUE
      KDFM(KWKIX) = KWI1
      KIMRGM(KWKIX) = KWI2
      IF (KWI1.EQ.GASAP) THEN
        KWIO(KWKIX) = GYES
        CALL GKIOBO(KIOSN,1,KDAT,NLEFT)
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
      CALL GKIOBO(KIOSN,1,KDAT,NLEFT)
      KWI1 = KNFAUP(KWKIX)
      GOTO 9999



* Clear display surface
   80 CONTINUE
*     write(*,*) 'CLEAR DISPLAY SURFACE entry point '
*     WRITE(*,*) 'kKWI, KDSMT = ', kwi1, kdsmt(kwkix)
      IF (KWI1.EQ.GALWAY.OR.KDSMT(KWKIX).EQ.GNEMPT) CALL GK5XCL
*     reset current workstation window and viewport
      CALL GKWCLD
      GOTO 9999



* Message
  100 CONTINUE
      GOTO 9999



* Escape
  110 CONTINUE
      KERROR = 180
      GOTO 9999



* Polyline
  120 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 8888
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN
        IF (KWLNTY(KWKIX).EQ.1) THEN
          FLAG = .FALSE.
        ELSE
          FLAG = .TRUE.
        ENDIF
        N = ICHUNK
        DO 122 I=1,NRD,ICHUNK-1
          IF (NRD-I.LT.ICHUNK-1) N = NRD-I + 1
          IF (N.EQ.1) GOTO 122
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
          CALL GKLCLP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),FLAG,25.0,
     :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK5XLN)
  122   CONTINUE
        CALL GKSTDA(KREALS,IOFF)
      ENDIF
      GOTO 8888



* Polymarker
  130 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 8888
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN
        N = ICHUNK
        DO 132 I=1,NRD,ICHUNK
          IF (NRD-I.LT.ICHUNK) N = NRD-I + 1
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
          CALL GKMTYP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),
     :                   KWMKTY(KWKIX),QNMMKS(KWKIX),QWMKSZ(KWKIX),
     :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK5XLN)
  132   CONTINUE
        CALL GKSTDA(KREALS,IOFF)
      ENDIF
      GOTO 8888



* Text
  140 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 8888
* If precision nedds CHAR or STRING? then GKXDWC will be required
* along with two routines GK5XXF and GK5XXC. See the EXTERNAL statement

*     IF(KWTXPR(KWKIX) .EQ. GSTRKP) THEN
         CALL GKXDWO(NID,IDAT,GK5XLN)
*     ELSE
*        CALL GKXDWC(NID,IDAT,QWCHRX(KWKIX),QWCHRY(KWKIX),
*    :               GK5XXF,GK5XXC)
*     ENDIF
      GOTO 8888



* Fill area
  150 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 8888
      CALL GKFILS(NRD,RX,RY,IFILSC,GK5XLN,GK5XLN)
      GOTO 8888



* Cell array ... do minimal simulation
  160 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 8888
*     CALL GKCASM(GK5XLN)
*
* DO PROPER CELL ARRAY, CALLING GKCELL
*
         CALL GKCELL( NID,IDAT,GK5XRO )
      GOTO 8888



* GDP
  170 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 8888
* First, check GDP identifier
      IF (KWI1.EQ.0) THEN
        KERROR = 102
        GOTO 9999
      ELSE IF (KWI1.LT.-4.OR.KWI1.GT.-1) THEN
        KERROR = 104
        GOTO 9999
      ELSE IF (KWI1.EQ.-1) THEN
* Arc
        CALL GKCRCS(KWI1,NRD,RX,RY,IFILSC,.FALSE.,20.0,
     :       GK5XLN, GK5XRO)
      ELSE
* Filled chord, pie or circle
        CALL GKCRCS(KWI1,NRD,RX,RY,IFILSC,.FALSE.,20.0,
     :       GK5XLN,GK5XRO)
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
      IF (KWMKTY(KWKIX).GT.5) KWMKTY(KWKIX) = 3
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
     :    KWFASI(KWKIX) = -1
      ENDIF
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
      GOTO 9999



* Request stroke
  700 CONTINUE
      GOTO 9999



* Request valuator
  710 CONTINUE
      GOTO 9999



* Request choice
  720 CONTINUE
      GOTO 9999



* Request pick
  730 CONTINUE
      GOTO 9999



* Request string
  740 CONTINUE
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
     :                  RX,RY,GK0TXF)

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



* Inquire colour representation
 1440 CONTINUE
      IF (KWI2.EQ.GREALI) THEN
        QWR1 = 0.0
        QWR2 = 1.0
        QWR3 = 0.0
      ELSE
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      ENDIF
      GOTO 9999



* Inquire pick device state
 1510 CONTINUE
      GOTO 9999



* Inquire workstation classification
 1710 CONTINUE
      KWI1 = GVECTR
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
      KWI2 = GMONOC
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KWI1=2
      GOTO 9999



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





         SUBROUTINE GK5XIO
      INCLUDE '../../include/check.inc'
      INCLUDE '../../include/gkxer.cmn'

*
* LOCALS
*
* XPAGE  - X PAGE SIZE (NOMINALLY IN INCHES)
* YPAGE  - Y PAGE SIZE (NOMINALLY IN INCHES)
* IDUMMY - DUMMY VARIABLE
*

         REAL XPAGE,YPAGE
*        PARAMETER (XPAGE=8.25,YPAGE=10.75)

         PARAMETER (XPAGE=8.00,YPAGE=11.31)

         INTEGER IDUMMY, MAXREC
         INTEGER LOR, IROT

         IOVUN = 1
         OUTREC = 1
* OUTREC WILL BE IMCREMENTED IN GK5XCB

         IWORD = 1
         RECSIZ = 1024
*        IOIRBC = 4*RECSIZ
         IOIRBC = 0
         IOIRMX = 0
*        IOIRMX = 255
         IOTRBC = 0
* MAXREC doulbled to 5000: 17/07/89
         MAXREC = 5000
         MAXPAT = 0
         XNSCAN = 144
         RESOL = 300.0
         UNITS = 1.0
*        MSBIT = IROT(1,-1)
         MSBIT = 32768
         MSK16 = LOR(1,IROT(32767,1))
*        WRITE( 33,1111 ) MSBIT,MSK16
*1111    FORMAT( 1X,'MSBIT,MSK16 = ',2I15 )
         IDRAW0 = 1
*
* OUTPUT NEEDS TO BE VIA PRINTER TO HARD COPY
*
         IMGDSP = -2
*        IMGDSP = 1
         IXPXLS = INT( RESOL*XPAGE + 0.5 )
         IYPXLS = INT( RESOL*YPAGE + 0.5 )
         MXPXLX = IXPXLS
*        MXLINE = IYPXLS
******** MXLINE = -MXPXLX
         MXLINE = -IYPXLS
*        MXLINE = -1
         NUMVEC = 0
         BUFSIZ = 1536
         CALL GK5XCB

* OPEN FILE
         IOVRBC = 4*RECSIZ
         CALL IODADF( IOVUN,MAXREC,IOVRBC,IDUMMY )
*        CALL IODAD( IOVUN,MAXREC,IOVRBC,IDUMMY )

         RETURN
         END





         SUBROUTINE GK5XCS
      INCLUDE '../../include/check.inc'
      INCLUDE '../../include/gkxer.cmn'

*
* LOCALS
*
*

         INTEGER N , IPCC, IMRGTX

* Check for END-OF-DRAWING VECTOR. If only a single frame, GK5XCL
* wont have been called.
* Checking the buffer and flushing occurs in GK5XCL

*     write (*,*) 'GK5XCS entered '
         CALL GK5XCL

* Write out 1st record of complete file

*        WRITE( 33,1122 )
*1122    FORMAT( 1X,'*** GK5XCS, CLOSE WORKSTATION ***' )
         VECBUF(1) = 31

* Set Image Disposition Indicator to tell EPIC Phase II to merge
* DJDE's into the transportable file (default value is zero - inhibit).
         IMRGTX = 0
* Set printer Control Character Indicator to select a format
* statement that adds a blank character before each image record.
* (default value is zero - inhibit).
         IPCC = 1

* OUTREC points to the next record, therefore it is set for output in
* the first record.

         VECBUF(2) = OUTREC
         VECBUF(3) = IDRAW0
         VECBUF(4) = IMGDSP
         VECBUF(5) = XNSCAN
         VECBUF(6) = 10.0*RESOL*UNITS + 0.5
         VECBUF(7) = MXPXLX
         VECBUF(8) = MXLINE
         VECBUF(9) = IXPXLS
         VECBUF(10) = IYPXLS
         VECBUF(11) = MAXPAT

* Next word is record length, in bytes, of host resident image file.

         VECBUF(12) = IOIRBC
         VECBUF(13) = IOIRMX
         VECBUF(14) = IOTRBC

* The next two array elements were not set in this driver for EPIC 2.2

         VECBUF(15) = IMRGTX

* VECBUF(16)  needs to be 1 for EPIC 3.1, probably due to a bug fix.
         VECBUF(16) = IPCC

* Write out

         OUTREC = 1
         WRITE( IOVUN,REC=OUTREC ) (VECBUF(N),N=1,RECSIZ)
*        CALL IOVWRT( VECBUF )
         CLOSE (UNIT = IOVUN, STATUS = 'KEEP')

         RETURN
         END





         SUBROUTINE GK5XCL
      INCLUDE '../../include/check.inc'
      INCLUDE '../../include/gkxer.cmn'
*
* LOCALS
*
         INTEGER MINONE,ZERO
         PARAMETER (MINONE=-1,ZERO=0)
         INTEGER NUMWRD,ENDVEC,YES,NO
         PARAMETER (YES=1,NO=0)
         INTEGER N
         INTEGER IX1,IY1,IX2,IY2,NDLTX
         INTEGER LOR, SHFTL, LAND
*     WRITE (*,*) 'GK5XCL entered '
*
* ALGORITHM
*
* CHECK last vector for END-OF-DRAWING vector
* IF last vector was NOT END-OF-DRAWING vector THEN
*    IF Buffer FULL THEN
*       flush Buffer
*    ENDIF
*    put E-O-D vector into Buffer
* ENDIF
* IF Buffer NOT empty THEN flush Buffer

* Routine performs an implied frame advance by putting a -1 followed by
* a zero into the buffer.
* Check if vectors are 2 or 4 words long

         NUMWRD=4
         IF( MXLINE.GT.0 ) NUMWRD=2
         ENDVEC=NO

* Check if last vector was an END-OF-DRAWING vector
* IWORD>NUMWRD indicates at least 1 vector in buffer

         IF( IWORD.GT.NUMWRD ) THEN
             IF( VECBUF(IWORD-NUMWRD).EQ.MINONE .AND.
     1           VECBUF(IWORD-NUMWRD+1).EQ.ZERO ) ENDVEC=YES
         ENDIF

* See if buffer can hold NUMWRD words to mark end of vector drawing
* Assuming that ENDVEC is NO
* IWORD points to the next free location

         IF( ENDVEC.EQ.NO ) THEN
             IF( (IWORD+2*NUMWRD-1).GT.RECSIZ ) THEN
*                 WRITE( 33,1111 ) OUTREC
*1111             FORMAT( 1X,'GK5XCL, ENTRY 1. OUTREC= ',I7 )
                  WRITE( IOVUN,REC=OUTREC ) (VECBUF(N),N=1,RECSIZ)
*                 CALL IOVWRT( VECBUF )
                  CALL GK5XCB
*                 NUMVEC=0
             ENDIF

*
* Put a point at (0,IYPXLS-1) to (1,IYPXLS-1) to anchor picture and
* prevent it from slipping to top of page.
*

             IX1=0
             IY1=IYPXLS-1
             IX2=1
             IY2=IYPXLS-1
             IF( NUMWRD.EQ.2 ) THEN
                 IX1 = LOR( SHFTL(IX1,16),LAND(IY1,MSK16) )
                 NDLTX=0
                 IY1 = LOR( SHFTL(NDLTX,16),LAND(IY2,MSK16) )
                 VECBUF(IWORD)=IX1
                 VECBUF(IWORD+1)=IY1
             ELSE
                 VECBUF(IWORD)=IX1
                 VECBUF(IWORD+1)=IY1
                 VECBUF(IWORD+2)=IX2
                 VECBUF(IWORD+3)=IY2
            ENDIF
            IWORD=IWORD+NUMWRD

             VECBUF(IWORD)=-1
             VECBUF(IWORD+1)=0
             IWORD=IWORD+NUMWRD
         ENDIF

* If buffer not empty, flush it


         IF( IWORD.GT.1 ) THEN
*        WRITE( 33,1122 ) OUTREC
*1122    FORMAT( 1X,'GK5XCL, ENTRY 2. OUTREC = ',I7 )
             WRITE( IOVUN,REC=OUTREC) (VECBUF(N),N=1,RECSIZ)
*            CALL IOVWRT( VECBUF )
             CALL GK5XCB
         ENDIF
         RETURN
         END





      SUBROUTINE GK5XLN(N,X,Y)

*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Outputs polyline to vector file
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     INP N   - number of points
*     INP X,Y - coordinates of points
*
      INTEGER N
      REAL X(N),Y(N)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkxer.cmn'
*
*  LOCALS
*  ------
*
         INTEGER ISHORT
         INTEGER I
         INTEGER JLOOP,NUML
         INTEGER NUMLIN,LENG
         INTEGER IPOINT
         INTEGER DIREC
*        PARAMETER (NUMLIN=3)
         PARAMETER (NUMLIN=2)
         INTEGER LOR,LAND
         PARAMETER (ISHORT=2)
         INTEGER NVEC
         INTEGER ILOOP
         INTEGER NUMWRD
         INTEGER IX1,IY1,IX2,IY2,NDLTX
         INTEGER ISTART,IEND
         INTEGER SHFTL
         INTEGER ITEMP
*
*---------------------------------------------------------------------

* ALGORITHM
* ---------
*
* Values have already been transformed to DC by GKTWD and clipped by
* GKLCLP. Need to divide up N point polyline into vectors in order
* to output them.
*
*        WRITE(33, *) 'IXPXLS, IYPXLS', IXPXLS, IYPXLS

*        NN=40
*        IF( N.LT.NN ) NN=N
*        WRITE( 33,1133 )
*1133    FORMAT( 1X,'*** 1ST <=40 ELEMENTS OF X,Y ***' )
*      DO 1234 II=1,NN
*        WRITE( 33,1122 ) X(II),Y(II)

*      DO 1234 II=1,N
*        IF(X(II).GT.IXPXLS.OR.Y(II).GT.IYPXLS.OR.X(II).LT.0.
*    :     OR.Y(II).LT.0)
*    :    WRITE( 33,1122 ) X(II),Y(II)
*         WRITE( 33,1122 ) X(II),Y(II)
*1122    FORMAT( 1X,'X,Y (DC) = ',F10.3,1X,F10.3 )
*1234 CONTINUE
         NVEC=N-1
         IF( NVEC.LT.1 ) RETURN
* Point to start and end
         ISTART = 0
         IEND = 1
* Loop over the NVEC vectors

      DO 1000 ILOOP = 1,NVEC

         ISTART = ISTART+1
         IEND = IEND+1

         IX1 = INT( X(ISTART) + 0.5 )
         IY1 = INT( Y(ISTART) + 0.5 )
         IX2 = INT( X(IEND) + 0.5 )
         IY2 = INT( Y(IEND) + 0.5 )

         NUML=NUMLIN
*
* Test if vector is a single point, if so increase the lines generated
* to 2 pixels long
*
         IPOINT=0
         NDLTX=IX2-IX1
         LENG=INT(SQRT(FLOAT(NDLTX*NDLTX + (IY2-IY1)*(IY2-IY1))+0.5))
         IF( LENG.EQ.1 ) IPOINT=1
         IF( IPOINT.EQ.1 ) THEN
             IF( IX1.LT.(IXPXLS-1) .AND. IX2.GT.2 ) THEN
                 IX2=IX1+2
             ENDIF
             IY2=IY1
         ENDIF
*
* Put in the LINE THICKNESS, ONLY if ASF(2) is SET to INDIVIDUAL
*
         IF( KIPLAF(2).EQ.GINDIV ) NUML=NUML*QWLNWD( KWKIX )
         IF( NUML.LT.NUMLIN ) NUML=NUMLIN
*        WRITE( 6,9911 ) KIPLAF(2),QWLNWD(KWKIX),NUML
*9911    FORMAT( 1X,'LINE WIDTH ASF + VALUE, NUML = ',I7,F10.7,I7 )
      DO 1000 JLOOP=1,NUML
*
* check line direction, otherwise xtra vectors may fall on top of origin
*
         DIREC=1
         IF( (IX1.LT.IX2) .AND. (IY1.GT.IY2) ) DIREC=-1
         IF( (IX1.GT.IX2) .AND. (IY1.LT.IY2) ) DIREC=-1
*
* Set up additional lines, parallel to the original, shifted 1 pixel
*
         IF( JLOOP.NE.1 ) THEN
             IX1=IX1-DIREC
             IF( IX1.LT.1 ) IX1=1
             IY1=IY1+1
             IF( IY1.GT.IYPXLS ) IY1=IYPXLS
             IX2=IX2-DIREC
             IF( IX2.LT.1 ) IX2=1
             IY2=IY2+1
             IF( IY2.GT.IYPXLS ) IY2=IYPXLS
         ENDIF

* Order on increasing X

         IF( IX2.LT.IX1 ) THEN
             ITEMP = IX1
             IX1 = IX2
             IX2 = ITEMP
             ITEMP = IY1
             IY1 = IY2
             IY2 = ITEMP
         ENDIF
         NDLTX = IX2-IX1

*        WRITE( 33,1144 ) ILOOP,IX1,IY1,IX2,IY2
*1144    FORMAT( 1X,'LOOP =',I3,'IX/Y1, IX/Y2 = ',4(I10,1X) )

* See if vectors need packing

         IF( MXLINE.GT.0 ) THEN
             IX1 = LOR( SHFTL(IX1,16),LAND(IY1,MSK16) )
             IY1 = LOR( SHFTL(NDLTX,16),LAND(IY2,MSK16) )
             NUMWRD = 2
         ELSE
             NUMWRD = 4
         ENDIF
* Update drawable vectors and buffer pointer

         NUMVEC = NUMVEC+1
         IF( (IWORD+NUMWRD) .GT. RECSIZ ) THEN
             WRITE( IOVUN,REC=OUTREC ) (VECBUF(I),I=1,RECSIZ)
*            CALL IOVWRT( VECBUF )
             CALL GK5XCB
* Update record count

         ENDIF

         VECBUF(IWORD) = IX1
         VECBUF(IWORD+1) = IY1
         IF( NUMWRD.GT.ISHORT ) THEN
             VECBUF(IWORD+2) = NDLTX
             VECBUF(IWORD+3) = IY2
         ENDIF
         IWORD = IWORD + NUMWRD
*        WRITE( 33,1166 ) IWORD
*1166    FORMAT( 1X,'IWORD = ',I5 )

 1000 CONTINUE
         RETURN
      END





      SUBROUTINE GK5XRO(X,Y,NXPIX,NYPIX,NXDIM,ICOLAR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:             GMC
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Outputs a NXPIX x NYPIX array.
* Each scanline consists of a set of black and white pixels.
* The start and end of a consecutive set of black pixels are
* marked and output as a 2 point polyline using GK5XLN.
* This routine will take care of the buffering.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     INP X,Y    - coordinates of raster origin
*     INP NXPIX  - No of pixels per scanline
*     INP NYPIX  - No of scanlines in raster
*     INP NXDIM  - First dimension of colour array
*     INP ICOLAR - Integer colour array to output
*
      REAL    X, Y
      INTEGER NXPIX, NYPIX, NXDIM, ICOLAR(NXDIM,NYPIX)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkxer.cmn'
*
*  LOCALS
*  ------
*
* IYES       A value of 1 for YES
* NO         A value of 0 for NO
* ISAVEC     Save the vector, given value of YES or NO.
* ICOLR      Colour present, ie black. Given a value of 1
* NOCOLR     Colour absent, ie white. Given a value of 0
* LASCOL     Colour of last pixel
* NEWCOL     Colour of new (present) pixel
*

       INTEGER IYES,NO
       PARAMETER (IYES=1,NO=0)
       INTEGER ISAVEC,LASCOL,NEWCOL
       INTEGER IROW,ICLM
       INTEGER ICOLR,NOCOLR
       PARAMETER (ICOLR=1,NOCOLR=0)
         REAL XVEC(2),YVEC(2)
*
*---------------------------------------------------------------------

* Set up LASCOL to be NOCOLR

       LASCOL=NOCOLR
* Output each row of raster

      DO 200 IROW=1,NYPIX
* Output each pixel in the row

        DO 100 ICLM=1,NXPIX

* Set up whether to save vector in buffer
* Find out current pixel colour, using MOD( pixel_colour,2 )
* IF LASCOL was NOCOLR and NEWCOL is ICOLR THEN start of vector
* ELSE IF LASCOL was ICOLR and NEWCOL is NOCOLR THEN end of vector

            ISAVEC=NO
            NEWCOL=MOD( ICOLAR(ICLM,IROW),2 )
            IF( LASCOL.EQ.NOCOLR .AND. NEWCOL.EQ.ICOLR ) THEN
                XVEC(1)=X+FLOAT(ICLM-1)
                YVEC(1)=Y+FLOAT(IROW-1)
            ELSE IF( LASCOL.EQ.ICOLR .AND. NEWCOL.EQ.NOCOLR ) THEN
                XVEC(2)=X+FLOAT(ICLM-1)
                YVEC(2)=Y+FLOAT(IROW-1)
                ISAVEC=IYES
            ENDIF

* Save vector

            IF( ISAVEC.EQ.IYES ) THEN
            CALL GK5XLN( 2,XVEC,YVEC )
            ENDIF
            LASCOL=NEWCOL
  100   CONTINUE

*
* Force out any non-white line
*
         IF( .NOT.(LASCOL.EQ.NOCOLR) ) THEN
             XVEC(2)=X+FLOAT(NXPIX-1)
             YVEC(2)=Y+FLOAT(IROW-1)
             CALL GK5XLN( 2,XVEC,YVEC )
         ENDIF

  200 CONTINUE

      END





         SUBROUTINE GK5XCB
      INCLUDE '../../include/check.inc'

*
* LOCALS
*
*     I - Loop variable
*

      INCLUDE '../../include/gkxer.cmn'
         INTEGER I
* Clear out buffer array
* and set to fisrt word

      DO 1000 I=1,BUFSIZ
         VECBUF(I)=0
 1000 CONTINUE
         IWORD=1

* Increment Record count

         OUTREC = OUTREC+1
         RETURN
         END
