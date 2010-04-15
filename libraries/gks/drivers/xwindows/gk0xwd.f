      SUBROUTINE GK0XWD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:             DLT
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Experimental X-windows driver for GKS-UK
*
*  MAINTENANCE LOG
*  ---------------
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
      INCLUDE '../../include/gkplb.cmn'
*
*  EXTERNALS
*  ---------
*
      EXTERNAL GK0XLN, GK0XRO, GK0XCS, GK0XGI, GK0XGD
      INTEGER GK0XOP, GK0XWS, GK0XCF, GK0XGD
*
*  LOCALS
*  ------
*     INKWI1 Holds initial value of KWI1 in inquire fill area facils
*     ICHUNK Number of points in each chunk of output primitive
*     INTXFP No. of font/precision pairs
*     IOFF   Stack offset for output primitive chunks
*     NOUT   Number of bytes returned on input
*     I      Various
*     N      Various
*     INTA   Local integer array with multiple uses
*              - receiving WDT info for Inq Text Facil (size 19 max)
*              - sending linetype code (size 2)
*     IPREC  Stores available text precisions
*     REALA  Local real array for receiving WDT info for Inq Text Facil
*              - (size 12 max)
*     IXCOL  whether display has colour
*     IXDYN  whether display has dynamic colour table
*     IXXSP  display x size in pixels
*     IXYSP  display y size in pixels
*     XXSM  display x size in metres
*     XYSM  display y size in metres
*     IXBPC  bits per colour
*     R,G,B  RGB colours
*     NCOLS  Temporary used for pixing window depth
*     NEW    Flag to show if window has been created
*
      INTEGER    ICHUNK
      PARAMETER (ICHUNK=200)
      INTEGER IOFF,  I, N, NCOLS, NEW
      INTEGER INTA(19), IPREC(KFNTMX)
      REAL  REALA(12), R, G, B, XXSM, XYSM

      INTEGER IXCOL, IXDYN, IXXSP, IXYSP, IXBPC
*
*  Offsets in KWKDAT workspace
*
      INTEGER ICHFLG, ICHSTA, ICHNUM
      PARAMETER (ICHFLG=KMXWKI, ICHSTA=KMXWKI-1, ICHNUM=KMXWKI-2)
      INTEGER ILCFLG, ILCSTA, ILCX, ILCY
      PARAMETER (ILCFLG=KMXWKI-3, ILCSTA=KMXWKI-4)
      PARAMETER (ILCX=KMXWKR, ILCY=KMXWKR-1)
*
*  Offsets in QWKDAT workspace
*
      INTEGER  XSCALE, YSCALE
      PARAMETER (XSCALE = 1, YSCALE = 2)

*  Starlink no screen clear escape
      INTEGER INOCLR
      SAVE INOCLR
      DATA INOCLR/GNO/

*
*  STACK USAGE
*  -----------
*     POLYLINE and POLYMARKER for transformations
*
*
*  COMMENTS
*  --------
*     The default window size is hard wired instead of being read from
*     the workstation description file. See GK0XSS
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
     :      1111,1410,1111,1111,1440,1111,1111,1470,1111,1111,
     :      1111,1510,1111,9999,9999,9999,9999,9999,9999,9999,
     :      9999,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1111,1111,1720,1730,1111,1111,1760,1770,1111,1790,
     :      1111,1111,1111,1111,1111,1850,1860,1111,1111,1890,
     :      1111,1111,1111,1111,1111,1111,1960,1970,1111) IENT-119

      GOTO 9999


* Open workstation
   10 CONTINUE
* Set up workstation state list and workstation description table
      CALL GKIWSL(KWKIX,KWKTYP)
      IF( KERROR.NE.0 ) GOTO 9999

      KCID(KWKIX) = KWI1
      KDFM(KWKIX) = GBNIG
      KWIO(KWKIX) = GNO
      KIMRGM(KWKIX) = GSUPPD


* Initialise flags used by escapes -1 and -2
      KWKDAT(ICHFLG, KWKIX) = KNIL
      KWKDAT(ILCFLG, KWKIX) = KNIL

*  Create a window
      KERROR = GK0XOP(KWKIX, KWKTYP, KWI1, KDSRX(KWKIX), KDSRY(KWKIX),
     :  QDSDX(KWKIX), QDSDY(KWKIX), NCOLS, IXDYN, IXCOL, NEW)
      IF (KERROR.NE.0) GOTO 9999

*  reduce the number of colours if it is more than allowed by the size
*  of the GKS data structures
      IF (NCOLS.LT.KPCI(KWKIX)) KPCI(KWKIX) = NCOLS

*  Reset either the line type or colour of the predefined polyline
*  bundles according to the properties of the server
      DO 12 I = 1,5
          IF (IXCOL.EQ.GYES .AND. I.LT.NCOLS) THEN
              KLNTY(I,KWKIX) = 1
          ELSE
              KPLCI(I,KWKIX) = 1
          ENDIF
   12 CONTINUE

*  Correct for different definitions of window size
      KDSRX(KWKIX) = KDSRX(KWKIX) - 1
      KDSRY(KWKIX) = KDSRY(KWKIX) - 1

*  Calculate device metres to device pixels scale
      QWKDAT(XSCALE,KWKIX) = REAL(KDSRX(KWKIX)) / REAL(QDSDX(KWKIX))
      QWKDAT(YSCALE,KWKIX) = REAL(KDSRY(KWKIX)) / REAL(QDSDY(KWKIX))

*  Update GKS data structures with the new size
      QRWVXL(KWKIX) = 0.0
      QRWVXR(KWKIX) = QDSDX(KWKIX)
      QRWVYB(KWKIX) = 0.0
      QRWVYT(KWKIX) = QDSDY(KWKIX)

      QCWVXL(KWKIX) = 0.0
      QCWVXR(KWKIX) = QDSDX(KWKIX)
      QCWVYB(KWKIX) = 0.0
      QCWVYT(KWKIX) = QDSDY(KWKIX)

      IF (NEW.EQ.GYES .OR. INOCLR.EQ.GNO) THEN

* Load the default colour table
         DO 15 I = 0, KPCI(KWKIX)-1
            CALL GK0XDC(IXCOL, KPCI(KWKIX), I, R, G, B)
            KERROR = GK0XGD(KWKTYP, I, R, G, B)
            IF (KERROR.NE.0) GOTO 9999
            CALL GK0XSC( KWKIX, I, R, G, B)

* Record the actual values in the GKS state list
            QHP(KHPXR(KCTBPT(1,KWKIX))+I) = R
            QHP(KHPXR(KCTBPT(2,KWKIX))+I) = G
            QHP(KHPXR(KCTBPT(3,KWKIX))+I) = B
   15    CONTINUE

*     Clear the window
         CALL GK0XCL(KWKIX)
      END IF

* Set segment list pointer
      KSSGPT(KWKIX) = KNIL

      INOCLR = GNO
      KWI1 = GOUTIN
      GOTO 9999

* Close workstation
   20 CONTINUE
      IF( KWI1.EQ.1 ) THEN
        KWDONE=KRFUSE
      ELSE
        CALL GK0XCW(KWKIX)

*      Close GKS data structures for this workstation
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
        CALL GK0XFL(KWKIX)
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
      CALL GK0XFL(KWKIX)
      KWI1 = KNFAUP(KWKIX)
      GOTO 9999


* Clear display surface
   80 CONTINUE
      IF (KWI1.EQ.GALWAY.OR.KDSMT(KWKIX).EQ.GNEMPT) CALL GK0XCL(KWKIX)
      CALL GKWCLD
      GOTO 9999


* Message
  100 CONTINUE
      GOTO 9999


* Escape
  110 CONTINUE
* Starlink no screen clear escape
      IF (KWI1.EQ.-3) THEN
         INOCLR = KWI2
         GOTO 9999
      END IF

      CALL GKESC
      GOTO 9999


* Polyline
  120 CONTINUE
      IF( KCVIS.EQ.GINVIS ) GOTO 9999
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN

*   Set hardware linestyle, line width and colour
        CALL GK0XLS(KWKIX, KWLNTY(KWKIX), NINT(QWLNWD(KWKIX)),
     :              KWPLCI(KWKIX))

*   Split polyline into manageable chunks and deliver them
        N = ICHUNK
        DO 122 I=1,NRD,ICHUNK-1
          IF (NRD-I.LT.ICHUNK-1) N = NRD-I + 1
          IF (N.EQ.1) GOTO 122
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
          CALL GKLCLP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),
     :         .FALSE.,  1.0,
     :         QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),QWCLYT(KWKIX),
     :         GK0XLN)
  122   CONTINUE
        CALL GKSTDA(KREALS,IOFF)
      ENDIF
      GOTO 8888


* Polymarker
  130 CONTINUE
      IF( KCVIS.EQ.GINVIS ) GOTO 9999

*   Set hardware linestyle, line width and colour
      CALL GK0XLS(KWKIX,GLSOLI,1,KWPMCI(KWKIX))

      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN
        N = ICHUNK
        DO 132 I=1,NRD,ICHUNK
          IF (NRD-I.LT.ICHUNK) N = NRD-I + 1
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
          CALL GKMTYP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),
     :                   KWMKTY(KWKIX),QNMMKS(KWKIX),QWMKSZ(KWKIX),
     :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK0XLN)
  132   CONTINUE
        CALL GKSTDA(KREALS,IOFF)
      ENDIF
      GOTO 8888


* Text
  140 CONTINUE
      IF( KCVIS.EQ.GINVIS ) GOTO 9999

*   Set hardware linestyle, line width and colour
      CALL GK0XLS(KWKIX,GLSOLI,1,KWTXCI(KWKIX))
      CALL GKXDWO(NID,IDAT,GK0XLN)
      GOTO 8888


* Fill area
  150 CONTINUE
      IF( KCVIS.EQ.GINVIS ) GOTO 9999

*   Set hardware linestyle, line width and colour
      IF (KWFAIS(KWKIX).NE.GPATTR)
     :                     CALL GK0XLS(KWKIX,GLSOLI,1,KWFACI(KWKIX))

*   Check to see whether pattern representation has been set (either
*   predefined or set by the user) - if not, use the pattern index 1
      IF (KWFAIS(KWKIX).EQ.GPATTR)THEN
         CALL GKDRGE(KPABPT(KWKIX),KWFASI(KWKIX),3,0,INTA,REALA)
         IF (KERROR.NE.0) THEN
            KWFASI(KWKIX)=1
            I=88
            KERROR=0
         ENDIF
      ENDIF
      CALL GKFILS(NRD,RX,RY,1,GK0XLN,GK0XRO)

*   Pattern uses cell array so flush the buffer
      IF (KWFAIS(KWKIX).EQ.GPATTR) CALL GK0XUP(KWKIX)

      GOTO 8888


* Cell array
  160 CONTINUE
      IF( KCVIS.EQ.GINVIS ) GOTO 9999
      CALL GKCELL(KWI1*KWI2,IDAT,GK0XRO)
      CALL GK0XUP(KWKIX)
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
*   Set hardware linestyle, line width and colour
        CALL GK0XLS(KWKIX,KWLNTY(KWKIX), NINT(QWLNWD(KWKIX)),
     :              KWPLCI(KWKIX))
        CALL GKCRCS(KWI1, NRD, RX, RY, 1, .TRUE., 1.0, GK0XLN, GK0XRO)
      ELSE
*    filled chord, pie or circle
        CALL GK0XLS(KWKIX,GLSOLI,1,KWFACI(KWKIX))

*   Use I as error flag
        I=0
*   Check to see whether pattern representation has been set (either
*   predefined or set by the user) - if not, use the pattern index 1
        IF (KWFAIS(KWKIX).EQ.GPATTR)THEN
           CALL GKDRGE(KPABPT(KWKIX),KWFASI(KWKIX),3,0,INTA,REALA)
           IF (KERROR.NE.0) THEN
              KWFASI(KWKIX)=1
              I=88
              KERROR=0
           ENDIF
        ENDIF

        CALL GKCRCS(KWI1, NRD, RX, RY, 1, .TRUE., 1.0, GK0XLN, GK0XRO)

*   Pattern uses cell array so flush the buffer
        IF (KWFAIS(KWKIX).EQ.GPATTR) CALL GK0XUP(KWKIX)

* Set error flag AFTER a pattern has been output
        KERROR=I
      ENDIF
      GOTO 9999


* Set polyline attributes
  180 CONTINUE
      CALL GKDPLB
* Need to check because individual settings won't have been checked.
* (note that polyline style 5 doesn't work).
      IF (KWLNTY(KWKIX).LT.0.OR.KWLNTY(KWKIX).GT.5) KWLNTY(KWKIX) = 1
      IF (KWPLCI(KWKIX).GE.KPCI(KWKIX)) KWPLCI(KWKIX) = 1
      IF (QWLNWD(KWKIX).LT.1.0) QWLNWD(KWKIX) = 1.0
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
      GOTO 9999


* Set fill area attributes
  210 CONTINUE
      CALL GKDFAB
* Need to check because individual settings won't have been checked.
      IF (KWFAIS(KWKIX).EQ.GHATCH .AND. (KWFASI(KWKIX).LT.-10 .OR.
     :    KWFASI(KWKIX).GT.-1)) KWFASI(KWKIX) = -1
      IF (KWFAIS(KWKIX).EQ.GPATTR .AND. KWFASI(KWKIX).GT.3)
     :    KWFASI(KWKIX) = 1
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
* Make sure that fonts are available
      IF( KDBFLS.EQ.KFLNA ) THEN
        KERROR=-1042
        GOTO 9999
      ENDIF
      IF( KDBFLS.EQ.KFLCL ) CALL GKXON
      IF( KERROR.NE.0 ) GOTO 9999
      DO 255 I=1,KFNTMX
        IPREC(I) = GSTRKP
  255 CONTINUE
      CALL GKSRTX(KFNTMX,KHFONT,IPREC,.FALSE.)
      GOTO 9999


* Set fill area representation
  260 CONTINUE
      CALL GKSRFA(.FALSE.)
      GOTO 9999


* Set pattern representation
  270 CONTINUE
      IF(KWI1.GT.KMXPAB(KWKIX))THEN
         KERROR=85
      ELSE
         CALL GKSRPA(NID,IDAT)
      ENDIF
      GOTO 9999


* Set colour representation
  280 CONTINUE
* See if colour index valid
      IF (KWI1.GE.0 .AND. KWI1.LT.KPCI(KWKIX)) THEN
        QHP(KHPXR(KCTBPT(1,KWKIX))+KWI1) = QWR1
        QHP(KHPXR(KCTBPT(2,KWKIX))+KWI1) = QWR2
        QHP(KHPXR(KCTBPT(3,KWKIX))+KWI1) = QWR3
        CALL GK0XSC( KWKIX, KWI1, QWR1, QWR2, QWR3)
      ELSE
        KERROR = 93
      ENDIF
      GOTO 9999


* Normalisation transformation
  310 CONTINUE
      CALL GKWKC4

* Convert from metres to raster coordinates
      DO 311 I = 1,3
         QWTOTT(I,KWKIX) = QWTOTT(I,KWKIX) * QWKDAT(XSCALE,KWKIX)
         QWTOTT(I+3,KWKIX) = QWTOTT(I+3,KWKIX) * QWKDAT(YSCALE,KWKIX)
  311 CONTINUE
      QWCLXL(KWKIX) = QWCLXL(KWKIX) * QWKDAT(XSCALE,KWKIX)
      QWCLYB(KWKIX) = QWCLYB(KWKIX) * QWKDAT(YSCALE,KWKIX)
      QWCLXR(KWKIX) = QWCLXR(KWKIX) * QWKDAT(XSCALE,KWKIX)
      QWCLYT(KWKIX) = QWCLYT(KWKIX) * QWKDAT(YSCALE,KWKIX)

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
      IF(KWI2.EQ.1) THEN
	  CALL GKINIP(GLOCAT,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        KERROR = 144
      ENDIF
      GOTO 9999


* Initialise stroke
  620 CONTINUE
      GOTO 9999


* Initialise valuator
  630 CONTINUE
      GOTO 9999


* Initialise choice
  640 CONTINUE
      IF (KWI1.EQ.1) THEN
	CALL GKINIP(GCHOIC,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
	KERROR = 144
      ENDIF
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

*   GKRQES doesn't have a mechanism for passing the initial
*   position so we shove it into QWKDAT for GKOXCS to read
      QWKDAT(3,KWKIX) = RX(1)
      QWKDAT(4,KWKIX) = RY(1)

      IF (KWKDAT(ILCFLG,KWKIX).NE.KNIL .AND. KWI1.EQ.1) THEN
*        Escape has been used to associate choice device 2 with
*        locator device 1.
*        If no stored data, need to request operator action
         IF (KWKDAT(ILCFLG,KWKIX) .EQ. GNO) CALL GKRQES(GK0XCS)
*        Pass locator data back
         KWI1 = KWKDAT(ILCSTA, KWKIX)
         RX(1) = QWKDAT(ILCX, KWKIX)
         RY(1) = QWKDAT(ILCY, KWKIX)
*        Clear stored locator data flag
         KWKDAT(ILCFLG, KWKIX) = GNO
      ELSE
*        Normal locator input
         CALL GKRQLC(GK0XCS, RX(1), RY(1))
      ENDIF
      GOTO 9999


* Request stroke
  700 CONTINUE
      GOTO 9999


* Request valuator
  710 CONTINUE
      GOTO 9999


* Request choice
  720 CONTINUE
*     Data expected:
*     KWI1   : Device Number
*
*     Data returned:
*     KWI1   : Status
*     KWI2   : Choice value

      IF (KWKDAT(ICHFLG,KWKIX) .NE. KNIL .AND. KWI1.EQ.2) THEN
*        Escape has been used to associate locator device 1 with
*        choice device 2.
*        If no stored choice data, need to request operator action
         IF (KWKDAT(ICHFLG,KWKIX) .EQ. GNO) CALL GKRQES(GK0XCS)
*        Pass choice data back
         KWI1 = KWKDAT(ICHSTA, KWKIX)
         KWI2 = KWKDAT(ICHNUM, KWKIX)
*        Clear stored choice data flag
         KWKDAT(ICHFLG, KWKIX) = GNO
      ELSE
*        Normal choice input
*        Call the utility
         CALL GKRQCH(KWI1,KWI2,GK0XGI)
      ENDIF
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



* Inquire text extent
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
*     QWCHWX(KWKIX),QWCHWY(KWKIX) : baseline vector
*
*     Data returned:
*     KERROR : error indicator
*     RX(1-4): x-text extent
*     RY(1-4): y-text extent
*     QWR7   : x-concatenation point
*     QWR8   : y-concatenation point

      CALL GKXQXO(NID,IDAT,RX,RY)
      GOTO 9999



* Inquire list of pattern indices
 1410 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999

* Inquire colour representation
 1440 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      IF (KWI2.EQ.GREALI) THEN
         CALL GK0XCR( KWKIX, KWI1, QWR1, QWR2, QWR3)
      ENDIF
      GOTO 9999


* Inquire locator device state
 1470 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999


* Inquire pick device state
 1510 CONTINUE
      GOTO 9999


* Inquire maximum display surface
 1720 CONTINUE
      KWI1 = GMETRE
      KERROR = GK0XWS(KWKTYP, KWI2, KWI3, QWR1, QWR2)
      GOTO 9999

* Inquire dynamic modification of workstation attributes
 1730 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KERROR = GK0XCF(KWKTYP, IXCOL, IXDYN, IXBPC, NCOLS)
      IF (IXDYN.EQ.GYES) THEN
         KWI6 = GIMM
      ENDIF
      GOTO 9999

* Inquire predefined polyline representation

* The wdt file predefines bundles with both line types and colours set
* so these have to be modified to be one or the other.
 1760 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KERROR = GK0XCF(KWKTYP, IXCOL, IXDYN, IXBPC, NCOLS)
      IF (KERROR.EQ.0) THEN
          IF (IXCOL.EQ.GYES .AND. KWI2.LT.NCOLS) THEN
              KWI1 = 1
          ELSE
              KWI2 = 1
          ENDIF
      ENDIF
      GOTO 9999


* Inquire polymarker facilities
 1770 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KERROR = GK0XWS(KWKTYP, IXXSP, IXYSP, XXSM, XYSM)
      IF (KERROR.EQ.0) THEN
         QWR1 = QWR1 / REAL(IXXSP) * XXSM
         QWR2 = QWR2 / REAL(IXXSP) * XXSM
         QWR3 = QWR3 / REAL(IXXSP) * XXSM
      ENDIF
      GOTO 9999


* Inquire text facilities

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

* Inquire colour facilities
 1850 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KERROR = GK0XCF(KWKTYP, IXCOL, IXDYN, IXBPC, NCOLS)
      IF (KERROR.EQ.0) THEN
         KWI1 = 2**IXBPC
         IF (KWI3.GT.NCOLS) KWI3 = NCOLS
         IF (IXCOL.EQ.GYES) THEN
            KWI2 = GCOLOR
         ELSE
            KWI2 = GMONOC
         END IF
      ENDIF
      GOTO 9999


* Inquire predefined colour representation
 1860 CONTINUE
      KERROR = GK0XCF(KWKTYP, IXCOL, IXDYN, IXBPC, NCOLS)
      IF (KERROR.EQ.0) THEN
         IF (KWI1.GE.NCOLS) THEN
            KERROR = 93
            GO TO 9999
         END IF

*    Get definition from WDT
         CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)

*    Match with X server properties
         KERROR = GK0XGD(KWKTYP, KWI1, QWR1, QWR2, QWR3)
      END IF
      GOTO 9999


* Inquire maximum length of workstation state tables
 1890 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KERROR = GK0XCF(KWKTYP, IXCOL, IXDYN, IXBPC, NCOLS)
      IF (KERROR.NE.0) GOTO 9999
      IF (KWI6.GT.NCOLS) KWI6 = NCOLS
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
      IF (KWIO(KWKIX).EQ.GYES) CALL GK0XFL(KWKIX)

 9999 CONTINUE

      END
