C# IL>=a, OL>=0
      SUBROUTINE GK5IWD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)

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
* Driver routine for IBM 4250 electro erosion printer
*
*  MAINTENANCE LOG
*  ---------------
*
*     11/02/88  DSG  IS Conversion.
*     11/05/88  DSG  GKS4250 LIST4250 output diverted from T-disk
*                    to L-disk for testing.
*     20/09/88  DSG  GK5IRO added
*     25/07/90  PLP  Removed unused locals; extended I4250 common in
*                    GK5ICS to its full length, to please the Sun's
*                    Fortran compiler.
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
*
*  EXTERNALS
*  ---------
*
      EXTERNAL GK5ILN,GK5IMK,GK5IRO
      EXTERNAL GK0TXF
* If TEXT uses CHAR precision, the second EXTERNAL statement will be nee
*    EXTERNAL GK5ILN,GK5IXF,GK5IXC
*
*  LOCALS
*  ------
*     IBAUD  Offset in KWKDAT of baud rate for this terminal
*     INKWI1 Holds initial value of KWI1 in inquire fill area facils
*     ICHUNK Number of points in each chunk of output primitive
*     ICTLZ  ASCII code for ctrl/Z
*     INTXFP No. of font/precision pairs
* IXLN,IYLN Stack pointers for GK5ILN
* XTX,YTX Real variables to hold transformed text positions
* IXMK,IYMK Stack pointers for GK5IMK
*     ITXF   Hardware fonts
*     ITXP   Hardware precisions
*     I      Various
*     NLEFT  Returned by output buffering routine (amount of buffer left
*     INTA   Local integer array with multiple uses
*              - receiving input device state (size 10 max)
*              - receiving WDT info for Inq Text Facil (size 19)
*     IPREC  Stores available text precisions
*     REALA  Local real array with multiple uses
*              - receiving input device state (size 7 max)
*              - receiving WDT info for Inq Text Facil (size 12)
*     IXFI,IYFI Offsets in stack for Fill Area
*     IFILSC Fill area scale factor to pass to utility
*
      INTEGER    IBAUD, ICHUNK, ICTLZ, INTXFP
      PARAMETER (IBAUD=1, ICHUNK=200, ICTLZ=26, INTXFP=2)
      INTEGER INKWI1, I,
     :    NLEFT
      INTEGER IXLN,IYLN,IXMK,IYMK
      REAL XTX,YTX
      INTEGER INTA(19),IPREC(KFNTMX),ITXP(INTXFP),ITXF(INTXFP)
      REAL REALA(12)
         INTEGER IXFI,IYFI,IFILSC
      PARAMETER (IFILSC = 1)
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
* Ask operating system to make a connection
*       CALL GKIOOP(KWKTYP,KCID(KWKIX),KWCID(KWKIX))
*       IF (KERROR.EQ.0) THEN
* Get terminal speed
*         CALL GKGTSP(KWCID(KWKIX),KWKDAT(IBAUD,KWKIX))
* Initialise output buffering
*          CALL GKIOBO(KIOIT,1,KDAT,NLEFT)

********************************************************
**
** Set head and tail of buffer output to Tektronix lookalikes
*          CALL GK0TLA
**
********************************************************

* Erase screen
* OPEN FILE TO 4250
          CALL GK5IIO
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
        CALL GK5ICS
      ENDIF
      GOTO 9999



* Clear workstation
   30 CONTINUE
      IF( KWI1.EQ.2 ) THEN
*       KWDONE=KRFUSE
*       CALL GKSLDL(KSSGPT(KWKIX))
         KWDONE=KACEPT
         CALL GK5ICL
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
      IF (KWI1.EQ.GALWAY.OR.KDSMT(KWKIX).EQ.GNEMPT) CALL GK5ICL
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
*     CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
*     IF (KERROR.EQ.0) THEN
*       IF (KWLNTY(KWKIX).EQ.1) THEN
*         FLAG = .FALSE.
*       ELSE
*         FLAG = .TRUE.
*       ENDIF
*       N = ICHUNK
*       DO 122 I=1,NRD,ICHUNK-1
*         IF (NRD-I.LT.ICHUNK-1) N = NRD-I + 1
*         IF (N.EQ.1) GOTO 122
*         CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
*         CALL GKLCLP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),FLAG,25.0,
*    :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
*    :                   QWCLYT(KWKIX),GK5ILN)
* 122   CONTINUE
*       CALL GKSTDA(KREALS,IOFF)
*       ENDIF

*
* GKLCLP causes looping when linetype is not solid. use GDDM only
*
         CALL GKSTAL( KREALS,NRD,IXLN )
         IF( KERROR.NE.0 ) GO TO 8888
         CALL GKSTAL( KREALS,NRD,IYLN )
         IF( KERROR.NE.0 ) GO TO 8888
         CALL GKTWD( NRD,RX(1),RY(1),QSTACK(IXLN),QSTACK(IYLN) )
         CALL GK5ILN( NRD,QSTACK(IXLN),QSTACK(IYLN),
     1                KWLNTY(KWKIX),QWLNWD(KWKIX) )
         CALL GKSTDA( KREALS,IYLN )
         CALL GKSTDA( KREALS,IXLN )
      GOTO 8888



* Polymarker
  130 CONTINUE
*     CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
*     IF (KERROR.EQ.0) THEN
*       N = ICHUNK
*       DO 132 I=1,NRD,ICHUNK
*         IF (NRD-I.LT.ICHUNK) N = NRD-I + 1
*         CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
*         CALL GKMTYP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),
*    :                   KWMKTY(KWKIX),QNMMKS(KWKIX),QWMKSZ(KWKIX),
*    :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
*    :                   QWCLYT(KWKIX),GK5IMK)
* 132   CONTINUE
*       CALL GKSTDA(KREALS,IOFF)
*     ENDIF
         CALL GKSTAL( KREALS,NRD,IXMK )
         IF( KERROR.NE.0 ) GO TO 8888
         CALL GKSTAL( KREALS,NRD,IYMK )
         IF( KERROR.NE.0 ) GO TO 8888
         CALL GKTWD( NRD,RX(1),RY(1),QSTACK(IXMK),QSTACK(IYMK) )
         CALL GK5IMK( NRD,QSTACK(IXMK),QSTACK(IYMK),
     1                KWMKTY(KWKIX),QWMKSZ(KWKIX) )
         CALL GKSTDA( KREALS,IYMK )
         CALL GKSTDA( KREALS,IXMK )
      GOTO 8888



* Text
  140 CONTINUE
* If precision nedds CHAR or STRING? then GKXDWC will be required
* along with two routines GK5IXF and GK5IXC. See the EXTERNAL statement

*     IF(KWTXPR(KWKIX) .EQ. GSTRKP) THEN
*        CALL GKXDWO(NID,IDAT,GK5ILN)
*     ELSE
*        CALL GKXDWC(NID,IDAT,QWCHRX(KWKIX),QWCHRY(KWKIX),
*    :               GK5IXF,GK5IXC)
*     ENDIF
         CALL GKTWD( 1,QWR1,QWR2,XTX,YTX )
         CALL GK5ITX( NID,IDAT,XTX,YTX,KWTXFN(KWKIX),KWTXPR(KWKIX),
     1                KWTXP(KWKIX),QWCHHX(KWKIX),QWCHHY(KWKIX),
     2                QWCHWX(KWKIX),QWCHWY(KWKIX),KWHTXA(KWKIX),
     3                KWVTXA(KWKIX),QWCHXP(KWKIX),QWCHSP(KWKIX) )
      GOTO 8888



* Fill area
  150 CONTINUE
*     CALL GKFILL(NRD,RX,RY,GK5ILN,GK5ILN)
*
* TRANSFORM TO DEVICE COORDS
*
         CALL GKSTAL( KREALS,NRD,IXFI )
         IF( KERROR.NE.0 ) GO TO 8888
         CALL GKSTAL( KREALS,NRD,IYFI )
         IF( KERROR.NE.0 ) GO TO 8888
         CALL GKTWD( NRD,RX,RY,QSTACK(IXFI),QSTACK(IYFI) )
*
* COULD PUT IN CLIPPING HERE (FOR POLYGONS?)
* BUT GDDM IS PROBABLY MORE EFFICIENT AT IT?
*
*        CALL GK5IFI( NRD,RX,RY )
         CALL GK5IFI( NRD,QSTACK(IXFI),QSTACK(IYFI),
     1                KWFAIS(KWKIX),KWFASI(KWKIX) )
         CALL GKSTDA( KREALS,IYFI )
         CALL GKSTDA( KREALS,IXFI )
      GOTO 8888



* Cell array ... do minimal simulation
  160 CONTINUE
      CALL GKCASM(GK5ILN)
      GOTO 8888



* GDP
  170 CONTINUE
      KERROR = 104
      GOTO 9999



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
      IF (KWFAIS(KWKIX).EQ.GHATCH .AND. KWFASI(KWKIX).LT.-10
     : .OR.KWFASI(KWKIX).GT.-1) KWFASI(KWKIX) = 1
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
      IF( KWI2.NE.GPATTR ) THEN
          CALL GKSRFA( .TRUE. )
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
      IF ((KWI1.LE.0.OR.KWI1.GT.3) .AND.
     :    (KWI2.LT.-10.OR.KWI2.GT.-1)) THEN
        KERROR = 2002
      ELSE
        INKWI1 = KWI1
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
        IF (KERROR.EQ.0) THEN
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





         SUBROUTINE GK5IIO
      INCLUDE '../../include/check.inc'
         COMMON /I4250/ DEVID,RANGE

         INTEGER DEVID,FAMILY,PCOUNT,PLIST(12),NCOUNT
         CHARACTER*8 TOKEN,NLIST(3)
         REAL RANGE
         INTEGER SWATHE,WIDTH,HEIGHT
         INTEGER IERROR
         INTEGER IN4250
         DATA IN4250/9/
*        DATA TOKEN/'*       '/
         DATA TOKEN/'IMG600X '/
         DATA FAMILY/4/,PCOUNT/12/,NCOUNT/3/
         DATA PLIST/5,0,6,0,  7,150,  8,297,297,1,9,1/
         DATA NLIST/'GKS4250 ','LIST4250','T      '/

*
* LOCALS
*
* DEVID     - The device identifier, must be > 1 see GDDM Programming
*             Reference (5748-XXH, rel 4 ) chapter 3 page 87
* FAMILY    - Device-family code, = 4 for High resolution image file eg
* TOKEN     - Tells GDDM where to find device properties. Use 'IMG600X'.
* PCOUNT    - The number of words in the procopt list. Use 0 for empty.
* PLIST     - The procopt list, see chapter 3 page 88+
*             For 4250 printers, the groups 5,6,7,8 and 9 are relevant.
*
*             Group 5: 1st word = 5, 2nd word = 0, primary data stream
*                                               1, secondary data stream
*             Group 6: 1st word = 6, 2nd word = 0, spill file for pictur
*                                               1, main storage only.
*             Group 7: 1st word = 7, 2nd word = 1(default), no. of swath
*             Group 8: 1st word = 8, 2nd word = 297, width of 'paper'
*                                    3rd word = 297, depth of 'paper'
*                                    4th word = 0, units of 1/10 inches
*                                             = 1, units of mms.
*             Group 9: 1st word = 9, 2nd word = 0, unformatted file.
*                                             = 1, formatted output (def
*
* NCOUNT    - The number of name parts in name list. Use 0 for empty.
* NLIST     - The name part list, see chapter 3 page 107+
*             For a 4250 choose the 3 part file description eg
*             GKS4250 LIST4250 T
* RANGE      - The range of the WINDOW in GDDM
* IERROR     - Error from INQUIRE about status of file JOB4250 DATA T
*              Input on channel IN4250
* SWATHE     - The number of swathes
* WIDTH      - The width of output
* HEIGHT     - The height of output
*
*
*
* Algorithm
*
* To set up using GDDM, we need to call FSINIT
*                                       DSOPEN
*                                       DSUSE
*
* The default segment will be taken, and other output dependents will
* currently be the default.
*
         DEVID=2
*        RANGE=7000.0
* Set up RANGE to be 100000, for use with CELL ARRAY
*
         RANGE=100000.0
         CALL FSINIT
*
* Read from file JOB4250 DATA, if it exists, the number of swathes,
* and the width and height of the output
*
         INQUIRE( UNIT=IN4250,IOSTAT=IERROR )
         IF( IERROR.EQ.0 ) THEN
             READ( IN4250,*,END=6000,ERR=6000 ) SWATHE,WIDTH,HEIGHT
             IF( SWATHE.LT.1 .OR. SWATHE.GT. 150 ) SWATHE=150
             IF( WIDTH.LT.1 .OR.WIDTH.GT.297 ) WIDTH=40
             IF( HEIGHT.LT.1 .OR.HEIGHT.GT.297 ) HEIGHT=40
             PLIST(6)=SWATHE
             PLIST(8)=WIDTH
             PLIST(9)=HEIGHT
         ENDIF
 6000 CONTINUE
         CALL DSOPEN( DEVID,FAMILY,TOKEN,PCOUNT,PLIST,NCOUNT,NLIST )
         CALL DSUSE( 1,DEVID )
*
* Set up a window and viewport. GKS has done conversion to device coords
* Assume that with a 297 mm plotting width and depth, and 600 dots per
* inch, the DC given by GKS is approximately 0 to 7000.
* So set range to 7000.
*
         CALL GSWIN( 0.,RANGE,0.,RANGE )
         CALL GSVIEW( 0.,1.,0.,1. )
         RETURN
         END





         SUBROUTINE GK5ICS
      INCLUDE '../../include/check.inc'
         COMMON /I4250/ DEVID,RANGE
         INTEGER DEVID
         REAL RANGE
*
* LOCALS
*
* NONE
*

*
* ALGORITHM
*
*
* Force out whatever may be lurking with FSFRCE
* CLose down the device             with DSCLS
* CLOSE down GDDM                   with FSTERM
*
         CALL FSFRCE
         CALL DSCLS( DEVID,1 )
         CALL FSTERM
         RETURN
         END





         SUBROUTINE GK5ICL
      INCLUDE '../../include/check.inc'
*
* Purpose
*
* Forces output to the printer
* Clears the workstation
* Redefine the window and viewport
*

*
* LOCALS
*
* DEVID,RANGE from common I4250. RANGE is needed to redefine WINDOW
* after FSPCLR
*
         COMMON /I4250/DEVID,RANGE
         INTEGER DEVID
         REAL RANGE
*
*
*
         CALL FSFRCE
         CALL FSPCLR
         CALL GSWIN( 0.,RANGE,0.,RANGE )
         CALL GSVIEW( 0.,1.,0.,1. )
         RETURN
         END





         SUBROUTINE GK5ILN( N,X,Y,LTYPE,WIDTH )
      INCLUDE '../../include/check.inc'
*
* Purpose of Routine
*
* To output a polyline using GSPLNE
* GSPLNE differs from GPL in that it draws N lines from the current poin
* to X(1),Y(1) and on into the rest of the X,Y array as endpoints until
* N is satisfied.
* The easiest solution is just to set the current point to X(1),Y(1).
*

*
* Arguments
*
* INP N - number of points
* INP X,Y - coordinates
*
         INTEGER N
         REAL X(N),Y(N)

*
* LOCALS
*
* LINTYP - An array to point at the GDDM linetypes (base programming
*          reference p.227)
* WIDTH  - The line width scale factor
* MINTYP - The minimum linetype value
* MAXTYP - The maximum linetype value
* MINWID - The minimum width dcale factor
* MAXWID - The maximum width scale factor
* LTYPE  - The line type
*
         INTEGER LINTYP(5),LTYPE,MINTYP,MAXTYP
         REAL WIDTH,MINWID,MAXWID
         DATA LINTYP/0,2,4,3,6/
         DATA MINTYP/1/,MAXTYP/5/
         DATA MINWID/0.0/,MAXWID/100.0/
*
*
*
         IF( LTYPE.LT.MINTYP .OR. LTYPE.GT.MAXTYP ) THEN
             LTYPE=1
         ENDIF
*
* Linewidth scale factor
*
         IF( WIDTH.LT.MINWID .OR. WIDTH.GT.MAXWID ) THEN
             WIDTH=1.0
         ENDIF
*
* Set up GDDM calls
*
         CALL GSLT( LINTYP(LTYPE) )
*        CALL GSLW( IFIX(WIDTH) )
         CALL GSLW( INT(WIDTH) )
*
* Set up polyline
*
         CALL GSMOVE( X(1),Y(1) )
         CALL GSPLNE( N,X,Y )
         RETURN
         END





         SUBROUTINE GK5IMK( N,X,Y,MKTYPE,MKSIZE )
      INCLUDE '../../include/check.inc'
*
* Purpose of Routine
*
* To output a polymarker using GSMRKS
*

*
* Arguments
*
* INP N - number of points
* INP X,Y - coordinates
*
         INTEGER N
         REAL X(N),Y(N)
         INTEGER MKTYPE
         REAL MKSIZE

*
* LOCALS
*
* MINMK,MAXMK The minimum and maximum marker types.
* DEFLT,SCALE The default marker and scale (To give a 'point')
* MARKS The array for GDDM markers to which MKTYPE points
* MTYPE The marker type derived from MKTYPE
* WIDTH, WSAVE The fractional line width to be set up and the current saved
*              value
*
         INTEGER MINMK,MAXMK
         INTEGER DEFLT
         REAL SCALE
         INTEGER MARKS(5)
         INTEGER MTYPE
         REAL WIDTH,WSAVE
         DATA WIDTH/0.1/,WSAVE/1.0/
         DATA MARKS/1,2,6,4,1/
         DATA MINMK/0/,MAXMK/5/
         DATA DEFLT/1/,SCALE/0.05/
*
*
*
         IF( N.LT.1 ) RETURN
         CALL GSQFLW( WSAVE )
         CALL GSFLW( WIDTH )
         MTYPE=MKTYPE
*
*
*
         IF( MKTYPE.LT.MINMK .OR. MKTYPE.GT.MAXMK ) THEN
             MTYPE=1
         ENDIF
         IF( MTYPE.EQ.DEFLT ) THEN
             CALL GSMSC( SCALE )
         ELSE
*
* Set to larger default, use a factor of 1.0
*
             CALL GSMSC( MKSIZE/1.0 )
         ENDIF
         CALL GSMS( MARKS(MTYPE) )
*
*
*
         CALL GSMRKS( N,X,Y )
         CALL GSFLW( WSAVE )
         RETURN
         END





         SUBROUTINE GK5ITX( NID,IDAT,X,Y,TXFN,TXPR,TXPATH,
     1                      CHARHX,CHARHY,CHARWX,CHARWY,
     2                      XTXAL,YTXAL,CHAEXP,CHASP )
      INCLUDE '../../include/check.inc'
      INCLUDE '../../include/GKS_PAR'
*
* Purpose
*
* To output a character string in a GKS compatible mode using GDDM
*
         INTEGER NID,IDAT(NID)
         REAL X,Y
         INTEGER TXFN,TXPR,TXPATH
         INTEGER XTXAL,YTXAL
         REAL CHARHX,CHARHY,CHARWX,CHARWY
         REAL CHAEXP,CHASP
*
* LOCALS
*
* WSCAL,HSCAL are used to adjust the size of characters on the plot,
*       when stroke precision is used, wrt hardware characters.
*
         INTEGER CHDIR
         INTEGER ICHAR
         REAL GK5IVL
         REAL WSCAL,HSCAL
         REAL ATOP,ABASE,AHALF,ALEFT,ARIGHT,ACENT
         REAL HFAC,VFAC
         REAL HORIZ,VERTIC,XX,YY
         REAL XHOR,YHOR,XVERT,YVERT
         REAL XMOVE,YMOVE,VALUE
         REAL DX,DY,DHX,DHY,DWX,DWY,CWIDTH,CHEIGT
         REAL ALEN
         CHARACTER*400 STRING
         CHARACTER*400 BL400
         CHARACTER*1 BLANK(400)
         COMMON /CHATEX/ ALEFT,ACENT,ARIGHT,ATOP,ABASE,AHALF,
     1                  VALUE,HFAC,VFAC
         EQUIVALENCE (BLANK(1),BL400 )
         DATA BLANK/400*' '/
         DATA WSCAL/1.0/
         DATA HSCAL/1.8/
*
*
*
         STRING=BL400
         CALL GKATON( NID,IDAT,STRING )
*
* The text font and precision come from TXFN, TXPR
* The text path is in TXPATH
* The character box height and width vectors are, CHARHX,HY and CHARWX,W
* The text alignment (horiz + vert) are, XTXAL, YTXAL
*
* Currently GDDM does not have easily called facilities for text alignme
*
*        WRITE( 6,1111 ) TXFN,TXPR,TXPATH
*1111    FORMAT( 1X,'TXFN,TXPR,TXPATH = ',3I10 )
*        WRITE( 6,2222 ) CHARHX,CHARHY,CHARWX,CHARWY
*2222    FORMAT( 1X,'CHARHX,Y: CHARWX,Y = ',4(F10.5,1X) )
*        WRITE( 6,3333 ) XTXAL,YTXAL
*3333    FORMAT( 1X,'XTXAL,YTXAL = ',2I10 )
*
* Set up character mode using TXPR
*
        CALL GSCM( TXPR+1 )
*
* Set up character box size from CHARHX,Y and CHARWX,Y
*
         DHX=GK5IVL( CHARHX )
         DHY=GK5IVL( CHARHY )
         DWX=GK5IVL( CHARWX )
         DWY=GK5IVL( CHARWY )
         CWIDTH=SQRT( DWX*DWX + DWY*DWY )
         CHEIGT=SQRT( DHX*DHX + DHY*DHY )
         ALEN=CWIDTH
         CWIDTH=CWIDTH*WSCAL
         CHEIGT=CHEIGT*HSCAL
         CALL GSCB( CWIDTH*CHAEXP,CHEIGT )
*
* Set up ALEFT,ACENT,ARIGHT; ATOP,ABASE,AHALF
* for individual characters
*
         VALUE=CWIDTH*CHAEXP
         ALEFT=0.0
         ARIGHT=-VALUE
         ACENT=ARIGHT/2.0
         ATOP=-CHEIGT
         ABASE=0.0
         AHALF=ATOP/2.0
*
* Set up character angle from DWX,Y
*
         DX=CHARWX/ALEN
         DY=CHARWY/ALEN
         CALL GSCA( DX,DY )
         YMOVE=VALUE*DY
*
* Set up parameters from the character UP VECTOR and TXPATH.
* TEXT ALIGNMENT is subsidiary to this.
*
         IF( TXPATH.EQ.GRIGHT ) THEN
             CHDIR=1
             XMOVE=(VALUE + CWIDTH*CHASP )*DX
             YMOVE=(VALUE + CWIDTH*CHASP)*DY
             HFAC=FLOAT(NID)
             VFAC=1.0
             CALL GK5IRT( XTXAL,YTXAL,CWIDTH,CHASP,HORIZ,VERTIC )
         ELSE IF( TXPATH.EQ.GLEFT ) THEN
             CHDIR=3
             XMOVE=-(VALUE + CWIDTH*CHASP)*DX
             YMOVE=-(VALUE + CWIDTH*CHASP)*DY
             HFAC=FLOAT(NID)
             VFAC=1.0
             CALL GK5ILF( XTXAL,YTXAL,CWIDTH,CHASP,HORIZ,VERTIC )
         ELSE IF( TXPATH.EQ.GUP ) THEN
             CHDIR=4
             XMOVE=-CHEIGT*(1.0 + CHASP)*DY
             YMOVE=CHEIGT*(1.0 + CHASP)*DX
             HFAC=1.0
             VFAC=FLOAT(NID)
             CALL GK5IUP( XTXAL,YTXAL,CHEIGT,CHASP,HORIZ,VERTIC )
         ELSE IF( TXPATH.EQ.GDOWN ) THEN
             CHDIR=2
             XMOVE=CHEIGT*(1.0 + CHASP)*DY
             YMOVE=-CHEIGT*(1.0 + CHASP)*DX
             HFAC=1.0
             VFAC=FLOAT(NID)
             CALL GK5IDN( XTXAL,YTXAL,CHEIGT,CHASP,HORIZ,VERTIC )
         ENDIF
*
* Set up the rotated start position
*
         XHOR=DX*HORIZ
         YHOR=DY*HORIZ
         XVERT=DY*VERTIC
         YVERT=DX*VERTIC
*
*
*
*        WRITE( 6,6666 ) CHDIR
*6666    FORMAT( 1X,'CHDIR = ',I10 )
*        WRITE( 6,4444 ) DX,DY
*4444    FORMAT( 1X,'DX,DY = ',2(F10.5,1X) )
*        WRITE( 6,5555 ) CWIDTH,CHEIGT
*5555    FORMAT( 1X,'CWIDTH,CHEIGT = ',2(F10.5,1X) )
*        WRITE( 6,7777 ) HORIZ,VERTIC
*7777    FORMAT( 1X,'HORIZ,VERTIC = ',2(F15.5,1X) )
*        WRITE( 6,8888 ) XHOR,YHOR,XVERT,YVERT
*8888    FORMAT( 1X,'XHOR,YHOR: XVERT,YVERT = ',4(F15.5,1X) )
*        WRITE( 6,9999 ) CHAEXP,CHASP
*9999    FORMAT( 1X,'CHAEXP,CHASP = ',2(F15.5,1X) )
*
* Loop over the character
*
         XX=X + XHOR + XVERT
         YY=Y + YHOR + YVERT
      DO 1000 ICHAR=1,NID
         CALL GSCHAR( XX,YY,1,STRING(ICHAR:ICHAR) )
         XX=XX+XMOVE
         YY=YY+YMOVE
 1000 CONTINUE
         RETURN
         END
*
* FUNCTION to return an almost zero value in Device Coordinates (DC)
*
         REAL FUNCTION GK5IVL( X )
      INCLUDE '../../include/check.inc'
         REAL X
*
* LOCALS
*
         REAL MINVAL
         DATA MINVAL/0.0001/
*
* Test for abs value > MINVAL, and return value
* If not, return the signed value of MINVAL
*
         IF( ABS(X).GT.MINVAL ) THEN
             GK5IVL=X
         ELSE
             GK5IVL=SIGN( MINVAL,X )
         ENDIF
         END





         SUBROUTINE GK5IFI( N,X,Y,INTROR,STYLE )
      INCLUDE '../../include/check.inc'
      INCLUDE '../../include/GKS_PAR'
*
* Purpose
*
* To fill an area bounded by the N points with coords in X,Y
*
         INTEGER N,INTROR,STYLE
         REAL X(N),Y(N)
*
* LOCALS
*
* DEFPAT - The default pattern on the 4250 eg 15 for HOLLOW
* ISTYLE - The current fill area interior style
* IPAT   - The current pattern number as a pointer into SELPAT
* SELPAT - The pattern table for GDDM (base programming reference p238)
* MINPAT - The minimum pattern number
* MAXPAT - The maximum pattern number
* ILINE  - The saved current linetype
* ISOLID - The GDDM solid linetype
* IWIDTH - The saved current linewidth
* ITHIN  - The GDDM fill area boundary
*
         INTEGER DEFPAT,IPAT
         INTEGER SELPAT(16)
         INTEGER MINPAT,MAXPAT
         INTEGER ILINE,ISOLID
         INTEGER IWIDTH,ITHIN
         INTEGER ISAVE
         DATA ISAVE/0/,ISOLID/0/
         DATA IWIDTH/1/,ITHIN/0/
         DATA DEFPAT/15/
         DATA SELPAT/10,9,13,11,5,14,12,8,6,7,4,3,2,1,16,15/
         DATA MINPAT/1/,MAXPAT/16/

*
* Algorithm
*
* Set up the pattern, define the area (with or without boundary),
* Draw the polyline, close area and fill it, reset pattern to default
*
         IPAT=2
*
* Check for INTROR being GHOLLO, GSOLID, GPATTR, GHATCH.
*
         IF( INTROR.EQ.GHOLLO ) THEN
             IPAT=16
*            WRITE( 6,1111 ) IPAT
*1111        FORMAT( 1X,'HOLLOW: IPAT = ',I10 )
         ELSE IF( INTROR.EQ.GSOLID ) THEN
             IPAT=15
*            WRITE( 6,2222 ) IPAT
*2222        FORMAT( 1X,'SOLID: IPAT = ',I10 )
         ELSE IF( INTROR.EQ.GPATTR .OR. INTROR.EQ.GHATCH ) THEN
             IPAT=STYLE
             IF( IPAT.LT.MINPAT ) THEN
                 IPAT=MINPAT
             ELSE IF( IPAT.GT.MAXPAT ) THEN
                 IPAT=MAXPAT
             ENDIF
*            WRITE( 6,3333 ) IPAT
*3333        FORMAT( 1X,'PATTERN OR HATCH: IPAT = ',I10 )
         ENDIF
*
* Check IPAT
*
         IF( IPAT.LT.MINPAT .OR. IPAT.GT.MAXPAT ) THEN
             IPAT=9
         ENDIF
*
* Set clipping on (GDDM)
*
         CALL GSCLP( 1 )
         CALL GSPAT( SELPAT(IPAT) )
*
* Set current point to first X,Y
*
         CALL GSMOVE( X(1),Y(1) )
*
* IF interior is HOLLOW THEN set solid boundary, do area and
*    reset linetype to what it was
*    ELSE output the area with no boundary.
*
         IF( INTROR.EQ.GHOLLO ) THEN
             CALL GSQLT( ILINE )
             CALL GSQLW( IWIDTH )
             CALL GSLT( ISOLID )
             CALL GSLW( ITHIN )
             CALL GSAREA( 1 )
             CALL GSPLNE( N,X,Y )
             CALL GSENDA
             CALL GSLT( ILINE )
             CALL GSLW( IWIDTH )
         ELSE
             CALL GSAREA( 0 )
             CALL GSPLNE( N,X,Y )
             CALL GSENDA
         ENDIF
*
* Reset pattern
*
         CALL GSPAT( DEFPAT )
         RETURN
         END
*
* This routine returns parameters about the positioning of characters
* with TXPATH = GDOWN
*
         SUBROUTINE GK5IDN( XTXTAL,YTXAL,CHEIGT,CHASP,HORIZ,VERTIC )
*
* The positioning of the origin is done by HORIZ and VERTIC
* XTXAL,YTXAL are the horizontal and vertical text alignments
*
         INTEGER XTXAL,YTXAL
         REAL HORIZ,VERTIC,CHEIGT,CHASP
         REAL ALEFT,ACENT,ARIGHT,ATOP,ABASE,AHALF,VALUE
         REAL HFAC,VFAC
         COMMON /CHATEX/ ALEFT,ACENT,ARIGHT,ATOP,ABASE,AHALF,
     1                  VALUE,HFAC,VFAC
*
* COMMONS
*
      INCLUDE '../../include/GKS_PAR'
*
* LOCALS
*
* NONE
*

*
* HORIZONTAL Origin alignment
*
         IF( XTXAL.EQ.GAHNOR ) THEN
             HORIZ=ALEFT
         ELSE IF( XTXAL.EQ.GALEFT ) THEN
             HORIZ=ALEFT
         ELSE IF( XTXAL.EQ.GACENT ) THEN
             HORIZ=ACENT*HFAC + CHASP*CHEIGT
         ELSE IF( XTXAL.EQ.GARITE ) THEN
             HORIZ=ARIGHT*HFAC + CHASP*CHEIGT
         ENDIF
*
* VERTCAL Origin alignment
*
         IF( YTXAL.EQ.GAVNOR ) THEN
             VERTIC=ABASE*VFAC
         ELSE IF( YTXAL.EQ.GATOP ) THEN
             VERTIC=ATOP*VFAC
         ELSE IF( YTXAL.EQ.GACAP ) THEN
            VERTIC=ATOP*VFAC
         ELSE IF( YTXAL.EQ.GAHALF ) THEN
            VERTIC=AHALF*VFAC
         ELSE IF( YTXAL.EQ.GABASE ) THEN
            VERTIC=ABASE*VFAC
         ELSE IF( YTXAL.EQ.GABOTT ) THEN
           VERTIC=ABASE*VFAC
         ENDIF

         RETURN
         END
*
* This routine returns parameters about the positioning of characters
* with TXPATH = GALEFT
*
         SUBROUTINE GK5ILF( XTXTAL,YTXAL,CWIDTH,CHASP,HORIZ,VERTIC )
*
* The positioning of the origin is done by HORIZ and VERTIC
* XTXAL,YTXAL are the horizontal and vertical text alignments
*
         INTEGER XTXAL,YTXAL
         REAL HORIZ,VERTIC,CWIDTH,CHASP
         REAL ALEFT,ACENT,ARIGHT,ATOP,ABASE,AHALF,VALUE
         REAL HFAC,VFAC
         COMMON /CHATEX/ ALEFT,ACENT,ARIGHT,ATOP,ABASE,AHALF,
     1                  VALUE,HFAC,VFAC
*
* COMMONS
*
      INCLUDE '../../include/GKS_PAR'
*
* LOCALS
*
* NONE
*

*
* HORIZONTAL Origin alignment
*
         IF( XTXAL.EQ.GAHNOR ) THEN
             HORIZ=ALEFT
         ELSE IF( XTXAL.EQ.GALEFT ) THEN
             HORIZ=ALEFT
         ELSE IF( XTXAL.EQ.GACENT ) THEN
             HORIZ=ACENT*HFAC + CHASP*CWIDTH
         ELSE IF( XTXAL.EQ.GARITE ) THEN
             HORIZ=ARIGHT*HFAC + CHASP*CWIDTH
         ENDIF
*
* VERTCAL Origin alignment
*
         IF( YTXAL.EQ.GAVNOR ) THEN
             VERTIC=ABASE*VFAC
         ELSE IF( YTXAL.EQ.GATOP ) THEN
             VERTIC=ATOP*VFAC
         ELSE IF( YTXAL.EQ.GACAP ) THEN
            VERTIC=ATOP*VFAC
         ELSE IF( YTXAL.EQ.GAHALF ) THEN
            VERTIC=AHALF*VFAC
         ELSE IF( YTXAL.EQ.GABASE ) THEN
            VERTIC=ABASE*VFAC
         ELSE IF( YTXAL.EQ.GABOTT ) THEN
           VERTIC=ABASE*VFAC
         ENDIF

         RETURN
         END
*
* This routine returns parameters about the positioning of characters
* with TXPATH = GRIGHT
*
         SUBROUTINE GK5IRT( XTXTAL,YTXAL,CWIDTH,CHASP,HORIZ,VERTIC )
*
* The positioning of the origin is done by HORIZ and VERTIC
* XTXAL,YTXAL are the horizontal and vertical text alignments
*
         INTEGER XTXAL,YTXAL
         REAL HORIZ,VERTIC,CWIDTH,CHASP
         REAL ALEFT,ACENT,ARIGHT,ATOP,ABASE,AHALF,VALUE
         REAL HFAC,VFAC
         COMMON /CHATEX/ ALEFT,ACENT,ARIGHT,ATOP,ABASE,AHALF,
     1                  VALUE,HFAC,VFAC
*
* COMMONS
*
      INCLUDE '../../include/GKS_PAR'
*
* LOCALS
*
* NONE
*

*
* HORIZONTAL Origin alignment
*
         IF( XTXAL.EQ.GAHNOR ) THEN
             HORIZ=ALEFT
         ELSE IF( XTXAL.EQ.GALEFT ) THEN
             HORIZ=ALEFT
         ELSE IF( XTXAL.EQ.GACENT ) THEN
             HORIZ=ACENT*HFAC + CHASP*CWIDTH
         ELSE IF( XTXAL.EQ.GARITE ) THEN
             HORIZ=ARIGHT*HFAC + CHASP*CWIDTH
         ENDIF
*
* VERTCAL Origin alignment
*
         IF( YTXAL.EQ.GAVNOR ) THEN
             VERTIC=ABASE*VFAC
         ELSE IF( YTXAL.EQ.GATOP ) THEN
             VERTIC=ATOP*VFAC
         ELSE IF( YTXAL.EQ.GACAP ) THEN
            VERTIC=ATOP*VFAC
         ELSE IF( YTXAL.EQ.GAHALF ) THEN
            VERTIC=AHALF*VFAC
         ELSE IF( YTXAL.EQ.GABASE ) THEN
            VERTIC=ABASE*VFAC
         ELSE IF( YTXAL.EQ.GABOTT ) THEN
           VERTIC=ABASE*VFAC
         ENDIF

         RETURN
         END
*
* This routine returns parameters about the positioning of characters
* with TXPATH = GUP
*
         SUBROUTINE GK5IUP( XTXTAL,YTXAL,CHEIGT,CHASP,HORIZ,VERTIC )
*
* The positioning of the origin is done by HORIZ and VERTIC
* XTXAL,YTXAL are the horizontal and vertical text alignments
*
         INTEGER XTXAL,YTXAL
         REAL HORIZ,VERTIC,CHEIGT,CHASP
         REAL ALEFT,ACENT,ARIGHT,ATOP,ABASE,AHALF,VALUE
         REAL HFAC,VFAC
         COMMON /CHATEX/ ALEFT,ACENT,ARIGHT,ATOP,ABASE,AHALF,
     1                  VALUE,HFAC,VFAC
*
* COMMONS
*
      INCLUDE '../../include/GKS_PAR'
*
* LOCALS
*
* NONE
*

*
* HORIZONTAL Origin alignment
*
         IF( XTXAL.EQ.GAVNOR ) THEN
             HORIZ=ACENT + CHEIGT*CHASP
         ELSE IF( XTXAL.EQ.GALEFT ) THEN
             HORIZ=ALEFT
         ELSE IF( XTXAL.EQ.GACENT ) THEN
             HORIZ=ACENT + CHASP*CHEIGT
         ELSE IF( XTXAL.EQ.GARITE ) THEN
             HORIZ=ARIGHT + CHASP*CHEIGT
         ENDIF
*
* VERTCAL Origin alignment
*
         IF( YTXAL.EQ.GAVNOR ) THEN
             VERTIC=ATOP
         ELSE IF( YTXAL.EQ.GATOP ) THEN
             VERTIC=ATOP
         ELSE IF( YTXAL.EQ.GACAP ) THEN
            VERTIC=ATOP
         ELSE IF( YTXAL.EQ.GAHALF ) THEN
            VERTIC=AHALF + (VFAC-1.0)*CHEIGT*(1.0+CHASP) + CHEIGT*CHASP
         ELSE IF( YTXAL.EQ.GABASE ) THEN
            VERTIC=ABASE*(VFAC-1.0) + CHEIGHT*CHASP
         ELSE IF( YTXAL.EQ.GABOTT ) THEN
            VERTIC=ABASE*(VFAC-1.0) + CHEIGHT*CHASP
         ENDIF

         RETURN
         END
