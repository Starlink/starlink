C# IL>=a, OL>=0
      SUBROUTINE GK0IWD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:             AS
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Workstation driver for IBM 3100 & 3200 series:
*     1400 = 3179-G
*
*  MAINTENANCE LOG
*  ---------------
*     **/03/88  CIA   Stabilized
*     14/04/88  CIA   Emulated set colour representation
*     20/07/88  CIA   Removed routines duplicated in utilities
*     25/07/88  CIA   Fixed bug - now allows more than 3 user defined
*                     patterns. Altered shutdown sequence in GK0IFL.
*     08/03/89  CIA   Added inquiry, so that driver works for PS/2.
*     23/08/89  RMK   Do pattern index check in set fill area attributes
*                     rather than fill area entry (was fix to S79).
*     24/07/90  PLP   Removed unused locals.
*     17/04/91  KEVP  In GK0ICA converted clipping rectangle to Raster
*                     coordinates (C65). Also derived conversion factor
*                     throughout driver from WDT.
*     17/04/91  KEVP  Use GKPXAD to inQuire PiXel Array Dimensions (C66).
*     17/04/91  KEVP  When Setting Colour Representation, put RGB values
*                     into heap, so that they can be used by
*                     inQuire Colour Representation (C73).
*     17/04/91  KEVP  Put GKINIP into Initialise Input device entry
*                     points and check for PET validity (C74).
*     18/04/91  KEVP  Ensured the colour representation of index 0
*                     is always the same as the background (black) and
*                     that realised values in inQuire Colour Represent-
*                     -ation are correct (C75).
*     19/04/91  KEVP  Replaced all occurences of IFIX with INT.
*     19/04/91  KEVP  Put into GK0IMP conversion of pattern reference
*                     point to raster coordinates and shifted the point
*                     one pixel down and left in pattern space (C76).
*     23/04/91  KEVP  Added a working STRING INPUT device (C77).
*     25/04/91  KEVP  Modified Fill Area to use new utility GKFILH
*                     for Hollow, solid and hatched fill-styles.
*                     Now complicated areas are now clipped correctly.
*                     This change is needed to enable cell array echoplay
*                     as part of the fix of bug C78.
*     25/04/91  KEVP  Added PICK INPUT using the utility GKRQPK (C78).
*     25/04/91  KEVP  Prevented STROKE INPUT from triggering or returning
*                     error 152 if outside of workstation viewport.
*     26/04/91  KEVP  Restricted use of GK0IFL to patterned fill by using
*                     GKFILH for other styles (needed in GDP). Removed
*                     code and arguments made redundant by this change.
*     30/04/91  KEVP  Improved speed of Raster Output routine GK0IRO,
*                     by using colour mixing. The number of passes by
*                     GSIMG was reduced from 8 to 3. It was necessary
*                     to first black out the area used to ensure nothing
*                     behind it would show through.
*     30/04/91  KEVP  Made fill area call GKFILC, if no GDDM hatch-styles
*                     are loaded (KWKDAT used).
*     01/05/91  KEVP  Made patterned fill even faster by making GK0IFL
*                     work in Raster Coordinates, so that it does the
*                     correct number of scans (not too many as before!).
*                     The routine GK0IMP, which converted to Raster
*                     Coordinates, became unnecessary and was replaced by
*                     the utility GKPMAP from which it was derived.
*                     A conversion of the pattern reference point to
*                     Raster Coordinates was put in the Set Fill Area
*                     Attributes entry point with 1.0 added to each
*                     coordinate to prevent reoccurence of bug C76.
*     07/05/91  KEVP  Sent entry point KQDPK=197 (inQuire Default Pick
*                     device data) to statement 1111 to call GKQWK
*                     (C79,C80 & C81).
*                     Sent the Inquire Input Device state entry points
*                     that only call GKQWK directly to statement 1111.
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
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwke.par'
      INCLUDE '../../include/gwksgl.par'
      INCLUDE '../../include/gkinp.par'
      INCLUDE '../../include/gkfls.par'
      INCLUDE '../../include/gkpca.par'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkcon.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkpca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkfls.cmn'
      INCLUDE '../../include/gkxfd.cmn'
      INCLUDE '../../include/gkerr.cmn'
*
*  EXTERNALS
*  ---------
*
      EXTERNAL GK0ILN, GK0IFA, GK0IRO, GK0ICS, GK0IPP, GKXXF
*
*  EXTERNAL FUNCTION DEFINITIONS
*  -----------------------------
*

*
*  LOCALS
*  ------
*     Offsets in KWKDAT
*     1-9         Colour Table
*     IGHTCH=10   GDDM Hatching Indicator
*
*     IPREC       Precision array for Text;
*                 ***** Note the local precision array will
*                 ***** disappear when there is a decent utility to
*                 ***** handle font names and precisions.
*     GDDMCOL     Used for converting GKS colour indices into GDDM
*                 equivalents
*     RGBCOL      Used for converting GKS RGB values into GDDM colour
*                 indices
*     GDDMLT      Used for converting GKS linetypes into GDDM
*                 equivalents
*
*     ICHUNK      Size of stack used for splitting polylines
*     PROMPT      The prompt preceding input implemented by keyboard
*     STRING      String of characters representing string input
*     VALSTR      String of characters representing valuator input
*     IOFF        Offset for stack
*     I           Temporary integer
*     I1,I2,I3    Temporary integer
*     N           Count (temporary)
*     ICHOIC      Choice number returned
*     IFLD        Field identifier
*     NOUT        Number of bytes returned on input
*     WCX,WCY     Cell array rectangle corners in WC
*     INTA        Local integer array with multiple uses:
*                 - receiving input device state (size 10 max)
*                 - setting representations (size 1)
*                 - sending escape characters in linestyle selection (size 1)
*                 - receiving WDT for Inq Text Facil (size 19)
*                 - sending NULs in Open Wkstn (size 1)
*     REALA       Local real array with multiple uses:
*                   - input device state (size 7 max)
*                   - receiving WDT for Inq Text Facil (size 12)
*     FORMT       Character variable to hold format in Request Valuator
*     CHRSET      Character variable to hold character set
*
      INTEGER    IGHTCH
      PARAMETER (IGHTCH=10)

      INTEGER IPREC(KFNTMX)
      INTEGER GDDMLT(5),GDDMCOL(9),RGBCOL(0:7)
      INTEGER    ICHUNK,ICHDVC,IDI,IDT
      PARAMETER (ICHUNK=200)
      CHARACTER PROMPT*50, VALSTR*12, STRING*80
      CHARACTER*7 FORMT
      CHARACTER CHRSET*95
      INTEGER IOFF, I, N,ICHOIC,I1,I2,I3,
     :        NOUT, IFLD
      REAL    WCX(4),WCY(4)
      INTEGER INTA(19)
      REAL REALA(12)

      DATA GDDMLT/0,5,1,3,6/
      DATA GDDMCOL/8,7,2,4,1,6,5,3,8/
      DATA RGBCOL/8,2,4,6,1,3,5,7/
      DATA CHRSET/' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRS
     +TUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~'/
*
*  STACK USAGE
*  -----------
*     POLYLINE and POLYMARKER for transformations
*
*  ERRORS
*  ------
*      32   Specified workstation is not of category MO
*      34   Specified workstation is not of category MI
*      93   Colour index is invalid
*     104   Cannot generate GDP
*     180   Specified function is not supported
*
*---------------------------------------------------------------------



*  Intercept request for Pick Echoplay for primitive
      IF(KPKECO .EQ. KPECHO)THEN
        IF((IENT .GE. KPL) .AND. (IENT .LE. KGDP))THEN
*         Echoplay requested, close GDDM segment to make echoplay
*         into non-retainable graphics. Go on to do echoplay.
          CALL GSSCLS
        ENDIF
      ELSEIF(KPKECO .EQ. KPUNEC)THEN
        IF((IENT .GE. KPL) .AND. (IENT .LE. KGDP))THEN
*         Do nothing. UNECHO occurs automatically, because echoplay
*         occurs outside of a GDDM segment and hence is not retained.
          GOTO 9999
        ENDIF
      ENDIF

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

      GOTO (1200,1111,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1111,1111,1111,1330,1111,1111,1111,1370,1111,1111,
     :      1111,1111,1111,1111,1440,1111,1460,1111,1111,1111,
     :      1111,1111,1111,9999,9999,9999,9999,9999,9999,9999,
     :      9999,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1111,1111,1111,1730,1111,1111,1111,1111,1111,1790,
     :      1111,1111,1111,1111,1111,1850,1111,1870,1880,1111,
     :      1111,1111,1111,1111,1111,1111,1960,1111,1111) IENT-119

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
*   Initialise GDDM
          CALL FSINIT
          CALL FSQURY(0,3,4,INTA)
          CALL GSFLD(1,1,INTA(1),INTA(2))
          CALL GSUWIN(0.0,1400.0,0.0,1000.0)
          IF (INTA(3).EQ.12.AND.INTA(4).EQ.9)THEN
             CALL GSLSS(3,'GKSHTCHC',0)
             KWKDAT(IGHTCH,KWKIX) = 1
          ELSE
             KWKDAT(IGHTCH,KWKIX) = 0
          ENDIF
          CALL GSLSS(4,'GKSMARKS',0)
          CALL GSSEG(0)
*   Move cursor to bottom right (as far out of the way as possible)
          CALL ASFCUR(0,INTA(1),INTA(2))
          KWI1 = GOUTIN
        ENDIF
      ENDIF

*   Initialise colour map
      DO 15 I=1,9
         KWKDAT(I,KWKIX)=GDDMCOL(I)
  15  CONTINUE
      GOTO 9999



* Close workstation
   20 CONTINUE
      IF (KWI1.EQ.1) THEN
        KWDONE = KRFUSE
      ELSE
        CALL GKIOCL(KFWKT,KCID(KWKIX),KWCID(KWKIX))
        CALL GKCWSL(KWKIX)
        CALL GKSLDL(KSSGPT(KWKIX))
        CALL GSSCLS
        CALL FSTERM
      ENDIF
      GOTO 9999



* Clear workstation
   30 CONTINUE
      IF( KWI1.EQ.2 ) THEN
        KWDONE=KRFUSE
        CALL GKSLDL(KSSGPT(KWKIX))
        CALL ASREAD(I1,I2,I3)
      ENDIF
      GOTO 9999



* Redraw all segments on workstation
   40 CONTINUE
      KWDONE = KRFUSE
      GOTO 9999



* Update workstation
   50 CONTINUE
      KWDONE = KRFUSE
      CALL ASREAD(I1,I2,I3)
      GOTO 9999



* Set deferral state
   60 CONTINUE
      KDFM(KWKIX) = KWI1
      KIMRGM(KWKIX) = KWI2
      IF (KWI1.EQ.GASAP) THEN
        KWIO(KWKIX) = GYES
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
      KWI1 = KNFAUP(KWKIX)
      GOTO 9999



* Clear display surface
   80 CONTINUE
      IF (KWI1.EQ.GALWAY .OR. KDSMT(KWKIX).EQ.GNEMPT) THEN
        CALL GSCLR
C       CALL GSSATI(4,2)
C       CALL GSSEG(1)
        CALL GSSEG(0)
      ENDIF
      CALL GKWCLD
      GOTO 9999



* Message
  100 CONTINUE
      IF (NID.LE.80) THEN
         CALL GKATON(NID,IDAT,STR)
      ELSE
         CALL GKATON(80,IDAT,STR)
      ENDIF
      CALL ASDFLD(1,1,60,4,20,2)
      CALL ASFCOL(1,2)
      CALL ASCPUT(1,NID,STR(1))
      GOTO 9999



* Escape
  110 CONTINUE
      KERROR = 180
      GOTO 9999



* Polyline
  120 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.NE.0) GOTO 8888

* Set up device colour
      CALL GK0ISC (KWPLCI(KWKIX))

* Set up linetype
      CALL GSLT(GDDMLT(KWLNTY(KWKIX)))
      I1=QWLNWD(KWKIX)
      CALL GSLW(I1)
      N = ICHUNK
      DO 122 I=1,NRD,ICHUNK-1
         IF (NRD-I.LT.ICHUNK-1) N = NRD-I + 1
         IF (N.EQ.1) GOTO 122
         CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
         CALL GKLCLP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),.FALSE.,0.0,
     :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK0ILN)
122   CONTINUE

* Reset linetype if necessary
      IF (KWLNTY(KWKIX).NE.1) CALL GSLT(GDDMLT(1))

      CALL GKSTDA(KREALS,IOFF)

      GOTO 8888



* Polymarker
  130 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
* Set up device colour
      CALL GK0ISC(KWPMCI(KWKIX))

      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN
        CALL GSMS(KWMKTY(KWKIX)+100)
        IF(KWMKTY(KWKIX).EQ.1) THEN
           CALL GSMSC(0.0001)
        ELSE
           CALL GSMSC(QWMKSZ(KWKIX))
        ENDIF

        N = ICHUNK
        DO 132 I=1,NRD,ICHUNK
          IF (NRD-I.LT.ICHUNK) N = NRD-I + 1
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
          CALL GK0IMK(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),
     :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX))
  132   CONTINUE
        CALL GKSTDA(KREALS,IOFF)
      ENDIF
      GOTO 8888



* Text
  140 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
* Set up device colour
      CALL GK0ISC(KWTXCI(KWKIX))
      CALL GKXDWO(NID,IDAT,GK0ILN)
      GOTO 8888



* Fill area
  150 CONTINUE

      IF (KCVIS.EQ.GINVIS) GOTO 9999

*     Set up device colour
      CALL GK0ISC(KWFACI(KWKIX))
*
      IF(KPKECO .EQ. KPECHO)THEN
*     If Pick echoplay, solid fill
         I = KWFAIS(KWKIX)
         KWFAIS(KWKIX) = GSOLID
         CALL GKFILH(NRD,RX,RY,GK0IFA)
         KWFAIS(KWKIX) = I
      ELSE
*     Normal Fill Area
        IF(KWFAIS(KWKIX) .EQ. GPATTR)THEN
*       For patterned fill, fill in with scan-lines of pattern
           CALL GK0IFL(NRD,RX,RY)
        ELSEIF(KWFAIS(KWKIX).EQ.GHATCH .AND.
     :         KWKDAT(IGHTCH,KWKIX).EQ.0)THEN
*       For hatched fill if no GDDM hatching is available
           CALL GKFILC(NRD,RX,RY,2,GK0ILN)
        ELSE
*       For other styles use GDDM GSAREA within GK0IFA with clipping
*       handled by GKFILH.
           CALL GKFILH(NRD,RX,RY,GK0IFA)
        ENDIF
      ENDIF

      GOTO 8888



* Cell array
  160 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999

*     Get cell array box
      WCX(1) = QWR1
      WCY(1) = QWR2
      WCX(2) = QWR5
      WCY(2) = QWR6
      WCX(3) = QWR3
      WCY(3) = QWR4
      WCX(4) = WCX(3) - WCX(2) + WCX(1)
      WCY(4) = WCY(3) - WCY(2) + WCY(1)

*     Check for pick echoplay.
      IF(KPKECO .EQ. KPECHO) GOTO 163

*     Draw cell array box in black, to block out anything behind.
      I = KWFAIS(KWKIX)
      KWFAIS(KWKIX) = GSOLID
      CALL GSCOL(8)
      CALL GKFILH (4,WCX,WCY,GK0IFA)
      KWFAIS(KWKIX) = I

*     Draw Cell Array
      I=((KWI5+7)/8)*KWI6
      CALL GK0ICA (NID,IDAT,GK0IRO)
      GOTO 8888
*
  163 CONTINUE
*     Pick Echoplay - Draw cell array box solid white
      I = KWFAIS(KWKIX)
      KWFAIS(KWKIX) = GSOLID
      CALL GSCOL (7)
      CALL GKFILH (4,WCX,WCY,GK0IFA)
      KWFAIS(KWKIX) = I
      GOTO 8888
*
* GDP
  170 CONTINUE
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
* Set up device colour
        CALL GK0ISC(KWPLCI(KWKIX))
        CALL GK0ICR(KWI1,NRD,RX,RY,1,KWLNTY(KWKIX).NE.1,20.0)
      ELSE
* Filled chord, pie, circle

* Set up device colour
        CALL GK0ISC(KWFACI(KWKIX))

        IF(KPKECO .EQ. KPECHO)THEN
*         Pick echoplay, use solid fill.
          I = KWFAIS(KWKIX)
          KWFAIS(KWKIX) = GSOLID
          CALL GK0ICR(KWI1,NRD,RX,RY,1,.FALSE.,20.0)
          KWFAIS(KWKIX) = I
        ELSE
*         Normal filled GDP
          CALL GK0ICR(KWI1,NRD,RX,RY,1,.FALSE.,20.0)
        ENDIF
      ENDIF
      GOTO 8888


* Set polyline attributes
  180 CONTINUE
      CALL GKDPLB
* Need to check because individual settings won't have been checked.
      IF (KWLNTY(KWKIX).LT.0 .OR. KWLNTY(KWKIX).GT.5) KWLNTY(KWKIX) = 1
      IF (KWPLCI(KWKIX).GE.KPCI(KWKIX)) KWPLCI(KWKIX) = 1
      GOTO 9999


* Set polymarker attributes
  190 CONTINUE
      CALL GKDPMB
* Need to check because individual settings won't have been checked.
      IF (KWMKTY(KWKIX).LT.0 .OR. KWMKTY(KWKIX).GT.5) KWMKTY(KWKIX) = 3
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
*   Convert pattern reference point to Raster coordinates
      QWPAX(KWKIX) = (KDSRX(KWKIX)/QDSDX(KWKIX))*QWPAX(KWKIX)+1.0
      QWPAY(KWKIX) = (KDSRY(KWKIX)/QDSDY(KWKIX))*QWPAY(KWKIX)+1.0
*   Need to check because individual settings won't have been checked.
      IF (KWFAIS(KWKIX).EQ.GHATCH) THEN
        IF (KWFASI(KWKIX).GT.-1 .OR. KWFASI(KWKIX).LT.-10)
     :    KWFASI(KWKIX) = -1
      ENDIF
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


* Set pick identifier
  220 CONTINUE
      GOTO 9999


* Set polyline representation
  230 CONTINUE
      INTA(1) = 5
      CALL GKSRPL(1,INTA,.FALSE.)
      GOTO 9999


* Set polymarker representation
  240 CONTINUE
      CALL GKSRPM(0,INTA,.FALSE.)
      GOTO 9999


* Set text representation
  250 CONTINUE
      IF( KWI3.EQ.GSTRKP ) THEN

*       Stroke Precision
*       Make sure that fonts are available
        IF( KDBFLS.EQ.KFLNA ) THEN
          KERROR = -1009
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
      CALL GKSRFA(.FALSE.)
      GOTO 9999



* Set pattern representation
  270 CONTINUE
      CALL GKSRPA(NID,IDAT)
      GOTO 9999



* Set colour representation
  280 CONTINUE

      IF(KWI1.GE.0.AND.KWI1.LT.KPCI(KWKIX)) THEN
*   Put set values in for inQuire Colour Representation (set values).
         QHP(KHPXR(KCTBPT(1,KWKIX))+KWI1) = QWR1
         QHP(KHPXR(KCTBPT(2,KWKIX))+KWI1) = QWR2
         QHP(KHPXR(KCTBPT(3,KWKIX))+KWI1) = QWR3

*   Background colour can not be changed, set colour only if not 0.
         IF(KWI1 .GT. 0)THEN
*   IBM isn't an RGB machine so map RGBs onto nearest colour index
           I1=NINT(QWR1)
           I2=(NINT(QWR2))*2
           I3=(NINT(QWR3))*4
           KWKDAT(KWI1+1,KWKIX)=RGBCOL((I1+I2+I3))
         ENDIF
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
* First, check that the PET number is valid - only one supported
      IF (KWI2.EQ.1) THEN
        CALL GKINIP(GLOCAT,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        KERROR = 144
      ENDIF
      GOTO 9999


* Initialise stroke
  620 CONTINUE
* First, check that the PET number is valid - only one supported
      IF (KWI2.EQ.1) THEN
        CALL GKINIP(GSTROK,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        KERROR = 144
      ENDIF
      GOTO 9999


* Initialise valuator
  630 CONTINUE
* First, check that the PET number is valid - only one supported
      IF (KWI2.EQ.1) THEN
        CALL GKINIP(GVALUA,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        KERROR = 144
      ENDIF
      GOTO 9999


* Initialise choice
  640 CONTINUE
* First, check that the PET number is valid - only one supported
      IF (KWI2.EQ.1) THEN
        CALL GKINIP(GCHOIC,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        KERROR = 144
      ENDIF
      GOTO 9999


* Initialise pick
  650 CONTINUE
* First, check that the PET number is valid - only one supported
      IF (KWI2.EQ.1) THEN
        CALL GKINIP(GPICK,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        KERROR = 144
      ENDIF
      GOTO 9999


* Initialise string
  660 CONTINUE
* First, check that the PET number is valid - only one supported
      IF (KWI2.EQ.1) THEN
        CALL GKINIP(GSTRIN,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        KERROR = 144
      ENDIF
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
*     Call request locator utility
      CALL GKRQLC (GK0ICS,RX(1),RY(1))
      GOTO 9999



* Request stroke
  700 CONTINUE
      CALL GK0IRK(NRD,RX,RY)
      GOTO 9999



* Request valuator
  710 CONTINUE

*   Get valuator device information
      CALL GKRQIP(GVALUA,KWI1,4,7,INTA,REALA)
      IF(KERROR.NE.0)GOTO 9999

*   Output prompt, and get input from operator
      WRITE(PROMPT, '(A,G12.4,A,G12.4,A)' )
     :   'Value between ',REALA(KVLMNV),'and ',REALA(KVLMXV),' :'

*   Set up fields & display prompt
      CALL ASDFLD(1,1,55,2,26,2)
      CALL ASDFLD(2,3,55,1,12,0)
      CALL ASFCOL(1,5)
      CALL ASFCOL(2,3)
      CALL ASFHLT(2,4)
      CALL ASCPUT(1,46,PROMPT)
      CALL ASCPUT(2,12,'            ')
      CALL ASFCUR(2,1,1)
      CALL ASREAD(IDT,I,NOUT)

*   If choice is break (clear key)
      IF (IDI.EQ.5) GOTO 713
      CALL ASCGET(2,12,VALSTR)
      CALL ASDFLD(1,0,0,0,0,0)
      CALL ASDFLD(2,0,0,0,0,0)

      NOUT=12
      DO 711 I =12,1,-1
         N=INDEX(CHRSET,VALSTR(I:I))
         IF(N.NE.1)THEN

*   See whether number is a digit or decimal point
            IF((N.LT.15).OR.(N.EQ.16).OR.(N.GT.26)) GOTO 713
         ELSE
            NOUT=I-1
         ENDIF
  711 CONTINUE
*   Interpret the incoming string. If not recognisable then set
*   status = none.

      IF( NOUT.LT.1 ) THEN
          GOTO 713
      ELSE
*       Convert to a real number
          WRITE(FORMT, '(A,I2,A)')  '(F',NOUT,'.0)'
          READ(VALSTR(1:NOUT), FORMT, ERR=713) QWR1
          GOTO 718

*       No good
  713     KWI1=GNONE
          QWR1=QNIL
          GOTO 719

*       Here QWR1 contains a valid real number. Is it in range?
  718     IF( REALA(KVLMNV).LE.QWR1 .AND. QWR1.LE.REALA(KVLMXV) ) THEN
              KWI1=GOK
          ELSE
              KWI1=GNONE
              QWR1=QNIL
          ENDIF

*       Drop to 719
      ENDIF
  719 GOTO 9999



* Request choice
  720 CONTINUE

*   Get choice device information
      CALL GKRQIP(GCHOIC,KWI1,6,4,INTA,REALA)
      IF(KERROR.NE.0)GOTO 9999
      ICHDVC=KWI1
      IF(ICHDVC.EQ.1)THEN
         PROMPT='Enter Choice from 1 to9, using PF keys:   '
      ELSEIF(ICHDVC.EQ.2)THEN
         ICHDVC=10
         PROMPT='Enter Choice from 1 to2, using mouse keys:'
      ENDIF
      CALL GSQLID(1,ICHDVC,1,I)
      IF (I.NE.-1)THEN
         CALL GSENAB(1,0,1)
         CALL GSENAB(1,5,1)
         CALL GSENAB(1,ICHDVC,1)
      ELSE
         KERROR=140
         GOTO 9999
      ENDIF
      CALL ASDFLD(1,1,59,2,22,2)
      CALL ASFCOL(1,5)
      CALL ASCPUT(1,42,PROMPT)
      CALL GSREAD(1,IDT,IDI)
      CALL ASDFLD(1,0,59,2,22,2)

*   If input type not choice, or choice is break (clear key)
      IF ((IDT.NE.1).OR.(IDI.EQ.5)) THEN
         KWI1=GNONE
         KWI2=KNIL
      ELSE
         CALL GSQCHO(ICHOIC)
         IF( 1.LE.ICHOIC .AND. ICHOIC.LE.9 ) THEN
            KWI1=GOK
            KWI2=ICHOIC
         ELSE IF (ICHOIC.EQ.0) THEN
            KWI1 = GNCHOI
            KWI2 = KNIL
         ELSE
            KWI1=GNONE
            KWI2=KNIL
         ENDIF


      ENDIF
      CALL GSENAB(1,0,0)
      CALL GSENAB(1,5,0)
      CALL GSENAB(1,ICHDVC,0)
      GOTO 9999



* Request pick
  730 CONTINUE
*     First, change Text precision indicator to STROKE
*     so that it acts as though text is at stroke precision.
*     Then we don't need a GK0IXF (use 2nd GK0ICS as dummy argument).
*     Save original value
      I = KWTXPR(KWKIX)
      KWTXPR(KWKIX) = GSTRKP

*     Call pick utility with a pick aperture of 10 DC units
      CALL GKRQPK (10.0,GK0IPP,GK0ICS,GKXXF)

*     Restore Text precision indicator
      KWTXPR(KWKIX) = I
      GOTO 9999



* Request string
  740 CONTINUE

*   Get string device information
      CALL GKRQIP(GSTRIN,KWI1,9,4,INTA,REALA)
      IF(KERROR.NE.0)GOTO 9999

*   Get initial string from heap
      IF(INTA(KSTINS) .NE. KNIL)THEN
        CALL GKHPGI (INTA(KSTINS),0,INTA(KSTINL),IDAT)
      ENDIF

*   Translate from integers to characters
      STRING = ' '
      CALL GKATON(INTA(KSTINL),IDAT,STRING(1:INTA(KSTINL)))

*   Set up field for string - One underscored line length 22
*                             along top right of screen
*                             unprotected to take alphanumeric data
      CALL ASDFLD (1,1,59,1,22,0)
      CALL ASFCOL (1,5)
      CALL ASFHLT (1,4)

*   Put in initial string and set initial cursor position
      CALL ASCPUT(1,INTA(KSTINL),STRING)
      CALL ASFCUR(1,1,INTA(KSTICP))

*   Get String Input
      CALL ASREAD(IDT,I,NOUT)
      CALL ASCGET(1,80,STRING)
      IFLD = 1
      CALL ASQCUR(1,IFLD,I1,I2)
      KNIR = I2-1

*   Delete the field
      CALL ASDFLD (1,0,0,0,0,0)

*   Interpret String Input
      IF(IFLD.EQ.1) THEN
*     Cursor is in string field;
*     output upto and including cursor position.
         CALL GKNTOA(KNIR,STRING(1:KNIR),IDAT)
         IF(KERROR .EQ. 101)THEN
*        String is unrecognisable; break
            KWI1 = GNONE
            KNIR = 0
            KERROR = 0
         ELSE
*        Assumed OK
            KWI1 = GOK
         ENDIF
      ELSE
*     Cursor is out of string field; break
         KWI1 = GNONE
         KNIR = KNIL
      ENDIF

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


*  Inquire Pixel Array Dimensions
 1200 CONTINUE
*     Raster coordinates are different from DC, therefore call GKPXAD.
      CALL GKPXAD
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



* Inquire colour representation
 1440 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
*
      IF(KWI2.EQ.GREALI)THEN
*       Realised RGB values
        IF(KWI1 .EQ. 0)THEN
*         Background always black
          QWR1 = 0.0
          QWR2 = 0.0
          QWR3 = 0.0
        ELSE
*         Nearest of White, Red, Green, Blue, Yellow, Magenta, Cyan &
*         Black - Take nearest integer of each RGB value
          QWR1 = FLOAT(NINT(QWR1))
          QWR2 = FLOAT(NINT(QWR2))
          QWR3 = FLOAT(NINT(QWR3))
        ENDIF
      ENDIF
      GOTO 9999


* Inquire set members of segment names on workstation
 1460 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KSGRQ = KSGRQ + 1
      GOTO 9999



* Inquire dynamic modification of workstation attributes
 1730 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999


* Inquire text facilities ... on entry KWI1 specifies list element requested
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
            KERROR = -1009
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
      KWI1 = KWI3
      KWI2 = GCOLOR
      GOTO 9999



* Inquire List Element of Available Generalised Drawing Primitives
 1870 CONTINUE
      IF (KWI1.GE.1 .AND. KWI1.LE.4) THEN
        KWI2 = -KWI1
      ELSE
        KERROR = 2002
        KWI2 = KNIL
      ENDIF
      KWI1 = 4
      GOTO 9999


* Inquire Generalised Drawing Primitive
 1880 CONTINUE
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


* Inquire default choice device data
 1960 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KWI4=9
      GOTO 9999


*   Here after all output primitives to sort out buffering etc
 8888 CONTINUE
      KDSMT(KWKIX) = GNEMPT
*   Reopen GDDM segment after echoplay, so that graphics is retainable.
      IF(KPKECO .EQ. KPECHO) CALL GSSEG(0)

 9999 CONTINUE

      END
      SUBROUTINE GK0ICS(ICODE,XDC,YDC,INTA,EAREA)
C====================================================C
C=                                                  =C
C  THIS ROUTINE HANDLES THE GDDM REQUEST LOCATOR     C
C  FOR GKS LOCATOR AND PICK INPUT.                   C
C====================================================C

      INCLUDE '../../include/check.inc'
*
*  ARGUMENTS
*  ---------
*     OUT ICODE Code determining if a break has occurred
*     I/O XDC   Initial locator position -> output locator position DC
*     I/O YDC   Initial locator position -> output locator position DC
*     INP INTA  Integer Input Device Data
*     INP EAREA Echo Area
*
      INTEGER ICODE, INTA(4)
      REAL XDC,YDC, EAREA(4)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkinp.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     IX,IY   Point
*     INTA,REALS Arrays to hold info returned by GKRQIP
*     IBREAK  Code to indicate break
*     IVALID  Code to indicate valid point
*
      INTEGER IDT,IDI,II

      INTEGER    IBREAK,    IVALID
      PARAMETER (IBREAK=26, IVALID=32)
*
*---------------------------------------------------------------------


      CALL GSILOC(1,2,XDC,YDC)

      CALL GSENAB(2,1,1)
      CALL GSENAB(1,5,1)
      CALL GSREAD(1,IDT,IDI)
*    Clear is the break character
      IF (IDT.EQ.1) THEN
*    Break
        ICODE = IBREAK
      ELSE
*    Get point in pseudo device coordinates
        CALL GSQLOC(II,XDC,YDC)
        ICODE = IVALID
      ENDIF

* Turn cursor off
      CALL GSENAB(1,5,0)
      CALL GSENAB(2,1,0)

      END
      SUBROUTINE GK0IRK(NRD,XNDC,YNDC)
C====================================================C
C=                                                  =C
C  THIS ROUTINE IMPLEMENTS REQUEST STROKE USING      C
C  THE GDDM REQUEST LOCATOR ROUTINE.                 C
C====================================================C

*
      INCLUDE '../../include/check.inc'
*
*  ARGUMENTS
*  ---------
*     INP NRD    Length of arrays XNDC,YDC
*     OUT XNDC   Coordinates of points
*     OUT YNDC   Coordinates of points
*
      INTEGER NRD
      REAL XNDC(NRD), YNDC(NRD)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkinp.par'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
      INTEGER I, INTA(10)
      INTEGER NIPTS2, IOFFX, IOFFY,IDT,IDI,II
      REAL XDC(2), YDC(2), REALA(10)
*
*---------------------------------------------------------------------


* Limit stroke to 100 points
      IF (NRD.GT.100) NRD = 100

* First, get stroke device information
      CALL GKISK(NRD, KNRR, XNDC, YNDC, INTA, REALA,
     :           NIPTS2, IOFFX, IOFFY)
      IF (KERROR.NE.0) GOTO 9999
* Draw initial stroke if there are any points and echo is set
      IF (NIPTS2.GE.2 .AND. INTA(KIPE).EQ.GECHO)
     :  CALL GK0ILN(NIPTS2, QSTACK(IOFFX), QSTACK(IOFFY))
* Save first point. After that we have no further need of the stack.
      IF (NIPTS2.GE.1) THEN
         XDC(1) = QSTACK(IOFFX+NIPTS2-1)
         YDC(1) = QSTACK(IOFFY+NIPTS2-1)
      ENDIF
      CALL GKSTDA(KREALS, IOFFX)
* If no points in initial stroke, set value up for later
      IF (KNRR.EQ.0) KNRR = 1
*    Get point in pseudo device coordinates
      CALL GSILOC(1,2,XDC,YDC)

      CALL GSENAB(2,1,1)
      CALL GSENAB(1,0,1)
      CALL GSENAB(1,5,1)

* Get stroke
      DO 10 I=KNRR,NRD
  9      CONTINUE
         CALL GSREAD(1,IDT,IDI)

         IF (IDT.EQ.1) THEN
*     Clear is the break character
            IF (IDI.EQ.5) GOTO 30
*     Enter is the trigger
            IF (IDI.EQ.0) GOTO 20
         ENDIF
*    Get point in pseudo device coordinates
         CALL GSQLOC(II,XDC(2),YDC(2))
         CALL GKTDN(1,XDC(2),YDC(2),XNDC(I),YNDC(I))
         IF(KERROR .NE. 0)THEN
*        If point is outside workstation window, try again
             KERROR = 0
             GOTO 9
         ENDIF
* Echo, ie draw line from previous point
         IF (I.GT.1) CALL GK0ILN(2,XDC,YDC)
         XDC(1) = XDC(2)
         YDC(1) = YDC(2)
   10 CONTINUE
      KNRR = I+1

   20 CONTINUE
      KWI1 = GOK
      KNRR = I-1
      GOTO 40
* Handle break
   30 KWI1=GNONE
   40 CONTINUE
* Turn cursor off
      CALL GSENAB(1,0,0)
      CALL GSENAB(1,5,0)
      CALL GSENAB(2,1,0)

9999  CONTINUE
      END
      SUBROUTINE GK0IPP(ITYPE,XP,YP,NR,RX,RY,BOXDEF,SQPKAP,
     :                  SQDIST,INSIDE)
*
*---------------------------------------------------------------------
*
*  Author:             KEVP
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Primitive Pick scan routine
*
*  ARGUMENTS
*  ---------
*     INP  ITYPE   Type of primitive
*     INP  XP,YP   The Pick Point
*     INP  NR      Number of points
*     INP  RX,RY   The points (coord system determined by ITYPE)
*     INP  BOXDEF  True, if bounding box needs defining
*     INP  SQPKAP  Squared Pick aperture in Square DC units
*     I/O  SQDIST  Nearest squared distance any primitive so far
*                  in Square DC units
*     OUT  INSIDE  True, if pick point is inside primitive
*
      INTEGER ITYPE, NR
      REAL XP,YP, RX(NR),RY(NR), SQPKAP, SQDIST
      LOGICAL BOXDEF, INSIDE
*
*---------------------------------------------------------------------
*
*     Tolerance for direct hit is 1 DC unit (about half a pixel).
      CALL GKPPPR(ITYPE,XP,YP,NR,RX,RY,BOXDEF,1.0,SQPKAP,
     :            SQDIST,INSIDE)
      END
      SUBROUTINE GK0IRO(X,Y,NXPIX,NYPIX,NXDIM,ICOLAR)
C====================================================C
C=                                                  =C
C  THIS ROUTINE OUTPUTS AN NXPIX BY NYPIX CELL ARRAY C
C  USING THE GDDM GSIMG ROUTINE.                     C
C====================================================C
C  NB: The area on which the cell array is put must  C
C      be coloured black, otherwise things drawn     C
C      behind may show through.                      C
C====================================================C
*
      INCLUDE '../../include/check.inc'
*
*  ARGUMENTS
*  ---------
*     INP X,Y    - coordinates of raster origin (raster coords)
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
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     CHASTR  Character String Containing Raster Row (max 2048 pxls)
*     XA,YA   Starting Point in Raster Coordinates
*     XFACTR,
*      YFACTR Conversion factor to Raster Coordinates
*     IROW    Row
*     ICLM    Column
*
*     ICOL    Current colour index
*     IGCOL   GDDM code for current colour index
*
*     IRGB    Do loop index over RGB (1=RED,2=GREEN,3=BLUE)
*     IARR    Array containing integers packed to be converted
*             into a characters (one for each RGB element)
*     IGCOLA  Array containing GDDM codes for each RGB element.
*
*     CHN     Current position in character string
*     ODBITS  Number of odd bits for padding
*     BITCNT  Position in block of 8 pixels to be converted into
*             a character.
*     JUMP    Jump to miss out padding if row length is
*             divisible by 8.
*
      CHARACTER*256 CHASTR(3)
      REAL XA(1), YA(1),XFACTR,YFACTR
      INTEGER IROW, ICLM, ICOL,IGCOL,IRGB,IARR(3),IGCOLA(3)
      INTEGER CHN,ODBITS,BITCNT,JUMP

      DATA IGCOLA /2,4,1/
*
*  ALGORITHM
*  ---------
*     The colour mixing mode is set to mix mode.
*
*     Each block of 8 cells/pixels is split into RED, GREEN and BLUE
*     components and each of these components is packed into a string
*     of its colour (RED, GREEN or BLUE) .
*
*     Each of these three strings is sent by GSIMG set to its colour.
*
*     The colour mixing mode is set back to overpaint
*
*
*---------------------------------------------------------------------

*   Set factor for mapping pseudo-device co-ords to pixels
      XFACTR=QDSDX(KWKIX)/KDSRX(KWKIX)
      YFACTR=QDSDY(KWKIX)/KDSRY(KWKIX)
      XA(1) = X*XFACTR + 0.0*XFACTR
      YA(1) = Y*YFACTR + 1.0*YFACTR

*   GSIMG needs a multiple of 8 pixels, so calculate the number of
*   pixels to be padded
      ODBITS=MOD(NXPIX,8)
      IF(ODBITS.GT.0)THEN
         JUMP=0
      ELSE
         JUMP=1
      ENDIF

* Set colour mixing mode to mix mode (OR on bits of GDDM colour code)
      CALL GSMIX(1)

* Output each row of raster
      DO 200 IROW=1,NYPIX

* Move to beginning of this row
         YA(1) = YA(1) - 1.0*YFACTR
         CALL GSMOVE(XA, YA)
         CHN=0

*   Pack the pixels into a character string, using a different string
*   for each RGB component,
*   since GSIMG can only output 1 colour bit at a time.
         DO 100 ICLM=1,NXPIX-ODBITS,8
            DO 11 IRGB=1,3
               IARR(IRGB)=0
 11         CONTINUE

*   Set the appropriate bits for each RGB component
            DO 30 BITCNT=0,7
               DO 20 IRGB=1,3
                  IARR(IRGB)=IARR(IRGB)*2
 20            CONTINUE
               ICOL = MOD((ICOLAR(ICLM+BITCNT,IROW)),8)
               IGCOL = KWKDAT(ICOL+1,KWKIX)
               IARR(1) = IARR(1) + MOD(IGCOL/2,2)
               IARR(2) = IARR(2) + MOD(IGCOL/4,2)
               IARR(3) = IARR(3) + MOD(IGCOL  ,2)
 30         CONTINUE
            CHN=CHN+1

*   Pack the bytes into the character strings
            DO 40 IRGB=1,3
               CHASTR(IRGB)(CHN:CHN)=CHAR(IARR(IRGB))
 40         CONTINUE
 100     CONTINUE

*   If pixels is a multiple of 8, output string; otherwise calculate
*   padding bits
         GOTO(110,170)JUMP+1
 110     CONTINUE

         DO 120 IRGB=1,3
           IARR(IRGB)=0
 120     CONTINUE

*   Calculate leftover and padding pixels
         DO 140 BITCNT=1,ODBITS
            DO 130 IRGB=1,3
               IARR(IRGB)=IARR(IRGB)*2
 130        CONTINUE
            ICOL = ICOLAR(NXPIX-ODBITS+BITCNT,IROW)
            IGCOL = KWKDAT(ICOL+1,KWKIX)
            IARR(1) = IARR(1) + MOD(IGCOL/2,2)
            IARR(2) = IARR(2) + MOD(IGCOL/4,2)
            IARR(3) = IARR(3) + MOD(IGCOL  ,2)
 140     CONTINUE
         CHN=CHN+1
*   Pack the bytes into the character strings
         DO 150 IRGB=1,3
            IARR(IRGB)=IARR(IRGB)*(2**(8-ODBITS))
            CHASTR(IRGB)(CHN:CHN)=CHAR(IARR(IRGB))
 150     CONTINUE

*   Output the strings
 170     CONTINUE
         DO 180 IRGB=1,3
            CALL GSCOL(IGCOLA(IRGB))
            CALL GSIMG(0,NXPIX,1,CHN,CHASTR(IRGB))
 180     CONTINUE
 200  CONTINUE

*   Return to overpaint mode
      CALL GSMIX (2)

      RETURN
      END
      SUBROUTINE GK0ILN(N,X,Y)
C====================================================C
C=                                                  =C
C  THIS ROUTINE OUTPUTS A POLYLINE.                  C
C====================================================C
*
      INCLUDE '../../include/check.inc'
*
*  ARGUMENTS
*  ---------
*     INP N   - number of points
*     INP X,Y - coordinates of points
*
      INTEGER N
      REAL X(N),Y(N)
*
*---------------------------------------------------------------------
*
      CALL GSMOVE(X(1),Y(1))
      IF (N.EQ.1) THEN
         CALL GSLINE(X(1),Y(1))
      ELSE
         CALL GSPLNE(N-1,X(2),Y(2))
      ENDIF

      RETURN
      END
      SUBROUTINE GK0IMK(N,X,Y,XMIN,YMIN,XMAX,YMAX)
C====================================================C
C=                                                  =C
C  THIS ROUTINE OUTPUTS A POLYMARKER.                C
C====================================================C
*
      INCLUDE '../../include/check.inc'
*
*  ARGUMENTS
*  ---------
*     INP N   - number of points
*     INP X,Y - coordinates of points
*
      INTEGER N,I
      REAL X(N),Y(N),XMIN,XMAX,YMIN,YMAX
*
*---------------------------------------------------------------------
*
      DO 10 I=1,N

* Check if marker inside clipping rectangle.
        IF ( X(I).GE.XMIN .AND. X(I).LE.XMAX .AND.
     :       Y(I).GE.YMIN .AND. Y(I).LE.YMAX ) CALL GSMARK(X(I),Y(I))

 10   CONTINUE
      RETURN
      END
      SUBROUTINE GK0ISC(ICOL)
* --------------------------------------------------------------
*
*     Author:  KEVP  (as part of the fix to bug C78)
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Select colour for next primitive
*
*  ARGUMENTS
*  ---------
*     INP ICOL   New value of colour index
*
      INTEGER ICOL
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkpca.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkpca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     IGCOL      GDDM Colour Code
*
      INTEGER IGCOL
*
*  ALGORITHM
*  ---------
*
*       Set with GSCOL using the GDDM colour code in KWKDAT
*       Set different colour if pick echoplay.
*
* --------------------------------------------------------------

*     Get GDDM colour code
      IGCOL = KWKDAT(ICOL+1,KWKIX)

*     Modify GDDM colour code, if pick echoplay
      IF(KPKECO .EQ. KPECHO)THEN
         IF(IGCOL .EQ. 7)THEN
            IGCOL = 2
         ELSE
            IGCOL = 7
         ENDIF
      ENDIF

*     Set new colour
      CALL GSCOL (IGCOL)

      END
      SUBROUTINE GK0ICA (ISIZ,ICA,ROSUB)
C====================================================C
C=                                                  =C
C  THIS ROUTINE MAPS A CELL ARRAY OF N BY M CELLS    C
C  ONTO A WC RECTANGLE DELIMITED BY ITS ORIGIN, AND  C
C  THE TWO OPPOSITE CORNERS, P AND Q.                C
C  (CONVERTED FROM GKCELL).                          C
C====================================================C
*
      INCLUDE '../../include/check.inc'
*
*  ARGUMENTS
*  ---------
*     INP ISIZ  Cell Array Dimensions
*     INP ICA   Cell Array
*     INP ROSUB Device Driver RasterOut routine
*
      INTEGER ISIZ, ICA(ISIZ)
      EXTERNAL ROSUB
*
*  COMMON BLOCK USAGE
*  ------------------
*    W/S Comms Area:
*     KWKIX     : Workstation Identifier
*     QWR1,QWR2 : WC X,Y of point P
*     QWR3,QWR4 : WC X,Y of point Q
*     QWR5,QWR6 : WC X,Y of point R
*
*     KWI1,KWI2 : dimensions of colour array
*     KWI3,KWI4 : start column, start row
*     KWI5,KWI6 : number of columns, number of rows
*
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkstk.cmn'
*
*  LOCALS
*  ------
*     RETLOY Scan conversion lower limit
*     RETHIY Scan conversion higher limit
*     ILOY   Integer scan conversion lower limit
*     IHIY   Integer scan conversion higher limit
*     XMIN   Clipping rectangle
*     XMAX
*     YMIN
*     YMAX
*     BASEDX Transformed Cell Array vectors
*     BASEDY
*     SIDEDX
*     SIDEDY
*     ISIDE  Pixel Rows
*     IBASE  Pixel Cols
*     IWIDTH Cell columns
*     IDEPTH Cell rows
*     ISCAN  Pixel Array, Cell Array first dimension
*     WCX    WC Coord arrays for transformation
*     WCY
*     DCX    DC Coord arrays for transformation
*     DCY
*     ROX    DC Cell Array Origin Mapping
*     ROY
*     SWAPXY 90 degree rotation flag
*     INVX   Axis reflection flags
*     INVY
*     ISX    Offset for submatrix of colour array
*     ISY
*     IXS    Source DDA controls
*     IYS
*     IXD    Destination DDA controls
*     IYD
*     IXE    DDA controls
*     IYE
*     IC
*     ICOUNT
*     IV     =Four
*     IAB    StackBase for Pixel buffer
*     ZLX,ZRX,ZBY,ZTY  Intersection of cell array bounding rectangle
*                      and clip rectangle in DC
*     ILX,IRX,IBY,ITY  Intersection in integer coordinates
*     ICX,ICY Cell coordinates of back-transformed pixel
*     V,F1,F2,F3,F4  Back-Transformation constants
*     SRX    Start of row after clipping
*     ERX    End of row after clipping
*     NRX    Number of pixels is in row after clipping
*     ISKIPS Number of pixels to skip at start of row after clipping
*     ISKIPE Number of pixels to skip at end of row after clipping
*     DCRCX DCRCY Conversion factors from DC to Raster Coords


      REAL       SMALL
      PARAMETER (SMALL=1.0E-4)
      INTEGER ISIDE, IBASE, IWIDTH, IDEPTH, IXS, IYS, IXD, IYD,
     :        IXE, IYE, IC, ICOUNT, IV, IAB, I, J, IX, IY,
     :        ILX,IRX,ITY,IBY,ICX,ICY,ISCAN,ISX,ISY,NRX,ISKIPS,ISKIPE
      REAL XMIN,YMIN,XMAX,YMAX,BASEDX,BASEDY,
     :     SIDEDX, SIDEDY, WCX(4), WCY(4), DCX(4), DCY(4), ROX, ROY,
     :     V,F1,F2,F3,F4,ZLX,ZRX,ZBY,ZTY,SRX,ERX,RY, DCRCX,DCRCY
      LOGICAL SWAPXY,INVX,INVY
*
*  STACK USAGE
*  -----------
*     Integer workspace at IAB, size IBASE, to hold a raster row
*
*  ERRORS
*  ------
*    300 : Stack Full  -occurs if unable to allocate sufficient stack.
*
*  ALGORITHM
*  ---------
*     See Utility GKCELL
*---------------------------------------------------------------------

* Extract total clip rectangle using W/S ID from Comms Area:
      XMIN=QWCLXL(KWKIX)
      YMIN=QWCLYB(KWKIX)
      XMAX=QWCLXR(KWKIX)
      YMAX=QWCLYT(KWKIX)

* Transform Corners
      IV=4
* [PX,PY] = Point P
      WCX(2) = QWR1
      WCY(2) = QWR2

* [RX,RY] = Point R
      WCX(3) = QWR5
      WCY(3) = QWR6

* [QX,QY] = Point Q
      WCX(4) = QWR3
      WCY(4) = QWR4

* [ZX,ZY] = fourth corner
* ZX = PX + QX - RX
      WCX(1) = QWR1 + QWR3 - QWR5
* ZY = PY + QY - RY
      WCY(1) = QWR2 + QWR4 - QWR6

*
* transform to device coordinates
      CALL GKTWD(IV,WCX,WCY,DCX,DCY)
*
*   The local variables are now the transformation variables  A,B,C,D,E and F.C
*
* Convert to Raster Coordinates
      DCRCX = KDSRX(KWKIX)/QDSDX(KWKIX)
      DCRCY = KDSRY(KWKIX)/QDSDY(KWKIX)
      DO 10 I=1,IV
         DCX(I) = DCX(I) * DCRCX
         DCY(I) = DCY(I) * DCRCY
   10 CONTINUE
      XMIN = XMIN * DCRCX
      XMAX = XMAX * DCRCX
      YMIN = YMIN * DCRCY
      YMAX = YMAX * DCRCY
*
* ..and derive Pixel Coordinate Frame

* (point 1 is Z; point 2 is P; point 3 is R; point 4 is Q )

* Find intersection of bounding box and clip rectangle
      ZLX = DCX(1)
      IF (DCX(2) .LT. ZLX) ZLX = DCX(2)
      IF (DCX(3) .LT. ZLX) ZLX = DCX(3)
      IF (DCX(4) .LT. ZLX) ZLX = DCX(4)
      IF (XMIN .GT. ZLX) ZLX = XMIN

      ZRX = DCX(1)
      IF (DCX(2) .GT. ZRX) ZRX = DCX(2)
      IF (DCX(3) .GT. ZRX) ZRX = DCX(3)
      IF (DCX(4) .GT. ZRX) ZRX = DCX(4)
      IF (XMAX .LT. ZRX) ZRX = XMAX

      ZBY = DCY(1)
      IF (DCY(2) .LT. ZBY) ZBY = DCY(2)
      IF (DCY(3) .LT. ZBY) ZBY = DCY(3)
      IF (DCY(4) .LT. ZBY) ZBY = DCY(4)
      IF (YMIN .GT. ZBY) ZBY = YMIN

      ZTY = DCY(1)
      IF (DCY(2) .GT. ZTY) ZTY = DCY(2)
      IF (DCY(3) .GT. ZTY) ZTY = DCY(3)
      IF (DCY(4) .GT. ZTY) ZTY = DCY(4)
      IF (YMAX .LT. ZTY) ZTY = YMAX

      ILX = INT(ZLX)
      IRX = INT(ZRX)
      IBY = INT(ZBY)
      ITY = INT(ZTY)

* Check for zero intersection

      IF (ZLX .GE. XMAX .OR. ZRX .LE. XMIN .OR.
     :    ZBY .GE. YMAX .OR. ZTY .LE. YMIN) GOTO 100
* (X-Axis is PR)
      BASEDX=DCX(3)-DCX(2)
* (Y-inversion intentional as Y-Axis downwards)
      BASEDY=DCY(2)-DCY(3)

* (Y-Axis is PZ)
      SIDEDX=DCX(1)-DCX(2)
* (Y-inversion intentional as Y-Axis downwards)
      SIDEDY=DCY(2)-DCY(1)

* check for simple cases
      IF (ABS(SIDEDX).GT.SMALL) GOTO 20
      IF (ABS(BASEDY).GT.SMALL) GOTO 20
*** Transformed Array is rectangular,
* and parallel to normal device axes, so..

      SWAPXY=.FALSE.
      IWIDTH = KWI5
      IDEPTH = KWI6
      ISCAN = KWI1
      GOTO 25

   20 CONTINUE
*** Transformed Pixel X,Y Axes not parallel to Device X,Y Axes;
* i.e. SideDX and/or BaseDY significant
*     -check for 90 degree rotation
      IF (ABS(SIDEDY).GT.SMALL) GOTO 70
      IF (ABS(BASEDX).GT.SMALL) GOTO 70

*** SideDY and BaseDX insignificant, i.e. result is rectangular,
*   but parallel to opposite axes.
*   All references to the source Cell Array must swap X and Y
*  (note: calculations on destination Raster unaffected)
      SWAPXY=.TRUE.
* new Width is original Depth
      IWIDTH = KWI6
* new Depth is original Width
      IDEPTH = KWI5
      ISCAN = KWI1

* (simulate transform of flipped source CellArray)
      BASEDX=SIDEDX
      SIDEDY=BASEDY
      SIDEDX=0
      BASEDY=0


   25 CONTINUE
* First reject zero width/height
      IF (NINT(BASEDX).EQ.0) GOTO 100
      IF (NINT(SIDEDY).EQ.0) GOTO 100


* Rectangular
*
* check for reflections
*
      IBASE=NINT(BASEDX)
      ISIDE=NINT(SIDEDY)

      INVX=.FALSE.
      INVY=.FALSE.

* find DC of Raster Origin to cope with reflections
      ROX=DCX(2)
      ROY=DCY(2)
* if IBASE was negative,
*       -we start ABS(IBASE) pixels further to the left
*        and index Cell columns from right to left
      IF (IBASE.LT.0) THEN
         INVX=.TRUE.
         IBASE=-IBASE
         ROX=ROX-IBASE
      ENDIF

* if ISIDE was negative,
*       -we start ABS(ISIDE) pixels higher on the device
*        and index Cell rows from bottom to top
      IF (ISIDE.LT.0) THEN
         INVY=.TRUE.
         ISIDE=-ISIDE
         ROY=ROY+ISIDE
      ENDIF

* Find start column after clipping
      SRX = ROX
      ISKIPS = 0

  110 CONTINUE
      IF (SRX .LT. XMIN) THEN
        SRX = SRX + 1.0
        ISKIPS = ISKIPS + 1
        GO TO 110
      END IF

* Find end column after clipping
      ERX = ROX + FLOAT(IBASE)
      ISKIPE = 0

  120 CONTINUE
      IF (ERX .GT. XMAX) THEN
        ERX = ERX - 1.0
        ISKIPE = ISKIPE + 1
        GOTO 120
      END IF

* Find number of pixels in a row after clipping
      NRX = IBASE - ISKIPS - ISKIPE


* interpolate rows/columns between source and destination

* first initialise the colour array submatrix offsets

      ISX = KWI3
      IF (SWAPXY) ISX = KWI4
      ISY = KWI4
      IF (SWAPXY) ISY = KWI3

* then get enough workspace to hold a raster row:
      CALL GKSTAL(KINTGS,IBASE,IAB)
      IF (KERROR.NE.0) GOTO 100

********************
*                  *
*     CASE 1       *
*                  *
********************

* Rectangular one-to-one mapping

      IF (IBASE .EQ. IWIDTH .AND. ISIDE .EQ. IDEPTH) THEN

        DO 140 IYD = 1,ISIDE

          IY = IYD + ISY - 1
          IF (INVY) IY = ISIDE + ISY - IYD
          DO 130 IXD = 1,IBASE
            IX = IXD + ISX - 1
            IF (INVX) IX = IBASE + ISX - IXD
            IF (SWAPXY) THEN
              KSTACK(IAB+IXD-1) = ICA((IX-1)*ISCAN+IY)
            ELSE
              KSTACK(IAB+IXD-1) = ICA((IY-1)*ISCAN+IX)
            END IF
  130     CONTINUE

          RY = ROY - FLOAT(IYD) + 1.0
          IF (RY .GE. YMIN .AND. RY .LE. YMAX)
     :      CALL ROSUB(SRX,RY,NRX,1,NRX,KSTACK(IAB+ISKIPS))

  140   CONTINUE

        GOTO 160

      END IF

********************
*                  *
*     CASE 2       *
*                  *
********************

* Rectangular, but not one-to-one mapping

* We now have two nested loops, columns within rows,
* Each loop uses a count-up/count-down DDA to handle cases:
*   a) where there are more cells than pixels
*      and cells have to be skipped
*   b) where there are fewer cells than pixels
*      so they must be repeated

* initialise the Y DDA variables:
      IYS=0
      IYD=0
*     IYE=MOD(ISIDE-IDEPTH,ISIDE)
*     IYE=MOD(IYE+ISIDE,ISIDE)
* (not sure what we need for symmetrical DDA !)
      IYE=0

* Outer Loop: spreading IDEPTH Cell rows over ISIDE pixel rows
   30 CONTINUE
* (Re)initialise X DDA variables:
      IXS=0
      IXD=1
*     IXE=MOD(IBASE-IWIDTH,IBASE)
*     IXE=MOD(IXE+IBASE,IBASE)
* (not sure what we need for symmetrical DDA !)
      IXE=0

* Get a source cell start row

*
* skip source rows if necessary
* (count down until negative)
   35 CONTINUE
      IF (IYE.GE.0) THEN
         IYS=IYS+1
         IYE=IYE-ISIDE
         GOTO 35
      ENDIF
* (IYE now negative, IYS is start Cell row)
   40 CONTINUE
*....Inner Loop: spreading IWIDTH Cells over IBASE pixels
*    -First get a source cell start column

*....skip source columns if necessary
*....(count down until negative)
   45 CONTINUE
      IF (IXE.GE.0) THEN
         IXS=IXS+1
         IXE=IXE-IBASE
         GOTO 45
      ENDIF
*....(IXE now negative, IXS is start Cell)
*....derive cell repetition count
      ICOUNT=0
*....(count up until positive)
   50 CONTINUE
      IF (IXE.LT.0) THEN
         ICOUNT=ICOUNT+1
         IXE=IXE+IWIDTH
         GOTO 50
      ENDIF
*
*
*....we now have IXS pointing at the source cell;
*                       (always in range 1:IWIDTH)
*                 IYS pointing at the source cell row;
*                       (always in range 1:IDEPTH)
*                 IXD pointing at the destination start pixel;
*                       (always in range 1:IBASE)
*                 ICOUNT  holding the repetition count of pixels;
*
*....so copy cell to pixel(s):

      IX = IXS + ISX - 1
      IF (INVX) IX = IWIDTH + ISX - IXS
      IY = IYS + ISY - 1
      IF (INVY) IY = IDEPTH + ISY - IYS

      DO 55 IC=1,ICOUNT
         IF (SWAPXY) THEN
            KSTACK(IAB+IXD-1)=ICA((IX-1)*ISCAN+IY)
         ELSE
            KSTACK(IAB+IXD-1)=ICA((IY-1)*ISCAN+IX)
         ENDIF
         IXD=IXD+1
         IF (IXD.GT.IBASE) GOTO 60
   55 CONTINUE



*....repeat until row complete
      IF (IXD.LE.IBASE) GOTO 40

   60 CONTINUE

*Now see if we need to replicate this row:
*(count up until positive)
   65 CONTINUE
      IF (IYE.LT.0) THEN
         IYD=IYD+1
         IYE=IYE+IDEPTH
* send the Pixel row to the Device's Raster Output routine, at new IYD
*  a check is made that the y-coordinate of the row is within the
*  clipping rectangle. Similarly only that part of the row within
*  the clipping rectangle is output
        RY = ROY - FLOAT (IYD) + 1.0
        IF (RY .GE. YMIN .AND. RY .LE. YMAX)
     :    CALL ROSUB(SRX,RY,NRX,1,NRX,KSTACK(IAB+ISKIPS))
         GOTO 65
      ENDIF

* repeat until all rows  complete
      IF (IYD.LT.ISIDE) GOTO 30

  160 CONTINUE

* Deallocate workspace
      CALL GKSTDA(KINTGS,IAB)
      GOTO 100

***********************************************************************



********************
*                  *
*     CASE   3     *
*                  *
********************

*** General Case
   70 CONTINUE
      ISCAN = KWI1

* As a result of segment/insert transformations, the cell axes are no
* longer parallel to device axes: the cellarray has a rotation component

* The DC transformed positions of the four corners of the CellArray
* are in the arrays DCX and DCY, in the order Z,P,R and Q.


* get enough workspace to hold a raster row:
      CALL GKSTAL(KINTGS,IRX-ILX+1,IAB)
      IF (KERROR.NE.0) GOTO 100

* compute inverse transformation constants
      V=BASEDX*SIDEDY-BASEDY*SIDEDX
      F1 = KWI5 * SIDEDY / V
      F2 = -KWI5 * SIDEDX / V
      F3 = -KWI6 * BASEDY / V
      F4 = KWI6 * BASEDX / V


* for each scanline ...
      DO 80  IY=ITY,IBY,-1

* initialise left and right limits
         I=IRX
         J=ILX

* for each pixel ...
         DO 75  IX=ILX,IRX

* back-transform to cell-space
            ICX=INT((FLOAT(IX)-DCX(2))*F1+(DCY(2)-FLOAT(IY))*F2+1.0)
            ICY=INT((FLOAT(IX)-DCX(2))*F3+(DCY(2)-FLOAT(IY))*F4+1.0)

* only use pixels that map inside cell array
            IF (((ICX .GT. 0) .AND. (ICX .LE. KWI5))
     :      .AND. ((ICY .GT. 0) .AND. (ICY .LE. KWI6)))
     :      THEN

* copy colour index
               KSTACK(IAB+IX-ILX) = ICA((KWI4+ICY-2)*ISCAN+(KWI3+ICX-1))

* update left and right limits
               IF (IX.LT.I) I=IX
               IF (IX.GT.J) J=IX
            ENDIF
   75    CONTINUE

* scanline complete: output relevant portion
         IF (J.GT.I)
     :     CALL ROSUB(FLOAT(I),FLOAT(IY),
     :              J-I+1,1,J-I+1,KSTACK(IAB+I-ILX))

   80 CONTINUE


* Deallocate workspace
      CALL GKSTDA(KINTGS,IAB)


*   end of CASE 3  *
*                  *
********************

  100 CONTINUE

      END
      SUBROUTINE GK0IFL(NRD,RX,RY)
C====================================================C
C=                                                  =C
C  THIS ROUTINE SUPPLIES POLYGON WITH PATTERNED      C
C  FILLING. (REPLACES GKFILS).                       C
C====================================================C
*
      INCLUDE '../../include/check.inc'
*
*
*  ARGUMENTS
*  ---------
*     INP NRD    Number of vertices
*     INP RX,RY  Vertex coordinates
*
      INTEGER NRD
      REAL    RX(NRD),RY(NRD)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     RETLOY Raster Coordinate Y-range covered by Area (Lowest)
*     RETHIY Raster Coordinate Y-range covered by Area (Highest)
*     ILOY   Scanline Pixel Y-range covered by Area
*     IHIY   Scanline Pixel Y-range covered by Area
*     IETI   Edge Table Control Variables
*     IETMAX Edge Table Control Variables
*     IETXB  Stack bases of Edge Table arrays
*     IETYB  Stack bases of Edge Table arrays
*     IETDXB Stack bases of Edge Table arrays
*     IETEYB Stack bases of Edge Table arrays
*     IETNXB Stack bases of Edge Table arrays
*     ITXB   Stack bases of transformed vertex coords
*     ITYB   Stack bases of transformed vertex coords
*     ITEMP  Temporary integer to save data into
*     XMIN   Clipping rectangle in Raster Coords
*     XMAX   Clipping rectangle in Raster Coords
*     YMIN   Clipping rectangle in Raster Coords
*     YMAX   Clipping rectangle in Raster Coords
*
      INTEGER ILOY, IHIY, IETI,
     :        IETMAX, IETXB, IETYB, IETDXB, IETEYB, IETNXB, ITXB, ITYB,
     :        N, IY, ITEMP
      REAL RETLOY, RETHIY, XMIN,XMAX,YMIN,YMAX
*
*  EXTERNALS
*  ---------
*
      EXTERNAL GKTILT,GKXLIN,GKPMAP,  GK0IFA,GK0IRO

*
*  HEAP USAGE
*  ----------
*     Pattern details
*
*  STACK USAGE
*  -----------
*     -for transformation to Raster Coordinates:
*       REAL workspace of size NRD starting at ITXB
*       REAL workspace of size NRD starting at ITYB
*
*     -otherwise, for scan-conversion Edge Table:
*       REAL workspace of size NRD+1 starting at IETXB
*       REAL workspace of size NRD+1 starting at IETYB
*       REAL workspace of size NRD+1 starting at IETDXB
*       REAL workspace of size NRD+1 starting at IETEYB
*       INT  workspace of size NRD+1 starting at IETNXB
*
*
*  ALGORITHM
*  ---------
*
*     The fill-area is output in BLACK SOLID to mask out
*     anything behind, which GK0IRO could let show through.
*
*     The polygon vertices are first transformed into DC
*     and then into Raster Coordinates using the stack.
*     The clipping rectangle is converted to Raster Coordinates.
*
*     GKMET constructs the Edge Table
*
*     GKSCAN delivers slices to GKPMAP
*     to produce the patterned fill with GK0IRO.
*     NB: GK0IRO takes Raster Coordinates.
*
*     At the end of the day all work-space acquisitions are
*     released, whether used or not
*
*  ERRORS
*  ------
*     300  Not enough stack available
*
*---------------------------------------------------------------------



* Extract total clip rectangle using W/S ID from Comms Area
* and convert it to Raster Coordinates.
      XMIN=(KDSRX(KWKIX)/QDSDX(KWKIX))*QWCLXL(KWKIX)
      XMAX=(KDSRX(KWKIX)/QDSDX(KWKIX))*QWCLXR(KWKIX)
      YMIN=(KDSRY(KWKIX)/QDSDY(KWKIX))*QWCLYB(KWKIX)
      YMAX=(KDSRY(KWKIX)/QDSDY(KWKIX))*QWCLYT(KWKIX)

* pre-set stack pointers for tidy collapse
      ITXB   = KNIL
      ITYB   = KNIL
      IETXB  = KNIL
      IETYB  = KNIL
      IETDXB = KNIL
      IETEYB = KNIL
      IETNXB = KNIL


* Transform vertices to Raster Coordinates
      CALL GKSTAL(KREALS,NRD,ITXB)
      IF (KERROR.NE.0) GOTO 101
      CALL GKSTAL(KREALS,NRD,ITYB)
      IF (KERROR.NE.0) GOTO 100

      CALL GKTWD (NRD,RX,RY,QSTACK(ITXB),QSTACK(ITYB))
      CALL GKTDR (NRD,QSTACK(ITXB),QSTACK(ITYB),
     :                QSTACK(ITXB),QSTACK(ITYB))


      IF(KWFAIS(KWKIX) .EQ. GPATTR)THEN
* fillstyle pattern - don't want to waste time if this routine is
*                     called for an incorrect fill-style.
* acquire space for Edge Table
         CALL GKSTAL(KREALS,NRD+1,IETXB)
         IF (KERROR.NE.0) GOTO 96
         CALL GKSTAL(KREALS,NRD+1,IETYB)
         IF (KERROR.NE.0) GOTO 95
         CALL GKSTAL(KREALS,NRD+1,IETDXB)
         IF (KERROR.NE.0) GOTO 94
         CALL GKSTAL(KREALS,NRD+1,IETEYB)
         IF (KERROR.NE.0) GOTO 93
         CALL GKSTAL(KINTGS,NRD+1,IETNXB)
         IF (KERROR.NE.0) GOTO 92

         CALL GKMET(NRD,QSTACK(ITXB),QSTACK(ITYB),RETLOY,RETHIY,
     :                    IETMAX,QSTACK(IETXB),QSTACK(IETYB),
     :                    QSTACK(IETDXB),QSTACK(IETEYB),KSTACK(IETNXB))

         ILOY=NINT(RETLOY)
         IHIY=NINT(RETHIY)
         IETI=0
         IY=IHIY

*        Draw black solid area to mask out anything behind
         ITEMP = KWFAIS(KWKIX)
         KWFAIS(KWKIX) = GSOLID
         CALL GSCOL(8)
         CALL GKFILH(NRD,RX,RY,GK0IFA)
         KWFAIS(KWKIX) = ITEMP
         IF(KERROR .NE. 0) GOTO 92

*  Loop over scanlines ------------------
   40    CONTINUE

* call SCAN with instructions to call PMAP for each output span
         CALL GKSCAN(IY+1,IY,IETI,IETMAX,QSTACK(IETXB),
     :                    QSTACK(IETYB),QSTACK(IETDXB),QSTACK(IETEYB),
     :                    KSTACK(IETNXB),XMIN,XMAX,YMIN,YMAX,
     :                    GKPMAP,GK0IRO)
* (uses derived Pattern details out of Pattern COMMON Data)

         IY=IY-1
         IF (IY.GE.ILOY)  GOTO 40
*  End of scanline loop --------------------

      ENDIF

* Release Workspace
   92 CONTINUE
      CALL GKSTDA(KINTGS,IETNXB)
   93 CONTINUE
      CALL GKSTDA(KREALS,IETEYB)
   94 CONTINUE
      CALL GKSTDA(KREALS,IETDXB)
   95 CONTINUE
      CALL GKSTDA(KREALS,IETYB)
   96 CONTINUE
      CALL GKSTDA(KREALS,IETXB)
  100 CONTINUE
      CALL GKSTDA(KREALS,ITYB)
  101 CONTINUE
      CALL GKSTDA(KREALS,ITXB)

      END
C# IL>=a, OL>=0
      SUBROUTINE GK0IFA(NRD,RX,RY)
*
*     Author:  KEVP   (taken from GK0IFL)
*
C====================================================C
C=                                                  =C
C  DEVICE FILL AREA OUTPUT ROUTINE for               C
C  FILL-STYLES HOLLOW, SOLID and HATCH               C
C====================================================C
*
      INCLUDE '../../include/check.inc'
*
*
*  ARGUMENTS
*  ---------
*     INP NRD    Number of vertices
*     INP RX,RY  Vertex coordinates in pseudo DC
*
      INTEGER NRD
      REAL    RX(NRD),RY(NRD)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     none
*
*---------------------------------------------------------------------

*     Set Fill-style
      IF (KWFAIS(KWKIX) .EQ. GHOLLO) THEN
         CALL GSPAT(15)
         CALL GSAREA(1)
      ELSEIF (KWFAIS(KWKIX) .EQ. GSOLID) THEN
         CALL GSPAT(16)
         CALL GSAREA(0)
      ELSEIF (KWFAIS(KWKIX) .EQ. GHATCH) THEN
         CALL GSPAT(200-KWFASI(KWKIX))
         CALL GSAREA(0)
      ELSE
         GOTO 999
      ENDIF

*     Call device polyline output routine and close
      CALL GK0ILN (NRD,RX,RY)
      CALL GSENDA
*     ...non-degenerate polygons available


  999 CONTINUE
      END
C# IL>=a, OL>=0
      SUBROUTINE GK0ICR(IOPT,NRD,RX,RY,IFILSC,LSIMUL,SIMLEN)
C====================================================C
C=                                                  =C
C  THIS ROUTINE GENERATES AN ARC/CIRCLE FOR GDP.     C
C  (FROM GKCRCS).                                    C
C====================================================C
*
      INCLUDE '../../include/check.inc'
*
*  ARGUMENTS
*  ---------
*     INP   IOPT    Curve option
*                     -1: arc unstyled
*                     -2: arc (chord) with interior styled
*                     -3: arc (pie) with interior styled
*                     -4: circle with interior styled
*     INP   RX }    Co-ordinates of three points on arc.
*     INP   RY }
*     INP   IFILSC  FILL AREA scale factor
*     INP   LSIMUL
*     INP   SIMLEN
*
      INTEGER IOPT,NRD, IFILSC
      REAL     RX(3), RY(3), SIMLEN
      LOGICAL  LSIMUL
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*   Values of IOPT
*     JARC   = -1 Plain Arc
*     JCHORD = -2 Chord  (fillable)
*     JPIE   = -3 Pie    (fillable)
*     JCIRC  = -4 Circle (fillable)
*
*   Values of ISTAT
*     JENDS  = 0  Arc with end points
*     JFULL  = 1  Full Circle
*     JLIN   = 2  Line
*
*   Offset in KWKDAT
*     IGHTCH = 10 GDDM hatching indicator
*
      INTEGER    JARC,    JCHORD,    JPIE,    JCIRC
      PARAMETER (JARC=-1, JCHORD=-2, JPIE=-3, JCIRC=-4)

      INTEGER    JENDS,   JFULL,   JLIN
      PARAMETER (JENDS=0, JFULL=1, JLIN=2)

      INTEGER    IGHTCH
      PARAMETER (IGHTCH=10)

*     AXFM       normalisation transform
*     BXFM       inverse AXFM (backtransform to WC)
*     DXA,DYA    vector from First point of arc to Centre
*     IOFFST     Stack pointer of complete arc
*     IOFFSX                   of curve's X-coords
*     IOFFSY                   of curve's Y-coords
*     ISTAT      status of arc 0=with ends, 1=full, 2=line
*     NMAX       maximum number of points in curve
*     NPTS       number of points in arc curve
*     NPTSA      number of points in arc curve plus any extra
*     RADIUS     radius in WC
*     THETA      arc's turning angle (negative if clockwise)
*     TWOPI      Two pi
*     TQCOPY     copy of w/s total transform (QWTOTT)
*     XCEN,YCEN  centre of arc in WC
*     XPTS,YPTS  copy holding corrected points (back tranformed)
*     DUMMY      used in call to GKCRCN
*
      REAL TWOPI
      PARAMETER (TWOPI = 6.283185072)
      REAL DXA,DYA
      REAL XCEN,YCEN,RADIUS,THETA
      INTEGER I,ISTAT,IOFFST,IOFFSX,IOFFSY,NPTS,NPTSA
      REAL    AXFM(3,2),BXFM(3,2),TQCOPY(6)
      REAL     XPTS(3), YPTS(3)
      LOGICAL DUMMY
*
      INTEGER    NMAX
      PARAMETER (NMAX=120)

*  EXTERNALS
*  ---------
      EXTERNAL GK0IFA, GK0ILN, GK0IRO

*  ALGORITHM
*  ---------
*     1: Obtain details of transform. Change world coords to
*        take account of the registration points.
*
*     2: Calculate the centre and radius in WC
*
*     3: Calculate the number of lines needed
*         -assume number of lines = Square root maximum radius (DC)
*          gives tidy arc.
*
*     4: Generate polygon in stack
*
*     5: Draw outline & fill/pattern inside as appropriate
*
*---------------------------------------------------------------------

*     Check that number of points is correct for Curve Option.
      IF (.NOT.((NRD.EQ.3 .AND. IOPT.NE.JCIRC) .OR.
     :          (NRD.EQ.2 .AND. IOPT.EQ.JCIRC))
     :   )THEN
         KERROR = 100
         GOTO 9999
      ENDIF
*
*     Save original total segment transformation (ie WC to DC)
      DO 10 I=1,6
         TQCOPY(I) = QWTOTT(I,KWKIX)
   10 CONTINUE

*     Change world coords to take account of registration points.
*     (The original WC are restored at the end of the routine.)
      CALL GKMTDV(QWRA(1),QWRA(4),AXFM)
      CALL GKMTIV(AXFM,BXFM)
      CALL GKMTXF(BXFM,NRD,RX,RY,XPTS,YPTS)
      CALL GKMTML(AXFM,QWTOTT(1,KWKIX),QWTOTT(1,KWKIX))


*     -- handle circle parameters --
      IF (IOPT.EQ.JCIRC) THEN
         XPTS(3) = XPTS(1)
         YPTS(3) = YPTS(1)
         DXA    = XPTS(2) - XPTS(1)
         DYA    = YPTS(2) - YPTS(1)
         XCEN   = XPTS(2)
         YCEN   = YPTS(2)
         RADIUS = SQRT(DXA*DXA + DYA*DYA)
         THETA  = TWOPI
         ISTAT  = JFULL
      ELSE
         CALL GKCRCE (XPTS,YPTS,XCEN,YCEN,RADIUS,THETA,ISTAT)
      ENDIF
*
*     Calculate the number of points required to define arc
      IF(ISTAT .EQ. JLIN)THEN
         NPTS = 2
      ELSE
         CALL GKCRCN (RADIUS,THETA,NMAX,NPTS,DUMMY)
      ENDIF
*     Add extra pt for pie if required
      IF((ISTAT .EQ. JENDS) .AND. (IOPT .EQ. JPIE))THEN
        NPTSA = NPTS + 1
      ELSE
        NPTSA = NPTS
      ENDIF
*
*     Allocate Stack for these points
      CALL GKSTAL (KREALS,2*NPTSA,IOFFST)
      IF(KERROR .NE. 0)GOTO 9999
      IOFFSX = IOFFST
      IOFFSY = IOFFSX + NPTSA

*     Put centre into input array
      XPTS(2) = XCEN
      YPTS(2) = YCEN
*     Define the points on the curve
      CALL GKCRCV (XPTS,YPTS,THETA,NPTS,
     :             QSTACK(IOFFSX),QSTACK(IOFFSY))
*     Note: Routine GKCRCN ensures that NPTS is at least 2

*     Add in cente if pie and bounded arc
      IF(NPTSA .EQ. NPTS+1)THEN
          QSTACK(IOFFSX+NPTS) = XCEN
          QSTACK(IOFFSY+NPTS) = YCEN
      ENDIF

*     -- now output arc/circle

      IF (IOPT.EQ.JARC .OR. NPTS.EQ.2) THEN
         CALL GKMTXF(QWTOTT(1,KWKIX),NPTS,
     :                      QSTACK(IOFFSX),QSTACK(IOFFSY),
     :                      QSTACK(IOFFSX),QSTACK(IOFFSY))
         CALL GKLCLP(NPTS,QSTACK(IOFFSX),QSTACK(IOFFSY),
     :                  LSIMUL,SIMLEN,
     :                  QWCLXL(KWKIX),QWCLYB(KWKIX),
     :                  QWCLXR(KWKIX),QWCLYT(KWKIX),
     :                  GK0ILN)
      ELSE
        IF(KWFAIS(KWKIX) .EQ. GPATTR)THEN
*       Patterned fill, fill in with scan-lines of pattern
          CALL GK0IFL(NPTSA,QSTACK(IOFFSX),QSTACK(IOFFSY))
        ELSEIF(KWFAIS(KWKIX).EQ.GHATCH .AND.
     :         KWKDAT(IGHTCH,KWKIX).EQ.0)THEN
*       Hatched fill, if no GDDM hatching is available
               CALL GKFILC(NRD,RX,RY,2,GK0ILN)
        ELSE
*       Use GDDM fill.
          CALL GKFILH(NPTSA,QSTACK(IOFFSX),QSTACK(IOFFSY),GK0IFA)
        ENDIF
      ENDIF


      CALL GKSTDA(KREALS,IOFFST)
      DO 20 I=1,6
         QWTOTT(I,KWKIX) = TQCOPY(I)
   20 CONTINUE

 9999 CONTINUE
      END
