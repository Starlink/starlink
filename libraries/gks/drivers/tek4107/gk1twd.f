C# IL>=a, OL>=0
      SUBROUTINE GK1TWD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)

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
*
*  MAINTENANCE LOG
*  ---------------
*     13/01/84  AS    Original version stabilized (T4010)
*     21/01/85  GGT   Support for hardware characters (T4010)
*     25/01/85  GGT   Converted for Tek 4107
*     08/02/85  GGT   Support hardware for segments
*     06/11/86  GGT   Make hardware segment support optional
*     27/05/87  PLP   Changed to use GKFILS and GKCRCS rather than GKFILL
*                     GKCIRC.  Re-ordered routines in file.
*     12/08/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.  Removed
*                     local common block GK1T.INC, and added CHECK.INC.
*                     Changed to use new parameters to GKIOOP/GKIOCL.
*                     Changes for IS conversion to GDP and GDP inquiries,
*                     set PL/PM/FA attributes, request choice and error
*                     numbers.  Change to Inquire GDP for IS binding.
*     13/08/87  RMK   Changed colour table set in GK1TID to be same as
*                     the one used for Sigmex 5684.
*     19/08/87  RMK   Removed machine-specific code from GK1TCS.
*     16/11/87  RMK   Corrected error checking in GDP entry (S275).
*     18/11/87  RMK   Changed request choice entry to expect 6
*                     integers in GKRQIP call.
*     31/07/89  RMK   Added entry for inquire set member of segment
*                     names on workstation (S362).
*     23/08/89  RMK   Changed pattern index check in set fill area
*                     attributes to allow for user-defined patterns as
*                     well as predefined ones (S79).
*     25/10/89  RMK   In GK1TRO, use MOD to ensure that colour indices
*                     are valid (S350).
*     16/07/92  DLT   Add starlink escape.
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
      INCLUDE '../../include/gks.par'
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
      EXTERNAL GK1TLN, GK1TXF, GK1TXC, GK1TRO
*
*  LOCALS
*  ------
*     HARDSG TRUE if hardware segments are to be used
*     IBAUD  Offset in KWKDAT of baud rate for this terminal
*     ICOL   Offset in KWKDAT for current device colour index (not actually
*            used in this routine, but is used elsewhere in driver)
*     ICHAHI Offset in QWKDAT for hardware character height
*     ICHAWI Offset in QWKDAT for hardware character width
*     ICHROT Offset in QWKDAT for hardware character rotation
*     ICHUNK Number of points in each chunk of output primitive
*     ICTLZ  ASCII code for ctrl/Z
*     INTXFP No. of font/precision pairs
*     IOFF   Stack offset for output primitive chunks
*   IOFFX,IOFFY Offsets for stack holding coordinates of initial stroke
*     IOCISW Function code for input from terminal
*     ITXF   Hardware fonts
*     ITXP   Hardware precisions
*     IVAL   Integer form of valuator if input that way
*     NOUT   Number of bytes returned on input
*     I      Various
*     N      Various
*     NLEFT  Returned by output buffering routine (amount of buffer left
*     ICODE  Key hit returned with cursor position
*     NIPTS2 Number of points in initial stroke after checking
*     NSEE   Number of character we have room for both in echo area and
*            input buffer
*     RED,GREEN,BLUE RGB values in range 0.0 to 100.0
*     ICHOIC Choice returned
*     INTA   Local integer array with multiple uses
*              - receiving input device state (size 10 max)
*              - receiving WDT info for Inq Text Facil (size 19)
*     IPREC  Stores available text precisions
*     PI     PI radians to degrees
*     REALA  Local real array with multiple uses
*              - receiving input device state (size 7 max)
*              - receiving WDT info for Inq Text Facil (size 12)
*   XDC,YDC  2-element arrays used to hold single locator value in DC
*            or part of stroke in DC (previous and current point)
*     LSIMUL Used to indicate whether linetype simulation is required.
*     VALSTR String of characters representing valuator input
*     CHOSTR String of single character representing choice input
*     PROMPT The prompt preceding input implemented by keyboard
*     CDUMMY A dummy character variable
*     IFILSC Fill area scale factor
*
      INTEGER    IBAUD,   ICHAHI,   ICHAWI,   ICHROT
      PARAMETER (IBAUD=1, ICHAHI=1, ICHAWI=2, ICHROT=3)
      INTEGER    ICHUNK, ICTLZ, INTXFP, RENEW(4),DELSEG(4)
      PARAMETER (ICHUNK=200, ICTLZ=26, INTXFP=2)
      INTEGER IOFF, IOFFX,IOFFY, IOCISW,IVAL,NOUT, I, N, NLEFT,
     :    ICODE, NIPTS2, NSEE, ICHOIC
      INTEGER INTA(19),IPREC(KFNTMX),ITXP(INTXFP),ITXF(INTXFP)
      INTEGER RED,GREEN,BLUE
      INTEGER IFILSC
      PARAMETER (IFILSC=1)
      REAL REALA(12), XDC(2),YDC(2),PI
      PARAMETER (PI=3.14159)
      LOGICAL LSIMUL, HARDSG
      PARAMETER (HARDSG=.FALSE.)
      CHARACTER VALSTR*12,CHOSTR*1, PROMPT*50, CDUMMY*1
*     text
      INTEGER ICHX
      REAL RCHHX,RCHHY,RCHWX,RCHWY,RCHRX,RCHRY
*  Starlink no screen clear escape
      INTEGER INOCLR
      SAVE INOCLR
      DATA INOCLR/GNO/

      DATA ITXF /1,1/,
     :     ITXP /GSTRP,GCHARP/
      DATA RENEW /27,75,78,48/
      DATA DELSEG /27,83,75,33/
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
     :      1111,1410,1420,1111,1440,1111,1460,1111,1111,1111,
     :      1111,1510,1111,9999,9999,9999,9999,9999,9999,9999,
     :      9999,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1111,1710,1111,1111,1111,1111,1111,1111,1111,1790,
     :      1111,1111,1111,1111,1840,1850,1111,1870,1880,1111,
     :      1900,1910,1111,1111,1111,1111,1960,1970,1111) IENT-119

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
* Get terminal speed
          CALL GKGTSP(KWCID(KWKIX),KWKDAT(IBAUD,KWKIX))
* Initialise output buffering
          CALL GKIOBO(KIOIT,1,KDAT,NLEFT)
* Initialise device
          CALL GK1TID
* Erase screen
          IF (INOCLR.EQ.GNO) CALL GK1TCL
        ENDIF
      ENDIF
      KWI1 = GOUTIN
      GOTO 9999



* Close workstation
   20 CONTINUE
      IF( KWI1.EQ.1 ) THEN
        KWDONE=KRFUSE
      ELSE
*
        CALL GK1TIE
        CALL GKIOBO(KIOSN,1,KDAT,NLEFT)
        CALL GKIOCL(KFWKT,KCID(KWKIX),KWCID(KWKIX))
        CALL GKCWSL(KWKIX)
      IF (.NOT.HARDSG) CALL GKSLDL(KSSGPT(KWKIX))
      ENDIF
      GOTO 9999



* Clear workstation
   30 CONTINUE
      IF( KWI1.EQ.2 ) THEN
        KWDONE=KRFUSE
      IF (HARDSG) THEN
         CALL GKIOBO(KIOPB,4,DELSEG,NLEFT)
      ELSE
         CALL GKSLDL(KSSGPT(KWKIX))
      ENDIF
      ENDIF
      GOTO 9999



* Redraw all segments on workstation
   40 CONTINUE
      IF (HARDSG) THEN
         KWDONE = KACEPT
         CALL GKIOBO(KIOPB,4,RENEW,NLEFT)
      ELSE
         KWDONE=KRFUSE
      ENDIF
      GOTO 9999



* Update workstation
   50 CONTINUE
      IF (HARDSG) THEN
         CALL GKIOBO(KIOSN,1,KDAT,NLEFT)
         KWI1 = KNFAUP(KWKIX)
         KWDONE = KACEPT
      ELSE
         KWDONE=KRFUSE
      ENDIF
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
      IF (KWI1.EQ.GALWAY.OR.KDSMT(KWKIX).EQ.GNEMPT) CALL GK1TCL
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

      KERROR = 180
      GOTO 9999



* Polyline
  120 CONTINUE
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN
*     select pen from polyline colour index
      CALL GK1TSP(KWPLCI(KWKIX))
* Select linetype
      CALL GK1TSL(KWLNTY(KWKIX),LSIMUL)
        N = ICHUNK
        DO 122 I=1,NRD,ICHUNK-1
          IF (NRD-I.LT.ICHUNK-1) N = NRD-I + 1
          IF (N.EQ.1) GOTO 122
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
        CALL GKLCLP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),LSIMUL,25.0,
     :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK1TLN)
  122   CONTINUE
        CALL GKSTDA(KREALS,IOFF)
      ENDIF
      GOTO 8888



* Polymarker
  130 CONTINUE
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN
*     select pen from polyline colour index
        CALL GK1TSP(KWPMCI(KWKIX))
* Select linetype 1 - solid
        CALL GK1TSL(1,LSIMUL)
        N = ICHUNK
        DO 132 I=1,NRD,ICHUNK
          IF (NRD-I.LT.ICHUNK) N = NRD-I + 1
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
          CALL GKMTYP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),
     :                   KWMKTY(KWKIX),QNMMKS(KWKIX),QWMKSZ(KWKIX),
     :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK1TLN)
  132   CONTINUE
        CALL GKSTDA(KREALS,IOFF)
      ENDIF
      GOTO 8888



* Text
 140  CONTINUE
*     select pen from text colour index
      CALL GK1TSP(KWTXCI(KWKIX))
*     solid lines
      CALL GK1TSL(1,LSIMUL)
*  -- stroke precision
      IF(KWTXPR(KWKIX).EQ.GSTRKP) THEN
        CALL GKXDWO(NID,IDAT,GK1TLN)
*  -- string or char precision
      ELSE
        CALL GKXDWC(NID,IDAT,
     :                 QWCHRX(KWKIX),QWCHRY(KWKIX),
     :                 GK1TXF,GK1TXC)
      ENDIF
      GOTO 8888

* Fill area
  150 CONTINUE
*     select pen from text colour index
      CALL GK1TSP(KWFACI(KWKIX))
*     solid lines
      CALL GK1TSL(1,LSIMUL)
      IF(KWFAIS(KWKIX).EQ.GSOLID) THEN
      CALL GK1TFA(NRD,RX,RY,KWFACI(KWKIX),GK1TLN)
      ELSE
      CALL GKFILS(NRD,RX,RY,IFILSC,GK1TLN,GK1TRO)
      ENDIF
      GOTO 8888

* Cell array
  160 CONTINUE
*     solid lines
      CALL GK1TSL(1,LSIMUL)
      CALL GKCELL(NID,IDAT,GK1TRO)
      GOTO 8888



* GDP
 170  CONTINUE
* First, check GDP identifier
      IF (KWI1.EQ.0) THEN
        KERROR = 102
        GOTO 9999
      ELSE IF (KWI1.LT.-4 .OR. KWI1.GT.-1) THEN
        KERROR = 104
        GOTO 9999
      ELSE IF (KWI1.EQ.-1) THEN
* Arc
*     -- Set line colour and style
        CALL GK1TSP(KWPLCI(KWKIX))
        CALL GK1TSL(KWLNTY(KWKIX),LSIMUL)
        CALL GKCRCS(1,NRD,RX,RY,IFILSC,LSIMUL,10.0,GK1TLN,GK1TRO)
      ELSE
* Filled chord, pie, circle
*     -- Set line colour and style
        CALL GK1TSP(KWFACI(KWKIX))
        CALL GK1TSL(KWLNTY(KWKIX),LSIMUL)
        CALL GKCRCS(KWI1,NRD,RX,RY,IFILSC,.FALSE.,10.0,GK1TLN,GK1TRO)
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
 200  CONTINUE
*     note GKDTXB sets KWTXFI from KWTXFN
      CALL GKDTXB
      IF(KWTXCI(KWKIX).GE.KPCI(KWKIX)) KWTXCI(KWKIX) = 1
*     done if stroke precision
      IF(KWTXPR(KWKIX).EQ.GSTRKP) GOTO 9999
*     default string,char hardware font
      ICHX=1
      KWTXFN(KWKIX)=1
*     character height
      RCHHX=QWCHHX(KWKIX)
      RCHHY=QWCHHY(KWKIX)
      QWKDAT(ICHAHI,KWKIX)=SQRT(RCHHX*RCHHX+RCHHY*RCHHY)
*     character width
      RCHWX=QWCHWX(KWKIX)
      RCHWY=QWCHWY(KWKIX)
      QWKDAT(ICHAWI,KWKIX)=SQRT(RCHWX*RCHWX+RCHWY*RCHWY)
*     character rotation
      RCHRX=QWCHWX(KWKIX)
      RCHRY=QWCHWY(KWKIX)
      QWKDAT(ICHROT,KWKIX)=
     : FLOAT(MOD(NINT(180.0*ATAN2(RCHRY,RCHRX)/PI)+360,360))
*     realised values - used by GK1TXC,GK1TXF in device units
      KWTXFI(KWKIX)=ICHX
      KWCHHT(KWKIX)=INT(QWKDAT(ICHAHI,KWKIX))
      KWCHWD(KWKIX)=INT(QWKDAT(ICHAWI,KWKIX))
      QWCHRX(KWKIX)=RCHRX
      QWCHRY(KWKIX)=RCHRY
      CALL GK1TSS
      GOTO 9999

* Set fill area attributes
  210 CONTINUE
      CALL GKDFAB
* Need to check because individual settings won't have been checked.
      IF (KWFAIS(KWKIX).EQ.GHATCH) THEN
        IF (KWFASI(KWKIX).GT.-1 .OR. KWFASI(KWKIX).LT.-10)
     :  KWFASI(KWKIX) = -1
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
      CALL GKSRPL(1,INTA,.TRUE.)
      GOTO 9999



* Set polymarker representation
  240 CONTINUE
      CALL GKSRPM(0,INTA,.TRUE.)
      GOTO 9999



* Set text representation
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
* See if colour index valid
      IF (KWI1.GE.0 .AND. KWI1.LT.KPCI(KWKIX)) THEN
        QHP(KHPXR(KCTBPT(1,KWKIX))+KWI1) = QWR1
        QHP(KHPXR(KCTBPT(2,KWKIX))+KWI1) = QWR2
        QHP(KHPXR(KCTBPT(3,KWKIX))+KWI1) = QWR3
      RED   = IFIX(QWR1*100.0)
      GREEN = IFIX(QWR2*100.0)
      BLUE  = IFIX(QWR3*100.0)
      CALL GK1TPC(KWI1,RED,GREEN,BLUE)
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
      IF (HARDSG) THEN
         CALL GK1TSH(IENT)
      ELSE
         CALL GKSGWK(IENT,.FALSE.)
      ENDIF
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

*   Get locator device information
      CALL GKILC(RX,RY, INTA,REALA, XDC,YDC)
      IF( KERROR.NE.0 ) GOTO 9999

*   Start of loop
  692 CONTINUE

*       Read key hit and cursor
          CALL GK1TCS(ICODE, XDC,YDC)

*       ctrl/Z is break character (this is rather Vax/VMS dependent)
          IF( ICODE.EQ.ICTLZ ) THEN

*           Break
              KWI1=GNONE
              GOTO 694
          ELSE

*           Valid character ... transform coordinates
              CALL GKTDN(1,XDC,YDC, RX,RY)

*           If point is not within wkstn window, try again
              IF( KERROR.NE.0 ) THEN
                KERROR=0
              ELSE
                KWI1=GOK
                GOTO 694
              ENDIF
          ENDIF

*   Here, only if (not a break) and (point outside wkstn wndow).
*   In this case, repeat.
      GOTO 692

  694 GOTO 9999



* Request stroke
  700 CONTINUE
*   First, get stroke device information
      CALL GKISK(NRD,KNRR,RX,RY, INTA,REALA, NIPTS2,
     :      IOFFX,IOFFY)
      IF( KERROR.NE.0 ) GOTO 9999

*   Draw initial stroke if there are any points and echo is set
      IF( NIPTS2.GE.2 .AND. INTA(KIPE).EQ.GECHO )
     :      CALL GK1TLN(NIPTS2,QSTACK(IOFFX),QSTACK(IOFFY))

*   Save last point. After that we have no further need of the stack.
      IF( NIPTS2.GE.1 ) THEN
        XDC(1)=QSTACK(IOFFX+NIPTS2-1)
        YDC(1)=QSTACK(IOFFY+NIPTS2-1)
      ENDIF
      CALL GKSTDA(KREALS,IOFFX)

*   Cycle once for each point in stroke
      DO 705 I=NIPTS2+1,NRD

*      Further loop to repeat input of point if outside wkstn window
  702   CONTINUE

*         Read key hit and cursor.
            CALL GK1TCS(ICODE, XDC(2),YDC(2))

*         Branch on key hit (ICODE).
*         If break (ctrl/Z), then we quit with status=none
*         If normal terminator (ICODE=13), then finish stroke, not
*         accepting the point.
*         Otherwise inspect the point returned and
*            if OK go and get next one
*            if not OK try again (without stepping on)
            IF(      ICODE.EQ.ICTLZ) THEN
              KWI1=GNONE
              GOTO 709
            ELSE IF( ICODE.EQ.13 ) THEN
              GOTO 706
            ELSE
              CALL GKTDN(1,XDC(2),YDC(2),RX(I),RY(I))
              IF( KERROR.NE.0 ) THEN
                  KERROR=0
                  GOTO 702
              ENDIF
              IF( I.GT.1 .AND. INTA(KIPE).EQ.GECHO )
     :              CALL GK1TLN(2,XDC,YDC)
              XDC(1)=XDC(2)
              YDC(1)=YDC(2)
            ENDIF
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
      GOTO 9999



* Request valuator
  710 CONTINUE
*   First get valuator device information
      CALL GKRQIP(GVALUA,KWI1,4,7,INTA,REALA)

*   Display prompt in echo area and find out how much room there is
*   for input
      WRITE(PROMPT, '(A)')
     :  'Real value :'
      CALL GK1TPD(INTA,REALA,PROMPT,NSEE)

*   Set echoing according to echo switch and input the string
      IF( INTA(KIPE).EQ.GECHO .AND. NSEE.GT.LEN(VALSTR) ) THEN
          IOCISW=KIOEN
      ELSE
          IOCISW=KIONN
      ENDIF
      CALL GKIOCI(IOCISW, 0,CDUMMY, VALSTR,NOUT)

*   Interpret the incoming string. If not recognisable then set
*   status = none.
      IF( NOUT.LT.1 ) THEN
          KWI1=GNONE
      ELSE

*       If valid integer, then convert to most appropriate real
          READ(VALSTR(1:NOUT), '(I12)',   ERR=712) IVAL
          QWR1=FLOAT(IVAL)
          GOTO 718

*       Else, treat as  real
  712     READ(VALSTR(1:NOUT), '(G12.4)', ERR=713) QWR1
          GOTO 718

*       No good
  713     KWI1=GNONE
          GOTO 719

*       Here QWR1 contains a valid real number. Is it in range?
  718     IF( REALA(KVLMNV).LE.QWR1 .AND. QWR1.LE.REALA(KVLMXV) ) THEN
              KWI1=GOK
          ELSE
              KWI1=GNONE
          ENDIF

*       Drop to 719
      ENDIF
  719 GOTO 9999



* Request choice
  720 CONTINUE
*   Get choice device information
      CALL GKRQIP(GCHOIC,KWI1,6,4,INTA,REALA)

*   Display prompt in echo area and find out how much room there is
*   for input
      CALL GK1TPD(INTA,REALA, 'Choice 1 to 9 :', NSEE)

*   Input the string
      IF( INTA(KIPE).EQ.GECHO .AND. NSEE.GT.1 )  THEN
          IOCISW=KIOEP
      ELSE
          IOCISW=KIONP
      ENDIF
      CALL GKIOCI(IOCISW, 0,CDUMMY, CHOSTR,NOUT)

*   Interpret the results: if not recognisable choice then status=none
      IF( NOUT.LT.1 ) THEN
        KWI1 = GNONE
        KWI2 = KNIL
      ELSE
        ICHOIC=ICHAR(CHOSTR)-ICHAR('0')
        IF( 1.LE.ICHOIC .AND. ICHOIC.LE.9 ) THEN
          KWI1=GOK
          KWI2=ICHOIC
        ELSE IF (ICHOIC.EQ.0) THEN
          KWI1 = GNCHOI
          KWI2 = KNIL
        ELSE
          KWI1 = GNONE
          KWI2 = KNIL
        ENDIF
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
        QWR1 = 1.0
      ENDIF
      GOTO 9999



* Inquire text representation
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

      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
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
     :                  RX,RY,GK1TXF)

      ENDIF
      GOTO 9999



* Inquire list of pattern indices
 1410 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999



* Inquire pattern representation
 1420 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999



* Inquire colour representation
 1440 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999



* Inquire set member of segment names on workstation
 1460 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KSGRQ = KSGRQ + 1
      GOTO 9999



* Inquire pick device state
 1510 CONTINUE
      GOTO 9999



* Inquire workstation classification
 1710 CONTINUE
      KWI1 = GVECTR
      GOTO 9999


* Inquire text facilities
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

      IF( KWI1.GT.KFNTMX+2 ) THEN
        KERROR = 2002
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



* Inquire predefined pattern representation
 1840 CONTINUE
      GOTO 1111



* Inquire colour facilities
 1850 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KWI1=16
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

      KWI2 = KNIL
      IF (KWI1.GE.1 .AND. KWI1.LE.4) KWI2 = -KWI1
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

*------------------------------------------------------------
* Inquire number of segment priorities
*------------------------------------------------------------
1900  CONTINUE
      IF (HARDSG) THEN
         KWI1=32767
         GOTO 9999
      ELSE
         GOTO 1111
      ENDIF

*------------------------------------------------------------
* Inquire dynamic modification of segment attributes
*------------------------------------------------------------
1910  CONTINUE
      IF (HARDSG) THEN
         KWI1=GIMM
         KWI2=GIMM
         KWI3=GIMM
         KWI4=GIMM
         KWI5=GIMM
         KWI6=GIMM
         KWI7=GIMM
         GOTO 9999
      ELSE
         GOTO 1111
      ENDIF

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
