*
      SUBROUTINE GK0QWD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:             DLT  Original Versatec driver
*                      AJC  Modifications for Printronix P300
*                      PLP  Modifications for PRIME
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Workstation driver for PRINTRONIX P300 Lineprinter workstation
*
*  MAINTENANCE LOG
*  ---------------
*
*     25/11/85  PTW   Improved polyline drawing and short horizontal
*                     line bug fix.
*     27/11/85  DLT   Fix page boundary problem, eliminate reading of
*                     colour table from WDT.
*     05/12/85  DLT   Fix error in vertical white lines
*     06/12/85  DLT   Return correct workstation catagory
*     15/05/86  DLT   Convert to work in metres
*     07/01/87  PLP   Introduced character array instead of
*                     virtual memory for the bitmap. VAX
*                     system calls removed.
*     15/01/87  PLP   Switched to GKIOOP in Open Workstation
*                     entry and to GKIOFO in bitmap output.
*                     Altered GDP entrypoint to use new utility
*                     GKCRCS.
*     23/01/87  PLP   Using CHARACTER*80 Character Heap
*                     for the bitmap. Removed dummy entrypoints
*                     and replaced them by 9999's in the
*                     computed GOTO.
*     17/02/87  PLP   Following the changes to the Heap Manager
*                     (Integer and Character heap equivalenced)
*                     bitmap information stored on CHARACTER*1
*                     heap using function GK0QLC.
*     26/02/87  PLP   Text entries in WDT tables,  Request Input
*                     entries in GK0QWD modified; border drawing
*                     and initialisation routines added.
*     05/03/87  PLP   PORTrait and LANDscape WS types introduced,
*                     bits of code for frame rotation added to
*                     GK0QLN and GK0QRO.
*     25/03/87  PLP   Reduced nominal marker size. Fixed bug
*                     in Set Colour Representation entry and
*                     corrected maximum style index handling
*                     in Set Fill Area attributes entry.
*     15/04/87  PLP   Number of colours in WDT reduced to 2
*                     (white background and black "ink").
*                     Subsequently changed corresponding
*                     entrypoints and GK0QRO utility
*                     (gray-scale simulation removed).
*     24/06/87  PLP   Concatenated all the transformations
*                     and upgraded the driver to 7.4
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*     06/07/87  PLP   Added error handling in GK0QCL and GK0QWD so
*                     as to cope with errors while writing bitmap
*                     to an external file.
*     16/11/87  RMK   Corrected error checking in GDP entry (S275).
*     06/12/89  PLP   Corrected Inquire list element of available GDPs (S368).
*     08/05/90  PLP   Introduced parameters IDDFRM and IDIRGM in order to
*                     have the driver's default deferral mode and the default
*                     implicit regeneration mode information stored.
*                     Subsequently altered the Open Workstation and Inquire
*                     Default Deferral State Values entries (S283 and S284).
*     09/08/90  KEVP  Copied XSCALE parameter from GK0QID and use it in
*                     WDT inquiries instead of KWKDAT(NXSC,KWKIX), which
*                     is unset if workstation is not open (S304).
*                     In inquire text facilities, delayed DC conversion,
*                     so that it done whether workstation is open or not.
*     22/08/90  DSG   Shortened the Errror 83 comment line (would not
*                     compile).  Still will not compile on IBM CMS - the
*                     Intrinsic Functions OR and AND are not known.
*                     Bug C24 raised.
*     29/10/90  KEVP  Made it call GKPXAD to return correct values for
*                     inQuire PiXel Array Dimensions (C57 & S371).
*     30/10/90  PLP   Introduced GKLOR and GKLAND, new system utilities
*                     which perform logical OR and logical AND operations.
*     07/11/90  KEVP  Removed code, no longer needed, because of C18 fix.
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
      INCLUDE '../../include/gks.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwke.par'
      INCLUDE '../../include/gwksgl.par'
      INCLUDE '../../include/gkinp.par'
      INCLUDE '../../include/gkfls.par'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkcon.cmn'
      INCLUDE '../../include/gkwca.cmn'
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
      EXTERNAL GK0QLN, GK0QRO
*
*     Intrinsic functions declaration
*
      INTRINSIC NINT,AMAX1,AMIN1,IABS
*
*  LOCALS
*  ------
*
*     IPREC       Precision array for Text;
*                 ***** Note the local precision array will
*                 ***** disappear when there is a decent utility to
*                 ***** handle font names and precisions.
*     ICHUNK      Size of stack used for splitting polylines
*     IDDFRM      Default deferral mode.
*     IDIRGM      Default implicit regeneration mode.
*     IOFF        Offset for stack
*     I,J         Temporary integers
*     N           Count (temporary)
*     INTA        Local integer array with multiple uses:
*                  - receiving WDT for Inq Text Facil (size 19)
*     REALA       Local real array with multiple uses:
*                  - receiving WDT for Inq Text Facil (size 12)
*                  - temporary to hold values for the clipping
*                    rectangle in Norm. Trans. entry
*     IERR        Temporary to hold value of error indicator while
*                 cleaning up
*     IFILSC      Fill area scale factor
*

*
*     Parameters
*     ----------
*
*     Offsets in KWKDAT
*
      INTEGER NVADR,NVIS
      PARAMETER (NVADR=1,NVIS=4)
*
      INTEGER NWSTYP,LLAND
      PARAMETER (NWSTYP=6,LLAND=1)
*
*     Offsets in QWKDAT
*
      INTEGER NXSC,NYSC
      PARAMETER (NXSC=1,NYSC=2)

*     default deferral mode and default implicit regeneration mode
      INTEGER IDDFRM, IDIRGM
      PARAMETER (IDDFRM=GASTI, IDIRGM=GSUPPD)
*
*     X scale factor (needed for WDT inquiries, in case W/S is not open)
      REAL XSCALE
      PARAMETER (XSCALE=2362.2039)
*
      INTEGER IPREC(KFNTMX)
      INTEGER    ICHUNK,IFILSC
      PARAMETER (ICHUNK=200,IFILSC=1)
      INTEGER IOFF, I, N
      INTEGER INTA(19)
      REAL REALA(12)
*
*
*  STACK USAGE
*  -----------
*     POLYLINE and POLYMARKER for transformations
*
*  ERRORS
*  ------
*      26   Specified workstation cannot be open
*      32   Specified workstation is not of category MO
*      34   Specified workstation is not of category MI
*      38   Specified workstation has no input devices
*      41   Specified workstation type is not able to generate
*           the specified generalized drawing primitive
*      64   Specified linetype is not supported on this workstation
*      66   Polymarker index is invalid
*      70   Specified marker type is not supported on this workstation
*      76   Requested text font is not supported for the specified precision
*           on this workstation
*      83   Specified fill area interior style is not supported on this
*           workstation
*      86   Specified hatch style is not supported on this workstation
*      93   Colour index is invalid
*     102   Generalized drawing primitive identifier is invalid
*     104   Workstation is not able to generate the specified gdp
*     180   Specified escape function is not supported
*     304   Input/Output error has occured while sending data to
*           a workstation
*    2002   List element or set member not available
*   -1009   Unable to access the database file
*
* --------------------------------------------------------------
* Conditional GOTO on entrypoint code
* --------------------------------------------------------------
*     WS entry : 1 - 119
      GOTO (       10,  20,  30,  40,  50,  60,  70,  80,9999,
     :      9999, 110, 120, 130, 140, 150, 160, 170, 180, 190,
     :       200, 210,9999, 230, 240, 250, 260, 270, 280,9999,
     :      9999, 310, 320, 330,9999,9999,9999,9999,9999,9999,
     :      9999, 410, 410, 410, 410, 410, 410, 410, 410, 410,
     :       410, 410,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999, 610, 610, 610, 610, 610, 610, 670, 680, 610,
     :       690, 690, 690, 690, 690, 750, 750, 750, 750, 750,
     :       750, 810,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999, 910, 920, 930) IENT

*     WS entry : 120 - 206
      GOTO (1200,1111,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1111,1111,1111,1330,1111,1111,1111,1370,1111,1111,
     :      1111,1410,1420,1111,1440,1111,1460,1470,1480,1111,
     :      1111,1510,1111,9999,9999,9999,9999,9999,9999,9999,
     :      9999,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1700,1111,1720,1111,1740,1750,1111,1770,1111,1790,
     :      1111,1111,1111,1111,1111,1850,1111,1870,1880,1111,
     :      1111,1111,1111,1111,1111,1111,1960,9999,1111) IENT-119

      GOTO 9999





*===============================================================
*                 CONTROL FUNCTIONS
*===============================================================

* --------------------------------------------------------------
* Open workstation
* --------------------------------------------------------------
   10 CONTINUE
*     Data expected:
*     KWI1   : connection identifier
*     KWKTYP : workstation type
*
*     Data returned:
*     KWI1   : workstation category (IO)
*     KERROR : error response or zero

*     Set up workstation state list and workstation description table
      CALL GKIWSL(KWKIX,KWKTYP)
      IF (KERROR.NE.0) GOTO 9999
*     connection identifier
      KCID(KWKIX) = KWI1
*     deferral mode - at some time in the future (see above).
      KDFM(KWKIX) = IDDFRM
*     ws immediate output - no
      KWIO(KWKIX) = GNO
*     implicit regeneration - suppressed (see above).
      KIMRGM(KWKIX) = IDIRGM
*     Ask operating system to make a connection
      CALL GKIOOP(KFWKFW,KCID(KWKIX),KWCID(KWKIX))
      IF (KERROR.EQ.0) THEN
*        Initialise device
         CALL GK0QID
      ENDIF
*     no input devices
      KWI1 = GOUTPT
      GOTO 9999


* --------------------------------------------------------------
* Close workstation
* --------------------------------------------------------------
   20 CONTINUE
*     Data expected:
*     KWI1   : first/second pass

*     first pass
      IF (KWI1.EQ.1) THEN
*       indicate second pass expected
        KWDONE = KRFUSE
      ELSE
*       second pass
        IF (KDSMT(KWKIX).EQ.GNEMPT)THEN
           CALL GK0QCL
*          KERROR=304 if there's been an error while dumping the
*          frame to an external file.
           IF(KERROR.NE.0)GOTO 9999
        ENDIF
*       Deallocate heap
        CALL GKHPDA(KWKDAT(NVADR,KWKIX),KCHARS)
*       Close file and disconnect WS
        CALL GKIOCL(KFWKFW,KCID(KWKIX),KWCID(KWKIX))
        CALL GKCWSL(KWKIX)
        CALL GKSLDL(KSSGPT(KWKIX))
      ENDIF
      GOTO 9999


* --------------------------------------------------------------
* Clear workstation(F/E announcement before Clear surface)
* --------------------------------------------------------------
   30 CONTINUE
*     Data expected:
*     KWI1   : first/second pass
*     KWI2   : conditional - Clear Control Flag (ignored)

*     second pass
      IF( KWI1.EQ.2 ) THEN
*        indicate clear surface entry expected
         KWDONE=KRFUSE
*        delete WS segments
         CALL GKSLDL(KSSGPT(KWKIX))
      ENDIF
      GOTO 9999


* --------------------------------------------------------------
* Redraw all segments on workstation
* --------------------------------------------------------------
   40 CONTINUE
      KWDONE = KRFUSE
      GOTO 9999


* --------------------------------------------------------------
* Update workstation(F/E announcement before Close WK)
* --------------------------------------------------------------
   50 CONTINUE
*     Data expected:
*     KWI1   : GPERFO    - Perform update?

      KWDONE = KRFUSE
      GOTO 9999


* --------------------------------------------------------------
* Set deferral state
* --------------------------------------------------------------
   60 CONTINUE
*     Data expected:
*     KWI1   : New deferral mode (ASAP,BNIG,BNIL,ASTI)
*     KWI2   : New implicit regeneration mode (SUPPRESSED,ALLOWED)

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


* --------------------------------------------------------------
* Do deferred output actions
* --------------------------------------------------------------
   70 CONTINUE
*     Data returned:
*     KWI1   : new frame action necessary at update

      KWI1 = KNFAUP(KWKIX)
      GOTO 9999


* --------------------------------------------------------------
* Clear display surface
* --------------------------------------------------------------
   80 CONTINUE
*     Data expected:
*     KWI1   : conditional - Clear Control Flag (COND,ALWAYS)

      IF (KWI1.EQ.GALWAY .OR. KDSMT(KWKIX).EQ.GNEMPT) THEN
         CALL GK0QCL
*        KERROR=304 if there's been an error while dumping the
*        frame to an external file.
         IF(KERROR.NE.0)GOTO 9999
         CALL GKHPDA(KWKDAT(NVADR,KWKIX),KCHARS)
         CALL GK0QVM
      ENDIF
*     reset current wk window and viewport
      CALL GKWCLD
      GOTO 9999


* --------------------------------------------------------------
* Escape
* --------------------------------------------------------------
  110 CONTINUE
*     Data expected:
*     KWI1   : function identifier

      KERROR = 180
      GOTO 9999



*===============================================================
*                OUTPUT FUNCTIONS
*===============================================================

* --------------------------------------------------------------
* Polyline
* --------------------------------------------------------------
  120 CONTINUE
*     If the surface is clear we need to advance to a new frame
      IF(KDSMT(KWKIX).EQ.GEMPTY)CALL GK0QNF
      IF(KCVIS.EQ.GINVIS)GOTO 9999
*     Drawing in background colour?
      IF (KWPLCI(KWKIX).EQ.0) THEN
         KWKDAT(NVIS,KWKIX) = 0
      ELSE
         KWKDAT(NVIS,KWKIX) = 1
      END IF
*     Get enough stack space for a chunk of transformed points
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN
*        Output in managable polyline sections
         N = ICHUNK
         DO 122 I=1,NRD,ICHUNK-1
             IF (NRD-I.LT.ICHUNK-1) N = NRD-I + 1
             IF (N.EQ.1) GOTO 122
             CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
             CALL GKLCLP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),
     :                   (KWLNTY(KWKIX).NE.1),10.0,
     :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK0QLN)
  122    CONTINUE
         CALL GKSTDA(KREALS,IOFF)
      ENDIF
      GOTO 8888


* --------------------------------------------------------------
* Polymarker
* --------------------------------------------------------------
  130 CONTINUE
*     If the surface is clear we need to advance to a new frame
      IF(KDSMT(KWKIX).EQ.GEMPTY)CALL GK0QNF
      IF(KCVIS.EQ.GINVIS)GOTO 9999
*     Drawing in background colour?
      IF (KWPMCI(KWKIX).EQ.0) THEN
         KWKDAT(NVIS,KWKIX) = 0
      ELSE
         KWKDAT(NVIS,KWKIX) = 1
      END IF
*     Get enough stack space for a chunk of transformed points
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN
*        Output in managable polymarker sections
         N = ICHUNK
         DO 132 I=1,NRD,ICHUNK
            IF (NRD-I.LT.ICHUNK) N = NRD-I + 1
            CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
            CALL GKMTYP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),
     :                   KWMKTY(KWKIX),QNMMKS(KWKIX),QWMKSZ(KWKIX),
     :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK0QLN)
  132    CONTINUE
         CALL GKSTDA(KREALS,IOFF)
      ENDIF
      GOTO 8888


* --------------------------------------------------------------
* Text
* --------------------------------------------------------------
  140 CONTINUE
*     If the surface is clear we need to advance to a new frame
      IF(KDSMT(KWKIX).EQ.GEMPTY)CALL GK0QNF
      IF(KCVIS.EQ.GINVIS)GOTO 9999
*     Writing in background colour?
      IF (KWTXCI(KWKIX).EQ.0) THEN
         KWKDAT(NVIS,KWKIX) = 0
      ELSE
         KWKDAT(NVIS,KWKIX) = 1
      END IF
*     Stroke precision
      CALL GKXDWO(NID,IDAT,GK0QLN)
      GOTO 8888


* --------------------------------------------------------------
* Fill area
* --------------------------------------------------------------
  150 CONTINUE
*     If the surface is clear we need to advance to a new frame
      IF(KDSMT(KWKIX).EQ.GEMPTY)CALL GK0QNF
      IF(KCVIS.EQ.GINVIS)GOTO 9999
*     Filling area in background colour?
      IF (KWFACI(KWKIX).EQ.0) THEN
         KWKDAT(NVIS,KWKIX) = 0
      ELSE
         KWKDAT(NVIS,KWKIX) = 1
      END IF
*     Fill area - GK0QLN for hollow, hatch
      CALL GKFILS(NRD,RX,RY,IFILSC,GK0QLN,GK0QRO)
      GOTO 8888


* --------------------------------------------------------------
* Cell array
* --------------------------------------------------------------
  160 CONTINUE
*     If the surface is clear we need to advance to a new frame
      IF(KDSMT(KWKIX).EQ.GEMPTY)CALL GK0QNF
      IF(KCVIS.EQ.GINVIS)GOTO 9999
*     Use utility
      CALL GKCELL(NID,IDAT,GK0QRO)
      GOTO 8888


* --------------------------------------------------------------
* GDP
* --------------------------------------------------------------
  170 CONTINUE
*     If the surface is clear we need to advance to a new frame
      IF(KDSMT(KWKIX).EQ.GEMPTY)CALL GK0QNF
      IF(KCVIS.EQ.GINVIS)GOTO 9999
* First, check GDP identifier
      IF (KWI1.EQ.0) THEN
         KERROR = 102
         GOTO 9999
      ELSE IF (KWI1.LT.-4 .OR. KWI1.GT.-1) THEN
         KERROR = 104
         GOTO 9999
      ELSE IF (KWI1.EQ.-1) THEN
* Arc
         IF (KWPLCI(KWKIX).EQ.0) THEN
            KWKDAT(NVIS,KWKIX) = 0
         ELSE
            KWKDAT(NVIS,KWKIX) = 1
         END IF
         CALL GKCRCS(KWI1,NRD,RX,RY,IFILSC,KWLNTY(KWKIX).NE.1,10.0,
     :                  GK0QLN,GK0QRO)
      ELSE
* Filled chord, pie, circle
         IF (KWFACI(KWKIX).EQ.0) THEN
            KWKDAT(NVIS,KWKIX) = 0
         ELSE
            KWKDAT(NVIS,KWKIX) = 1
         END IF
         CALL GKCRCS(KWI1,NRD,RX,RY,IFILSC,.FALSE.,10.0,GK0QLN,GK0QRO)
      ENDIF
      GOTO 8888



*===============================================================
*                WORKSTATION ATTRIBUTES (REALISED)
*===============================================================

* --------------------------------------------------------------
* Set polyline attributes
* --------------------------------------------------------------
  180 CONTINUE
      CALL GKDPLB
*     Need to check because individual settings won't have been checked.
      IF (KWPLCI(KWKIX).GE.KPCI(KWKIX)) KWPLCI(KWKIX) = 1
      GOTO 9999


* --------------------------------------------------------------
* Set polymarker attributes
* --------------------------------------------------------------
  190 CONTINUE
*     Need to check because individual settings won't have been checked.
      CALL GKDPMB
      IF (KWMKTY(KWKIX).GT.5) KWMKTY(KWKIX) = 3
      IF (KWPMCI(KWKIX).GE.KPCI(KWKIX)) KWPMCI(KWKIX) = 1
      GOTO 9999


* --------------------------------------------------------------
* Set text attributes
* --------------------------------------------------------------
  200 CONTINUE
      CALL GKDTXB
*     Need to check because individual settings won't have been checked.
      IF (KWTXCI(KWKIX).GE.KPCI(KWKIX)) KWTXCI(KWKIX) = 1
      GOTO 9999


* --------------------------------------------------------------
* Set fill area attributes
* --------------------------------------------------------------
  210 CONTINUE
      CALL GKDFAB
*     realise with hatch and current fill area colour
      IF (KWFAIS(KWKIX).EQ.GHATCH .AND. (KWFASI(KWKIX).LT.-10 .OR.
     :KWFASI(KWKIX).GT.-1))KWFASI(KWKIX) = -1
      IF (KWFAIS(KWKIX).EQ.GPATTR .AND. KWFASI(KWKIX).GT.3)
     :KWFASI(KWKIX) = 1
      IF (KWFACI(KWKIX).GE.KPCI(KWKIX)) KWFACI(KWKIX) = 1
      GOTO 9999



*===============================================================
*                WORKSTATION ATTRIBUTES (REPRESENTATIONS)
*===============================================================

* --------------------------------------------------------------
* Set polyline representation
* --------------------------------------------------------------
  230 CONTINUE
*     Data expected:
*     KWI1   : polyline index
*     KWI2   : linetype
*     QWR1   : linewidth scale factor
*     KWI3   : polyline colour index

      INTA(1) = 5
      CALL GKSRPL(1,INTA,.TRUE.)
      GOTO 9999


* --------------------------------------------------------------
* Set polymarker representation
* --------------------------------------------------------------
  240 CONTINUE
*     Data expected:
*     KWI1   : polymarker index
*     KWI2   : markertype
*     QWR1   : marker scale factor
*     KWI3   : polymarker colour index

      CALL GKSRPM(0,INTA,.TRUE.)
      GOTO 9999


* --------------------------------------------------------------
* Set text representation
* --------------------------------------------------------------
  250 CONTINUE
*     Data expected:
*     KWI1   : Text Index
*     KWI2   : Font
*     KWI3   : Precision  ( STRING, CHAR, STROKE )
*     QWR1   : Character Expansion Factor
*     QWR2   : Character Spacing Factor
*     KWI4   : Text Colour Index
*     Data returned:
*     KERROR : error indicator

*     Make sure that fonts are available
        IF( KWI3.EQ.GSTRKP ) THEN
*       Stroke Precision
        IF( KDBFLS.EQ.KFLNA ) THEN
          KERROR=-1009
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


* --------------------------------------------------------------
* Set fill area representation
* --------------------------------------------------------------
  260 CONTINUE
*     Data expected:
*     KWI1   : Fill Area Index
*     KWI2   : Fill Area Interior Style
*     KWI3   : Fill Area Style Index
*     KWI4   : Fill Area Colour Index

      CALL GKSRFA(.FALSE.)
      GOTO 9999


* --------------------------------------------------------------
* Set pattern representation
* --------------------------------------------------------------
  270 CONTINUE
*     Data expected:
*     KWI1   : Pattern Index
*     KWI2   : Pattern X dimension
*     KWI3   : Pattern Y dimension
*     KWI4   : First dimension of colour index array

      CALL GKSRPA(NID,IDAT)
      GOTO 9999


* --------------------------------------------------------------
* Set colour representation
* --------------------------------------------------------------
  280 CONTINUE
*     Data expected:
*     KWI1   : Colour Index
*     QWR1   : RED component
*     QWR2   : GREEN component
*     QWR3   : BLUE component

*     Colour table bundles are stored on the HEAP; bundles can be
*     modified but new ones may not be added; the total no of bundle
*     entries (set by WDT) is found from KPCI(KWKIX).

*     See if colour index valid
      IF (KWI1.GE.0 .AND. KWI1.LT.KPCI(KWKIX)) THEN
        QHP(KHPXR(KCTBPT(1,KWKIX))+KWI1) = QWR1
        QHP(KHPXR(KCTBPT(2,KWKIX))+KWI1) = QWR2
        QHP(KHPXR(KCTBPT(3,KWKIX))+KWI1) = QWR3
*       See if regeneration necessary
        IF (KDSMT(KWKIX).EQ.GNEMPT) THEN
          IF (KIMRGM(KWKIX).EQ.GALLOW) THEN
            KRGN = .TRUE.
            KWRGN(KWKIX) = .TRUE.
          ELSE
            KNFAUP(KWKIX) = GYES
          ENDIF
        ENDIF
      ELSE
        KERROR = 93
      ENDIF
      GOTO 9999



*===============================================================
*                TRANSFORMATIONS
*===============================================================

* --------------------------------------------------------------
* Normalisation transformation
* --------------------------------------------------------------
  310 CONTINUE
      CALL GKWKC4
*
      IF(KWKDAT(NWSTYP,KWKIX).EQ.LLAND)THEN
*
*        LANDscape
*        Multiply total transformation to convert from metres to raster
*        coordinates. The Printronix has different resolution along the
*        axis, hence the different scale factors.
*
         QWTOTT(1,KWKIX) = QWTOTT(1,KWKIX)*QWKDAT(NXSC,KWKIX)
         QWTOTT(2,KWKIX) = QWTOTT(2,KWKIX)*QWKDAT(NXSC,KWKIX)
         QWTOTT(3,KWKIX) = QWTOTT(3,KWKIX)*QWKDAT(NXSC,KWKIX)
         QWTOTT(4,KWKIX) = QWTOTT(4,KWKIX)*QWKDAT(NYSC,KWKIX)
         QWTOTT(5,KWKIX) = QWTOTT(5,KWKIX)*QWKDAT(NYSC,KWKIX)
         QWTOTT(6,KWKIX) = QWTOTT(6,KWKIX)*QWKDAT(NYSC,KWKIX)
*
*        Convert clipping rectangle to raster coordinates
*
         QWCLXL(KWKIX) = QWCLXL(KWKIX)*QWKDAT(NXSC,KWKIX)
         QWCLYB(KWKIX) = QWCLYB(KWKIX)*QWKDAT(NYSC,KWKIX)
         QWCLXR(KWKIX) = QWCLXR(KWKIX)*QWKDAT(NXSC,KWKIX)
         QWCLYT(KWKIX) = QWCLYT(KWKIX)*QWKDAT(NYSC,KWKIX)
      ELSE
*
*        PORTrait
*        We should concatenate transformations to effect the
*        PORTrait orientation. However, we cannot go all the
*        way because for the GKCELL to work properly in the
*        non-square pixel environment "rotation" must be done
*        at the driver's utilities level.
*        So: multiply total transformation to convert from metres to
*        raster coordinates, ie "swapp" the scaling, and translate the
*        origin from lower left to upper left corner. Note that
*        in the event QWTOTT(3,KWKIX) becomes negative.
*
         QWTOTT(1,KWKIX) = QWTOTT(1,KWKIX)*QWKDAT(NYSC,KWKIX)
         QWTOTT(2,KWKIX) = QWTOTT(2,KWKIX)*QWKDAT(NYSC,KWKIX)
         QWTOTT(3,KWKIX) = (QWTOTT(3,KWKIX) - QDSDX(KWKIX))*
     :                      QWKDAT(NYSC,KWKIX)
         QWTOTT(4,KWKIX) = QWTOTT(4,KWKIX)*QWKDAT(NXSC,KWKIX)
         QWTOTT(5,KWKIX) = QWTOTT(5,KWKIX)*QWKDAT(NXSC,KWKIX)
         QWTOTT(6,KWKIX) = QWTOTT(6,KWKIX)*QWKDAT(NXSC,KWKIX)
*
*        Convert clipping rectangle to raster coordinates,
*        keeping the normalisation transformation changes in
*        mind. Note that QWCLXL and QWCLXR become non-positive.
*
         QWCLXL(KWKIX) = (QWCLXL(KWKIX)-QDSDX(KWKIX))*
     :                    QWKDAT(NYSC,KWKIX)
         QWCLYB(KWKIX) = QWCLYB(KWKIX)*QWKDAT(NXSC,KWKIX)
         QWCLXR(KWKIX) = (QWCLXR(KWKIX)-QCWVXR(KWKIX))*
     :                    QWKDAT(NYSC,KWKIX)
         QWCLYT(KWKIX) = QWCLYT(KWKIX)*QWKDAT(NXSC,KWKIX)
      ENDIF
      GOTO 9999


* --------------------------------------------------------------
* Set workstation window
* --------------------------------------------------------------
  320 CONTINUE
*     Data expected:
*     QWR1-4 : workstation window XL, XR, YB, YT
*
*     Data returned:
*     KRGN & KWRGN : .TRUE. if regeneration needed

*     set up requested window
      CALL GKSWKW
      GOTO 9999


* --------------------------------------------------------------
* Set workstation viewport
* --------------------------------------------------------------
  330 CONTINUE
*     Data expected:
*     QWR1-4 : workstation viewport XL, XR, YB, YT
*
*     Data returned:
*     KRGN & KWRGN : .TRUE. if regeneration needed

*     Set up requested viewport
      CALL GKSWKV
      GOTO 9999


* --------------------------------------------------------------
* Segment entrypoints (410 - 510)
* --------------------------------------------------------------
  410 CONTINUE
      CALL GKSGWK(IENT,.FALSE.)
      GOTO 9999



*===============================================================
*                INPUT FUNCTIONS - INITIALISATION
*===============================================================

* --------------------------------------------------------------
* Initialise locator/stroke/valuator/choice/pick/string
* --------------------------------------------------------------
  610 CONTINUE
*     Printronix has no input devices
      KERROR = 38
      GOTO 9999



*===============================================================
*                INPUT FUNCTIONS - SET MODE
*===============================================================

* --------------------------------------------------------------
* Set input operating modes
* --------------------------------------------------------------
  670 CONTINUE
      GOTO 9999


* --------------------------------------------------------------
* Set input mode
* --------------------------------------------------------------
  680 CONTINUE
*     Printronix has no input devices
      KERROR = 38
      GOTO 9999



*===============================================================
*                INPUT FUNCTIONS - REQUEST
*===============================================================

* --------------------------------------------------------------
* Request locator/stroke/valuator/choice/pick/string
* --------------------------------------------------------------
  690 CONTINUE
*     Printronix has no input devices
      KERROR = 38
      GOTO 9999



*===============================================================
*                INPUT FUNCTIONS - SAMPLE
*===============================================================

* --------------------------------------------------------------
* Sample locator/stroke/valuator/choice/pick/string
* --------------------------------------------------------------
  750 CONTINUE
*     Printronix has no input devices
      KERROR = 38
      GOTO 9999



*===============================================================
*                INPUT FUNCTIONS - EVENT
*===============================================================

* --------------------------------------------------------------
* Flush device events
* --------------------------------------------------------------
  810 CONTINUE
      GOTO 9999



*===============================================================
*                METAFILE FUNCTIONS
*===============================================================

* --------------------------------------------------------------
* Write item to GKSM
* --------------------------------------------------------------
  910 CONTINUE
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
  930 CONTINUE
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
* Inquire polyline representation
* --------------------------------------------------------------
 1330 CONTINUE
      IF (KWI2.EQ.GSET) THEN
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
        QWR1 = 1.0
      ENDIF
      GOTO 9999


* --------------------------------------------------------------
* Inquire text representation
* --------------------------------------------------------------
 1370 CONTINUE
*     Data expected:
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

*     Printronix has Stroke precision text only
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999


* --------------------------------------------------------------
* Inquire list element of pattern indices
* --------------------------------------------------------------
 1410 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999


* --------------------------------------------------------------
* Inquire pattern representation
* --------------------------------------------------------------
 1420 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999


* --------------------------------------------------------------
* Inquire colour representation
* --------------------------------------------------------------
 1440 CONTINUE
*     Data expected:
*     KWI1   : colour table index
*     KWI2   : type of returned values ( "GSET" or "GREALI" )
*
*     Data returned:
*     KERROR : error indicator
*     QWR1   : colour (red intensity)
*     QWR2   : colour (green intensity)
*     QWR3   : colour (blue intensity)

      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      IF (KWI2.EQ.GREALI) THEN
         QWR1 = 0.5 * (AMIN1(QWR1,QWR2,QWR3) + AMAX1(QWR1,QWR2,QWR3))
         QWR2 = QWR1
         QWR3 = QWR1
      ENDIF
      GOTO 9999


* ---------------------------------------------------------------------
* Inquire Set Members of Segment Names on Workstation
* ---------------------------------------------------------------------
 1460 CONTINUE
*     Data expected:
*     KWI1   : list element requested
*
*     Data returned:
*     KERROR : error indicator
*     KWI1   : no of segment names
*     KWI2   : Nth member of set of stored segments for this
*              workstation

      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KSGRQ = KSGRQ + 1
      GOTO 9999


* --------------------------------------------------------------
* Inquire locator device state
* --------------------------------------------------------------
 1470 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999


* --------------------------------------------------------------
* Inquire stroke device state
* --------------------------------------------------------------
 1480 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999


* --------------------------------------------------------------
* Inquire pick device state
* --------------------------------------------------------------
 1510 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999



*===============================================================
*             INQUIRY FUNCTIONS - WS DESC TABLE
*===============================================================

* --------------------------------------------------------------
* Inquire workstation catagory
* --------------------------------------------------------------
 1700 CONTINUE
*     Data returned:
*     KWI1   : Workstation Category

      KWI1 = GOUTPT
      GOTO 9999


* --------------------------------------------------------------
* Inquire maximum display surface
* --------------------------------------------------------------
 1720 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KWI1 = GMETRE
      GOTO 9999


* ---------------------------------------------------------------
* Inquire Default Deferral State Values
* ---------------------------------------------------------------
 1740 CONTINUE
*     Data returned:
*     KERROR : error indicator
*     KWI1   : default value for deferral mode
*     KWI2   : default value for implicit regeneration mode

      KWI1 = IDDFRM
      KWI2 = IDIRGM
      GOTO 9999


* --------------------------------------------------------------
* Inquire polyline facilities
* --------------------------------------------------------------
 1750 CONTINUE
*     Data expected:
*     KWI1   : list element requested
*
*     Data returned:
*
*     KERROR : error indicator
*     KWI1   : number of available linetypes
*     KWI2   : n-th element of list of available linetypes
*     KWI3   : number of available linewidths
*     KWI4   : number of predefined polyline indices
*     QWR1   : nominal linewidth (DC)
*     QWR2   : minimum linewidth (DC)
*     QWR3   : maximum linewidth (DC)

      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
*     X-axis scale factor is used in the Raster to Device
*     Coordinates conversion as it is the inverse of what
*     was done in the WDT.
*     Parameter XSCALE rather than workspace KWKDAT(NXSC,KWKIX) is used,
*     since workstation need not be open (S304).
      QWR1 = QWR1/XSCALE
      QWR2 = QWR2/XSCALE
      QWR3 = QWR3/XSCALE
      GOTO 9999


* --------------------------------------------------------------
* Inquire polymarker facilities
* --------------------------------------------------------------
 1770 CONTINUE
*     Data expected:
*     KWI1   : list element requested
*
*     Data returned:
*
*     KERROR : error indicator
*     KWI1   : number of available marker types
*     KWI2   : n-th element of list of available marker types
*     KWI3   : number of available marker sizes
*     KWI4   : number of predefined polymarker indices
*     QWR1   : nominal marker size (DC)
*     QWR2   : minimum marker size (DC)
*     QWR3   : maximum marker size (DC)

      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
*     X-axis scale factor is used in the Raster to Device
*     Coordinates conversion as it is the inverse of what
*     was done in the WDT.
*     Parameter XSCALE rather than workspace KWKDAT(NXSC,KWKIX) is used,
*     since workstation need not be open (S304).
      QWR1 = QWR1/XSCALE
      QWR2 = QWR2/XSCALE
      QWR3 = QWR3/XSCALE
      GOTO 9999


* --------------------------------------------------------------
* Inquire text facilities
* --------------------------------------------------------------
 1790 CONTINUE
*     Data expected:
*     KWI1   : list element requested
*
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

*     Allow for string and char precision font (number 1) explicitly;
*     Make sure that fonts are available;

*     Stroke precision font
      IF( KDBFLS.EQ.KFLNA ) THEN
        KERROR=-1009
        GOTO 9999
      ENDIF
      IF( KDBFLS.EQ.KFLCL ) CALL GKXON
      IF( KERROR.NE.0 ) GOTO 9999
      KWI2 = KHFONT(KWI1)
      KWI3 = 2
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
      END IF
*     X-axis scale factor is used in the Raster to Device
*     Coordinates conversion as it is the inverse of what
*     was done in the WDT.
*     Parameter XSCALE rather than workspace KWKDAT(NXSC,KWKIX) is used,
*     since workstation need not be open (S304).
      QWR1 = QWR1/XSCALE
      QWR2 = QWR2/XSCALE
      GOTO 9999


* --------------------------------------------------------------
* Inquire colour facilities
* --------------------------------------------------------------
 1850 CONTINUE
*     Data returned:
*     KERROR : error indicator
*     KWI1   : number of available colours
*     KWI2   : colour available
*     KWI3   : number of predefined colour indices

      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KWI1 = KWI3
      KWI2 = GMONOC
      GOTO 9999


* ----------------------------------------------------------------
* Inquire List Element of Available Generalised Drawing Primitives
* ----------------------------------------------------------------
 1870 CONTINUE
*     Data expected:
*     KWI1   : list element requested
*
*     Data returned:
*     KERROR : error indicator
*     KWI1   : no of available Generalised Drawing Primitives
*     KWI2   : Nth element of list of available GDP's

      IF (KWI1.GE.1 .AND. KWI1.LE.4) THEN
        KWI2 = -KWI1
      ELSE
        KERROR=2002
        KWI2=KNIL
      ENDIF
      KWI1 = 4
      GOTO 9999


* --------------------------------------------------------------
* Inquire Generalised Drawing Primitive
* --------------------------------------------------------------
 1880 CONTINUE
*     Data expected:
*     KWI1   : GDP identifier
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
         IF (KWI1.EQ.-1)  IDAT(1) = GPLATT
      ELSE
         KERROR = 41
      ENDIF
      GOTO 9999


* --------------------------------------------------------------
* Inquire default choice device data
* --------------------------------------------------------------
 1960 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999


* --------------------------------------------------------------
* Common end path for output primitives
* --------------------------------------------------------------
 8888 CONTINUE
*     Output makes paper not empty
      KDSMT(KWKIX) = GNEMPT


* --------------------------------------------------------------
* Return
* --------------------------------------------------------------
 9999 CONTINUE

      END
