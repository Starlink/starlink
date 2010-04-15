C# IL>=a, OL>=0
      SUBROUTINE GK0BWD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
*
* (C) COPYRIGHT ICL & SERC  1985
*
*----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:             DRJF/KF
*
      INCLUDE '../../include/check.inc'
*
*
*  EXTERNALS
*  ---------
*
      EXTERNAL GK0BLN, GK0BRO, GK0BXC, GK0BXF
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Benson B1302,B1332 Plotters
*
*  MAINTENANCE LOG
*  ---------------
*     01/02/86  DRJF  Original version stabilized
*     10/03/86  DRJF  Introduced character width factor. Reduces box
*                     like appearance of characters.
*                     Changed pattern length of simulated linetype
*                     from 300 to 150.
*     01/10/86  RMK   Changed end record set in GK0BID to a single
*                     FF20 order.
*     03/11/86  RMK   Fix to text utility - need to make sure string text
*                     will fit into record, as can't break over boundary.
*                     Also must inquire NLEFT before using it.
*     22/01/87  JCS   IS conversion. Error number changes.
*     05/06/87  RMK   Removed use of GARC in Inquire GDP entry.
*     08/06/87  RMK   Other changes for IS conversion. Changed GDP entry
*                     and inquiries to use -ve GDP identifiers.
*                     Changed linetype test in set PL attributes entry.
*                     Changed markertype test in set PM attributes.
*                     Changed set FA attributes to use -ve hatch styles.
*                     Altered inquire FA facilities to return negative
*                     hatch style.
*     18/06/87  RMK   Changed CHAR in message entry to GKAN1.
*     23/06/87  RMK   Corrected repeat length used in GKCRCS calls.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*     06/07/87  RMK   Changed Inquire GDP to use IS Fortran binding names
*                     for GDP attributes.
*     16/11/87  RMK   Corrected error checking in GDP entry (S275).
*     25/05/90  KEVP  Changed font 2 to -2 in data statement of ITXF and
*                     entry points KSTXA=20 & KSTXR=25 (S309).
*     29/10/90  KEVP  Made it call GKPXAD to return correct values for
*                     inQuire PiXel Array Dimensions (C57).
*     07/11/90  KEVP  Removed code in GK0BID, no longer needed, because of
*                     C18 fix.
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
      INCLUDE '../../include/GKS_PAR'
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
*  EXTERNAL FUNCTION DEFINITIONS
*  -----------------------------
*
      CHARACTER*1 GKAN1
*
*
*  LOCALS
*  ------
*
*     Offsets in KWKDAT:
*
      INTEGER    KFRX,   KFRY,   KFRORX,   KFRORY,   KORDER
      PARAMETER (KFRX=1, KFRY=2, KFRORX=3, KFRORY=4, KORDER=5)
      INTEGER    KMAXX,   KXACFR,   IXPEN,   IYPEN
      PARAMETER (KMAXX=6, KXACFR=7, IXPEN=9, IYPEN=10)
      INTEGER    KCPEN
      PARAMETER (KCPEN=11)

*     Gap between frames
      INTEGER    KGAP
      PARAMETER (KGAP=200)

*     Used to convert device coordinates to raster coordinates
      INTEGER    QBCON
      PARAMETER (QBCON=20000.0)

*     Character size and orientation order
      INTEGER    KCHSZ
      PARAMETER (KCHSZ=2)

*     Xacross orientation
      INTEGER    KXACR
      PARAMETER (KXACR=1)

*     Parameters
*     ----------
*
*     standard chunk for polyline,polymarker
      INTEGER    ICHUNK
      PARAMETER (ICHUNK=200)

*     no of text font and precision pairs
      INTEGER    INTXFP
      PARAMETER (INTXFP=2)

*     pi radians
      REAL       PI
      PARAMETER (PI=3.14159)

*     fill area scale factor
      INTEGER    IFILSC
      PARAMETER (IFILSC=10)

*     positive value
      INTEGER    IPOS
      PARAMETER (IPOS=0)

*     negative value
      INTEGER    INEG
      PARAMETER (INEG=1)

*     right binary shift
      INTEGER    IRSH6
      PARAMETER (IRSH6=64)

*     left binary shift
      INTEGER    ILSH2
      PARAMETER (ILSH2=4)

*
*     Variables
*     ---------

*     dummy pen
      INTEGER IDUMPN

*     red, green, blue intensities (set values)
      REAL REDIN, GRNIN, BLUIN

*     text
      INTEGER ICHH,ICHW,ICHX
      INTEGER IHF(1),IHP(1),ITXF(INTXFP),ITXP(INTXFP)
      REAL RCHHX,RCHHY,RCHWX,RCHWY,RCHRX,RCHRY,RCHA

*     miscellaneous variables
      INTEGER IOFF, IOFSET, I, N, INKWI2, INT
      INTEGER INTA(19)
      REAL REALA(12)

*     no of bytes remaining in output buffer
      INTEGER NLEFT

*     When workstation is closed these variables are used to calculate
*     the distance that needs to be moved to clear the last plot
      INTEGER IFINX,IFINY

*     Vecter order
      INTEGER IHFF02(4)

*     Character Size and Orientation order
      INTEGER IHFF03(12)

*     Sign of Character Size and Orientation Order
      INTEGER ISIGN

*     Hershey fonts
      INTEGER IHFONT(15)

*     Initialise Vector order
      DATA IHFF02/255,2,0,0/

*     Initialise Size and Orientation order
      DATA IHFF03/255,3,0,0,0,0,0,0,0,0,0,0/

*     Hershey font list
      DATA IHFONT/-101,-102,-103,-104,-105,-106,-107,
     :            -108,-109,-110,-111,-112,-113,-114,-115/

*     Hardware fonts
      DATA ITXF /-2,-2/

*     Hardware precisions
      DATA ITXP /GSTRP,GCHARP/

*
*  STACK USAGE
*  -----------
*     2*ICHUNK REAL     polyline, polymarker transformed points
*
*  ERRORS
*  ------
*       34   BENSON workstation is not of category MI
*       64   Linetype invalid
*       70   Marker type invalid
*       76   Text font not supported for specified precision
*       83   Fill area interior style not supported (PATTERN)
*       86   Fill area HATCH style index invalid
*       93   Colour index invalid
*    -1004   No room in bundle table
*
*  COMMENTS
*  --------
*
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
     :      1700,1710,1111,1111,1111,1111,1111,1770,1111,1790,
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
      KDFM(KWKIX) = GASTI
*     ws immediate output - no
      KWIO(KWKIX)=GNO
*     implicit regeneration - suppressed
      KIMRGM(KWKIX) = GSUPPD
*     ask operating system to make a connection
      CALL GKIOOP(KFWKFW,KCID(KWKIX),KWCID(KWKIX))
      IF (KERROR.EQ.0) THEN
*       initialise device
        CALL GK0BID
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
      IF (KWI1.EQ.1) THEN
*     indicate second pass expected
        KWDONE=KRFUSE
*     second pass
      ELSE
*       picture clear
        CONTINUE
*       move pen on past last frame
        IF (KWKDAT(KXACFR,KWKIX).EQ.KXACR) THEN
          IFINX=KWKDAT(KFRORX,KWKIX)-KWKDAT(KFRX,KWKIX)+
     :          KWKDAT(KMAXX,KWKIX)+KGAP
          IFINY=KWKDAT(KFRORY,KWKIX)+KWKDAT(KFRY,KWKIX)+KGAP
        ELSE
          IFINX=KWKDAT(KFRORX,KWKIX)+KWKDAT(KMAXX,KWKIX)+KGAP
          IFINY=KWKDAT(KFRORY,KWKIX)-KGAP
        END IF
*       convert to a relative move
        IFINX=IFINX-KWKDAT(IXPEN,KWKIX)
        IFINY=IFINY-KWKDAT(IYPEN,KWKIX)
*       Find out how many bytes are left in the current record
        CALL GKIOFO(KIOQS,1,KDAT,NLEFT)
*       If there are not enough bytes left in the current
*       to put an order and a piece of data in, end the current
*       record and start a new one, and add a vector header
        IF (NLEFT.LE.8) CALL GKIOFO(KIOER,1,KDAT,NLEFT)
*       put order in current record
        CALL GKIOFO(KIOPB,4,IHFF02,NLEFT)
        CALL GK0BXY(IFINX,IFINY,0,NLEFT)
*       find out how many bytes are left in the current record
        CALL GKIOFO(KIOQS,1,KDAT,NLEFT)
*       if a record has been started end it
        IF (NLEFT.GT.0) THEN
          CALL GKIOFO(KIOER,1,KDAT,NLEFT)
*       Reset order identifier
          KWKDAT(KORDER,KWKIX)=0
        END IF
*       send the buffer
        CALL GKIOFO(KIOSN,1,KDAT,NLEFT)
*       disconnect ws
        CALL GKIOCL(KFWKFW,KCID(KWKIX),KWCID(KWKIX))
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
      END IF
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
*       find out how many bytes are left in the current record
        CALL GKIOFO(KIOQS,1,KDAT,NLEFT)
*       if a record has been started end it
        IF (NLEFT.GT.0) THEN
          CALL GKIOFO(KIOER,1,KDAT,NLEFT)
*         Reset order identifier
          KWKDAT(KORDER,KWKIX)=0
        END IF
*       Send buffer
        CALL GKIOFO(KIOSN,1,KDAT,NLEFT)
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

*     find out how many bytes are left in the current record
      CALL GKIOFO(KIOQS,1,KDAT,NLEFT)
*     if a record has been started end it
      IF (NLEFT.GT.0) THEN
        CALL GKIOFO(KIOER,1,KDAT,NLEFT)
*       Reset order identifier
        KWKDAT(KORDER,KWKIX)=0
      END IF
*     Send buffer
      CALL GKIOFO(KIOSN,1,KDAT,NLEFT)
      KWI1 = KNFAUP(KWKIX)
      GOTO 9999

* --------------------------------------------------------------
* Clear display surface
* --------------------------------------------------------------
 80   CONTINUE
*     Data expected:
*     KWI1 : conditional - Clear Control Flag (COND,ALWAYS)

*     reset current wk window and viewport
      CALL GKWCLD
      GOTO 9999

* --------------------------------------------------------------
* Message
* --------------------------------------------------------------
 100  CONTINUE
*     (Should really have a system utility to output message)

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
*     If the surface is clear we need to advance to a new frame
      IF (KDSMT(KWKIX).EQ.GEMPTY) CALL GK0BNF
      IF (KCVIS.EQ.GINVIS)  GOTO 9999
*     No action if drawing in Background colour
      IF(KWPLCI(KWKIX).EQ.0) GOTO 9999
*     Get enough stack space for a chunk of transformed points
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.NE.0) GOTO 9999
*     Select pen from polyline colour index
      CALL GK0BSP(KWPLCI(KWKIX),KCTBPT(1,KWKIX),
     :            KCTBPT(2,KWKIX),KCTBPT(3,KWKIX))
*     Output in manageable polyline sections
      N = ICHUNK
      DO 122 I=1,NRD,ICHUNK-1
        IF (NRD-I.LT.ICHUNK-1) N = NRD-I + 1
        IF (N.EQ.1) GOTO 122
        CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
        CALL GKLCLP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),.TRUE.,150.0,
     :              QWCLXL(KWKIX),QWCLYB(KWKIX),
     :              QWCLXR(KWKIX),QWCLYT(KWKIX),GK0BLN)
122   CONTINUE
      CALL GKSTDA(KREALS,IOFF)
      GOTO 8888

* --------------------------------------------------------------
* Polymarker
* --------------------------------------------------------------
 130  CONTINUE
*     If the surface is clear we need to advance to a new frame
      IF (KDSMT(KWKIX).EQ.GEMPTY) CALL GK0BNF
      IF (KCVIS.EQ.GINVIS)  GOTO 9999
*     No action if drawing in Background colour
      IF(KWPMCI(KWKIX).EQ.0) GOTO 9999
*     Get enough stack space for a chunk of transformed points
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.NE.0) GOTO 9999
*     Select pen from polymarker colour index
      CALL GK0BSP(KWPMCI(KWKIX),KCTBPT(1,KWKIX),
     :            KCTBPT(2,KWKIX),KCTBPT(3,KWKIX))
*     Output in manageable polymarker sections
      N = ICHUNK
      DO 132 I=1,NRD,ICHUNK
        IF (NRD-I.LT.ICHUNK) N = NRD-I + 1
        CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
        CALL GKMTYP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),
     :              KWMKTY(KWKIX),QNMMKS(KWKIX),QWMKSZ(KWKIX),
     :              QWCLXL(KWKIX),QWCLYB(KWKIX),
     :              QWCLXR(KWKIX),QWCLYT(KWKIX),GK0BLN)
132   CONTINUE
      CALL GKSTDA(KREALS,IOFF)
      GOTO 8888

* --------------------------------------------------------------
* Text
* --------------------------------------------------------------
 140  CONTINUE
*     If the surface is clear we need to advance to a new frame
      IF (KDSMT(KWKIX).EQ.GEMPTY) CALL GK0BNF
      IF(KCVIS.EQ.GINVIS) GOTO 9999
*     No action if drawing in Background colour
      IF(KWTXCI(KWKIX).EQ.0) GOTO 9999
*     Select pen from text colour index
      CALL GK0BSP(KWTXCI(KWKIX),KCTBPT(1,KWKIX),
     :            KCTBPT(2,KWKIX),KCTBPT(3,KWKIX))
*  -- Stroke precision
      IF(KWTXPR(KWKIX).EQ.GSTRKP) THEN
        CALL GKXDWO(NID,IDAT,GK0BLN)
*  -- String or char precision
      ELSE
        CALL GKXDCS(NID,IDAT,QWCHRX(KWKIX),QWCHRY(KWKIX),GK0BXF,GK0BXC)
      ENDIF
      GOTO 8888

* --------------------------------------------------------------
* Fill area
* --------------------------------------------------------------
 150  CONTINUE
*     If the surface is clear we need to advance to a new frame
      IF (KDSMT(KWKIX).EQ.GEMPTY) CALL GK0BNF
      IF (KCVIS.EQ.GINVIS)  GOTO 9999
*     For hollow,hatch no action if Background colour
      IF(KWFACI(KWKIX).EQ.0) GOTO 9999
*     Select pen from fill area colour index
      CALL GK0BSP(KWFACI(KWKIX),KCTBPT(1,KWKIX),
     :            KCTBPT(2,KWKIX),KCTBPT(3,KWKIX))
*     Fill area -  GK0BLN for hollow,hatch
      CALL GKFILS(NRD,RX,RY,IFILSC,GK0BLN,GK0BRO)
      GOTO 8888

* --------------------------------------------------------------
* Cell array
* --------------------------------------------------------------
 160  CONTINUE
*     If the surface is clear we need to advance to a new frame
      IF (KDSMT(KWKIX).EQ.GEMPTY) CALL GK0BNF
      IF (KCVIS.EQ.GINVIS)  GOTO 9999
*     Select pen from default colour index
      CALL GK0BSP(1,KCTBPT(1,KWKIX),
     :            KCTBPT(2,KWKIX),KCTBPT(3,KWKIX))
*     Use utility to perform minimal simulation
      CALL GKCASM(GK0BLN)
      GOTO 8888

* --------------------------------------------------------------
* GDP
* --------------------------------------------------------------
 170  CONTINUE
*     If the surface is clear we need to advance to a new frame
      IF (KDSMT(KWKIX).EQ.GEMPTY) CALL GK0BNF
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
        CALL GK0BSP(KWPLCI(KWKIX),KCTBPT(1,KWKIX),
     :              KCTBPT(2,KWKIX),KCTBPT(3,KWKIX))
        CALL GKCRCS(KWI1,NRD,RX,RY,IFILSC,.FALSE.,150.0,GK0BLN,GK0BRO)
      ELSE
* Filled chord, pie, circle
*     -- Set line colour and width
        IF(KWFACI(KWKIX).EQ.0) GOTO 9999
        CALL GK0BSP(KWFACI(KWKIX),KCTBPT(1,KWKIX),
     :              KCTBPT(2,KWKIX),KCTBPT(3,KWKIX))
        CALL GKCRCS(KWI1,NRD,RX,RY,IFILSC,.FALSE.,150.0,GK0BLN,GK0BRO)
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
      IF(KWLNTY(KWKIX).LT.1 .OR. KWLNTY(KWKIX).GT.5) KWLNTY(KWKIX) = 1
      IF(KWPLCI(KWKIX).GE.KPCI(KWKIX)) KWPLCI(KWKIX) = 1
      GOTO 9999

* --------------------------------------------------------------
* Set polymarker attributes
* --------------------------------------------------------------
 190  CONTINUE
      CALL GKDPMB
*     reset workstation dependent (realised) values
      IF(KWMKTY(KWKIX).LT.1 .OR. KWMKTY(KWKIX).GT.5) KWMKTY(KWKIX) = 3
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
      IF(KWTXFI(KWKIX).EQ.-2) THEN
*       hershey font for char precision
        KWTXFN(KWKIX)=IHFONT(1)
*       hardware font for string,char precision
        ICHX=KWTXFI(KWKIX)-1
      ENDIF
*     character height
      RCHHX=QWCHHX(KWKIX)
      RCHHY=QWCHHY(KWKIX)
      ICHH=NINT(SQRT(RCHHX*RCHHX+RCHHY*RCHHY))
*     character width
      RCHWX=QWCHWX(KWKIX)
      RCHWY=QWCHWY(KWKIX)
*     factor of 0.8 produces a character with a greater height than
*     width. Eliminates box like appearance of characters.
      ICHW=NINT(SQRT(RCHWX*RCHWX+RCHWY*RCHWY)*QWCHXP(KWKIX)*0.8)
*     character rotation
      RCHRX=QWCHWX(KWKIX)
      RCHRY=QWCHWY(KWKIX)
      RCHA=AMOD(ATAN2(RCHRY,RCHRX)+2*PI,2*PI)
*     validate character size
      ICHH=MAX(NINT(QMNCHH(KWKIX)),ICHH)
      ICHW=MAX(NINT(QMNCHH(KWKIX)),ICHW)
      ICHH=MIN(NINT(QMXCHH(KWKIX)),ICHH)
      ICHW=MIN(NINT(QMXCHH(KWKIX)),ICHW)
*     realised values - used by GK0BXC,GK0BXF
      KWTXFI(KWKIX)=ICHX
      KWCHHT(KWKIX)=ICHH
      KWCHWD(KWKIX)=ICHW
      QWCHRX(KWKIX)=RCHRX
      QWCHRY(KWKIX)=RCHRY
*     now prime the device
      CALL GKIOFO(KIOQS,1,KDAT,NLEFT)
      IF (NLEFT.LT.12) CALL GKIOFO(KIOER,1,KDAT,NLEFT)
*
*     Set angle for characters depending on frame orientation
*
      IF (KWKDAT(KXACFR,KWKIX).EQ.KXACR) RCHA=PI/2.00+RCHA
*
*     Plant Character Size and Orientation order
*
      IHFF03(3)=KWKDAT(KCPEN,KWKIX)
*
*     Get enough space for the four parameters of the Character Size
*     and Orientation order
*
      CALL GKSTAL(KINTGS,4,IOFF)
      IF (KERROR.NE.0) GOTO 9999
      KSTACK(IOFF)=NINT((4.0/3.0)*ICHW*COS(RCHA))
      KSTACK(IOFF+1)=NINT((-4.0/3.0)*ICHH*SIN(RCHA))
      KSTACK(IOFF+2)=NINT((4.0/3.0)*ICHW*SIN(RCHA))
      KSTACK(IOFF+3)=NINT((4.0/3.0)*ICHH*COS(RCHA))
*
*     Format the parameters so that the Character Size and Orientation
*     order understands them
*
      IOFSET=0
      DO 202 I=5,11,2
        INT=ABS(KSTACK(IOFF+IOFSET))
        IF (KSTACK(IOFF+IOFSET).LT.0) THEN
*
*       Sign of parameter is negative
*
          ISIGN=INEG
        ELSE
*
*       Sign of parameter is positive
*
          ISIGN=IPOS
        END IF
*
*       Put 8 bits in IHFF03(I) and 8 bits in IHFF03(I+1)
*
        IHFF03(I)=MOD(INT,64)*ILSH2+ISIGN
        IHFF03(I+1)=INT/IRSH6
        IOFSET=IOFSET+1
  202 CONTINUE
      CALL GKIOFO(KIOPB,12,IHFF03,NLEFT)
      CALL GKSTDA(KINTGS,IOFF)
*
*     Set current order to be that stated above
*
      KWKDAT(KORDER,KWKIX)=KCHSZ
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
      IF(KWFAIS(KWKIX).EQ.GHATCH) THEN
         IF(KWFASI(KWKIX).GT.-1 .OR. KWFASI(KWKIX).LT.-10)
     :      KWFASI(KWKIX) = -1
      ENDIF
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
      IF(KWI2.EQ.-2) THEN
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
        KERROR = 76
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
*
*     Multiply the current workstation total transformation matrix by
*     an additional matrix to change device coordinates into raster
*     coordinates
*
      DO 315 I=1,6
        QWTOTT(I,KWKIX)=QWTOTT(I,KWKIX)*QBCON
  315 CONTINUE
*
*     Transform clipping rectangle to raster coordinates
*
      QWCLXL(KWKIX)=QWCLXL(KWKIX)*QBCON
      QWCLYB(KWKIX)=QWCLYB(KWKIX)*QBCON
      QWCLXR(KWKIX)=QWCLXR(KWKIX)*QBCON
      QWCLYT(KWKIX)=QWCLYT(KWKIX)*QBCON
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
 330  CONTINUE
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
* Benson has no input devices
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
* Benson has no input devices
      KERROR = 38
      GOTO 9999


*===============================================================
*                INPUT FUNCTIONS - SAMPLE
*===============================================================

* --------------------------------------------------------------
* Sample locator/stroke/valuator/choice/pick/string
* --------------------------------------------------------------
 750  CONTINUE
* Benson has no input devices
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
     :                  RX,RY,GK0BXF)

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
        REDIN=QWR1
        GRNIN=QWR2
        BLUIN=QWR3
        CALL GK0BPN(REDIN,GRNIN,BLUIN,IDUMPN,QWR1,QWR2,QWR3)
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
* Benson has no input devices
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
* Inquire polymarker facilities
* --------------------------------------------------------------
 1770 CONTINUE
*     Data returned:
*     QWR1 : nominal marker size (DC)
*     QWR2 : minimum marker size (DC)
*     QWR3 : maximum marker size (DC)
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      QWR1=QWR1/QBCON
      QWR2=QWR2/QBCON
      QWR3=QWR3/QBCON
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
        KWI1 = N
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

*     List of interior styles supported: 1=hollow, 2=hatch

      IF (KWI1.LT.1 .OR. KWI1.GT.2) THEN
        KERROR = 2002
      ELSE IF (KWI1.EQ.2 .AND. (KWI2.LT.1.OR.KWI2.GT.10)) THEN
        KERROR = 2002
      ELSE
        KWI3 = 10
        KWI4 = -KWI2
        IF (KWKIX.NE.KNIL) THEN
          KWI5 = KPFAI(KWKIX)
        ELSE
          CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
          IF (KERROR.EQ.0) THEN
            KWI5 = INTA(11)
          ENDIF
        ENDIF
        IF(KWI1.EQ.1) THEN
          KWI2=GHOLLO
        ELSE
          KWI2=GHATCH
        ENDIF
        KWI1 = 2
      ENDIF
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
********
*
*     KWI1 takes the value 4 for the 3 pen BENSON and the value
*     5 for the 4 pen BENSON. The way the value of KWI1 is initiated
*     will have to be sorted out later.
*
********
      KWI1 = 4
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
* Benson has no input devices
      KERROR = 38
      GOTO 9999

* --------------------------------------------------------------
* Inquire default locator/stroke/valuator/choice/pick/string device data
* --------------------------------------------------------------
 1930 CONTINUE
* Benson has no input devices
      KERROR = 38
      GOTO 9999

* --------------------------------------------------------------
* Common end path for output primitives
* --------------------------------------------------------------
 8888 CONTINUE
*     output makes paper not empty
      KDSMT(KWKIX) = GNEMPT
*     flush buffer if necessary
      IF(KWIO(KWKIX).EQ.GYES) THEN
*     find out how many bytes are left in the current reord
        CALL GKIOFO(KIOQS,1,KDAT,NLEFT)
*       if a record has been started end it
        IF (NLEFT.GT.0) THEN
          CALL GKIOFO(KIOER,1,KDAT,NLEFT)
*         Reset order identifier
          KWKDAT(KORDER,KWKIX)=0
        END IF
*       send buffer
        CALL GKIOFO(KIOSN,1,KDAT,NLEFT)
      END IF

* --------------------------------------------------------------
* Return
* --------------------------------------------------------------
 9999 CONTINUE
      RETURN
      END
