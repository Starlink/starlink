C# IL>=a, OL>=0
      SUBROUTINE GK0SWD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)

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
*  Workstation driver for Sigma 5000 series:
*     100 = 5600             107 = 5688-T8 with joystick
*     101 = 5664             108 = 5688-T8 with tablet
*     102 = 5674-T1 (A)      109 = 5470-T1
*     103 = 5674-T1 (B)      110 = 5484-T4 with joystick
*     104 = 5674-T4 (A)      111 = 5484-T4 with tablet
*     105 = 5674-T4 (B)      112 = 5663
*     106 = 5684-T4
*
*  MAINTENANCE LOG
*  ---------------
*     13/01/84  AS    Stabilized
*     19/01/84  JRG   Remove REDRAW ONE SEGMENT, correct CLEAR & CLOSE WK
*     17/02/84  JRG   Incorporated RSK changes, but in a form which allows
*                     both single-plane mono and 4-plane colour to work:
*                     (changes are: drive blue gun from blue value not green,
*                     sort out INQ COL FACIL, correct workstation types)
*     20/02/84  RSK   Attempt to fix premature exit from pixel plotting
*                     mode in GK0SRO primitive. Added GDP code for Arc
*                     Circle.
*     23/02/84  JGW   Enquiries 187-188 and 190 provided.
*     29/02/84  RSK   Still trying to fix bug in GK0SRO primtive
*     01/03/84  RSK   Added code to supply list of Font, Precision
*                     pairs in  Set Text Representation entrypoint.
*                     Also added a GH command to be output to the
*                     Sigma on Open WorkStation, to allow a return to
*                     NGS from  Alpha State if Data is lost.
*       7/3/84  JRG   Make this workstation not specify that it plays back
*                     (it doesn't handle it properly yet anyway).
*      23/3/84  JRG   (a) Distinguish between A & B series Sigmas for
*                     line types (fixing bug I170), (b) remove
*                     unused locals, (c) document locals.
*     27/03/84  JRG   Deal with zero length polylines (fix bug I173 Sigma)
*     29/03/84  RSK   Made GDP's using label 172 goto 8888 instead of 9999
*                     This allows them to be output as soon as possible if
*                     so required. Also they use the Fill Area colour Index
*                     not the Polyline Colour Index.
*                     Added call to GKQWK for Inquire No Of Available Logical
*                     Input Devices and Inquire xxx Device Data.
*     01/04/84  RSK   Merged version 56f ( ICL ) with version f57 (SERC) to
*                     keep varies bug fixes.
*     03/04/84  RSK   Added code to handle Inquire Text Extent
*     08/04/84  RSK   Change rs232 characteristics of Sigma and Perq on Open
*                     Workstation. Changed range of BUFFER read, in Request
*                     Loacator, from 3-9 to 2-8.
*     13/04/84  RSK   Raise pen before moving to initial Locator position.
*     24/04/84  RSK   Changed Dash-Dot data value from C4C4 to FE10, and
*                     Dash-Dot-Dot data value from CACA to F888.
*                     Also changed tests for CR in Request Locator/Stroke.
*     26/04/84 RSK    Increment KRQSG by 1 after explicit call to gkqwk (I192)
*     17/05/84  JRG   In pixel plot (GK0SRO), correct use of NLEFT (was
*                     used before being defined;
*                     in GDP, need error number for unrecognised GDP
*                     cursor needs to disappear after breaking from a stroke
*                     (bug S58);
*                     inq list els of GDP needs error 902 (bug S59);
*                     identifier (104 for output, 41 for inquiry) (bug S60);
*                     inq seg pri should return 1;
*                     various inquiries can simply go straight to 1111 and
*                     shorten the code;
*                     in linetype, keep Perq change but tidy up the comment;
*                     in Inquire Text Extent, CHAR and STRING precision are
*                     valid, they are currently the same as STROKE;
*                     for locator, separate Perq and Vax versions completely;
*                     change array size of IFONT and IPREC to local.
*     21/05/84  JRG   Filler characters before using cursor (part bug S68)
*     26/06/84  JRG   Incorporate linetype for GDP and use of error
*                     102 (from RSK)
*     05/07/84  JRG   Change order of subroutines so that calls in file are
*                     always forwards (Prime needs it; as far as I  know this
*                     does not cause problems elsewhere) (part bug S69)
*                     Use names GLOCAT and GSTROK instead of numbers.
*     20/07/84  JRG   Add choice and valuator (part of bug S69)
*     27/07/84  JRG   Add sync's after a reset (fix part bug S68).
*                     Fix font list (bug S65).
*     03/08/84  JRG   Further correct font list (ensure that fonts file
*                     is open).
*     08/08/84  RMK   In Open Workstation, enable XON/XOFF (fix part bug S69)
*     09/08/84  RMK   Added Inquire default choice device data (entry 196)
*                     Flush typeahead buffer in Request choice.
*     07/12/84  JRG   Bookkeeping ... put in refs to bug fixes S65,68&69.
*     05/02/85  RMK   Changed GK0SLN to allow for high resolution models.
*                     Made request locator use GK0SLN for addressing.
*     13/02/85  RMK   Made changes to handling of colours to allow addition
*                     of remaining models.
*     26/02/85  RMK   Preceeded X format descriptors by a repeat count in
*                     GK0SRL and GK0SRK (S109).
*                     In GK0SPD, split GKIOCO call into 2 separate calls(S111).
*                     Made GK0SRO use GK0SLN for addressing.
*     18/03/85  RMK   Changes to choose correct dash mask for new models.
*                     Set fill area attributes now ensures that a valid
*                     pattern is selected.
*     03/06/85  RMK   Removed call to GKGTSP, and output of sync characters
*                     by GKSYN.
*     06/06/85  RMK   In GK0SRK, made sure that no more than 20 points are
*                     input - temporary fix - routine should be rewritten
*                     in same way as Tek driver. For PRIME, added 128 to
*                     numbers used with CHAR in GK0SRL and GK0SRK.
*     03/12/85  RMK   Check segment visible at start of primitive entries(I231)
*                     Changed GDP entry to goto 9999 rather then 8888 if
*                     segment invisible.
*                     In Request Valuator, corrected formats used by internal
*                     reads (I236).
*                     In GK0SLN, filter out multiple identical points, but
*                     retaining dots.
*                     Fixed Inquire Dynamic Modification of Wkstn Attributes
*                     (S160). Corrected Request Stroke to allow 100 rather
*                     than 20 points (S128).
*                     Specify full crosshair cursor on Open Workstation.
*                     Added code for message entry.
*     06/12/85  RMK   Altered GK0SRL to use locator information utility
*                     and to use byte input.
*     01/07/86  RMK   Changed to use new interface to GKIOOP/GKIOCL and new
*                     utilities GKCRCS/GKFILS. Changed use of ICHAR/CHAR
*                     to GKNA1/GKAN1 (S103).
*     06/10/86  RMK   Fixed bug in decoding of coordinates in GK0SRL (S208).
*     21/10/86  RMK   Changed all input code to send the prompt with
*                     GKIOBI/CI. Created GK0SGI (replacing GK0SPD) which gets
*                     the input as well as outputting the prompt.
*     24/10/86  RMK   Changed GK0SRK to use GKISK for stroke info.
*     16/01/87  RMK   Simplified request valuator entry by using a single
*                     internal READ with Fnout.0 format (S225).
*                     In request choice entry, used char variable rather
*                     than constant in GKNA1 call (S224).
*     19/01/87  RMK   IS conversion. Changed GDP inquiry entries to use
*                     negative GDP identifiers.
*     21/01/87  RMK   IS conversion. Changes to set colour repres entry
*                     to use new formula for greyscale devices, and to
*                     use new error number.
*     05/06/87  RMK   Removed GARC, GCIRCL from Inquire GDP (S227).
*     23/06/87  RMK   Continuing IS conversion. Changed GDP to use
*                     negative GDP identifiers. Error number changes.
*                     Changed linetype test in set PL attributes entry.
*                     Changed markertype test in set PM attributes.
*                     Changed set FA attributes to use -ve hatch styles.
*                     Request choice changed to return GNCHOI for
*                     choice 0.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*     06/07/87  RMK   Changed Inquire GDP to use IS Fortran binding names
*                     for GDP attributes.
*     14/07/87  RMK   Added GKHP.PAR to GK0SRK.
*     16/11/87  RMK   Corrected error checking in GDP entry (S275).
*     18/11/87  RMK   Changed request choice entry to expect 6
*                     integers in GKRQIP call.
*     03/02/88  PLP   Request choice now handled by the GKRQCH
*                     utility. Implemented necessary changes to
*                     relevant entries(Request Choice, Inquire
*                     default choice device data), removed obsolete
*                     locals and GKNA1 function. Introduced second
*                     choice device in all WDT files.
*     22/08/89  RMK   Changed pattern index check in set fill area
*                     attributes to allow for user-defined patterns as
*                     well as predefined ones (S79).
*     24/10/89  RMK   In GK0SRO, use MOD to ensure that the colour
*                     indices are valid (S350).
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
      EXTERNAL GK0SLN, GK0SRO, GK0SGI
*
*  EXTERNAL FUNCTION DEFINITIONS
*  -----------------------------
*
*
*  LOCALS
*  ------
*     IPREC       Precision array for Text;
*                 ***** Note the local precision array will
*                 ***** disappear when there is a decent utility to
*                 ***** handle font names and precisions.
*     Offsets in KWKDAT:
*     IHIRES      High resolution flag
*     ICOLOR      Indicates whether monochrome or colour model:
*                 either GMONOC or GCOLOR
*     ITABLE      Indicates whether or not the model has a hardware
*                 colour table
*     IDASH       Indicates length of dash mask, either 12 or 16
*     IJOY        Indicates model has a joystick (rather than a tablet)
*
*     ICHUNK      Size of stack used for splitting polylines
*     BUFFER      Small local CHARACTER constructing device orders
*     IINDEX      Table used for converting a number to hex (minus 1)
*     PROMPT      The prompt preceding input implemented by keyboard
*     VALSTR      String of characters representing valuator input
*     LNTYPA      Table of linetypes for 'A' series terminals (12-bit cycle)
*     LNTYPB       -----------"--------- 'B'   ------"------  (16-bit cycle)
*     LNT         Used to evaluate linetype order for current terminal
*     IOFF        Offset for stack
*     I           Temporary integer
*     N           Count (temporary)
*     IR,IG,IB    R,G,B intensities in range 1 to 16 (integer)
*     NLEFT       Reply from GKIOCO/GKIOBO (how many chars/bytes left)
*     NOUT        Number of bytes returned on input
*     INTA        Local integer array with multiple uses:
*                 - receiving input device state (size 10 max)
*                 - setting representations (size 1)
*                 - sending escape characters in linestyle selection (size 1)
*                 - receiving WDT for Inq Text Facil (size 19)
*                 - sending NULs in Open Wkstn (size 1)
*     IBEGB, IENDB  hold ASCII codes sent at start and end of buffer
*     R,G,B       R,G,B in GKS form (range 0.0 to 1.0)
*     REALA       Local real array with multiple uses:
*                   - input device state (size 7 max)
*                   - receiving WDT for Inq Text Facil (size 12)
*     FORMT       Character variable to hold format in Request Valuator
*
      INTEGER IPREC(KFNTMX)
*     Integers used as offsets in KWKDAT array
      INTEGER IHIRES, ICOLOR, ITABLE, IDASH, IJOY
      PARAMETER (IHIRES=2, ICOLOR=3, ITABLE=4, IDASH=5, IJOY=6)
      INTEGER    ICHUNK
      PARAMETER (ICHUNK=200)
      CHARACTER BUFFER*15, IINDEX*16, PROMPT*50,
     :          VALSTR*12
      CHARACTER*4 LNTYPA(2:5),LNTYPB(2:5)
      CHARACTER*7 LNT, FORMT
      INTEGER IOFF, I, N, IR, IG, IB, NLEFT,
     :        NOUT
      INTEGER INTA(19)
      INTEGER IBEGB(5),IENDB(2)
      REAL R, G, B
      REAL REALA(12)
* Initialise list of Fonts

      DATA IINDEX/'0123456789ABCDEF'/
* Linetypes: 1 solid,  2 dashed,  3 dotted,  4 dash-dot,  5 dash-dot-dot
      DATA LNTYPA/'R   ', 'M924', 'MEBA', 'MF24'/
      DATA LNTYPB/'F0F0', '8888', 'FE10', 'F888'/
* To be sent by KIOBB: <CR>+-*/
      DATA IBEGB/13, 43, 45, 42, 47/
* To be sent by KIOEB: <CR><LF>
      DATA IENDB/13, 10/
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
     :      1111,1111,1111,1330,1111,1111,1111,1370,1111,1111,
     :      1111,1111,1111,1111,1440,1111,1460,1470,1480,1111,
     :      1111,1510,1111,9999,9999,9999,9999,9999,9999,9999,
     :      9999,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1111,1111,1111,1730,1111,1111,1111,1111,1111,1790,
     :      1111,1111,1111,1111,1111,1850,1111,1870,1880,1111,
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
*
* Set up flags to be used elsewhere, to differentiate between models.
* Should limit tests on workstation type to this entry.
* But Inquire Colour Facilities must have test on wkstn type to
* determine whether colour or monochrome.
*
* Decide whether device is high resolution, ie whether need extra
* byte for addressing.
          KWKDAT(IHIRES, KWKIX) = 0
          IF (KDSRX(KWKIX).EQ.1536) KWKDAT(IHIRES, KWKIX) = 1
* Set flag to show whether model is monochrome or colour
          KWKDAT(ICOLOR,KWKIX) = GCOLOR
          IF (KWKTYP.EQ.100 .OR. (KWKTYP.GE.102.AND.KWKTYP.LE.105)
     :        .OR. KWKTYP.EQ.109) KWKDAT(ICOLOR,KWKIX) = GMONOC
* Set flag to say whether or not the model has a colour table
          KWKDAT(ITABLE,KWKIX) = 1
          IF (KWKTYP.EQ.100 .OR. KWKTYP.EQ.109) KWKDAT(ITABLE,KWKIX) = 0
* Set flag to say whether or not model has a joystick
          KWKDAT(IJOY,KWKIX) = 1
          IF (KWKTYP.EQ.108.OR.KWKTYP.EQ.111) KWKDAT(IJOY,KWKIX) = 0
* Decide whether device needs 12 or 16 bit dash mask
          KWKDAT(IDASH,KWKIX) = 16
          IF ( (KWKTYP.GE.100.AND.KWKTYP.LE.102) .OR.
     :      KWKTYP.EQ.104 .OR. KWKTYP.EQ.112 )
     :      KWKDAT(IDASH,KWKIX) = 12
*
* Initialise output buffering
          CALL GKIOCO(KIOIT,' ',NLEFT)
          CALL GKIOBO(KIOBB,5,IBEGB,NLEFT)
          CALL GKIOBO(KIOEB,2,IENDB,NLEFT)
* Initialise device including clear screen (which is followed
* some nuls to allow device to settle after reset)
          CALL GKIOCO(KIOPB,'HH',NLEFT)
          INTA(1) = 0
          DO 12 I=1,10
            CALL GKIOBO(KIOPB,1,INTA,NLEFT)
   12     CONTINUE
          CALL GKIOCO(KIOSN,' ',NLEFT)
*
* Alter device defaults
          CALL GKIOCO(KIOPB,'DD000DG017DF019GAAJ',NLEFT)
* For high resolution models, enable 4 digit absolute coords - needed for input
* (default is 3 characters - IP)
          IF (KWKDAT(IHIRES,KWKIX).EQ.1) CALL GKIOCO(KIOPB,'IO',NLEFT)
* Initialise device colour table, if it has one
          IF (KWKDAT(ITABLE,KWKIX).EQ.1) THEN
            DO 14 I=0,KPCI(KWKIX)-1
              R = QHP(KHPXR(KCTBPT(1,KWKIX))+I)
              G = QHP(KHPXR(KCTBPT(2,KWKIX))+I)
              B = QHP(KHPXR(KCTBPT(3,KWKIX))+I)
              IR = NINT(15.0*R) + 1
              IG = NINT(15.0*G) + 1
              IB = NINT(15.0*B) + 1
              WRITE(BUFFER,'(''JG'',4(A1),''0'')')
     :        IINDEX(I+1:I+1), IINDEX(IR:IR), IINDEX(IG:IG),
     :        IINDEX(IB:IB)
              CALL GKIOCO(KIOPB,BUFFER(1:7),NLEFT)
   14       CONTINUE
          ENDIF
        ENDIF
      ENDIF
* For 5660, set normal mode - strictly only necessary if fitted with joystick
      IF (KWKTYP.EQ.101) CALL GKIOCO(KIOPB,'IE',NLEFT)
* For all models except 5600 and 5660s, select full crosshair cursor
      IF (KWKTYP.NE.100.AND.KWKTYP.NE.101.AND.KWKTYP.NE.112)
     : CALL GKIOCO(KIOPB,'CK1',NLEFT)
      CALL GKIOCO(KIOSN,' ',NLEFT)
      KWI1 = GOUTIN
      GOTO 9999



* Close workstation
   20 CONTINUE
      IF (KWI1.EQ.1) THEN
        KWDONE = KRFUSE
      ELSE
        CALL GKIOCO(KIOSN,' ',NLEFT)
        CALL GKIOCL(KFWKT,KCID(KWKIX),KWCID(KWKIX))
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
      IF (KWI1.EQ.GALWAY .OR. KDSMT(KWKIX).EQ.GNEMPT) THEN
        CALL GKIOCO(KIOPB,'DA',NLEFT)
        CALL GKIOCO(KIOSN,' ',NLEFT)
      ENDIF
      CALL GKWCLD
      GOTO 9999



* Message
  100 CONTINUE
      CALL GKIOCO(KIOSN,' ',NLEFT)
      CALL GKIOCO(KIOPB,'AABH',NLEFT)
      IF (NID.GE.1) CALL GKIOBO(KIOPB,NID,IDAT,NLEFT)
      CALL GKIOCO(KIOSN,' ',NLEFT)
      GOTO 9999



* Escape
  110 CONTINUE
      KERROR = 180
      GOTO 9999



* Polyline
  120 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN

* Set up device colour
        WRITE(BUFFER(1:3),'(''HI'',A1)')
     :  IINDEX(KWPLCI(KWKIX)+1:KWPLCI(KWKIX)+1)
        CALL GKIOCO(KIOPB,BUFFER(1:3),NLEFT)

* Set up linetype
        IF (KWLNTY(KWKIX).GT.1) THEN
* Set linetype (other than 1). Depends on 'A' or 'B' series.
* Use I for GKS linetype, LNT for device orders of length J.
* By default, linestyles are continuous over consecutive vectors.
          I = KWLNTY(KWKIX)
          IF ( KWKDAT(IDASH,KWKIX).EQ.12 ) THEN
*           A series - 12 bit dash mask
*           Need to use AGS command (<ESC>M) as some models don't accept
*           the equivalent NGS instruction (EK).
*           So, send  JH<ESC>Mhhh<US>FD
            CALL GKIOCO(KIOPB,'JH',NLEFT)
            INTA(1) = 27
            CALL GKIOBO(KIOPB,1,INTA,NLEFT)
            CALL GKIOCO(KIOPB,LNTYPA(I)(1:4),NLEFT)
            INTA(1) = 31
            CALL GKIOBO(KIOPB,1,INTA,NLEFT)
            CALL GKIOCO(KIOPB,'FD',NLEFT)
          ELSE
*           B series - 16 bit dash mask
            LNT='DL'//LNTYPB(I)(1:4)
            CALL GKIOCO(KIOPB,LNT(1:6)//'FD',NLEFT)
          ENDIF
        ENDIF

        N = ICHUNK
        DO 122 I=1,NRD,ICHUNK-1
          IF (NRD-I.LT.ICHUNK-1) N = NRD-I + 1
          IF (N.EQ.1) GOTO 122
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
          CALL GKLCLP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),.FALSE.,0.0,
     :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK0SLN)
  122   CONTINUE

* Reset linetype if necessary
        IF (KWLNTY(KWKIX).NE.1) CALL GKIOCO(KIOPB,'FGFC',NLEFT)

        CALL GKSTDA(KREALS,IOFF)
      ENDIF
      GOTO 8888



* Polymarker
  130 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
* Set up device colour
      WRITE(BUFFER(1:3),'(''HI'',A1)')
     :IINDEX(KWPMCI(KWKIX)+1:KWPMCI(KWKIX)+1)
      CALL GKIOCO(KIOPB,BUFFER(1:3),NLEFT)

      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN
        N = ICHUNK
        DO 132 I=1,NRD,ICHUNK
          IF (NRD-I.LT.ICHUNK) N = NRD-I + 1
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
          CALL GKMTYP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),
     :                   KWMKTY(KWKIX),QNMMKS(KWKIX),QWMKSZ(KWKIX),
     :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK0SLN)
  132   CONTINUE
        CALL GKSTDA(KREALS,IOFF)
      ENDIF
      GOTO 8888



* Text
  140 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
* Set up device colour
      WRITE(BUFFER(1:3),'(''HI'',A1)')
     :IINDEX(KWTXCI(KWKIX)+1:KWTXCI(KWKIX)+1)
      CALL GKIOCO(KIOPB,BUFFER(1:3),NLEFT)
      CALL GKXDWO(NID,IDAT,GK0SLN)
      GOTO 8888



* Fill area
  150 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
* Set up device colour
      WRITE(BUFFER(1:3),'(''HI'',A1)')
     :IINDEX(KWFACI(KWKIX)+1:KWFACI(KWKIX)+1)
      CALL GKIOCO(KIOPB,BUFFER(1:3),NLEFT)

      CALL GKFILS(NRD,RX,RY,1,GK0SLN,GK0SRO)
      GOTO 8888



* Cell array
  160 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
      CALL GKCELL(NID,IDAT,GK0SRO)
      GOTO 8888



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
        WRITE(BUFFER(1:3),'(''HI'',A1)')
     :  IINDEX(KWPLCI(KWKIX)+1:KWPLCI(KWKIX)+1)
        CALL GKIOCO(KIOPB,BUFFER(1:3),NLEFT)
**** (Ought to use the terminal's own linetype mechanism) ****
        CALL GKCRCS(KWI1,NRD,RX,RY,1,KWLNTY(KWKIX).NE.1,20.0,
     :                 GK0SLN,GK0SRO)
      ELSE
* Filled chord, pie, circle
* Set up device colour
        WRITE(BUFFER(1:3),'(''HI'',A1)')
     :  IINDEX(KWFACI(KWKIX)+1:KWFACI(KWKIX)+1)
        CALL GKIOCO(KIOPB,BUFFER(1:3),NLEFT)
        CALL GKCRCS(KWI1,NRD,RX,RY,1,.FALSE.,20.0,GK0SLN,GK0SRO)
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
* Need to check because individual settings won't have been checked.
      IF (KWFAIS(KWKIX).EQ.GHATCH) THEN
        IF (KWFASI(KWKIX).GT.-1 .OR. KWFASI(KWKIX).LT.-10)
     :    KWFASI(KWKIX) = -1
      ENDIF
      IF (KWFACI(KWKIX).GE.KPCI(KWKIX)) KWFACI(KWKIX) = 1
      IF (KWFAIS(KWKIX).EQ.GPATTR) THEN
*       Check whether pattern repres has been set (either predefined or
*       set by the user) - if not, use pattern index 1
        CALL GKDRGE(KPABPT(KWKIX),KWFASI(KWKIX),3,0,INTA,REALA)
        IF (KERROR.NE.0) THEN
           KWFASI(KWKIX)=1
           KERROR=0
        ENDIF
      ENDIF
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
* See if colour index valid
      IF (KWI1.GE.0 .AND. KWI1.LT.KPCI(KWKIX)) THEN
        IF (KWKDAT(ITABLE,KWKIX).EQ.1) THEN
* Can only redefine if the device has a colour table
          QHP(KHPXR(KCTBPT(1,KWKIX))+KWI1) = QWR1
          QHP(KHPXR(KCTBPT(2,KWKIX))+KWI1) = QWR2
          QHP(KHPXR(KCTBPT(3,KWKIX))+KWI1) = QWR3
          IF (KWKDAT(ICOLOR,KWKIX).EQ.GCOLOR) THEN
            IR = NINT(15.0*QWR1) + 1
            IG = NINT(15.0*QWR2) + 1
            IB = NINT(15.0*QWR3) + 1
          ELSE
            IR = NINT(15.0*(0.30*QWR1+0.59*QWR2+0.11*QWR3)) + 1
            IG = IR
            IB = IR
          ENDIF
          WRITE(BUFFER,'(''JG'',4(A1),''0'')')
     :    IINDEX(KWI1+1:KWI1+1),IINDEX(IR:IR),IINDEX(IG:IG),
     :    IINDEX(IB:IB)
          CALL GKIOCO(KIOPB,BUFFER(1:7),NLEFT)
          CALL GKIOCO(KIOSN,' ',NLEFT)
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
      CALL GK0SRL(NRD,RX,RY)
      GOTO 9999



* Request stroke
  700 CONTINUE
      CALL GK0SRK(NRD,RX,RY)
      GOTO 9999



* Request valuator
  710 CONTINUE

*   Get valuator device information
      CALL GKRQIP(GVALUA,KWI1,4,7,INTA,REALA)

*   Output prompt, and get input from operator
      WRITE(PROMPT, '(A,G12.4,A,G12.4,A)' )
     :   'Value between ',REALA(KVLMNV),' and ',REALA(KVLMXV),' :'
      CALL GK0SGI(INTA,REALA,PROMPT,VALSTR,NOUT)

*   Interpret the incoming string. If not recognisable then set
*   status = none.
      IF( NOUT.LT.1 ) THEN
          KWI1=GNONE
          QWR1=QNIL
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
*     Data expected:
*     KWI1   : Device Number
*
*     Data returned:
*     KWI1   : Status
*     KWI2   : Choice value

*     Call the utility
      CALL GKRQCH(KWI1,KWI2,GK0SGI)

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



* Inquire colour representation
 1440 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      IF (KWI2.EQ.GREALI .AND. KWKDAT(ICOLOR,KWKIX).EQ.GMONOC) THEN
        QWR1 = 0.5 * ( MIN(QWR1,QWR2,QWR3) + MAX(QWR1,QWR2,QWR3) )
        QWR2 = QWR1
        QWR3 = QWR1
      ENDIF
      GOTO 9999


* Inquire set members of segment names on workstation
 1460 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KSGRQ = KSGRQ + 1
      GOTO 9999



* Inquire locator device state
 1470 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999



* Inquire stroke device state
 1480 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999



* Inquire pick device state
 1510 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999



* Inquire dynamic modification of workstation attributes
 1730 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
* If device has a colour table, colour representation changes are immediate
      IF (KWKTYP.NE.100.AND.KWKTYP.NE.109) KWI6 = GIMM
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
      IF (KWKTYP.EQ.100 .OR. (KWKTYP.GE.102.AND.KWKTYP.LE.105)
     :    .OR. KWKTYP.EQ.109) KWI2 = GMONOC
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
*     Data expected:
*     NCD    : maximum space for default choice data record
*     KWI1   : logical input device number
*     KWI2   : list element requested
*
*     Data returned:
*     KWI4   : maximum number of choice alternatives

      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      IF(KWI1.EQ.1)THEN
         KWI4=9
      ELSEIF(KWI1.EQ.2)THEN
         KWI4=95
      ELSE
         KWI4=KNIL
      ENDIF
      GOTO 9999


* Inquire default pick device data
 1970 CONTINUE
      GOTO 9999



*   Here after all output primitives to sort out buffering
 8888 CONTINUE
      KDSMT(KWKIX) = GNEMPT
      IF (KWIO(KWKIX).EQ.GYES) CALL GKIOCO(KIOSN,' ',NLEFT)

 9999 CONTINUE

      END






