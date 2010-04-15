
      SUBROUTINE GK9SWD(IENT, NID, IDAT, NRD, RX, RY, NCD, STR)

*-----------------------------------------------------------------------
*
*  RAL GKS SYSTEM
*
*  Type of routine:  Workstation Driver
*  Authors:          ACA, PJWR, TAW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Workstation driver for SMI workstations.
*
*  MAINTENANCE LOG
*  ---------------
*     10/03/87  TAW   Created version 0.9.
*     01/04/87  PJWR  Created version 1.0.
*     14/07/87  PJWR  IS: Converted driver to GKS 7.4.
*     17/07/87  PJWR  Modified Set Text Attributes to force (1,STRING)
*                     if a given font/precision pair is unavailable.
*                     Added code for the "hardware" fonts in Inquire
*                     Text Facilities.
*     11/08/87  TAW   Changed polyline code to use hardware styles
*                     and created gk9spl() to do it.
*     08/09/87  PJWR  Cleared up miscellaneous bugs in "hardware" font
*                     selection.
*     10/08/87  PJWR  Modification of 17/07/87 was in error due to mis-
*                     -interpretation of the standard.  All text not in
*                     font 1 is now done at STROKE precision.
*     15/09/87  PJWR  Calculation of monochrome intensity now done by
*                     GK9SCM rather than in the main driver.
*     08/10/87  PJWR  Corrected hatch index range tests in Set Fill Area
*                     attributes.
*     16/10/87  PJWR  Modified to use new polyline external.  This requires
*                     the line style to be in KWKDAT(ILNTY, KWKIX) and the
*                     linewidth in pixels to be in KWKDAT(ILNWD,KWKIX).
*     22/10/87  PJWR  Improved error checking in open and close workstation.
*     30/10/87  PJWR  Changed Fill Area entrypoint to use GK9SFA not GKFILS.
*                     Constrained line width scale factor to /GKYWDT/KLNWD.
*     03/11/87  PJWR  Screen updating for output primitives now done in the
*                     driver,  rather than in the output routines.
*     05/11/87  PJWR  Changed default deferral state to ASAP.
*     09/12/87  PJWR  Updated Inquire Colour Facilities to cope with colour.
*     10/11/88  TAW   Added reset of polyline style and width at end of
*                     polyline entry point and created gk9ssr.
*     30/01/89  TAW   Took out reset of polyline style and width and
*                     Changed polymarker and text output to save current line
*                     style and restore it after primitive. Took out setting
*                     of KWKDAT(ILNTY, KWKIX) and KWKDAT(ILNWD,KWKIX) in the
*                     set polymarker, text and fill area attributes
*                     entry points.
*     12/04/89  RMK   Changed computed goto so it picks up code for
*                     inquire text facilities rather than using inquire
*                     everything.
*                     And similarly for inquire display space size.
*     23/08/89  RMK   Changed pattern index check in set fill area
*                     attributes to allow for user-defined patterns as
*                     well as predefined ones (S79).
*     20/11/89  RMK   Added code for Request Pick.  Changes to entries
*                     for Set Workstation Window and Viewport, and the
*                     Segment Entrypoint.
*                     Use Inquire Everything for Inquire Pick Device
*                     State and Inquire Default Pick Device Data
*                     entries.
*                     Changed Request Locator to use GKRQLC plus the
*                     new workstation cursor input routine GK9SCI.
*     31/01/90  RMK   Changed close wkstn entry to INQUIRE and CLOSE
*                     both files (S353).
*     27/02/90  PLP   Revised set text representation, text extent and
*                     text inquiries. Introduced IHFONT and HERSH, and
*                     gave a font id of -1 to the hardware font (S370).
*     08/05/90  PLP   Introduced parameters IDDFRM and IDIRGM in order to
*                     have the driver's default deferral mode and the default
*                     implicit regeneration mode information stored.
*                     Subsequently altered the Open Workstation and Inquire
*                     Default Deferral State Values entries (S283 and S284).
*     16/05/90  RMK   In initialise input entries, added check of the
*                     PET number (S384).  Added explicit entries for
*                     inquire default locator/stroke/valuator/choice
*                     devices, as GKQWK assumes only 1 PET (S346).
*     10/12/90  KEVP  Made inQuire Text Facilities pass all possible data
*                     even if list element is out of range (C63).
*     11/01/91  PLP   Corrected calling sequence in the GKEPBB call (S436).
*     18/01/91  KEVP  Removed all non-Hershey font numbers from IHFONT and
*                     parameterised its new length as IFNTMX (S423,S424).
*     18/01/91  PLP   Corrected linewidth validation in the Set Polyline
*                     Attributes entry (S433).
*     30/01/91  KEVP  Made sure inQuire TeXt Facilities returns font 1 at
*                     ALL precisions (C27).
*     08/02/91  KEVP  In inQuire Colour Representation, got correct
*                     REALIsed values output (S455).
*     13/02/91  PLP   Changed the Set Text Attributes entry, so that
*                     expansion factor for string precision is always
*                     1.0 (S453).
*     22/02/91  KEVP  For String precision text derived character width vector
*                     in DC (S468).
*     22/02/91  PLP   Moved the font loading call from Set Text Attributes
*                     to the Text entrypoint. Expanded the calling sequence
*                     og GK9SSF to include arrays KWTYIX and KLAWKT - these
*                     are needed to work out workstation type from the
*                     workstation ID (S469).
*     14/03/91  PLP   Of the above fix, in the end only the bit about moving
*                     the font loading call to the Text entrypoint turned out
*                     to be needed. Restored the original version of GK9SSF
*                     and added the call to it in the Enquire Text Extent
*                     entrypoint (needed for open but not active WS).
*
*  ARGUMENTS
*  ---------
*     INP IENT  - Entrypoint code.
*     INP NID   - Size of array IDAT.
*     I/0 IDAT  - Integer data passed to or from workstation.
*     INP NRD   - Size of arrays RX and RY.
*     I/O RX    - Real X-coordinate data passed to or from workstation.
*     I/O RY    - Real Y-coordinate data passed to or from workstation.
*     INP NCD   - Size of character array.
*     I/O STR   - Character array.
*
      INTEGER IENT, NID, IDAT(NID), NRD, NCD
      REAL RX(NRD), RY(NRD)
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*  Read    /GKYFLS/  KDBFLS
*          /GKYHP/   KHPXR
*          /GKYWCA/  KCVIS, KNIR, KNRR, KWI1..3, KWKIX, KWKTYP, QWR1..3
*          /GKYWDT/  QNMMKS
*          /GKYWKD/  KWFAIS, KWLNTY, KWMKTY, KWTXPR, QWCHHX, QWCHHY,
*                    QWCHWX, QWCHWY, QWCLXL, QWCLXR, QWCLYB, QWCLYT,
*                    QWMKSZ
*          /GKYWSL/  KCTBPT, KNFAUP, KSSGPT
*          /GKYXFD/  KHFONT
*  Modify  /GKYERR/  KERROR
*          /GKYHP/   QHP
*          /GKYSTK/  QSTACK
*          /GKYWCA/  KRGN, KSGRQ, KWDONE, KWI1, KWRGN
*          /GKYWDT/  KPCI
*          /GKYWKD/  KWCID, KWFACI, KWFASI, KWIO, KWKDAT, KWPLCI, KWPMCI
*                    KWTXCI, KWTXFN, QWKDAT
*          /GKYWSL/  KCID, KDFM, KDSMT, KIMRGM
*
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkerr.par'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkdt.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkpca.par'
      INCLUDE '../../include/gkpca.cmn'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwcb.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwke.par'
      INCLUDE '../../include/gkwdt.par'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkfls.par'
      INCLUDE '../../include/gkfls.cmn'
      INCLUDE '../../include/gkxfd.cmn'
*
*  EXTERNAL SUBROUTINES
*  --------------------
*     GK9SLN  Solid / styled line output routine.
*     GK9SRO  Raster output routine.
*     GK9SXF  Text details routine.
*     GK9SXC  Text output routine.
*     GK9SSF  Hardware font setting.
*     GK9SCM  Set Sun colourmap entry.
*     GK9SWC  Set WW foreground colour.
*     GK9SFA  Fill area routine.
      EXTERNAL GK9SLN, GK9SRO, GK9SXF, GK9SXC, GK9SSF, GK9SCM, GK9SWC
     :         GK9SFA, GK9SCS, GK9SPP
*
*  EXTERNAL FUNCTIONS
*  ------------------
*     FPUTC   UNIX library function for single byte output.
*     GKAN1   Converts integer ASCII code to native character.
*     GK9SRV  Request valuator input.
*     GK9SRC  Request choice input.
*     GK9SRT  Request string input.
*     GK9SRK  Request stroke input.
*
      INTEGER FPUTC, GK9SRV, GK9SRC, GK9SRT, GK9SRK
      CHARACTER*1 GKAN1
*
*  LOCALS
*  ------
*     HERSH   Logical to signal that a Hershey font is requested.
*     ICREAT  Function code for GK9SWW() - create window.
*     IDDFRM  Default deferral mode.
*     IDIRGM  Default implicit regeneration mode.
*     ISELCT  Function code for GK9SWW() - select window.
*     IDELET  Function code for GK9SWW() - delete window.
*     IUPDAT  Function code for GK9SWW() - update window.
*     ICOSET  Function code for GK9SCM() - set WW colour index.
*     ICOGET  Function code for GK9SCM() - get WW colour index.
*             - ICOSET and ICOGET equal WW's COSET and COGET.
*     ICHSIZ  Index in KWKDAT for index of raster font loaded.
*     ILEFT   Index in KWKDAT for display update area left bound.
*     IRIGHT  Index in KWKDAT for display update area right bound.
*     ITOP    Index in KWKDAT for display update area top bound.
*     IBOTT   Index in KWKDAT for display update area bottom bound.
*     ILNTY   Index in KWKDAT for line type to be drawn by GK9SLN.
*     ILNWD   Index on KWKDAT for line width to be drawn by GK9SLN.
*     ILSCRI  Index in KWKDAT for the last set colour representation index.
*     IQPAX   Index in QWKDAT of X coordinate for INQUIRE PIXEL ARRAY.
*     IQPAY   Index in QWKDAT of Y coordinate for INQUIRE PIXEL ARRAY.
*     ICHHT   Index in QWKDAT for char. height of raster font loaded.
*     ICHWD   Index in QWKDAT for char. width of raster font loaded.
*     ICHCT   Index in QWKDAT for cap to top line distance of raster
*             font loaded.
*     ICHBB   Index in QWKDAT for base to bottom line distance of
*             raster font loaded.
*     ICHGS   Index in QWKDAT for offset of GKS baseline from SunView
*             baseline for the raster font loaded.
*     ICHUNK  Size of stack used for splitting polyline and polymarker.
*     ILCPET  List of locator PETs supported on this workstation
*     ISKPET  List of stroke PETs supported on this workstation
*     IVLPET  List of valuator PETs supported on this workstation
*     ICHPET  List of choice PETs supported on this workstation
*     MNFAHS  Minimum hatch style index.
*     MXFAHS  Maximum hatch style index.
*     MNLNTY  Minmum line type.
*     MXLNTY  Maximum line type.
*     MNPMTY  Minmum polymarker type.
*     MXPMTY  Maximum polymarker type.
*     PATLEN  Length of pattern for line types (value irrelevant).
*     I       Integer used as temporary / subsript / loop control.
*     N       Integer used as temporary count.
*     IOFF    Offset for stack.
*     IOS     Return status from Fortran I/O system and FPUTC (see above).
*     ISOP1   Status return for INQUIRE().
*     ISOP2   Status return for INQUIRE().
*     IPREC   Precision for each font (STRING, CHAR, STROKE).
*     IHFONT  Array containing font ids of Hershey fonts.
*     RICOL   Colour intensity for monochrome
*     INKWI2  Temporary integer, stores type of returned values in inquires
*     INTA    Integer array with multiple uses:
*             - setting representations
*             - integer data return from GKQWDT()
*             - temporary storage for polyline attributes when used by text
*               and marker primitive.
*     REALA   Real array with multiple uses:
*             - testing colour representation
*             - real data return from GKQWDT()
*
*     Function code parameters
      INTEGER    ICOSET,    ICOGET
      PARAMETER (ICOSET= 2, ICOGET= 1)
      INTEGER   ICREAT,     ISELCT,     IDELET,     IUPDAT
      PARAMETER(ICREAT = 1, ISELCT = 2, IDELET = 3, IUPDAT = 4)

*     Integer workspace offset parameters
      INTEGER   ICHSIZ,     ILEFT,     IRIGHT,     ITOP,     IBOTT
      PARAMETER(ICHSIZ = 1, ILEFT = 2, IRIGHT = 3, ITOP = 4, IBOTT = 5)
      INTEGER   ILNTY,      ILNWD,     ILSCRI
      PARAMETER(ILNTY = 6,  ILNWD = 7, ILSCRI = 8)

*     Real workspace offset parameters
      INTEGER   IQPAX,     IQPAY,     ICHHT,     ICHWD,     ICHCT
      PARAMETER(IQPAX = 1, IQPAY = 2, ICHHT = 3, ICHWD = 4, ICHCT = 5)
      INTEGER   ICHBB,     ICHGS
      PARAMETER(ICHBB = 6, ICHGS = 7)

*     Other parameters
      INTEGER   ICHUNK
      PARAMETER(ICHUNK = 1024)

*     Backslash character, doubled for UNIX compilers.
      CHARACTER*1 BS
      PARAMETER( BS = '\\' )

*     Arrays holding lists of PETs supported by this workstation
      INTEGER ILCPET(6), ISKPET(4), IVLPET(3), ICHPET(6)

      REAL      PATLEN
      PARAMETER(PATLEN = 0.0)
      INTEGER   MNFAHS,       MNLNTY,     MNPMTY
      PARAMETER(MNFAHS = -10, MNLNTY = 0, MNPMTY = 0)
      INTEGER   MXFAHS,      MXLNTY,     MXPMTY
      PARAMETER(MXFAHS = -1, MXLNTY = 5, MXPMTY = 5)
*     default deferral mode and default implicit regeneration mode
      INTEGER IDDFRM, IDIRGM
      PARAMETER (IDDFRM=GASAP, IDIRGM=GSUPPD)

*     Local variables
      INTEGER I, IOFF, IOS, N, INKWI2, IFNTMX
      PARAMETER (IFNTMX = KFNTMX-1)
      INTEGER IPREC(KFNTMX), IHFONT(IFNTMX)
      INTEGER INTA(19)
      REAL RICOL
*     Hershey font list
      DATA IHFONT/-101,-102,-103,-104,-105,-106,-107,-110,-115/
*     Lists of PETs
      DATA ILCPET/1, 2, 3, 4, 5, 6/,   ISKPET/1, 2, 3, 4/,
     :     IVLPET/1, 2, 3/,            ICHPET/1, 2, 3, 4, 5, -1/

      LOGICAL ISOP1, ISOP2, HERSH

      REAL REALA(12)
*
*  STACK USAGE
*  -----------
*     POLYLINE and POLYMARKER for transformations.
*
*  HEAP USAGE
*  ----------
*     SET COLOUR REPRESENTATION for colour table.
*
*  STREAMS USED
*  ------------
*     (KCID(KWKIX) + 1) for MESSAGE.
*
*  ERRORS
*  ------
*
*        26  Specified workstation cannot be opened
*        93  Colour index is invalid
*       102  Generalised drawing primitive identifier is invalid
*       104  At least one open workstation is not able to generate the
*            specified generalized drawing primitive
*     -1042  Error while trying to open internal segment store
*
*  COMMENTS
*  --------
*     Both the ww and SunView window management toolkits are used to
*     perform input and output for this driver.  As opening windows
*     isn't amongst GKIOOP()s functions,  this driver also manages
*     its own connections and is hence
*
*                             SYSTEM DEPENDANT
*
*-----------------------------------------------------------------------

*  Intercept request for Pick Echoplay for primitive
      IF(KPKECO .EQ. KPECHO)THEN
        IF((IENT .GE. KPL) .AND. (IENT .LE. KGDP))THEN
*         Pick echoplay requested
          CALL GKEPBB (IENT, NID, IDAT, NRD, RX, RY, 0.02,
     :                 GK9SLN, GK9SXF)
          GOTO 8888
        ENDIF
      ENDIF

* Conditional GOTO on entrypoint code.

      GOTO (       10,  20,  30,  40,  50,  60,  70,  80,9999,
     :       100, 110, 120, 130, 140, 150, 160, 170, 180, 190,
     :       200, 210, 220, 230, 240, 250, 260, 270, 280,9999,
     :      9999, 310, 320, 330,9999,9999,9999,9999,9999,9999,
     :      9999, 410, 410, 410, 410, 410, 410, 410, 410, 410,
     :       410, 410,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999, 610, 620, 630, 640, 650, 660, 670, 680, 690,
     :       700, 710, 720, 730, 740, 750, 760, 770, 780, 790,
     :       800, 810,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999, 910, 920, 930 ) IENT

      GOTO (1111,1210,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1111,1111,1111,1111,1111,1111,1111,1370,1380,1111,
     :      1111,1111,1111,1111,1440,1111,1460,1111,1111,1111,
     :      1111,1111,1111,9999,9999,9999,9999,9999,9999,9999,
     :      9999,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1111,1111,1720,1111,1740,1111,1111,1111,1111,1790,
     :      1111,1111,1111,1111,1111,1850,1111,1870,1880,1111,
     :      1111,1111,1111,1930,1940,1950,1960,1111,1111 ) IENT - 119

      GOTO 9999

* Open Workstation.
   10 CONTINUE
*     Set up workstation description table and workstation state list.
      CALL GKIWSL(KWKIX, KWKTYP)
      IF (KERROR.NE.0) GOTO 9999
*     Initialise state for workstation utilities.
      KCID(KWKIX) = KWI1
*     Deferral mode - as soon as possible (see above)
      KDFM(KWKIX) = IDDFRM
      KWIO(KWKIX) = GYES
*     Implicit regeneration - suppressed (see above)
      KIMRGM(KWKIX) = IDIRGM
*     Here we should call GKIOOP() to establish connections.  Instead
*     we do it locally as GKIOOP() can't handle Sun requirements.
      IF (KCID(KWKIX).GT.KMNIO.AND.KCID(KWKIX).LT.KMXIO) THEN
*       Check the status of both streams.
        INQUIRE(UNIT = KCID(KWKIX), OPENED = ISOP1)
        INQUIRE(UNIT = (KCID(KWKIX) + 1), OPENED = ISOP2)
        IF((.NOT.ISOP1).AND.(.NOT.ISOP2)) THEN
*       Not using standard streams,  so open the output stream to
*       /dev/tty for messages.
          OPEN(UNIT = KCID(KWKIX) + 1, FILE = '/dev/tty')
        ELSE IF (KCID(KWKIX).NE.5.AND.(ISOP1.OR.ISOP2)) THEN
*         One or both streams open and it isn't streams 5 and/or 6.
          KERROR = 26
          GOTO 9999
        ENDIF
      ELSE
*       The output channel will collide with the runtime files
*       or the input channel will collide with stderr.
        KERROR = 21
        GOTO 9999
      ENDIF
*     Now create the workstation.
      CALL GK9SWW(ICREAT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
*     Initialise workspace variables.  Raster font size is set to an
*     impossible value so the first font raster selection will force a
*     load, the screen update area to an impossible rectangle so the
*     first primitive will always reset the update area and
*     the last colour index whose representation was set is set to
*     an impossible value.
      KWKDAT(ICHSIZ,KWKIX) = KNIL
      KWKDAT(ILEFT,KWKIX) = KDSRX(KWKIX) - 1
      KWKDAT(IRIGHT,KWKIX) = 0
      KWKDAT(ITOP,KWKIX) = KDSRY(KWKIX) - 1
      KWKDAT(IBOTT,KWKIX) = 0
      KWKDAT(ILSCRI,KWKIX) = KNIL
*     Initialise colour table.
      DO 11, I = 0, KPCI(KWKIX) - 1
        CALL GK9SCM(ICOSET, I,
     :              QHP(KHPXR(KCTBPT(1,KWKIX))+I),
     :              QHP(KHPXR(KCTBPT(2,KWKIX))+I),
     :              QHP(KHPXR(KCTBPT(3,KWKIX))+I))
   11 CONTINUE
*     Set up workstation category and return.
      KWI1 = GOUTIN
      GOTO 9999

* Close workstation.
   20 CONTINUE
      IF (KWI1.EQ.1) THEN
        KWDONE = KRFUSE
      ELSE
        CALL GK9SWW(IDELET, KWCID(KWKIX))
        IF (KERROR.NE.0) GO TO 9999
        INQUIRE(UNIT = KCID(KWKIX), OPENED = ISOP1)
        INQUIRE(UNIT = (KCID(KWKIX)+1), OPENED = ISOP2)
        IF (ISOP1.AND.KCID(KWKIX).NE.5)
     :    CLOSE(UNIT = KCID(KWKIX), IOSTAT = IOS)
        IF (ISOP2.AND.(KCID(KWKIX)+1).NE.6)
     :    CLOSE(UNIT = (KCID(KWKIX)+1), IOSTAT = IOS)
        IF (IOS.NE.0) THEN
          KERROR = 25
          GOTO 9999
        ENDIF
        CALL GKCWSL(KWKIX)
        CALL GKSLDL(KSSGPT(KWKIX))
      ENDIF
      GOTO 9999

* Clear workstation.
   30 CONTINUE
      IF (KWI1.EQ.2) THEN
        KWDONE = KRFUSE
        CALL GKSLDL(KSSGPT(KWKIX))
      ENDIF
      GOTO 9999

* Redraw all segments on workstation.
   40 CONTINUE
      KWDONE = KRFUSE
      GOTO 9999

* Update workstation.
   50 CONTINUE
      KWDONE = KRFUSE
      GOTO 9999

* Set deferral state.
   60 CONTINUE
      KDFM(KWKIX) = KWI1
      KIMRGM(KWKIX) = KWI2
      IF (KWI1.EQ.GASAP) THEN
        KWIO(KWKIX) = GYES
*       Update window
        CALL GK9SWW(IUPDAT, KWCID(KWKIX))
      ELSE
        KWIO(KWKIX) = GNO
      ENDIF
      IF (KIMRGM(KWKIX).EQ.GALLOW.AND.KNFAUP(KWKIX).EQ.GYES.AND.
     :    KDSMT(KWKIX).EQ.GNEMPT) THEN
        KWRGN(KWKIX) = .TRUE.
        KRGN = .TRUE.
      ENDIF
      GOTO 9999

* Do deferred output actions.
   70 CONTINUE
*     Update window
      CALL GK9SWW(IUPDAT, KWCID(KWKIX))
      KWI1 = KNFAUP(KWKIX)
      GOTO 9999

* Clear display surface.
   80 CONTINUE
      IF (KWI1.EQ.GALWAY.OR.KDSMT(KWKIX).EQ.GNEMPT) THEN
        CALL GK9SWW(ISELCT, KWCID(KWKIX))
        IF (KERROR.NE.0) GO TO 9999
        CALL GK9SCD
      ENDIF
      CALL GKWCLD
      GOTO 9999

* Message.
  100 CONTINUE
      CALL FLUSH(KCID(KWKIX) + 1)
      DO 101, I = 1, NID
        IOS = FPUTC(KCID(KWKIX) + 1, GKAN1(IDAT(I)))
  101 CONTINUE
      IOS = FPUTC(KCID(KWKIX) + 1, BS//'n')
      CALL FLUSH(KCID(KWKIX) + 1)
      GOTO 9999

* Escape [Not Implemented].
  110 CONTINUE
      KERROR = 180
      GOTO 9999

* Polyline.
  120 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
      CALL GKSTAL(KREALS, ICHUNK * 2, IOFF)
      IF (KERROR.EQ.0) THEN
        CALL GK9SWW(ISELCT, KWCID(KWKIX))
        IF (KERROR.NE.0) GO TO 9999
*       Set up device colour.
        CALL GK9SWC(KWPLCI(KWKIX))
*       Output line a chunk at a time. Use the line utility.
*       Transform world coordinates to device coordinates and
*       output clipped line a chunk at a time.
        N = ICHUNK
        DO 121 I = 1, NRD, ICHUNK - 1
          IF (NRD - I.LT.ICHUNK - 1) N = NRD - I + 1
          IF (N.EQ.1) GOTO 121
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF + ICHUNK))
*         Output clipped line.
          CALL GKLCLP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),.FALSE.,PATLEN,
     :                QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                QWCLYT(KWKIX),GK9SLN)
  121   CONTINUE
        CALL GKSTDA(KREALS,IOFF)
      ENDIF
      GOTO 8888

* Polymarker.
  130 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
*     Transform polymarker points and output them a chunk at a time.
      CALL GKSTAL(KREALS,ICHUNK * 2,IOFF)
      IF (KERROR.EQ.0) THEN
        CALL GK9SWW(ISELCT, KWCID(KWKIX))
        IF (KERROR.NE.0) GO TO 9999
*       Set up device colour, linestyle and width.
        CALL GK9SWC(KWPMCI(KWKIX))
        INTA(1) = KWKDAT(ILNTY,KWKIX)
        KWKDAT(ILNTY,KWKIX) = 1
        INTA(2) = KWKDAT(ILNWD,KWKIX)
        KWKDAT(ILNWD,KWKIX) = 0
        N = ICHUNK
        DO 131 I = 1, NRD, ICHUNK
          IF (NRD - I.LT.ICHUNK) N = NRD - I + 1
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF + ICHUNK))
*         Could change this later to draw first marker
*         and copy the others from it.
          CALL GKMTYP(N,QSTACK(IOFF),QSTACK(IOFF + ICHUNK),
     :                   KWMKTY(KWKIX),QNMMKS(KWKIX),QWMKSZ(KWKIX),
     :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK9SLN)
  131   CONTINUE
        CALL GKSTDA(KREALS,IOFF)
        KWKDAT(ILNTY,KWKIX) = INTA(1)
        KWKDAT(ILNWD,KWKIX) = INTA(2)
      ENDIF
      GOTO 8888

* Text.
  140 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
*     Find and load the raster font with the height nearest the
*     magnitude of the character height vector.
      CALL GK9SSF(SQRT(QWCHHX(KWKIX)**2 + QWCHHY(KWKIX)**2))
*     Select display surface, set up colour and solid unbroken line style.
      CALL GK9SWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
      CALL GK9SWC(KWTXCI(KWKIX))
      INTA(1) = KWKDAT(ILNTY,KWKIX)
      KWKDAT(ILNTY,KWKIX) = 1
      INTA(2) = KWKDAT(ILNWD,KWKIX)
      KWKDAT(ILNWD,KWKIX) = 0
*     Output text.
      IF(KWTXPR(KWKIX).EQ.GSTRKP) THEN
        CALL GKXDWO(NID, IDAT, GK9SLN)
      ELSE
        CALL GKXDCS(NID, IDAT, QWCHWX(KWKIX), QWCHWY(KWKIX),
     :              GK9SXF, GK9SXC)
      ENDIF
      KWKDAT(ILNTY,KWKIX) = INTA(1)
      KWKDAT(ILNWD,KWKIX) = INTA(2)
      GOTO 8888

* Fill Area.
  150 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
      CALL GK9SWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
*     Set up device colour.
      CALL GK9SWC(KWFACI(KWKIX))
*     Draw fill area.
      CALL GK9SFA(NRD, RX, RY)
      GOTO 8888

* Cell Array.
  160 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
      CALL GK9SWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
      CALL GKCELL(NID,IDAT,GK9SRO)
      GOTO 8888

* GDP.
  170 CONTINUE
      IF (KCVIS.EQ.GINVIS) GOTO 9999
*     Check the GDP identifier
      IF (KWI1.EQ.0) THEN
        KERROR = 102
        GOTO 9999
      ELSE IF (KWI1.LT.-4.OR.KWI1.GT.-1) THEN
        KERROR = 104
        GOTO 9999
      ELSE
*       Select workstation
        CALL GK9SWW(ISELCT, KWCID(KWKIX))
        IF (KERROR.NE.0) GO TO 9999
        IF (KWI1.EQ.-1) THEN
*         Arc - use polyline colour index
          CALL GK9SWC(KWPLCI(KWKIX))
          CALL GKCRCS(KWI1,NRD,RX,RY,1,.TRUE.,PATLEN,GK9SLN,GK9SRO)
        ELSE
*         Filled chord, pie or circle - use fill area colour index
          CALL GK9SWC(KWFACI(KWKIX))
          CALL GKCRCS(KWI1,NRD,RX,RY,1,.TRUE.,PATLEN,GK9SLN,GK9SRO)
        ENDIF
      ENDIF
      GOTO 8888

* Set Polyline Attributes.
  180 CONTINUE
      CALL GKDPLB
*     Need to check because individual settings won't have been checked.
      IF (KWLNTY(KWKIX).LT.MNLNTY.OR.KWLNTY(KWKIX).GT.MXLNTY)
     :  KWLNTY(KWKIX) = 1
      IF (KWPLCI(KWKIX).GE.KPCI(KWKIX)) KWPLCI(KWKIX) = 1
*     Set up workspace variables for polyline device external.
      KWKDAT(ILNTY,KWKIX) = KWLNTY(KWKIX)
      KWKDAT(ILNWD,KWKIX) = MIN(KLNWD(KWKIX),INT(QWLNWD(KWKIX)))
      GOTO 9999

* Set Polymarker Attributes.
  190 CONTINUE
      CALL GKDPMB
*     Need to check because individual settings won't have been checked.
      IF (KWMKTY(KWKIX).LT.MNPMTY.OR.KWMKTY(KWKIX).GT.MXPMTY)
     :  KWMKTY(KWKIX) = 3
      IF (KWPMCI(KWKIX).GE.KPCI(KWKIX)) KWPMCI(KWKIX) = 1
      GOTO 9999

* Set Text Attributes.
  200 CONTINUE
      CALL GKDTXB
*     Need to check because individual settings won't have been checked.
      IF (KWTXCI(KWKIX).GE.KPCI(KWKIX)) KWTXCI(KWKIX) = 1
*     For STRING and CHAR precision check the font number (this also
*     ensures font 1 in STRING and CHAR is catered for correctly).
      IF(KWTXPR(KWKIX).NE.GSTRKP)THEN
*        Is the font in question a Hershey font?
         DO 205 I=1,IFNTMX
            IF (KWTXFN(KWKIX).EQ.IHFONT(I)) THEN
*              Ensure stroke precision will be used.
               KWTXPR(KWKIX) = GSTRKP
               GOTO 9999
            ENDIF
  205    CONTINUE
*        Only have one hardware font
         KWTXFN(KWKIX) = -1
         IF(KWTXPR(KWKIX).EQ.GSTRP)THEN
*        String precision - derive character expansion and width vector.
             QWCHXP(KWKIX)=  1.0
             QWCHWX(KWKIX)=  QWCHHY(KWKIX)
             QWCHWY(KWKIX)= -QWCHHX(KWKIX)
         ENDIF
      ENDIF
      GOTO 9999

* Set Fill Area Attributes.
  210 CONTINUE
      CALL GKDFAB
*     Need to check because individual settings won't have been checked.
      IF (KWFAIS(KWKIX).EQ.GHATCH.AND.
     :    (KWFASI(KWKIX).LT.MNFAHS.OR.KWFASI(KWKIX).GT.MXFAHS))
     :  KWFASI(KWKIX) = -1
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

* Set Pick Identifier [Not Implemented].
  220 CONTINUE
      GOTO 9999

* Set Polyline Representation.
  230 CONTINUE
      INTA(1) = 5
      CALL GKSRPL(1,INTA,.FALSE.)
      GOTO 9999

* Set Polymarker Representation.
  240 CONTINUE
      CALL GKSRPM(0,INTA,.FALSE.)
      GOTO 9999

* Set Text Representation.
  250 CONTINUE
*     Is the requested font a Hershey font?
      DO 252 I=1,IFNTMX
        IF (KWI2.EQ.IHFONT(I)) THEN
           HERSH = .TRUE.
*          Make sure that fonts are available.
           IF (KDBFLS.EQ.KFLNA) THEN
             KERROR = -1042
             GOTO 9999
           ENDIF
           IF (KDBFLS.EQ.KFLCL) CALL GKXON
           IF (KERROR.NE.0) GOTO 9999
           GOTO 253
        ENDIF
  252 CONTINUE
      HERSH = .FALSE.

*     Deal with hardware font and a special case of font 1 first.
  253 IF(KWI2.EQ.-1.OR.KWI2.EQ.1) THEN
         INTA(1)=KWI2
         IPREC(1)=KWI3
         CALL GKSRTX(1,INTA,IPREC,.FALSE.)
      ELSEIF (HERSH) THEN
*       Prepare the precision array (accept the user's setting)
        DO 255 I = 1, KFNTMX
          IPREC(I) = KWI3
  255   CONTINUE
        CALL GKSRTX(KFNTMX,KHFONT,IPREC,.FALSE.)
      ELSE
*       Font not known
        KERROR=76
      ENDIF
      GOTO 9999

* Set Fill Area Representation.
  260 CONTINUE
      CALL GKSRFA(.FALSE.)
      GOTO 9999

* Set Pattern Representation.
  270 CONTINUE
      CALL GKSRPA(NID,IDAT)
      GOTO 9999

* Set Colour Representation.
  280 CONTINUE
*     See if colour index is valid and set up GKS colour table.
      IF (KWI1.GE.0.AND.KWI1.LT.KPCI(KWKIX)) THEN
        QHP(KHPXR(KCTBPT(1,KWKIX))+KWI1) = QWR1
        QHP(KHPXR(KCTBPT(2,KWKIX))+KWI1) = QWR2
        QHP(KHPXR(KCTBPT(3,KWKIX))+KWI1) = QWR3
*       Select workstation display surface.
        CALL GK9SWW(ISELCT,KWCID(KWKIX))
        IF (KERROR.NE.0) GO TO 9999
*       Set the colour representation.
        CALL GK9SCM(ICOSET, KWI1, QWR1, QWR2, QWR3)
*       Store colour index
        KWKDAT(ILSCRI,KWKIX) = KWI1
      ELSE
        KERROR = 93
      ENDIF
      GOTO 9999

* Normalization Transformation.
  310 CONTINUE
      CALL GKWKC4
      GOTO 9999

* Set Workstation Window.
  320 CONTINUE
      CALL GKSWKW
      CALL GKPPBU
      GOTO 9999

* Set Workstation Viewport.
  330 CONTINUE
      CALL GKSWKV
      CALL GKPPBU
      GOTO 9999

* Segment Entrypoints.
  410 CONTINUE
      CALL GKSGWB(IENT,.FALSE.)
      GOTO 9999

* Initialise Locator.
  610 CONTINUE
* First, check that the PET number is valid.
      DO 612 I=1,6
        IF (ILCPET(I) .EQ. KWI2) GOTO 615
  612 CONTINUE
      KERROR = 144
      GOTO 9999
  615 CONTINUE
      CALL GKINIP(GLOCAT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999

* Initialise Stroke.
  620 CONTINUE
* First, check that the PET number is valid.
      DO 622 I=1,4
        IF (ISKPET(I) .EQ. KWI2) GOTO 625
  622 CONTINUE
      KERROR = 144
      GOTO 9999
  625 CONTINUE
      CALL GKINIP(GSTROK,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999

* Initialise Valuator.
  630 CONTINUE
* First, check that the PET number is valid.
      DO 632 I=1,3
        IF (IVLPET(I) .EQ. KWI2) GOTO 635
  632 CONTINUE
      KERROR = 144
      GOTO 9999
  635 CONTINUE
      CALL GKINIP(GVALUA,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999

* Initialise Choice.
  640 CONTINUE
* First, check that the PET number is valid.
      DO 642 I=1,6
        IF (ICHPET(I) .EQ. KWI2) GOTO 645
  642 CONTINUE
      KERROR = 144
      GOTO 9999
  645 CONTINUE
      CALL GKINIP(GCHOIC,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999

* Initialise Pick.
  650 CONTINUE
* First, check that the PET number is valid - only one supported
      IF (KWI2.EQ.1) THEN
        CALL GKINIP(GPICK,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        KERROR = 144
      ENDIF
      GOTO 9999

* Initialise String.
  660 CONTINUE
* First, check that the PET number is valid - only one supported
      IF (KWI2.EQ.1) THEN
        CALL GKINIP(GSTRIN,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        KERROR = 144
      ENDIF
      GOTO 9999

* Set Input Operating Modes.
  670 CONTINUE
      GOTO 9999

* Set Input Modes.
  680 CONTINUE
      CALL GKSIPM
      GOTO 9999

* Request Locator.
  690 CONTINUE
      CALL GK9SWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
      CALL GKRQLC(GK9SCS, RX(1), RY(1))
      GOTO 9999

* Request Stroke.
  700 CONTINUE
      CALL GK9SWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
      KWI1 = GK9SRK(KNRR,NRD,RX,RY)
      GOTO 9999

* Request Valuator.
  710 CONTINUE
      CALL GK9SWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
      KWI1 = GK9SRV(QWR1)
      GOTO 9999

* Request Choice.
  720 CONTINUE
      CALL GK9SWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
      KWI1 = GK9SRC(KWI2)
      GOTO 9999

* Request Pick.
  730 CONTINUE
*     KWI2 input  KNIL=pick begining, KPECHO=echoplay done
*                                     KPUNEC=unecho done
*                 Suppress UNECHO by making GKRQPK think that it's
*                 already been done, if ECHO done.
      IF(KWI2 .EQ. KPECHO)KWI2 = KPUNEC
      CALL GKRQPK (4.0,GK9SPP,GK9SCS,GK9SXF)
      GOTO 9999

* Request String.
  740 CONTINUE
      CALL GK9SWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
      KWI1 = GK9SRT(NID,STR,KNIR)
      CALL GKNTOA(KNIR,STR,IDAT)
      GOTO 9999

* Sample Locator [Not Implemented].
  750 CONTINUE
      GOTO 9999

* Sample Stroke [Not Implemented].
  760 CONTINUE
      GOTO 9999

* Sample Valuator [Not Implemented].
  770 CONTINUE
      GOTO 9999

* Sample Choice [Not Implemented].
  780 CONTINUE
      GOTO 9999

* Sample Pick [Not Implemented].
  790 CONTINUE
      GOTO 9999

* Sample String [Not Implemented].
  800 CONTINUE
      GOTO 9999

* Flush Device Events.
  810 CONTINUE
      GOTO 9999

* Write Item To GKSM.
  910 CONTINUE
      KERROR = 32
      GOTO 9999

* Get Item Type From GKSM.
  920 CONTINUE
      KERROR = 34
      GOTO 9999

* Read Item From GKSM.
  930 CONTINUE
      KERROR = 34
      GOTO 9999

* Inquire Everything.
 1111 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999

* Inquire Pixel Array.
 1210 CONTINUE
      CALL GK9SWW(ISELCT, KWCID(KWKIX))
      IF (KERROR.NE.0) GO TO 9999
      CALL GKTWD(1,QWRA(1),QWRA(2),QWKDAT(IQPAX,KWKIX),
     :  QWKDAT(IQPAY,KWKIX))
      CALL GK9SPA(NINT(QWKDAT(IQPAX,KWKIX)),NINT(QWKDAT(IQPAY,KWKIX)),
     :  IDAT)
      GOTO 9999

* Inquire text representation
 1370 CONTINUE

      INKWI2 = KWI2
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      IF(INKWI2.EQ.GREALI) THEN
*       hardware fonts
        IF(KWI1.EQ.-1) THEN
            IF(KWI2.EQ.GSTRP .OR. KWI2.EQ.GCHARP) GOTO 1379
        ENDIF
*       hershey stroke fonts
        IF(KWI2.EQ.GSTRKP) THEN
          DO 1374 I=1,IFNTMX
            IF(KWI1.EQ.IHFONT(I)) GOTO 1379
 1374     CONTINUE
        ENDIF
*       default font
        KWI1=1
 1379   CONTINUE
        IF(KWI3.GE.KPCI(KWKIX)) KWI3 = 1
      ENDIF
      GOTO 9999

* Inquire Text Extent.
 1380 CONTINUE
*     Is the font in question a Hershey font?
      DO 1382 I=1,IFNTMX
        IF (KWTXFN(KWKIX).EQ.IHFONT(I)) THEN
           HERSH = .TRUE.
           GOTO 1383
        ENDIF
 1382 CONTINUE
      HERSH = .FALSE.

*     Stroke Precision.
 1383 IF (KWTXPR(KWKIX).EQ.GSTRKP.OR.HERSH) THEN
        CALL GKXQXO(NID,IDAT,RX,RY)
*     String and Char Precision.
      ELSE
*       Find and load the raster font with the height nearest the
*       magnitude of the character height vector. This is needed
*       so as to return proper values for opened but not active
*       workstations (when no foat has been loaded and consequently
*       no font data stored into QWKDAT and KWKDAT).
        CALL GK9SSF(SQRT(QWCHHX(KWKIX)**2 + QWCHHY(KWKIX)**2))
*       Baseline vector from WS Set Text Attributes entry.
        CALL GKXQXC (NID,IDAT,QWCHWX(KWKIX),QWCHWY(KWKIX),
     :                  RX,RY,GK9SXF)

      ENDIF
      GOTO 9999

* Inquire colour representation
 1440 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      IF(KERROR .EQ. 0)THEN
        IF(KWI2 .EQ. GREALI .AND. KPCI(KWKIX) .EQ. 2)THEN
*          Realised values for monochrome workstation
           IF(KWKDAT(ILSCRI,KWKIX) .NE. KNIL) THEN
*          Colour representation has been set
             IF(KWKDAT(ILSCRI,KWKIX) .EQ. 1-KWI1)THEN
*              Last colour set is not the one inquired - inquire it
               KWI1 = 1 - KWI1
               CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
               KWI1 = 1 - KWI1
*              Get the intensity of its opposite
               RICOL = 1.0 - 0.3*QWR1 - 0.59*QWR2 - 0.11*QWR3
             ELSE
*              Last colour set is the one inquired - get its intensity
               RICOL = 0.3*QWR1 + 0.59*QWR2 + 0.11*QWR3
             ENDIF
*            Change intensity to nearest black or white
             IF(RICOL .GT. 0.5)THEN
                RICOL = 1.0
             ELSE
                RICOL = 0.0
             ENDIF
*            Output realised RGB values
             QWR1 = RICOL
             QWR2 = RICOL
             QWR3 = RICOL
           ENDIF
        ENDIF
      ENDIF
      GOTO 9999

* Inquire Set Member of Segment Names on Workstation.
 1460 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KSGRQ = KSGRQ + 1
      GOTO 9999

* Inquire Maximum Display Surface Size.
 1720 CONTINUE
*     This must be read direct from the WDT because the state list items
*     get modified if the application changes the display space on OPEN
*     WORKSTATION.
      CALL GKQWDT(KWKTYP, KWDT, KNONE, 19, 12, INTA, REALA)
      IF (KERROR.NE.0) GOTO 9999
      KWI1 = GOTHU
      KWI2 = INTA(1)
      KWI3 = INTA(2)
      QWR1 = REALA(1)
      QWR2 = REALA(2)
      GOTO 9999


* Inquire Default Deferral State Values
 1740 CONTINUE
*     Data returned:
*     KERROR : error indicator
*     KWI1   : default value for deferral mode
*     KWI2   : default value for implicit regeneration mode

      KWI1 = IDDFRM
      KWI2 = IDIRGM
      GOTO 9999


* Inquire Text Facilities.
 1790 CONTINUE
*     no of text font and precision pairs (Hardware font,
*     Hershey fonts, plus font 1)
      N=IFNTMX+3
*     precisions guaranteed by ws
      IF(KWI1.EQ.1) THEN
        KWI2 = 1
        KWI3 = GSTRP
      ELSEIF(KWI1.EQ.2) THEN
        KWI2 = 1
        KWI3 = GCHARP
      ELSEIF((KWI1.GE.3) .AND. (KWI1 .LE. N))THEN
*       Stroke precision font - first make sure that fonts are available
        KWI3=GSTRKP
*       stroke font 1 special case
        IF(KWI1.EQ.3) THEN
          KWI2=1
        ELSE
          KWI2=IHFONT(KWI1-3)
        ENDIF
        IF(KDBFLS.EQ.KFLNA) THEN
           KERROR=-1009
           GOTO 9999
        ENDIF
*       Open fonts database if necessary
        IF (KDBFLS.EQ.KFLCL) CALL GKXON
        IF (KERROR.NE.0) GOTO 9999
      ELSE
*       List element of of range, error dealt by front-end
        KWI2 = KNIL
        KWI3 = KNIL
      ENDIF
*
*     Get remaining data (other than KWI2 & KWI3)
      KWI1 = N
      IF (KWKIX.NE.KNIL) THEN
*     A suitable workstation is open,  so use available state
         KWI4 = KCHH(KWKIX)
         KWI5 = KCHXPF(KWKIX)
         KWI6 = KPTXI(KWKIX)
         QWR1 = QMNCHH(KWKIX)
         QWR2 = QMXCHH(KWKIX)
         QWR3 = QMNCHX(KWKIX)
         QWR4 = QMXCHX(KWKIX)
      ELSE
*       Interrogate the WDT file
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


* Inquire Colour Facilities.
 1850 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KWI1 = KWI3
      IF (KWI1.GT.2) KWI2 = GCOLOR
      GOTO 9999

* Inquire List Element of Available GDPs.
 1870 CONTINUE
      IF (KWI1.GE.1.AND.KWI1.LE.4) THEN
        KWI2 = -KWI1
      ELSE
        KERROR = 2002
        KWI2 = KNIL
      ENDIF
      KWI4 = 4
      GOTO 9999


* Inquire GDP.
 1880 CONTINUE
      KNIR = 1
      IDAT(2) = KNIL
      IDAT(3) = KNIL
      IDAT(4) = KNIL
      IF (KWI1.EQ.-1) THEN
        IDAT(1) = GPLATT
      ELSE IF (KWI1.LE.-2.AND.KWI1.GE.-4) THEN
        IDAT(1) = GFAATT
      ELSE
        KNIR = 0
        IDAT(1) = KNIL
        KERROR = 41
      ENDIF
      GOTO 9999

* Inquire Default Locator Device Data.
 1930 CONTINUE
* First, check the list element requested
      IF (KWI2.LT.1 .OR. KWI2.GT.6) THEN
        KERROR = 2002
        GOTO 9999
      ENDIF
      KWI3 = ILCPET(KWI2)
      KWI2 = 6
      CALL GKQWDT(KWKTYP,KLC,KWI1,6,6,INTA,REALA)
      IF (KERROR.EQ.0) THEN
        QWR1 = REALA(1)
        QWR2 = REALA(2)
        QWR3 = REALA(3)
        QWR4 = REALA(4)
        QWR5 = REALA(5)
        QWR6 = REALA(6)
        CALL GKPIDD(GLOCAT,NCD,KNCR,STR)
      ELSE
        KERROR = 140
      ENDIF
      GOTO 9999

* Inquire Default Stroke Device Data.
 1940 CONTINUE
* First, check the list element requested
      IF (KWI2.LT.1 .OR. KWI2.GT.4) THEN
        KERROR = 2002
        GOTO 9999
      ENDIF
      KWI3 = ISKPET(KWI2)
      KWI2 = 4
      CALL GKQWDT(KWKTYP,KSK,KWI1,8,6,INTA,REALA)
      IF (KERROR.EQ.0) THEN
        QWR1 = REALA(1)
        QWR2 = REALA(2)
        QWR3 = REALA(3)
        QWR4 = REALA(4)
        KWI4 = INTA(7)
        KWI5 = INTA(8)
        CALL GKPIDD(GSTROK,NCD,KNCR,STR)
      ELSE
        KERROR = 140
      ENDIF
      GOTO 9999

* Inquire Default Valuator Device Data.
 1950 CONTINUE
* First, check the list element requested
      IF (KWI2.LT.1 .OR. KWI2.GT.3) THEN
        KERROR = 2002
        GOTO 9999
      ENDIF
      KWI3 = IVLPET(KWI2)
      KWI2 = 3
      CALL GKQWDT(KWKTYP,KVL,KWI1,6,7,INTA,REALA)
      IF (KERROR.EQ.0) THEN
        QWR1 = REALA(1)
        QWR2 = REALA(2)
        QWR3 = REALA(3)
        QWR4 = REALA(4)
        QWR5 = REALA(5)
        QWR6 = REALA(6)
        QWR7 = REALA(7)
        CALL GKPIDD(GVALUA,NCD,KNCR,STR)
      ELSE
        KERROR = 140
      ENDIF
      GOTO 9999

* Inquire Default Choice Device Data.
 1960 CONTINUE
* First, check the list element requested
      IF (KWI2.LT.1 .OR. KWI2.GT.6) THEN
        KERROR = 2002
        GOTO 9999
      ENDIF
      KWI3 = ICHPET(KWI2)
      KWI2 = 6
      KWI4 = 10
      CALL GKQWDT(KWKTYP,KCH,KWI1,6,4,INTA,REALA)
      IF (KERROR.EQ.0) THEN
        QWR1 = REALA(1)
        QWR2 = REALA(2)
        QWR3 = REALA(3)
        QWR4 = REALA(4)
        CALL GKPIDD(GCHOIC,NCD,KNCR,STR)
      ELSE
        KERROR = 140
      ENDIF
      GOTO 9999

* N.B.  Don't need entries for inquire default pick and string device
*       data, as this driver only supports 1 PET for these.

* Update display usage information and update window if necessary.
 8888 CONTINUE
      KDSMT(KWKIX) = GNEMPT
      IF (KWIO(KWKIX).EQ.GYES) THEN
        CALL GK9SWW(IUPDAT, KWCID(KWKIX))
      ENDIF

* Exit.
 9999 CONTINUE
      RETURN

      END
