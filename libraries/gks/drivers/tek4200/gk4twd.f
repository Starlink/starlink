      SUBROUTINE GK4TWD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:             AS (Original W/S GK0TWD)
*
      INCLUDE '../../include/check.inc'
*
*   Version: 8
*
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Workstation Driver (Main routine)
*             for TEKTRONIX 4200 Series:
*
*             250 = TEK 4205
*             251 = TEK 4207/4208/4209
*             252 = TEK 4224
*             253 = TEK 4225/4235/4236/4237  4 bit-planes
*             254 = TEK 4225/4235/4236/5237  8 bit-planes
*             255 = TEK 4235/4236/4237      12 bit-planes
*
*  SUBROUTINES
*  -----------
*   Device Setting Routines
*     GK4TID:  Initialise device
*     GK4TMV:  Move cursor
*     GK4TSC:  Set fill-area colour (used for cell array utility)
*     GK4TSD:  Set defaults for hardware segments
*     GK4TSI:  Set hardware fill area interior
*     GK4TSL:  Set hardware line style
*     GK4TSP:  Set hardware line or text colour
*     GK4TST:  Set hardware text attributes
*     GK4TSW:  Set hardware line width (w/s 252 to 255 only)
*
*   Graphics Routines
*     GK4TCC:  Circular arc
*     GK4TCL:  Clear display
*     GK4TCS   Get input through cursor
*     GK4TFA:  Device Fill area - takes DC
*     GK4TFL:  Fill area (front end) - takes WC and clips
*     GK4TGI:  Display prompt and get input
*     GK4THC:  Circular arc with hardware curve
*     GK4TLN:  Device polyline
*     GK4TME:  Message
*     GK4TPC:  Set RGB value for a colour index
*     GK4TRO:  Device raster output (limited use)
*     GK4TSH:  Hardware segment handler
*     GK4TXT:  Hardware text output
*
*   Service routines
*     GK4TCT:  Convert device coords to terminal space coords
*     GK4TRD:  Dummy Raster output routine (never should be entered)
*     GK4TRS:  Reverse the segment transformation on a set of pts
*              (open segment displayed by hardware segment only)
*     GK4TXF:  Obtain hardware font details
*
*   Buffer Interface Routines
*     GK4TTI:  Put integer into output buffer
*     GK4TTP:  Put position into output buffer
*     GK4TTR:  Put real number into output buffer
*     GK4TTS:  Handle long array to ouput buffer (should be in GKIOBO)
*
*
*  MAINTENANCE LOG
*  ---------------
*     The maintenance logs for all workstation subroutines
*     are included here.
*     13/01/84  AS    Original version stabilized (T4010)
*     07/03/84  MGC   Original version of GK0TXF stabilised (CC81)
*     08/01/85  GGT   Original versions of GK0TID & GK0TPC stabilized
*     21/01/85  GGT   Support for hardware characters (T4010)
*     25/01/85  GGT   Converted for Tek 4107
*     23/12/87  KEVP  Converted for TEK 4200 series (version 0)
*     19/01/88  DSG   Typing of SF and GARC put in.
*     25/01/88  KEVP  Deleted raster output argument of GK4TCC
*     28/01/88  KEVP  GK4TCC corrected number of vectors in arc
*     10/02/88  KEVP  GK4TCC corrected number of vectors in circle
*     19/02/88  KEVP  Created GK4TSI to contain hardware area setting
*     04/03/88  KEVP  Update input initialisation utilities
*     14/03/88  KEVP  Correct small inaccuracy in inquire
*                     pixel array dimensions (should be in utility)
*     15/03/88  KEVP  Created GK4TFL as front end to Fill Area.
*                     Changed GK4TFA to device fill area routine
*                     and used GKFILS for clipping.
*                     GK4TRD dummy raster output routine created
*     21/03/88  KEVP  Changed final agument of GK4THC from vectors per
*                     radian to angle per vector
*                     and removed redundant arguments.
*     21/03/88  KEVP  IS conversion: Updated all error numbers to 7.4.
*     24/03/88  KEVP  Changed coordinate system for open segments
*                     displayed by hardware segments. Now coords
*                     go through complete transformation via GKWKC4
*                     for clipping and then back transformed to C2
*                     before sending to device. This is so open
*                     transformed segments clip corectly.
*                     Entry point KNT=31 simplified and GK4TRS created.
*     24/03/88  KEVP  Made driver switch to software segments, if
*                     workstation viewport does not fit inside the
*                     terminal space of the open segment (if one exists).
*                     Changes at KSWKVP=33 and KSSGT=47.
*     28/03/88  KEVP  Made switch to software segments smooth by
*                     suppressing segment playback prior to regeneration.
*     05/04/88  KEVP  Restructured cell array entry point and routines
*                     to cut down unnecessary code by using the proposed
*                     utility GKCELT that gives cell array box in DC
*                     and the cell array transformation and passing
*                     the transformation to GK4TCA.
*                     Utilities GKMTDV, GKMTIV and GKMTXF put in GK4TCA.
*     14/04/88  KEVP  Replaced text utility GKXDWC with GKXDCS for string
*                     and character precision text.Single character
*                     output routine GK4TXC replaced by multi-character
*                     output routine GK4TXT. Made improvements to the
*                     hardware character width and spacing.
*     15/04/88  KEVP  Included pick echoplay into primitive entry point.
*                     Pick unimplemented pending selection of utilities.
*                     Removed the restoration of terminal environment
*                     and included rest of GK4TIE into main routine.
*                     (Version 1 Released)
*     20/04/88  KEVP  Replaced cell array routine GK4TCA with the
*                     utility GKCELA and so eliminated distortion of
*                     cells that cross the terminal space boundary.
*     26/04/88  KEVP  Changed GK4TXT to treat single character as
*                     character precision for both string and char. prec.
*     27/04/88  KEVP  To clear screen all views(not just the current one)
*                     are now renewed (necessary for multiple windows).
*                     TEK command PAGE replaced by RENEW all views.
*     27/04/88  KEVP  Enforce minimum character height (according to WDT)
*                     for string precision text.
*                     (Version 2 Released)
*     14/06/88  KEVP  Made GK4TWV refer to WDT for display dimensions
*     15/06/88  KEVP  Updated and corrected Inquiry entry-points
*     02/08/88  KEVP  Updated and corrected Set Fill-Area Attributes
*     19/08/88  KEVP  Fixture of Colour setting bug confirmed
*                     (Version 3 Released)
*     26/08/88  KEVP  Updated error numbers for entry pts KSTXR and KSCR
*     31/08/88  KEVP  Removed corrections for KQPXAD from GK4TWD
*     31/08/88  KEVP  Corrected Cell array utility selection at KCA
*     05/09/88  KEVP  Corrected 'Set Graphtext Size' in GK4TID to 'MC'
*     25/10/88  KEVP  Enabled STRING input to work without initialisation
*     08/11/88  KEVP  Corrected Argument to GK4TRS in GK4TFA (NR to NRD)
*                     (Version 4 Released)
*     02/12/88  KEVP  Took GIN locator report of length 6, to be a break.
*                     This length used to be 0.
*     02/12/88  KEVP  Allowed for singular segment transformation.
*     05/12/88  KEVP  Made it abandon hardware segments,
*                     if transformation is not of form SCALE-ROTATE.
*     12/12/88  KEVP  Replaced prompt routine GK4TPD with prompt and
*                     get input routine GK4TGI with prompt written on
*                     dialog area ignoring echo area. Approprate changes
*                     were made to VALUATOR, CHOICE and STRING input.
*     13/12/88  KEVP  Made sure that every entry-point had its GKWKE
*                     parameter name in brackets placed at begining to
*                     enable quick search.
*     14/12/88  KEVP  Introduced new utility GKRQCH for CHOICE input.
*     15/12/88  KEVP  Introduced new utility GKRQST for STRING input.
*                     Consequently updated the Request choice & string
*                     and Inquire default choice device data entries,
*                     removed obselete local variables.
*     15/12/88  KEVP  Added GOTO 9999 to end of entry point KQDLC/KQDSK
*     20/12/88  KEVP  Enabled inquiry entry-points KQSGP,KQDSGA,KQDLC
*                     and KQDSK to work, when workstation is NOT open.
*     22/12/88  KEVP  Made keyboard/mouse/tablet LOCATOR devices
*                     1,2 and 3, rather than different PETs.
*                     Similarly for STROKE input. Entry points KQDLC
*                     and KQDSK consequently diverterd to GKQWK.
*                     Appropriate changes made to WDTs.
*     10/01/89  KEVP  Introduced request locator utility GKRQLC and
*                     and replaced GK4TLC & GK4TLI with GK4TCS which
*                     is used as an argument for GKRQLC.
*     12/01/89  KEVP  Made some corrections to string precision text
*                     concerning normal character spacing.
*     23/02/89  KEVP  Put in new utility GKFILH for Hollow and Hardware
*                     Fill-Area. It will now work properly if clipping
*                     splits polygon into several intersecting polygons.
*     21/03/89  KEVP  Put conversion factor between DC and TSC in QWKDAT
*     12/04/89  KEVP  Ensured that segment list is deleted on clearing
*                     the workstation, if hardware segments are in use.
*     28/04/89  KEVP  Introduced new utility GKSGWB to handle segments
*                     with bounding boxes.
*     04/05/89  KEVP  Introduced GKPPBU to set all bounding boxes to
*                     undefined in event of a change in workstation
*                     transformation (necessary cos boxes are in DC)
*     09/05/89  KEVP  (Version 5 released)
*     12/05/89  KEVP  Pick echoplay made to work properly via GK4TSP.
*     30/06/89  KEVP  Used hardware pixel facility in GK4TRO, instead of
*                     polyline. Still not suitable for hardware segments.
*                     Made GK4TTP always include extra byte. It is not
*                     used in Polyline or Fill area for sending out
*                     a large number of coordinates.
*     03/07/89  KEVP  Speed up GK4TRO by having multiple runcodes in
*                     array. Raised HCRIT from 2.0 to 7.0.
*     19/09/89  KEVP  Removed KHFONT and KFNTMX so that new stroke
*                     precision utilities can be used. IFONTN and 9
*                     are respectively used instead (temporary measure).
*     20/09/89  KEVP  Tidied up Inquire text facilities entry point,
*                     so that most variables are handled by GKQWK.
*     21/09/89  KEVP  (Version 6 released)
*     04/10/89  KEVP  Put fill area style into workspace for GK4TCC,
*                     to restore correct operation of GDP after
*                     installation of GKFILH fill-area utility.
*     13/10/89  KEVP  Corrected handling of extra byte in GK4TLN.
*     17/10/89  KEVP  In GK4TWV, changed the border colour index in
*                     SET VIEW ATTRIBUTES to 1, because on the
*                     Tektronix 42xx, 43xx, this is also the input
*                     Cursor Colour Index. This change should not effect
*                     the Tektronix 420x workstations.
*     23/10/89  KEVP  Corrected length of colour setting array in GK4TPC.
*     23/10/89  KEVP  Put INCLUDE(CHECKIBM) in ALL routines but GK4TRD.
*     23/10/89  KEVP  Put hardware linewidth in use for w/s 252 to 255,
*                     giving rise to new subroutine GK4TSW.
*     24/10/89  KEVP  Adapted Cell array for w/s 252 to 255.
*     24/10/89  KEVP  Limited runlength in GK4TRO for GK4TTI.
*     25/10/89  KEVP  Made STROKE input work properly with mouse.
*     26/10/89  KEVP  Put exit GOTO at end of entrypoint KRQCH.
*     10/11/89  KEVP  Corrected bug concerneing the setting of the
*                     EOL and EOM characters in GK4TID. This change
*                     enables Single Key Input.
*     14/11/89  KEVP  For Mouse input enabled first two keys of mouse
*                     to act as ordinary trigger and third as break
*                     for locator, while for stroke the first mouse
*                     key completes the stroke (other two as locator).
*     15/11/89  KEVP  Before STROKE input and PICK echoplay, closed
*                     the open hardware segment, if any and reopened
*                     it afterwards. This was done to prevent
*                     echoing being included in the open segment,
*                     when it is a hardware segment.
*     17/11/89  NMH   Changed report eom frequency to less frequent
*                     in GK4TID to stop host sending two CR characters
*                     at the end of a GIN report.
*     17/11/89  NMH   Changed error number in GK4TCS from 303 to 302.
*     17/11/89  NMH   Altered GK4TCS so that it works on IBM and VAX.
*     22/11/89  KEVP  Made sure primitive is not drawn, if an invisible
*                     segment is open and segments are played back from
*                     CSS (ie, hardware segments not in use).
*     22/11/89  KEVP  Made sure inquire polyline representation gives
*                     correct value for realised line width for
*                     workstations 252 to 255.
*                     (Version 7 released)
*     24/11/89  NMH   Made Inquire pick device state call GKQWK
*     24/11/89  NMH   Made Inquire default pick device data call GKQWK
*     24/11/89  NMH   Removed temporary initialise pick code
*     24/11/89  NMH   Changed Inquire Colour Facilities entry to check
*                     KWKTYP for workstation type so that it works with
*                     unopen workstation.
*     24/11/89  NMH   Added INCLUDE of check file to GK4TRD and declare
*                     all variables.
*     28/11/89  NMH   Removed some debug write statements left in.
*     28/11/89  NMH   Changed GK4TGI to read with purge of type ahead
*                     buffer.
*     28/11/89  KEVP  Enabled GK4TPC to set colours with indices
*                     greater than 15 properly.
*     29/11/89  KEVP  Made GK4TSC use WDT to get maximum number of
*                     colour indices.
*     29/11/89  KEVP  Made GK4TWV get ratio of display dimensions
*                     from the WDT, rather than assume 4/3.
*     29/11/89  KEVP  Corrected maximum runlength in GK4TRO.
*     12/12/89  NMH   Removed comment statements in GK4TID enabling
*                     w'kstation dependent initialisation of GIN devices.
*                     Declared parameter IDSIZE.
*      1/12/89  KEVP  Simplified Inquire Colour Facilities.
*     12/12/89  KEVP  Enabled GK4TSP to take colour indices 16+
*      9/01/90  KEVP  Allowed viewing keys to be used at user's risk.
*     15/01/90  NMH   Removed MORE debug write statements.
*     15/01/90  NMH   Inserted missing routine GK4TSP
*                     (released to Pete Hallowell)
*     29/01/90  KEVP  Corrected total number of colours to include the
*                     whole palette, not the simultaneously displayable.
*     02/02/90  KEVP  For Char precision text, set stringlength of
*                     graphtext command in execution not data statement.
*     12/04/90  KEVP  In GK4TRO ensured colour index is in range so that
*                     runlength is not effected if colour index too high.
*     30/05/90  KEVP  Remove the dummy routine GK4TRD. Use GK4TRO
*                     instead, so getting rid of error -9999 (S246).
*                     (Version 8 released)
*     24/07/90  PLP   Removed unused locals and installed under MS.
*     17/01/91  KEVP  Multiplied hardware character width by character
*                     expansion, when setting text attributes (C46,C64).
*     27/03/91  KEVP  Put check on validity of the PET when initialising
*                     input devices (C39).
*     28/03/91  KEVP  In inQuire Colour Facilities return KWI3 colours
*                     as available (ie, simultaneously displayable)
*                     in line with the official fix of bug C12.
*     04/04/91  KEVP  Use pattern utilities for patterned fill enabling
*                     user to set pattern and fixing bugs C31 & C33.
*     11/04/91  KEVP  Corrected order of KWI1 & KNFAUP at Update
*                     WorKstation entry point (C71).
*     12/04/91  KEVP  Set GIN AREA to current view to keep mouse with
*                     cursor in Locator and Pick input (C72).
*     08/05/91  KEVP  Changed Standard fixup level from 6 to 2, so that
*                     update characteristics of hardware segments match
*                     those of CSS segments. Sent the inQuire Dynamic
*                     modification of SeGment Attributes entry-point to
*                     statement 1111 to just call GKQWK.
*                     Called GK4TWV at the start of GK4TCL to ensure that
*                     the whole screen is cleared, not just the current
*                     TEK viewport (C51).
*     08/05/91  KEVP  In inQuire Dynamic modifi'ion of WorKstation Attr's
*                     entry point return colour representation as
*                     immediate (C84).
*     10/05/91  KEVP  Set Update WorKstation entrypoint to Refuse,
*                     even if hardware segments are in use (C83).
*     13/05/91  KEVP  In GK4TSH after statement 7777, made sure that
*                     the view(s) are renewed, only if regeneration is
*                     allowed (C82).
*     08/05/92  NMH   Fix bug C98 Pass KFWKT as first argument to
*                     GKIOOP instead of KWKTYP
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
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkinp.par'
      INCLUDE '../../include/gkfls.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkpca.par'
      INCLUDE '../../include/gwksgl.par'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkpca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkfls.cmn'
      INCLUDE '../../include/gkxfd.cmn'
      INCLUDE '../../include/gkpc.cmn'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkerr.cmn'
*
*  EXTERNALS
*  ---------
*     GK4TCS    Device cursor input routine
*     GK4TFA    Device fill area output routine
*     GK4TGI    Device prompt and input routine
*     GK4TLN    Device polyline output routine
*     GK4TRO    Device raster output routine (economy version)
*     GK4TSC    Device routine for setting cell array colour index
*     GK4TXT    Device text output routine
*     GK4TXF    Device text details routine
*
      EXTERNAL GK4TCS, GK4TFA, GK4TGI, GK4TLN, GK4TRO, GK4TSC
      EXTERNAL GK4TXF, GK4TXT, GK4TPP
*
*  LOCALS
*  ------
*     Offsets in KWKDAT:
*     IBAUD =1    Baud rate for this terminal
*     IDSIZE=2    Display Size of terminal  (in ..CT & ..LI)
*     INFAIS=3    Fill Area Style (in ..FA & ..FL)
*     IPPAT =4    Current panel pattern index (in ..SC & ..SI)
*     ICLCOL=5    Current colour index for device polyline
*                                         (in ..RO, ..SI & ..SP)
*     ICTCOL=6    Current colour index for device text (in ..SP)
*     IGSGOP=7    Name of open GKS segment displayed by a
*                 hardware segment, 0 if no such segment
*                                        (in ..CC, ..CT  & ..RS)
*     ISGUSE=8    Segment use indicator (hardware or software)
*     IOPSGT=9    Indicates whether the open segment has been
*                 transformed.                (in ..RS )
*     IBITPL=10   Number of bit-planes used by the workstation (in ..RO)
*
*     Offset in QWKDAT:
*     IDCTSC=1    Conversion factor of DC to TSC
*
*     The parameter ICLCOL is trapped in the
*     device raster routine GK4TRO and
*     can not be transmitted through subroutine arguments
*     The same applies to IGSGOP in GK4TLN & GK4TRS
*     and IPPAT in GK4TSC.
*
      INTEGER IBAUD, IDSIZE, INFAIS,  ICLCOL, ICTCOL
      INTEGER IGSGOP, ISGUSE, IOPSGT, IDCTSC, IBITPL
      PARAMETER (IBAUD=1, IDSIZE=2, INFAIS=3)
      PARAMETER (ICLCOL=5, ICTCOL=6)
      PARAMETER (IGSGOP=7, ISGUSE=8, IOPSGT=9, IBITPL=10)
      PARAMETER (IDCTSC=1)
*
*     ANGLE  Text angle
*     BCOLOR Buffer for colour vector component
*     DCX,DCY  Cell array rectangle corners in DC
*     FORMT  Variable format for interpreting valuator string
*     GARC   GDP parameter for arc
*     HX,HY  Cell height vector
*     HCHAR  Character height
*     HCRIT  Critical cell height for selecting cell array algorithm
*     HSMALL a negiligible value for H1 or H2
*     I      Various uses
*     ICHUNK Number of points in each chunk of output primitive
*     ICODE  Key hit returned with cursor position
*     ICOLOR RGB colour vector (range 0 to 100)
*     ICTLZ  ASCII code for ctrl/Z
*     IDVO   Input device option
*     INDEV  GIN device code
*     IGO    Variable for goto statement
*     INTA   Local integer array with multiple uses
*              - receiving input device state (size 10 max)
*              - receiving WDT info for Inq Text Facil (size 19)
*     INTXFP No. of font/precision pairs
*     IOFF   Stack offset for output primitive chunks
*   IOFFX,IOFFY Offsets for stack holding coordinates of initial stroke
*     IFONTN Stores available font numbers
*     IPREC  Stores available text precisions
*     IPRMST Integer array containing string prompt-start
*     ITXF   Hardware fonts
*     ITXP   Hardware precisions
*     IX,IY  Point in terminal space coordinates
*     JC     Component index for RGB colour vector
*     JCSS   Segments displayed from Central Segment Store
*     JHAB   Hardware segments to be abandoned
*     JHARDW Segments displayed from hardware segments
*     LSGDIR Length of segment directory entries
*     LSIMUL Used to indicate whether linetype simulation is required.
*     N, NN  Various
*     NINSEG Number of hardware segments reserved for input
*     NIPTS2 Number of points in initial stroke after checking
*     NLEFT  Returned by output buffering routine (amount of buffer left
*     NOUT   Number of bytes returned on input
*     NSEG   Segment number for Pick
*     NUMLS  Number of hardware linestyles
*     PI     PI for radians to degrees
*     PROMPT The prompt preceding input implemented by keyboard
*     REALA  Local real array with multiple uses
*              - receiving input device state (size 7 max)
*              - receiving WDT info for Inq Text Facil (size 12)
*     SPACE  Character spacing for string precision text
*     TRNSF  Coordinate transformation matrix
*     TEKCOL Parameter for converting colour vectors for TEK
*     VALSTR String of characters representing valuator input
*     WCHAR  Character width
*     WCX,WCY  Cell array rectangle corners in WC
*     WX,WY   Cell width vector
*     XDC,YDC 2-element arrays used to hold single locator value in DC
*            or part of stroke in DC (previous and current point)
*
*     NUMINT  Array with number of integers in input device directory
*             entries
*     NUMREA  Same but for real entries
*
      INTEGER NUMINT(6), NUMREA(6), IPRMST(8)
      INTEGER IDVO, INDEV(3)

      INTEGER    ICHUNK, ICTLZ, INTXFP, NINSEG
      PARAMETER (ICHUNK=200, ICTLZ=26, INTXFP=2, NINSEG=10)
*     TEK Commands
      INTEGER IRENEW(4), IDELSG(4), ICLOSG(3), IREOPS(8)
      INTEGER IRUBB(6), ISTART(10), IDIAVI(4), IPMODE(6)
      INTEGER IANSI(4)

      INTEGER IOFF, IOFFX,IOFFY,NOUT, I, N, NN,
     :    NLEFT, ICODE, NIPTS2, NPSEUF, LTS
      INTEGER INTA(19),IPREC(9),IFONTN(9),ITXP(INTXFP),ITXF(INTXFP)
      INTEGER IGO,JC,ICOLOR(3),LSGDIR,IX,IY
      REAL REALA(12), XDC(2),YDC(2),PI,BCOLOR,TEKCOL,TRNSF(3,2)
      CHARACTER VALSTR*12, PROMPT*80, FORMT*8
      REAL WX,WY,HX,HY,HCRIT,HSMALL, WCX(4),WCY(4), DCX(4),DCY(4)
      PARAMETER (PI=3.14159, HCRIT=7.0, HSMALL=0.0001)
      LOGICAL  LSIMUL
      PARAMETER (NPSEUF=8, TEKCOL=100.999, LSGDIR=3, LTS=4095)
      INTEGER JCSS, JHAB, JHARDW
      PARAMETER(JCSS=0, JHAB=1, JHARDW=2)
      INTEGER NUMLS, GARC
      PARAMETER (NUMLS=5, GARC=-1)
*     text
      INTEGER ICHX
      REAL ANGLE,HCHAR,WCHAR,SPACE
      REAL RCHHX,RCHHY,RCHWX,RCHWY,RCHRX,RCHRY
      DATA ITXF /1,1/,
     :     ITXP /GSTRP,GCHARP/

      DATA INDEV /0,64,8/
      DATA NUMINT/5,10,4,6,9,9/
      DATA NUMREA/6, 4,7,4,4,4/
      DATA IPRMST/83,116,114,105,110,103,63,58/

      DATA IFONTN /-101,-102,-103,-104,-105,-106,-107,-110,-115/
*
*     TEK Commands:-
*
*     IANSI   Select ANSI Mode (leave TEK mode)
*     ICLOSG  Close segment
*     IDELSG  Delete segment
*     IDIAVI  Set dialog area visible
*     IRENEW  Renew all Views (clears screan of all non-sements)
*     IRUBB   Set GIN rubberbanding
*     ISTART  Set GIN display start point
*     IPMODE  Set Fill Panel Mode
*     IREOPS  Insert into segment (at end)
*
      DATA IRENEW /27,75,78,33/
      DATA IDELSG /27,83,75,33/
      DATA IRUBB  /27,73,82,3*0/
      DATA ISTART /27,73,88,7*0/
      DATA IANSI  /27,37,33,49/
      DATA IDIAVI /27,76,86,49/
      DATA IPMODE /27,77,83,48,0,49/
      DATA ICLOSG /27,83,67/
      DATA IREOPS /27,85,73,5*0/
*
*  STACK USAGE
*  -----------
*     POLYLINE and POLYMARKER for transformations
*
*  ERRORS
*  ------
*      32   Specified workstation is not of category MO
*      34   Specified workstation is not of category MI
*      83   Fill area interior style not supported
*      90   Pattern not supported
*      93   Colour index is invalid
*     104   Cannot generate GDP
*     180   Specified function is not supported

*  ALGORITHM
*  ---------
*
*     TEK Commands:-
*
*     IRENEW     esc k n -1     Renew all views
*     IDELSG     esc s k -1     Delete all segments
*     IRUBB      esc i r - -    Set GIN rubberbanding
*     ISTART     esc i x -      Set GIN start point
*     ICLOSG     esc s c        Close hardware segment
*     IREOPS     esc u i - -1 1 Reopen hardware segment
*     IANSI      esc % ! 1      Select ANSI mode
*
*     Workstations 252 to 255 only
*     IPMODE     esc m s 0 - 1  Set Fill Panel Mode
*
*
*
*  COMMENTS
*  --------
*     To help you locate entry-points quickly,
*     the GKWKE parameter name has been put in
*     a comment at the start of each entry point
*     For example: To find FILL AREA entry point
*                  search for string 'KFA' in brackets ().
*
*     Device Coordinates are surface/pixel coordinates
*                        not terminal space coordinates
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
     :      1111,1410,1420,1111,1111,1111,1460,1111,1111,1111,
     :      1111,1111,1111,9999,9999,9999,9999,9999,9999,9999,
     :      9999,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1111,1111,1111,1730,1111,1111,1111,1111,1111,1790,
     :      1111,1111,1111,1111,1840,1850,1111,1870,1880,1111,
     :      1111,1111,1111,1111,1111,1111,1960,1111,1111) IENT-119

      GOTO 9999


* -------------------------
* OPEN WORKSTATION  (KOPWK)
* -------------------------
   10 CONTINUE
* Set up workstation state list and workstation description table
      CALL GKIWSL(KWKIX,KWKTYP)
      IF (KERROR.EQ.0) THEN
        KCID(KWKIX) = KWI1
        KDFM(KWKIX) = GBNIG
        KWIO(KWKIX) = GNO
        KIMRGM(KWKIX) = GSUPPD
* GKIWSO (which is called by GKIWSL) subtracts 1.0 from the upper
* limits of the workstation viewport. This is wrong.
* Instead an infinitessimal quantity should be subtracted,
* to ensure that the DC for NDC=1.0 round down to the
* last row or column of pixels.
* If anythying greater is subtracted,
* then the last row and column of pixels are not adequately utilised.
* However until the utilities are corrected,
* the limits are reset here and the infinitessimal quantity
* is subtracted when the normalisation transformation is set.
      QRWVXR(KWKIX) = QDSDX(KWKIX)
      QRWVYT(KWKIX) = QDSDY(KWKIX)
      QCWVXR(KWKIX) = QDSDX(KWKIX)
      QCWVYT(KWKIX) = QDSDY(KWKIX)
*     End of temporary commands
* Ask operating system to make a connection
        CALL GKIOOP(KFWKT,KCID(KWKIX),KWCID(KWKIX))
        IF (KERROR.EQ.0) THEN
* Get terminal speed
          CALL GKGTSP(KWCID(KWKIX),KWKDAT(IBAUD,KWKIX))

* Set up flags to be used elsewhere, to differentiate between different
*   types of workstation
*
*   set flags to indicate size of terminal display,
*   the number of bit-planes and the
*   conversion factor for DC to TSC.
          IF (KWKTYP .EQ. 250) THEN
              KWKDAT (IDSIZE,KWKIX) = 0
              KWKDAT (IBITPL,KWKIX) = 4
              QWKDAT (IDCTSC,KWKIX) = 8.0
          ELSEIF(KWKTYP .EQ. 251)THEN
              KWKDAT (IDSIZE,KWKIX) = 1
              KWKDAT (IBITPL,KWKIX) = 4
              QWKDAT (IDCTSC,KWKIX) = 6.0
          ELSEIF(KWKTYP .EQ. 252)THEN
              KWKDAT (IDSIZE,KWKIX) = 2
              KWKDAT (IBITPL,KWKIX) = 8
              QWKDAT (IDCTSC,KWKIX) = 4.0
          ELSEIF(KWKTYP .EQ. 253)THEN
              KWKDAT (IDSIZE,KWKIX) = 3
              KWKDAT (IBITPL,KWKIX) = 4
              QWKDAT (IDCTSC,KWKIX) = 3.0
          ELSEIF(KWKTYP .EQ. 254)THEN
              KWKDAT (IDSIZE,KWKIX) = 3
              KWKDAT (IBITPL,KWKIX) = 8
              QWKDAT (IDCTSC,KWKIX) = 3.0
          ELSEIF(KWKTYP .EQ. 255)THEN
              KWKDAT (IDSIZE,KWKIX) = 3
              KWKDAT (IBITPL,KWKIX) = 12
              QWKDAT (IDCTSC,KWKIX) = 3.0
          ENDIF

* Initialise output buffering
          CALL GKIOBO(KIOIT,1,KDAT,NLEFT)
*         GKIOBO with KIOBB & KIOEB would be here if required
* Initialise device
          CALL GK4TID (NPSEUF)
* Obtain and set up default colour table
          DO 15 I=0,KPCI(KWKIX)-1
             DO 14 JC=1,3
               BCOLOR = QHP(KHPXR(KCTBPT(JC,KWKIX))+I)
               ICOLOR(JC) = INT(TEKCOL*BCOLOR)
   14        CONTINUE
             CALL GK4TPC (I,ICOLOR(1),ICOLOR(2),ICOLOR(3))
   15     CONTINUE
          KWKDAT(ICLCOL,KWKIX) = -1
          KWKDAT(ICTCOL,KWKIX) = -1
* Erase screen
          CALL GK4TCL (KWKDAT(IBAUD,KWKIX))
* Initialise variables for linetype simulation
          QWOLDX(KWKIX) = -99.0
          QWOLDY(KWKIX) = -99.0
* Initialise segment indicators
          KWKDAT (IGSGOP, KWKIX) = 0
          KWKDAT (ISGUSE, KWKIX) = JHARDW
          KWKDAT (IOPSGT, KWKIX) = 0
        ENDIF
      ENDIF
      KWI1 = GOUTIN
      GOTO 9999

*  -------------------------
*  CLOSE WORKSTATION (KCLWK)
*  -------------------------
   20 CONTINUE
      IF( KWI1.EQ.1 ) THEN
        KWDONE=KRFUSE
      ELSE
*
*       Set dialog area visible
        CALL GKIOBO(KIOPB,4,IDIAVI,NLEFT)
*       Select ANSI mode
        CALL GKIOBO(KIOPB,4,IANSI ,NLEFT)
*       Send output buffer, appending the end bytes
        CALL GKIOBO(KIOSN,1,KDAT,NLEFT)
*       Disconnect terminal
        CALL GKIOCL(KWKTYP,KCID(KWKIX),KWCID(KWKIX))
*       Delete Workstation State List
        CALL GKCWSL(KWKIX)
*       and segment list
        CALL GKSLDL(KSSGPT(KWKIX))
      ENDIF
      GOTO 9999

* --------------------------
* CLEAR WORKSTATION (KCLRWK)
* --------------------------
   30 CONTINUE
      IF( KWI1.EQ.2 ) THEN
        KWDONE=KRFUSE
        IF (KWKDAT(ISGUSE,KWKIX) .EQ. JHARDW) THEN
           CALL GKIOBO(KIOPB,4,IDELSG,NLEFT)
           CALL GKIOBO(KIOPB,4,IRENEW,NLEFT)
        ENDIF
        CALL GKSLDL(KSSGPT(KWKIX))
        KWKDAT(ISGUSE,KWKIX) = JHARDW
      ENDIF
      GOTO 9999

* -------------------------------------------
* REDRAW ALL SEGMENTS ON WORKSTATION (KRSGWK)
* -------------------------------------------
   40 CONTINUE
      IF (KWKDAT(ISGUSE,KWKIX) .EQ. JHARDW) THEN
         CALL GKIOBO(KIOPB,4,IRENEW,NLEFT)
*        Send output buffer, appending the end bytes
         CALL GKIOBO(KIOSN,1,KDAT,NLEFT)
         KWDONE = KACEPT
      ELSE
         KWDONE=KRFUSE
      ENDIF
      GOTO 9999

* -------------------------
* UPDATE WORKSTATION (KUWK)
* -------------------------
   50 CONTINUE
      KWDONE=KRFUSE
      GOTO 9999

* -------------------------
* SET DEFERRAL STATE (KSDS)
* -------------------------
   60 CONTINUE
      KDFM(KWKIX) = KWI1
      KIMRGM(KWKIX) = KWI2
      IF (KWI1.EQ.GASAP) THEN
        KWIO(KWKIX) = GYES
*       Send output buffer, appending the end bytes
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

*----------------------------------
* DO DEFERRED OUTPUT ACTIONS (KDDA)
*----------------------------------
   70 CONTINUE
*     Send output buffer, appending the end bytes
      CALL GKIOBO(KIOSN,1,KDAT,NLEFT)
      KWI1 = KNFAUP(KWKIX)
      GOTO 9999

* -----------------------------
* CLEAR DISPLAY SURFACE (KCLDS)
* -----------------------------
   80 CONTINUE
      IF((KWI1.EQ.GALWAY).OR.(KDSMT(KWKIX).EQ.GNEMPT))THEN
           CALL GK4TCL (KWKDAT(IBAUD,KWKIX))
           CALL GKWCLD
      ENDIF
      GOTO 9999

* --------------
* MESSAGE (KMSG)
* --------------
  100 CONTINUE
*     Message is delivered via the integer data array argument
*     of the workstation driver
      CALL GK4TME(NID,IDAT)
      GOTO 9999

* -------------
* ESCAPE (KESC)
* -------------
  110 CONTINUE
      KERROR = 180
      GOTO 9999

* --------------
* POLYLINE (KPL)
* --------------
  120 CONTINUE
      IF ((KWKDAT(ISGUSE,KWKIX) .EQ. JCSS) .AND.
     :    (KCVIS .EQ. GINVIS))  GOTO 9999
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN
*       select pen from polyline colour index
        CALL GK4TSP(ICLCOL,KWPLCI(KWKIX))
*       Select linetype and linewidth (if possible)
        CALL GK4TSL(KWLNTY(KWKIX),LSIMUL)
        IF(KWKDAT(IDSIZE,KWKIX) .GE. 2)CALL GK4TSW(QWLNWD)
* Break Polyline into chunks. Convert chunk to device coordinates.
* Store converted chunk in stack.
* Execute TEK Polyline within polyline utility.
        N = ICHUNK
        DO 122 I=1,NRD,ICHUNK-1
          IF (NRD-I.LT.ICHUNK-1) N = NRD-I + 1
          IF (N.EQ.1) GOTO 122
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
          CALL GKLCLP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),LSIMUL,
     :              25.0,QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK4TLN)
  122   CONTINUE
        CALL GKSTDA(KREALS,IOFF)
      ENDIF
      GOTO 8888

* ----------------
* POLYMARKER (KPM)
* ----------------
  130 CONTINUE
      IF ((KWKDAT(ISGUSE,KWKIX) .EQ. JCSS) .AND.
     :    (KCVIS .EQ. GINVIS))  GOTO 9999
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN
*     select pen from polymarker colour index
        CALL GK4TSP(ICLCOL,KWPMCI(KWKIX))
* Select linetype 1 - solid
        CALL GK4TSL(1,LSIMUL)
* Ensure unit linewidth
        IF(KWKDAT(IDSIZE,KWKIX) .GE. 2)CALL GK4TSW(1.0)
* Break Polymarker into chunks. Convert chunk to device coordinates.
* Store converted chunk in stack.
* Execute TEK Polyline within polymarker utility
        N = ICHUNK
        DO 132 I=1,NRD,ICHUNK
          IF (NRD-I.LT.ICHUNK) N = NRD-I + 1
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
          CALL GKMTYP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),
     :                   KWMKTY(KWKIX),QNMMKS(KWKIX),QWMKSZ(KWKIX),
     :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK4TLN)
  132   CONTINUE
        CALL GKSTDA(KREALS,IOFF)
      ENDIF
      GOTO 8888

* ----------
* TEXT (KTX)
* ----------
 140  CONTINUE
      IF ((KWKDAT(ISGUSE,KWKIX) .EQ. JCSS) .AND.
     :    (KCVIS .EQ. GINVIS))  GOTO 9999
*  -- stroke precision
      IF(KWTXPR(KWKIX).EQ.GSTRKP) THEN
*        select pen from text colour index
         CALL GK4TSP(ICLCOL,KWTXCI(KWKIX))
*        solid lines unit linewidth
         CALL GK4TSL(1,LSIMUL)
         IF(KWKDAT(IDSIZE,KWKIX) .GE. 2)CALL GK4TSW(1.0)
         CALL GKXDWO(NID,IDAT,GK4TLN)
*  --  char precision
      ELSEIF(KWTXPR(KWKIX).EQ.GCHARP) THEN
*        select pen from text colour index
         CALL GK4TSP(ICTCOL,KWTXCI(KWKIX))
         CALL GKXDCS(NID,IDAT,
     :                 QWCHRX(KWKIX),QWCHRY(KWKIX),
     :                 GK4TXF,GK4TXT)
*  --  string precision
      ELSE
         CALL GK4TSP(ICTCOL,KWTXCI(KWKIX))
         CALL GKXDCS(NID,IDAT,
     :                 QWCHRX(KWKIX),QWCHRY(KWKIX),
     :                 GK4TXF,GK4TXT)
      ENDIF
      GOTO 8888

* ---------------
* FILL AREA (KFA)
* ---------------
  150 CONTINUE
      IF ((KWKDAT(ISGUSE,KWKIX) .EQ. JCSS) .AND.
     :    (KCVIS .EQ. GINVIS))  GOTO 9999
*     select pen from Fill Area colour index
      CALL GK4TSI(KWFAIS(KWKIX),KWFACI(KWKIX),KWFASI(KWKIX))
*     solid lines unit linewidth
      CALL GK4TSL(1,LSIMUL)
      IF(KWKDAT(IDSIZE,KWKIX) .GE. 2)CALL GK4TSW(1.0)
*     Fill Area
      KWKDAT (INFAIS,KWKIX) = KWFAIS(KWKIX)
      CALL GK4TFL(NRD,RX,RY)
      GOTO 8888

* ----------------
* CELL ARRAY (KCA)
* ----------------
  160 CONTINUE
      IF ((KWKDAT(ISGUSE,KWKIX) .EQ. JCSS) .AND.
     :    (KCVIS .EQ. GINVIS))  GOTO 9999
*
*     Select cell array algorithm
*       The panel based algorithm
*       is used if any of the following apply
*          1: A GKS segment is open and hardware segments are in use
*          2: The cell rectangle is sloping
*          3: The cell height is large
*       otherwise a polyline based algorithm is used.

*     Get cell array details
      CALL GKCELT(DCX,DCY,TRNSF)
      WX = ABS(TRNSF(1,1))
      WY = ABS(TRNSF(1,2))
      HX = ABS(TRNSF(2,1))
      HY = ABS(TRNSF(2,2))

*     Pick echoplay is done with a simple rectangle
      IF (KPKECO .EQ. KPECHO) GOTO 163
*     Is segment displayed by hardware segment open?
      IF (KWKDAT (IGSGOP, KWKIX) .GT. 0) GOTO 162
*     Is Cell array Horizontal-Vertical?
      IF(((HX .GT. HSMALL) .OR. (WY .GT. HSMALL)) .AND.
     :   ((WX .GT. HSMALL) .OR. (HY .GT. HSMALL))) GOTO 162
*     Is Cell extent in Y direction not small?
      IF((WY .GT. HCRIT) .OR. (HY .GT. HCRIT)) GOTO 162

*     Cell Array by Hardware Pixel Operations
  161 CONTINUE
      CALL GKCELL(NID,IDAT,GK4TRO)
      GOTO 8888

*     Cell Array by hardware panels with edges included
  162 CONTINUE
      KWKDAT(INFAIS,KWKIX) = GSOLID
      IPMODE(5) = 49
      CALL GKIOBO (KIOPB,6,IPMODE,NLEFT)
      CALL GKCELA(NID,IDAT,GK4TFA,GK4TSC,DCX,DCY,TRNSF,.TRUE.)
      IPMODE(5) = 48
      CALL GKIOBO (KIOPB,6,IPMODE,NLEFT)
      GOTO 8888

*     Cell Array echoplay
  163 CONTINUE
      CALL GK4TSI (GSOLID,2,1)
      KWKDAT (INFAIS,KWKIX) = GSOLID
      WCX(1) = QWR1
      WCY(1) = QWR2
      WCX(2) = QWR5
      WCY(2) = QWR6
      WCX(3) = QWR3
      WCY(3) = QWR4
      WCX(4) = WCX(3) - WCX(2) + WCX(1)
      WCY(4) = WCY(3) - WCY(2) + WCY(1)
      CALL GK4TFL (4,WCX,WCY)
      GOTO 8888

* ------------------------------------
* GENERALISED DRAWING PRIMITIVE (KGDP)
* ------------------------------------
 170  CONTINUE
      IF ((KWKDAT(ISGUSE,KWKIX) .EQ. JCSS) .AND.
     :    (KCVIS .EQ. GINVIS))  GOTO 9999
*
      IGO = - KWI1

      GOTO (171,172,172,172) IGO
      IF (KWI1 .LT. 1) THEN
        KERROR = 102
      ELSE
        KERROR = 104
      ENDIF
      GOTO 9999

  171 CONTINUE
*     -- Set line attributes
      CALL GK4TSP(ICLCOL,KWPLCI(KWKIX))
      CALL GK4TSL(KWLNTY(KWKIX),LSIMUL)
      IF(KWKDAT(IDSIZE,KWKIX) .GE. 2)CALL GK4TSW(QWLNWD)
*     -- Draw Arc GDP
      CALL GK4TCC(-1,NRD,RX,RY,LSIMUL,10.0,GK4TLN)
      GOTO 8888

  172 CONTINUE
*     -- Set line attributes
      CALL GK4TSI(KWFAIS(KWKIX),KWFACI(KWKIX),KWFASI(KWKIX))
      CALL GK4TSL(1,LSIMUL)
      IF(KWKDAT(IDSIZE,KWKIX) .GE. 2)CALL GK4TSW(1.0)
*     -- Draw Filled GDP
      IF(KWFAIS(KWKIX) .EQ. GHATCH)THEN
        CALL GKCRCS(KWI1,NRD,RX,RY,1,.FALSE.,10.0,GK4TLN,GK4TRO)
      ELSEIF(KWFAIS(KWKIX) .EQ. GPATTR)THEN
        CALL GKCRCS(KWI1,NRD,RX,RY,1,.FALSE.,10.0,GK4TLN,GK4TRO)
*       Can't use hardware segments anymore .. set to abandon them
        IF(KWKDAT(ISGUSE,KWKIX) .EQ. JHARDW)
     :     KWKDAT(ISGUSE,KWKIX) = JHAB
      ELSE
        KWKDAT (INFAIS,KWKIX) = KWFAIS(KWKIX)
        CALL GK4TCC(KWI1,NRD,RX,RY,.FALSE.,10.0,GK4TLN)
      ENDIF
      GOTO 8888

* -------------------------------
* SET POLYLINE ATTRIBUTES (KSPLA)
* -------------------------------
  180 CONTINUE
      CALL GKDPLB
* Need to check because individual settings won't have been checked.
      IF (KWLNTY(KWKIX).GT.NUMLS) KWLNTY(KWKIX) = 1
      IF (KWPLCI(KWKIX).GE.KPCI(KWKIX)) KWPLCI(KWKIX) = 1
      GOTO 9999

* ---------------------------------
* SET POLYMARKER ATTRIBUTES (KSPMA)
* ---------------------------------
  190 CONTINUE
      CALL GKDPMB
* Need to check because individual settings won't have been checked.
      IF (KWMKTY(KWKIX).GT.5) KWMKTY(KWKIX) = 3
      IF (KWPMCI(KWKIX).GE.KPCI(KWKIX)) KWPMCI(KWKIX) = 1
      GOTO 9999

* ---------------------------
* SET TEXT ATTRIBUTES (KSTXA)
* ---------------------------
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
      HCHAR=SQRT(RCHHX*RCHHX+RCHHY*RCHHY)
*     character width
      RCHWX=QWCHWX(KWKIX)
      RCHWY=QWCHWY(KWKIX)
      WCHAR=SQRT(RCHWX*RCHWX+RCHWY*RCHWY)
*     Make hardware characters narrower
*    (more like default stroke precision font)
      WCHAR = 0.6*WCHAR
*     Put in Character Expansion
      WCHAR = QWCHXP(KWKIX)*WCHAR
*     character rotation
      RCHRX=QWCHWX(KWKIX)
      RCHRY=QWCHWY(KWKIX)
      ANGLE=FLOAT(MOD(NINT(180.0*ATAN2(RCHRY,RCHRX)/PI)+360,360))
*     set hardware text attributes
      IF (KWTXPR(KWKIX) .EQ. GCHARP) THEN
*     character precision:
        CALL GK4TST (WCHAR,HCHAR,ANGLE,0.0,-1)
      ELSE
*     string precision:
*       character spacing - add normal spacing
*                         - enforce minimum size (as in WDT)
        SPACE = QWCHSP(KWKIX) + 0.3
        IF(HCHAR .LT. QMNCHH(KWKIX))THEN
           WCHAR = WCHAR*QMNCHH(KWKIX)/HCHAR
           HCHAR = QMNCHH(KWKIX)
        ENDIF
        CALL GK4TST (WCHAR,HCHAR,ANGLE,SPACE,KWTXP(KWKIX))
      ENDIF
*     realised values - used by GK4TXT,GK4TXF in device units
      KWTXFI(KWKIX)=ICHX
      KWCHHT(KWKIX)=NINT(HCHAR)
      KWCHWD(KWKIX)=NINT(WCHAR + 0.3*HCHAR)
      QWCHRX(KWKIX)=RCHRX
      QWCHRY(KWKIX)=RCHRY
      GOTO 9999

* --------------------------------
* SET FILL AREA ATTRIBUTES (KSFAA)
* --------------------------------
  210 CONTINUE
      CALL GKDFAB
* Need to check because individual settings won't have been checked.
      IF (KWFAIS(KWKIX).EQ.GHATCH) THEN
        IF (KWFASI(KWKIX).GT.-1 .OR. KWFASI(KWKIX).LT.-10)
     :    KWFASI(KWKIX) = -1
      ENDIF
      IF (KWFAIS(KWKIX).EQ.GPATTR .AND. KWFASI(KWKIX).GT.16)
     :KWFASI(KWKIX) = 1
      IF (KWFACI(KWKIX).GE.KPCI(KWKIX)) KWFACI(KWKIX) = 1
      GOTO 9999

* ----------------------------
* SET PICK IDENTIFIER (KSPKID)
* ----------------------------
  220 CONTINUE
      GOTO 9999

* -----------------------------------
* SET POLYLINE REPRESENTATION (KSPLR)
* -----------------------------------
  230 CONTINUE
      INTA(1) = NUMLS
      CALL GKSRPL(1,INTA,.TRUE.)
      GOTO 9999

* -------------------------------------
* SET POLYMARKER REPRESENTATION (KSPMR)
* -------------------------------------
  240 CONTINUE
      CALL GKSRPM(0,INTA,.TRUE.)
      GOTO 9999

* -------------------------------
* SET TEXT REPRESENTATION (KSTXR)
* -------------------------------
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
          KERROR=-1009
          GOTO 9999
        ENDIF
        IF( KDBFLS.EQ.KFLCL ) CALL GKXON
        IF( KERROR.NE.0 ) GOTO 9999
        DO 255 I=1,9
          IPREC(I) = GSTRKP
  255   CONTINUE
        CALL GKSRTX(KFNTMX,IFONTN,IPREC,.FALSE.)
      ELSE

*       String or Char precision
        IPREC(1)=KWI3
        INTA(1)=1
        CALL GKSRTX(1,INTA,IPREC,.FALSE.)
      ENDIF
      GOTO 9999

* ------------------------------------
* SET FILL AREA REPRESENTATION (KSFAR)
* ------------------------------------
  260 CONTINUE
      CALL GKSRFA(.FALSE.)
*     Render pattern acceptable if in range
      IF(KERROR .EQ. 85)THEN
        IF((KWI3 .GT. 0)
     :   .AND.(KWI3 .LE. KMXPAB(KWKIX)))KERROR = 0
      ENDIF
      GOTO 9999

* ----------------------------------
* SET PATTERN REPRESENTATION (KSPAR)
* ----------------------------------
  270 CONTINUE
      CALL GKSRPA(NID,IDAT)
      GOTO 9999

* --------------------------------
* SET COLOUR REPRESENTATION (KSCR)
* --------------------------------
  280 CONTINUE
* See if colour index valid
      IF (KWI1.GE.0 .AND. KWI1.LT.KPCI(KWKIX)) THEN
        QHP(KHPXR(KCTBPT(1,KWKIX))+KWI1) = QWR1
        QHP(KHPXR(KCTBPT(2,KWKIX))+KWI1) = QWR2
        QHP(KHPXR(KCTBPT(3,KWKIX))+KWI1) = QWR3
        ICOLOR(1) = INT(QWR1*TEKCOL)
        ICOLOR(2) = INT(QWR2*TEKCOL)
        ICOLOR(3) = INT(QWR3*TEKCOL)
        CALL GK4TPC(KWI1,ICOLOR(1),ICOLOR(2),ICOLOR(3))
      ELSE
        KERROR = 93
      ENDIF
      GOTO 9999

* ----------------------------------
* NORMALISATION TRANSFORMATION (KNT)
* ----------------------------------
  310 CONTINUE
      CALL GKWKC4
*     Set hardware viewport and window
      CALL GK4TWV(QWCLXL(KWKIX),QWCLXR(KWKIX),
     :            QWCLYB(KWKIX),QWCLYT(KWKIX))
      GOTO 9999


* -------------------------------
* SET WORKSTATION WINDOW (KSWKWN)
* -------------------------------
  320 CONTINUE
      CALL GKSWKW
      CALL GKPPBU
      GOTO 9999

* ---------------------------------
* SET WORKSTATION VIEWPORT (KSWKVP)
* ---------------------------------
  330 CONTINUE
      CALL GKSWKV
      CALL GKPPBU

*     If the open segment is displayed by a hardware segment
*     and has been transformed, check that new viewport fits
*     inside the terminal space of the segment.
      IF(KWKDAT(IOPSGT,KWKIX) .EQ. 1)THEN
        DCX(1) = QWR1
        DCY(1) = QWR3
        DCX(2) = QWR1
        DCY(2) = QWR4
        DCX(3) = QWR2
        DCY(3) = QWR4
        DCX(4) = QWR2
        DCY(4) = QWR3
        CALL GK4TRS (4,DCX,DCY)
        DO 333 I=1,4
           CALL GK4TCT (DCX(I),DCY(I),1,IX,IY)
           IF((IX .LE. 0) .OR. (LTS .LE. IX))GOTO 335
           IF((IY .LE. 0) .OR. (LTS .LE. IY))GOTO 335
  333   CONTINUE
*       New viewport fits - carry on with hardware segments
        GOTO 339

  335   CONTINUE
*       Abandon Hardware segments and switch to CSS segments
*       because new viewport won't fit inside open segment.
        IDELSG(4) = 33
        CALL GKIOBO (KIOPB,4,IDELSG,NLEFT)
        KRPCC = KRPVIS
        KRGN  = .TRUE.
        KWRGN(KWKIX) = .TRUE.
        KWKDAT(ISGUSE,KWKIX) = JCSS
        KWKDAT(IGSGOP,KWKIX) = 0
        KWKDAT(IOPSGT,KWKIX) = 0
      ENDIF
  339 CONTINUE

      GOTO 9999

* ------------------- *
* SEGMENT ENTRYPOINTS *
* ------------------- *
  410 CONTINUE
*     CSS segments with bounding boxes
      CALL GKSGWB(IENT,.FALSE.)
*     Hardware segments
      IF(KWKDAT(ISGUSE,KWKIX) .EQ. JHARDW)THEN
        CALL GK4TSH(IENT,KWKDAT(IGSGOP,KWKIX),
     :              KWKDAT(ISGUSE,KWKIX),KWKDAT(IOPSGT,KWKIX))
        IF(KWKDAT(ISGUSE,KWKIX) .EQ. JHARDW)THEN
*       If hardware segments are displayed,
*         suppress all playbacks
*         of software segments.
          KRPCC = KRPNO
          KWRGN(KWKIX) = .FALSE.
        ENDIF
      ENDIF
      GOTO 9999

* ----------------- *
* INPUT ENTRYPOINTS *
* ----------------- *
* Initialise locator  (KINLC)
  610 CONTINUE
* First, check that the PET number is valid - only one supported
      IF (KWI2.EQ.1) THEN
        CALL GKINIP(GLOCAT,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        KERROR = 144
      ENDIF
      GOTO 9999

* Initialise stroke  (KINSK)
  620 CONTINUE
* First, check that the PET number is valid - only one supported
      IF (KWI2.EQ.1) THEN
        CALL GKINIP(GSTROK,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        KERROR = 144
      ENDIF
      GOTO 9999

* Initialise valuator  (KINVL)
  630 CONTINUE
* First, check that the PET number is valid - only one supported
      IF (KWI2.EQ.1) THEN
        CALL GKINIP(GVALUA,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        KERROR = 144
      ENDIF
      GOTO 9999

* Initialise choice  (KINCH)
  640 CONTINUE
* First, check that the PET number is valid - only one supported
      IF (KWI2.EQ.1) THEN
        CALL GKINIP(GCHOIC,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        KERROR = 144
      ENDIF
      GOTO 9999

* Initialise pick  (KINPK)
  650 CONTINUE
* First, check that the PET number is valid - only one supported
      IF (KWI2.EQ.1) THEN
        CALL GKINIP(GPICK,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        KERROR = 144
      ENDIF
      GOTO 9999

* Initialise string  (KINST)
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

* ------------------------
* Request locator  (KRQLC)
* ------------------------
  690 CONTINUE
*     Call request locator utility
      CALL GKRQLC (GK4TCS,RX(1),RY(1))
      GOTO 9999



* -----------------------
* Request stroke  (KRQSK)
* -----------------------
  700 CONTINUE
*   First, get stroke device information
*     | Routine GKISK allocates stack at IOFFX |
      CALL GKISK(NRD,KNRR,RX,RY, INTA,REALA, NIPTS2,
     :      IOFFX,IOFFY)
      IF( KERROR.NE.0 ) GOTO 709

*   Select device
*     KWI1 = 1  Keyboard/Joydisk
*          = 2  Mouse
*          = 3  Tablet (not valid on TEK 4205)
      IF(KWI1 .GT. 3)GOTO 709
      IF(KWI1 .GT. 2+KWKDAT(IDSIZE,KWKIX))GOTO 709
      IDVO = KWI1

*     Set GIN rubberbanding
      N = 4
      CALL GK4TTI (INDEV(IDVO),6,IRUBB,N)
      NN = N
      CALL GK4TTI(2,6,IRUBB,N)
      CALL GKIOBO(KIOPB,N-1,IRUBB,NLEFT)

*     If a GKS segment represented by a hardware segment is open,
*     close it, so that echoing is not drawn into it.
      IF(KWKDAT(IGSGOP,KWKIX) .NE. 0)
     :    CALL GKIOBO (KIOPB,3,ICLOSG,NLEFT)

*     Put GIN device code in 'set GIN start point' command
      N = 4
      CALL GK4TTI(INDEV(IDVO),10,ISTART,N)

*     In case there is just one point in initial stroke
      CALL GKTND (1,RX,RY,XDC(1),YDC(1))

*   Set line colour
      CALL GK4TSP(ICLCOL,KWPLCI(KWKIX))
*   Set line style solid and unit linewidth
      CALL GK4TSL(1,LSIMUL)
      IF(KWKDAT(IDSIZE,KWKIX) .GE. 2)CALL GK4TSW(1.0)
*   Draw initial stroke if there are any points and echo is set
      IF( NIPTS2.GE.2 .AND. INTA(KIPE).EQ.GECHO )
     :      CALL GK4TLN(NIPTS2,QSTACK(IOFFX),QSTACK(IOFFY))

*   Save last point. After that we have no further need of the stack.
      IF( NIPTS2.GE.2 ) THEN
        XDC(1)=QSTACK(IOFFX+NIPTS2-1)
        YDC(1)=QSTACK(IOFFY+NIPTS2-1)
      ENDIF
      CALL GKSTDA(KREALS,IOFFX)

*   Set initial cursor position to last point
      XDC(2) = XDC(1)
      YDC(2) = YDC(1)


*   Cycle once for each point in stroke
      DO 705 I=NIPTS2+1,NRD

*      Further loop to repeat input of point if outside wkstn window
  702   CONTINUE

*         Set GIN start point
          CALL GK4TCT (XDC(2),YDC(2),0,IX,IY)
          N = NN
          CALL GK4TTP(IX,IY,10,ISTART,N)
          CALL GKIOBO (KIOPB,N-1,ISTART,NLEFT)

*         Read key hit and cursor.
          CALL GK4TCS(ICODE, XDC(2),YDC(2))

*         Branch on key hit (ICODE).
*         If break (ctrl Z), then we quit with status=none
*         If normal terminator (ctrl key), then finish stroke, not
*         accepting this present point.
*         Otherwise inspect the point returned and
*            if OK go and get next one
*            if not OK try again (without progressing)
            IF(      ICODE.EQ.ICTLZ) THEN
*             Cancel Stroke
              KWI1=GNONE
              GOTO 708
            ELSE IF( ICODE.LT.32 ) THEN
*             Finish Stroke
              GOTO 706
            ELSE
              CALL GKTDN(1,XDC(2),YDC(2),RX(I),RY(I))
              IF( KERROR.NE.0 ) THEN
                  KERROR=0
                  GOTO 702
              ENDIF
              IF( I.GT.1 .AND. INTA(KIPE).EQ.GECHO )
     :              CALL GK4TLN(2,XDC,YDC)
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
*   Drop to 708

*   If a hardware segment has been closed to exclude echo from it,
*   reopen it.
      IF(KWKDAT(IGSGOP,KWKIX) .NE. 0)THEN
         N = 4
         CALL GK4TTI (KWKDAT(IGSGOP,KWKIX)+NINSEG,8,IREOPS,N)
         IREOPS(N)   = 33
         IREOPS(N+1) = 49
         CALL GKIOBO (KIOPB,N+1,IREOPS,NLEFT)
      ENDIF

*   Disable GIN rubberbanding
  708 CONTINUE
      N = NN
      CALL GK4TTI (0,6,IRUBB,N)
      CALL GKIOBO (KIOPB,N-1,IRUBB,NLEFT)
*   Drop to 709

  709 CONTINUE
      GOTO 9999

*----------------------------
* Request valuator    (KRQVL)
*----------------------------
  710 CONTINUE
*   First get valuator device information
      CALL GKRQIP(GVALUA,KWI1,NUMINT(GVALUA),NUMREA(GVALUA),
     :            INTA,REALA)

*   Display prompt in echo area and find out how much room there is
*   for input
      WRITE(PROMPT, '(A,G12.4,A,G12.4,A)' )
     :   'Value between ',REALA(KVLMNV),' and ',REALA(KVLMXV),' :'
      CALL GK4TGI(INTA,REALA,PROMPT(1:60),VALSTR,NOUT)

*   Interpret the incoming string. If not recognisable then set
*   status = none.
      IF( NOUT.LT.1 ) THEN
          KWI1=GNONE
          QWR1=QNIL
      ELSE
*       Convert to a real number
          FORMT = ' '
          IF(NOUT .LE. 9)THEN
             WRITE(FORMT, '(A,I1,A)')  '( F',NOUT,'.0)'
          ELSE
             WRITE(FORMT, '(A,I2,A)')  '( F',NOUT,'.0)'
          ENDIF
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
  719 CONTINUE

      GOTO 9999


* -----------------------
* Request choice  (KRQCH)
* -----------------------
  720 CONTINUE
*
*     Data expected:
*     KWI1   : Device Number
*
*     Data returned:
*     KWI1   : Status
*     KWI2   : Choice value

*   Call Choice Utility
      CALL GKRQCH (KWI1,KWI2,GK4TGI)

      GOTO 9999

* ---------------------
* Request pick  (KRQPK)
* ---------------------
  730 CONTINUE
      IF(KWI1 .EQ. 1)THEN
         CALL GKRQPK (4.0,GK4TPP,GK4TCS,GK4TXF)
      ELSE
         CALL GKRQPK (1.0,GK4TPP,GK4TCS,GK4TXF)
      ENDIF
*
*     If a GKS segment represented by a hardware segment is open,
*     close it, before echoplay so that echoplay is not drawn into it
*     and reopen it when finishing pick.
      IF(KWKDAT(IGSGOP,KWKIX) .NE. 0)THEN
        IF(KPECHO .EQ. KPKECO)THEN
           CALL GKIOBO (KIOPB,3,ICLOSG,NLEFT)
        ELSEIF(KPECHO .EQ. KNIL)THEN
           N = 4
           CALL GK4TTI (KWKDAT(IGSGOP,KWKIX)+NINSEG,8,IREOPS,N)
           IREOPS(N)   = 33
           IREOPS(N+1) = 49
           CALL GKIOBO (KIOPB,N+1,IREOPS,NLEFT)
        ENDIF
      ENDIF
      GOTO 9999

* -----------------------
* Request string  (KRQST)
* -----------------------
  740 CONTINUE
*
*     Data expected:
*     KWI1   : String device number
*
*     Data returned:
*     KWI1   : Status
*     KNIR   : Length of string
*     IDAT   : Integer array containing string
*
*     Call string input utility
      CALL GKRQST (KWI1,8,IPRMST,NID,KNIR,IDAT,GK4TGI)

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

* ----------------------------------- *
* METAFILE ENTRYPOINTS (NOT RELAVENT) *
* ----------------------------------- *
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


* ------------------- *
* ENQUIRY ENTRYPOINTS *
* ------------------- *
* Inquire everything *
 1111 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999

* Inquire polyline representation (KQPLR)
 1330 CONTINUE
      IF (KWI2.EQ.GSET) THEN
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
        IF(KWKDAT(IDSIZE,KWKIX) .GE. 2)THEN
          IF(NINT(QWR1) .GE. NINT(QMXLNW(KWKIX)))THEN
            QWR1 = QMXLNW(KWKIX)
          ELSEIF(NINT(QWR1) .LE. NINT(QMNLNW(KWKIX)))THEN
            QWR1 = QMNLNW(KWKIX)
          ENDIF
          QWR1 = FLOAT(NINT(QWR1))
        ELSE
          QWR1 = 1.0
        ENDIF
      ENDIF
      GOTO 9999



* Inquire text representation (KQTXR)
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
* Inquire text extent  (KQTXX)
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
     :                  RX,RY,GK4TXF)

      ENDIF
      GOTO 9999



* Inquire list of pattern indices (KQEPAI)
 1410 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999



* Inquire pattern representation (KQEPAR)
 1420 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999


* Inquire set members of segment names on workstation
 1460 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KSGRQ = KSGRQ + 1
      GOTO 9999


* Inquire dynamic modification of workstation attributes (KQDWKA)
 1730 CONTINUE
      KWI1 = GIRG
      KWI2 = GIRG
      KWI3 = GIRG
      KWI4 = GIRG
      KWI5 = GIRG
      KWI6 = GIMM
      KWI7 = GIRG
      GOTO 9999

* Inquire text facilities (KQTXF)
*           ... on entry KWI1 specifies list element requested
 1790 CONTINUE
*     Data sent:
*     KWI1   : list element N requested
*     Data returned:
*     KWI1   : number of text font and precision pairs
*     KWI2   : Nth element of list of text fonts
*     KWI3   : Nth element of list of text precisions
*     KWI4..6, QWR1..4 & KERROR are dealt by GKQWK

*     Save list original list element to allow shifting
      N = KWI1
* Allow for string and char precision font (number 1) explicitly
      IF( N .LE. 2 ) THEN
         CALL GKQWK (IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
         KWI2 = 1
*       String or Char precision font
         IF(N .EQ. 1) KWI3 = GSTRP
         IF(N .EQ. 2) KWI3 = GCHARP
      ELSE
*       Stroke precision - shifting list element down 2
         KWI1 = KWI1 - 2
         CALL GKQWK (IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      ENDIF
*     Add the two string and char font-precision pairs to total
      KWI1 = KWI1 + 2
      GOTO 9999


* Inquire predefined pattern representation (KQPPAR)
 1840 CONTINUE
      GOTO 1111



* Inquire colour facilities (KQCF)
 1850 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KWI1 = KWI3
      KWI2 = GCOLOR
      GOTO 9999

* ---------------------------------------------------------------
* Inquire List Element of Available GDPs   (KQEGDP)
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
* Inquire Generalised Drawing Primitive  (KQGDP)
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
      IF (KWI1.GE.-4 .AND. KWI1.LE.-1) THEN
         KNIR = 1
         IDAT(1) = GFAATT
*        Arc
         IF (KWI1.EQ.GARC)  IDAT(1) = GPLATT
      ELSE
        KERROR = 41
      ENDIF
      GOTO 9999


* Inquire default choice device data  (KQDCH)
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

*   Here after all output primitives to sort out buffering
 8888 CONTINUE
*   all primitives must be output,
*   otherwise WRITE statements could interfere.
      KDSMT(KWKIX) = GNEMPT
      CALL GKIOBO(KIOSN,1,KDAT,NLEFT)

 9999 CONTINUE

      END
      SUBROUTINE GK4TCC(IOPT,NRD,RX,RY,
     :                  LSIMUL,SIMLEN,LINSUB)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  UTILITY
*  Author:             KEVP  (based on JGWs GKCRCS)
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To generate an arc/circle for GDP,
*     for the TEK 4200s
*
*  ARGUMENTS
*  ---------
*     INP   IOPT    Curve option
*                     -1: arc unstyled
*                     -2: arc (chord) with interior styled
*                     -3: arc (pie) with interior styled
*                     -4: circle with interior styled
*     INP   RX }    Co-ordinates of three points on arc in
*     INP   RY }    World Coordinates transformed by registation pts
*     INP   LSIMUL
*     INP   SIMLEN
*     INP   LINSUB  Line drawing function.
*
      INTEGER IOPT,NRD
      REAL     RX(3), RY(3), SIMLEN
      LOGICAL  LSIMUL
      EXTERNAL LINSUB
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*    Values of IOPT
*     JARC   = Plain Arc
*     JCHORD = Fillable Chord
*     JPIE   = Fillable Pie
*     JCIRC  = Fillable Circle

      INTEGER JARC,JCHORD,JPIE,JCIRC
      PARAMETER (JARC=-1, JCHORD=-2, JPIE=-3, JCIRC=-4)
*
*   Values of ISTAT
*     JENDS  = 0  Arc with end points
*     JFULL  = 1  Full Circle
*     JLIN   = 2  Line

      INTEGER    JENDS,   JFULL,   JLIN
      PARAMETER (JENDS=0, JFULL=1, JLIN=2)
*
*   Changing WC
*     AXFM       transform induced by registration points
*     BXFM       inverse AXFM (backtransform to WC)
*     TTCOPY     copy of w/s total transform (QWTOTT)
*
*   Others
*     ANGLE      Angle of turn for whole arc in degrees
*     ANGVEC     Degrees per vector
*     DXA,DYA    vector from A to CEN
*     FACTOR     multiplying factor to increase number of vectors
*                if hardware segment is open
*     HCURVE     True, if hardware curve is used.
*     IGSGOP     Offset in KWKDAT for name of open GKS segment
*                displayed by hardware segment (0, if no such segment)
*     ISTAT      status of arc 0=with ends, 1=full, 2=line
*     ITSX,ITSY  Terminal Space coords
*     LTS        Length of terminal space in TSC
*     NMAX       Maximum number of points in curve
*     NPTS       Number of points generating the arc (curve only)
*     NPTSA      Number of points generating the arc + any extra
*     RADIUS     Radius in WC
*     THETA      Angle of turn for whole arc in radians
*
*     XCEN,YCEN  centre of arc
*     XPTS,XPTS  copy of points transformed to take account of
*                registration points
*     XDC,YDC    Device Coordinates
*
      REAL TWOPI
      PARAMETER (TWOPI = 6.283185072)
      REAL    XCEN,YCEN, XPTS(3),YPTS(3), DXA,DYA
      REAL    RADIUS, FACTOR, THETA,ANGLE,ANGVEC
      INTEGER I,ISTAT,LTS,NPTS,NPTSA,NMAX
      INTEGER IOFFST,IOFFSX,IOFFSY,  IGSGOP
      PARAMETER (LTS = 4095,  IGSGOP=7)
      REAL    AXFM(3,2),BXFM(3,2),TTCOPY(6)
      REAL     XDC(3),YDC(3)
      INTEGER  ITSX(3),ITSY(3)
      LOGICAL  HCURVE
*
*  ALGORITHM
*  ---------
*     1.  Obtain details of transform
*     2.  Calculate the centre and radius
*     3.  Calculate the number of chords/vectors needed
*     4.  Determine whether hardware curve routine can be used.
*           If so use it, otherwise:
*     5.  Generate polygon in stack
*     6.  Draw outline & fill/pattern inside as appropriate
*
*     No TEK Commands
*
*  COMMENTS
*  --------
*    In number of points required for an arc
*    is calculated so that fewer points are used
*    but a good curve is still drawn.
*
*---------------------------------------------------------------------

*    Check that number of points is compatible
*    with option
      IF (.NOT.((NRD.EQ.3 .AND. IOPT.NE.JCIRC) .OR.
     :          (NRD.EQ.2 .AND. IOPT.EQ.JCIRC))
     :   )THEN
         KERROR = 100
         GOTO 9999
      ENDIF
*
*     Save original total segment transformation (ie WC to DC)
      DO 10 I=1,6
         TTCOPY(I) = QWTOTT(I,KWKIX)
   10 CONTINUE
*
*     Change world coords to take account of registration points.
*     (The original WC are restored at the end of the routine.)
      CALL GKMTDV(QWRA(1),QWRA(4),AXFM)
      CALL GKMTIV(AXFM,BXFM)
      CALL GKMTXF(BXFM,NRD,RX,RY,XPTS,YPTS)
      CALL GKMTML(AXFM,QWTOTT(1,KWKIX),QWTOTT(1,KWKIX))

*     Add third point if required
      IF(NRD .EQ. 2)THEN
         XPTS(3) = XPTS(1)
         YPTS(3) = YPTS(1)
      ENDIF

*     Find the centre of the circle of the arc
*     for the various options
*     together with the radius and angle substended to centre.
*     -- handle circle parameters --
      IF (IOPT.EQ.JCIRC) THEN
         XPTS(3) = XPTS(1)
         YPTS(3) = YPTS(1)
         DXA    = XPTS(2) - XPTS(1)
         DYA    = YPTS(2) - YPTS(1)
         XCEN   = XPTS(2)
         YCEN   = YPTS(2)
         XPTS(2) = XCEN + DXA
         YPTS(2) = YCEN + DYA
         RADIUS = SQRT(DXA*DXA + DYA*DYA)
         THETA  = TWOPI
         ISTAT  = JFULL
      ELSE
         CALL GKCRCE (XPTS,YPTS,XCEN,YCEN,RADIUS,THETA,ISTAT)
      ENDIF

*     Determine number of vectors required for arc
      IF(KWKDAT(IGSGOP,KWKIX) .EQ. 0)THEN
        FACTOR = 1.0
        NMAX   = 120
      ELSE
        FACTOR = 8.0
        NMAX   = 360
      ENDIF
      CALL GKCRCN (RADIUS,FACTOR*THETA,NMAX,NPTS,HCURVE)
*
*     Check, whether transformation is suitable for
*            hardware curve routine

*     Firstly - Hardware curves are circular arcs in DC
*               hence can only be used if a circle in WC
*               is also a circle in DC.
*               This has already been done by GKCRCN
*
*     Secondly -  All the defining points must be within the
*                 terminal space for hardware curves to be used.

      CALL GKTWD  (3,XPTS,YPTS,XDC,YDC)
      CALL GK4TRS (3,XDC,YDC)
      DO 5 I=1,3
        CALL GK4TCT (XDC(I),YDC(I),1,ITSX(I),ITSY(I))
        HCURVE = HCURVE .AND. (ITSX(I) .GE. 0)
        HCURVE = HCURVE .AND. (ITSX(I) .LE. LTS)
        HCURVE = HCURVE .AND. (ITSY(I) .GE. 0)
        HCURVE = HCURVE .AND. (ITSY(I) .LE. LTS)
        IF(.NOT. HCURVE) GOTO 6
    5 CONTINUE

    6 CONTINUE

*     Use hardware curve if possible
      IF (HCURVE) THEN
          ANGLE = (360.0/TWOPI)*THETA
          ANGVEC = ANGLE/(NPTS-1)
          CALL GK4THC (IOPT,ITSX,ITSY,KWFAIS(KWKIX),ANGVEC)
         GOTO 8000
      ENDIF

*     Here software curve must be used. Points are calculated and
*     put into the stack. Extra pt added to stack if pie.
      IF((IOPT .EQ. JPIE) .AND. (ISTAT .EQ. JENDS))THEN
          NPTSA = NPTS+1
      ELSE
          NPTSA = NPTS
      ENDIF
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

*     Add in centre if pie, whose arc has ends
      IF(NPTSA .EQ. NPTS+1)THEN
          QSTACK(IOFFSX+NPTS) = XCEN
          QSTACK(IOFFSY+NPTS) = YCEN
      ENDIF

*     Output arc/circle/chord/pie using
*     fill area of polyline as appropiate
      IF (IOPT.EQ.JARC .OR. NPTS.EQ.2) THEN
         CALL GKMTXF(QWTOTT(1,KWKIX),NPTS,
     :                      QSTACK(IOFFSX),QSTACK(IOFFSY),
     :                      QSTACK(IOFFSX),QSTACK(IOFFSY))
         CALL GKLCLP(NPTS,QSTACK(IOFFSX),QSTACK(IOFFSY),
     :                  LSIMUL,SIMLEN,
     :                  QWCLXL(KWKIX),QWCLYB(KWKIX),
     :                  QWCLXR(KWKIX),QWCLYT(KWKIX),
     :                  LINSUB)
      ELSE
        CALL GK4TFL(NPTSA,QSTACK(IOFFSX),QSTACK(IOFFSY))
      ENDIF


*     Release stack
      CALL GKSTDA(KREALS,IOFFST)
*     and restore total workstation transformation.
 8000 CONTINUE
      DO 20 I=1,6
         QWTOTT(I,KWKIX) = TTCOPY(I)
   20 CONTINUE

 9999 CONTINUE
      END
      SUBROUTINE GK4TCL (JBAUD)

*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Clears screen
*
*  ARGUMENT
*  --------
*     INP   JBAUD  baud rate

      INTEGER JBAUD
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
*
*  LOCALS
*  ------
*     IWORK   Array containing clear screen orders
*     NLEFT   Number of bytes left in buffer
*
      INTEGER IWORK(3), NLEFT
*      ASCII codes  US  ESC  FF
      DATA IWORK/   31, 27, 12/
*
*---------------------------------------------------------------------
      CALL GK4TWV(0.0,QDSDX(KWKIX),0.0,QDSDY(KWKIX))


      CALL GKIOBO(KIOPB,3,IWORK,NLEFT)
      CALL GKIOBO(KIOSN,1,IWORK,NLEFT)
      CALL GKSYN(JBAUD, 0.8)

      END
      SUBROUTINE GK4TCS (ICODE, XDC,YDC)

*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (part of) Workstation Driver
*  Author:             JRG (Tek 4010, GK0TCS)
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To obtain key hit and cursor position from Tektronix 420X
*         - select cursor
*         - turn cursor on
*         - obtain location and key hit from terminal
*
*  ARGUMENTS
*  ---------
*     OUT  ICODE   Ascii value of key hit
*     INP  XDC,YDC Initial Cursor Position (Device Coords)
*     OUT  XDC,YDC Final Cursor Position (Device Coords)
*
      INTEGER ICODE
      REAL    XDC(1), YDC(1)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /ERR/    KERROR
*     Read   /WCA/    KWKIX
*     Read   /WKD/    KWKDAT
*

      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkio.cmn'
*
*  LOCALS
*  ------
*     INDEV  Array of GIN device codes (w/s 250,251) :
*                 0 = keyboard (joydisk) only
*                64 = mouse
*                 8 = tablet  PORT 0 (absolute)
*     ICSN   Array of codes for set GIN cursor
*     IPOS   Array of codes for setting initial cursor position
*     IEGIN  Array of codes for turning cursor on
*     IAREA  Array of codes for setting GIN area to current view
*     IDGIN  Array of codes for turning cursor off
*
*     ICX,ICY   Initial position of cursor in TSC
*     NB     Position in buffer
*     NDV    Device Number
*
*     IDUMMY Used where integer array needed, but contents do not matter
*     IER    Local error indicator
*     IN     Input buffer
*     MXLCIN Maximum size of array to hold bytes from Tek
*     NOUT   Number of bytes returned by I/O buffer routine
*     PTORX,PTORY  Conversion factors for TSC/DC local copies
*     IDCTSC Offset of conversion factor TSC/DC
*     ICTRLZ Ascii code for control-Z
*
*
      INTEGER ICX,ICY,NB,NDV
      INTEGER IEBX,IEBY,J,K,ITEMP(6)
      INTEGER MXLCIN,IDCTSC,ICTRLZ,ICR
      PARAMETER (MXLCIN=10,IDCTSC=1,ICTRLZ=26,ICR=13)
      INTEGER IDUMMY(1),IN(MXLCIN)
      INTEGER IER,NOUT
      INTEGER IEGIN(6), ICSN(8), IPOS(11), INDEV(3), IAREA(6)
      REAL    PTORX, PTORY
      DATA ICSN /27,73,67,5*0/
      DATA IPOS /27,83,88,8*0/
      DATA IEGIN/27,73,69,3*0/
      DATA IAREA/27,73,86,3*0/
      DATA INDEV   /0,64,8/
*
*  ERRORS
*  ------
*      302   I/O error while reading
*

*
*  COMMENTS
*  --------
*      Does not set GIN display start point.
*
*  ALGORITHM
*  ---------
*
*      TEK Commands:-
*
*      ICSN      esc i c INDEV 0       Set GIN Cursor (cross-hairs)
*      IPOS      esc s x 0  - -        Set segment position (of cursor)
*      IAREA     esc i v INDEV 0       Set GIN Area to current view
*      IEGIN     esc i e INDEV 1       Enable GIN
*
*-----------------------------------------------------------------------
*   Get device number from WCA
      NDV = KWI1
      IF(NDV .GT. 3)NDV=1
      IF(NDV .LT. 1)NDV=1
*   Select Cursor and set its initial position
      NB = 4
      CALL GK4TTI (INDEV(NDV),8,ICSN,NB)
      CALL GK4TTI (0,8,ICSN,NB)
      CALL GKIOBO (KIOPB,NB-1,ICSN,NOUT)
      NB = 4
      CALL GK4TTI (0,11,IPOS,NB)
      CALL GK4TCT (XDC,YDC,0,ICX,ICY)
      CALL GK4TTP (ICX,ICY,11,IPOS,NB)
      CALL GKIOBO (KIOPB,NB-1,IPOS,NOUT)
*   Define GIN Area as current view
      NB = 4
      CALL GK4TTI(INDEV(NDV),6,IAREA,NB)
      CALL GK4TTI(0,6,IAREA,NB)
      CALL GKIOBO(KIOPB,NB-1,IAREA,NOUT)
*   Turn Cursor on  , Enable GIN on appropriate device
      NB = 4
      CALL GK4TTI(INDEV(NDV),6,IEGIN,NB)
      CALL GK4TTI(1,6,IEGIN,NB)
      CALL GKIOBO(KIOPB,NB-1,IEGIN,NOUT)
*   Set local error indicator to be no error
      IER=0

*   Get conversion factors
      PTORX = QWKDAT (IDCTSC,KWKIX)
      PTORY = PTORX

*   Flush output buffer and read -- noecho (from
*   operating system to terminal), purge typeahead buffer if there is one
      CALL GKIOBO(KIOSN, 1,IDUMMY, NOUT)
   5  CALL GKIOBI(KIOEP, 0,IDUMMY, MXLCIN,IN, NOUT)
      IF( KERROR.NE.0)  GOTO 9302

*   Inspect input buffer IN (number of bytes returned is NOUT)
      IF(NOUT.EQ.0)THEN
*   This catches the situation of the device sending multpile
*   CR characters on the end of the previous input
        GOTO 5
      ELSE IF( NOUT.EQ.7 ) THEN
*      Here, ICODE contains key hit in ASCII
       ICODE=IN(2)
*      If mouse or tablet count left key '1' as CR and
*                              right key '3' as CONTROL-Z
       IF(NDV .GE. 2)THEN
          IF(ICODE .EQ. 49)THEN
             ICODE = ICR
          ELSEIF(ICODE .EQ. 51)THEN
             ICODE = ICTRLZ
             GOTO 9900
          ENDIF
       ENDIF
      ELSE IF((NOUT .EQ. 6) .OR. (NOUT .EQ. 5))THEN
*       Here the hit key was control-z.
*       Record key hit as being control-z and quit.
        ICODE=ICTRLZ
        GOTO 9900
      ELSE IF (NOUT .EQ. 1) THEN
*       Here the hit key was a control key such as CR or LF
*       so read again to get rest of the buffer.  Record key hit
*       as being CR.
        ICODE = ICR
        CALL GKIOBI(KIOEN, 0, IDUMMY, MXLCIN-2, IN(3), NOUT)
        IF (KERROR.NE.0 .OR. NOUT.NE.5) GOTO 9302
      ELSE
       GOTO 9302
      ENDIF

*   Here, ICODE contains key hit in ASCII and
*         IN(3) to IN(7) contain bytes making up X and Y
      DO 10 J=3,7
        K=IN(J)-32
        IF(K.LT.0 .OR. K.GT.31) GOTO 9302
        ITEMP(J-1)=K
10    CONTINUE
      IF(ITEMP(3).GT.15)ITEMP(3)=ITEMP(3)-16
      IEBX=ITEMP(3)
      IEBX = IEBX - 4*(IEBX/4)
      XDC(1)=FLOAT(ITEMP(5)*128 + ITEMP(6)*4 + IEBX -1)/PTORX
      IEBY=ITEMP(3)/4
      YDC(1)=FLOAT(ITEMP(2)*128 + ITEMP(4)*4 + IEBY -1)/PTORY
      GOTO 9900

*   I/O error while reading
 9302 IER=302
      ICODE = ICTRLZ
*   Drop to 9900

 9900 CONTINUE
      KERROR=IER
*
      END
      SUBROUTINE GK4TCT (RX,RY,ISEB,IX,IY)
*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'

*  PURPOSE
*  -------
*     Converts device coordinates to terminal space coordinates
*
*  ARGUMENTS
*  ---------
*     INP   RX,RY    Device coordinates
*     INP   ISEB     Status of extra byte (0=round off,1=include)
*     OUT   IX,IY    Terminal space coordinates
*
      INTEGER IX, IY,  ISEB
      REAL    RX, RY
*
*  COMMON BLOCK USAGE
*  ------------------
*     Workstation work area
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'

*  LOCALS
*  ------
*     IDCTSC   Offset of conversion factor
*     PTORX,PTORY    Conversion factors
*     RRX,RRY  Rounded coordinates
*
      INTEGER IDCTSC, IGSGOP
      PARAMETER (IDCTSC=1, IGSGOP=7)

      REAL PTORX, PTORY, RRX, RRY

*  COMMENTS
*  --------
*    The reverse conversion occurs in GK4TCS
*---------------------------------------------------------------------
*
*     Get conversion factor
      PTORX = QWKDAT(IDCTSC,KWKIX)
      PTORY = PTORX
*
*     If no GKS segment is open and extra byte need not be included,
*     round the device coords
      IF((ISEB .EQ. 0).AND.(PTORX .GE. 3.9).AND.
     :   (KWKDAT(IGSGOP,KWKIX) .EQ. 0))THEN
        RRX = FLOAT(INT(RX)) + 0.5
        RRY = FLOAT(INT(RY)) + 0.5

*     Convert Coords
        IX = INT (PTORX*RRX) + 1
        IY = INT (PTORY*RRY) + 1

        IX = 4*(IX/4)
        IY = 4*(IY/4)
      ELSE
*     Convert Coords
        IX = INT (PTORX*RX) + 1
        IY = INT (PTORX*RY) + 1
      ENDIF

      RETURN
      END
      SUBROUTINE GK4TFA(NRD,RX,RY)
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             KEVP
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Device fill area routine for TEK 4200s
*     Fills supplies polygon either solid
*     or with predifined pattern
*     Clipping assumed done.
*
*  ARGUMENTS
*  ---------
*     INP NRD    Number of vertices
*     INP RX,RY  Vertex coordinates in DC ( = raster coords )
*
      INTEGER NRD
      REAL    RX(NRD),RY(NRD)
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     Workstation workarea offset
      INTEGER INFAIS
      PARAMETER (INFAIS=3)

*     Terminal Space Coords
      INTEGER IX,IY

*     Buffer variables
      INTEGER NB, NLEFT

*     Tek Commands
      INTEGER IBGPNL(9), IENPNL(3)
      DATA IBGPNL /27,76,80,6*0/
      DATA IENPNL /27,76,69/
*
*  EXTERNALS
*  ---------
*
*  ALGORITHM
*  ---------
*     TEK Commands:-
*
*     IBGPNL    esc l p             Begin panel boundary
*     IENPNL    esc l e             End panel
*
*  COMMENTS
*  --------
*     The workstation work-area is used for storing the
*     Fill-Area Interior Style rather than KWFAIS(KWKIX).
*     The latter can be set to GHOLLO to fool GKFILS into
*     accepting this routine in the desired manner
*     when used as an argument instead of GK4TLN.
*
*     If a segment is open arguments RX,RY may be changed in value.
*
*---------------------------------------------------------------------

*     If Fill Area Style is SOLID or PATTERN
      IF((KWKDAT(INFAIS,KWKIX) .EQ. GSOLID)
     :   .OR. (KWKDAT(INFAIS,KWKIX) .EQ. GPATTR))THEN

*     Do anything only if Number of points is at least 3
        IF(NRD .GE. 3)THEN
*         Reverse the segment transformation, if GKS segment is open
*         and displayed by a hardware segment.
          CALL GK4TRS (NRD,RX,RY)
*     Set up and send the begin panel command
          CALL GK4TCT(RX(1),RY(1),0,IX,IY)
          NB = 4
          CALL GK4TTP (IX,IY,9,IBGPNL,NB)
          CALL GK4TTI (0,9,IBGPNL,NB)
          CALL GKIOBO(KIOPB,NB-1,IBGPNL,NLEFT)
*
*         Draw panel boundary
          CALL GK4TLN(NRD-1,RX(2),RY(2))
*         End panel
          CALL GKIOBO(KIOPB,3,IENPNL,NLEFT)
        ENDIF
*
*     Fill Area Style HOLLOW
      ELSEIF(KWKDAT(INFAIS,KWKIX) .EQ. GHOLLO)THEN
        CALL GK4TLN (NRD,RX,RY)
      ENDIF
      END
      SUBROUTINE GK4TFL(NRD,RX,RY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             KEVP
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Perform appropiate fill-area operation
*     and clip.
*
*  ARGUMENTS
*  ---------
*     INP NRD    Number of vertices
*     INP RX,RY  Vertex coordinates in WC
*
      INTEGER NRD
      REAL    RX(NRD),RY(NRD)
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkpca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     ISGUSE  KWKDAT parameter indicating type of segments in use
*     JHAB    Hardware segments to be abandonned
*     JHARDW  Hardware segments in use
*
      INTEGER    ISGUSE,   JHAB,   JHARDW
      PARAMETER (ISGUSE=8, JHAB=1, JHARDW=2)
*
*  EXTERNALS
*  ---------
      EXTERNAL GK4TFA, GK4TLN, GK4TRO
*
*  ALGORITHM
*  ---------
*---------------------------------------------------------------------
*     Hatched Fill Area
      IF(KWFAIS(KWKIX) .EQ. GHATCH)THEN
         CALL GKFILS (NRD,RX,RY,1,GK4TLN,GK4TRO)
      ELSEIF(KWFAIS(KWKIX) .EQ. GPATTR)THEN
         CALL GKFILS (NRD,RX,RY,1,GK4TLN,GK4TRO)
*        Can't use hardware segments any more - set to abandon them,
         IF(KWKDAT(ISGUSE,KWKIX) .EQ. JHARDW)
     :      KWKDAT(ISGUSE,KWKIX) = JHAB
*     Hollow, solid or patterned Fill Area
      ELSE
         CALL GKFILH (NRD,RX,RY,GK4TFA)
      ENDIF
*
      END
      SUBROUTINE GK4TGI(INTA, REALA, PROMPT, STR, NOUT)
*
*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (Part of) workstation driver
*  Author:             KEVP (based on DLTs GK0TGI)
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To display a prompt on the DIALOG AREA and read input.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*     From 12/12/88 onwards
*
*  ARGUMENTS
*  ---------
*   INP    INTA   contains INT part of device state
*   INP    REALA  contains REAL part of device state (not used)
*   INP    PROMPT prompt to be displayed, trailing spaces are removed
*   OUT    STR    string typed on keyboard
*   OUT    NOUT   number of characters in string
*
      INTEGER NOUT, INTA(4)
      REAL REALA(4)
      CHARACTER*(*) STR, PROMPT
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkinp.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     NSIGS   Number of significant characters (i.e. without trailing
*             blanks) in STR
*     NLEFT   Number of characters left in output buffer
*     IOCISW  Function code for input from terminal
*     IDIAVI  Set Dialog Area Visible
*
      INTEGER NSIGS, NLEFT, IOCISW, IDIAVI(4)
*
      DATA IDIAVI /27,76,86,49/

*  ALGORITHM
*  ---------
*   Set Dialogue Area Visible, Output prompt, read input
*--------------------------------------------------------------------


*   Set Dialog Area Visible
      CALL GKIOBO (KIOPB,4,IDIAVI,NLEFT)

*   Find last significant character (position will be NSIGS) in ST
      DO 100 NSIGS=LEN(PROMPT),1,-1
        IF( PROMPT(NSIGS:NSIGS).NE.' ' ) GOTO 105
  100 CONTINUE
      NSIGS=0

*   Here, NSIGS has been set (=0 if all spaces)
  105 CONTINUE

*   Check echo
      IF (INTA(KIPE).EQ.GECHO) THEN
         IOCISW = KIOEP
      ELSE
         IOCISW = KIONP
      END IF
*     Flush buffer without adding end bytes
      CALL GKIOCO(KIOSO,' ',NLEFT)
*     Get input
      CALL GKIOCI(IOCISW, NSIGS,PROMPT(1:NSIGS), STR, NOUT)

      END
      SUBROUTINE GK4THC(IOPT,IX,IY,IFAIS,ANGVEC)
*
*--------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To generate an arc/circle for GDP.
*     And fill in if appropiate and
*     fill area style is SOLID or PATTERN
*
*  ARGUMENTS
*  ---------
*     INP   IOPT    Curve option,  NB: The valid values are negative
*                     -1: arc alone
*                     -2: arc with chord fillable
*                     -3: arc as pie fillable
*                     -4: circle fillable
*     INP   IX }    Co-ordinates of three points on arc in TSC.
*     INP   IY }
*     INP   IFAIS   Fill area interior style
*     INP   ANGVEC  Angle of turn between vectors in degrees
*
      INTEGER IOPT, IFAIS, IX(3),IY(3)
      REAL ANGVEC
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkpca.par'
      INCLUDE '../../include/gkpca.cmn'
*
*  LOCALS
*  ------
*     ICURVE     TEK draw Curve
*     ICVOPT     Hardware Draw curve option
*     IENPNL     TEK end Panel
*     IMOVE      TEK move graphics point (to first point)
*     ISMOO      TEK set curve smoothness
*
*     JARC       Plain arc
*     JCHORD     Fillable Chord
*     JPIE       Fillable Pie
*     JCIRC      Fillable Circle
*
*     NB,NLEFT   Buffer Variables
*
*     SMOOTH     Smoothness of arc (for TEK Draw Curve)
*
      INTEGER JARC,JCHORD,JPIE,JCIRC
      PARAMETER (JARC=-1, JCHORD=-2, JPIE=-3, JCIRC=-4)
      REAL    SMOOTH
      INTEGER IBGPNL(9),IENPNL(3),ICURVE(15),IMOVE(8),ISMOO(9)
      INTEGER NB,NLEFT, ICVOPT

      DATA IBGPNL /27,76,80,6*0/
      DATA IENPNL /27,76,69/
      DATA ICURVE /27,85,67,0,50,10*0/
      DATA IMOVE  /27,76,70,5*0/
      DATA ISMOO  /27,85,71,6*0/
*
*  ALGORITHM
*  ---------
*     1. In case of echoplay change style and index
*     2. Derive arc smoothness
*     3. TEK Begin Panel, if necessary
*     4. TEK Draw Curve
*
*     TEK Commands:-
*
*         IBGPNL    esc l p    begin panel boundary
*         IENPNL    esc l e    end panel
*         ICURVE    esc u c    draw curve
*         IMOVE     esc l f    move graphics point
*         ISMOO     esc u g    set curve smoothness
*
*    COMMENT
*    -------
*    This routine will not work,
*    if any of the defining points
*    are outside the terminal space
*
*---------------------------------------------------------------------
*
*
*     Select Curve Smoothness

      SMOOTH = (ANGVEC - 1.0)/44.0
      IF(SMOOTH .LT. 0.0) THEN
         SMOOTH = 0.0
      ELSEIF(SMOOTH .GT. 1.0) THEN
         SMOOTH = 1.0
      ENDIF
 1100 CONTINUE
*
*     Set Curve Smoothness
      NB = 4
      CALL GK4TTR (SMOOTH,9,ISMOO,NB)
      CALL GKIOBO (KIOPB,NB-1,ISMOO,NLEFT)
*
*     Determine hardware curve option
      IF(IOPT .EQ. JCIRC)THEN
        ICVOPT = 1
      ELSEIF((IX(1) .EQ. IX(3)) .AND. (IY(1) .EQ. IY(3)))THEN
        ICVOPT = 1
      ELSE
        ICVOPT = -IOPT
      ENDIF

*
*     Begin Panel boundary if filled interior is required
*
      IF((IOPT .NE. JARC) .AND. (IFAIS .NE. GHOLLO))THEN
        NB = 4
        CALL GK4TTP (IX(1),IY(1),9,IBGPNL,NB)
        CALL GK4TTI (0,9,IBGPNL,NB)
        CALL GKIOBO (KIOPB,NB-1,IBGPNL,NLEFT)
      ELSE
        NB = 4
        CALL GK4TTP(IX(1),IY(1),8,IMOVE,NB)
        CALL GKIOBO (KIOPB,NB-1,IMOVE,NLEFT)
        ICVOPT = 1
      ENDIF
*
*     Draw Curve
*
      NB = 4
      CALL GK4TTI (ICVOPT,15,ICURVE,NB)
      NB = 6
      CALL GK4TTP (IX(2),IY(2),15,ICURVE,NB)
      CALL GK4TTP (IX(3),IY(3),15,ICURVE,NB)
      CALL GKIOBO (KIOPB,NB-1,ICURVE,NLEFT)
*
*     End Panel, if necessary
*
      IF((IOPT .NE. JARC) .AND. (IFAIS .NE. GHOLLO))THEN
        CALL GKIOBO (KIOPB,3,IENPNL,NLEFT)
      ENDIF
*
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE GK4TID (NPSEUF)
*---------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     TEK 4200s Initialise Device
*
*  ARGUMENTS
*  ---------
*     INP  NPSEUF Pseudofile number for terminal enviroment

      INTEGER NPSEUF

*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*     IBITPL offset in KWKDAT of number of bit-planes
*     IDSIZE offset in KWKDAT of display size of terminal
      INTEGER IBITPL,IDSIZE
      PARAMETER (IBITPL = 10, IDSIZE=2)
*     Tek Command Buffers (see ALGORITHM)
      INTEGER IECHO(4)
      INTEGER ITEK(4),IDSG(4),ISURF(5),IERTH(4),IRMLL(4)
      INTEGER IENVSV(15)
      INTEGER IRSC(8),IEOL(5),IEOM(5),IEOMF(4)
      INTEGER ICOLM(6),IBIN(5),IBVIS(4),IBCOL(6)
      INTEGER IEDIA(4),IFIX(4),IGAWM(4)
      INTEGER IGTCP(4),IGTF(4),IGTPR(4)
      INTEGER IGTR(5),IGTSZ(8),ICURC(9)
      INTEGER ILI(4),ILS(4),ITI(4)
      INTEGER IGRF(4)
      INTEGER IRLENS(6),IPMODE(6),ILT(3),ILW(4)
      INTEGER IGM1(20), IGM2(14), IGM3(14)

*     Report Parameters
      INTEGER IRSIGN, ITSIGN
      PARAMETER (IRSIGN=84,ITSIGN=0)

*     NB   Pointer to position within buffer
*     NLEFT  Space left in buffer
*     I      Do Loop Index
      INTEGER NB,NLEFT

      DATA IECHO /27,75,69,48/
      DATA ITEK  /27,37,33,48/
      DATA IENVSV /27,74,86,51,69,78,86,33,50,84,79,51,77,0,58/
      DATA IDSG  /27,83,75,33/
      DATA ISURF /27,82,68,49,0/
      DATA IERTH /27,75,84,50/
      DATA IRMLL /27,73,76,57/
      DATA IRSC  /27,73,83,33,4*0/
      DATA IEOL  /27,78,84,49,61/
      DATA IEOM  /27,78,67,61,48/
      DATA IEOMF /27,73,77,48/

      DATA ICOLM /27,84,77,49,49,49/
      DATA IBIN  /27,77,66,48,48/
      DATA IBVIS /27,82,69,48/
      DATA IBCOL /27,84,66,48,48,48/
      DATA IEDIA /27,75,65,49/
      DATA IFIX  /27,82,70,50/
      DATA IGAWM /27,77,71,49/
      DATA IGTCP /27,77,78,48/
      DATA IGTF  /27,77,70,51/
      DATA IGTPR /27,77,81,50/
      DATA IGTR  /27,77,82,48,48/
      DATA IGTSZ /27,77,67,66,55,67,59,60/
      DATA ICURC /27,84,67,70,52,68,51,66,49/
      DATA ILI   /27,77,76,49/
      DATA ILS   /27,77,86,48/
      DATA ITI   /27,77,84,49/
      DATA IGRF  /27,73,75,48/
*     Tektronix 422X & 423X only
      DATA IRLENS /27,74,71,51,51,51/
      DATA IPMODE /27,77,83,48,48,49/
      DATA ILT    /27,77,87/
      DATA ILW    /27,77,86,49/
      DATA IGM1 /27,73,66,48,  50, 49,50, 51,
     :                 95,111,47, 95,111,46,  95,111,45, 50,49,49/
      DATA IGM2 /27,73,66,56,  50, 53,54, 49, 95,111,43, 50,59,49/
      DATA IGM3 /27,73,66,49,  50, 57,58, 49, 95,111,42, 50,49,49/
*
*  ALGORITHM
*  ---------
*
*     TEK Commands:-
*
*       IBCOL  esc t b 0 0 0   Set Background Color (Black)
*       IBIN   esc m b 0 0     Set Background Indices
*       IBVIS  esc r e 0       Set View Border Invisible
*       ICOLM  esc t m 1 1 1   Set Colour Mode RGB
*       ICURC  esc t c 100 70 30  Set GIN cursor colour
*       IDSG   esc s k -1      Delete all segments
*       IEDIA  esc k a 1       Enable dialogue area
*       IECHO  esc k e 0       Set Echo off
*       IEOL   esc n t         Set eol string
*       IEOM   esc n c         Set eom characters
*       IEOMF  esc i m 0       Set report eom frequency
*       IENVSV esc j v 3 e n v -1 2 t o 3 m - :
*                              Save terminal environment
*       IERTH  esc k t 2       Set error threshold
*       IFIX   esc r f 2       Set Fixup Level
*       IGAWM  esc m g 1       Set Graphics Area Write Mode
*       IGRF   esc i k 0       Set GIN report format
*       IGTCP  esc m n 0       Set Graphtext Character Path to normal
*       IGTF   esc m f 3       Set Graphtext Font (UK)
*       IGTPR  esc m q 2       Set Graphtext font precision (Stroke)
*       IGTR   esc m r 0.0     Set Graphtext Rotation to normal
*       IGTSZ  esc m c 39 59 12    Graphtext Size
*       ILI    esc m l 1       Set Line Index (Colour 1 = White)
*       ILS    esc m v 0       Set Line Style Solid
*       IRMLL  esc i l 9       Set report max line length
*       IRSC   esc i s -1      Set report signature characters
*       ISURF  esc r d 1 N     Define one surface with N bit-planes
*       ITEK   esc % ! 0       Tek mode
*       ITI    esc m t 1       Set Text Index (Colour 1 = White)
*
*       Tektronix 422X & 4235 only
*       ILT    esc m x         Set line type to pixel-wide
*       ILW    esc m w         Set line width to 1
*       IPMODE esc m s         Set panel fill mode
*       IRLENS esc j g         Set report lengths
*
*       IGM1   esc i b 0 2 1 2 3 -32511 -32510 -32509 2 1 1
*                              Map GIN device 0 to cursor pad A
*                              and keyboard
*       IGM2   esc i b 8 2 5  6 1 -32507 2 1 1
*                              Map GIN device 8 to mouse/thumbwheels
*       IGM3   esc i b 1 2 9 10 1 -32506 2 1 1
*                              Map GIN device 1 to absolute tablet
*                              on PORT 0
*
* --------------------------------------------------------------

      CALL GKIOBO(KIOPB,4,ITEK,NLEFT)
      IENVSV(14) = 48 + NPSEUF
      CALL GKIOBO(KIOPB,15,IENVSV,NLEFT)
      CALL GKIOBO(KIOPB,4,IECHO,NLEFT)
      CALL GKIOBO(KIOPB,4,IDSG,NLEFT)
      ISURF(5) = KWKDAT(IBITPL,KWKIX) + 48
      CALL GKIOBO(KIOPB,5,ISURF,NLEFT)
      CALL GKIOBO(KIOPB,4,IERTH,NLEFT)
      CALL GKIOBO(KIOPB,4,IRMLL,NLEFT)
      NB = 5
      CALL GK4TTI(IRSIGN,8,IRSC,NB)
      CALL GK4TTI(ITSIGN,8,IRSC,NB)
      CALL GKIOBO(KIOPB,NB-1,IRSC,NLEFT)
      CALL GKIOBO(KIOPB,5,IEOL,NLEFT)
      CALL GKIOBO(KIOPB,5,IEOM,NLEFT)
      CALL GKIOBO(KIOPB,4,IEOMF,NLEFT)
*
*     Initialise default terminal settings
*
      CALL GKIOBO(KIOPB,6,ICOLM,NLEFT)
      CALL GKIOBO(KIOPB,5,IBIN ,NLEFT)
      CALL GKIOBO(KIOPB,4,IBVIS,NLEFT)
      CALL GKIOBO(KIOPB,6,IBCOL,NLEFT)
      CALL GKIOBO(KIOPB,4,IEDIA,NLEFT)
      CALL GKIOBO(KIOPB,4,IFIX ,NLEFT)
      CALL GKIOBO(KIOPB,4,IGAWM,NLEFT)
      CALL GKIOBO(KIOPB,4,IGTCP,NLEFT)
      CALL GKIOBO(KIOPB,4,IGTF ,NLEFT)
      CALL GKIOBO(KIOPB,4,IGTPR,NLEFT)
      CALL GKIOBO(KIOPB,5,IGTR ,NLEFT)
      CALL GKIOBO(KIOPB,8,IGTSZ,NLEFT)
      CALL GKIOBO(KIOPB,9,ICURC,NLEFT)
      CALL GKIOBO(KIOPB,4,ILI  ,NLEFT)
      CALL GKIOBO(KIOPB,4,ILS  ,NLEFT)
      CALL GKIOBO(KIOPB,4,ITI  ,NLEFT)
      CALL GKIOBO(KIOPB,4,IGRF ,NLEFT)

*     Initialisation for Tektronix 422X and 423X only
      IF(KWKDAT(IDSIZE,KWKIX) .GE. 2)THEN
         CALL GKIOBO(KIOPB,6,IRLENS,NLEFT)
         CALL GKIOBO(KIOPB,6,IPMODE,NLEFT)
         CALL GKIOBO(KIOPB,3,ILT   ,NLEFT)
         CALL GKIOBO(KIOPB,4,ILW   ,NLEFT)
*
         CALL GKIOBO(KIOPB,20,IGM1,NLEFT)
         CALL GKIOBO(KIOPB,15,IGM2,NLEFT)
         CALL GKIOBO(KIOPB,14,IGM3,NLEFT)
      ENDIF

*     Set default for all future segments (TEK segment -2)
      CALL GK4TSD

      RETURN
      END
      SUBROUTINE GK4TLN(N,X,Y)

*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Outputs polyline to buffer
*     GKS DEVICE POLYLINE routine
*
*  ARGUMENTS
*  ---------
*     INP N   - number of points
*     INP X,Y - coordinates of points in DC ( = Raster Coords )
*
      INTEGER N
      REAL X(N),Y(N)
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*      IBAUD   Offset of baud rate within KWKDAT
*      ISYN,IVECTR,IALPHA  ASCII codes for SYN, GS and US
*      IIY,LOY,IIX,LOX      Hi Y:  Lo Y:  Hi X:  Lo X
*      IEB     Extra byte
*      LIIY,LLOY,LIIX  Previous contents of IIY,LOY,IIX. These
*              are initialised to zero to indicate no previous contents.
*      LOXE,LOYE   Composite of Lo X,Y and Extra byte
*      I       Loop counter
*      NLEFT   Number of bytes left in buffer
*
      INTEGER IBAUD, ISYN,IVECTR,IALPHA,IX,IY,IEB,LIEB
      PARAMETER (IBAUD=1, ISYN=22,IVECTR=29,IALPHA=31)
      INTEGER LIIY,LLOY,LIIX,IIY,LOY,IIX,LOX,LOYE,LOXE,I,NLEFT
*
*  COMMENT
*  -------
*     If segment is open argments X,Y may be changed in value.
*
*---------------------------------------------------------------------
*     IVECTR   Enter Vector mode
      CALL GKIOBO(KIOPB,1,IVECTR,NLEFT)
      LIIY = 0
      LLOY = 0
      LIIX = 0
      LIEB = 0

*     Reverse the segment transformation, if GKS segment is open
*     and displayed by a hardware segment.
      CALL GK4TRS (N,X,Y)

      DO 10 I=1,N
*      Evaluate high and low, X and Y bytes
        CALL GK4TCT (X(I),Y(I),0,IX,IY)
        IIX = IX/128
        IIY = IY/128
        LOXE = IX-IIX*128
        LOYE = IY-IIY*128
        LOX=LOXE/4
        LOY=LOYE/4

*      Compute extra byte
        IEB=4*(LOYE-LOY*4)+(LOXE-LOX*4)

*        Add bias
        IIX=IIX+32
        IIY=IIY+32
        LOX=LOX+64
        LOY=LOY+96
        IEB=IEB+96

*      Now output the bytes omitting those we can
        IF (IIY.NE.LIIY)THEN
          CALL GKIOBO(KIOPB,1,IIY,NLEFT)
        ENDIF
*
        IF (IEB.NE.LIEB)THEN
          CALL GKIOBO(KIOPB,1,IEB,NLEFT)
          CALL GKIOBO(KIOPB,1,LOY,NLEFT)
          IF (IIX.NE.LIIX)THEN
             CALL GKIOBO(KIOPB,1,IIX,NLEFT)
          ENDIF
        ELSE
          IF (LOY.NE.LLOY)THEN
             CALL GKIOBO(KIOPB,1,LOY,NLEFT)
             IF (IIX.NE.LIIX)THEN
               CALL GKIOBO(KIOPB,1,IIX,NLEFT)
             ENDIF
          ELSE
             IF (IIX.NE.LIIX)THEN
               CALL GKIOBO(KIOPB,1,LOY,NLEFT)
               CALL GKIOBO(KIOPB,1,IIX,NLEFT)
             ENDIF
          ENDIF
        ENDIF
        CALL GKIOBO(KIOPB,1,LOX,NLEFT)
        LIIX = IIX
        LIIY = IIY
        LLOY = LOY
        LIEB = IEB
   10 CONTINUE

*   Finally, switch to ALPHA mode (we don't have to change GSMODE since
*   it is local)
      CALL GKIOBO(KIOPB,1,IALPHA,NLEFT)

      END
      SUBROUTINE GK4TME (LENGTH,MESSAG)
*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE
*  -------
*     GKS Message routine - Puts message in dialog area
*
*  ARGUMENTS
*  ---------
*     LENGTH    length of message in characters
*     MESSAG    the message encoded in ASCII as an integer array
*
      INTEGER   LENGTH, MESSAG(LENGTH)

*   COMMON BLOCK USAGE
*   ------------------
      INCLUDE '../../include/gkio.par'

*  LOCALS
*  ------
*     MXLLIN    maximum length of a line of message
*     LINLEN    Actual Length of Line
*     STRING    one line of message
*     LSTART    start of line in message
*     LEND      end of line in message
*     NLEFT     length of buffer left
*
*     I         DO-loop variable
*
*     IDIAV     set dialog area visible
*
      INTEGER MXLLIN, LINLEN, LSTART, LEND, NLEFT, I, IDIAV(4)
      PARAMETER (MXLLIN=80)
      CHARACTER*(MXLLIN) STRING

      DATA IDIAV /27,76,86,49/

*---------------------------------------------------------------------
*
*     Make Dialog Area Visible and send buffer
*
      CALL GKIOBO(KIOPB,4,IDIAV,NLEFT)
      CALL GKIOBO(KIOSN,1,IDIAV,NLEFT)


*     Write Message (separating into different lines if necessary)

      LEND = MXLLIN
      LSTART = 1
*     -----------
*     LOOP BEGINS
*     -----------
   10 CONTINUE
      IF (LEND .LT. LENGTH) THEN
*       Find suitable point to end line
        DO 20 I=0,MXLLIN-1
           IF (MESSAG(LEND-I+1) .EQ. 32) THEN
              LINLEN = MXLLIN - I
              GOTO 25
           ENDIF
   20   CONTINUE
        LINLEN = MXLLIN
   25   CONTINUE
        CALL GKATON (LINLEN,MESSAG(LSTART),STRING)
        WRITE(6,*)STRING(1:LINLEN)
      ELSE
        LINLEN = MXLLIN - LEND + LENGTH
        CALL GKATON (LINLEN,MESSAG(LSTART),STRING)
        WRITE (6,*) STRING (1:LINLEN)
        GOTO 999
      ENDIF
      LSTART = LSTART + LINLEN
      LEND = LEND + LINLEN
      GOTO 10
*     ---------
*     LOOP ENDS
*     ---------
  999 RETURN
      END
      SUBROUTINE GK4TMV(PX,PY)
*
*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (Part of) workstation driver
*  Author:             JRG
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To move Tek cursor to specified position (D.C.). Switches
*     terminal out of graphics mode afterwards.
*
*  ARGUMENTS
*  ---------
*     INP  PX,PY   Coordinates of point (in D.C.) to which cursor is to
*                  be moved
*
      REAL PX,PY
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkio.par'
*
*  LOCALS
*  ------
*     IVEC,IALPHA  ASCII codes for switching into vector and alpha mode
*     IB     Holds bytes that are to be sent to terminal
*     IX,IY  Point in terminal space coordinates
*     NLEFT  Number of bytes left in buffer (value not used)
*     NSYNC  Number of sync bytes to be added to buffer
*     I      DO loop variable
*
      INTEGER ISYN,IVEC,IALPHA,NB,I
      PARAMETER (ISYN=22,IVEC=29,IALPHA=31)
      INTEGER NLEFT, IB(9)
      INTEGER IX,IY,NSYNC
      PARAMETER (NSYNC=2)
*


*   Fill buffer with: graphics mode, high y, low y, high x, low x,
*   normal mode, 2 sync bytes (ought to be dependent on speed)
      CALL GK4TCT (PX,PY,0,IX,IY)
*
*     Vector Mode (Graphics)
      IB(1)=IVEC
*     Coordinates
      NB = 2
      CALL GK4TTP (IX,IY,9,IB,NB)
*     Alpha Mode (Normal)
      IB(NB)=IALPHA
      DO 10 I=1,NSYNC
        IB(NB+I)=ISYN
   10 CONTINUE
      NB = NB + NSYNC

      CALL GKIOBO(KIOPB,NB,IB,NLEFT)

      END

      SUBROUTINE GK4TPC(IND,IRED,IGREEN,IBLUE)
*---------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     TEK 4200s Set colour for a linetype index
*
*  ARGUMENTS  (NB all are Integers)
*  ---------
*
*       IND      INP     Colour index
*       IRED     INP     Red component (percent of max)
*       IGREEN   INP     Green " "
*       IBLUE    INP     Blue  " "

      INTEGER IND,IRED,IGREEN,IBLUE
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkio.par'
*
*  LOCALS
*  ------
*     ICOLOR  TEK set surface color map
*     NB,NLEFT  output buffer variables
*
      INTEGER ICOLOR(23),NB,NLEFT
      DATA ICOLOR /27,84,71,49,52,18*0/
*
*  ALGORITHM
*  ---------
*
*     TEK Command is
*
*     esc t g 1 4 (index,red,green,blue)  Set surface color map
*
*     Tek surface 1 is used rather than the supersurface
*     to improve stability of colours.
*     Surfaces must be defined so that all 4 bit-planes belong
*     to suface 1
* --------------------------------------------------------------

      NB = 6
      CALL GK4TTI(IND,23,ICOLOR,NB)
      CALL GK4TTI(IRED,23,ICOLOR,NB)
      CALL GK4TTI(IGREEN,23,ICOLOR,NB)
      CALL GK4TTI(IBLUE,23,ICOLOR,NB)
      CALL GKIOBO(KIOPB,NB-1,ICOLOR,NLEFT)
*
      RETURN
      END


      SUBROUTINE GK4TPP(ITYPE,XP,YP,NR,RX,RY,BOXDEF,SQPKAP,
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
*     Primitive Pick scan routine for TEK 4200s
*
*  MAINTENANCE LOG
*  ---------------
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
*  COMMON BLOCK USAGE
*  ------------------
*     none
*
*  LOCALS
*  ------
*
*  ERRORS
*  ------
*
*  COMMENTS
*  --------
*---------------------------------------------------------------------
      CALL GKPPPR(ITYPE,XP,YP,NR,RX,RY,BOXDEF,0.25,SQPKAP,
     :            SQDIST,INSIDE)
      END
      SUBROUTINE GK4TRO(X,Y,IWIDTH,IHIGHT,NDIM,ICOLOR)
*---------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     TEK 4200s Raster Output
*     GKS DEVICE RASTER routine
*
*  ARGUMENTS
*  ---------
*     INP X,Y    Upper left corner position for raster (DC)
*     INP IWIDTH  Scanline length (DC)
*     INP IHIGHT No of scanlines
*     INP NDIM   First dimension of colour array
*     INP ICOLOR Array of colour indices for raster
*
      REAL X,Y
      INTEGER IWIDTH,IHIGHT,NDIM,ICOLOR(NDIM,IHIGHT)
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     IB     do loop index of bit-planes
*     ICOLM  do loop index of columns of raster array
*     IROW   do loop index of rows of raster array
*     IND    colour index currently in use
*     IX,IY  coordinates of upper left corner (in pixels)
*     JX,JY  coordinates of lower right corner (in pixels)
*     LRUN   runlength
*     MBINT  maximum number of bytes per integer
*     MBRUN  maximum number of bytes for runlength codes in array
*     MCOL   upper bound to number of colours (a power of 2)
*     MHIGH  raster array height (made at least 1)
*     MXLRUN maximum runlength
*     NRUN   number of elements in runlength array (less than 16)
*
*     IBITPL Offset for number of bit-planes
*
*     NB     pointer to command buffer
*     NLEFT  space in buffer
*
*     IBEGPX Begin Pixel Operations
*     ISPXVP Set Pixel Viewport
*     IRNLEN Runlegth Code
*
      INTEGER IX,IY,JX,JY,IROW,ICOLM,IND,MHIGH,NB,NLEFT,IB
      INTEGER IBITPL,MCOL,MBRUN,MBINT,NRUN,LRUN,MXLRUN
      PARAMETER (MBRUN = 12, MBINT = 3)
      INTEGER IBEGPX(7), ISPXVP(13), IRNLEN(4+MBRUN)

      PARAMETER (IBITPL = 10)
*
      DATA IBEGPX /27,82,85,33,59,0,0/
      DATA ISPXVP /27,82,83,10*0/
      DATA IRNLEN /27,82,76,0,MBRUN*0/

*  ALGORITHM
*  ---------
*     Output width by height colour array
*     by pixel runlength write for each row.
*
*     esc r u -1 11 -      begin pixel operations
*     esc r s (-,-) (-,-)  set pixel viewport
*     esc r l 1 -          runlength code (upto MBRUN bytes in array)
*
*  COMMENTS
*  --------
*     When used in the GKS utilities,
*     the  input coords X,Y are assumed
*     to be device raster coords,
*     when rounded down.
*
*     This routine is (through GKS utilities) unsuitable for use
*     when a GKS segment is open and hardware segments are in use,
*     because the output is not stored in the hardware segment.
*
* --------------------------------------------------------------
* Check number of bitplanes
      IF(KWKDAT(IBITPL,KWKIX) .GT. 16) GOTO 99
*
* Begin Pixel Operations
      MCOL = 2
      DO 10 IB=2,KWKDAT(IBITPL,KWKIX)
         MCOL = MCOL + MCOL
   10 CONTINUE
      MXLRUN = 32768/MCOL-1
      NB = 6
      CALL GK4TTI (KWKDAT(IBITPL,KWKIX),7,IBEGPX,NB)
      CALL GKIOBO (KIOPB,NB-1,IBEGPX,NLEFT)

* Set pixel viewport (assumed to be inclusive)
      IX=INT(X)
      IY=INT(Y)
      JX=IX+IWIDTH-1
      JY=IY-IHIGHT+1
      NB = 4
      CALL GK4TTP (IX,IY,13,ISPXVP,NB)
      CALL GK4TTP (JX,JY,13,ISPXVP,NB)
      CALL GKIOBO (KIOPB,NB-1,ISPXVP,NLEFT)

* Scan rows starting at base and working upwards
      MHIGH=MAX(1,IHIGHT)
      DO 30 IROW=MHIGH,1, -1
*
* Scan through this row looking for colour changes
*
         IND=ICOLOR(1,IROW)
         IND = IND - MCOL*(IND/MCOL)
         NB = 5
         NRUN = 1
         LRUN = 1

         DO 20 ICOLM=2,IWIDTH
           IF((IND .NE. ICOLOR(ICOLM,IROW)) .OR.
     :                   (LRUN .GE. MXLRUN))THEN
             CALL GK4TTI(MCOL*LRUN+IND,4+MBRUN,IRNLEN,NB)
             IF(NB+MBINT .GT. 4+MBRUN)THEN
               IRNLEN(4) = NRUN + 48
               CALL GKIOBO(KIOPB,NB-1,IRNLEN,NLEFT)
               NB = 5
               NRUN = 1
             ELSE
               NRUN = NRUN + 1
             ENDIF
             IND=ICOLOR(ICOLM,IROW)
             IND = IND - MCOL*(IND/MCOL)
             LRUN = 0
           ENDIF
           LRUN = LRUN + 1
   20   CONTINUE

*       Finish off row
        IRNLEN(4) = NRUN + 48
        CALL GK4TTI(MCOL*LRUN+IND,4+MBRUN,IRNLEN,NB)
        CALL GKIOBO(KIOPB,NB-1,IRNLEN,NLEFT)
*
   30 CONTINUE
*
   99 CONTINUE
      RETURN
      END
      SUBROUTINE GK4TRS(NRD,RX,RY)
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             KEVP
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Reverse the open segment transformation
*     on the supplied vertices, if a hardware
*     segment is open.
*
*  ARGUMENTS
*  ---------
*     INP NRD    Number of vertices
*     I/O RX,RY  Vertex coordinates in DC
*
      INTEGER NRD
      REAL    RX(NRD),RY(NRD)
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     IOPSG        Offset in KWKDAT of open segment name
*     IOPST        Offset in KWKDAT of open segment transformation
*                  indicator
*     IV,IH,RP,ID  Not used
*     SEGTF        Segment transformation
*     RSGTF        Reverse segment transformation
*     TEMP         Temporary variable
*     IERR         Error indicator

      REAL SEGTF(6), RSGTF(6), RP, TEMP
      INTEGER    IOPSG, IOPST, IERR, IV,IH,ID
      PARAMETER (IOPSG=7, IOPST=9)

*  ALGORITHM
*  ---------
*     Do anything only if a GKS segment displayed by a
*     hardware segment  is open.
*     Get segment transformation and convert it to DC by using GKMTDN.
*     Invert the result using GKMTIV and apply to coords using GKMTXF.
*
*     No TEK Commands
*
*  NOTE
*  ----
*     This routine owes its existance to hardware segments.
*
*  COMMENTS
*  --------
*     Segment coords are transformed for clipping
*     and untransformed before putting into an
*     OPEN HARDWARE SEGMENT.
*
*---------------------------------------------------------------------
*
*     Do anything only if KWKDAT(IOPSG,KWKIX) is positive
*     ie, a segment is open and displayed by a hardware segment.
      IF(KWKDAT(IOPSG,KWKIX) .GT. 0)THEN
*       Do anything only if open segment has been transformed.
        IF(KWKDAT(IOPST,KWKIX) .GT. 0)THEN
*         Get open segment transformation from CSS
          CALL GQSGA (KWKDAT(IOPSG,KWKIX),IERR,SEGTF,IV,IH,RP,ID)
*         Change order for GKMT.. utilities
          TEMP = SEGTF(2)
          SEGTF(2) = SEGTF(3)
          SEGTF(3) = SEGTF(5)
          SEGTF(5) = SEGTF(4)
          SEGTF(4) = TEMP
*         Convert transformation from NDC to DC
          CALL GKMTND (SEGTF,SEGTF(3),SEGTF(6))
*
*         Invert segment transformation
          CALL GKMTIV (SEGTF,RSGTF)
*         Apply inverse
          CALL GKMTXF (RSGTF,NRD,RX,RY,RX,RY)
        ENDIF
      ENDIF
      END
      SUBROUTINE GK4TSC(ICOL)
* --------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Device cell colour setting routine
*     for TEK 4200s (used in GKCELA)
*
*  ARGUMENTS
*  ---------
*     INP ICOL   New value of colour index
*
      INTEGER ICOL
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkpca.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkpca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     IPPAT   Offset in KWKDAT of current panel pattern index
*     ISEPAT  Output buffer for TEK commands
*     NB      Buffer variable
*     NLEFT   Buffer variable
*     NPAT    New panel pattern
*
      INTEGER    IPPAT,   NPAT
      PARAMETER (IPPAT=4)
      INTEGER ISEPAT(5),NB,NLEFT
      DATA ISEPAT /27,77,80,0,0/
*
*     esc m p -    set pattern index
*
* --------------------------------------------------------------
*
*     Derive panel pattern index
      NPAT = KPCI(KWKIX)*(ICOL/KPCI(KWKIX)) - ICOL

*     Set new index, if different from old (in KWKDAT)
      IF(NPAT.NE.KWKDAT(IPPAT,KWKIX)) THEN
         NB = 4
         CALL GK4TTI (NPAT,5,ISEPAT,NB)
         CALL GKIOBO(KIOPB,NB-1,ISEPAT,NLEFT)
         KWKDAT(IPPAT,KWKIX)=NPAT
      ENDIF
  999 RETURN
      END
      SUBROUTINE GK4TSD
*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE
*  -------
*     Set Default values to the status of future segments
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkio.par'
*
*  LOCALS
*  ------
*     LSEG Total length of segment status attributes (integers)
*                          except for image transform
*     IDEF      list of segment defaults
*     ISSEG     Segment TEK Command (for all future segments)
*     ISSGIM    Set segment image tranform to identity
*     ISSTAT    Segment Status Characters as ASCII numbers
*     I         DO loop variable
*     NB        Buffer Pointer
*     NLEFT     Buffer Variable
*
       INTEGER LSEG
       PARAMETER (LSEG=4)
       INTEGER ISSEG(8),ISSTAT(LSEG)
       INTEGER IDEF(LSEG),ISSGIM(12),NB,NLEFT,I

       DATA ISSEG /27,83,0,34,0,0,0,0/
       DATA ISSTAT /68,72,86,77/
*                    D  H  V  M
       DATA IDEF  /  0, 0, 1, 1/
       DATA ISSGIM /27,83,73,34,49,48,49,48,48,48,48,48/

*
*
*  ALGORITHM
*  ---------
*
*     TEK Commands:-
*
*    ISSEG   esc s (status) -2    Set segment status (all future segments)
*
*              d (68)                     detectability    off
*              h (72)                     highlighting     off
*              v (86)                     visibility       on
*              m (77)                     writing mode     over
*
*
*    ISSGIM
*         esc s i -2  1.0, 1.0   0.0   0.0   Set segment image transform
*                                            (to identity)
*
*---------------------------------------------------------------------
*
      DO 10 I=1,LSEG
         ISSEG(3) = ISSTAT(I)
         NB = 5
         CALL GK4TTI (IDEF(I),6,ISSEG,NB)
         CALL GKIOBO (KIOPB,NB-1,ISSEG,NLEFT)
   10 CONTINUE
      ISSEG(3) = 88
      NB = 5
      CALL GK4TTP (0,0,8,ISSEG,NB)
      CALL GKIOBO (KIOPB,NB-1,ISSEG,NLEFT)
      CALL GKIOBO (KIOPB,12,ISSGIM,NLEFT)

      RETURN
      END
      SUBROUTINE GK4TSH(IENT,NOGSGH,IHSEG,IOSEGT)
*
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S UTILITY
*  Author:             AS (GKSGWK)
*                      KEVP this version
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Segment entrypoint handler for Tektronix 4200s
*     hardware segments
*
*  ARGUMENTS
*  ---------
*     INP IENT   - Entrypoint code
*     I/O NOGSGH - Name of open GKS segment, being displayed by a
*                  hardware segment - 0 if no such segment
*     I/O IHSEG  - Indicates whether hardware segments are in use
*     I/O IOSEGT - Indicates whether the open segment has been
*                  transformed (0=FALSE, 1=TRUE). False if hardware
*                  are not in use.
*
      INTEGER IENT,NOGSGH,IHSEG,IOSEGT
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwke.par'
      INCLUDE '../../include/gwksgl.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkio.par'
*
*  LOCALS
*  ------
*     ANG       segment rotation angle
*     IC        loop index for box
*     ICREAT    TEK create segment
*     ICLOSE    TEK close segment
*     IDETEC    TEK set segment detectability
*     IDELET    TEK delete segment
*     IFIXUP    TEK set fixup level
*     IPRI      segment priority
*     IPRIOR    TEK set segment priority
*     IRENAM    TEK rename segment
*     IRENEW    TEK renew view
*     IREOPS    TEK insert into segment (at end)
*     JCCS      Software segments in use
*     JHARDW    Harware segments in use
*     JPIVOT    coordinate of pivot point (both X and Y)
*     ITRANS    TEK set segment image transformation
*     IX,IY     segment position (terminal space coords)
*     LTS       Length of terminal space in TSC
*     NB        Buffer variable
*     NINSEG    Number of segments reserved for input
*     NLEFT     Buffer variable
*     PI        pi
*     REOPEN    Hardware segment needs reopening
*     SX,SY     segment scale factor
*     TINY      tolerance on reals
*     TMAT      Transformation matrix for terminal space coords
*     TMINV     Inverse transformation matrix
*     TX,TY         Real values of terminal space coords
*     WVX,WVY       Workstation viewport
*     XPANMX        Maximum expansion tolerated for hardware segments

*
      INTEGER NLEFT, NB, IPRI, IX,IY, LTS, NINSEG, IC
      INTEGER JPIVOT, JCSS,JHAB,JHARDW
      INTEGER ICREAT(6), IRENAM(9), IDELET(6), ITRANS(29)
      INTEGER IVISIB(9), IDETEC(9), IPRIOR(9), ICLOSE(3)
      INTEGER IRENEW(4), IREOPS(8), IPIVOT(8), IFIXUP(6)
      REAL    SX,SY, ANG1,ANG2, TMAT(3,2), TMINV(3,2)
      REAL    PI, TX,TY, WVX(4),WVY(4), TINY
      REAL    XPANMX
      LOGICAL REOPEN
*
      PARAMETER(NINSEG=10, LTS=4095, JPIVOT=LTS/2, XPANMX=2.0)
      PARAMETER (TINY=0.0001)
      PARAMETER (JCSS=0, JHAB=1, JHARDW=2)
*
      DATA ICREAT /27,83,79,0,0,0/
      DATA ICLOSE /27,83,67/
      DATA IRENAM /27,83,82,0,0,0,0,0,0/
      DATA IDELET /27,83,75,0,0,0/
      DATA IFIXUP /27,82,70,0,0,0/
      DATA ITRANS /27,83,73,26*0/
      DATA IVISIB /27,83,86,6*0/
      DATA IDETEC /27,83,68,6*0/
      DATA IPIVOT /27,83,80,5*0/
      DATA IPRIOR /27,83,83,6*0/
      DATA IRENEW /27,75,78,33/
      DATA IREOPS /27,85,73,5*0/
*
*  NOTE
*  ----
*       This routine owes its existance to hardware segments
*
*  COMMENTS
*  --------
*       This routine is based on the CSS routine GKSGWK
*
*       Unlike GKS segments, the hardware segments
*       can not have their attributes changed,
*       whilst the segment is open.
*       In such a case the hardware segment is closed,
*       the attribute is changed and
*       then the segment is reopened.
*
*       Hardware segments are abandoned, if an impossible
*       transformation is made. Software segments are
*       then to be used.

*  ALGORITHM
*  ---------
*
*     TEK Commands:-
*
*       ICREAT    esc s o -        Begin segment
*       ICLOSE    esc s c          End segment
*       IRENAM    esc s r - -      Rename segment
*       IDELET    esc s k -        Delete segment
*       ITRANS    esc s i 5*(-)    Set segment image transform
*       IVISIB    esc s v - -      Set segment visibility
*       IDETEC    esc s d - -      Set segment detectability
*       IPIVOT    esc s p (-,-)    Set pivot point
*       IPRIOR    esc s s - -      Set segment display priority
*       IRENEW    esc k n -1       Renew all views
*       IREOPS    esc u i - -1 1   Reopen Segment
*---------------------------------------------------------------------


*     If hardware segments are not in use, quit
      IF(IHSEG .NE. JHARDW)THEN
         GOTO 9999
      ENDIF

      REOPEN = .FALSE.
      PI=4.0*ATAN(1.0)


* Conditional GOTO on entrypoint code

      GOTO (410,420,430,440,450,460,470,480,490,500,510) IENT-KCRSG+1

      GOTO 9999


*------------------------------------------------------------------
* Create segment  KCRSG
*------------------------------------------------------------------
  410 CONTINUE
*     If segment name too large, abandon hardware segments
      IF(KWI1 .GT. 32767-NINSEG)THEN
         IHSEG = JHAB
         GOTO 9999
      ENDIF
*     Create hardware segment
      NB=4
      CALL GK4TTP(JPIVOT,JPIVOT,9,IPIVOT,NB)
      CALL GKIOBO(KIOPB,NB-1,IPIVOT,NLEFT)
      NB=4
      CALL GK4TTI(KWI1+NINSEG,9,ICREAT,NB)
      CALL GKIOBO(KIOPB,NB-1,ICREAT,NLEFT)
      NOGSGH = KWI1
      GOTO 9999

*------------------------------------------------------------------
* Close segment  KCLSG
*------------------------------------------------------------------
  420 CONTINUE
      CALL GKIOBO(KIOPB,3,ICLOSE,NLEFT)
      NOGSGH = 0
      IOSEGT = 0
      GOTO 9999



*------------------------------------------------------------------
* Rename segment  KRENSG
*------------------------------------------------------------------
  430 CONTINUE
*     If new segment name too large, abandon hardware segments
      IF(KWI2 .GT. 32767-NINSEG)THEN
         IHSEG = JHAB
         GOTO 9999
      ENDIF
*     If hardware segment is open, close temporarily to rename
      IF(KWI1 .EQ. NOGSGH)THEN
        CALL GKIOBO(KIOPB,3,ICLOSE,NLEFT)
        REOPEN = .TRUE.
        NOGSGH =  KWI2
      ENDIF
*     Rename
      NB=4
      CALL GK4TTI(KWI1+NINSEG,9,IRENAM,NB)
      CALL GK4TTI(KWI2+NINSEG,9,IRENAM,NB)
      CALL GKIOBO(KIOPB,NB-1,IRENAM,NLEFT)
      GOTO 9999



*------------------------------------------------------------------
* Delete segment  KDSG
*------------------------------------------------------------------
  440 CONTINUE
      KWI2 = GPRSNT
      NB=4
      CALL GK4TTI(0,6,IFIXUP,NB)
      CALL GKIOBO(KIOPB,NB-1,IFIXUP,NLEFT)
      NB=4
      CALL GK4TTI(KWI1+NINSEG,6,IDELET,NB)
      CALL GKIOBO(KIOPB,NB-1,IDELET,NLEFT)
      GOTO 7777



*------------------------------------------------------------------
* Begin segment  KBGSG
*------------------------------------------------------------------
  450 CONTINUE
      GOTO 9999



*------------------------------------------------------------------
* End segment  KENSG
*------------------------------------------------------------------
  460 CONTINUE
      GOTO 9999



*------------------------------------------------------------------
* Set segment transformation  KSSGT
*------------------------------------------------------------------
  470 CONTINUE
      IF (KWI5.EQ.2) THEN
        IF(KWI1 .EQ. NOGSGH)THEN
          CALL GKIOBO(KIOPB,3,ICLOSE,NLEFT)
          REOPEN = .TRUE.
        ENDIF
*       Get angles, that vectors (1,0) and (0,1) are rotated.
        SX=SQRT(QWR1*QWR1 + QWR4*QWR4)
        SY=SQRT(QWR2*QWR2 + QWR5*QWR5)
*       (ATAN2 gives the direction of any non-zero vector)
        ANG1 = 0.0
        IF(SX .GT. TINY) ANG1=ATAN2(QWR4,QWR1)*180.0/PI
        ANG2 = ANG1
        IF(SY .GT. TINY)THEN
           ANG2=ATAN2(-QWR2,QWR5)*180.0/PI
           IF(SX .LE. TINY) ANG1 = ANG2
        ENDIF
*       If these two angles are different,
*          the transformation is not of form SCALE-ROTATE
*          and therefore can not be used for hardware segments.
        IF(ABS(ANG1-ANG2) .GT. TINY)GOTO 479
*
*       Convert transformation to DC
        TMAT(1,1) = QWR1
        TMAT(2,1) = QWR2
        TMAT(3,1) = QWR3
        TMAT(1,2) = QWR4
        TMAT(2,2) = QWR5
        TMAT(3,2) = QWR6
        CALL GKMTND (TMAT,TMAT(3,1),TMAT(3,2))
*       If the segment is open, check whether the workstation viewport
*       is within the hardware segment (ie transformed Terminal Space).
        IF(KWI1 .EQ. NOGSGH)THEN
*         Abandon hardware segments, if transformation is singular
          IF((SX .LE. TINY).OR.(SY .LE. TINY))GOTO 479
*         else invert transformation
          CALL GKMTIV (TMAT,TMINV)
          WVX(1) = QCWVXL(KWKIX)
          WVY(1) = QCWVYB(KWKIX)
          WVX(2) = QCWVXR(KWKIX)
          WVY(2) = QCWVYB(KWKIX)
          WVX(3) = QCWVXR(KWKIX)
          WVY(3) = QCWVYT(KWKIX)
          WVX(4) = QCWVXL(KWKIX)
          WVY(4) = QCWVYT(KWKIX)
*         Back transform copy of workstation viewport
          CALL GKMTXF (TMINV,4,WVX,WVY,WVX,WVY)
*         Abandon hardware segments, if workstation viewport
*         is not contained within the hardware segment
          DO 474 IC=1,4
             CALL GK4TCT (WVX(IC),WVY(IC),1,IX,IY)
             IF((IX .LE. 0) .OR. (LTS .LE. IX))GOTO 479
             IF((IY .LE. 0) .OR. (LTS .LE. IY))GOTO 479
  474     CONTINUE
        ENDIF

*       Convert transformation to Terminal Space Coords
        CALL GK4TCT(TMAT(3,1),TMAT(3,2),1,IX,IY)
        TMAT(3,1) = FLOAT(IX)
        TMAT(3,2) = FLOAT(IY)
        CALL GKMTXF(TMAT,1,FLOAT(JPIVOT),FLOAT(JPIVOT),TX,TY)
        IX = NINT(TX)
        IY = NINT(TY)
*       Delay fixup
        NB=4
        CALL GK4TTI(0,6,IFIXUP,NB)
        CALL GKIOBO(KIOPB,NB-1,IFIXUP,NLEFT)
*       If new position is outside terminal space
*       the position change won't work, in such a case
*       hardware segments are abandoned
*       and software segments are used instead.
        IF((IX .LT. 0) .OR. (IY .LT. 0))THEN
        ELSEIF((IX .GT. LTS) .OR. (IY .GT. LTS))THEN
        ELSEIF(ABS(SX) .GT. XPANMX)THEN
        ELSEIF(ABS(SY) .GT. XPANMX)THEN
        ELSE
*
          NB=4
          CALL GK4TTI(KWI1+NINSEG,29,ITRANS,NB)
          CALL GK4TTR(SX,29,ITRANS,NB)
          CALL GK4TTR(SY,29,ITRANS,NB)
          CALL GK4TTR(ANG1,29,ITRANS,NB)
          CALL GK4TTP(IX,IY,29,ITRANS,NB)
*
          CALL GKIOBO(KIOPB,NB-1,ITRANS,NLEFT)
          IOSEGT = 1
          GOTO 7777
        ENDIF

  479   CONTINUE
*
*       Here - hardware segments set to be abandoned
        IHSEG = JHAB
        REOPEN = .FALSE.
        NB = 4
        CALL GK4TTI(2,6,IFIXUP,NB)
        CALL GKIOBO(KIOPB,NB-1,IFIXUP,NLEFT)
      ENDIF
      GOTO 9999


*------------------------------------------------------------------
* Set visibility  KSVIS
*------------------------------------------------------------------
  480 CONTINUE
      IF(KWI1 .EQ. NOGSGH)THEN
        CALL GKIOBO(KIOPB,3,ICLOSE,NLEFT)
        REOPEN = .TRUE.
      ENDIF
      NB=4
      IF(KWI4.EQ.0)THEN
        CALL GK4TTI(0,6,IFIXUP,NB)
        CALL GKIOBO(KIOPB,NB-1,IFIXUP,NLEFT)
      ENDIF
      NB=4
      CALL GK4TTI(KWI1+NINSEG,9,IVISIB,NB)
      CALL GK4TTI(KWI4,9,IVISIB,NB)
      CALL GKIOBO(KIOPB,NB-1,IVISIB,NLEFT)
      IF(KWI4.EQ.0) THEN
        GOTO 7777
      ELSE
        GOTO 8888
      ENDIF


*------------------------------------------------------------------
* Set highlighting  KSHLIT
*------------------------------------------------------------------
  490 CONTINUE
*     Segment highlighting not supported
*     Because it is not supported for software segments in GKSGWK
      GOTO 9999


*------------------------------------------------------------------
* Set segment priority  KSSGP
*------------------------------------------------------------------
  500 CONTINUE
      IF(KWI1 .EQ. NOGSGH)THEN
        CALL GKIOBO(KIOPB,3,ICLOSE,NLEFT)
        REOPEN = .TRUE.
      ENDIF
      NB=4
      CALL GK4TTI(KWI1+NINSEG,9,IPRIOR,NB)
      IPRI=INT(QWR1*65279.999)
      IPRI = IPRI - 32768
      CALL GK4TTI(IPRI,9,IPRIOR,NB)
      CALL GKIOBO(KIOPB,NB-1,IPRIOR,NLEFT)
      GOTO 9999



*------------------------------------------------------------------
* Set detectability  KSDTEC
*------------------------------------------------------------------
  510 CONTINUE
      IF(KWI1 .EQ. NOGSGH)THEN
        CALL GKIOBO(KIOPB,3,ICLOSE,NLEFT)
        REOPEN = .TRUE.
      ENDIF
      NB=4
      CALL GK4TTI(KWI1+NINSEG,9,IDETEC,NB)
      CALL GK4TTI(KWI4,9,IDETEC,NB)
      CALL GKIOBO(KIOPB,NB-1,IDETEC,NLEFT)

      GOTO 9999


*----------------------------------------------------
* Renew the view, if allowed  and reset fixup level
*----------------------------------------------------
 7777 CONTINUE
      IF(KIMRGM(KWKIX) .EQ. GALLOW)
     :   CALL GKIOBO(KIOPB,4,IRENEW,NLEFT)
      NB=4
      CALL GK4TTI(2,6,IFIXUP,NB)
      CALL GKIOBO(KIOPB,NB-1,IFIXUP,NLEFT)


 8888 CONTINUE
      IF (KIMRGM(KWKIX).EQ.GALLOW) THEN
* If regenerate immediate then suppress the playback
        KRPCC = KRPNO
        KRGN  = .TRUE.
        KWRGN(KWKIX) = .TRUE.
      ELSE
        KNFAUP(KWKIX) = GYES
      ENDIF


 9999 CONTINUE
* Abandon hardware segments if necessary
      IF(IHSEG .EQ. JHAB)THEN
*       Delete hardware segments
        IDELET(4) = 33
        CALL GKIOBO (KIOPB,4,IDELET,NLEFT)
*       Allow software segments to be replayed
        KRPCC = KRPVIS
        KWRGN(KWKIX) = .TRUE.
        KRGN = .TRUE.
*       Turn hardware segment indicators off
        IHSEG  = JCSS
        NOGSGH = 0
        IOSEGT = 0

* Reopen Hardware segment, if required
      ELSEIF (REOPEN) THEN
        NB = 4
        CALL GK4TTI (NOGSGH+NINSEG,8,IREOPS,NB)
        IREOPS(NB)   = 33
        IREOPS(NB+1) = 49
        CALL GKIOBO (KIOPB,NB+1,IREOPS,NLEFT)
      ENDIF

* Send Output Buffer
      CALL GKIOBO(KIOSN,1,NLEFT,NLEFT)
      END
      SUBROUTINE GK4TSI(ISTYLE,ICOL,IPAT)
* --------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Tek 4200s Select interior for solid or patterned panel
*     according to specification
*
*  ARGUMENTS
*  ---------
*     INP ISTYLE Fill area interior style
*     INP ICOL   New value of colour index
*     INP IPAT   New pattern index
*
      INTEGER ISTYLE, ICOL, IPAT
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkpca.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkpca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     ICLCOL  Offset in KWKDAT of current line colour index
*     IPPAT   Offset in KWKDAT of current panel pattern index
*     ISEPAT  Output buffer for TEK commands
*     NB      Buffer variable
*     NLEFT   Buffer variable
*     NPAT    New panel pattern
*
      INTEGER    ICLCOL,   IPPAT,   NPAT
      PARAMETER (ICLCOL=5, IPPAT=4)
      INTEGER ISEPAT(6),NB,NLEFT
      DATA ISEPAT /27,77,80,0,0,0/
*
*     esc m p -    set pattern index
*
* --------------------------------------------------------------
*
*     Derive panel pattern index or set line colour
      IF(ISTYLE .EQ. GSOLID)THEN
         NPAT = 0 - ICOL
      ELSEIF(ISTYLE .EQ. GPATTR)THEN
         NPAT = IPAT
      ELSE
         CALL GK4TSP(ICLCOL,ICOL)
         GOTO 999
      ENDIF

*

*     In case of pick echoplay change new colour index
      IF(KPKECO .EQ. KPECHO)THEN
         IF(NPAT .EQ. -1)THEN
            NPAT = -2
         ELSE
            NPAT = -1
         ENDIF
      ENDIF

*     Set new index, if different from old (in KWKDAT)
      IF(NPAT.NE.KWKDAT(IPPAT,KWKIX)) THEN
         NB = 4
         CALL GK4TTI (NPAT,6,ISEPAT,NB)
         CALL GKIOBO(KIOPB,NB-1,ISEPAT,NLEFT)
         KWKDAT(IPPAT,KWKIX)=NPAT
      ENDIF
  999 RETURN
      END
      SUBROUTINE GK4TSL(LNTYPE,LSIMUL)
* --------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     TEK4200s Set TEK LineStyle from GKS LineType
*
*  ARGUMENTS
*  ---------
*     INP LNTYPE   - GKSLineType [1..5]
*     OUT LSIMUL   - True if simulation is required
*
      INTEGER LNTYPE
      LOGICAL LSIMUL
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkio.par'
*
*  LOCALS
*  ------
*     ISLS    TEK Set line style
*     ITYPES  Array specifying which TEK linestyle (value)
*             corresponds to which GKS linetype    (index)
*     NLEFT   Number of bytes left in buffer
*     NTEKLS  Number of Tek Line styles
*
      INTEGER    NTEKLS,NLEFT
      PARAMETER (NTEKLS=8)
      INTEGER ITYPES(NTEKLS),ISLS(4)
      DATA ITYPES/0,3,1,2,5,4,6,7/
      DATA ISLS/27,77,86,0/
*
*  ALGORITHM
*  ---------
*
*     TEK Command:-
*
*     ISLS   esc m v -   Set line style
*
*
*  COMMENTS
*  --------
*             GKS Linetype   TEK Linestyle     Description
*
*               1 GSOLI            0             Solid
*               2 GLDASH           3             Dashed
*               3 GLDOT            1             Dotted
*               4 GLDASD           2             Dot-Dashed
*                   5              5             3Dot-Dash
*                   6              4             Long Dash
*                   7              6             Dot-Long Dash
*                   8              7             Long gap Dash
*
*
* --------------------------------------------------------------------

      IF(LNTYPE.LT.1 .OR. LNTYPE.GT.NTEKLS) THEN
        LSIMUL=.TRUE.
        ISLS(4)=48
      ELSE
        LSIMUL=.FALSE.
        ISLS(4) = 48 + ITYPES(LNTYPE)
      ENDIF
      CALL GKIOBO(KIOPB,4,ISLS,NLEFT)
      RETURN
      END
      SUBROUTINE GK4TSP(ITYPE,INDEC)
* --------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Tek 4200s Select colour for device polyline or text
*     according to specification
*
*  ARGUMENTS
*  ---------
*     INP ITYPE  Type of colour index being changed 5=LINE, 6=TEXT
*     INP INDEC  New value of colour index
*
      INTEGER ITYPE, INDEC
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkpca.par'
      INCLUDE '../../include/gkpca.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     ICLCOL     Offset in KWKDAT of current line colour index
*     ICTCOL     Offset in KWKDAT of current text colour index
*     LINDEC     Local copy of Colour index
*     IBUFF      Output buffer for TEK commands
*     NB,NLEFT   Buffer variables
*
      INTEGER    ICLCOL,  ICTCOL
      PARAMETER (ICLCOL=5,ICTCOL=6)
      INTEGER IBUFF(6),LINDEC,NB,NLEFT
      DATA IBUFF /27,77,0,0,0,0/
*                 esc,m,l,lineindex     Set line index
*                 esc,m,t,textindex     Set text index
*
*  ALGORITHM
*  ---------
*
*       If required colour index is not current index then
*          Select line index (esc M L lineindex) or
*          Select text index (esc M T textindex) as required
*          Update common block (KWKDAT) with current index
*       else
*          exit
*       endif
*
* --------------------------------------------------------------
*      Set for line or text index, in accordance to ITYPE
       IF(ITYPE .EQ. ICTCOL)THEN
*         Text index
          IBUFF(3) = 84
       ELSEIF(ITYPE.EQ. ICLCOL)THEN
*         Line index
          IBUFF(3) = 76
       ELSE
*         Not line index either
          GOTO 999
       ENDIF

*      Modify index if pick echoplay
       IF(KPKECO .EQ. KPECHO)THEN
         IF(INDEC .EQ. 1)THEN
            LINDEC = 2
         ELSE
            LINDEC = 1
         ENDIF
       ELSE
         LINDEC = INDEC
       ENDIF

*     Set new index, if different from old (in KWKDAT)
      IF(LINDEC.NE.KWKDAT(ITYPE,KWKIX)) THEN
        NB = 4
        CALL GK4TTI (LINDEC,6,IBUFF,NB)
        CALL GKIOBO(KIOPB,NB-1,IBUFF,NLEFT)
        KWKDAT(ITYPE,KWKIX)=LINDEC
      ENDIF
  999 RETURN
      END
      SUBROUTINE GK4TST (WCHAR,HCHAR,ANGLE,SPACE,NPATH)
*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE
*  -------
*     Set GRAPHTEXT attributes

*  ARGUMENTS
*  ---------
*     INP  WCHAR  Character width (DC)
*     INP  HCHAR  character height (DC)
*     INP  ANGLE  text angle
*     INP  SPACE  character spacing (in character heights)
*     INP  NPATH  text path number (as in GKS)
*
      REAL WCHAR, HCHAR, ANGLE, SPACE
      INTEGER  NPATH

*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/GKS_PAR'
*
*  LOCALS
*  ------
*     IPATH   TEK set graphtext character path
*     IROT    TEK set graphtext rotation
*     ISIZE   TEK set graphtext size
*
*     ICH     Character height (Terminal Space Coords)
*     ICW     character width (TSC)
*     IOX,IOY Device Coord origin in Terminal Space Coords
*     ISP     character spacing (TSC)
*     IUSP    Unit of character spacing (TSC)
*     NB      output buffer pointer
*     NGPATH  graphtext character path
*     NLEFT   output buffer variable (space left in buffer)

      INTEGER ISIZE(12),IROT(10),IPATH(4),NB,NLEFT
     :        ,ICW,ICH,IUSP,IOX,IOY,ISP,NGPATH
*
      DATA ISIZE /27,77,67,9*0/ ,IROT /27,77,82,0,0,0,0,0,0,0/
*     esc m c --- set grphtxt size   esc m r -  set grphtxt rotation
*
      DATA IPATH /27,77,78,0/
*     esc m n -- set graphtext char. path

*     COMMENTS
*     --------
*       Character expansion is implicit in character width
*       If NPATH does not correspond to a textpath,
*       the text path is set to GRIGHT and
*       the character spacing to 1 terminal space coord
*---------------------------------------------------------------------
*
*     convert to terminal space coords
      CALL GK4TCT (WCHAR,HCHAR,1,ICW,ICH)
      CALL GK4TCT (0.0,0.0,1,IOX,IOY)
      ICW = ICW - IOX
      ICH = ICH - IOY
      CALL GK4TCT (HCHAR,0.0,1,IUSP,IOY)
      IUSP = IUSP - IOX
*     derive graphtext character path and character spacing in TSC
      NGPATH = NPATH
      IF ((NPATH .EQ. GRIGHT) .OR. (NPATH .EQ. GLEFT)) THEN
         ISP = NINT(SPACE*IUSP)
      ELSEIF ((NPATH .EQ. GDOWN) .OR. (NPATH .EQ. GUP)) THEN
         ISP = NINT(SPACE*ICH)
      ELSE
         ISP = 1
         NGPATH = GRIGHT
      ENDIF
*     character height, width and spacing
      NB=4
      CALL GK4TTI(ICW,12,ISIZE,NB)
      CALL GK4TTI(ICH,12,ISIZE,NB)
      CALL GK4TTI(ISP,12,ISIZE,NB)
      CALL GKIOBO(KIOPB,NB-1,ISIZE,NLEFT)
*     text rotation (character up vector)
      NB=4
      CALL GK4TTR(ANGLE,10,IROT,NB)
      CALL GKIOBO(KIOPB,NB-1,IROT,NLEFT)
*     text path
      NB = 4
      CALL GK4TTI (NGPATH,4,IPATH,NB)
      CALL GKIOBO (KIOPB,4,IPATH,NLEFT)
*
      RETURN
      END
      SUBROUTINE GK4TSW (RLNWD)
* --------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     TEK4200s Set  LineWidth
*
*  ARGUMENTS
*  ---------
*     INP RLNWD    - Line width
*
      REAL RLNWD
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
*
*  LOCALS
*  ------
*     ISLW    TEK Set line width (w/s 252 to 255 only)
*     NB      Pointer to byte in buffer
*     NLEFT   Number of bytes left in buffer
*     LNW     Realised line width in pixels
*     LMNW,LMXW  Minimum/Maximum line width in pixels
*
      INTEGER ISLW(5), NB, NLEFT, LNW, LMNW, LMXW
      DATA ISLW/27,77,87,0,0/
*
*  ALGORITHM
*  ---------
*
*     TEK Command:-
*
*     ISLW   esc m w -   Set line width
*
*     Tektronix 4224,4225,4235,4236 & 4237 only (w/s 252 to 255)
*
*  COMMENTS
*  --------
*     Tek line type must be pixelwide.
*     QMXLNW must not exceed 1023.0.
* --------------------------------------------------------------------
*
*     Get realised line width
      LNW = NINT(RLNWD)
      LMNW = NINT(QMNLNW(KWKIX))
      LMXW = NINT(QMXLNW(KWKIX))

      IF (LNW .LT. LMNW)THEN
          LNW = LMNW
      ELSEIF (LNW .GT. LMXW)THEN
          LNW = LMXW
      ENDIF
*
*     Set line width
      NB = 4
      CALL GK4TTI (LNW,5,ISLW,NB)
      CALL GKIOBO (KIOPB,NB-1,ISLW,NLEFT)
      RETURN
      END
      SUBROUTINE GK4TTI(NUM,LB,IBUFF,NB)
*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE
*  -------
*     Encodes integer NUM for terminal
*     Places it in buffer at NB
*     and raises NB so that next item would be placed afterwards
*
*  ARGUMENTS
*  ---------
*     NUM    Integer to be encoded
*     LB     Length of buffer
*     IBUFF  Buffer
*     NB     Position in buffer
*
      INTEGER LB,IBUFF(LB),NUM,NB
*
*  LOCALS
*  ------
      INTEGER IP, IPH1
*     Partially encoded integers
      INTEGER MLO,MHI1,MHI2
*     Unoffset bytes
      INTEGER JHI1,JHI2,LO
*     Three One-Byte components of integer to be encoded
*
*---------------------------------------------------------------------
      IF(NB .GT. LB)GOTO 99
*     Lowest byte
      IP=IABS(NUM)
*     If absolute value is more than or equal to 16,
      IF(IP .GE. 16)THEN
         IPH1 = IP/16
         MLO = IP-IPH1*16

*        High bytes
*        If value for first high byte is
*        more than or equal to 64,
*        use both high bytes
         IF (IPH1.GE.64) THEN
             MHI2 = IPH1/64
             MHI1 = IPH1-MHI2*64

*            check the value of the highest byte
             IF(MHI2 .GT. 64)THEN
*            if too large cut off excess bits (this should not occur)
                MHI2 = MHI2 - 64*(MHI2/64)
             ENDIF

*            Put highest byte into buffer
             JHI2 = MHI2 + 64
             IBUFF(NB)=JHI2
             NB=NB+1
             IF(NB .GT. LB)GOTO 99

          ELSE
*         miss out highest byte
             MHI1 = IPH1
          ENDIF

*         Put lower high byte into buffer
          JHI1 = MHI1 + 64
          IBUFF(NB)=JHI1
          NB=NB+1
          IF(NB .GT. LB)GOTO 99

      ELSE
*     miss out higher bytes.
         MLO = IP
      ENDIF

*     Put low byte into buffer
      IF(NUM.GE.0)THEN
        LO=MLO+48
      ELSE
        LO=MLO+32
      ENDIF
      IBUFF(NB)=LO
      NB=NB+1
      IF(NB .GT. LB)GOTO 99

   99 RETURN
      END
      SUBROUTINE GK4TTP(IX,IY,LB,IBUFF,NB)
*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE
*  -------
*     Encodes coordinates (IX,IY) for terminal
*     Places it in buffer at NB
*     and raises NB so that next item would be placed afterwards
*     (In vector mode, use ONLY if all the coords fit into buffer)
*
*  ARGUMENTS
*  ---------
*     IX,IY  Coordinates to be encoded
*     LB     Length of buffer
*     IBUFF  Buffer
*     NB     Position in buffer
*
      INTEGER LB,IBUFF(LB),IX,IY,NB
*
*  LOCALS
*  ------
      INTEGER JHIY,JEB,LOY,JHIX,LOX
*     Five One-Byte components of coordinate pair to be encoded
*     All bytes are sent except the extra byte when not needed
*
*---------------------------------------------------------------------
*
*     Check that coordinate values are in range
*     if not then set to nearest value in range
*  (these values should be in range before this routine is called)
      IF(IX .LT. 0)THEN
         IX = 0
      ELSEIF(IX .GE. 4096)THEN
         IX = 4092
      ENDIF
      IF(IY .LT. 0)THEN
         IY = 0
      ELSEIF(IY .GE. 4096)THEN
         IY = 4092
      ENDIF
*
*     Extra Byte
*
      JEB = 4*MOD(IY,4) + MOD(IX,4)
*
*     X - Coordinate (without extra byte's bits)
*
      JHIX = IX/128
      LOX  = MOD(IX/4,32)
*
*     Y - Coordinate (without extra byte's bits)
*
      JHIY = IY/128
      LOY  = MOD(IY/4,32)

*   Put bytes into buffer  (quit if buffer overflows)
*
      IBUFF(NB)=JHIY+32
      NB=NB+1
      IF(NB .GT. LB)GOTO 99

C     IF(JEB .NE. 0)THEN
        IBUFF(NB)=JEB+96
        NB=NB+1
        IF(NB .GT. LB)GOTO 99
C     ENDIF

      IBUFF(NB)=LOY+96
      NB=NB+1
      IF(NB .GT. LB)GOTO 99

      IBUFF(NB)=JHIX+32
      NB=NB+1
      IF(NB .GT. LB)GOTO 99

      IBUFF(NB)=LOX+64
      NB=NB+1
      IF(NB .GT. LB)GOTO 99

   99 RETURN
      END
      SUBROUTINE GK4TTR(RNUM,LB,IBUFF,NB)
*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE
*  -------
*     Encodes real number RNUM for terminal
*     Places it in buffer at NB
*     and raises NB so that next item would be placed afterwards
*
*  ARGUMENTS
*  ---------
*     RNUM   Real number to be encoded
*     LB     Length of buffer
*     IBUFF  Buffer
*     NB     Position in buffer
*
      INTEGER LB,IBUFF(LB),NB
      REAL    RNUM
*
*  LOCALS
*  ------
*     MANT   Mantissa
*     IEXP   Exponent
*
*     PMANT  Real number eventually to become mantissa (premantissa)
*     H14    2 to the power of -14
*
      INTEGER MANT,IEXP
      REAL H14, PMANT
      PARAMETER (H14=0.00006104)
*---------------------------------------------------------------------
      IEXP = 0
      PMANT = ABS(RNUM)
   10 IF(PMANT .LE. AINT(PMANT) + H14)GOTO 20
*     If fractional part of premantissa is not negligible
      PMANT = PMANT*2
      IEXP = IEXP-1
      GOTO 10
   20 IF(PMANT .LT. 32767.0) GOTO 30
*     If premantissa is too big to be an integer
      PMANT = PMANT*0.5
      IEXP = IEXP+1
      GOTO 20
*     Premantissa is rounded to mantissa and its sign is set
   30 MANT = AINT(PMANT + 0.5)
      IF(RNUM .LT. 0.0)MANT = -MANT
*
*     Put in buffer
*
      CALL GK4TTI (MANT,LB,IBUFF,NB)
      CALL GK4TTI (IEXP,LB,IBUFF,NB)
*
      RETURN
      END
      SUBROUTINE GK4TTS (NINT,INTA,NINP)
*------------------------------------------------------------------------
*     Author: KEVP

      INCLUDE '../../include/check.inc'
*
*  PURPOSE
*  -------
*     Place bytes in a integer array into the buffer
*     using GKIOBO, sending buffer when full.
*     Can be used if the array is longer than the buffer.
*
*  ARGUMENTS
*  ---------
*     INP NINT  - number of integers in INTA (as big as you like)
*     INP INTA  - array of integers, decimal ASCII
*     I/O NINP  - number of bytes remaining
*
      INTEGER NINT, INTA(NINT), NINP
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkio.par'
*
*  LOCALS
*  ------
*     ISEND  Index of next integer placed into buffer
*     LCHUNK Length of of array chuck placed into buffer

      INTEGER ISEND, LCHUNK


*  COMMENTS
*  --------
* +++ Should be a Utility or incoporated in GKIOBO (KIOPB. +++
*
*---------------------------------------------------------------------
      ISEND = 1
   90 CONTINUE
*     Send buffer if full
      IF(NINP .LE. 0)CALL GKIOBO(KIOSN,1,INTA,NINP)
*     Find length of chunk
      IF(NINT-ISEND+1 .LE. NINP)THEN
         LCHUNK = NINT-ISEND+1
      ELSE
         LCHUNK = NINP
      ENDIF
*     Put chunk into buffer
      CALL GKIOBO (KIOPB,LCHUNK,INTA(ISEND),NINP)
      ISEND = ISEND + LCHUNK
*     Go back if another chunk
      IF(ISEND .LE. NINT)GOTO 90
      END
      SUBROUTINE GK4TWV (XMIN,XMAX,YMIN,YMAX)
*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE
*  -------
*     Set TEK View so that the viewport has
*     limits as spacified by the arguments
*
*  ARGUMENTS
*  ---------
*     INP   XMIN,XMAX  Range of X in Viewport (DC)
*     INP   YMIN,YMAX  Range of Y in Viewport (DC)

      REAL XMIN,XMAX, YMIN,YMAX
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     PXMIN,PXMAX  Range of X in Viewport adjusted to pixels
*     PYMIN,PYMAX  Range of Y in Viewport adjusted to pixels
*
*
*     ICNR1X,ICNR1Y  First Corner in terminal space coords
*     ICNR2X,ICNR2Y  Second Corner in terminal space coords
*
*     ISEVWA  Set View Attributes  (esc r a 0 0 1)
*     ISVP    Set Viewport   (esc r v -- -- )
*     ISWDW   Set Window     (esc r w -- -- )
*
*     LTS     Length of terminal space
*
*     NB,NLEFT  Buffer variables
*
*     TL      Length of Viewport in Terminal Space Coords
*
      INTEGER LTS, ICNR1X,ICNR1Y,ICNR2X,ICNR2Y,NB,NLEFT
      INTEGER  ISEVWA(6), ISVP(13), ISWDW(13)
*
      REAL  TL, PXMIN,PXMAX, PYMIN,PYMAX
*
      PARAMETER (LTS = 4096)
*
      DATA ISEVWA /27,82,65,48,48,49/
      DATA ISVP /27,82,86,10*0/ ISWDW /27,82,87,10*0/
*---------------------------------------------------------------------
*
*     Adjust limits to fit in with pixels
      PXMIN = XMIN
      PXMAX = XMAX
      PYMIN = YMIN
      PYMAX = YMAX
*
*     Derive the two corners in terminal space coords
*     for the TEK window and set it
*
      CALL GK4TCT (PXMIN,PYMIN,1,ICNR1X,ICNR1Y)
      CALL GK4TCT (PXMAX,PYMAX,1,ICNR2X,ICNR2Y)
      ICNR1X = ICNR1X - 1
      ICNR1Y = ICNR1Y - 1
      NB = 4
      CALL GK4TTP (ICNR1X,ICNR1Y,13,ISWDW,NB)
      CALL GK4TTP (ICNR2X,ICNR2Y,13,ISWDW,NB)
      CALL GKIOBO (KIOPB,NB-1,ISWDW,NLEFT)
*
*     Derive the two corners in terminal space coords
*     for the viewport and set it
*
      TL = FLOAT(LTS) - 0.001
      IF(QDSDX(KWKIX) .GE. QDSDY(KWKIX))THEN
        ICNR1X = INT(PXMIN*(TL/QDSDX(KWKIX)))
        ICNR1Y = INT(PYMIN*(TL/QDSDX(KWKIX)))
        ICNR2X = INT(PXMAX*(TL/QDSDX(KWKIX)))
        ICNR2Y = INT(PYMAX*(TL/QDSDX(KWKIX)))
      ELSE
        ICNR1X = INT(PXMIN*(TL/QDSDY(KWKIX)))
        ICNR1Y = INT(PYMIN*(TL/QDSDY(KWKIX)))
        ICNR2X = INT(PXMAX*(TL/QDSDY(KWKIX)))
        ICNR2Y = INT(PYMAX*(TL/QDSDY(KWKIX)))
      ENDIF
      NB = 4
      CALL GK4TTP (ICNR1X,ICNR1Y,13,ISVP,NB)
      CALL GK4TTP (ICNR2X,ICNR2Y,13,ISVP,NB)
      CALL GKIOBO (KIOPB,NB-1,ISVP,NLEFT)
*
*     Set View Attributes
*
      CALL GKIOBO (KIOPB,6,ISEVWA,NLEFT)

      RETURN
      END
      SUBROUTINE GK4TXF(IFID,RHT,RMAXWD,RBOT,RTOP,RWD)
* --------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Tek 4200s Supply Font Details
*     GKS DEVICE TEXT DETAILS routine
*
*  ARGUMENTS
*  ---------
*     INP IFID     Font identifier (ignored)
*     OUT RHT      Height from base to cap line
*     OUT RMAXWD   Width of widest character
*     OUT RBOT     Distance from base to bottom line
*     OUT RTOP     Distance from cap to top line
*     OUT RWD      Character widths array
*
      INTEGER IFID
      REAL RHT,RMAXWD,RBOT,RTOP,RWD(*)
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
      INTEGER IFCHAR
*     Character height and width vectors
*
*     No TEK commands

* --------------------------------------------------------------------

*     character height
      RHT = FLOAT(KWCHHT(KWKIX))
*     character width
      RMAXWD = FLOAT(KWCHWD(KWKIX))
*     Cap to top
      RTOP=0.0
*     Bottom to Base
      RBOT=0.0
*     widths for chars [32..126]
      DO 10 IFCHAR=1,95
      RWD(IFCHAR)=RMAXWD
 10   CONTINUE
      RETURN
      END
      SUBROUTINE GK4TXT(IFID,ITXPR,NCH,ICHARS,X,Y)
* --------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Tek 4200s character output
*     for string or char precision text
*     GKS DEVICE TEXT routine for GKXDCS
*
*  ARGUMENTS
*  ---------
*     INP IFID     Font identifier (ignored)
*     INP ITXPR    Text Precision  (string or char)
*     INP NCH      Number of characters
*     INP ICHARS   Character values (ASCII)
*     INP X,Y      Centre position for each cell  (Char Prec.)
*                         position for first cell (String Prec.)
*
      INTEGER IFID,ITXPR,NCH,ICHARS(NCH)
      REAL X(NCH),Y(NCH)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
*     ICH     Do loop index over (1:NSEND)
*     IGTEXT  TEK Graphic text (one character)
*     IPTR    Buffer pointer
*     MULTI   True, if multiple characters are to be sent in one go
*     NSEND   Number of strings sent
*     NLEFT   Number of bytes left in buffer (actually used here)
*     RCHRX,RCHRY  character rotation vector
*     RCHH  length of character height vector
*     RCHW  length of character width  vector
*     RCHRL length of character rotation vector
*     RCENX,RCENY    character centre
*     RCHSIN,RCHCOS  normalised character rotation vector
*     ROTCX,ROTCY    rotated character centre
*     TX,TY   TEK text position (DC)
*     XC,YC   Current character position
*
      INTEGER    NSEND, IPTR, ICH
      INTEGER IGTEXT(6),NLEFT
      REAL TX,TY, XC,YC, ROTCX,ROTCY
      REAL RCHH,RCHW,RCHSIN,RCHCOS
      REAL RCENX,RCENY,RCHRX,RCHRY,RCHRL
      LOGICAL MULTI

      DATA IGTEXT /27,76,84,0,0,0/
*     IGTEXT     esc l t - -   graphic text (one character)
*                              (or start of several characters)
*
*  COMMENTS
*  --------
*     Pen position is unknown after text output.
*
* --------------------------------------------------------------------
*
*     If CHAR precision output each character of string
*     individually. If STRING precision output string as a lump.
      IF(ITXPR .EQ. GCHARP)THEN
         NSEND = NCH
*        Put string length 1 into graphtext command
         IGTEXT(4) = 49
      ELSEIF(ITXPR .EQ. GSTRP)THEN
         NSEND = 1
*        Put string length NCH into graphtext command
         IPTR = 4
         CALL GK4TTI(NCH,6,IGTEXT,IPTR)
      ENDIF


*     character height
      RCHH = FLOAT(KWCHHT(KWKIX))
*     character width (with normal spacing removed)
      RCHW = FLOAT(KWCHWD(KWKIX)) - 0.3*RCHH
*     character centre
      RCENX=RCHW/2.0
      RCENY=RCHH/2.0
*     character rotation
      RCHRX=QWCHRX(KWKIX)
      RCHRY=QWCHRY(KWKIX)
      RCHRL=SQRT(RCHRX*RCHRX+RCHRY*RCHRY)
      RCHSIN=RCHRY/RCHRL
      RCHCOS=RCHRX/RCHRL
*     rotate character centre
      ROTCX = RCENX*RCHCOS - RCENY*RCHSIN
      ROTCY = RCENX*RCHSIN + RCENY*RCHCOS

*     determine if multiple characters
      MULTI = ((NCH .GT. 1) .AND. (NSEND .EQ. 1))

*   Character production
      DO 100 ICH = 1,NSEND
         XC = X(ICH)
         YC = Y(ICH)
         TX = XC - ROTCX
         TY = YC - ROTCY
         CALL GK4TRS (1,TX,TY)
*        Move to character position
         CALL GK4TMV (TX,TY)
*        Send to buffer
         IF(MULTI)THEN
            CALL GKIOBO (KIOPB,IPTR-1,IGTEXT,NLEFT)
            CALL GK4TTS (NCH,ICHARS,NLEFT)
         ELSE
            IGTEXT(5) = ICHARS(ICH)
            CALL GKIOBO (KIOPB,5,IGTEXT,NLEFT)
         ENDIF
  100 CONTINUE
      END
