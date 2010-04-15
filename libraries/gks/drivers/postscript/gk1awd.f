      SUBROUTINE GK1AWD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
*
* (C) COPYRIGHT ICL & SERC  1986
*
*
* --------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:             B. C. Cogan (Mt. Stromlo Observatory)
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Apple LaserWriter (PostScript) workstation driver
*
*  MAINTENANCE LOG
*  ---------------
*     12/09/86  BCC   Changes for new version of GKS (SSC250)
*     12/11/86  BCC   Changes to polyline postscript code
*     07/11/87  MJP   Removed Vax code and updated to GKS 7.4
*     09/02/88  TAW   Split into one source file per routine.
*     20/05/88  PLP   Extended fill  area code  in GK1AID to cope with patterns
*                     Added code to the Set Text Representation entry in GK1AWD
*                     Straightened-up  the WDT2700 file and enhanced commenting
*                     throughout.
*     02/08/88  PLP   Completed the hardware  text re-shuffle; replaced the old
*                     GK1ATX routine with a new GK1AXS and introduced GK1AXF to
*                     supply the hardware font  data. In GK1AID introduced user
*                     coordinate system translation to the lower left corner of
*                     the imageable area.
*     16/08/88  PLP   Changed output primitives  attributes handling in GK1AWD,
*                     GK1APL, GK1APM, GK1AFA, GK1AXS. Introduced GK1AST,routine
*                     to send colour index,  linestyle and linewidth before any
*                     of the output primitives is executed.Consequently removed
*                     the now obsolete GK1APL.
*                     Split GK1AID into two routines:  GK1AID which initialises
*                     the device,and GK1APS which sends the PostScript prologue
*                     Re-structured GK1AWD to other drivers' standard.
*     19/08/88  PLP   Real coordinates and RGB colour introduced throughout the
*                     driver.The coordinate integerisation removed from:GK1ACA,
*                     GK1ACB, GK1AFA, GK1ALN, GK1APM, GK1ARO and GK1AXS;integer
*                     version of the colour table removed from GK1AID; obsolete
*                     and crude clipping removed from all output primitives.  A
*                     new function GK1AGL introduced - returns  NTSC grey level
*                     given an RGB colour (just to satisfy the image operator).
*                     GK1ACB shortened: sends but the clipping rectangle to the
*                     device.Introduced metres as device coordinates.This meant
*                     changes in wdt file and GK1AID  (now have scaling) and in
*                     GK1AWD (scaling,normalisation transformations, inquires).
*                     Generally improved inquires in GK1AWD.
*     02/09/88  PLP   Straightened up the nominal linewidths, marker sizes etc.
*                     for LaserWriters. Since we are now using points as raster
*                     coordinates added code to handle linewidths scale factors
*                     and marker size scale factors in GK1AWD. Instead of using
*                     polymarker utility in GK1AWD, GK1APM was  put on line and
*                     corresponding  PostScript  procedures  were corrected and
*                     activated. Several changes in GK1APS: a new procedure  to
*                     handle initialisation for both orientation types.  GK1ACL
*                     fitted with a flag to differ between ordinary clearing up
*                     and clearing up before closing.
*     28/09/88  PLP   Supplied missing hardware  font information in GK1AXF and
*                     altered GK1AXS so as to handle text rotation properly. In
*                     GK1AWD, GK1AXS and GK1AXF introduced workstation types to
*                     ease the addition of new PostScript devices.
*     03/10/88  PLP   Altered GK1ACA to cover all possible cell array cases.
*     26/10/88  PLP   Expanded the driver to produce a  minimum conforming EPSF
*                     file.This meant introducing two new routines - GK1AHD and
*                     GK1ATR, which output header and trailer sections of  EPSF
*                     comments and also numerous changes in the initialisation,
*                     prologue, clipping and text handling. Routines changed or
*                     ( and ) extended include: GK1AID, GK1APS, GK1AXS, GK1AXF,
*                     GK1ACL, GK1ACB. Also, the dash patterns in  the linestyle
*                     array plsty in GK1APS were converted to points,
*                     coordinates transformation bug was removed from the
*                     Polymarker entry in GK1AWD.
*     31/10/88  PLP   Abandoned old method of pattern replication in GK1APS and
*                     adopted new.Also in both GK1APS and GK1AFA introduced the
*                     code to handle the pattern reference point.
*     03/11/88  PLP   Made changes in GK1APS  so that a structured prologue is
*                     produced.  Also ensured that throughout the driver prior
*                     to invocation of a new PS procedure the output buffer is
*                     flushed.
*     23/11/88  PLP   In GK1ACA fixed the scaling problem.
*     25/11/88  PLP   In GK1APS introduced the fifth linestyle. Also in GK1APS
*                     moved device initialisation before End Of Prologue so as
*                     to render the order in which the output pages get printed
*                     irrelevant.
*     02/12/88  PLP   Sorted out pattern reference point handling in GK1APS.
*                     Reverted to using the utility for hollow fill area, since
*                     a way of doing it properly in PostScript wasn't found.
*     05/12/88  PLP   Throughout the driver increased output format for
*                     the coordinates to F11.3. Reduced maximum linewidth in
*                     the WDT table. Improved inquiries.
*     08/12/88  PLP   In GK1APS made the linetype definition dependent on the
*                     current linewidth. Altered markertype 5 definition,
*                     corrected text expansion algorithm.
*     14/12/88  PLP   All output primitives now save the Workstation
*                     Communication Area on entry and restore on exit.
*                     This was necessary after a bug involving data
*                     corruption was discovered in GK1ACA.
*     21/12/88  PLP   Introduced GK1ABG to deal with background colour. Changed
*                     GK1AST accordingly.
*     11/01/89  PLP   Implemented Message entrypoint in GK1AWD. Added an EPSF
*                     identifier to the opening line of a header in GK1AHD.
*     20/01/89  PLP   Improved commenting throughout.
*     03/02/89  PLP   Fixed the big Cell Arrays splitting bug in GK1ACA and
*                     corrected the formats in GK1ARO and GK1ACA to allow for
*                     larger arrays.
*     07/04/89  PLP   Made various changes in GK1ARO, GK1AID, GK1AWD,
*                     in order to achieve VAX acceptance.
*     18/05/89  PLP   Now that QWKDAT is increased, added expansion to
*                     the list of workspace real variables. Consequently
*                     changed GK1AID, GK1AXS (expansion factor treated
*                     just like any other text attribute) and GK1APS.
*     23/05/89  PLP   Introduced STRING text in GK1AXS and GK1APS.
*     24/05/89  PLP   Removed chunking from FILL AREA and changed the
*                     Fill Area routine's name.
*     31/05/89  PLP   Removed eoclip from the fasoldo procedure in GK1APS
*                     because eofill does its job anyway and, also, eoclip
*                     has a bug which means that some patterns loop the
*                     LaserWriter.
*     23/08/89  RMK   Changed pattern index check in set fill area
*                     attributes to allow for user-defined patterns as
*                     well as predefined ones (S79).
*     25/10/89  RMK   In GK1AGL, use MOD to ensure that colour index is
*                     valid (S350).
*     23/11/89  PLP   Introduced moveto to a first point of the array to
*                     be filled in GK1AFL. This was necessary to avoid
*                     PostScript forming subpaths in a chunked fill array.
*                     Consequently changed fagen procedure in GK1APS.
*     27/11/89  PLP   In GK1AID changed display area margins and the scale
*                     factor value, to reflect changes in printer settings
*                     at RAL. Also changed display area sizes of all WDTS
*                     and corrected the corrupted data in wdt2702.
*     06/12/89  PLP   Temporarily returned eoclip (tests showed it is
*                     needed, so have to use it until better solution is
*                     found). Took out scale factor declaration from
*                     GK1AID and put it in GK1AWD in order to fix the
*                     primitives facilities inquiries (S367) bug.
*     06/12/89  PLP   Corrected Inquire list element of available GDPs (S368).
*     11/12/89  PLP   Corrected text inquiries sections.
*     21/12/89  PLP   Changed save-restore handling in GK1APS and GK1ACL,
*                     so that they are first/last thing done in any frame.
*     27/02/90  PLP   Revised set text representation, text extent and
*                     text inquiries.(S370).
*     03/04/90  PLP   Corrected Inquire Polyline Representation and Inquire
*                     Polymarker Representation entries against the wrong
*                     values being returned for GREALIzed. Also corrected
*                     slight inaccuracies and variables naming in GK1APM,
*                     GK1AST and GK1AWD.
*     08/05/90  PLP   Introduced parameters IDDFRM and IDIRGM in order to
*                     have the driver's default deferral mode and the default
*                     implicit regeneration mode information stored.
*                     Subsequently altered the Open Workstation and Inquire
*                     Default Deferral State Values entries (S283 and S284).
*     22/06/90  RMK   Corrected error generated by Inquire Pick Device
*                     State (S383).
*     24/10/90  KEVP  In inQuire TeXt Facilities, delayed conversion of
*                     character heights to DC, so that it is done
*                     whether or not the workstation is open (C21).
*     25/10/90  KEVP  Ensured the inQuire TeXt Facilities, returns
*                     data even if list element is out of range (C55).
*     29/10/90  KEVP  Made it call GKPXAD to return correct values for
*                     inQuire PiXel Array Dimensions (C57).
*     07/11/90  KEVP  Removed code, no longer needed, because of C18 fix.
*     17/01/91  KEVP  Corrected GK1AXS to avoid division by zero when
*                     RUHH = 0.0 (S416).
*     23/01/91  PLP   Corrected the SC2700 value and the display surface
*                     size in all PostScript WDT files, so as to make sure
*                     no part of the display surface is clipped by the
*                     printer (S417).
*     29/01/91  PLP   Changed GK1AFL and GK1APS to introduce proper handling
*                     of transformed patterns (i.e. pattern size and pattern
*                     reference point - see S415).
*     30/01/91  KEVP  Made sure inQuire TeXt Facilities returns font 1 at
*                     ALL precisions (C27).
*     08/02/91  KEVP  Added line omitted from the fix to C27 (S443).
*     12/02/91  PLP   Changed the scan line index initialisation at the
*                     beginning of the function faput in GK1APS (S457).
*                     Also took out the call to hitdot in fapi - this was
*                     made obsolete by the introduction of the pattern
*                     size code.
*     13/02/91  KEVP  Removed all non-existent Hershey font numbers from
*                     the array IHFONT of Hershey font numbers.
*                     Parameterized its length to IFNTMX, which was then
*                     used for all the DO loops over IHFONT (S465).
*     24/02/91  DLT   Changed declaration of centre position arrays
*                     in GK1AXS so that upper bound is not specified.
*     11/04/91  KEVP  Eliminated false assumption that postscript
*                     coordinates are raster coordinates, deriving them
*                     always from DC scaled by SCFACT (C68). Defined
*                     SCFACT explicitly as number of Points per Metre.
*     28/05/91  KEVP  Enabled GK1ACA to use the 'image' operator directly
*                     for sloping cell array.
*     28/05/91  KEVP  Made sure EPSF starts new file for each new page,
*                     so that there is only one page in an EPSF file.
*                     All these changes were made in GK1ACL.
*     12/06/91  KEVP  Made GDP with the 'arc' and 'arcn' operators
*                     in new routine GK1ACC. This saves space and enables
*                     SOLID fill to be drawn properly.
*                     This involved moving the pattern filling into a new
*                     routine GK1APA called from both GK1ACC and GK1AFL.
*     12/06/91  KEVP  Moved bounding box data from header to trailer
*                     and get them from current workstation viewport.
*     13/06/91  KEVP  Began new postscript dictionary for use by the
*                     workstation ending it before the trailer. In EPSF,
*                     this prevents any definitions, from contaminating
*                     the including application.
*     14/06/91  KEVP  In the prologue procedure 'init', initialise the
*                     Current Transformation Matrix, only if non-EPSF.
*                     EPSF file assumes CTM to be initialised to default.
*     17/06/91  KEVP  Implemented Colour Cell Array. This involved
*                     replacing the function GK1AGL with a new routine
*                     GK1ACX, which provides the hexadecimal colour code
*                     for a given colour index in either monochrome or
*                     colour. Unlike GK1AGL, GK1ACX does not interfere
*                     with the WCA. Hence the WCA need not be saved.
*     19/06/91  KEVP  Implemented Colour Patterned Fill. Chunked the
*                     writing of pattern data to allow for wide patterns.
*     01/07/91  KEVP  Corrected BoundingBox Order to XMIN,YMIN,XMAX,YMAX.
*     25/07/91  KEVP  Put BoundingBox into integers to comply with EPSF
*                     structuring conventions.
*     29/07/91  KEVP  Header now '%!PS-Adobe-2.1 EPSF-2.0'.
*     01/08/91  KEVP  Fixed inQuire Colour Facil's for Colour PS (C88).
*     02/09/91  KEVP  Introduced variable formats, selected by GK1ASF,
*                     for coordinates in PolyLine, PolyMarker and
*                     Fill Area to save Filespace. In these cases, the
*                     maximum format was changed from F11.3 to F10.2.
*                     < Created 'SF, Changed 'FL, 'LN, 'PM >
*     03/09/91  KEVP  In comments, replaced system-dependent filenames
*                     with system-independent routine names.
*                     (eg, 'gk1afl.f' was changed to 'GK1AFL').
*                     < Changed 'ID and 'PS >
*     25/10/91  KEVP  Format for coordinates changed to have one instead
*                     of two digits after decimal point (ie max fmt F9.1)
*                     < Changed 'SF >
*     30/10/91  KEVP  Ensured that 'showpage' is done BEFORE correspond-
*                     -ing 'restore', for correct transformations (C90).
*                     < Changed 'CL >
*     12/11/91  KEVP  Introduced trapezoid decomposition for Fill Area
*                     too complex for Postscript to handle in one path
*                     (C69). < Created 'TF, Changed 'FL, 'PS >
*     25/11/91  KEVP  Made sure the BoundingBox is in DEFAULT Postscript
*                     Coordinates for all workstation types,
*                     especially, EPSF Landscape (type 2703) (C94).
*                     < Changed 'TR >
*     27/11/91  KEVP  Removed the saving of the WCA, by primitives. This
*                     is no longer necessary, since GK1AGL has been
*                     replaced by GK1ACX. < Changed 'FL,'LN,'PM,'RO,'XS >
*     28/11/91  KEVP  Wrote ALGORITHM Comments for GK1ACL.
*     03/12/91  KEVP  Included complete list of Device Subroutines
*                     in comments. < Changed 'WD >
*     12/12/91  KEVP  Changed FORTRAN format code for Cell Array
*                     transformation from '6F10.5' to '4F11.6, 2F11.3'.
*                     < Changed 'CA >
*
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
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkfls.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkio.cmn'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkxfd.cmn'
      INCLUDE '../../include/gktxb.cmn'
*
*  EXTERNAL FUNCTION DEFINITIONS
*  -----------------------------
*
      CHARACTER*1 GKAN1
*
*
*  DEVICE SUBROUTINES (Including EXTERNALS)
*  ----------------------------------------
*     GK1ABG  Device Background Colour Routine
*     GK1ACA  Cell array routine.
*     GK1ACB  Device Clipping Boundary Routine
*     GK1ACC  Arc rountine for GDPs
*     GK1ACL  Clear Page or Close File
*     GK1ACX  Colour Hexcodes Routine
*     GK1AFL  Fill Area routine (Solid or Patterned)
*     GK1AHD  Header writing routine
*     GK1AID  Initialise Device
*     GK1ALN  Solid / styled line output routine.
*     GK1APA  Patterned Fill Completion
*     GK1APM  Poly Marker routine
*     GK1APS  Prolog writine routine
*     GK1ARO  Raster output routine.
*     GK1ASF  Format Selection routine for coordinates
*     GK1ATF  Device Trapezoid Fill output routine
*     GK1AST  Setting of device colour, linetype and linewidth
*     GK1ATR  Trailer writing routine
*     GK1AXF  Supply hardware font details.
*     GK1AXS  Output hardware string.
*
      EXTERNAL GK1ALN, GK1ARO, GK1AXF, GK1AXS
*
*  LOCALS
*  ------
*     DUMMY       Dummy character, required by the buffering routine.
*     HERSH       Logical to signal that a Hershey font is requested.
*     I,J         Temporary integers
*     ICHUNK      Size of stack used for splitting polylines
*     IDDFRM      Default deferral mode.
*     IDIRGM      Default implicit regeneration mode.
*     IFILSC      Fill area scale factor (space between hatchlines)
*     IHFONT      Array of Hershey Font Identifiers
*     INKWI2      Temporary integer, stores type of returned values in inquires
*     INMAX       Maximum dimension of variables for hardware text's font and
*                 precision pairs.
*     INTA        Local integer array with multiple uses:
*     IOFF        Offset for stack
*     IPAGE       A flag to indicate that the action concerns a page and
*                 not the whole file.
*     IPxxxx      Array of hardware precisions for workstation xxxx
*     IPREC       Precision array for Text;
*                 ***** Note the local precision array will
*                 ***** disappear when there is a decent utility to
*                 ***** handle font names and precisions.
*     IPSFNT      Maximum number of PS fonts on current workstation.
*     IREM        Dummy integer, required by the buffering routine.
*     ITXF        Array of hardware fonts for current workstation
*     ITXP        Array of hardware precisions for current workstation
*     IWHOLE      A flag to indicate that the action concerns the whole
*                 postscript file and not just a page.
*     N           Count (temporary)
*                 - receiving WDT for Inq Text Facil (size 19)
*     REALA       Local real array with multiple uses:
*                  - receiving WDT for Inq Text Facil (size 12)
*                  - temporary to hold values for the clipping
*                    rectangle in Norm. Trans. entry
*     RVW         Real for temporary storage
*     RVS         Real for temporary storage
*
*
*     Offsets for Integer Workspace (KWKDAT)
*     --------------------------------------
*     ILNTYP=1   Current Linetype
*     IMKTYP=2   Current Markertype
*     IFASTY=4   Current Fill Area Style
*     ICLIND=5
*     ICHWFT=6
*     IPAGES=7   Current Number of Pages Completed
*     IFTINT=8   Font Parameter
*     IFTNAM=9   ..   ..
*     IFTPSE=10  ..   ..
*     IFTMAP=11  ..   ..
*     IFTUSD=12  ..   ..
*     IFTHDT=13  ..   ..
*     IORIEN=14  Page Orientation (Portrait,Lanscape)
*     IWIDTH=15  Page Width
*     IFORMT=16  Format flag to indicate whether EPSF (0,IIEPSF=1)
*     ICOLR=17   Workstation Monochrome (GMONOC) or Colour (GCOLOR)
*
      INTEGER    ILNTYP
      PARAMETER (ILNTYP =  1)
      INTEGER    IFTINT,      IFTNAM,      IFTPSE,        IFTMAP
      PARAMETER (IFTINT =  8, IFTNAM =  9, IFTPSE = 10,   IFTMAP = 11)
      INTEGER    IFTUSD,      IFTHDT,      IFORMT
      PARAMETER (IFTUSD = 12, IFTHDT = 13, IFORMT = 16)
*
*     Offsets for Real Workspace (QWKDAT)
*     -----------------------------------
*     ILNWID=1   Current Linewidth
*     IMKSZ=2    Current Markersize
*     ICCHHT=3   Current Character Height
*     ICCHAN=4   Current Character ???
*     ICLPXL=6   Clipping box
*     ICLPYB=7   ....     ...
*     ICLPXR=8   ....     ...
*     ICLPYT=9   ....     ...
*     IMARGN=10  Margin Width
*     ICCHXF=11
*
      INTEGER    ILNWID,      IMKSZ
      PARAMETER (ILNWID =  1, IMKSZ  = 2)
*
*     Other Parameters
*     ----------------
*     standard chunk for polyline,polymarker
      INTEGER    ICHUNK,     IFILSC
      PARAMETER (ICHUNK=200, IFILSC=1)

*     Page, Wholefile and EPSF flags
      INTEGER    IWHOLE,   IPAGE,   IIEPSF
      PARAMETER (IWHOLE=0, IPAGE=1, IIEPSF=1)

*     Conversion factor from DC (metres) to Postscript Coords (points).
*     NB: There are 72 points to an inch and 2.54cms to an inch.
      REAL       SCFACT
      PARAMETER (SCFACT = 72.0/0.0254)

*     Number of hardware fonts in WDT for each workstation
      INTEGER    IH2700,    IH2701
      PARAMETER (IH2700=13, IH2701=13)

*     maximum no of font and precision pairs (biggest of the above)
      INTEGER    INMAX
      PARAMETER (INMAX=13)

*     Number of Hershey fonts, other than 1
      INTEGER    IFNTMX
      PARAMETER (IFNTMX=KFNTMX-1)

*     default deferral mode and default implicit regeneration mode
      INTEGER IDDFRM, IDIRGM
      PARAMETER (IDDFRM=GASTI, IDIRGM=GSUPPD)
*
*
*     Variables
*     ---------

*     Hershey font text variables
      INTEGER IPREC(KFNTMX), IHFONT(IFNTMX)

*     Hardware font text variables
      INTEGER ITXF(INMAX), ITXP(INMAX)

*     miscellaneous variables
      INTEGER IOFF, I, N, INKWI2, INTA(19), IREM, IPSFNT
      REAL REALA(12), RVW, RVS
*
      CHARACTER DUMMY
*
      LOGICAL HERSH
*     Hershey font list
      DATA IHFONT/-101,-102,-103,-104,-105,-106,-107,-110,-115/

*
*  STACK USAGE
*  -----------
*
*     2*ICHUNK REAL     polyline, polymarker transformed points
*
*
*  ERRORS
*  ------
*
*       32   Specified workstation is not of category MO
*       34   Specified workstation is not of category MI
*       38   Specified workstation is neither of category INPUT nor of
*            category OUTIN.
*       41   Specified workstation is not able to generate GDP
*       93   Colour index invalid
*      102   GDP identifier is invalid
*      104   At least one active workstation is not able to generate the
*            specified G.D.P.
*      140   Specified input device is not present on this workstation
*      180   Specified escape function is not supported
*    -1009   Unable to access the database file
*     2002   List element or set member not available
*
*  COMMENTS
*  --------
*
*
* --------------------------------------------------------------------
*

*     Set the number of hardware fonts and text variables to the
*     to the values for the current workstation type.
*               They are not held in workstation workspace, because
*     we need to use them in inquiries that can be called when
*     the workstation is not opened (i.e. no initialisation has been
*     done and the workspace has rubbish in it).
*
      IF(KWKTYP.EQ.2700)THEN
          IPSFNT=IH2700
      ELSEIF(KWKTYP.EQ.2701)THEN
          IPSFNT=IH2701
      ELSEIF(KWKTYP.EQ.2702)THEN
          IPSFNT=IH2700
      ELSEIF(KWKTYP.EQ.2703)THEN
          IPSFNT=IH2701
      ELSEIF(KWKTYP.EQ.2720)THEN
          IPSFNT=IH2700
      ELSEIF(KWKTYP.EQ.2721)THEN
          IPSFNT=IH2701
      ELSEIF(KWKTYP.EQ.2722)THEN
          IPSFNT=IH2700
      ELSEIF(KWKTYP.EQ.2723)THEN
          IPSFNT=IH2701
      ELSE
          IPSFNT=KNIL
      ENDIF

*        Initialise hardware text variables (highest precision GCHARP).
         DO 5 I=1,IPSFNT
            ITXF(I)=-I
            ITXP(I)=GCHARP
    5    CONTINUE


* --------------------------------------------------------------------
* Conditional GOTO on entrypoint code
* --------------------------------------------------------------------
*     WS Entry : 1-119
      GOTO (       10,  20,  30,  40,  50,  60,  70,  80,9999,
     :       100, 110, 120, 130, 140, 150, 160, 170, 180, 190,
     :       200, 210,9999, 230, 240, 250, 260, 270, 280,9999,
     :      9999, 310, 320, 330,9999,9999,9999,9999,9999,9999,
     :      9999, 410, 410, 410, 410, 410, 410, 410, 410, 410,
     :       410, 410,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999, 610, 610, 610, 610, 610, 610, 670, 680, 690,
     :       690, 690, 690, 690, 690, 750, 750, 750, 750, 750,
     :       750,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999, 910, 920, 930) IENT
*     WS Entry : 120-206
      GOTO (1200,1111,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1111,1111,1111,1330,1111,1350,1111,1370,1380,1111,
     :      1111,1111,1111,1111,1440,1111,1460,1470,1470,1470,
     :      1470,1510,1470,1111,9999,9999,9999,9999,9999,9999,
     :      9999,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1700,1710,1720,1111,1740,1750,1111,1770,1111,1790,
     :      1111,1111,1111,1111,1111,1850,1111,1870,1880,1111,
     :      1111,1111,1920,1930,1930,1930,1930,1930,1930) IENT-119
      GOTO 9999



*===============================================================
*                 CONTROL FUNCTIONS
*===============================================================
*
* ---------------------------------------------------------------
* Open workstation
* ---------------------------------------------------------------
   10 CONTINUE
*     data expected
*     KWI1   connection identifier
*     KWKTYP
*     data returned
*     KWI1   workstation category (IO)
*     KERROR error response or zero

*     Set up workstation state list and WDT
      CALL GKIWSL(KWKIX,KWKTYP)
      IF (KERROR.EQ.0) THEN
*        Connection identifier
         KCID(KWKIX) = KWI1
*        Deferral mode - output at some time in the future (see above)
         KDFM(KWKIX) = IDDFRM
*        ws immediate output - no
         KWIO(KWKIX)= GNO
*        Implicit regeneration - suppressed (see above)
         KIMRGM(KWKIX) = IDIRGM
*        Ask operating system to make a connection.
         CALL GKIOOP(KFWKFW,KCID(KWKIX),KWCID(KWKIX))
         IF (KERROR.EQ.0) THEN
*           initialise device
            CALL GK1AID
         ENDIF
*        Set segment list pointer
         KSSGPT(KWKIX) = KNIL
      ENDIF
*     Set up workstation category and return
      KWI1 = GOUTPT
*
      GOTO 9999

* --------------------------------------------------------------
* Close workstation
* --------------------------------------------------------------
   20 CONTINUE
*     Data expected
*     KWI1 : first/second pass

*     First pass
      IF(KWI1.EQ.1) THEN
*        indicate second pass expected
         KWDONE=KRFUSE
*        second pass
      ELSE
*       picture clear
        CALL GK1ACL(IWHOLE)
*       end dictionary
        CALL GKFOCO(KIOPB,'end',IREM)
        CALL GKFOCO(KIOSN,DUMMY,IREM)
*       send the Trailer section
        CALL GK1ATR(IWHOLE)
*       Deallocate the heap
        CALL GKHPDA(KWKDAT(IFTNAM,KWKIX),KCHARS)
        CALL GKHPDA(KWKDAT(IFTPSE,KWKIX),KINTGS)
        CALL GKHPDA(KWKDAT(IFTMAP,KWKIX),KINTGS)
        CALL GKHPDA(KWKDAT(IFTUSD,KWKIX),KINTGS)
*       flush the buffer
        CALL GKFOCO(KIOSN,DUMMY,IREM)
*       disconnect ws
        CALL GKIOCL(KFWKFW,KCID(KWKIX),KWCID(KWKIX))
        CALL GKCWSL(KWKIX)
        CALL GKSLDL(KSSGPT(KWKIX))
      ENDIF
      GOTO 9999

* --------------------------------------------------------------
* Clear workstation (F/E announcement before clr surface)
* --------------------------------------------------------------
   30 CONTINUE
*     Data expected:
*     KWI1 : first/second pass
*     KWI2 : conditional - Clear Control Flag (ignored)


*     second pass
      IF (KWI1 .EQ. 2) THEN
*        indicate clear surface entry expected
         KWDONE = KRFUSE
*        delete ws segments
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
* Update workstation (F/E announcement before close wk)
* --------------------------------------------------------------
   50 CONTINUE
*     Data expected:
*     KWI1 : GPERFO    - Perform update?
      KWDONE = KRFUSE
      GOTO 9999

* --------------------------------------------------------------
* Set deferral state
* --------------------------------------------------------------
   60 CONTINUE
*     Data expected:
*     KWI1 : New deferral mode (ASAP,BNIG,BNIL,ASTI)
*     KWI2 : New implicit regeneration mode (SUPPRESSED,ALLOWED)

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
*     KWI1 : new frame action necessary at update

      CALL GKFOCO(KIOSN,DUMMY,IREM)
      KWI1 = KNFAUP(KWKIX)
      GOTO 9999

* --------------------------------------------------------------
* Clear display surface
* --------------------------------------------------------------
   80 CONTINUE
*     Data expected:
*     KWI1 : conditional - Clear Control Flag (COND,ALWAYS)
      IF(KWI1.EQ.GALWAY.OR.KDSMT(KWKIX).EQ.GNEMPT)
     :  CALL GK1ACL(IPAGE)
*     reset current wk window and viewport
      CALL GKWCLD
      GOTO 9999

* --------------------------------------------------------------
* Message
* --------------------------------------------------------------
  100 CONTINUE
      IF (NID.GE.1)  WRITE(KERRFL,111) (GKAN1(IDAT(I)),I=1,NID)
  111                FORMAT(80A1)
      GOTO 9999

* --------------------------------------------------------------
* Escape
* --------------------------------------------------------------
  110 CONTINUE
*     Data expected:
*     KWI1 : function identifier
      KERROR = 180
      GOTO 9999

*===============================================================
*                OUTPUT FUNCTIONS
*===============================================================

* --------------------------------------------------------------
* Polyline
* --------------------------------------------------------------
  120 CONTINUE
      IF(KCVIS.EQ.GINVIS)GOTO 9999
*     Get enough stack space for a chunk of transformed points
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN
*        Set up device colour, linestyle and linewidth.
         CALL GK1AST(KWPLCI(KWKIX),KWLNTY(KWKIX),QWLNWD(KWKIX))
*        Output in manageable polyline sections
         N = ICHUNK
         DO 122 I=1,NRD,ICHUNK-1
            IF (NRD-I.LT.ICHUNK-1) N = NRD-I + 1
            IF (N.EQ.1) GOTO 122
            CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
            CALL GK1ALN(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
  122    CONTINUE
         CALL GKSTDA(KREALS,IOFF)
      ENDIF
      GOTO 8888

* --------------------------------------------------------------
* Polymarker
* --------------------------------------------------------------
  130 CONTINUE
      IF(KCVIS.EQ.GINVIS)GOTO 9999
*     Transform polymarker points and output them a chunk at a time.
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN
*        Set up device colour, nominal linestyle and nominal linewidth.
         CALL GK1AST(KWPMCI(KWKIX),1,QNMLNW(KWKIX))
*        Output in manageable polymarker sections
         N = ICHUNK
         DO 132 I=1,NRD,ICHUNK
            IF (NRD-I.LT.ICHUNK) N = NRD-I + 1
            CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
            CALL GK1APM(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
  132   CONTINUE
        CALL GKSTDA(KREALS,IOFF)
      ENDIF
      GOTO 8888

* --------------------------------------------------------------
* Text
* --------------------------------------------------------------
  140 CONTINUE
      IF(KCVIS.EQ.GINVIS)GOTO 9999
*     Set up device colour, nominal linestyle and nominal linewidth.
      CALL GK1AST(KWTXCI(KWKIX),1,QNMLNW(KWKIX))
      IF(KWTXPR(KWKIX).EQ.GSTRKP) THEN
*        Stroke precision text
         CALL GKXDWO(NID,IDAT,GK1ALN)
      ELSE
*        Char or String precision text.
         CALL GKXDCS(NID,IDAT,QWCHWX(KWKIX),QWCHWY(KWKIX),
     :               GK1AXF,GK1AXS)
      ENDIF
      GOTO 8888

* --------------------------------------------------------------
* Fill area
* --------------------------------------------------------------
  150 CONTINUE
      IF(KCVIS.EQ.GINVIS)GOTO 9999
*     Set up device colour, nominal linestyle and nominal linewidth.
      CALL GK1AST(KWFACI(KWKIX),1,QNMLNW(KWKIX))
*     Fill the area - utility for hatch & hollow , own routine for
*     the rest.
      IF(KWFAIS(KWKIX).EQ.GHATCH.OR.KWFAIS(KWKIX).EQ.GHOLLO)THEN
         CALL GKFILS(NRD,RX,RY,IFILSC,GK1ALN,GK1ARO)
      ELSE
         CALL GK1AFL(NRD, RX, RY)
      ENDIF
      GOTO 8888

* --------------------------------------------------------------
* Cell array
* --------------------------------------------------------------
  160 CONTINUE
      IF(KCVIS.EQ.GINVIS)GOTO 9999
      CALL GK1ACA(KWI1,KWI2,IDAT)
      GOTO 8888

* --------------------------------------------------------------
* GDP
* --------------------------------------------------------------
  170 CONTINUE
      IF(KCVIS.EQ.GINVIS)GOTO 9999
*     First, check GDP identifier
      IF (KWI1.EQ.0) THEN
        KERROR = 102
        GOTO 9999
      ELSE IF (KWI1.LT.-4.OR.KWI1.GT.-1) THEN
        KERROR = 104
      GOTO 9999
      ELSE IF (KWI1.EQ.-1) THEN
*       Arc - use polyline attributes.
        CALL GK1AST(KWPLCI(KWKIX),KWLNTY(KWKIX),QWLNWD(KWKIX))
        CALL GK1ACC(KWI1,NRD,RX,RY)
      ELSE
*       Filled chord, pie or circle - use fill area attributes
        CALL GK1AST(KWFACI(KWKIX),1,QNMLNW(KWKIX))
        IF(KWFAIS(KWKIX) .EQ. GHATCH)THEN
          CALL GKCRCS(KWI1,NRD,RX,RY,IFILSC,.FALSE.,10.0,GK1ALN,GK1ARO)
        ELSE
          CALL GK1ACC(KWI1,NRD,RX,RY)
        ENDIF
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
      IF(KWLNTY(KWKIX).LT.1 .OR. KWLNTY(KWKIX).GT.5) KWLNTY(KWKIX) = 1
      IF(KWPLCI(KWKIX).GE.KPCI(KWKIX)) KWPLCI(KWKIX) = 1
*     QWLNWD holds linewidth scale factor --> work out the actual linewidth
      QWLNWD(KWKIX) = QWLNWD(KWKIX)*QNMLNW(KWKIX)
      GOTO 9999

* --------------------------------------------------------------
* Set polymarker attributes
* --------------------------------------------------------------
  190 CONTINUE
      CALL GKDPMB
*     Need to check because individual settings won't have been checked.
      IF(KWMKTY(KWKIX).LT.1 .OR. KWMKTY(KWKIX).GT.5) KWMKTY(KWKIX) = 3
      IF(KWPMCI(KWKIX).GE.KPCI(KWKIX)) KWPMCI(KWKIX) = 1
*     QWMKSZ holds marker size scale factor --> work out the actual marker size
      QWMKSZ(KWKIX) = QWMKSZ(KWKIX)*QNMMKS(KWKIX)
      GOTO 9999

* --------------------------------------------------------------
* Set  text attributes
* --------------------------------------------------------------
  200 CONTINUE
      CALL GKDTXB
*     Need to check because individual settings won't have been checked.
      IF(KWTXCI(KWKIX).GE.KPCI(KWKIX)) KWTXCI(KWKIX) = 1
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
*        Do we recognise this hardware font?
         DO 207 I=1,IPSFNT
*           Yes - leave the loop.
            IF (KWTXFN(KWKIX).EQ.ITXF(I)) GOTO 9999
  207    CONTINUE
*        No - set to default font.
         KWTXFN(KWKIX) = ITXF(1)
      ENDIF
*
      GOTO 9999

* --------------------------------------------------------------
* Set fill area attributes
* --------------------------------------------------------------
  210 CONTINUE
      CALL GKDFAB
*     Need to check because individual settings won't have been checked.
      IF (KWFAIS(KWKIX).EQ.GHATCH.AND.
     :    (KWFASI(KWKIX).LT.-10.OR.KWFASI(KWKIX).GT.-1))
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
      IF(KWFACI(KWKIX).GE.KPCI(KWKIX)) KWFACI(KWKIX) = 1
      GOTO 9999

*===============================================================
*                WORKSTATION ATTRIBUTES (REPRESENTATIONS)
*===============================================================

* --------------------------------------------------------------
* Set polyline representation
* --------------------------------------------------------------
  230 CONTINUE
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
  240 CONTINUE
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
  250 CONTINUE
*     Data expected:
*     KWI1   : text index
*     KWI2   : font
*     KWI3   : precision  (STRING,CHAR,STROKE)
*     QWR1   : character expansion factor
*     QWR2   : character spacing factor
*     KWI4   : text colour index
*     Data returned:
*     KERROR : error indicator

*     Is the requested font a Hershey font?
      DO 252 I=1,IFNTMX
        IF (KWI2.EQ.IHFONT(I)) THEN
            HERSH = .TRUE.
*           Make sure that fonts are available.
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

*     Deal with hardware fonts first.
  253 IF(KWI2.LE.ITXF(1) .AND. KWI2.GE.ITXF(IPSFNT)) THEN
         CALL GKSRTX(IPSFNT,ITXF,ITXP,.FALSE.)
      ELSEIF(KWI2.EQ.1) THEN
*     Font 1 is a special case
        INTA(1)=1
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

*     pattern
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
*
*     See if colour index valid
      IF (KWI1.GE.0 .AND. KWI1.LT.KPCI(KWKIX)) THEN
        QHP(KHPXR(KCTBPT(1,KWKIX))+KWI1) = QWR1
        QHP(KHPXR(KCTBPT(2,KWKIX))+KWI1) = QWR2
        QHP(KHPXR(KCTBPT(3,KWKIX))+KWI1) = QWR3
*     See if regeneration necessary
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
*     Multiply the current workstation total transformation matrix by
*     an additional matrix to change device coordinates (Metres) into
*     postscript coordinates (Points).
*
      DO 315 I=1,6
        QWTOTT(I,KWKIX)=QWTOTT(I,KWKIX)*SCFACT
  315 CONTINUE
*
*     Transform clipping rectangle to postscript coordinates
*
      QWCLXL(KWKIX)=QWCLXL(KWKIX)*SCFACT
      QWCLYB(KWKIX)=QWCLYB(KWKIX)*SCFACT
      QWCLXR(KWKIX)=QWCLXR(KWKIX)*SCFACT
      QWCLYT(KWKIX)=QWCLYT(KWKIX)*SCFACT
*
*     Now set the clipping boundary on the PostScript device
*
      CALL GK1ACB
*
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

*     Set up requested viewport
      CALL GKSWKV
      GOTO 9999

* --------------------------------------------------------------
* Segment entrypoints (410-510)
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
*     No input devices
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
*     No input devices
      KERROR = 38
      GOTO 9999


*===============================================================
*                INPUT FUNCTIONS - REQUEST
*===============================================================

* --------------------------------------------------------------
* Request locator/stroke/valuator/choice/pick/string
* --------------------------------------------------------------
  690 CONTINUE
*     No input devices
      KERROR = 38
      GOTO 9999


*===============================================================
*                INPUT FUNCTIONS - SAMPLE
*===============================================================

* --------------------------------------------------------------
* Sample locator/stroke/valuator/choice/pick/string
* --------------------------------------------------------------
  750 CONTINUE
*     No input devices
      KERROR = 38
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
 1330  CONTINUE
*     Input data :
*     KWI1   : polyline index
*     KWI2   : type of returned value ( GSET or GREALIzed )
*     Data returned:
*     KERROR : error indicator
*     KWI1   : linetype
*     KWI2   : polyline colour index
*     QWR1   : linewidth scale factor

      INKWI2 = KWI2
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      IF(INKWI2.EQ.GREALI) THEN
*       Validate the returned width.
        RVW = AMAX1(QMNLNW(KWKIX),QWR1)
        RVW = AMIN1(QMXLNW(KWKIX),RVW)
*       Return linewidth scale factor, not the actual linewidth
        QWR1 = RVW/QNMLNW(KWKIX)
        IF(KWI1.GT.5) KWI1 = 1
        IF(KWI2.GE.KPCI(KWKIX)) KWI2 = 1
      ENDIF
      GOTO 9999

* --------------------------------------------------------------
* Inquire polymarker representation
* --------------------------------------------------------------
 1350 CONTINUE
*     Input data :
*     KWI1   : polymarker index
*     KWI2   : type of returned value ( GSET or GREALIzed )
*
*     Data returned:
*     KERROR : error indicator
*     KWI1   : marker type
*     KWI2   : polymarker colour index
*     QWR1   : marker size scale factor

      INKWI2 = KWI2
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      IF(INKWI2.EQ.GREALI) THEN
*       Validate the returned size.
        RVS = AMAX1(QMNMKS(KWKIX),QWR1)
        RVS = AMIN1(QMXMKS(KWKIX),RVS)
*       Return marker size scale factor, not the actual marker size
        QWR1 = RVS/QNMMKS(KWKIX)
        IF(KWI1.GT.5) KWI1 = 1
        IF(KWI2.GE.KPCI(KWKIX)) KWI2 = 1
      ENDIF
      GOTO 9999

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
*       Is the font in question a Hershey font?
        DO 1371 I=1,IFNTMX
           IF (KWI1.EQ.IHFONT(I)) THEN
              HERSH = .TRUE.
              GOTO 1372
           ENDIF
 1371   CONTINUE
        HERSH = .FALSE.

*       String and Char hardware fonts
 1372   CONTINUE
*       A recognised hardware font?
        DO 1373 I=1,IPSFNT
           IF(KWI1.EQ.ITXF(I).AND.KWI2.LE.ITXP(I)) GOTO 1379
 1373   CONTINUE
*       Hershey stroke fonts
        IF(HERSH) KWI2 = GSTRKP
*       default font
        KWI1=1
 1379   CONTINUE
        IF(KWI3.GE.KPCI(KWKIX)) KWI3 = 1
      ENDIF
      GOTO 9999

* --------------------------------------------------------------
* Inquire Text Extent.
* --------------------------------------------------------------
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
*       Baseline vector from WS Set Text Attributes entry.
        CALL GKXQXC (NID,IDAT,QWCHWX(KWKIX),QWCHWY(KWKIX),
     :                  RX,RY,GK1AXF)

      ENDIF
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

      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      IF(KWI2.EQ.GREALI)THEN
         IF(KWKTYP .LT. 2720)THEN
*          Workstation types less than 2720 assumed to be Monochrome
           QWR1 = 0.3 * QWR1  +  0.59 * QWR2  +  0.11 * QWR3
           QWR2 = QWR1
           QWR3 = QWR1
         ENDIF
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
* Inquire locator/stroke/valuator/choice/string device state
* --------------------------------------------------------------
 1470 CONTINUE
*     PostScript Devices have no input devices
      KERROR = 38
      GOTO 9999

* --------------------------------------------------------------
* Inquire pick device state
* --------------------------------------------------------------
 1510 CONTINUE
      KERROR = 37
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

      KWI1 = GRASTR
      GOTO 9999

* ---------------------------------------------------------------
* Inquire maximum display surface size
* ---------------------------------------------------------------
 1720 CONTINUE
*     Data returned
*     KERROR : error indicator
*     KWI1   : device coordinate units
*     KWI2   : X maximum display surface
*     KWI3   : Y in raster units
*     QWR1   : X maximum display surface
*     QWR2   : Y in device units

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
*
      QWR1 = QWR1/SCFACT
      QWR2 = QWR2/SCFACT
      QWR3 = QWR3/SCFACT
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
*
      QWR1 = QWR1/SCFACT
      QWR2 = QWR2/SCFACT
      QWR3 = QWR3/SCFACT
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
*     KWI5   : number of available character expansion factors
*     KWI6   : number of predefined text indices
*     QWR1   : minimum character height
*     QWR2   : maximum character height
*     QWR3   : minimum character expansion factor
*     QWR4   : maximum character expansion factor

*     no of text font and precision pairs
      N=IPSFNT+3+IFNTMX
      IF (KWI1.LE.0 .AND. KWI1.GT.N) THEN
        KERROR = 2002
      ELSE
*       Font 1 at each precision
        IF(KWI1 .GT. 0 .AND. KWI1 .LE. 3) THEN
          KWI2 = 1
          KWI3 = KWI1 - 1
*       Hershey
        ELSEIF(KWI1 .GT. 3 .AND. KWI1 .LE. IFNTMX+3) THEN
          KWI2 = IHFONT(KWI1-3)
          KWI3 = GSTRKP
*       Hardware fonts
        ELSEIF(KWI1 .GT. IFNTMX+3 .AND. KWI1 .LE. N) THEN
          KWI2 = ITXF(KWI1-IFNTMX-3)
          KWI3 = ITXP(KWI1-IFNTMX-3)
       ENDIF
      ENDIF
      KWI1 = N
*     If workstation open return current values:
      IF(KWKIX.NE.KNIL) THEN
        KWI4 = KCHH(KWKIX)
        KWI5 = KCHXPF(KWKIX)
        KWI6 = KPTXI(KWKIX)
        QWR1 = QMNCHH(KWKIX)
        QWR2 = QMXCHH(KWKIX)
        QWR3 = QMNCHX(KWKIX)
        QWR4 = QMXCHX(KWKIX)
      ELSE
*       Workstation closed - return WDT values:
        CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
        IF (KERROR.EQ.0) THEN
          KWI4 = INTA(5)
          KWI5 = INTA(6)
          KWI6 = INTA(10)
*
          QWR1 = REALA(9)
          QWR2 = REALA(10)
          QWR3 = REALA(11)
          QWR4 = REALA(12)
        ENDIF
*       Convert character heights to DC
        QWR1 = QWR1/SCFACT
        QWR2 = QWR2/SCFACT
      ENDIF
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

      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KWI1 = KWI3
      IF(KWKTYP .GE. 2720) KWI2 = GCOLOR
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
*
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
*     No input devices
      KERROR = 38
      GOTO 9999

* --------------------------------------------------------------
* Inquire default locator/stroke/valuator/choice/pick/string device data
* --------------------------------------------------------------
 1930 CONTINUE
*     No input devices
      KERROR = 38
      GOTO 9999

* --------------------------------------------------------------
* Common end path for output primitives
* --------------------------------------------------------------
 8888 CONTINUE
*     Output makes paper not empty
      KDSMT(KWKIX) = GNEMPT
*     Flush buffer if necessary
      IF(KWIO(KWKIX).EQ.GYES) CALL GKFOCO(KIOSN,DUMMY,IREM)

* --------------------------------------------------------------
* Return
* --------------------------------------------------------------
 9999 CONTINUE
      RETURN
      END
