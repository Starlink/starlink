      SUBROUTINE GK0TWD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)

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
*     Driver for Tektronix storage tubes 4010 and 4014 with
*     Extended Graphics Module and certain emulators with simple
*     extensions.
*
*  MAINTENANCE LOG
*  ---------------
*     13/01/84  AS    Original version stabilized
*     20/01/84  JRG   Correct CLEAR & CLOSE WKSTN; remove REDRAW ONE SEG
*     17/02/84  JRG   Correct some inquiries
*       9/5/84  JRG   Add inquire text extent, clear up inq fill area facil.
*     05/07/84  JRG   Add Request Locator and Stroke.
*     12/07/84  JRG   Correct the end of buffer effects in polyline and
*                     (in GK0TLN) correct value of IBAUD
*     13/07/84  JRG   Put SYN's after clear screen instead of ESC ENQ
*     17/07/84  JRG   Add choice and valuator
*     24/07/84  RMK   In GK0TLN, re-send last point of previous buffer
*                     at start of new buffer.
*     25/07/84  JRG   Minimal simulation for cell array
*     27/07/84  JRG   Sort out font lists (bug S63)
*     03/08/84  JRG   Further correction to font lists (need to ensure
*                     that fonts file is open)
*     08/08/84  RMK   In GK0TLN, removed code which filtered out zero
*                     length vectors.
*     09/08/84  RMK   Removed dummy entries 192 to 195 and 198 inquiries.
*                     Fixed Inquire default choice device data (entry 196)
*                     Flush typeahead buffer in Request choice.
*                     Fixed Inquire locator and stroke device state entries.
*     21/01/85  GGT   Support for hardware characters (S102). Routines
*                     GK0TXC and GK0TXF are added (based on Cal 81).
*     23/01/85  KWB   Support for Tektronix 4010 lookalikes added *
*     04/03/85  KWB   Master release prepared
*     04/06/85  RMK   Added Inquire Fill Area Facilities entry, as utility
*                     assumes all interior styles are available (S135).
*                     In GK0TLN, initialised local variables LLIY,etc. (S110)
*                     and increased from 16 the value used in test to set
*                     BIGGAP.
*     17/06/85  RMK   Send US on Close Workstn.
*     18/06/85  RMK   Removed output of GS after character (in GK0TXC)
*     03/09/85  RMK   Simplified Polyline entry, as GKLCLP doesn't call GKLTYP
*                     for solid lines (S155).
*                     Inquire Colour Repres corrected for realised values
*                     (S147). Removed initialisations of QWOLDX,QWOLDY (S158).
*                     In Request Valuator, corrected formats used by internal
*                     reads (S159). Also output range of values.
*     08/10/85  JRG   Removed blank line after last END.
*                     Ensure that GK0TXC output is not split.
*                     Add T4014 and some emulators (Cifer 2634).
*                     Change QWCHRX/Y to QWCHWX/Y (remove redundancy).
*                     Change routines specific to emulators to be 0E not 0T.
*                     GK0TPD now accepts ch sizes as REAL arguments.
*                     Generalise the number of syn chars after lines.
*                     Output not drawn if invisible segment being created.
*     24/04/86  RMK   Increased repeat length for 4014 broken lines.
*                     Changed GK0TLN so that values passed to GKIOBO are
*                     array elements (S181).
*                     Changed to use new fill area utility.
*     25/04/86  RMK   Changed to use new interface to GKIOOP/GKIOCL.
*     28/04/86  RMK   Removed GK0TMV, as GK0TLN can do a move (S179).
*     29/04/86  RMK   Split linestyle-setting code into a separate
*                     routine. Added code for GDP. Removed extra arguments
*                     from GKIOCO calls in GK0TPD. Removed unused local
*                     variables from all routines.
*     25/06/86  RMK   Corrected earlier fix to S147, Inquire Colour Repres.
*                     Tightened up error checking in Inquire Fill Area Facils.
*                     Changed use of ICHAR in choice entry to GKNA1 - but this
*                     needs to be tidied up properly.
*     14/10/86  RMK   Added DLT's code for Cifer T5 and Pericom Monterey.
*     25/11/86  RMK   Changed all input code to send the prompt with
*                     GKIOBI/CI. Created GK0TGI (replacing GK0TPD) which gets
*                     the input as well as outputting the prompt.
*     14/01/87  RMK   Simplified request valuator entry by using a single
*                     internal READ with Fnout.0 format (S225).
*                     In request choice entry, used char variable rather than
*                     constant in GKNA1 call (S224).
*     19/01/87  RMK   IS conversion. Changed GDP entry to use negative
*                     GDP identifiers. Added code to do GDP inquiries.
*                     Changed linetype test in set PL attributes entry.
*                     Changed markertype test in set PM attributes.
*                     Changed set FA attributes to use -ve hatch styles.
*                     Altered checking of hatch style index in inquire
*                     fill area facilities.
*     21/01/87  RMK   IS conversion. Changed request choice to return
*                     status GNCHOI for choice 0.
*     22/01/87  JCS   IS conversion. Error number changes.
*     09/06/87  RMK   Removed use of GCIRCL, GARC (S227).
*     22/06/87  RMK   At close workstation, move to top left hand
*                     corner (S226).
*     24/06/87  RMK   Changed GK0TGI to allow input to be echoed.
*     25/06/87  RMK   Changed valuator and choice to return QNIL/KNIL
*                     if input is invalid (S223).
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*     06/07/87  RMK   Changed Inquire GDP to use IS Fortran binding names
*                     for GDP attributes.
*     08/09/87  PLP   In GK0EGI added the 5th parameter to the list in the
*                     GKIOCI call.
*     21/09/87  PLP   Introduced branching in GK0TCS to allow for the non-
*                     standard "cursor-on" and "cursor-off" requirements
*                     of the lookalikes.
*     30/09/87  PLP   Implemented drawing in background colour ("Line
*                     Erase Mode"), ie added code to the main driver'
*                     routine, GK0TSL; corrected & activated GK0ESC.
*     02/10/87  PLP   Removed unnecessary code from and tightened up
*                     GK0EGI. Improved mode selection (alpha to graphics
*                     and back) protocol for Pericom Monterey. Made
*                     Update Workstation Entry aware of the devices
*                     with scrollable alpha screens.
*     19/10/87  PLP   Extended GK0TGH, GK0TSH, GK0TXF and GK0TXC so as to
*                     include emulators+ with a range of hardware font
*                     character sizes. Corrected font details for Pericom,
*                     removed character padding and one typos from GK0TXF.
*     26/10/87  PLP   Improved Pericom Monterey' hardware font geometry.
*                     In GK0TXC corrected and expanded code to determine
*                     the character starting position. Updated commenting
*                     throughout, now that the clear-cut distinction
*                     between Tek and emulators+ code is gone.
*     13/11/87  RMK   Corrected typo in Inquire list element of GDPs (S273).
*     16/11/87  PLP   Close Workstation Entry now treats Tek and the
*                     emulators separately.
*     18/11/87  RMK   Changed request choice entry to expect 6
*                     integers in GKRQIP call.
*     18/11/87  PLP   Following the Pericom pattern, completed the
*                     Cifer T5 Emulator. Routines changed or expanded:
*                     GK0TSH, GK0EBF, GK0ECL, GK0TXF, GK0TXC, GK0TLN,
*                     GK0TSL, GK0EGI, GK0ESC, GK0TGH, GK0TWD - consult
*                     internal comments for details.
*     27/11/87  PLP   Introuduced subroutine GK0EUW to cope with the
*                     Update Workstation requirements of the Lookalikes
*                     in a uniform manner.
*     07/12/87  PLP   Completed the Cifer 2634 Emulator. Routines changed
*                     or expanded: GK0EBF, GK0ECL, GK0EGI, GK0EUW. Consult
*                     internal comments for details.
*     11/12/87  PLP   Expanded the Pericom Emulator to cope with the
*                     Standard Pericom Monterey MG200.
*     14/12/87  PLP   Replaced Sample Input dummy entries by the 9999's
*                     in the opening computed GOTO.
*     12/01/88  PLP   Added necessary code so that for emulators the
*                     Message is sent to the alphanumeric screen.
*     23/01/88  PLP   Introduced GKRQCH, the Request Choice utility.
*                     Consequently updated the Request choice and Inquire
*                     default choice device data entries, removed obsolete
*                     local variables and GKNA1 function declaration.
*     01/02/88  PLP   Updated the Valuator, Choice and String echo areas
*                     of WDT files concerned.
*     23/06/88  RMK   Added support for escape functions -1 and -2.
*     17/04/89  KEVP  Implemented STRING input using GKRQST.
*     17/04/89  KEVP  Removed blank lines between subroutine and put in
*                     entrypoint names eg, KFA in brackets at entrypoints
*                     to aid search for required entrypoint when editing.
*     20/06/89  KEVP  Introduced PICK, using GKRQPK without echoplay.
*     20/06/89  KEVP  Enabled break to be used in LOCATOR and PICK input,
*                     by getting GK0TCS to output 26 (control-Z) rather
*                     than 13 (carriage return) in the event of a break.
*     21/06/89  KEVP  Put echoplay into pick using new utility GKEPBB.
*     31/07/89  RMK   Added entry for inquire set member of segment
*                     names on workstation (S362).
*     20/11/89  RMK   Added code in entry for GKQDPK; added argument
*                     list to GKQWK call in GQPKS entry.
*     09/02/90  RMK   Added initialise input - use calls of GKINIP in
*                     the initialise entries.
*     03/05/90  RMK   In request valuator entry, added check on KERROR
*                     after GKRQIP call.
*     17/05/90  RMK   In initialise input entries, added check of the
*                     PET number (S384).
*     14/06/90  KEVP  Removed debug statement from pick echoplay.
*     25/10/90  KEVP  Made Inquire Text and Fill Area Facilities
*                     entry points return data even if requested
*                     list element is out of range (C55).
*     07/12/90  PLP   Removed unused locals.
*     11/01/90  PLP   Corrected calling sequence in the GKEPBB call (S436).
*     21/02/91  KEVP  Quadrupled marker sizes for workst'n 203 in WDT
*                     (S446,S467).
*     26/02/91  KEVP  Removed from GK0TGI code, that's obselete, since
*                     GKIOCI handles both prompt and echo in one line
*                     (S472).
*     11/03/91  KEVP  Scaled up pick aperture for high resolution workstation.
*     01/08/91  KEVP  Moved Stack allocation for Polyline to after the
*                     colour index check for correct stack usage (C89).
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
      INCLUDE '../../include/gkinp.par'
      INCLUDE '../../include/gkfls.par'
      INCLUDE '../../include/gkwke.par'
      INCLUDE '../../include/gkpca.par'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkpca.cmn'
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
      EXTERNAL GK0TLN, GK0TXF, GK0TXC, GK0TCS, GK0TGI, GK0EGI,
     :         GK0TPP
*
* EXTERNAL FUNCTION DEFINITIONS
* -----------------------------
*
      CHARACTER*1 GKAN1
*
*  LOCALS
*  ------
*     .... Start of offsets and constants in KWKDAT & QWKDAT workspace ....
*     IBAUD  Offset for baud rate for this terminal
*     ICHSIZ Offset for whether number of hardware character sizes is
*            1 (GLOWER) or greater (GHIGHR)
*     ILTS   Similarly for linetypes: 1.i.e. solid (GLOWER) or
*            greater (GHIGHR)
*     ICLAS  Offset for whether this workstation is Tektronix storage
*            tube or whether it is an extended emulator (emulator+).
*     ICHSZD Offset for identifier of selected character height on a
*            workstation where there is a range of hardware character
*            heights, eg. Tek 4014, Pericom Monterey etc. 1 is smallest
*            and N (= 4 for Tek 4014 and 16 for Pericom) is largest. This
*            excludes spacing between the characters. Set up by the 'Set
*            Text Attributes' entry.
*     ITEK   Value that indicates Tektronix storage tube
*     IEMUL  Value that indicates emulator+
*     ICHFLG Stored choice data flag: can be GYES, GNO or KNIL
*            if there is no associated device
*     ICHSTA Choice status  }  stored choice data
*     ICHNUM Choice number  }
*     ILCFLG Stored locator data flag: can be GYES, GNO or KNIL
*            if there is no associated device
*     ILCSTA Locator status         }
*     ILCX   Locator X coord        }  stored locator data
*     ILCY   Locator Y coord        }
*     .... End of offsets and constants in KWKDAT & QWKDAT workspace ....
*     IFILSC Fill area scale factor to pass to utility
*     ICHUNK Number of points in each chunk of output primitive
*     ICTLZ  ASCII code for ctrl/Z
*     IOFF   Stack offset for output primitive chunks
*   IOFFX,IOFFY Offsets for stack holding coordinates of initial stroke
*     IUS(1) Holds ASCII code for US
*     NOUT   Number of bytes returned on input
*     I      Various
*     N      Various
*     NLEFT  Returned by output buffering routine (amount of buffer left)
*     ICODE  Key hit returned with cursor position
*     NIPTS2 Number of points in initial stroke after checking
*     INTA   Local integer array with multiple uses
*              - receiving input device state, largest is stroke (size 10)
*              - receiving WDT info for Inq Text Facil (size 19 max)
*              - sending linetype code (size 2)
*     IPREC  Stores available text precisions
*     PATPD  Linetype pattern period. Varies with resolution.
*     REALA  Local real array with multiple uses
*              - receiving input device state, largest is valuator (size 7)
*              - receiving WDT info for Inq Text Facil (size 12 max)
*    XDC,YDC 2-element arrays for use in several entrypoints to
*            hold location in DC.
*     HARD   .TRUE. iff char size being altered by hardware
*     SOFT   .TRUE.  if linetype being done by software
*     VALSTR String of characters representing valuator input
*     PROMPT The prompt preceding input implemented by keyboard
*     FORMT  Character variable to hold format in Request Valuator
*     IERASE Flag to switch the emulator from line draw (dft) to line erase mode
*     IDRAW  Flag to switch the emulator from line erase back to line draw  mode
*     IPRMST Prompt-Start for STRING input
*
*     .... Start of offsets and constants in KWKDAT & QWKDAT workspace ....
      INTEGER IBAUD, ICHSIZ, ILTS, ICLAS,ICHSZD
      PARAMETER (IBAUD=1, ICHSIZ=2, ILTS=3, ICLAS=4, ICHSZD=5)
      INTEGER IEMUL,ITEK
      PARAMETER (IEMUL=0,ITEK=1)
      INTEGER ICHFLG, ICHSTA, ICHNUM
      PARAMETER (ICHFLG=KMXWKI, ICHSTA=KMXWKI-1, ICHNUM=KMXWKI-2)
      INTEGER ILCFLG, ILCSTA, ILCX, ILCY
      PARAMETER (ILCFLG=KMXWKI-3, ILCSTA=KMXWKI-4)
      PARAMETER (ILCX=KMXWKR, ILCY=KMXWKR-1)
*     .... End of offsets and constants in KWKDAT & QWKDAT workspace .....
      INTEGER IFILSC
      INTEGER    ICHUNK, ICTLZ
      PARAMETER (ICHUNK=200, ICTLZ=26)
      INTEGER IOFF, IOFFX,IOFFY, NOUT, I, N,
     :    NLEFT, ICODE, NIPTS2, IERASE, IDRAW
      PARAMETER (IERASE=0,IDRAW=1)
      INTEGER INTA(19),IPREC(KFNTMX),IUS(1),IPRMST(8)
      REAL PATPD
      REAL REALA(12), XDC(2),YDC(2)
      LOGICAL HARD, SOFT
      CHARACTER VALSTR*12, PROMPT*50, FORMT*7
      DATA IUS(1)/31/
      DATA IPRMST /83,116,114,105,110,103,63,58/
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
*     102   GDP identifier is invalid
*     104   At least 1 active workstation cannot generate GDP
*     180   Specified escape function is not supported
*     302   I/O error on read
*   -1009   Font file not available
*    2002   List element not available
*   -2010   This driver is called with wrong workstation type
*
*  COMMENTS
*  --------
*      Facilities of T4014 used over and above T4010
*        - high resolution addressing (Extended Graphics Module); (so far
*          we only recognise high res D.C., not act upon it)
*        - control of character size by hardware
*        - control of linetype (except 5) by hardware (EGM)
*      Emulators+ supported (i.e. those emulators for which additional
*      facilities are recognised in this driver)
*        Cifer 2634
*          Cifer 2634 orders used are as follows (Cifer interim guide
*          is totally misleading) ... (ESC=27, UPARROW=94)
*          Note orders from host are destined for either the graphics
*          or alpha screen and the visibility of each screen does
*          not affect the routing (changes can happen invisibly).
*          Switch destination to be graphics ESC UPARROW DC2 (ctrlR=18)
*          Switch destination to be alpha     ESC UPARROW T (=84)
*          Switch current screen to be visible ESC  Z (=90)
*          Switch current screen to be invis  ESC  Q  (=81)
*          (Note: which screen is set visible/invisible depends
*           on the current destination)
*          Reset graphics board               ESC UPARROW U (=85)
*        Cifer T5
*        VT100 with Selanar 4010 board
*        Standard Pericom Monterey
*        Pericom Monterey with RAL mods
*      Note that a pure emulator can always be run by regarding it
*      as a T4010 (or possibly a T4014). It is only necessary to make
*      special provision where additional codes are supported and there
*      is some advantage in using them.
*
*     Originally, the main driver subroutine contained pure Tek
*     code with the addition of the calls to subroutines needed
*     to deal with device-specific facilities of the emulators.
*     These additional emulator+ subroutines had the root GK0E
*     and not GK0T in order to distinguish them from pure Tek.
*     This has changed in as much as GK0T routines now contain
*     references to and are used by some of the emulators+. The
*     GK0E routines, however, are still pure emulators+.
*
*      Functionally, the additional facilities found on emulators+ and
*      used in this driver are:
*        - automatic switch into a mode where the terminal behaves as a
*          Tek storage tube
*        - use of alpha scrolling store where provided
*        - use of zero-write to erase parts of the picture

*     The buffer strategy varies from one workstation to another.
*
*     Usually, on Open Workstation the driver ensures that the terminal is
*     switched to graphics mode and that it stays there untill Request
*     Input or Update Workstation call. These devices (Cifers, Pericoms)
*     have their End Of Buffer and Beginning Of Buffer strings in GKIOBO
*     intentionally uninitionalised. This is not so for VT100 with the
*     Selinar board, which at end of buffer and at end of the execution
*     of an entrypoint is switched from graphics to alpha mode.

*     Device attribute strategy for 4014 is to ensure that
*       - hardware linetype is returned to solid at end of entrypoint
*       - hardware character size is returned to smallest at end of
*         entrypoint and at end of buffer: this needs consideration
*         when hardware text goes over multiple buffers.
*     This implies being set to normal values on Open Workstation.
*
*---------------------------------------------------------------------



*  Intercept request for Pick Echoplay for primitive
      IF(KPKECO .EQ. KPECHO)THEN
        IF((IENT .GE. KPL) .AND. (IENT .LE. KGDP))THEN
*         Pick echoplay requested
          CALL GKEPBB (IENT, NID,IDAT, NRD,RX,RY,0.02,
     :                GK0TLN, GK0TXF)
          GOTO 8888
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
     :       700, 710, 720, 730, 740,9999,9999,9999,9999,9999,
     :      9999, 810,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999, 910, 920, 930) IENT

      GOTO (1111,1111,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1111,1111,1111,1330,1111,1111,1111,1370,1380,1111,
     :      1111,1410,1420,1111,1440,1111,1460,1111,1111,1111,
     :      1111,1510,1111,9999,9999,9999,9999,9999,9999,9999,
     :      9999,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1111,1710,1111,1111,1111,1111,1111,1111,1111,1790,
     :      1111,1810,1111,1111,1840,1850,1111,1870,1880,1111,
     :      1111,1111,1111,1111,1111,1111,1960,1970,1111) IENT-119

      GOTO 9999


* Open workstation (KOPWK)
   10 CONTINUE
* Set up workstation state list and workstation description table
      CALL GKIWSL(KWKIX,KWKTYP)
      IF( KERROR.NE.0 ) GOTO 9999

*  Set up workspace variables to control the subsequent operation
*  of the device.
*    Native Tektronix is in the 200's
      IF( KWKTYP.EQ.201 ) THEN
*       Tek 4010
        KWKDAT(ICHSIZ,KWKIX) = GLOWER
        KWKDAT(ILTS,  KWKIX) = GLOWER
        KWKDAT(ICLAS, KWKIX) = ITEK
      ELSEIF( KWKTYP.EQ.203 ) THEN
*       Tek 4014
        KWKDAT(ICHSIZ,KWKIX) = GHIGHR
        KWKDAT(ILTS,  KWKIX) = GHIGHR
        KWKDAT(ICLAS, KWKIX) = ITEK

*    Emulators+ are in the 800's
      ELSEIF( KWKTYP.EQ.800 ) THEN
*       Cifer 2634
        KWKDAT(ICHSIZ,KWKIX) = GLOWER
        KWKDAT(ILTS,  KWKIX) = GLOWER
        KWKDAT(ICLAS, KWKIX) = IEMUL
      ELSEIF( KWKTYP.EQ.801 ) THEN
*        Cifer T5
         KWKDAT(ICHSIZ,KWKIX) = GHIGHR
         KWKDAT(ILTS,  KWKIX) = GHIGHR
         KWKDAT(ICLAS, KWKIX) = IEMUL
      ELSEIF( KWKTYP.EQ.810 ) THEN
*        Selanar VT100 with graphics board
         KWKDAT(ICHSIZ,KWKIX) = GLOWER
         KWKDAT(ILTS,  KWKIX) = GLOWER
         KWKDAT(ICLAS, KWKIX) = IEMUL
      ELSEIF( KWKTYP.EQ.820 ) THEN
*        Standard Pericom Monterey
         KWKDAT(ICHSIZ,KWKIX) = GHIGHR
         KWKDAT(ILTS,  KWKIX) = GHIGHR
         KWKDAT(ICLAS, KWKIX) = IEMUL
      ELSEIF( KWKTYP.EQ.821 ) THEN
*        Pericom Monterey MG200 with RAL mods
         KWKDAT(ICHSIZ,KWKIX) = GHIGHR
         KWKDAT(ILTS,  KWKIX) = GHIGHR
         KWKDAT(ICLAS, KWKIX) = IEMUL
      ELSE
*        Have error here
         KERROR = -2010
      ENDIF

* Initialise flags used by escapes -1 and -2
      KWKDAT(ICHFLG, KWKIX) = KNIL
      KWKDAT(ILCFLG, KWKIX) = KNIL

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

* Send orders to switch terminal into Tektronix mode and where
* necessary set head and tail of buffer output to Tektronix lookalikes
          IF( KWKDAT(ICLAS,KWKIX).EQ.IEMUL ) CALL GK0EBF

* Initialise hardware settings if necessary
* Character size and linetype. Do it before clear screen
* so that it gets sent before anything else happens.
          IF( KWKDAT(ICHSIZ,KWKIX).EQ.GHIGHR )
     :          CALL GK0TSH(1)
          IF( KWKDAT(ILTS,KWKIX).EQ.GHIGHR )
     :          CALL GK0TSL(GLSOLI, SOFT, PATPD)

* Erase screen
          CALL GK0TCL
        ENDIF
      ENDIF
      KWI1 = GOUTIN
      GOTO 9999


* Close workstation (KCLWK)
   20 CONTINUE
      IF( KWI1.EQ.1 ) THEN
        KWDONE=KRFUSE
      ELSE

*      Move to the top left-hand corner of screen before finishing -
*      to avoid system prompt appearing over the picture.
        XDC(1) = 0.0
        IF (KDSRX(KWKIX).EQ.4096) THEN
          YDC(1) = 3062.0
        ELSE
          YDC(1) = 767.0
        ENDIF
        CALL GK0TLN(1,XDC,YDC)

*      Send orders to terminal to switch out of graphics mode. Do the
*      Tek lookalikes separately.
        IF( KWKDAT(ICLAS,KWKIX).EQ.IEMUL ) THEN
           CALL GK0ECL
        ELSE
           CALL GKIOBO(KIOPB,1,IUS,NLEFT)
        ENDIF
        CALL GKIOBO(KIOSN,1,KDAT,NLEFT)

*      Close connection to terminal
        CALL GKIOCL(KFWKT,KCID(KWKIX),KWCID(KWKIX))

*      Close GKS data structures for this workstation
        CALL GKCWSL(KWKIX)
        CALL GKSLDL(KSSGPT(KWKIX))
      ENDIF
      GOTO 9999


* Clear workstation (KCLRWK)
   30 CONTINUE
      IF( KWI1.EQ.2 ) THEN
        KWDONE=KRFUSE
        CALL GKSLDL(KSSGPT(KWKIX))
      ENDIF
      GOTO 9999


* Redraw all segments on workstation (KRSGWK)
   40 CONTINUE
      KWDONE = KRFUSE
      GOTO 9999


* Update workstation  (KUWK)
   50 CONTINUE
*  Emulators with scrollable alpha screens are treated separately and
*  in a standard manner: terminal is switched to the alpha mode, alpha
*  screen is unblanked and, if possible, graphics screen is unblanked.
*  Code to blank the alpha screen and switch back to graphics is put in
*  the output buffer, but not sent.
      IF( KWKDAT(ICLAS,KWKIX).EQ.IEMUL ) THEN
         CALL GK0EUW
         KWI1=KNFAUP(KWKIX)
         KWDONE=KACEPT
      ELSE
         KWDONE = KRFUSE
      ENDIF
      GOTO 9999


* Set deferral state  (KSDS)
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


* Do deferred output actions (KDDA)
   70 CONTINUE
      CALL GKIOBO(KIOSN,1,KDAT,NLEFT)
      KWI1 = KNFAUP(KWKIX)
      GOTO 9999


* Clear display surface  (KCLDS)
   80 CONTINUE
      IF (KWI1.EQ.GALWAY.OR.KDSMT(KWKIX).EQ.GNEMPT) CALL GK0TCL
      CALL GKWCLD
      GOTO 9999


* Message (KMSG)
  100 CONTINUE
*  Emulators with scrollable alpha screens are treated separately and
*  in a standard manner: terminal is switched to the alpha mode, alpha
*  screen is unblanked and, if possible, graphics screen is unblanked.
*  Code to blank the alpha screen and switch back to graphics is put in
*  the output buffer, but not sent.
*  Message is then output to the alphanumeric screen
      IF( KWKDAT(ICLAS,KWKIX).EQ.IEMUL ) THEN
         CALL GK0EUW
         IF (NID.GE.1)  WRITE(KERRFL,101) (GKAN1(IDAT(I)),I=1,NID)
  101                   FORMAT(80A1)
      ENDIF
      GOTO 9999


* Escape  (KESC)
  110 CONTINUE
      CALL GKESC
      GOTO 9999


* Polyline  (KPL)
  120 CONTINUE
      IF( KCVIS.EQ.GINVIS ) GOTO 9999

*   If drawing in background colour, switch to erase
*   mode for emulators and do nothing for a real Tek
      IF(KWPLCI(KWKIX).EQ.0)THEN
         IF(KWKDAT(ICLAS,KWKIX).EQ.IEMUL)THEN
            CALL GK0ESC(IERASE)
         ELSE
            GOTO 9999
         ENDIF
      ENDIF

*   Allocate Stack for Polyline Points
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.NE.0) GOTO 9999

*   Set hardware linestyle and decide on software repeat length
      CALL GK0TSL(KWLNTY(KWKIX), SOFT, PATPD)

*   Split polyline into manageable chunks and deliver them
      N = ICHUNK
      DO 122 I=1,NRD,ICHUNK-1
        IF (NRD-I.LT.ICHUNK-1) N = NRD-I + 1
        IF (N.EQ.1) GOTO 122
        CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
        CALL GKLCLP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),
     :      SOFT,  PATPD,
     :       QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),QWCLYT(KWKIX),
     :       GK0TLN)
  122 CONTINUE
      CALL GKSTDA(KREALS,IOFF)

*   Restore hardware linetype to solid if necessary
      IF(.NOT.SOFT .AND. KWLNTY(KWKIX).NE.1)
     :    CALL GK0TSL(GLSOLI, SOFT, PATPD)

*   Restore to drawing mode if necessary
      IF (   (KWKDAT(ICLAS,KWKIX).EQ.IEMUL)   .AND.
     :       (KWPLCI(KWKIX).EQ.0)         )    THEN
         CALL GK0ESC(IDRAW)
      ENDIF

      GOTO 8888


* Polymarker  (KPM)
  130 CONTINUE
      IF( KCVIS.EQ.GINVIS ) GOTO 9999
*   If drawing in background colour, switch to erase
*   mode for emulators and do nothing for a real Tek
      IF(KWPMCI(KWKIX).EQ.0)THEN
         IF(KWKDAT(ICLAS,KWKIX).EQ.IEMUL)THEN
            CALL GK0ESC(IERASE)
         ELSE
            GOTO 9999
         ENDIF
      ENDIF

*   Allocate Stack for Polymarker Points
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)

*   Deal with polymarker in Chunks
      IF (KERROR.EQ.0) THEN
        N = ICHUNK
        DO 132 I=1,NRD,ICHUNK
          IF (NRD-I.LT.ICHUNK) N = NRD-I + 1
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
          CALL GKMTYP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),
     :                   KWMKTY(KWKIX),QNMMKS(KWKIX),QWMKSZ(KWKIX),
     :                   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK0TLN)
  132   CONTINUE
        CALL GKSTDA(KREALS,IOFF)
*   Restore to drawing mode if necessary
        IF (   (KWKDAT(ICLAS,KWKIX).EQ.IEMUL)   .AND.
     :         (KWPMCI(KWKIX).EQ.0)         )    THEN
           CALL GK0ESC(IDRAW)
        ENDIF
      ENDIF
      GOTO 8888


* Text    (KTX)
  140 CONTINUE
      IF( KCVIS.EQ.GINVIS ) GOTO 9999
*   If drawing in background colour, switch to erase
*   mode for emulators and do nothing for a real Tek
      IF(KWTXCI(KWKIX).EQ.0)THEN
         IF(KWKDAT(ICLAS,KWKIX).EQ.IEMUL)THEN
            CALL GK0ESC(IERASE)
         ELSE
            GOTO 9999
         ENDIF
      ENDIF
      IF(KWTXPR(KWKIX) .EQ. GSTRKP) THEN
         CALL GKXDWO(NID,IDAT,GK0TLN)
      ELSE

*       Char or String precision text. Alter hardware character size if
*       device can do it and the required value is different from current.
          HARD = KWKDAT(ICHSIZ,KWKIX).EQ.GHIGHR .AND.
     :           KWKDAT(ICHSZD,KWKIX).NE.1
          IF(HARD) CALL GK0TSH(KWKDAT(ICHSZD,KWKIX))
          CALL GKXDWC(NID,IDAT,QWCHWX(KWKIX),QWCHWY(KWKIX),
     :               GK0TXF,GK0TXC)
          IF(HARD) CALL GK0TSH(1)
      ENDIF
*   Restore to drawing mode if necessary
      IF (   (KWKDAT(ICLAS,KWKIX).EQ.IEMUL)   .AND.
     :       (KWTXCI(KWKIX).EQ.0)         )    THEN
         CALL GK0ESC(IDRAW)
      ENDIF
      GOTO 8888


* Fill area   (KFA)
  150 CONTINUE
      IF( KCVIS.EQ.GINVIS ) GOTO 9999
*   If drawing in background colour, switch to erase
*   mode for emulators and do nothing for a real Tek
      IF(KWFACI(KWKIX).EQ.0)THEN
         IF(KWKDAT(ICLAS,KWKIX).EQ.IEMUL)THEN
            CALL GK0ESC(IERASE)
         ELSE
            GOTO 9999
         ENDIF
      ENDIF
*   Set fill area scale factor
      IF (KDSRX(KWKIX).EQ.4096) THEN
        IFILSC = 4
      ELSE
        IFILSC = 1
      ENDIF
      CALL GKFILS(NRD,RX,RY,IFILSC,GK0TLN,GK0TLN)
*   Restore to drawing mode if necessary
      IF (   (KWKDAT(ICLAS,KWKIX).EQ.IEMUL)   .AND.
     :       (KWFACI(KWKIX).EQ.0)         )    THEN
         CALL GK0ESC(IDRAW)
      ENDIF
      GOTO 8888


* Cell array ... do minimal simulation  (KCA)
  160 CONTINUE
      IF( KCVIS.EQ.GINVIS ) GOTO 9999
      CALL GKCASM(GK0TLN)
      GOTO 8888


* GDP  (KGDP)
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
*   If drawing in background colour, switch to erase
*   mode for emulators and do nothing for a real Tek
*          (Arc GDP uses Polyline attributes)
        IF(KWPLCI(KWKIX).EQ.0)THEN
           IF(KWKDAT(ICLAS,KWKIX).EQ.IEMUL)THEN
              CALL GK0ESC(IERASE)
           ELSE
              GOTO 9999
           ENDIF
        ENDIF
        CALL GK0TSL(KWLNTY(KWKIX), SOFT, PATPD)
        CALL GKCRCS(KWI1,NRD,RX,RY,1,SOFT,PATPD,
     :       GK0TLN, GK0TLN)
        CALL GK0TSL(GLSOLI, SOFT, PATPD)
*   Restore to drawing mode if necessary
        IF (   (KWKDAT(ICLAS,KWKIX).EQ.IEMUL)   .AND.
     :         (KWPLCI(KWKIX).EQ.0)         )    THEN
           CALL GK0ESC(IDRAW)
        ENDIF
      ELSE
* Filled chord, pie or circle
*   Set fill area scale factor
        IF (KDSRX(KWKIX).EQ.4096) THEN
          IFILSC = 4
        ELSE
          IFILSC = 1
        ENDIF
*   If drawing in background colour, switch to erase
*   mode for emulators and do nothing for a real Tek
*   (Chord, Pie Or Circle GDP's use Fill Area attr.)
        IF(KWFACI(KWKIX).EQ.0)THEN
           IF(KWKDAT(ICLAS,KWKIX).EQ.IEMUL)THEN
              CALL GK0ESC(IERASE)
           ELSE
              GOTO 9999
           ENDIF
        ENDIF
        CALL GKCRCS(KWI1,NRD,RX,RY,IFILSC,.FALSE.,1.0,
     :       GK0TLN,GK0TLN)
*   Restore to drawing mode if necessary
        IF (   (KWKDAT(ICLAS,KWKIX).EQ.IEMUL)   .AND.
     :         (KWPLCI(KWKIX).EQ.0)         )    THEN
           CALL GK0ESC(IDRAW)
        ENDIF
      ENDIF
      GOTO 8888


* Set polyline attributes  (KSPLA)
  180 CONTINUE
      CALL GKDPLB
* Need to check because individual settings won't have been checked.
      IF (KWLNTY(KWKIX).LT.0.OR.KWLNTY(KWKIX).GT.5) KWLNTY(KWKIX) = 1
      IF (KWPLCI(KWKIX).GE.KPCI(KWKIX)) KWPLCI(KWKIX) = 1
      GOTO 9999


* Set polymarker attributes  (KSPMA)
  190 CONTINUE
      CALL GKDPMB
* Need to check because individual settings won't have been checked.
      IF (KWMKTY(KWKIX).LT.0.OR.KWMKTY(KWKIX).GT.5) KWMKTY(KWKIX) = 3
      IF (KWPMCI(KWKIX).GE.KPCI(KWKIX)) KWPMCI(KWKIX) = 1
      GOTO 9999


* Set text attributes  (KSTXA)
  200 CONTINUE
      CALL GKDTXB
* Need to check because individual settings won't have been checked.
      IF (KWTXCI(KWKIX).GE.KPCI(KWKIX)) KWTXCI(KWKIX) = 1
* If not stroke precision, calculate device-dependent attributes:
* font number and character size identifier (if it is variable
* on the device)
      IF(KWTXPR(KWKIX).NE.GSTRKP) THEN
        KWTXFN(KWKIX)=1
        IF( KWKDAT(ICHSIZ,KWKIX).EQ.GHIGHR ) CALL GK0TGH
      ENDIF
      GOTO 9999


* Set fill area attributes  (KSFAA)
  210 CONTINUE
      CALL GKDFAB
* Need to check because individual settings won't have been checked.
      IF (KWFAIS(KWKIX).EQ.GPATTR) KWFAIS(KWKIX) = GHOLLO
      IF (KWFAIS(KWKIX).EQ.GHATCH) THEN
        IF (KWFASI(KWKIX).GT.-1 .OR. KWFASI(KWKIX).LT.-10)
     :    KWFASI(KWKIX) = -1
      ENDIF
      IF (KWFACI(KWKIX).GE.KPCI(KWKIX)) KWFACI(KWKIX) = 1
      GOTO 9999


* Set pick identifier  (KSPKID)
  220 CONTINUE
      GOTO 9999


* Set polyline representation   (KSPLR)
  230 CONTINUE
      INTA(1) = 5
      CALL GKSRPL(1,INTA,.TRUE.)
      GOTO 9999


* Set polymarker representation    (KSPMR)
  240 CONTINUE
      CALL GKSRPM(0,INTA,.TRUE.)
      GOTO 9999


* Set text representation          (KSTXR)
  250 CONTINUE
      IF( KWI3.EQ.GSTRKP ) THEN

*       Stroke Precision
*       Make sure that fonts are available
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


* Set fill area representation     (KSFAR)
  260 CONTINUE
      IF (KWI2.NE.GPATTR) THEN
        CALL GKSRFA(.TRUE.)
      ELSE
        KERROR = 83
      ENDIF
      GOTO 9999


* Set pattern representation       (KSPAR)
  270 CONTINUE
* Pattern not supported
      KERROR = 90
      GOTO 9999


* Set colour representation        (KSCR)
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


* Normalisation transformation     (KNT)
  310 CONTINUE
      CALL GKWKC4
      GOTO 9999


* Set workstation window           (KSWKWN)
  320 CONTINUE
      CALL GKSWKW
      CALL GKPPBU
      GOTO 9999


* Set workstation viewport         (KSWKVP)
  330 CONTINUE
      CALL GKSWKV
      CALL GKPPBU
      GOTO 9999


* Segment entrypoints *
  410 CONTINUE
      CALL GKSGWB(IENT,.FALSE.)
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


* Request locator (KRQLC)
  690 CONTINUE
      IF (KWKDAT(ILCFLG,KWKIX).NE.KNIL .AND. KWI1.EQ.1) THEN
*        Escape has been used to associate choice device 2 with
*        locator device 1.
*        If no stored data, need to request operator action
         IF (KWKDAT(ILCFLG,KWKIX) .EQ. GNO) CALL GKRQES(GK0TCS)
*        Pass locator data back
         KWI1 = KWKDAT(ILCSTA, KWKIX)
         RX(1) = QWKDAT(ILCX, KWKIX)
         RY(1) = QWKDAT(ILCY, KWKIX)
*        Clear stored locator data flag
         KWKDAT(ILCFLG, KWKIX) = GNO
      ELSE
*        Normal locator input
         CALL GKRQLC(GK0TCS, RX(1), RY(1))
      ENDIF
      GOTO 9999


* Request stroke   (KRQSK)
  700 CONTINUE
*   First, get stroke device information
      CALL GKISK(NRD,KNRR,RX,RY, INTA,REALA, NIPTS2,
     :      IOFFX,IOFFY)
      IF( KERROR.NE.0 ) GOTO 9999

*   Draw initial stroke if there are any points and echo is set
      IF( NIPTS2.GE.2 .AND. INTA(KIPE).EQ.GECHO )
     :      CALL GK0TLN(NIPTS2,QSTACK(IOFFX),QSTACK(IOFFY))

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
            CALL GK0TCS(ICODE, XDC(2),YDC(2))

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
     :              CALL GK0TLN(2,XDC,YDC)
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

*   Here, KNRR (number of points in stroke) is set. Now set status to be OK
  707 KWI1=GOK
*   Drop to 709

  709 CONTINUE
      GOTO 9999


* Request valuator   (KRQVL)
  710 CONTINUE
*   First get valuator device info and construct prompt
      CALL GKRQIP(GVALUA,KWI1,4,7,INTA,REALA)
      IF (KERROR.NE.0) GOTO 9999
      WRITE(PROMPT, '(A,G12.4,A,G12.4,A)') 'Value between ',
     :      REALA(KVLMNV), ' and ', REALA(KVLMXV), ':'

      IF (KWKDAT(ICLAS,KWKIX).EQ.IEMUL ) THEN
*         If emulator, prompt on alpha screen
          CALL GK0EGI(INTA,REALA,PROMPT,VALSTR,NOUT)
      ELSE
*         Have device without scrolling store
          CALL GK0TGI(INTA,REALA,PROMPT,VALSTR,NOUT)
      ENDIF

*   Interpret the incoming string. If not recognisable then set
*   status = none.
      IF( NOUT.LT.1 ) THEN
          KWI1=GNONE
          QWR1=QNIL
      ELSE
*     Convert to a real number
          WRITE(FORMT, '(A,I2,A)') '(F', NOUT, '.0)'
          READ(VALSTR(1:NOUT), FORMT, ERR=713) QWR1
          GOTO 718

*     No good
  713     KWI1=GNONE
          QWR1=QNIL
          GOTO 719

*     Here QWR1 contains a valid real number. Is it in range?
  718     IF( REALA(KVLMNV).LE.QWR1 .AND. QWR1.LE.REALA(KVLMXV) )THEN
              KWI1=GOK
          ELSE
              KWI1=GNONE
              QWR1=QNIL
          ENDIF

*      Drop to 719
      ENDIF
  719 GOTO 9999


* Request choice   (KRQCH)
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
         IF (KWKDAT(ICHFLG,KWKIX) .EQ. GNO) CALL GKRQES(GK0TCS)
*        Pass choice data back
         KWI1 = KWKDAT(ICHSTA, KWKIX)
         KWI2 = KWKDAT(ICHNUM, KWKIX)
*        Clear stored choice data flag
         KWKDAT(ICHFLG, KWKIX) = GNO
      ELSE
*        Normal choice input
*        Call the utility, but pass the right external
         IF (KWKDAT(ICLAS,KWKIX).EQ.IEMUL) THEN
            CALL GKRQCH(KWI1,KWI2,GK0EGI)
         ELSE
            CALL GKRQCH(KWI1,KWI2,GK0TGI)
         ENDIF
      ENDIF
      GOTO 9999


* Request pick     (KRQPK)
  730 CONTINUE
*     KWI2 input  KNIL=pick begining, KPECHO=echoplay done
*                                     KPUNEC=unecho done
*                 Suppress UNECHO by making GKRQPK think that it's
*                 already been done, if ECHO done.
      IF(KWI2 .EQ. KPECHO)KWI2 = KPUNEC
      IF(KDSRX(KWKIX) .EQ. 4096)THEN
*       Bigger pick aperture for high resolution
        CALL GKRQPK (16.0,GK0TPP,GK0TCS,GK0TXF)
      ELSE
*       Normal pick aperture
        CALL GKRQPK ( 4.0,GK0TPP,GK0TCS,GK0TXF)
      ENDIF
      GOTO 9999


* Request string   (KRQST)
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
*     Call string input utility, but pass correct external
      IF(KWKDAT(ICLAS,KWKIX) .EQ. IEMUL)THEN
        CALL GKRQST (KWI1,8,IPRMST,NID,KNIR,IDAT,GK0EGI)
      ELSE
        CALL GKRQST (KWI1,8,IPRMST,NID,KNIR,IDAT,GK0TGI)
      ENDIF
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


* Inquire polyline representation   (KQPLR)
 1330 CONTINUE
      IF (KWI2.EQ.GSET) THEN
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
        QWR1 = 1.0
      ENDIF
      GOTO 9999



* Inquire text representation     (KQTXR)
C THIS WILL CHANGE CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 1370 CONTINUE
      IF (KWI2.EQ.GSET) THEN
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      ENDIF
      GOTO 9999



* Inquire text extent    (KQTXX)
 1380 CONTINUE
*     stroke precision
      IF (KWTXPR(KWKIX) .EQ. GSTRKP) THEN
        CALL GKXQXO(NID,IDAT,RX,RY)
*     string and char precision
      ELSE
*       baseline vector from ws Set text attributes entry
        CALL GKXQXC (NID,IDAT,QWCHWX(KWKIX),QWCHWY(KWKIX),
     :                  RX,RY,GK0TXF)

      ENDIF
      GOTO 9999



* Inquire list of pattern indices  (KQEPAI)
 1410 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999


* Inquire pattern representation   (KQEPAR)
 1420 CONTINUE
      KERROR = 90
      GOTO 9999


* Inquire colour representation   (KQCR)
 1440 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
* For a real Tek, the realised background colour should always be black,
* and the foreground always white.
      IF (KWI2.EQ.GREALI) THEN
        IF (KWI1.EQ.0) THEN
          QWR1 = 0.0
        ELSE
          QWR1 = 1.0
        ENDIF
        QWR2 = QWR1
        QWR3 = QWR1
      ENDIF
      GOTO 9999


* Inquire set member of segment names on workstation     (KQSGWK)
 1460 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KSGRQ = KSGRQ + 1
      GOTO 9999


* Inquire pick device state    (KQPKS)
 1510 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999


* Inquire workstation classification    (KQWKCL)
 1710 CONTINUE
      KWI1 = GVECTR
      GOTO 9999



* Inquire text facilities    (KQTXF)
* Allow for string and char precision font (number 1) explicitly
 1790 CONTINUE
      IF( KWI1.GT.KFNTMX+2 ) THEN
        KERROR=2002
      ELSEIF( KWI1.GT.KFNTMX ) THEN

*       String or Char precision font
          IF( KWI1.EQ.KFNTMX+1 ) KWI3 = GSTRP
          IF( KWI1.EQ.KFNTMX+2 ) KWI3 = GCHARP
          KWI2 = 1
      ELSEIF( KWI1 .NE. 0) THEN

*       Stroke precision font
*       Make sure that fonts are available
          IF( KDBFLS.EQ.KFLNA ) THEN
            KERROR=-1009
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


* Inquire fill area facilities   (KQFAF)
 1810 CONTINUE
* Only 3 interior styles supported on this workstation
*     Call standard inquiry with list element shifted,
*     if necessary,  to exclude patterned fill.
      IF(KWI1 .GE. 3) KWI1 = KWI1+1
      CALL GKQWK (IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
*     Reset number of avaible fill-styles to exclude patterned fill.
      KWI1 = 3
      GOTO 9999



* Inquire predefined pattern representation   (KQPPAR)
 1840 CONTINUE
      KERROR = 90
      GOTO 9999


* Inquire colour facilities  (KQCF)
 1850 CONTINUE
      KWI2 = GMONOC
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KWI1=2
      GOTO 9999


* Inquire list element of available GDPs   (KQEGDP)
 1870 CONTINUE
      IF (KWI1.GE.1 .AND. KWI1.LE.4) THEN
        KWI2 = -KWI1
      ELSE
        KERROR = 2002
        KWI2 = KNIL
      ENDIF
      KWI1 = 4
      GOTO 9999


* Inquire GDP    (KQGDP)
 1880 CONTINUE
      KNIR = 0
      IDAT(1) = KNIL
      IDAT(2) = KNIL
      IDAT(3) = KNIL
      IDAT(4) = KNIL
      IF (KWI1.LE.-1 .AND. KWI1.GE.-4) THEN
        KNIR = 1
        IDAT(1) = GFAATT
        IF (KWI1.EQ.-1) IDAT(1) = GPLATT
      ELSE
        KERROR = 41
      ENDIF
      GOTO 9999


* Inquire default choice device data    (KQDCH)
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


* Inquire default pick device data  (KQDPK)
 1970 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999



*   Here after all output primitives to sort out buffering
 8888 CONTINUE
      KDSMT(KWKIX) = GNEMPT
      IF (KWIO(KWKIX).EQ.GYES) CALL GKIOBO(KIOSN,1,KDAT,NLEFT)

 9999 CONTINUE

      END
