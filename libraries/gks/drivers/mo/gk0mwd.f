C# IL>=a, OL>=0
      SUBROUTINE GK0MWD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    WORKSTATION DRIVER
*  Author:             DSG
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Metafile output workstation driver interface
*
*  MAINTENANCE LOG
*  ---------------
*     01/03/83  DSG   Original version stabilized
*     13/06/83  DSG   Addition of WCA interface variables to GK0MIT
*                     arising from GIN 67.
*     16/06/83  DSG   Primitive attributes sorted out
*     25/06/83  DSG   Check on requested workstation type
*     29/06/83  DSG   Change in error handling
*     17/01/84  DSG   General inquiry routine GKQWK and segment utility
*                     GKSGWK called. Local char string replaced by CSTR
*                     in GK0MIT.
*     20/01/84  DSG   More segment utilities called
*     24/01/84  DSG   User item corrected in GK0MIT.
*     15/02/84  DSG   Bug Fix I76
*     16/03/84  DSG   Bug fix I158 in GK0MIT - CELL ARRAY chunking error
*     22/03/84  DSG   Bug fix S34 in GK0MIT - Skip the output of GDP and
*                     ESCAPE data records for this version. The output
*                     of the three special points for GDP now added.
*     24/03/84  DSG   Bug fix S35 in GK0MIT - TEXT, CELL ARRAY and PATTERN
*                     REFERENCE POINT call GKTWD instead of GKTWDV.
*     29/03/84  DSG   Bug fix S39 in GK0MIT
*     18/04/84  DSG   Fix of bug S47 - Partial arrays of colour indices
*     19/04/84  DSG   CELL ARRAY: error introduced by bug fix S39
*     30/04/84  DSG   S50 in GK0MIT: Set Segment Transformation bug fixed
*     02/05/84  PGLS  Correct segment entries 470 on, found by DSG
*     03/05/84  DSG   S55: in GK0MIT, Clearing Control Flag becomes KWI2
*     05/05/84  PGLS  Bring up to date with SERC (many changes)
*     09/05/84  DSG   S57: Visibility and Highlight WCA variables fixed
*                     in GK0MIT
*     02/05/85  JRG   Temporary fix, replace JUNK by space character(S143)
*     13/08/85  GMC   Changed TEXT entry in GK0MIT to use GKATON (S152)
*     16/01/86  DSG   S176: Change in integer format used in Cell Array
*                     in GK0MIT (CMS compiler restriction)
*     03/02/86  DSG   Local copy of ASFs kept (initialised to bundled
*                     at open workstation)
*     07/02/86  DSG   GKATON used at Message entry in GK0MIT, instead of CHAR
*     04/03/86  DSG   GK0MBU buffer pointer put into common.
*     05/03/86  DSG   ASFs taken from Derived Data in GK0MIT
*     18/03/86  DSG   S141, I245: calls to GKSTAL modified in GK0MIT
*                     and KERROR checked
*     20/03/86  DSG   Correct response to Inquire Text Extent; comment out
*                     calls to GKBUG; remove lower case text (S138,S168)
*     18/04/86  DSG   WSL brought into use for inquiries to work O.K.
*     02/05/86  RMK   Changed to use new interface to GKIOOP/GKIOCL.
*     20/05/86  DSG   After Activate Workstation, the ASF item is output
*                     only if Set ASF is invoked. A primitive attribute
*                     item is output only if its ASF is individual.
*     03/06/86  DSG   ASF item (all bundled) output at Open Workstation
*     19/06/86  RMK   Incorporated maintenance log entries from other
*                     GK0Mxx routines. Removed unused local variables
*                     and labels. Removed calls to GKBUG.
*     25/06/86  RMK   In Open Wkstn entry, added code to correct wkstn
*                     viewport values set by GKIWSO.
*                     Removed calls to utilities GKSWKV & GKSWKW, and just
*                     saved current & requested values in WSL.
*     19/01/87  CJC   IS conversion. Change implementation name and
*                     version no (from 01 to  1).
*                     GDP item data record altered (order of lengths).
*                     Commented out code for unpacking GDP and ESCAPE
*                     data records removed as inaccurate following
*                     change to GUREC.
*                     Cell array and set pattern rep. changed following
*                     change in W.C.A.
*                     User item altered to write exact length.
*     19/01/87  DCS   IS conversion. Change order of pattern vectors in
*                     metafile item to width, height (as they should
*                     have been!).
*     09/03/87  DCS   Change to ensure polyline colour index item is
*                     only written when ASF is individual.
*     31/03/87  RMK   Changed GK0MFH to pick up implementation name and
*                     version number from GKMC.PAR.
*     01/05/87  DSG   De-allocate heap space at Close Workstation (S260).
*     18/05/87  DCS   Corrected error number for INQ WK CLASSN (S265).
*     28/05/87  DCS   Error numbers changed for IS.
*     01/06/87  DCS   Begin and End Segment entry points removed - not
*                     relevant for this driver.
*     11/06/87  DSG   Set Representation code altered. Item written out
*                     and no errors returned, unless an addressing
*                     problem could occur.
*     25/06/87  RMK   Changed open wkstn entry to give up if GKIWSL
*                     returns a non-zero value of KERROR (S260).
*     26/06/87  RMK   Integrated DSG's 11/06/87 fix into IS version
*                     of the driver.
*     29/06/87  RMK   Added INCLUDE for GKHP.PAR.
*     14/07/87  RMK   Added declaration of REALA.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*     14/12/87  DSG   Default deferral mode set to ASTI (Bug 283).
*     14/03/88  DSG   Workstation viewport limits raised (Bug 290).
*     15/03/88  DSG   Use enumerated types rather than integers for
*                     absent/present in delete segment entry (S299).
*     28/07/89  RMK   Added deletion of segments to clear workstation
*                     entry (S362). Removed unused local variables.
*     08/05/90  PLP   Introduced parameters IDDFRM and IDIRGM in order to
*                     have the driver's default deferral mode and the default
*                     implicit regeneration mode information stored.
*                     Subsequently altered the Open Workstation, Set
*                     Deferral State and Inquire Default Deferral State
*                     Values entries (S283 and S284).
*     10/05/90  RTP   Change formats SFMT(19) to 56F9.6 in GK0MIT
*                     Needed for Cray because of internal buffer limit
*     20/07/90  DSG   Set all ASF's to the WSL values at KSPLA, KSPMA
*                     KSTXA and KSFAA entry-points.  This fixes bug
*                     S366 and should satisfy NCC test mf0a01.
*                     NOTE: WSL common block was put in the driver
*                     to achieve this.
*     26/09/90  KEVP  Made it return error 39 for inQuire Default
*                     Deferral State (C40).
*     07/11/90  KEVP  Removed temporary code, no longer needed,
*                     because of the fix to bug C18.
*     25/01/91  DSG   Modification of the change of 20/07/90: set
*                     all ASF's to the local values of KSPLA, etc.
*     31/01/91  PLP   Removed unused local IENT from GK0MAF.
*     27/02/91  KEVP  Delete segment (item=84), only if segment exists
*                     in the segment list (S473).
*     28/01/00  RTP   Change GK0MFH for Y2K Compliance

*
*  ARGUMENTS
*  ---------
*     INP   IENT   Entrypoint code
*     INP   NID    Size of array IDAT
*     INP   IDAT   Integer data passed to workstation
*     INP   NRD    Size of arrays RX and RY
*     INP   RX     Real X-coordinate data passed to workstation
*     INP   RY     Real Y-coordinate data passed to workstation
*
      INTEGER IENT,NID,IDAT(NID),NRD,NCD
      REAL RX(NRD), RY(NRD)
*
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYWCA/    Workstation index and type
*     Read   /ASPCT/     KLNTYA,KPLCIA,KMKTYA,KPMCIA,KTXFNA,
*                        KTXCIA,KFAISA,KFACIA..(PAR)
*     Modify /GKYWCA/    Set error and workstation category
*     Modify /GKYWSL/    Set up KCID in the state list
*     Modify /GKYWKD/    Derive workstation 'total' transform
*                        In KWKDAT(n,KWKIX) keep local copy of AFSs in elements
*                        1 to 13 and store buffer pointer in 14.
*     Modify /GKYERR/    KERROR
*
      INCLUDE '../../include/gks.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwdt.par'
      INCLUDE '../../include/gwksgl.par'
      INCLUDE '../../include/gaspct.par'
      INCLUDE '../../include/gkfls.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkfls.cmn'
      INCLUDE '../../include/gkxfd.cmn'
      INCLUDE '../../include/gkplb.cmn'
      INCLUDE '../../include/gkpmb.cmn'
      INCLUDE '../../include/gktxb.cmn'
      INCLUDE '../../include/gkfab.cmn'
      INCLUDE '../../include/gksl.cmn'

*
*  LOCALS
*  ------
*
      INTEGER ITEM
* Do Loop Variables
      INTEGER I, J, JENT
*
      INTEGER ISGNO
      REAL SGPRI
*
*     IPL   offset to aspect list for polyline
*     IPM   offset to aspect list for polymarker
*     ITX   offset to aspect list for text
*     IFA   offset to aspect list for fill area
*
      INTEGER    IPL,  IPM,  ITX, IFA
      PARAMETER (IPL=0,IPM=3,ITX=6,IFA=10)
*
*     default deferral mode and default implicit regeneration mode
*
      INTEGER IDDFRM, IDIRGM
      PARAMETER (IDDFRM=GASTI, IDIRGM=GSUPPD)
*
      INTEGER INTA(19)
      REAL REALA(1)
*
*  ERRORS
*  ------

*    31   Workstation is of category MO
*    34   Workstation is not of category MI
*    37   Workstation is not of category OUTIN
*    38   Workstation is neither of category INPUT nor of
*         category OUTIN
*    39   Workstation is neither of category OUTPUT nor of
*         category OUTIN
*    93   Colour index invalid
* -1009   Unable to access the database file
*
*  COMMENTS
*  --------

* This part of the MO driver maps the GKS functionality, but
* has no knowledge of the metafile encoding except for the ITEM
* TYPE values.

*
*---------------------------------------------------------------------



* GOTO conditional on the entrypoint

      GOTO(  10,  20,  30,  40,  50,  60,  70,  80,  90, 100,
     :      110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
     :      210, 220, 230, 240, 250, 260, 270, 280,9999,9999,
     :      310, 320, 330,9999,9999,9999,9999,9999,9999,9999,
     :      410, 420, 430, 440,9999,9999, 470, 480, 490, 500,
     :      510,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :     9938,9938,9938,9938,9937,9938,9938,9938,9938,9938,
     :     9938,9938,9937,9938,9938,9938,9938,9938,9937,9938,
     :     9938,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      910,9934,9934) IENT

      GOTO(9939,9939,9999,9999,9999,9999,9999,9999,9999,9999,
     :     1300,1300,1300,1300,1300,1300,1300,1300,9939,1300,
     :     1300,1300,1300,1300,1300,1300,1300,9938,9938,9938,
     :     9938,9937,9938,9999,9999,9999,9999,9999,9999,9999,
     :     9999,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :     1700,9939,9931,9939,9939,9939,9939,9939,9939,9939,
     :     9939,9939,9939,9939,9939,9939,9939,9939,9939,9939,
     :     9939,9939,9938,9938,9938,9938,9938,9938,9938) IENT-119

      GOTO 9999
*
* OPEN WORKSTATION
* ----------------
   10 CONTINUE

* Set up WSL and WDT
      CALL GKIWSL(KWKIX, KWKTYP)
      IF (KERROR.NE.0) GOTO 9999
      KCID(KWKIX) = KWI1
*     Deferral mode - output at some time in the future (see above)
      KDFM(KWKIX) = IDDFRM
      KWIO(KWKIX) = GNO
*     Implicit regeneration - suppressed (see above)
      KIMRGM(KWKIX) = IDIRGM

* Ask operating system to make a connection.

      CALL GKIOOP(KFWKFW,KCID(KWKIX),KWCID(KWKIX))


      IF(KERROR.EQ.0) THEN

* Initialise the output buffering
        CALL GK0MBU(1,' ')

* Write the GKSM file header
        CALL GK0MFH

C??????
* Output the control item 'clear workstation (conditionally)'
        ITEM=1
        KWI2=0
        CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
C??????

*
        KSSGPT(KWKIX)=KNIL

* Set derived ASF's to bundled, and output ASF item
      DO 15 J=1,13
        KWKDAT(J,KWKIX) = GBUNDL
   15 CONTINUE
      ITEM = 43
      CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      ENDIF

* Workstation category (WDT) embedded
      KWI1=GMO

      GOTO 9999

*
* CLOSE WORKSTATION
* -----------------
   20 CONTINUE
      IF(KWI1.EQ.1) THEN
        KWDONE=KRFUSE
      ELSE
        ITEM=0
        CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
        CALL GK0MBU(3,' ')

* Ask the operating system to break the connection.
        CALL GKIOCL(KFWKFW,KCID(KWKIX),KWCID(KWKIX))
* Close GKS data structures for this workstation
        CALL GKCWSL(KWKIX)
      ENDIF
      GOTO 9999
*
* CLEAR WORKSTATION
* -----------------
   30 CONTINUE
      IF(KWI1.EQ.1) GOTO 9999
* Indicate that clear display surface entry NOT expected
      KWDONE = KACEPT
* Delete all segments on workstation
      CALL GKSLDL(KSSGPT(KWKIX))
      ITEM = 1
      GOTO 9994
*
* REDRAW ALL SEGMENTS
* -------------------
   40 CONTINUE
      KWDONE=KACEPT
      ITEM=2
      GOTO 9994
*
* UPDATE WORKSTATION
* ------------------
   50 CONTINUE
      KWDONE=KACEPT
      ITEM=3
      GOTO 9994
*
* SET DEFERRAL STATE
* ------------------
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
      KWDONE=KACEPT
      ITEM=4
      GOTO 9994
*
* DO DEFERRED OUTPUT ACTIONS
* --------------------------
   70 CONTINUE
      GOTO 9999
*
* CLEAR DISPLAY SURFACE
* ---------------------
   80 CONTINUE
      GOTO 9999
*
* REDRAW ONE SEGMENT
* ------------------
   90 CONTINUE
      GOTO 9999
*
* MESSAGE
* -------
  100 CONTINUE
      ITEM=5
      CALL GK0MIT(ITEM,NID,IDAT,1,QDAT,QDAT,NCD,STR)
      GOTO 9999
*
* ESCAPE
* ------
  110 CONTINUE
      ITEM=6
      CALL GK0MIT(ITEM,NID,IDAT,1,QDAT,QDAT,NCD,STR)
      GOTO 9999
*
* POLYLINE
* --------
  120 CONTINUE
      ITEM=11
      CALL GK0MIT(ITEM,1,KDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999
*
* POLYMARKER
* ----------
  130 CONTINUE
      ITEM=12
      CALL GK0MIT(ITEM,1,KDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999
*
* TEXT
* ----
  140 CONTINUE
      ITEM=13
      CALL GK0MIT(ITEM,NID,IDAT,1,QDAT,QDAT,NCD,STR)
      GOTO 9999
*
* FILL AREA
* ---------
  150 CONTINUE
      ITEM=14
      CALL GK0MIT(ITEM,1,KDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999
*
* CELL ARRAY
* ----------
  160 CONTINUE
      ITEM=15
      CALL GK0MIT(ITEM,NID,IDAT,1,QDAT,QDAT,NCD,STR)
      GOTO 9999
*
* GDP
* ---
  170 CONTINUE
      ITEM=16
      CALL GK0MIT(ITEM,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999
*
* SET POLYLINE ATTRIBUTES
* -----------------------
  180 CONTINUE
* Aspect source flags
* Data expected: KIPLAF(1) -> (3)     Polyline ASF's
*
      DO 182 J = KLNTYA,KPLCIA
        IF(KWKDAT(J+IPL,KWKIX).NE.KIPLAF(J))
     :   CALL GK0MAF(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
  182 CONTINUE
* Polyline index
      ITEM=21
      CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
* Linetype
      IF(KIPLAF(KLNTYA).EQ.GINDIV) THEN
      ITEM=22
      CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Linewidth scale factor
      IF(KIPLAF(KLNWDA).EQ.GINDIV) THEN
      ITEM=23
      CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Polyline colour index
      IF(KIPLAF(KPLCIA).NE.GINDIV) GOTO 9999
      ITEM=24
      GOTO 9994
*
* SET POLYMARKER ATTRIBUTES
* -------------------------
  190 CONTINUE
* Aspect source flags
* Data expected: KIPMAF(1) -> (3)     Polymarker ASF's
*
      DO 192 J = KMKTYA,KPMCIA
        IF(KWKDAT(J+IPM,KWKIX).NE.KIPMAF(J))
     :   CALL GK0MAF(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
  192 CONTINUE
* Polymarker index
      ITEM=25
      CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
* Marker type
      IF(KIPMAF(KMKTYA).EQ.GINDIV) THEN
      ITEM=26
      CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Marker size scale factor
      IF(KIPMAF(KMKSZA).EQ.GINDIV) THEN
      ITEM=27
      CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Polymarker colour index
      IF(KIPMAF(KPMCIA).NE.GINDIV) GOTO 9999
      ITEM=28
      GOTO 9994
*
* SET TEXT ATTRIBUTES
* -------------------
  200 CONTINUE
* Aspect source flags
* Data expected: KITXAF(1) -> (4)     Text ASF's
*
      DO 202 J = KTXFNA,KTXCIA
        IF(KWKDAT(J+ITX,KWKIX).NE.KITXAF(J))
     :   CALL GK0MAF(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
  202 CONTINUE
* Text index
      ITEM=29
      CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
* Text font and precision
      IF(KITXAF(KTXFNA).EQ.GINDIV) THEN
      ITEM=30
      CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Character expansion factor
      IF(KITXAF(KCHXPA).EQ.GINDIV) THEN
      ITEM=31
      CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Character spacing
      IF(KITXAF(KCHSPA).EQ.GINDIV) THEN
      ITEM=32
      CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Text colour index
      IF(KITXAF(KTXCIA).EQ.GINDIV) THEN
      ITEM=33
      CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Charactor vectors
      ITEM=34
      CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
* Text path
      ITEM=35
      CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
* Text alignment
      ITEM=36
      GOTO 9994
*
* SET FILL AREA ATTRIBUTES
* ------------------------
  210 CONTINUE
* Aspect source flags
* Data expected: KIFAAF(1) -> (3)     Fill Area ASF's
*
      DO 212 J = KFAISA,KFACIA
        IF(KWKDAT(J+IFA,KWKIX).NE.KIFAAF(J))
     :   CALL GK0MAF(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
  212 CONTINUE
* Fill area index
      ITEM=37
      CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
* Fill area interior style
      IF(KIFAAF(KFAISA).EQ.GINDIV) THEN
      ITEM=38
      CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Fill area style index
      IF(KIFAAF(KFASIA).EQ.GINDIV) THEN
      ITEM=39
      CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Fill area colour index
      IF(KIFAAF(KFACIA).EQ.GINDIV) THEN
      ITEM=40
      CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Pattern size
      ITEM=41
      CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
* Pattern reference point
      ITEM=42
      GOTO 9994
*
* SET PICK IDENTIFIER
* -------------------
  220 CONTINUE
      ITEM=44
      GOTO 9994
*
* SET POLYLINE REPRESENTATION
* ---------------------------
  230 CONTINUE
* Find out if index exists or there is room for another bundle
      DO 231 JENT=1,KMXPLB
        IF(KWI1.EQ.KPLI(JENT,KWKIX) .OR. KPLI(JENT,KWKIX).EQ.KNIL)
     :  GOTO 232
  231 CONTINUE
* No room
      KERROR = -1004
      GOTO 9999
  232 CONTINUE
* Now set representation
      KPLI(JENT,KWKIX) = KWI1
      KLNTY(JENT,KWKIX) = KWI2
      QLNWD(JENT,KWKIX) = QWR1
      KPLCI(JENT,KWKIX) = KWI3

      ITEM=51
      GOTO 9994
*
* SET POLYMARKER REPRESENTATION
* -----------------------------
  240 CONTINUE
* Find out if index exists or there is room for another bundle
      DO 241 JENT=1,KMXPMB
        IF(KWI1.EQ.KPMI(JENT,KWKIX) .OR. KPMI(JENT,KWKIX).EQ.KNIL)
     :    GOTO 242
  241 CONTINUE
* No room
      KERROR = -1004
      GOTO 9999
* Now set representation
  242 CONTINUE
      KPMI(JENT,KWKIX) = KWI1
      KMKTY(JENT,KWKIX) = KWI2
      QMKSZ(JENT,KWKIX) = QWR1
      KPMCI(JENT,KWKIX) = KWI3

      ITEM=52
      GOTO 9994
*
* SET TEXT REPRESENTATION
* -----------------------
  250 CONTINUE
* Find out if index exists or there is room for another bundle
      DO 251 JENT=1,KMXTXB
        IF(KWI1.EQ.KTXI(JENT,KWKIX) .OR. KTXI(JENT,KWKIX).EQ.KNIL)
     :    GOTO 252
  251 CONTINUE
* No room
      KERROR = -1004
      GOTO 9999
* Now set representation
  252 CONTINUE
      KTXI(JENT,KWKIX) = KWI1
      KTXFN(JENT,KWKIX) = KWI2
      KTXPR(JENT,KWKIX) = KWI3
      QCHXP(JENT,KWKIX) = QWR1
      QCHSP(JENT,KWKIX) = QWR2
      KTXCI(JENT,KWKIX) = KWI4

      ITEM=53
      GOTO 9994
*
* SET FILL AREA REPRESENTATION
* ----------------------------
  260 CONTINUE
* Find out if index exists or there is room for another bundle
      DO 261 JENT=1,KMXFAB
        IF(KWI1.EQ.KFAI(JENT,KWKIX) .OR. KFAI(JENT,KWKIX).EQ.KNIL)
     :    GOTO 262
  261 CONTINUE
* No room
      KERROR = -1004
      GOTO 9999
* Now set representation
  262 CONTINUE
      KFAI(JENT,KWKIX) = KWI1
      KIS(JENT,KWKIX) = KWI2
      KSI(JENT,KWKIX) = KWI3
      KFACI(JENT,KWKIX) = KWI4

      ITEM=54
      GOTO 9994
*
* SET PATTERN REPRESENTATION
* --------------------------
  270 CONTINUE
* Check if colour indices valid
      DO 272 J=KWI5, KWI5+KWI7-1
        DO 271 I=KWI4, KWI4+KWI6-1
          IF (IDAT((J-1)*KWI2+I).LT.0 .OR.
     :        IDAT((J-1)*KWI2+I).GT.KPCI(KWKIX)-1) THEN
            KERROR = 93
            GOTO 9999
          ENDIF
  271   CONTINUE
  272 CONTINUE

* Find out if index exists
      CALL GKDRGE(KPABPT(KWKIX),KWI1,3,0,INTA,REALA)
      IF (KERROR.EQ.0) THEN
* If it does, deallocate heap for pattern
        CALL GKHPDA(INTA(3),KINTGS)
      ELSE
* If it doesn't, directory will automatically extend
        KERROR = 0
      ENDIF

      INTA(1) = KWI6
      INTA(2) = KWI7
* Grab heap space for new pattern
      CALL GKHPAL(KWI6*KWI7,KINTGS,INTA(3))
      IF (KERROR.NE.0) GOTO 9999

* Fill directory entry for this pattern
      CALL GKDRPU(KPABPT(KWKIX),KWI1,3,0,INTA,REALA)
      IF (KERROR.NE.0) GOTO 9999

* Copy pattern into heap
      DO 274 I=KWI5, KWI5+KWI7-1
        CALL GKHPPI(INTA(3),(I-KWI5)*KWI6,KWI6,IDAT((I-1)*KWI2+KWI4))
  274 CONTINUE

      ITEM=55
      CALL GK0MIT(ITEM,NID,IDAT,1,QDAT,QDAT,NCD,STR)
      GOTO 9999
*
* SET COLOUR REPRESENTATION
* -------------------------
  280 CONTINUE
* See if colour index valid
      IF (KWI1.GE.0 .AND. KWI1.LT.KPCI(KWKIX)) THEN
        QHP(KHPXR(KCTBPT(1,KWKIX))+KWI1) = QWR1
        QHP(KHPXR(KCTBPT(2,KWKIX))+KWI1) = QWR2
        QHP(KHPXR(KCTBPT(3,KWKIX))+KWI1) = QWR3
      ELSE
        KERROR = 93
        GOTO 9999
      ENDIF
      ITEM=56
      GOTO 9994
*
* NORM TRANS
* ----------
*Data expected: QWR1,...,6     Transformation C2

  310 CONTINUE

* Derive 'workstation total transformation'. (For workstations
* of category MO this is the same as the normalisation
* transformation).
      QWTOTT(1,KWKIX)=QWR1
      QWTOTT(2,KWKIX)=QWR2
      QWTOTT(3,KWKIX)=QWR3
      QWTOTT(4,KWKIX)=QWR4
      QWTOTT(5,KWKIX)=QWR5
      QWTOTT(6,KWKIX)=QWR6

*Send clipping rectangle
      ITEM=61
      GOTO 9994
*
* SET WORKSTATION WINDOW
* ----------------------
  320 CONTINUE
* Store the requested and current values in the WSL
      QRWWXL(KWKIX) = QWR1
      QRWWXR(KWKIX) = QWR2
      QRWWYB(KWKIX) = QWR3
      QRWWYT(KWKIX) = QWR4
      QCWWXL(KWKIX) = QWR1
      QCWWXR(KWKIX) = QWR2
      QCWWYB(KWKIX) = QWR3
      QCWWYT(KWKIX) = QWR4
      ITEM=71
      GOTO 9994
*
* SET WORKSATION VIEWPORT
* -----------------------
  330 CONTINUE
* Store the requested and current values in the WSL
      QRWVXL(KWKIX) = QWR1
      QRWVXR(KWKIX) = QWR2
      QRWVYB(KWKIX) = QWR3
      QRWVYT(KWKIX) = QWR4
      QCWVXL(KWKIX) = QWR1
      QCWVXR(KWKIX) = QWR2
      QCWVYB(KWKIX) = QWR3
      QCWVYT(KWKIX) = QWR4
      ITEM=72
      GOTO 9994
*
* CREATE SEGMENT
* --------------
  410 CONTINUE
      CALL GKSGWK(IENT,.FALSE.)
      KSGRQ=KSGRQ-1
      ITEM=81
      GOTO 9994
*
* CLOSE SEGMENT
* -------------
  420 CONTINUE
      ITEM=82
      GOTO 9994
*
* RENAME SEGMENT
* --------------
  430 CONTINUE
      CALL GKSLGE(KSSGPT(KWKIX),KWI1,ISGNO,SGPRI,QDAT)
      IF(KWI1.EQ.ISGNO) THEN
        CALL GKSLNM(KSSGPT(KWKIX),KWI1,KWI2)
      ENDIF

      ITEM=83
      GOTO 9994
*
* DELETE SEGMENT
* --------------
  440 CONTINUE
      CALL GKSLGE(KSSGPT(KWKIX),KWI1,ISGNO,SGPRI,QDAT)
      IF(KWI1.EQ.ISGNO) THEN
* Segment identified OK
        KWI2=GPRSNT
        CALL GKSLDS(KSSGPT(KWKIX),KCURR)
        ITEM=84
        GOTO 9994
      ELSE
        KWI2=GABSNT
      ENDIF
      GOTO 9999


* SET SEGMENT TRANSFORMATION
* --------------------------
  470 CONTINUE
      CALL GKSLGE(KSSGPT(KWKIX),KWI1,ISGNO,SGPRI,QDAT)
      IF((KWI1.EQ.ISGNO).AND.(KWI5.EQ.2)) THEN
        ITEM = 91
        GOTO 9994
      ELSE
        GOTO 9999
      ENDIF
*
* SET VISIBILITY
* --------------
  480 CONTINUE
      CALL GKSLGE(KSSGPT(KWKIX),KWI1,ISGNO,SGPRI,QDAT)
      IF(KWI1.NE.ISGNO) GOTO 9999
      ITEM = 92
      GOTO 9994
*
* SET HIGHLIGHTING
* ----------------
  490 CONTINUE
      CALL GKSLGE(KSSGPT(KWKIX),KWI1,ISGNO,SGPRI,QDAT)
      IF(KWI1.NE.ISGNO) GOTO 9999
      ITEM = 93
      GOTO 9994
*
* SET SEGMENT PRIORITY
* --------------------
  500 CONTINUE
      CALL GKSLGE(KSSGPT(KWKIX),KWI1,ISGNO,SGPRI,QDAT)
      IF(KWI1.NE.ISGNO) GOTO 9999
      ITEM = 94
      GOTO 9994
*
* SET DETECTABILITY
* -----------------
  510 CONTINUE
      CALL GKSLGE(KSSGPT(KWKIX),KWI1,ISGNO,SGPRI,QDAT)
      IF(KWI1.NE.ISGNO) GOTO 9999
      ITEM = 95
      GOTO 9994
*
* WRITE ITEM TO GKSM
* ------------------
* The item type supplied is in KWI1, and it is be tested in
* GK0MIT to see if it has a value that is valid for a user
* item in this metafile. In order that GK0MIT knows that it
* has a user item, ITEM will be set here temporarily.
  910 CONTINUE
      ITEM=101
      CALL GK0MIT(ITEM,NID,IDAT,1,QDAT,QDAT,NCD,STR)
      GOTO 9999
*
* INQUIRE EVERYTHING
* ------------------
 1300 CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999
*
* INQUIRE WORKSTATION CATEGORY
* ----------------------------
 1700 CONTINUE
      KWI1=GMO
      GOTO 9999

* Workstation is of category MO
 9931 KERROR=31
      GOTO 9999

* Workstation is not of category MI
 9934 KERROR=34
      GOTO 9999

* Workstation is not of category OUTIN
 9937 KERROR=37
      GOTO 9999

* Workstation is neither of category INPUT nor of category
* OUTIN
 9938 KERROR=38
      GOTO 9999

* Workstation is neither of category OUTPUT nor of category
* OUTIN
 9939 KERROR=39
      GOTO 9999

 9994 CALL GK0MIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)

 9999 RETURN
      END
