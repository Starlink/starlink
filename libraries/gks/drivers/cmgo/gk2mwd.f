C# IL>=a, OL>=0
      SUBROUTINE GK2MWD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    WORKSTATION DRIVER
*  Author:             CIA
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     CGM output workstation driver interface
*
*  MAINTENANCE LOG
*  ---------------
*     **/11/87  CIAInitial Version Stabilised
*     11/01/88  CIA   Changed inquiry handling & added REALA
*     15/01/88  CIA   Added call to GKDTXB for text inquiry handling
*     19/01/88  CIA   Took out jump to 9931 as unnecessary
*     21/01/88  CIA   Changed name of CGM include file
*     25/01/88  CIA   Fixed Bug in GDP
*     29/01/88  CIA   Fixed Bug in GK2MRH for default exponent
*     01/02/88  CIA   Fixed Bug in Cell Array dimensions
*     15/02/88  CIA   Took out duplicate Picture header string
*                     Inserted background colour opcode
*                     Added VDC Integer & Real Precision Elements to
*                     Metafile Descriptor
*     11/03/88  CIA   Corrected Pattern Size vectors to height & width
*                     rather than width and height
*                     Corrected Background Colour opcode
*     **/04/88  CIA   Optimised Code, cutting out redundant settings,
*        to           setting attributes immediately before appropriate
*     **/06/88        primitives.
*     21/06/88  CIA   Allowed CGM to calculate the effect of segment
*                     transformations rather than ignoring them.
*     04/07/88  CIA   Changed GK2MTR cutting out the shrinking effect
*                     on the total transformation.
*     28/07/88  CIA   Improved the handling of segment functions
*     02/11/88  CIA   Bugfix - Text strings over 80 characters long are
*                     now handled correctly.
*     23/01/89  CIA   Moved VDC integer & real precision definitions
*                     into metafile defaults replacement.
*     03/02/89  CIA   Corrected pseudo ASF from 510 to 511
*     16/02/89  DBG   Added error report 31, which was previously missing.
*     16/02/89  DBG   Corrected the conditional goto block to goto the
*                     required error report, when user making inquires that
*                     are not permitted, (see ISO standard, Annex A, A.5).
*     08/03/89  CIA   Changed version No. to 2 . (Not too difficult!!)
*     12/04/89  RTP   Delete last change should still be version 1
*     31/07/89  RMK   Added deletion of segments to clear workstation
*                     entry and inserted entry for inquire set member of
*                     segment names on workstation (S362 fix from DBG).
*     18/04/89  RTP   Stop duplicate ASF being generated
*                     Send colour table at new frame (not bitstream)
*     19/04/89  RTP   Insert Font names instead of numbers
*     31/07/89  RTP   Correct Parameter setting in GK2MCL
*                     Add CHECKIBM to all subroutines
*                     Defer first BEGPIC till first drawing primitive
*     22/09/89  RTP   Increase real precision to 16 bits
*     25/09/89  RTP   Remove GK2MHD and output new headers
*                     Add date and time to MF Descriptor
*                     Add Character substitution for CR,LF,NULL and tilde
*     04/12/89  RTP   Add check for segment invis to all primitives
*                     Remove Unused Local variables
*                     Add entry point for inquire segments & w/s type MO
*     05/04/90  RTP   Store Set Representations in Bundle table so that
*                     inquires work (see GKS BUG - S372)
*     01/05/90  RTP   Add COMMON blocks for local arrays BIT & CHASET
*     03/05/90  RTP   Change entry for 'Inquire Text' entry to set
*                     KERROR = 39 (S392).
*     07/06/90  PLP   Removed IPREC (not used).
*     02/07/90  RTP   Write ASF to CGM when there is a change for all
*                     attributes (see GKS BUG - S407)
*                     Remove entries 179 & 185 from computed GOTO as
*                     should reply not OUT or OUTIN (see GKS BUG - S394)
*     17/07/90  PLP   RMK added GKS bug numbers to some of the logs.
*     01/08/90  RTP   Use segment utilities for all segment functions
*                     ie remove setting of New Frame flag (see GKS BUG C43)
*     07/11/90  KEVP  Removed temporary code, no longer needed,
*                     because of the fix to bug C18.
*     16/01/91  RTP   Add Application Data for User Items
*                     Correct MESSAGE code to output text > 80 bytes
*                     Add Escape code
*     28/01/00  RTP   Change date format for Y2K compliance
*
*    Last Modified on  16 Jan 1991  at  15:02:11
*
*  ARGUMENTS
*  ---------
*     INP   IENT   Entrypoint code
*     INP   NID    Size of array IDAT
*     INP   IDAT   Integer data passed to workstation
*     INP   NRD    Size of arrays RX and RY
*     INP   NCD    Size of Character array
*     INP   RX     Real X-coordinate data passed to workstation
*     INP   RY     Real Y-coordinate data passed to workstation
*     INP   STR    Character array
*
      INTEGER IENT,NID,IDAT(NID),NRD,NCD
      REAL RX(NRD), RY(NRD)
*
      CHARACTER*80 STR(NCD)
*
*   SUBROUTINE CALLS IN WORKSTATION DRIVER:
*      GK2MBS : Converts unsigned integers into hex characters in
*               bitstream format.

*      GK2MBU : Writes a hex character string into a string buffer;
*               when this is full, it writes the buffer into the CGM.

*      GK2MCL  : Does initialisation etc for each GKS Clear Workstation

*      GK2MEA : Converts text strings into ASCII characters

*      GK2MHA : Converts hex (as a character string) into ASCII
*               characters & writes them to CGM.

*      GK2MIC  : Initialises the common block

*      GK2MIH : Converts integers into hex characters

*      GK2MIW : Is given a list of integers, converts them to hex &
*               writes them to the CGM.

*      GK2MRG : Converts RGB values into hex characters, and writes
*               them to the CGM.

*      GK2MRH : Converts real numbers into hex mantissas & exponents

*      GK2MRW : Is given a list of real points, converts them to VDC,
*               converts them to hex characters, and writes them
*               to the CGM.

*      GK2MTR : Derives the workstation total transformation

*      GK2MWA : Changes asfs from 1 to 0 & vice versa, converts the
*               aspects from GKS to their CGM counterparts & writes
*               them to the CGM.

*   FUNCTION(S):
*      GK2MNA : Returns the ASCII code for a character

*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYWCA/    Workstation index and type
*     Read   /ASPCT/     KLNTYA,KPLCIA,KMKTYA,KPMCIA,KTXFNA,
*                        KTXCIA,KFAISA,KFACIA..(PAR)
*     Modify /GKYWCA/    Set error and workstation category
*     Modify /GKYWSL/    Set up KCID in the state list
*     Modify /GKYWKD/    Derive workstation 'total' transform
*                        In KWKDAT(n,KWKIX) keep current primitive indexes
*                        in 1 to 4 & store picture number in 5.
*     Modify /GKYERR/    KERROR
*
      INCLUDE '../../include/GKS_PAR'
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

*   Include the CGM data Common Block
      INCLUDE '../../include/gkcgm.cmn'

*
*  LOCALS
*  ------
*
* Do Loop Variables & temporary variable
      INTEGER I,N,K,J,TEMP

*   Buffer variables
      INTEGER RECLEN
      CHARACTER INCHAR*81, MANTIS*9, EXPONE*9
*
      REAL TEMPR, REALA(12)
*
*     IPL    offset to aspect list for polyline
*     IPM    offset to aspect list for polymarker
*     ITX    offset to aspect list for text
*     IFA    offset to aspect list for fill area
*     NFONTS No of Fonts in Font List
*     ISTST  Start string characters
*     ISTTM  Terminate string characters
*     FIRSTP Used for checking Attribute changes
*     BUNDLE Used to check if any ASF is Bundled
*
      INTEGER IDAY,MONTH,IYEAR,IHOUR,IMIN,ISEC

      INTEGER    IPL,  IPM,  ITX, IFA, OLD, NEW, NFONTS
      PARAMETER (IPL=0,IPM=3,ITX=6,IFA=10,OLD=41,NEW=42,NFONTS=16)
      INTEGER    REALPT, VDC, VDCX, VDCY
      PARAMETER (REALPT=1,VDC=2,VDCX=3,VDCY=4)
      CHARACTER*5  STSTRT, STTERM
      PARAMETER (STSTRT = '1B58 ', STTERM = '1B5C ')
      CHARACTER SLASH,COLON,SNAME*39
      PARAMETER (SLASH='/',COLON=':')

*     Used for translating RAL GKS fonts to CGM index
      INTEGER KRALFN(NFONTS), NAMELEN(NFONTS)
      CHARACTER*24 FNNAME(NFONTS)
*
      INTEGER INTA(19)
      LOGICAL FIRSTP, BUNDLE
*
      PARAMETER (RECLEN=80)
      DATA KRALFN/1,-101,-102,-103,-104,-105,-106,-107,-110,-115,
     +              -108,-109,-111,-112,-113,-114/
      DATA NAMELEN/8, 15,  14,  21,  15,  22,  15,  22,  21,  21,
     +                22,  22,  24,  22,  21,  22/
      DATA FNNAME/'Hardware',                'Hershey Simplex',
     +            'Hershey Duplex',          'Hershey Simplex Greek',
     +            'Hershey Complex',         'Hershey Complex Italic',
     +            'Hershey Triplex',         'Hershey Triplex Italic',
     +            'Hershey Complex Greek',   'Hershey Complex Maths',
     +            'Hershey Simplex Script',  'Hershey Complex Script',
     +            'Hershey Complex Cyrillic','Hershey English Gothic',
     +            'Hershey German Gothic',   'Hershey Italian Gothic'/
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
*
*  COMMENTS
*  --------

* This part of the MO driver maps the GKS functionality, but
* has no knowledge of the metafile encoding except for the ITEM
* TYPE values.

*
*---------------------------------------------------------------------



* GOTO conditional on the entrypoint

      GOTO(  10,  20,  30,  40,  50,  60,9999,9999,9999, 100,
     :      110, 120, 130, 140, 150, 160, 170,9999,9999,9999,
     :     9999,9999, 230, 240, 250, 260, 270, 280,9999,9999,
     :      310, 320, 330,9999,9999,9999,9999,9999,9999,9999,
     :      410, 410, 410, 410, 410, 410, 410, 410, 410, 410,
     :      410,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :     9938,9938,9938,9938,9937,9938,9938,9938,9938,9938,
     :     9938,9938,9937,9938,9938,9938,9938,9938,9937,9938,
     :     9938,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      910,9934,9934) IENT

      GOTO(9939,9939,9999,9999,9999,9999,9999,9999,9999,9999,
     :     1300,1300,1300,1300,1300,1300,1300,1370,9939,1300,
     :     1300,1300,1300,1300,1300,1300,1460,9938,9938,9938,
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
      IF(KERROR.NE.0) GOTO 9999
      KCID(KWKIX) = KWI1
      KDFM(KWKIX) = GBNIG
      KWIO(KWKIX) = GNO
      KIMRGM(KWKIX) = GSUPPD

      QWCHXP(KWKIX) = 1.0
      KWTXFN(KWKIX) = 1

* Ask operating system to make a connection.

      CALL GKIOOP(KFWKFW,KCID(KWKIX),KWCID(KWKIX))

      IF(KERROR.EQ.0) THEN

*   Initialise Common Block
         CALL GK2MIC
         DO 11 I=1,4
            KWKDAT(I,KWKIX) = 1
   11    CONTINUE

* Set picture number = 0 to prevent character substitution
         KWKDAT(5,KWKIX) = 0

         CALL GK2MHA('3020 ')

*  Character Substitution for tilde
         CALL GK2MHA('7E3E ')
*  Character Substitution for null
         CALL GK2MHA('7E40 ')
*  Character Substitution for LF
         CALL GK2MHA('7E4A ')
*  Character Substitution for CR
         CALL GK2MHA('7E4D ')
*  Character Substitution for ESC (Not used)
*        CALL GK2MHA('7E5B ')

*  Set Picture number = 1
         KWKDAT(5,KWKIX) = 1

         CALL GK2MHA(STTERM)
         CALL GK2MHA(STSTRT)

* Write out name and header
         CALL GK2MEA(10,'RAL GKS - ' )

* Obtain the date from the system
         CALL GKDATE(IYEAR,MONTH,IDAY)
* Take off century
         IYEAR = MOD(IYEAR,100)

* Obtain the time from the system
         CALL GKTIME(IHOUR,IMIN,ISEC)

* Format it for the metafile header
         WRITE(SNAME,'(I2.2,5(A1,I2.2))') IDAY,SLASH,MONTH,SLASH,IYEAR,
     :                             ' ',IHOUR,COLON,IMIN,COLON,ISEC

         CALL GK2MEA(17,SNAME)
         CALL GK2MHA(STTERM)

* Metafile Descriptor Elements

*---------------------------------------------
* Metafile Version (of standard) currently = 1
*---------------------------------------------
         CALL GK2MHA('3120 ')
         CALL GK2MIW(1,1)

*----------------------
* Metafile Description
*----------------------
         CALL GK2MHA('3121 ')
*   Construct & write header
         CALL GK2MHA(STSTRT)

* Pick up the system title from GKMC.PAR - only want 39 chars
         SNAME = CVERS

* Get rid of all trailing blanks but 1
         DO 12 I=39,0,-1
            IF(SNAME(I-1:I-1).NE.' ')GOTO 13
  12     CONTINUE
  13     CONTINUE
         CALL GK2MEA(I,SNAME)

         CALL GK2MHA(STTERM)

*----------------------
* VDC Type
*----------------------
         CALL GK2MHA('3122 ')
         CALL GK2MIW(1,KVDCTY(KWKIX))

*----------------------
* Integer Precision
*----------------------
         CALL GK2MHA('3123 ')

*   Largest integer code+1 (No of bits)
         CALL GK2MIW(1,KINTPR(KWKIX))

*----------------------
* Real Precision
*----------------------
         CALL GK2MHA('3124 ')

*   Largest real code+1 (No of bits)
         CALL GK2MIW(1,KMXRPR(KWKIX))

*   Smallest real code (No of bits)
         CALL GK2MIW(1,KMNRPR(KWKIX))

*   Default exponent for reals
         CALL GK2MIW(1,KDEFEX(KWKIX))

*   Exponents allowed (0=allowed,1=forbidden)
         CALL GK2MIW(1,KEXPAL(KWKIX))

*----------------------
* Index Precision
*----------------------
         CALL GK2MHA('3125 ')
         CALL GK2MIW(1,KIXPR(KWKIX))

*----------------------
* Colour Precision
*----------------------
         CALL GK2MHA('3126 ')
         CALL GK2MIW(1,KCOLPR(KWKIX))

*----------------------
* Colour Index Precision
*----------------------
         CALL GK2MHA('3127 ')
         CALL GK2MIW(1,KCIXPR(KWKIX))

*----------------------
* Maximum Colour Index
*----------------------
         CALL GK2MHA('3128 ')
         CALL GK2MIW(1,KMXCIX(KWKIX))

*----------------------
* Colour Value Extent
*----------------------
         CALL GK2MHA('3129 ')
         CALL GK2MRG(0.0,0.0,0.0,KCOLPR(KWKIX))
         CALL GK2MRG(1.0,1.0,1.0,KCOLPR(KWKIX))

*----------------------
* Metafile Element List
*----------------------
         CALL GK2MHA('312A ')
         CALL GK2MHA(STSTRT)

*   Drawing Plus Control set
         CALL GK2MIW(1,1)
         CALL GK2MHA(STTERM)

*----------------------
* Metafile Defaults Replacement
*----------------------
         CALL GK2MHA('312B ')

*   VDC Integer Precision
         CALL GK2MHA('3320 ')
*   Largest integer code+1 (No of bits)
         CALL GK2MIW(1,KVDCIP(KWKIX))

*   VDC Real Precision
         CALL GK2MHA('3321 ')
*   Largest real code+1 (No of bits)
         CALL GK2MIW(1,KVMXRP(KWKIX))

*   Smallest real code (No of bits)
         CALL GK2MIW(1,KVMNRP(KWKIX))

*   Default exponent for reals
         CALL GK2MIW(1,KVDEFX(KWKIX))

*   Exponents allowed (0=allowed,1=forbidden)
         CALL GK2MIW(1,KVXALL(KWKIX))

*   Change aspect source flag defaults to bundled
         CALL GK2MHA('3631 ')
         CALL GK2MWA(511,0)

*   End of metafile defaults replacement
         CALL GK2MHA('312C ')


*----------------------
* Font List
*----------------------
         CALL GK2MHA('312D ')
         DO 15 I = 1, NFONTS
            CALL GK2MHA(STSTRT)
            CALL GK2MEA( NAMELEN(I), FNNAME(I) )
            CALL GK2MHA(STTERM)
   15    CONTINUE

*----------------------
*  Character Set List
*----------------------
         CALL GK2MHA('312E ')

*   Specify UK 96 character set as character set index 1
         CALL GK2MIW(1,1)
         CALL GK2MHA(STSTRT)
         CALL GK2MHA('41 ')
         CALL GK2MHA(STTERM)

*----------------------
* Character Coding Announcer
*----------------------
         CALL GK2MHA('312F ')

*   Coding technique: basic 7-bit
         CALL GK2MIW(1,0)

**************************************
* End of Metafile Descriptor Elements
**************************************

*   Begin Picture - Defer until first drawing
         NEWFR = .TRUE.
         KCLEAR = .FALSE.

*---------------------------------------------------------------------*

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

*----------------------
* End Picture
*----------------------
         IF ( NEWFR ) CALL GK2MCL

         CALL GK2MHA('3024 ')

*----------------------
* End Metafile
*----------------------
         CALL GK2MHA('3021 ')

* Flush buffer
         IF(KCHANO(KWKIX).NE.1) THEN
*           CALL GKFOFO(KIOPB, COUTCH(KWKIX), ILEFT)
            WRITE(KWCID(KWKIX),22) COUTCH(KWKIX)(1:RECLEN)
            KCHANO(KWKIX)=1
         ENDIF
 22      FORMAT(A)

* Ask the operating system to break the connection.
         CALL GKIOCL(KFWKFW,KCID(KWKIX),KWCID(KWKIX))

* Close GKS data structures for this workstation
         CALL GKCWSL(KWKIX)
         CALL GKSLDL(KSSGPT(KWKIX))
      ENDIF
      GOTO 9999
*
* CLEAR WORKSTATION
* -----------------
   30 CONTINUE
      IF(KWI1.EQ.1) GOTO 9999
      KWDONE = KACEPT
* Delete all segments on workstation
      CALL GKSLDL(KSSGPT(KWKIX))

*   Flag begin picture, but defer all initialisation
      NEWFR = .TRUE.

      GOTO 9999


*----------------------
* Redraw all segments on workstation
*----------------------
   40 CONTINUE
      KWDONE=KRFUSE

*   Flag begin picture, but defer all initialisation
      NEWFR = .TRUE.

      GOTO 9999

*----------------------
* Update workstation; this is a good one!
*----------------------
   50 CONTINUE
      KWDONE=KRFUSE
      GOTO 9999

*----------------------
* Set deferral state
*----------------------

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



*----------------------
* Message
*----------------------
*   IDAT(NID) contains the message in ASCII integers

  100 CONTINUE
      IF ( NEWFR ) THEN
         CALL GK2MCL
         NEWFR = .FALSE.
      ENDIF
      CALL GK2MHA('3721 ')

*   Action/no Action required (No Action=0 | Action=1)
      CALL GK2MIW(1,0)

*   Send Character string to metafile, also used by TEXT
  101 CONTINUE

*   Open character String
      CALL GK2MHA(STSTRT)
      TEMP=NID
      N = 0

*   Split string into 80 character chunks & send to the buffer
  103 CONTINUE
      IF (TEMP.GT.80) THEN
         I=80
      ELSE
         I=TEMP
      ENDIF

      DO 106 J=1,I
         N=N+1
         INCHAR(J:J)=CHAR(IDAT(N))
  106 CONTINUE
      INCHAR(I+1:I+1)=CHAR(0)
      CALL GK2MBU(INCHAR)
      TEMP=TEMP-80
      IF (TEMP.GT.0) GOTO 103

*   Close character string
      CALL GK2MHA(STTERM)
      GOTO 9999

*----------------------
* ESCAPE
*----------------------
  110 CONTINUE
      IF ( NEWFR ) THEN
         CALL GK2MCL
         NEWFR = .FALSE.
      ENDIF

      CALL GK2MHA('3720 ')

*   Escape identifier
      CALL GK2MIW(1,KWI1)

*  Don't know length of data record but currently 0
      N = 0

* Write Data Record to CGM

  111 CONTINUE
      CALL GK2MHA(STSTRT)
      IF ( N.GT.0 ) THEN
         DO 112 I=1,(N-1)/80+1
            J = MIN(80, N-80*(I-1) )
            CALL GK2MEA ( J, STR(I)(1:J) )
  112    CONTINUE
      ENDIF
      CALL GK2MHA(STTERM)

      GOTO 9999

*----------------------
* Polyline
*----------------------
  120 CONTINUE
      IF ( KCVIS .EQ. GINVIS ) GOTO 9999
      IF ( NEWFR ) THEN
         CALL GK2MCL
         NEWFR = .FALSE.
      ENDIF
      BUNDLE = .FALSE.

* If Set Representation then set individual attributes
      IF (LIXSET(KIPLI,KWKIX)) THEN
         KSTLST(NEW,1,KWKIX)=KSTLST(KIPLI,1,KWKIX)
         QSTLST(NEW,1,KWKIX)=QSTLST(KIPLI,1,KWKIX)
         KSTLST(NEW,2,KWKIX)=KSTLST(KIPLI,2,KWKIX)
         DO 121 J=KLNTYA,KPLCIA
            KSETAS(NEW,J+IPL,KWKIX)=GINDIV
 121     CONTINUE
      ELSE
*  Set Current ASFs
         DO 1211 J=KLNTYA,KPLCIA
            KSETAS(NEW,J+IPL,KWKIX)=KIPLAF(J)
            IF ( KIPLAF(J) .EQ. GBUNDL ) BUNDLE = .TRUE.
 1211    CONTINUE
      ENDIF

* Set Index if different to current index
      IF( BUNDLE .AND. KWKDAT(1,KWKIX).NE.KIPLI) THEN
         KWKDAT(1,KWKIX)=KIPLI
         CALL GK2MHA('3520 ')
         CALL GK2MIW(1,KIPLI)
      ENDIF

* If ASFs individual set individual attributes
      I=KLNTYA
      IF (KIPLAF(I).EQ.GINDIV) THEN
         KSTLST(NEW,1,KWKIX)=KILNTY
      ENDIF
      IF (KIPLAF(I+1).EQ.GINDIV) THEN
         QSTLST(NEW,1,KWKIX)=QILNWD
      ENDIF
      IF (KIPLAF(I+2).EQ.GINDIV) THEN
         KSTLST(NEW,2,KWKIX)=KIPLCI
      ENDIF

* Set changed ASFs
      FIRSTP=.TRUE.
      DO 122 J=KLNTYA,KPLCIA
         IF (KSETAS(OLD,J+IPL,KWKIX).NE.KSETAS(NEW,J+IPL,KWKIX)) THEN
            KSETAS(OLD,J+IPL,KWKIX)=KSETAS(NEW,J+IPL,KWKIX)
            IF(FIRSTP) THEN
               CALL GK2MHA('3631 ')
               FIRSTP=.FALSE.
            ENDIF
            CALL GK2MWA(J+IPL,KSETAS(NEW,J+IPL,KWKIX))
         ENDIF
 122  CONTINUE

* Set current linetype
      IF(KSTLST(OLD,1,KWKIX).NE.KSTLST(NEW,1,KWKIX))THEN
         KSTLST(OLD,1,KWKIX)=KSTLST(NEW,1,KWKIX)
         CALL GK2MHA('3521 ')
         CALL GK2MIW(1,KSTLST(NEW,1,KWKIX))
      ENDIF

* Linewidth scale factor
      IF(QSTLST(OLD,1,KWKIX).NE.QSTLST(NEW,1,KWKIX))THEN
         QSTLST(OLD,1,KWKIX)=QSTLST(NEW,1,KWKIX)
         CALL GK2MHA('3522 ')
         CALL GK2MRH(QSTLST(NEW,1,KWKIX),MANTIS,EXPONE,REALPT)
         CALL GK2MBU(MANTIS)
         CALL GK2MBU(EXPONE)
      ENDIF

* Polyline colour index
      IF(KSTLST(OLD,2,KWKIX).NE.KSTLST(NEW,2,KWKIX))THEN
         KSTLST(OLD,2,KWKIX)=KSTLST(NEW,2,KWKIX)
         CALL GK2MHA('3523 ')
         CALL GK2MIW(1,KSTLST(NEW,2,KWKIX))
      ENDIF

* If attributes have been set for GDP to use then return to GDP
      IF(ISGDP(KWKIX)) GOTO 172
*---------------------------------------------------------------------

* Write polyline itself
      CALL GK2MHA('20 ')
      CALL GK2MRW(NRD,RX,RY,.TRUE.)
      GOTO 9999

*----------------------
* Polymarker
*----------------------
  130 CONTINUE
      IF ( KCVIS .EQ. GINVIS ) GOTO 9999
      IF ( NEWFR ) THEN
         CALL GK2MCL
         NEWFR = .FALSE.
      ENDIF
      BUNDLE = .FALSE.

* If Set Representation then set individual attributes
      IF (MIXSET(KIPMI,KWKIX)) THEN
         KSTLST(NEW,3,KWKIX)=KSTLST(KIPMI,3,KWKIX)
         QSTLST(NEW,2,KWKIX)=QSTLST(KIPMI,2,KWKIX)
         KSTLST(NEW,4,KWKIX)=KSTLST(KIPMI,4,KWKIX)
         DO 131 J=KMKTYA,KPMCIA
            KSETAS(NEW,J+IPM,KWKIX)=GINDIV
 131     CONTINUE
      ELSE
*  Set Current ASFs
         DO 1311 J=KMKTYA,KPMCIA
            KSETAS(NEW,J+IPM,KWKIX)=KIPMAF(J)
            IF ( KIPMAF(J) .EQ. GBUNDL ) BUNDLE = .TRUE.
 1311    CONTINUE
      ENDIF

* Set Index if different to current index
      IF( BUNDLE .AND. KWKDAT(2,KWKIX).NE.KIPMI ) THEN
         KWKDAT(2,KWKIX)=KIPMI
         CALL GK2MHA('3524 ')
         CALL GK2MIW(1,KIPMI)
      ENDIF

* If ASFs individual set individual attributes
      I=KMKTYA
      IF (KIPMAF(I).EQ.GINDIV) THEN
         KSTLST(NEW,3,KWKIX)=KIMKTY
      ENDIF
      IF (KIPMAF(I+1).EQ.GINDIV) THEN
         QSTLST(NEW,2,KWKIX)=QIMKSZ
      ENDIF
      IF (KIPMAF(I+2).EQ.GINDIV) THEN
         KSTLST(NEW,4,KWKIX)=KIPMCI
      ENDIF

* Set changed ASFs
      FIRSTP=.TRUE.
      DO 132 J=KMKTYA,KPMCIA
         IF (KSETAS(OLD,J+IPM,KWKIX).NE.KSETAS(NEW,J+IPM,KWKIX)) THEN
            KSETAS(OLD,J+IPM,KWKIX)=KSETAS(NEW,J+IPM,KWKIX)
            IF(FIRSTP) THEN
               CALL GK2MHA('3631 ')
               FIRSTP=.FALSE.
            ENDIF
            CALL GK2MWA(J+IPM,KSETAS(NEW,J+IPM,KWKIX))
         ENDIF
 132  CONTINUE

* Set current markertype
      IF(KSTLST(OLD,3,KWKIX).NE.KSTLST(NEW,3,KWKIX))THEN
         KSTLST(OLD,3,KWKIX)=KSTLST(NEW,3,KWKIX)
         CALL GK2MHA('3525 ')
         CALL GK2MIW(1,KSTLST(NEW,3,KWKIX))
      ENDIF

* Marker size scale factor
      IF(QSTLST(OLD,2,KWKIX).NE.QSTLST(NEW,2,KWKIX))THEN
         QSTLST(OLD,2,KWKIX)=QSTLST(NEW,2,KWKIX)
         CALL GK2MHA('3526 ')
         CALL GK2MRH(QSTLST(NEW,2,KWKIX),MANTIS,EXPONE,REALPT)
         CALL GK2MBU(MANTIS)
         CALL GK2MBU(EXPONE)
      ENDIF

* Polymarker colour index
      IF(KSTLST(OLD,4,KWKIX).NE.KSTLST(NEW,4,KWKIX))THEN
         KSTLST(OLD,4,KWKIX)=KSTLST(NEW,4,KWKIX)
         CALL GK2MHA('3527 ')
         CALL GK2MIW(1,KSTLST(NEW,4,KWKIX))
      ENDIF
*---------------------------------------------------------------------

* Write polymarker itself
      CALL GK2MHA('22 ')
      CALL GK2MRW(NRD,RX,RY,.TRUE.)
      GOTO 9999

*----------------------
* Text
*----------------------
  140 CONTINUE
      IF ( KCVIS .EQ. GINVIS ) GOTO 9999
      IF ( NEWFR ) THEN
         CALL GK2MCL
         NEWFR = .FALSE.
      ENDIF
      BUNDLE = .FALSE.

      CALL GKDTXB

* If Set Representation then set individual attributes
      IF (TIXSET(KITXI,KWKIX)) THEN

* 1st 2 not very relevant to GKS (Character Set indexes)
         KSTLST(NEW,5,KWKIX)=KSTLST(KITXI,5,KWKIX)
         KSTLST(NEW,6,KWKIX)=KSTLST(KITXI,6,KWKIX)
         KSTLST(NEW,7,KWKIX)=KSTLST(KITXI,7,KWKIX)
         KSTLST(NEW,8,KWKIX)=KSTLST(KITXI,8,KWKIX)
         QSTLST(NEW,3,KWKIX)=QSTLST(KITXI,3,KWKIX)
         QSTLST(NEW,4,KWKIX)=QSTLST(KITXI,4,KWKIX)
         KSTLST(NEW,9,KWKIX)=KSTLST(KITXI,9,KWKIX)
         DO 141 J=KTXFNA,KTXCIA
            KSETAS(NEW,J+ITX,KWKIX)=GINDIV
 141     CONTINUE
      ELSE
*  Set Current ASFs
         DO 1411 J=KTXFNA,KTXCIA
            KSETAS(NEW,J+ITX,KWKIX)=KITXAF(J)
            IF ( KITXAF(J) .EQ. GBUNDL ) BUNDLE = .TRUE.
 1411    CONTINUE
      ENDIF

* Set Index if different to current index
      IF(KWKDAT(3,KWKIX).NE.KITXI) THEN
         KWKDAT(3,KWKIX)=KITXI
         CALL GK2MHA('3530 ')
         CALL GK2MIW(1,KITXI)
      ENDIF

* If ASFs individual set individual attributes
      I=KTXFNA

* Set text font and precision
* (Has no exact equivalent in CGM )
      IF (KITXAF(I).EQ.GINDIV) THEN
         KSTLST(NEW,5,KWKIX)=1
         KSTLST(NEW,6,KWKIX)=1
         KSTLST(NEW,7,KWKIX)=KITXFN
         KSTLST(NEW,8,KWKIX)=KITXPR
      ENDIF

* Character expansion factor
      IF (KITXAF(I+1).EQ.GINDIV) THEN
         QSTLST(NEW,3,KWKIX)=QICHXP
      ENDIF

* Character spacing
      IF (KITXAF(I+2).EQ.GINDIV) THEN
         QSTLST(NEW,4,KWKIX)=QICHSP
      ENDIF

* Text colour index
      IF (KITXAF(I+3).EQ.GINDIV) THEN
         KSTLST(NEW,9,KWKIX)=KITXCI
      ENDIF

* Set changed ASFs
      FIRSTP=.TRUE.
      DO 142 J=KTXFNA,KTXCIA
         IF (KSETAS(OLD,J+ITX,KWKIX).NE.KSETAS(NEW,J+ITX,KWKIX)) THEN
            KSETAS(OLD,J+ITX,KWKIX)=KSETAS(NEW,J+ITX,KWKIX)
            IF(FIRSTP) THEN
               CALL GK2MHA('3631 ')
               FIRSTP=.FALSE.
            ENDIF
            CALL GK2MWA(J+ITX,KSETAS(NEW,J+ITX,KWKIX))
         ENDIF
 142  CONTINUE

*   Character set index
      IF(KSTLST(OLD,5,KWKIX).NE.KSTLST(NEW,5,KWKIX))THEN
         KSTLST(OLD,5,KWKIX)=KSTLST(NEW,5,KWKIX)
         CALL GK2MHA('353A ')
         CALL GK2MIW(1,KSTLST(NEW,5,KWKIX))
      ENDIF

*   Alternate character set index
      IF(KSTLST(OLD,6,KWKIX).NE.KSTLST(NEW,6,KWKIX))THEN
         KSTLST(OLD,6,KWKIX)=KSTLST(NEW,6,KWKIX)
         CALL GK2MHA('353B ')
         CALL GK2MIW(1,KSTLST(NEW,6,KWKIX))
      ENDIF

*   Text font index
      IF(KSTLST(OLD,7,KWKIX).NE.KSTLST(NEW,7,KWKIX))THEN
         KSTLST(OLD,7,KWKIX)=KSTLST(NEW,7,KWKIX)
         DO 143 I = 1,NFONTS
            IF (KSTLST(NEW,7,KWKIX).EQ.KRALFN(I)) THEN
               CALL GK2MHA('3531 ')
               CALL GK2MIW(1,I)
            ENDIF
 143     CONTINUE
      ENDIF

*   Text precision
      IF(KSTLST(OLD,8,KWKIX).NE.KSTLST(NEW,8,KWKIX))THEN
         KSTLST(OLD,8,KWKIX)=KSTLST(NEW,8,KWKIX)
         CALL GK2MHA('3532 ')
         CALL GK2MIW(1,KSTLST(NEW,8,KWKIX))
      ENDIF

* Character expansion factor
      IF(QSTLST(OLD,3,KWKIX).NE.QSTLST(NEW,3,KWKIX))THEN
         QSTLST(OLD,3,KWKIX)=QSTLST(NEW,3,KWKIX)
         CALL GK2MHA('3533 ')
         CALL GK2MRH(QSTLST(NEW,3,KWKIX),MANTIS,EXPONE,REALPT)
         CALL GK2MBU(MANTIS)
         CALL GK2MBU(EXPONE)
      ENDIF

* Set character spacing
      IF(QSTLST(OLD,4,KWKIX).NE.QSTLST(NEW,4,KWKIX))THEN
         QSTLST(OLD,4,KWKIX)=QSTLST(NEW,4,KWKIX)
         CALL GK2MHA('3534 ')
         CALL GK2MRH(QSTLST(NEW,4,KWKIX),MANTIS,EXPONE,REALPT)
         CALL GK2MBU(MANTIS)
         CALL GK2MBU(EXPONE)
      ENDIF

* Text colour index
      IF(KSTLST(OLD,9,KWKIX).NE.KSTLST(NEW,9,KWKIX))THEN
         KSTLST(OLD,9,KWKIX)=KSTLST(NEW,9,KWKIX)
         CALL GK2MHA('3535 ')
         CALL GK2MIW(1,KSTLST(NEW,9,KWKIX))
      ENDIF

* Charactor vectors
      IF( (QSTLST(OLD,5,KWKIX).NE.QICHHX).OR.
     +   (QSTLST(OLD,6,KWKIX).NE.QICHHY).OR.
     +   (QSTLST(OLD,7,KWKIX).NE.QICHWX).OR.
     +   (QSTLST(OLD,8,KWKIX).NE.QICHWY)) THEN
         QSTLST(OLD,5,KWKIX)=QICHHX
         QSTLST(OLD,6,KWKIX)=QICHHY
         QSTLST(OLD,7,KWKIX)=QICHWX
         QSTLST(OLD,8,KWKIX)=QICHWY
         CALL GK2MHA('3536 ')

* Transform vectors from WC to VDC
         CALL GKTWDV(QICHHX,QICHHY,QWCHHX(KWKIX),QWCHHY(KWKIX))
         CALL GKTWDV(QICHWX,QICHWY,QWCHWX(KWKIX),QWCHWY(KWKIX))

*   Work out character height
         TEMPR=SQRT((QWCHHX(KWKIX)**2)+(QWCHHY(KWKIX)**2))
         CALL GK2MRH(TEMPR,MANTIS,EXPONE,VDC)
*   Write to CGM
         CALL GK2MBU(MANTIS)
         CALL GK2MBU(EXPONE)

*   Character Orientation
         CALL GK2MHA('3537 ')

*   Character height vector
         CALL GK2MRH(QWCHHX(KWKIX),MANTIS,EXPONE,VDC)
         CALL GK2MBU(MANTIS)
         CALL GK2MBU(EXPONE)
         CALL GK2MRH(QWCHHY(KWKIX),MANTIS,EXPONE,VDC)
         CALL GK2MBU(MANTIS)
         CALL GK2MBU(EXPONE)

*   Character width vector
         CALL GK2MRH(QWCHWX(KWKIX),MANTIS,EXPONE,VDC)
         CALL GK2MBU(MANTIS)
         CALL GK2MBU(EXPONE)
         CALL GK2MRH(QWCHWY(KWKIX),MANTIS,EXPONE,VDC)
         CALL GK2MBU(MANTIS)
         CALL GK2MBU(EXPONE)
      ENDIF

* Text path
      IF(KSTLST(OLD,10,KWKIX).NE.KITXP)THEN
         KSTLST(OLD,10,KWKIX)=KITXP
         CALL GK2MHA('3538 ')
         CALL GK2MIW(1,KITXP)
      ENDIF

* Text alignment
      IF((KSTLST(OLD,11,KWKIX).NE.KIHTXA).OR.
     +   (KSTLST(OLD,12,KWKIX).NE.KIVTXA))THEN
         KSTLST(OLD,11,KWKIX)=KIHTXA
         KSTLST(OLD,12,KWKIX)=KIVTXA
         CALL GK2MHA('3539 ')
         CALL GK2MIW(1,KIHTXA)
         CALL GK2MIW(1,KIVTXA)

*   Put in 2 Dummy reals for continuous horizontal & vertical
*   Alignments; (Dummy cos they won't be used, since GKS doesn't
*   have continuous alignment).
         CALL GK2MRH(1.0,MANTIS,EXPONE,REALPT)
         CALL GK2MBU(MANTIS)
         CALL GK2MBU(EXPONE)
         CALL GK2MBU(MANTIS)
         CALL GK2MBU(EXPONE)
      ENDIF

*---------------------------------------------------------------------

* Write text itself
      CALL GK2MHA('23 ')

*   Starting Point
      CALL GK2MRW(1,QWRA(1),QWRA(2),.FALSE.)

*   Final/not final (Not final=0 | Final=1)
      CALL GK2MIW(1,1)

*   Write TEXT to CGM (use Message code)
      GOTO 101

*----------------------
* Fill area
*----------------------
  150 CONTINUE
      IF ( KCVIS .EQ. GINVIS ) GOTO 9999
      IF ( NEWFR ) THEN
         CALL GK2MCL
         NEWFR = .FALSE.
      ENDIF
      BUNDLE = .FALSE.

* If Set Representation then set individual attributes
      IF (FASET(KIFAI,KWKIX)) THEN
         KSTLST(NEW,13,KWKIX)=KSTLST(KIFAI,13,KWKIX)
         IF(KSTLST(NEW,13,KWKIX).EQ.2)THEN
            KSTLST(NEW,14,KWKIX)=KSTLST(KIFAI,14,KWKIX)
         ELSEIF(KSTLST(NEW,13,KWKIX).EQ.3)THEN
            KSTLST(NEW,15,KWKIX)=KSTLST(KIFAI,15,KWKIX)
         ENDIF
         KSTLST(NEW,16,KWKIX)=KSTLST(KIFAI,16,KWKIX)
         DO 151 J=KFAISA,KFACIA
            KSETAS(NEW,J+IFA,KWKIX)=GINDIV
 151     CONTINUE
      ELSE
         DO 1511 J=KFAISA,KFACIA
            KSETAS(NEW,J+IFA,KWKIX)=KIFAAF(J)
            IF ( KIFAAF(J) .EQ. GBUNDL ) BUNDLE = .TRUE.
 1511    CONTINUE
      ENDIF

* Set Index if different to current index
      IF( BUNDLE .AND. KWKDAT(4,KWKIX).NE.KIFAI ) THEN
         KWKDAT(4,KWKIX)=KIFAI
         CALL GK2MHA('3620 ')
         CALL GK2MIW(1,KIFAI)
      ENDIF

* If ASFs individual set individual attributes
      I=KFAISA
      IF (KIFAAF(I).EQ.GINDIV) THEN
         KSTLST(NEW,13,KWKIX)=KIFAIS
      ENDIF
      IF (KIFAAF(I+1).EQ.GINDIV) THEN
         KSTLST(NEW,14,KWKIX)=KIFASI
*   Juggle the RAL GKS Hatches to fit the CGM ones
         IF(KIFASI.GT.0)THEN
            TEMP=KIFASI
         ELSE
            TEMP=ABS(KIFASI)
            IF(TEMP.EQ.3) THEN
               TEMP=4
            ELSEIF(TEMP.EQ.4) THEN
               TEMP=3
            ELSEIF(TEMP.GT.6) THEN
               TEMP = -TEMP
            ENDIF
         ENDIF
         KSTLST(NEW,15,KWKIX)=TEMP
      ENDIF
      IF (KIFAAF(I+2).EQ.GINDIV) THEN
         KSTLST(NEW,16,KWKIX)=KIFACI
      ENDIF

* Set changed ASFs
      FIRSTP=.TRUE.
      DO 152 J=KFAISA,KFACIA
         IF (KSETAS(OLD,J+IFA,KWKIX).NE.KSETAS(NEW,J+IFA,KWKIX)) THEN
            KSETAS(OLD,J+IFA,KWKIX)=KSETAS(NEW,J+IFA,KWKIX)
            IF(FIRSTP) THEN
               CALL GK2MHA('3631 ')
               FIRSTP=.FALSE.
            ENDIF
            CALL GK2MWA(J+IFA,KSETAS(NEW,J+IFA,KWKIX))
         ENDIF
 152  CONTINUE

* Fill area interior style
      IF(KSTLST(OLD,13,KWKIX).NE.KSTLST(NEW,13,KWKIX))THEN
         KSTLST(OLD,13,KWKIX)=KSTLST(NEW,13,KWKIX)
         CALL GK2MHA('3621 ')
         CALL GK2MIW(1,KSTLST(NEW,13,KWKIX))
      ENDIF

* Set fill area style index
      IF(KSTLST(OLD,13,KWKIX).EQ.2)THEN
*   Pattern index
         IF(KSTLST(OLD,14,KWKIX).NE.KSTLST(NEW,14,KWKIX))THEN
            KSTLST(OLD,14,KWKIX)=KSTLST(NEW,14,KWKIX)
            CALL GK2MHA('3624 ')
            CALL GK2MIW(1,KSTLST(NEW,14,KWKIX))
         ENDIF
      ELSEIF(KSTLST(OLD,13,KWKIX).EQ.3)THEN
*   Hatch index
         IF(KSTLST(OLD,15,KWKIX).NE.KSTLST(NEW,15,KWKIX))THEN
            KSTLST(OLD,15,KWKIX)=KSTLST(NEW,15,KWKIX)
            CALL GK2MHA('3623 ')
            CALL GK2MIW(1,KSTLST(NEW,15,KWKIX))
         ENDIF
      ENDIF

* Fill area colour index
      IF(KSTLST(OLD,16,KWKIX).NE.KSTLST(NEW,16,KWKIX))THEN
         KSTLST(OLD,16,KWKIX)=KSTLST(NEW,16,KWKIX)
         CALL GK2MHA('3622 ')
         CALL GK2MIW(1,KSTLST(NEW,16,KWKIX))
      ENDIF

* Pattern size
      IF( (QSTLST(OLD,9,KWKIX).NE.QIPAWX).OR.
     +   (QSTLST(OLD,10,KWKIX).NE.QIPAWY).OR.
     +   (QSTLST(OLD,11,KWKIX).NE.QIPAHX).OR.
     +   (QSTLST(OLD,12,KWKIX).NE.QIPAHY))THEN
         QSTLST(OLD,9,KWKIX)=QIPAWX
         QSTLST(OLD,10,KWKIX)=QIPAWY
         QSTLST(OLD,11,KWKIX)=QIPAHX
         QSTLST(OLD,12,KWKIX)=QIPAHY

         CALL GK2MHA('362C ')
* Transform vectors from WC to NDC(VDC)
         CALL GKTWDV(QIPAWX,QIPAWY,QWPAWX(KWKIX),QWPAWY(KWKIX))
         CALL GKTWDV(QIPAHX,QIPAHY,QWPAHX(KWKIX),QWPAHY(KWKIX))
*   Write to CGM
         CALL GK2MRH(QWPAHX(KWKIX),MANTIS,EXPONE,VDC)
         CALL GK2MBU(MANTIS)
         CALL GK2MBU(EXPONE)

         CALL GK2MRH(QWPAHY(KWKIX),MANTIS,EXPONE,VDC)
         CALL GK2MBU(MANTIS)
         CALL GK2MBU(EXPONE)

         CALL GK2MRH(QWPAWX(KWKIX),MANTIS,EXPONE,VDC)
         CALL GK2MBU(MANTIS)
         CALL GK2MBU(EXPONE)

         CALL GK2MRH(QWPAWY(KWKIX),MANTIS,EXPONE,VDC)
         CALL GK2MBU(MANTIS)
         CALL GK2MBU(EXPONE)
      ENDIF

* Pattern reference point
      IF( (QSTLST(OLD,13,KWKIX).NE.QIPAX).OR.
     +   (QSTLST(OLD,14,KWKIX).NE.QIPAY))THEN
         QSTLST(OLD,13,KWKIX)=QIPAX
         QSTLST(OLD,14,KWKIX)=QIPAY
         CALL GK2MHA('362A ')
         CALL GK2MRW(1,QIPAX,QIPAY,.FALSE.)
      ENDIF

* If attributes have been set for GDP to use then return to GDP
      IF(ISGDP(KWKIX)) GOTO 172
*---------------------------------------------------------------------
      CALL GK2MHA('26 ')
      CALL GK2MRW(NRD,RX,RY,.TRUE.)
      GOTO 9999

*----------------------
* Cell array
*----------------------
* Data expected:
* QWR1,..,6 Coordinates of corner points of pixel array
* KWI1      Dimension (in X)
* KWI2      Dimension (in Y)
* KWI3      Start column
* KWI4      Start row
* KWI5      Number of columns
* KWI6      Number of rows
* IDAT(NID) Array of colour indices stored row by row

  160 CONTINUE
      IF ( KCVIS .EQ. GINVIS ) GOTO 9999
      IF ( NEWFR ) THEN
         CALL GK2MCL
         NEWFR = .FALSE.
      ENDIF

      CALL GK2MHA('28 ')

*   Get boundary points & write to CGM
      CALL GK2MRW(1,QWR1,QWR2,.FALSE.)
      CALL GK2MRW(1,QWR3,QWR4,.FALSE.)
      CALL GK2MRW(1,QWR5,QWR6,.FALSE.)

*   Number of columns
      CALL GK2MIW(1,KWI5)

*   Number of rows
      CALL GK2MIW(1,KWI6)

*   Local colour precision
      CALL GK2MIW(1,KCIXPR(KWKIX))

*   Suss out whether runlength or index list is best
      N=0
      TEMP=-1
      DO 162 K=KWI4,KWI4+KWI6-1
         DO 161 I=KWI3,KWI3+KWI5-1
            IF(TEMP.NE.IDAT((K-1)*KWI1+I)) N=N+2
            TEMP=IDAT((K-1)*KWI1+I)
  161    CONTINUE
  162 CONTINUE


      IF(NID.LT.N) THEN
*   Type of list; in this case Indexed normal colour list
         CALL GK2MIW(1,0)

*   Calculate the indices to be sent first & send them to the CGM
         DO 165 K=KWI4,KWI4+KWI6-1
            DO 164 I=KWI3,KWI3+KWI5-1
               CALL GK2MIW(1,IDAT((K-1)*KWI1+I))
  164       CONTINUE
  165    CONTINUE
      ELSE
*   Type of list; Runlength format
         CALL GK2MIW(1,2)

*   Calculate the indices to be sent first & send them to the CGM
         N=0
         TEMP=IDAT((KWI4-1)*KWI1+KWI3)
         CALL GK2MIW(1,TEMP)
         DO 168 K=KWI4,KWI4+KWI6-1
            DO 166 I=KWI3,KWI3+KWI5-1

*   If index same as last, increment counter
               IF(TEMP.EQ.IDAT((K-1)*KWI1+I)) THEN
                  N=N+1

*   If different, output number of repetitions for last number
*   and output new index
               ELSE
                  CALL GK2MIW(1,N)
                  N=1
                  TEMP=IDAT((K-1)*KWI1+I)
                  CALL GK2MIW(1,TEMP)
               ENDIF
  166       CONTINUE
  168    CONTINUE
         CALL GK2MIW(1,N)
      ENDIF

      GOTO 9999

*----------------------
* GDP
*----------------------
  170 CONTINUE
      IF ( KCVIS .EQ. GINVIS ) GOTO 9999
      IF ( NEWFR ) THEN
         CALL GK2MCL
         NEWFR = .FALSE.
      ENDIF

*   Check that attributes have been updated
      ISGDP(KWKIX)=.TRUE.
      IF(KWI1.EQ.-1)THEN

*  Polyline attributes
         GOTO 120
      ELSE

*  Fill area attributes
         GOTO 150
      ENDIF

 172  CONTINUE
      ISGDP(KWKIX)=.FALSE.

      CALL GK2MHA('29 ')

*   GDP identifier
      CALL GK2MIW(1,KWI1)

*   Make space for an extra 3 points
      DO 175 I=1,NRD
         REALA(I+3)=RX(I)
         REALA(I+3+6)=RY(I)
  175 CONTINUE

*   Put 3 corners of the unit square into the arrays
*   (GIITM wants em for some reason)
      REALA(1)=QWRA(1)
      REALA(7)=QWRA(2)
      REALA(2)=QWRA(3)
      REALA(8)=QWRA(4)
      REALA(3)=QWRA(5)
      REALA(9)=QWRA(6)

*   Write the points
      CALL GK2MRW(NRD+3,REALA,REALA(7),.TRUE.)

*   Data record (Nothing in it as yet), Use ESCAPE code
      N = 0
      GOTO 111

*----------------------
* Set polyline representation
* (No such thing in CGM, so simulate it, but set bundle table entry
*----------------------
  230 CONTINUE
* Find out if index exists or there is room for another bundle
      DO 231 J=1,KMXPLB
        IF(KWI1.EQ.KPLI(J,KWKIX) .OR. KPLI(J,KWKIX).EQ.KNIL)
     :  GOTO 232
  231 CONTINUE
* No room
      KERROR = -1004
      GOTO 9999
  232 CONTINUE
* Now set representation
      KPLI(J,KWKIX) = KWI1
      KLNTY(J,KWKIX) = KWI2
      QLNWD(J,KWKIX) = QWR1
      KPLCI(J,KWKIX) = KWI3

* KWI1 is the polyline index
* Set Line Representation flag to TRUE
      LIXSET(KWI1,KWKIX)=.TRUE.

* Set Line type
      KSTLST(KWI1,1,KWKIX)=KWI2

* Line width
      QSTLST(KWI1,1,KWKIX)=QWR1

* Line Colour
      KSTLST(KWI1,2,KWKIX)=KWI3

      GOTO 9999

*----------------------
* Set marker representation
*----------------------
  240 CONTINUE
* Find out if index exists or there is room for another bundle
      DO 241 J=1,KMXPMB
        IF(KWI1.EQ.KPMI(J,KWKIX) .OR. KPMI(J,KWKIX).EQ.KNIL)
     :    GOTO 242
  241 CONTINUE
* No room
      KERROR = -1004
      GOTO 9999
* Now set representation
  242 CONTINUE
      KPMI(J,KWKIX) = KWI1
      KMKTY(J,KWKIX) = KWI2
      QMKSZ(J,KWKIX) = QWR1
      KPMCI(J,KWKIX) = KWI3

* KWI1 is the polmarker index
* Set Marker Representation flag to TRUE
      MIXSET(KWI1,KWKIX)=.TRUE.

* Get Marker type
      KSTLST(KWI1,3,KWKIX)=KWI2

* Get Marker Size Scale factor
      QSTLST(KWI1,2,KWKIX)=QWR1

* Marker Colour
      KSTLST(KWI1,4,KWKIX)=KWI3

      GOTO 9999

*----------------------
* Text representation
*----------------------
  250 CONTINUE
* Find out if index exists or there is room for another bundle
      DO 251 J=1,KMXTXB
        IF(KWI1.EQ.KTXI(J,KWKIX) .OR. KTXI(J,KWKIX).EQ.KNIL)
     :    GOTO 252
  251 CONTINUE
* No room
      KERROR = -1004
      GOTO 9999
* Now set representation
  252 CONTINUE
      KTXI(J,KWKIX) = KWI1
      KTXFN(J,KWKIX) = KWI2
      KTXPR(J,KWKIX) = KWI3
      QCHXP(J,KWKIX) = QWR1
      QCHSP(J,KWKIX) = QWR2
      KTXCI(J,KWKIX) = KWI4

* KWI1 is the text bundle index
* Set Text Representation flag to TRUE
      TIXSET(KWI1,KWKIX)=.TRUE.

* Character set index
      KSTLST(KWI1,5,KWKIX)=1

* Alternate character set index
      KSTLST(KWI1,6,KWKIX)=1

* Text font index
      KSTLST(KWI1,7,KWKIX)=KWI2

* Text precision
      KSTLST(KWI1,8,KWKIX)=KWI3

* Character expansion factor
      QSTLST(KWI1,3,KWKIX)=QWR1

* Character spacing
      QSTLST(KWI1,4,KWKIX)=QWR2

* Text colour index
      KSTLST(KWI1,9,KWKIX)=KWI4

      GOTO 9999

*----------------------
* Set fill area representation
*----------------------
  260 CONTINUE
* Find out if index exists or there is room for another bundle
      DO 261 J=1,KMXFAB
        IF(KWI1.EQ.KFAI(J,KWKIX) .OR. KFAI(J,KWKIX).EQ.KNIL)
     :    GOTO 262
  261 CONTINUE
* No room
      KERROR = -1004
      GOTO 9999
* Now set representation
  262 CONTINUE
      KFAI(J,KWKIX) = KWI1
      KIS(J,KWKIX) = KWI2
      KSI(J,KWKIX) = KWI3
      KFACI(J,KWKIX) = KWI4

* Fill Bundle Index is KWI1
* Set Fill Area Representation flag to TRUE
      FASET(KWI1,KWKIX)=.TRUE.

* Set Fill Area Interior Style
      KSTLST(KWI1,13,KWKIX)=KWI2

* Set Fill Area Style Index
* Pattern
      IF (KWI2.EQ.2)THEN
         KSTLST(KWI1,14,KWKIX)=KWI3

* Hatch
      ELSE IF (KWI2.EQ.3)THEN
         IF(KWI3.GT.0)THEN
            TEMP=KWI3
         ELSE
            TEMP=ABS(KWI3)
            IF(TEMP.EQ.3) THEN
               TEMP=4
            ELSEIF(TEMP.EQ.4) THEN
               TEMP=3
            ELSEIF(TEMP.GT.6) THEN
               TEMP = -TEMP
            ENDIF
         ENDIF
         KSTLST(KWI1,15,KWKIX)=TEMP
      ENDIF

* Set Fill Area Colour Index
      KSTLST(KWI1,16,KWKIX)=KWI4
      GOTO 9999

*----------------------
* Set Pattern representation
*----------------------
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

      IF ( NEWFR ) THEN
         CALL GK2MCL
         NEWFR = .FALSE.
      ENDIF

*   Pattern Table Index
      CALL GK2MHA('362B ')
      CALL GK2MIW (1,KWI1)

*   Get Columns & write to CGM
      CALL GK2MIW (1,KWI2)

*   Get Rows & write to CGM
      CALL GK2MIW (1,KWI3)

*   Local colour precision
      CALL GK2MIW(1,KCIXPR(KWKIX))

*   Type of list; in this case Indexed normal colour list
      CALL GK2MIW(1,0)

*   Calculate the indices to be sent & send to the CGM
      DO 276 K=KWI5,KWI5+KWI7-1
         DO 275 I=KWI4,KWI4+KWI6-1
            CALL GK2MIW(1,IDAT((K-1)*KWI2+I))
  275    CONTINUE
  276 CONTINUE
      GOTO 9999

*----------------------
* Colour representation
* (Colour table is the nearest CGM has to this)
*----------------------
  280 CONTINUE

* See if colour index valid
      IF (KWI1.GE.0 .AND. KWI1.LT.KPCI(KWKIX)) THEN
        QHP(KHPXR(KCTBPT(1,KWKIX))+KWI1) = QWR1
        QHP(KHPXR(KCTBPT(2,KWKIX))+KWI1) = QWR2
        QHP(KHPXR(KCTBPT(3,KWKIX))+KWI1) = QWR3
*   Store values
        QCLREP(KWI1,1,KWKIX)=QWR1
        QCLREP(KWI1,2,KWKIX)=QWR2
        QCLREP(KWI1,3,KWKIX)=QWR3
      ELSE
        KERROR = 93
      ENDIF


*   Output Colour table if within picture body

      IF ( .NOT. NEWFR ) THEN

         CALL GK2MHA('3630 ')

*   Get index & write to CGM
         CALL GK2MIW(1,KWI1)

*   Specify Normal Format
         CALL GK2MIW(1,0)

*   Get RGB values & write to CGM
         CALL GK2MRG(QWR1,QWR2,QWR3,KCOLPR(KWKIX))
      ENDIF

      GOTO 9999
*
* NORM TRANS
* ----------
*Data expected: QWR1,...,6     Transformation C2

  310 CONTINUE

* Derive 'workstation total transformation'
      CALL GK2MTR

*----------------------
* Clip rectangle
*----------------------

*   If settings have changed then update the statelist
      IF( (QSTLST(OLD,15,KWKIX).NE.QWR7).OR.
     +    (QSTLST(OLD,16,KWKIX).NE.QWR9).OR.
     +    (QSTLST(OLD,17,KWKIX).NE.QWR8).OR.
     +    (QSTLST(OLD,18,KWKIX).NE.QWR10)) THEN
         DO 314 I=OLD,NEW
            QSTLST(I,15,KWKIX)=QWR7
            QSTLST(I,16,KWKIX)=QWR9
            QSTLST(I,17,KWKIX)=QWR8
            QSTLST(I,18,KWKIX)=QWR10
  314    CONTINUE

*  Output if within picture body
         IF ( .NOT. NEWFR ) THEN
            CALL GK2MHA('3324 ')
            DO 315 I=15,18
               CALL GK2MRH(QSTLST(OLD,I,KWKIX),MANTIS,EXPONE,REALPT)
               CALL GK2MBU(MANTIS)
               CALL GK2MBU(EXPONE)
  315       CONTINUE
         ENDIF
      ENDIF
      GOTO 9999

*----------------------
* Workstation window
* (Map to VDC Extent)
*----------------------
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

*   Set VDC Extent (in Common Block)
      QSTLST(NEW,19,KWKIX)=QWR1
      QSTLST(NEW,20,KWKIX)=QWR3
      QSTLST(NEW,21,KWKIX)=QWR2
      QSTLST(NEW,22,KWKIX)=QWR4

*   Signal New frame if necessary
      NEWFR = .TRUE.

      GOTO 9999

*----------------------
* Workstation viewport
* (No comparison in CGM)
*----------------------
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

      GOTO 9999

* Segment entrypoints *
  410 CONTINUE
      CALL GKSGWK(IENT,.FALSE.)
      GOTO 9999

*
* WRITE ITEM TO GKSM
* ------------------
* The item type supplied is in KWI1.
* Write APPLICATION DATA to CGM
  910 CONTINUE
      CALL GK2MHA('3722 ')
      CALL GK2MIW(1,KWI1)

* Check that Item Length is valid

      N = KWI2
      IF ( N.GT.NCD*80 .OR. N.LT.0 ) THEN
         KERROR = 161
         GOTO 9999
      ENDIF

* Write Data Record to CGM, Use Escape code

      GOTO 111
*
* INQUIRE EVERYTHING
* ------------------
 1300 CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999
*
*
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


      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      IF(KWI3.GE.KPCI(KWKIX)) KWI3 = 1
      GOTO 9999

* --------------------------------------------------------------
* Inquire set member of segment names on workstation
* --------------------------------------------------------------
 1460 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KSGRQ = KSGRQ + 1
      GOTO 9999

* INQUIRE WORKSTATION CATEGORY
* ----------------------------
 1700 CONTINUE
      KWI1=GMO
      GOTO 9999

* Inquire text facilities ... on entry KWI1 specifies list element reque
* Allow for string and char precision font (number 1) explicitly
*1790 CONTINUE
*     KWI6 = 6
*     IF( KWI1.GT.KFNTMX+2 ) THEN
*       KERROR=2002
*       KWI1=KFNTMX+2
*       GOTO 9999
*     ENDIF
*     IF( KWI1.GT.KFNTMX ) THEN

*       String or Char precision font
*         IF( KWI1.EQ.KFNTMX+1 ) KWI3 = GSTRP
*         IF( KWI1.EQ.KFNTMX+2 ) KWI3 = GCHARP
*         KWI2 = 1
*     ELSE

*       Stroke precision font
*       Make sure that fonts are available
*         IF( KDBFLS.EQ.KFLNA ) THEN
*           KERROR=-1009
*           GOTO 9999
*         ENDIF
*         IF( KDBFLS.EQ.KFLCL ) CALL GKXON
*         IF( KERROR.NE.0 ) GOTO 9999
*         KWI2 = KHFONT(KWI1)
*         KWI3 = 2
*     ENDIF
*     KWI1 = KFNTMX
*     IF (KWKIX.NE.KNIL) THEN
*       KWI4 = KCHH(KWKIX)
*       KWI5 = KCHXPF(KWKIX)
*       KWI6 = KPTXI(KWKIX)
*       QWR1 = QMNCHH(KWKIX)
*       QWR2 = QMXCHH(KWKIX)
*       QWR3 = QMNCHX(KWKIX)
*       QWR4 = QMXCHX(KWKIX)
*     ELSE
*       CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
*       IF (KERROR.EQ.0) THEN
*         KWI4 = INTA(5)
*         KWI5 = INTA(6)
*         KWI6 = INTA(10)
*         QWR1 = REALA(9)
*         QWR2 = REALA(10)
*         QWR3 = REALA(11)
*         QWR4 = REALA(12)
*       ENDIF
*     ENDIF
*     KWI6 = 6
*     GOTO 9999

* ---------------------------------------------------------------
* Inquire Colour facilities
* ---------------------------------------------------------------
*1850 CONTINUE
*     Data returned:
*     KWI1   : number of available colours
*     KWI2   : colour available
*     KWI3   : number of predefined colour indices

*     KWI1 = 256
*     KWI2 = GCOLOR
*     KWI3 = KWI1
*     GOTO 9999

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

 9999 RETURN
      END
