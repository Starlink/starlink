      PROGRAM GKSWDT

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  The formatted WDT is read in and a binary (direct access) version is
*  created. A listing is produced and CDRIVE.INC written.
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*     10/06/83  CJW   Use new error routine
*     07/05/86  RMK   Prime F77 - changed stream numbers used in
*                     WDTBUF.PAR. Deleted copy of GKNTOA as should use
*                     same one as in GKS library (LINK file picks this
*                     up).
*     11/11/87  RMK   Throughout file, removed unused local variables.
*     15/12/87  PJWR  Changed version string for UNIX.
*     17/03/89  PLP   Commented TA's correction to the READ
*                     statement in DOWDT. This differs the
*                     OS4 version of gkswdt.
*     29/05/92  RTP   Add IOSTAT to Direct access WRITE for Indigo
*     15/07/92  DLT   Divide record length by KRCBYT when opening
*                     direct access file.
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /ERROR/  ?
*
      INCLUDE '../../include/gkwdt.par'
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdterr.par'
      INCLUDE 'wdtwkd.cmn'
      INCLUDE 'wdterr.cmn'
*
*  LOCALS
*  ------
*     DONE   TRUE when have finished processing the WDT file
*     J      Loop index
*
      LOGICAL DONE
      INTEGER J
*
*  COMMENTS
*  --------
*     For Version number see subroutine HEADNG
*
*---------------------------------------------------------------------

*  Prologue

      WRITE(*,1000) 'UNIX  '//VERSN

      KNWK = 0

*     Initialize Error Handler

      DO 1 J = KCAUTN, KERROR
         KNERR(J) = 0
    1 CONTINUE

*     Open the formatted and Direct access WDT files

      CALL CONECT

*     Initialize Directory

      CALL INIDIR

*     Initialize Listing

      CALL HEADNG

      DONE = .FALSE.

*     Repeat
    2 CONTINUE

         CALL DOWDT(DONE)

*     Until Done
      IF (.NOT. DONE) GO TO 2

*     Close Directory

      CALL DIREND

*     Close Listing

      CALL FOOTNG

*     Close streams

      CALL DISCON


*  -------
*  FORMATS
*  -------

 1000 FORMAT('1',
     : 'GKS Workstation Description Table Generation Program',
     :       ' Version ',A/
     :       ' ',
     : '----------------------------------------------------',
     :       '---------','-----'/)
      KNWK = 0
      END

*
*
      CHARACTER * 40 FUNCTION APPEND (AMESS, BMESS)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Appends BMESS to AMESS and returns the resulting string is returned as
*  the value. Any trailing blanks in AMESS are ignored.
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP   AMESS  First character string
*     INP   BMESS  Second character string
*
      CHARACTER * (*) AMESS, BMESS
*
*  FUNCTION RETURN VALUE
*  ---------------------
*  AMESS // BMESS with trailing spaces on AMESS removed.
*
*
*  LOCALS
*  ------
*     LAMESS True length of AMESS
*     BLANK  Blank
*
      CHARACTER BLANK
      PARAMETER ( BLANK = ' ' )
      INTEGER LAMESS
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

*     Find true length of AMESS

         DO 1 LAMESS = LEN(AMESS), 1, -1
            IF (AMESS(LAMESS:LAMESS).NE.BLANK) GO TO 2
    1    CONTINUE
            LAMESS = 0

    2    CONTINUE

*     Put them together

         IF (LAMESS.GT.0) THEN
            APPEND = AMESS(1:LAMESS) // BMESS
         ELSE
            APPEND = BMESS
         END IF
      END

*
*
      SUBROUTINE CH(NITEM)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Process Choice device
*
*  MAINTENANCE LOG
*  ---------------
*     09/06/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT   NITEM  Number of devices read
*
      INTEGER NITEM
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     ?      /WDTWRK/ ?
*
      INCLUDE '../../include/gkwdt.par'
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtwrk.cmn'
*
*  LOCALS
*  ------
*     LFOUND  TRUE if PID found; FALSE if terminator
*
      LOGICAL LFOUND
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------


      NITEM = 0

      CALL SECBGN(KCH)

*     repeat { until terminator }
    1 CONTINUE

         CALL INPDEV (KCH, KCHDR, LFOUND)

         IF (LFOUND) THEN

            NITEM = NITEM + 1

*           No Choice Specific Data


         END IF

*     Until termintor
      IF (LFOUND) GO TO 1

*     Flush buffer

      CALL SECEND(KCH)

      END

*
*
      SUBROUTINE CONECT
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Make connections to required files.
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*     17/06/83  CJW   CDRIVE.INC added
*     23/04/85  RMK   Changes to use more suitable filenames on PRIME.
*     xx/03/86  DRJF  Prime FTN77 version - RECL measured in words.
*     03/05/86  RMK   Prime F77 version, RECL measured in bytes not words.
*     26/06/86  RMK   SHS (Feb 85) removed use of substring argument in
*                     statement function for CHKANS (S112).
*     15/12/87  PJWR  Changed default names for UNIX.
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     ?      /FILES/  ?
*
      INCLUDE '../../include/gkmc.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtfls.cmn'
*
*  LOCALS
*  ------
*     ANSWER A reply to a question
*     OK     OK if formatted file exists
*     NOTOK  NOT OK if output file already existed
*     DDAT   Default name of input WDT data file
*     DDAM   Default name of direct access WDT output file
*     DINC   Default name of call layer include file
*     DLIST  Default name of listing file
*     INP    Temp variable to hold filename input by user
*
      CHARACTER * 10 ANSWER
      LOGICAL NOTOK
      LOGICAL OK
*
      CHARACTER * 10 DDAT, DDAM, DINC
      CHARACTER * 11 DLIST
      PARAMETER ( DDAT = 'gkswdt.dat', DDAM = 'gks.wdt',
     :            DINC = 'cdrive.inc', DLIST = 'gkswdt.lst')
      CHARACTER * 40 INP
      CHARACTER * 1 BLANK
      PARAMETER (BLANK = ' ')
*
*  EXTERNALS
*  ---------
      CHARACTER * 10 TRUP
      CHARACTER * 40 APPEND
*
*  STATEMENT FUNCTIONS
*  -------------------
*     CHKANS is true if YES
*
      CHARACTER * 1 MYTXT
      LOGICAL CHKANS
      CHKANS( MYTXT ) = (TRUP(MYTXT) .EQ. 'Y')
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     FWDT     disk         OPENED
*     DWDT     disk         OPENED
*     LIST     disk         OPENED
*     CDRIVE   disk         OPENED
*
*  COMMENTS
*  --------
*     PRIME version: has sensible default filenames and makes
*                    no assumptions about file extensions.
*
*---------------------------------------------------------------------
*
*     Open the formatted WDT
*
      FFILE = APPEND (BLANK, DDAT)
      WRITE (*, 1000) DDAT
 1000 FORMAT ('Input data file? <CR> gives default as ', A)
      READ (*,1001) INP
 1001 FORMAT (A)
      IF (INP .NE. ' ') FFILE = APPEND (BLANK, INP)
      INQUIRE ( FILE = FFILE, EXIST = OK )
*
*     while not ok do
    1 CONTINUE
      IF (OK) GOTO 2
*
         WRITE (*,*) 'Cannot Find WDT Input File '//INP
         WRITE (*, 1000) DDAT
         READ (*,1001) INP
         IF (INP .NE. ' ') FFILE = APPEND (BLANK, INP)
         INQUIRE ( FILE = FFILE, EXIST = OK )
*
*     end while
      GOTO 1
    2 CONTINUE
*
      OPEN ( UNIT = FWDT, FILE = FFILE, STATUS = 'OLD' )
*     This is required by UNIX - does no harm on other systems
      REWIND FWDT
*
*     Open the Direct Access WDT
*
    3 CONTINUE
      DFILE = APPEND (BLANK, DDAM)
      WRITE (*,1002) DDAM
 1002 FORMAT ('Name of Direct Access File? <CR> gives default as ', A)
      READ (*,1001) INP
      IF (INP .NE. ' ') DFILE = APPEND (BLANK, INP)
      INQUIRE ( FILE = DFILE, EXIST = NOTOK )
      IF (NOTOK) THEN
         WRITE (*,1003)
 1003    FORMAT('Direct Access File already exists. OK to overwrite?')
         READ (*,1001) ANSWER
         OK = CHKANS(ANSWER(1:1))
      ELSE
         OK = .TRUE.
      ENDIF
      IF (.NOT. OK) GOTO 3
*
      OPEN ( UNIT = DWDT, FILE = DFILE, STATUS = 'UNKNOWN',
     :       ACCESS = 'DIRECT', RECL = KMXQBU/KRCBYT)
*
*     Open the Listing file
*
    4 CONTINUE
      LFILE = APPEND (BLANK, DLIST)
      WRITE(*,1004) DLIST
 1004 FORMAT('Name of Listing File? <CR> gives default as ', A)
      READ (*,1001) INP
      IF (INP .NE. ' ') LFILE = APPEND (BLANK, INP)
      INQUIRE ( FILE = LFILE, EXIST = NOTOK )
      IF (NOTOK) THEN
         WRITE (*,1005)
 1005 FORMAT ('Listing File already exists. OK to overwrite?')
         READ (*,1001) ANSWER
         OK = CHKANS(ANSWER(1:1))
      ELSE
         OK = .TRUE.
      ENDIF
      IF (.NOT. OK) GOTO 4
*
      OPEN ( UNIT = LIST, FILE = LFILE, STATUS = 'UNKNOWN' )
      REWIND LIST
*
*     Open CDRIVE.INC
*
    6 CONTINUE
      CFILE = APPEND (BLANK, DINC)
      WRITE (*,1006) DINC
 1006 FORMAT ('Name of Call Layer Code File? <CR> gives default as ', A)
      READ (*,1001) INP
      IF (INP .NE. ' ') CFILE = APPEND (BLANK, INP)
      INQUIRE ( FILE = CFILE, EXIST = NOTOK )
      IF (NOTOK) THEN
         WRITE (*,1007)
 1007    FORMAT('Call Layer Code File already exists.OK to overwrite?')
         READ (*,1001) ANSWER
         OK = CHKANS(ANSWER(1:1))
      ELSE
         OK = .TRUE.
      ENDIF
      IF (.NOT. OK) GOTO 6
*
      OPEN ( UNIT = CDRIVE, FILE = CFILE, STATUS = 'UNKNOWN' )
      REWIND CDRIVE
*
      END
*
      SUBROUTINE CT(NITEM)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Process Colour table.
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT   NITEM  Number of items found.
*
      INTEGER NITEM
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     ?      /WDTWRK/ ?
*
      INCLUDE '../../include/gkwdt.par'
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtwrk.cmn'
*
*  LOCALS
*  ------
*     NINT   Number of integers in an item
*     NREAL  Number of reals in an item
*
      INTEGER NINT, NREAL
      PARAMETER ( NINT = 1, NREAL = 3)
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     FWDT     DISK         Read formatted wdt
*     LIST     DISK         Write listing
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

      NITEM = 0

      CALL SECBGN(KPCTB)

*     Read Items

*                  KCTI    (= ITEM number)
      READ(FWDT,*) INTS(1)

*     Listing

      IF (INTS(1).GE.0) WRITE(LIST,1000)

*     while item>0 do
    1 CONTINUE
      IF (INTS(1).LT.0) GO TO 2

         NITEM = NITEM + 1

         CALL ITMNXT(KPCTB)

*                     RED,      GREEN,    BLUE
         READ(FWDT,*) REALS(1), REALS(2), REALS(3)

*        Listing

         WRITE(LIST,1001) INTS(1), REALS(1), REALS(2), REALS(3)

*        Add to buffer

         CALL ITMADD(KPCTB, NINT, INTS, NREAL, REALS)

*                     KCTI   (= ITEM number)
         READ(FWDT,*) INTS(1)

*     end while
      GO TO 1
    2 CONTINUE

*     Flush buffer

      CALL SECEND(KPCTB)

*  -------
*  FORMATS
*  -------

 1000 FORMAT(' COLOUR',T10,'  RED',T25,'  GREEN',T40,'  BLUE'/)
 1001 FORMAT(1X,I5,T10,F10.7,T25,F10.7,T40,F10.7)

      END

*
*
      SUBROUTINE DIREND

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  WDT file is initialised.
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     ?      /WDTWRK/ ?
*
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtwrk.cmn'
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------


*     Save Master directory

      CALL SECEND(KMDIR)

      END

*
*
      SUBROUTINE DISCON

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Disconect (close) files that were opened by CONECT.
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*     17/06/83  CJW   CDRIVE.INC added
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /WDTBUF/  unit numbers
*
      INCLUDE 'wdtbuf.par'
*
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     FWDT     DISK         Closed
*     DWDT     DISK         Closed
*     LIST     DISK         Closed
*     CDRIVE   DISK         Closed
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

      CLOSE (FWDT)
      CLOSE (DWDT)
      CLOSE (LIST)
      CLOSE (CDRIVE)

      END

*
*
      SUBROUTINE DOWDT( DONE )

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  The formatted WDT is read in and a binary (direct access) version is
*  created. A listing is produced . This routine is called once per WDT.
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*     09/06/83  CJW   Add input
*                     Specific error message for each 'Not Found'
*                     Code simplified by using loops
*     17/06/83  CJW   CDRIVE.INC supported by storing table of types and
*                     reading and storing the Driver identifier.
*     11/11/87  RMK   Cray compiler doesn't allow a character to be
*                     assigned to a function of itself.  So added a
*                     temporary string in use of TRUP (S222).
*     ??/??/??  TA    Added ERR=4 option to the new workstation type
*                     READ statement. This was required in order
*                     to make gkswdt palatable to the OS4
*                     fortran compiler.
*
*  ARGUMENTS
*  ---------
*     INOUT DONE   Becomes true when no WDT found
*
      LOGICAL DONE
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /ERROR/  ?
*     ?      /WDTBUF/  ?
*
      INCLUDE '../../include/gkwdt.par'
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtbuf.cmn'
      INCLUDE 'wdterr.par'
      INCLUDE 'wdtwkd.cmn'
*
*  LOCALS
*  ------
*
      INTEGER KLAST, KEND
      PARAMETER (KLAST = KST, KEND=KLAST+1)
      INTEGER NMBR(KWDT:KLAST), I
      LOGICAL FINISH, FOUNDT, FOUNDD
      CHARACTER * 2 AKEY, C(KWDT:KEND), TKEY
      CHARACTER * 10 TRUP
      DATA C /'WD', 'PL', 'PM', 'TX', 'FA', 'PA', 'CT', 'LC',
     :        'SK', 'VL', 'CH', 'PC', 'ST', 'EN'/
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     IDEBUG   printer      error messages written
*     IERROR   disk         error message file accessed
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

*  Prologue


*     Initialize counters to 'unset'

      DO 1 I = KWDT, KLAST
         NMBR(I) = KUNSET
    1 CONTINUE

*     Get workstation type

      READ(FWDT,*,END=4,ERR=4) KWKTYP
      IF (KWKTYP.NE.KUNSET) THEN

         KNWK = KNWK + 1
         KWKTY(KNWK) = KWKTYP

         READ(FWDT,1000) CWKDR(KNWK)
         WRITE(*,*)
         WRITE(*,*) ' Workstation :',KWKTYP,' Driver is GK',
     :              CWKDR(KNWK),'WD'
         WRITE(*,*) ' ==========='
         WRITE(*,*)

         FOUNDT = .FALSE.
         FOUNDD = .FALSE.
         DO 2 I = 1, KNWK-1
            FOUNDT = FOUNDT .OR. (KWKTYP.EQ.KWKTY(I))
            FOUNDD = FOUNDD .OR. (CWKDR(KNWK).EQ.CWKDR(I))
    2    CONTINUE

         IF (FOUNDT) THEN
            CALL ERROR(KERROR,
     :          'This Workstation type has already been read')
         ELSE IF (FOUNDD) THEN
            CALL ERROR(KWARNG,
     :          'Workstation Driver has been seen previously')
         END IF

*        Initialize Workstation directory

         CALL INIWKD

         FINISH = .FALSE.

*     Process the formatted WDT

*        Repeat -- until END found
    3    CONTINUE

         READ(FWDT,1000) AKEY
 1000    FORMAT(A)
         TKEY = TRUP(AKEY)
         AKEY = TKEY

*        Listing
         CALL SECTN(AKEY)

         WRITE(*,*) ' Section : ', AKEY
*        case akey of

         IF (AKEY .EQ. C(KWDT)) THEN

            IF (NMBR(KWDT) .NE. KUNSET)
     :         CALL ERROR(KWARNG,'WDT already read')
            CALL WDT(NMBR(KWDT))

         ELSE IF (AKEY .EQ. C(KPPLB)) THEN

            IF (NMBR(KPPLB) .NE. KUNSET)
     :         CALL ERROR(KWARNG,'PL already read')
            CALL PL(NMBR(KPPLB))

         ELSE IF (AKEY .EQ. C(KPPMB)) THEN

            IF (NMBR(KPPMB) .NE. KUNSET)
     :         CALL ERROR(KWARNG,'PM already read')
            CALL PM(NMBR(KPPMB))

         ELSE IF (AKEY .EQ. C(KPTXB)) THEN

            IF (NMBR(KPTXB) .NE. KUNSET)
     :         CALL ERROR(KWARNG,'TX already read')
            CALL TX(NMBR(KPTXB))

         ELSE IF (AKEY .EQ. C(KPFAB)) THEN

            IF (NMBR(KPFAB) .NE. KUNSET)
     :         CALL ERROR(KWARNG,'FA already read')
            CALL FA(NMBR(KPFAB))

         ELSE IF (AKEY .EQ. C(KPPASZ)) THEN

            IF (NMBR(KPPASZ) .NE. KUNSET)
     :         CALL ERROR(KWARNG,'PA already read')
            CALL PA(NMBR(KPPASZ))

         ELSE IF (AKEY .EQ. C(KPCTB)) THEN

            IF (NMBR(KPCTB) .NE. KUNSET)
     :         CALL ERROR(KWARNG,'CT already read')
            CALL CT(NMBR(KPCTB))

         ELSE IF (AKEY .EQ. C(KLC)) THEN

            IF (NMBR(KLC) .NE. KUNSET)
     :         CALL ERROR(KWARNG,'LC already read')
            CALL LC(NMBR(KLC))

         ELSE IF (AKEY .EQ. C(KSK)) THEN

            IF (NMBR(KSK) .NE. KUNSET)
     :         CALL ERROR(KWARNG,'SK already read')
            CALL SK(NMBR(KSK))

         ELSE IF (AKEY .EQ. C(KVL)) THEN

            IF (NMBR(KVL) .NE. KUNSET)
     :         CALL ERROR(KWARNG,'VL already read')
            CALL VL(NMBR(KVL))

         ELSE IF (AKEY .EQ. C(KCH)) THEN

            IF (NMBR(KCH) .NE. KUNSET)
     :         CALL ERROR(KWARNG,'CH already read')
            CALL CH(NMBR(KCH))

         ELSE IF (AKEY .EQ. C(KPC)) THEN

            IF (NMBR(KPC) .NE. KUNSET)
     :         CALL ERROR(KWARNG,'PC already read')
            CALL PC(NMBR(KPC))

         ELSE IF (AKEY .EQ. C(KST)) THEN

            IF (NMBR(KST) .NE. KUNSET)
     :         CALL ERROR(KWARNG,'ST already read')
            CALL ST(NMBR(KST))

         ELSE IF (AKEY .EQ. C(KEND)) THEN

            FINISH = .TRUE.

         ELSE

            CALL ERROR(KERROR,'Unknown key "'//AKEY//'" found')

         END IF

*        Until finish
         IF (.NOT. FINISH) GOTO 3

*        Save Workstation Directory

         CALL WKDEND

*        Check we have everything

         DO 5 I = KWDT, KLAST
            IF (NMBR(I).EQ.KUNSET) THEN
               CALL ERROR(KCAUTN,'Missing Input data section '// C(I))
               NMBR(I) = 0
            END IF
    5    CONTINUE

*        Do not want to store count of WDTs (always 1)

         DO 6 I = KWDT, KLAST-1
            NMBR(I) = NMBR(I+1)
    6    CONTINUE

*        Complete the WDT Section

         CALL WDTEND(KLAST-1, NMBR)

      ELSE

         DONE = .TRUE.

      END IF

      IF (KNWK .EQ. KMXWK) THEN
            CALL ERROR(KWARNG,'Maximum number of workstation processed')
            DONE = .TRUE.
      END IF

      RETURN

*     END-OF-FILE Handler

    4 CONTINUE
      DONE = .TRUE.

      END


*
*
      SUBROUTINE ERROR(LVL,TEXT)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Produce standard error message.
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*     09/06/83  CJW   Modified to support levels of error - rather than
*                     specific error numbers
*
*  ARGUMENTS
*  ---------
*     INP   LVL    Error Level
*     INP   TEXT   Error message
*
      INTEGER LVL
      CHARACTER * (*) TEXT
*
*  COMMON BLOCK USAGE
*  ------------------
*     WRITE  /ERROR/  Store error count
*
      INCLUDE 'wdterr.par'
      INCLUDE 'wdterr.cmn'
*
*  LOCALS
*  ------
*
      CHARACTER * 7 LEVEL(KCAUTN:KERROR)
      DATA LEVEL/'Caution','Warning','  Error'/
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

      WRITE(*,1000) LEVEL(LVL),TEXT
      KNERR(LVL) = KNERR(LVL) + 1

*  -------
*  FORMATS
*  -------

 1000 FORMAT(' ',A,' : ',A)

      END

*
*
      SUBROUTINE FA(NITEM)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Process Fill Area
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT   NITEM  Number of Bundles read
*
      INTEGER NITEM
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     ?      /WDTWRK/ ?
*
      INCLUDE '../../include/gkwdt.par'
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtwrk.cmn'
*
*  LOCALS
*  ------
*     NINT   Number of integers in an item
*     NREAL  Number of reals in an item
*
      INTEGER NINT, NREAL
      PARAMETER ( NINT = 4, NREAL = 0)
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     FWDT     DISK         Read
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------


      NITEM = 0

      CALL SECBGN(KPFAB)

*     Read Items

*                  KFAI    (= ITEM number)
      READ(FWDT,*) INTS(1)

*     Listing
      IF (INTS(1).GT.0) WRITE(LIST,1000)

*     while item>0 do
    1 CONTINUE
      IF (INTS(1).LT.0) GO TO 2

         NITEM = NITEM + 1

         CALL ITMNXT(KPFAB)

*                     KIS,     KSI,     KFACI
         READ(FWDT,*) INTS(2), INTS(3), INTS(4)

*        Listing

         WRITE(LIST,1001) INTS(1), INTS(2), INTS(3), INTS(4)

*        Add to buffer

         CALL ITMADD(KPFAB, NINT, INTS,NREAL, REALS)

*                     KFAI   (= ITEM number)
         READ(FWDT,*) INTS(1)

*     end while
      GO TO 1
    2 CONTINUE

*     Flush buffer

      CALL SECEND(KPFAB)

*  -------
*  FORMATS
*  -------

 1000 FORMAT(' BUNDLE',T10,'  KIS',T20,'  KSI',T30,' KFACI'/)
 1001 FORMAT(1X,I5,T10,I5,T20,I5,T30,I5)

      END

*
*
      SUBROUTINE FOOTNG

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Produce footing on Listing.
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*     09/06/83  CJW   Modified to support levels of error - rather than
*                     specific error numbers
*     17/06/83  CJW   CDRIVE.INC added
*     30/12/83  CJW   Change call layer to support CSS
*     07/05/86  RMK   Prime F77 version - added extra brackets in first
*                     WRITE on CDRIVE stream - compiler wasn't adding 10000.
*     26/06/86  RMK   Changed calculation of labels used in CDRIVE file to
*                     allow for negative workstation types.
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     Read   /ERROR/  List error counts
*
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdterr.par'
      INCLUDE 'wdterr.cmn'
      INCLUDE 'wdtwkd.cmn'
*
*  LOCALS
*  ------
*     ERRORS True if any error detected
*     J      Loop index
*     LEVEL  Names of error levels
*
      LOGICAL ERRORS
      INTEGER J
      CHARACTER * 7 LEVEL(KCAUTN:KERROR)
      DATA LEVEL/'Caution','Warning','  Error'/
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     CDRIVE   DISK         Output CDRIVE.INC
*     LIST     DISK         Output footing
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------


      ERRORS = .FALSE.

      DO 1 J = KCAUTN, KERROR
         IF (KNERR(J).GT.1) THEN
            IF (.NOT. ERRORS) WRITE(LIST,1000)
            WRITE(LIST,1001) LEVEL(J), KNERR(J)
            ERRORS = .TRUE.
         ELSE IF (KNERR(J).EQ.1) THEN
            IF (.NOT. ERRORS) WRITE(LIST,1000)
            WRITE(LIST,1002) LEVEL(J)
            ERRORS = .TRUE.
         END IF
    1 CONTINUE

*     Create CDRIVE.INC
      WRITE(CDRIVE,1003) ((J+10000), J = 1, KNWK-1)
      WRITE(CDRIVE,1004) 10000+KNWK
      WRITE(CDRIVE,1005) 10000, 'CS'
      DO 2 J = 1, KNWK
         WRITE(CDRIVE,1005) (J+10000), CWKDR(J)
    2 CONTINUE
      WRITE(CDRIVE,1006)

*  -------
*  FORMATS
*  -------

* Error Summary

 1000 FORMAT(/' Error Summary'/' ============='/)
 1001 FORMAT(' ',A,'s occurred ',I4,' times')
 1002 FORMAT('  ',A,' occurred once')

* CDRIVE.INC

 1003 FORMAT('      ITEMP = KIUSED'/
     :       '      GOTO ( 10000,'/
     :      ('     :  ',10(I5,',':)))
 1004 FORMAT('     :  ',I5,'  ) IWKTYP+1'//
     :      '      CALL GKBUG(-2018,NAME)'/
     :      '      GOTO 9999'/)

 1005 FORMAT(I5,' CONTINUE'/
     :     '      CALL GK',A2,'WD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)'/
     :     '      GOTO 9999'/)

 1006 FORMAT(' 9999 CONTINUE'/
     :     '      IF (KIUSED.NE.ITEMP) CALL GKBUG(-2000,NAME)'/)

      END

*
*
      SUBROUTINE HEADNG

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Produce heading on Listing.

*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     ?      /FILES/  ?
*
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtfls.cmn'
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     LIST     DISK         Output heading
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

      WRITE(LIST,1000) 'PRIME'//VERSN
      WRITE(LIST,1001) FFILE, DFILE

*  -------
*  FORMATS
*  -------

 1000 FORMAT('1',
     : 'GKS Workstation Description Table Generation Program',
     :       ' Version ',A/
     :       ' ',
     : '----------------------------------------------------',
     :       '---------','--- ---'/)
 1001 FORMAT(' Input File (Sequential) . . . . ',A/
     :       ' Output File (Direct Access) . . ',A)
      END

*
*
      SUBROUTINE INIDIR

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  WDT file is initialised.
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP   FRED   String to be displayed
*
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     ?      /WDTWRK/ ?
*
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtwrk.cmn'
*
*  LOCALS
*  ------
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     DWDT     DISK         WDT Directory written
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

*     Initialize buffer system

      CALL BFSET

*     Start Master directory

      CALL SECBGN(KMDIR)
      CALL ITMNXT(KMDIR)

      END

*
*
      SUBROUTINE INIWKD

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    WDT generator
*  Author:             anonymous
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  WDT file is initialised.
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*     07/02/90  KEVP  Used ENTBGN instead of SECBGN to get a valid record
*                     number for Master directory, now updated at end (S322).
*     07/02/90  KEVP  Eliminated superfluous parameter NINT (now KMXDIR).
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     ?      /WDTWRK/ ?
*
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtbuf.cmn'
      INCLUDE 'wdtwkd.cmn'
      INCLUDE 'wdtwrk.cmn'
      INCLUDE '../../include/gkwdt.par'
*
*  LOCALS
*  ------
*
      INTEGER J

      INTEGER    NDINT,     NDREAL
      PARAMETER (NDINT = 2, NDREAL = 0)
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     DWDT     disk         Update
*     LIST     disk         produce listing
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

*     Set up Workstation directory, getting record number
*     and preparing entry for master directory.

      INTS(1) = KWKTYP
      CALL ENTBGN(KWKDIR,INTS(2))
      CALL ITMNXT(KWKDIR)

      DO 1 J = 1, KMXDIR
         KWKITM(J) = KUNSET
    1 CONTINUE

*     Update master directory
      CALL ITMADD(KMDIR, NDINT, INTS, NDREAL, REALS)

*     Listing

      WRITE(LIST,1000)
      WRITE(LIST,1001) KWKTYP
      WRITE(LIST,1002)

*  -------
*  FORMATS
*  -------

 1000 FORMAT(/' ---------------------' )
 1001 FORMAT( ' Workstation Type ',I4 )
 1002 FORMAT( ' --------------------'/)

      END

*
*
      SUBROUTINE INPDEV (INTYPE, INDR, LFOUND)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Process Locator devices
*
*  MAINTENANCE LOG
*  ---------------
*     09/06/83  CJW   Original version stabilized
*     27/07/87  PJWR  Replaced call to PID with call to DR when PIDs
*                     were replaced by data records in the WDT.
*
*  ARGUMENTS
*  ---------
*     INP   INTYPE Device type
*     INP   INDR   Device Data Record Type
*     OUT   LFOUND ITEM found
*
      INTEGER INTYPE, INDR
      LOGICAL LFOUND
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     ?      /WDTWRK/ ?
*
      INCLUDE '../../include/gkwdt.par'
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtwrk.cmn'
*
*  LOCALS
*  ------
*     NINT   Number of integers in an item
*     NREAL  Number of reals in an item
*
      INTEGER NINT, NREAL
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     FWDT     DISK         Read LC devices
*     LIST     DISK         Write listing
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------



*        Device Number  (= ITEM number)

      READ(FWDT,*) INTS(1)

      LFOUND = (INTS(1).NE.KUNSET)

      IF (LFOUND) THEN
         WRITE(LIST,1000) INTS(1)
         CALL ITMNXT(INTYPE)

*           Echo Area

         READ(FWDT,*) REALS(1), REALS(2), REALS(3), REALS(4)
         WRITE(LIST,1001) REALS(1), REALS(2), REALS(3), REALS(4)

*           Add control to buffer

         NINT = 1
         NREAL = 4

         CALL ITMADD(INTYPE, NINT, INTS,NREAL, REALS)

*           Get Data Record - Uses INTS and REALS

         CALL DR(INDR)

*           Add sizes to buffer - left in INTS for me by DR

         NINT = 5
         NREAL = 0

         CALL ITMADD(INTYPE, NINT, INTS,NREAL, REALS)

      END IF

*  -------
*  FORMATS
*  -------

 1000 FORMAT(' DEVICE... ',I5)
 1001 FORMAT(T10,'Echo Area... ',F10.4,' : ',F10.4,', ',
     :       F10.4,' : ',F10.4)

      END

*
*
      SUBROUTINE LC(NITEM)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Process Locator devices
*
*  MAINTENANCE LOG
*  ---------------
*     09/06/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT   NITEM  Number of devices read
*
      INTEGER NITEM
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     ?      /WDTWRK/ ?
*
      INCLUDE '../../include/gkwdt.par'
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtwrk.cmn'
*
*  LOCALS
*  ------
*     LFOUND = True if input device, = False if terminator
*     NINT   Number of integers in an item
*     NREAL  Number of reals in an item
*
      LOGICAL LFOUND
      INTEGER NINT, NREAL
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     FWDT     DISK         Read LC devices
*     LIST     DISK         Write listing
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------


      NITEM = 0

      CALL SECBGN(KLC)

*     repeat { until terminator }
    1 CONTINUE

         CALL INPDEV (KLC, KLCDR, LFOUND)

         IF (LFOUND) THEN

            NITEM = NITEM + 1

*           Locator Specific Data

*                        QLCINX,   QLCINY
            READ(FWDT,*) REALS(1), REALS(2)
            WRITE(LIST,1000) REALS(1), REALS(2)

*           Add device class specific data to buffer

            NINT = 0
            NREAL = 2

            CALL ITMADD(KLC, NINT, INTS, NREAL, REALS)

         END IF

*     Until termintor
      IF (LFOUND) GO TO 1

*     Flush buffer

      CALL SECEND(KLC)

*  -------
*  FORMATS
*  -------

 1000 FORMAT(T10,'QLCINX...',F10.4,T40,'QLCINY...',F10.4)

      END

*
*
      SUBROUTINE PA(NITEM)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Process Pattern Arrays
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*     09/06/83  CJW   Corrected to read KPADX and KPADY
*
*  ARGUMENTS
*  ---------
*     OUT   NITEM  Number of Bundle read
*
      INTEGER NITEM
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     ?      /WDTWRK/ ?
*
      INCLUDE '../../include/gkwdt.par'
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdterr.par'
      INCLUDE 'wdtwrk.cmn'
*
*  LOCALS
*  ------
*     NINT   Maximum Number of integers in an item (P)
*     NREAL  Number of reals in an item (P)
*     I      Loop index
*     J      Loop index
*     IX     Pattern size X
*     IXIN   Amount of pattern read (<= KNUMS)
*     IY     Pattern size Y
*     ISKIP  Used to skip over part of pattern
*
      INTEGER I, J, IX, IXIN, IY, ISKIP
      INTEGER    NINT,     NREAL
      PARAMETER (NINT = 3, NREAL = 0)
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     FWDT     DISK         Read WDT
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------


      NITEM = 0

      CALL SECBGN(KPPASZ)

      CALL SECBGN(KPPAB)

*     Read Items

*                  KPAI    (= ITEM number)
      READ(FWDT,*) INTS(1)

*     Listing
      IF (INTS(1).GT.0) WRITE(LIST,1000)

*     while item>0 do
    1 CONTINUE
      IF (INTS(1).LT.0) GO TO 3

         NITEM = NITEM + 1

         CALL ITMNXT(KPPASZ)

*                     KPADX,   KPADY,   KPA
         READ(FWDT,*) INTS(2), INTS(3)
         CALL ITMADD(KPPASZ, NINT, INTS, NREAL, REALS)
         CALL ITMNXT(KPPAB)

*        Listing
         WRITE(LIST,1001) (INTS(J), J = 1, 3)

         IX = INTS(2)
         IY = INTS(3)
         IXIN = MIN(KNUMS,IX)

         IF (IX .GT. KNUMS)
     :         CALL ERROR(KERROR,'Pattern too Large to Store')

         DO 2 I = 1, IY

*                         KPA
            READ(FWDT,*) (INTS(J),J = 1, IXIN), (ISKIP,J = IXIN+1, IX)

*           Listing
            WRITE(LIST,1002) I, (INTS(J), J = 1, IXIN)

*           Add to buffer

            CALL ITMADD(KPPAB, IXIN, INTS, NREAL, REALS)
    2    CONTINUE

*                     KPAI   (= ITEM number)
         READ(FWDT,*) INTS(1)

*     end while
      GO TO 1
    3 CONTINUE

*     Flush buffer

      CALL SECEND(KPPAB)
      CALL SECEND(KPPASZ)

*  -------
*  FORMATS
*  -------

 1000 FORMAT(' BUNDLE',T10,'  KPADX',T20,'  KPADY',T30,'  KPA'/)
 1001 FORMAT(1X,I5,T10,I5,T20,I5)
 1002 FORMAT(T20,'(Y = ',I5,')' /
     :      (T30,I5,:', ',I5,:', ',I5,:', ',I5,:', ',I5,:', '))

      END


*
*
      SUBROUTINE PC(NITEM)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Process Pick devices
*
*  MAINTENANCE LOG
*  ---------------
*     09/06/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT   NITEM  Number of devices read
*
      INTEGER NITEM
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     ?      /WDTWRK/ ?
*
      INCLUDE '../../include/gkwdt.par'
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtwrk.cmn'
*
*  LOCALS
*  ------
*     LFOUND = True if input device, = False if terminator
*
      LOGICAL LFOUND
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     FWDT     DISK         Read PC devices
*     LIST     DISK         Write listing
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------


      NITEM = 0

      CALL SECBGN(KPC)

*     repeat { until terminator }
    1 CONTINUE

         CALL INPDEV (KPC, KPCDR, LFOUND)

         IF (LFOUND) THEN

            NITEM = NITEM + 1

*           No Pick Specific Data

         END IF

*     Until termintor
      IF (LFOUND) GO TO 1

*     Flush buffer

      CALL SECEND(KPC)

      END

*
*

      SUBROUTINE DR(ITYPE)

*
*  Copyright (C) SERC 1987
*
*-----------------------------------------------------------------------
*
*  RAL GKS SYSTEM
*
*  Author:             PJWR
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Translates input device data records from sequential access WDT
*     format.
*
*  MAINTENANCE LOG
*  ---------------
*     27/07/87  PJWR  Created.
*     21/12/87  PJWR  Removed inclusion of GKMC.PAR - no longer needed.
*
*  ARGUMENTS
*  ---------
*     INPUT    ITYPE    Key type (for example,  KLCDR).
*
      INTEGER ITYPE
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify   /WDTWRK/  INTS, REALS
*
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtwrk.cmn'
*
*  LOCALS
*  ------
*     IDRREC   Record number of this data record in direct access WDT.
*     NID      Number of integer data in this data record.
*     NRD      Number of real data in this data record.
*     NSD      Number of string data in this data record.
*     ISL      Number of words occupied by a given string in the direct
*              access WDT.
*     ITSL     Total number of words occupied by all strings in the
*              direct access WDT file.
*     ICOUNT   Number of elements of this data type in sequential WDT.
*              A negative value means "End of Data Record".
*     BUFFER   Character array to hold string data from sequential WDT.
*     LENGTH   Non-blank length of string read from the sequential WDT.
*     I        Loop index variable.
*
      INTEGER IDRREC, NID, NRD, NSD, ISL, ITSL, ICOUNT, LENGTH, I
      CHARACTER*80 BUFFER
*
*  STREAMS USED
*  ------------
*     FWDT     DISK     Read data record.
*     LIST     DISK     Write listing.
*
*  COMMENTS
*  --------
*     This routine replaces the routine PID by CJW as PIDs are no longer
*     used in RAL GKS.
*
*     Data records are stored in the sequential access WDT in the this
*     format:
*
*                       +------------------------------------
*                       |
*                       |               +--------------------
*                       |               | Number of integers
*                       | Integers -----+
*                       |               | Integer values
*                       |               +--------------------
*                       |
*                       |               +--------------------
*                       |               | Number of reals
*                       | Reals --------+
*     Data Record ------+               | Real values
*                       |               +--------------------
*                       |
*                       |               +--------------------
*                       |               | Number of strings
*                       | Strings ------+
*                       |               | String values
*                       |               +--------------------
*                       |
*                       | Terminator
*                       |
*                       +------------------------------------
*
*     Any section may have a trailing count,  and any trailing section
*     may be omitted.  The terminator is any negative integer found
*     where a count is expected.  Strings are written one per line and
*     delimited by any character.  The subroutine XTRACT is used to
*     read strings from the sequential WDT and return them as an array
*     of integers with the length of the string returned in the first
*     element of the array.
*
*     The data record is stored in the direct access WDT as a 3 item
*     entry.  Item 1 is integer data,  item 2 is real data and item 3 is
*     string data.  String data is held as ASCII integers.  Each string
*     consists of a length count followed by the string data and all the
*     strings are concatenated into item 3.
*
*     A summary of the data record is returned in INTS,  this being:
*     INTS(1)  Number of integer data
*     INTS(2)  Number of real data
*     INTS(3)  Number of string data
*     INTS(4)  Record number of this entry
*     INTS(5)  Total length of string data
*
*-----------------------------------------------------------------------

*     Initialise summary information and start entry for data record.
      NID = 0
      NRD = 0
      NSD = 0
      CALL ENTBGN(ITYPE, IDRREC)
      ITSL = 0

*     Write the data record announcer to the listing file.
      WRITE(LIST,1000)

*     Obtain integer data count / terminator from seqential WDT.
      READ(FWDT,*) ICOUNT

*     Process integer data - first set up the integer data item.
      CALL ITMNXT(ITYPE)
*     Only write header if we found no integers or the terminator,
*     otherwise write header then obtain the integer data and add it to
*     the item.
      IF (ICOUNT.LE.0) THEN
      WRITE(LIST,1001) 0
      ELSE
      WRITE(LIST,1001) ICOUNT
      NID = ICOUNT
      READ(FWDT,*) (INTS(I), I = 1, NID)
*       Write data to listing file.
      WRITE(LIST,1002) (INTS(I), I = 1, NID)
*       Add data to item.
      CALL ITMADD(ITYPE, NID, INTS, 0, REALS)
      END IF

*     Process real data - first set up the real data item.
      CALL ITMNXT(ITYPE)
*     Check that we haven't found the terminator.  If we have then just
*     write header,  otherwise get the real data count and proceed as
*     for integer data.
      IF (ICOUNT.LT.0) THEN
      WRITE(LIST,1001) 0
      ELSE
      READ(FWDT,*) ICOUNT
      IF (ICOUNT.LE.0) THEN
        WRITE(LIST,1003) 0
      ELSE
        WRITE(LIST,1003) ICOUNT
        NRD = ICOUNT
        READ(FWDT,*) (REALS(I), I = 1, NRD)
*         Write data to listing file.
        WRITE(LIST,1004) (REALS(I), I = 1, NRD)
*         Add data to item.
        CALL ITMADD(ITYPE, 0, INTS, NRD, REALS)
      END IF
      END IF

*     Process string data - first set up the string data item.
      CALL ITMNXT(ITYPE)
*     Check that we haven't found the terminator.  If we have then just
*     write header,  otherwise get the string data count.  If this is
*     zero or the terminator is found,  once agin just write the header.
*     If it is positive,  read in the strings and translate them using
*     XTRACT.
      IF (ICOUNT.LT.0) THEN
      WRITE(LIST,1005) 0
      ELSE
      READ(FWDT,*) ICOUNT
      IF (ICOUNT.LE.0) THEN
        WRITE(LIST,1005) 0
      ELSE
        WRITE(LIST,1005) ICOUNT
        NSD = ICOUNT
        DO 20, I = 1, NSD
          READ(FWDT,1006) BUFFER
*           Report the non-blank part of BUFFER in the listing file.
          LENGTH = LEN(BUFFER) + 1
   10       CONTINUE
            LENGTH = LENGTH - 1
          IF (BUFFER(LENGTH:LENGTH).EQ.' ') GO TO 10
          WRITE(LIST,1007) BUFFER(1:LENGTH)
          CALL XTRACT(BUFFER, KNUMS, INTS)
*           Allow for length count at start of string.
          ISL = INTS(1) + 1
          ITSL = ITSL + ISL
*           Add data to buffer
          CALL ITMADD(ITYPE, ISL, INTS, 0, REALS)
   20     CONTINUE
*         Finding strings means we've got to read the terminator.
        READ(FWDT,*) ICOUNT
      END IF
      END IF

*     Write data record terminator to listing file.
      WRITE(LIST,1008)

*     Close entry and return summary information.
      CALL ENTEND(ITYPE)
      INTS(1) = NID
      INTS(2) = NRD
      INTS(3) = NSD
      INTS(4) = IDRREC
      INTS(5) = ITSL

      RETURN

 1000 FORMAT(/T10,'Data Record'/T10,'-----------'/)
 1001 FORMAT(T20, I5, ' integer(s):')
 1002 FORMAT((T30,6(I5,2X)))
 1003 FORMAT(T20, I5, ' real(s):')
 1004 FORMAT((T30,4(F10.4,2X)))
 1005 FORMAT(T20, I5, ' string(s):')
 1006 FORMAT(A)
 1007 FORMAT(T30,A)
 1008 FORMAT(/T10,'End of Data Record'/T10,'------------------'/)

      END

*
*
      SUBROUTINE PL(NITEM)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Process Polyline bundles
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT   NITEM  Number of Bundles read
*
      INTEGER NITEM
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     ?      /WDTWRK/ ?
*
      INCLUDE '../../include/gkwdt.par'
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtwrk.cmn'
*
*  LOCALS
*  ------
*     NINT   Number of integers in an item
*     NREAL  Number of reals in an item
*
      INTEGER NINT, NREAL
      PARAMETER ( NINT = 3, NREAL = 1)
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     FWDT     DISK         Read PL Bundles
*     LIST     DISK         Write listing
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

      NITEM = 0

      CALL SECBGN(KPPLB)

*     Read Items

*                  KPLI    (= ITEM number)
      READ(FWDT,*) INTS(1)

*     Listing
      IF (INTS(1).GT.0) WRITE(LIST,1000)

*     while item>0 do
    1 CONTINUE
      IF (INTS(1).LT.0) GO TO 2

         NITEM = NITEM + 1

         CALL ITMNXT(KPPLB)

*                     KLNTY,   QLNWD,    KPLCI
         READ(FWDT,*) INTS(2), REALS(1), INTS(3)

*        Listing
         WRITE(LIST,1001) INTS(1), INTS(2), REALS(1), INTS(3)

*        Add to buffer

         CALL ITMADD(KPPLB, NINT, INTS, NREAL, REALS)

*                     KPLI   (= ITEM number)
         READ(FWDT,*) INTS(1)

*     end while
      GO TO 1
    2 CONTINUE

*     Flush buffer

      CALL SECEND(KPPLB)

*  -------
*  FORMATS
*  -------

 1000 FORMAT(' BUNDLE',T10,'   KLNTY',T25,'  QLNWD',T40,'  KPLCI'/)
 1001 FORMAT(1X,I5,T10,I5,T25,F10.4,T40,I5)

      END

*
*
      SUBROUTINE PM(NITEM)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Process Polymarker bundles
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*     25/04/85  RMK   Corrected bug in output to Listing file (S91).
*
*  ARGUMENTS
*  ---------
*     OUT   NITEM  Number of Bundles read
*
      INTEGER NITEM
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     ?      /WDTWRK/ ?
*
      INCLUDE '../../include/gkwdt.par'
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtwrk.cmn'
*
*  LOCALS
*  ------
*     NINT   Number of integers in an item
*     NREAL  Number of reals in an item
*
      INTEGER NINT, NREAL
      PARAMETER ( NINT = 3, NREAL = 1)
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     FWDT     DISK         Read PM Bundles
*     LIST     DISK         Write listing
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

      NITEM = 0

      CALL SECBGN(KPPMB)

*     Read Items

*                  KPMI    (= ITEM number)
      READ(FWDT,*) INTS(1)

*     Listing
      IF (INTS(1).GT.0) WRITE(LIST,1000)

*     while item>0 do
    1 CONTINUE
      IF (INTS(1).LT.0) GO TO 2

         NITEM = NITEM + 1

         CALL ITMNXT(KPPMB)

*                     KMKTY,   QMKSZ,    KPMCI
         READ(FWDT,*) INTS(2), REALS(1), INTS(3)

*        Listing
         WRITE(LIST,1001) INTS(1), INTS(2), REALS(1), INTS(3)

*        Add to buffer

         CALL ITMADD(KPPMB, NINT, INTS, NREAL, REALS)

*                     KPMI   (= ITEM number)
         READ(FWDT,*) INTS(1)

*     end while
      GO TO 1
    2 CONTINUE

*     Flush buffer

      CALL SECEND(KPPMB)

*  -------
*  FORMATS
*  -------

 1000 FORMAT(' BUNDLE',T10,'   KMKTY',T25,'  QMKSZ',T40,'  KPMCI'/)
 1001 FORMAT(1X,I5,T10,I5,T25,F10.4,T40,I5)

      END

*
*
      SUBROUTINE SECTN (NAME)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Produce section heading on listing
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP   NAME   Name of section
*
      CHARACTER * (*) NAME
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*
      INCLUDE 'wdtbuf.par'
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     LIST     DISK         Write message
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

      WRITE(LIST,1000) NAME

*  -------
*  FORMATS
*  -------

 1000 FORMAT(/' Section ',A/' ----------'/)
      END

*
*
      SUBROUTINE SK(NITEM)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Process Stroke devices
*
*  MAINTENANCE LOG
*  ---------------
*     09/06/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT   NITEM  Number of devices read
*
      INTEGER NITEM
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     ?      /WDTWRK/ ?
*
      INCLUDE '../../include/gkwdt.par'
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtwrk.cmn'
*
*  LOCALS
*  ------
*     LFOUND = True if input device, = False if terminator
*     NINT   Number of integers in an item
*     NREAL  Number of reals in an item
*
      LOGICAL LFOUND
      INTEGER NINT, NREAL
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     FWDT     DISK         Read SK devices
*     LIST     DISK         Write listing
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------


      NITEM = 0

      CALL SECBGN(KSK)

*     repeat { until terminator }
    1 CONTINUE

         CALL INPDEV (KSK, KSKDR, LFOUND)

         IF (LFOUND) THEN

            NITEM = NITEM + 1

*           Stroke Specific Data

*                        KSKMXB,  KSKINB
            READ(FWDT,*) INTS(1), INTS(2)
            WRITE(LIST,1000) INTS(1), INTS(2)

*           Add device class specific data to buffer

            NINT = 2
            NREAL = 0

            CALL ITMADD(KSK, NINT, INTS, NREAL, REALS)

         END IF

*     Until terminator
      IF (LFOUND) GO TO 1

*     Flush buffer

      CALL SECEND(KSK)

*  -------
*  FORMATS
*  -------

 1000 FORMAT(T10,'KSKMXB...',I5,T40,'KSKINB...',I5)
      END

*
*
      SUBROUTINE ST(NITEM)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Process String devices
*
*  MAINTENANCE LOG
*  ---------------
*     09/06/83  CJW   Original version stabilized
*     11/11/87  RMK   Change by PJWR July 87 to correct value
*                     of NINT.
*
*  ARGUMENTS
*  ---------
*     OUT   NITEM  Number of devices read
*
      INTEGER NITEM
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     ?      /WDTWRK/ ?
*
      INCLUDE '../../include/gkwdt.par'
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtwrk.cmn'
*
*  LOCALS
*  ------
*     LFOUND = True if input device, = False if terminator
*     NINT   Number of integers in an item
*     NREAL  Number of reals in an item
*
      LOGICAL LFOUND
      INTEGER NINT, NREAL
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     FWDT     DISK         Read ST devices
*     LIST     DISK         Write listing
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------


      NITEM = 0

      CALL SECBGN(KST)

*     repeat { until terminator }
    1 CONTINUE

         CALL INPDEV (KST, KSTDR, LFOUND)

         IF (LFOUND) THEN

            NITEM = NITEM + 1

*           String Specific Data

*                        KSTMXB   KSTINB
            READ(FWDT,*) INTS(1), INTS(2)
            WRITE(LIST,1000) INTS(1), INTS(2)

*           Add device class specific data to buffer

            NINT = 2
            NREAL = 0

            CALL ITMADD(KST, NINT, INTS, NREAL, REALS)


         END IF

*     Until terminator
      IF (LFOUND) GO TO 1

*     Flush buffer

      CALL SECEND(KST)

*  -------
*  FORMATS
*  -------

 1000 FORMAT(T10,'KSTMXB...',I5,T40,'KSTINB...',I5)
      END

*
*
      CHARACTER * 10 FUNCTION TRUP(AMESS)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Translate to upper case
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*     14/11/84  SHS/CJC    Modified to avoid  using ICHAR for checking
*                          characters to be in range 97-122 Ascii
*     06/05/86  RMK   Changed use of CHAR/ICHAR to GKAN1/GKNA1 (S103).
*     19/11/87  RMK   Changed assignment of return value to avoid
*                     compiler bug on Pyramid (S272).
*
*  ARGUMENTS
*  ---------
*     INP   AMESS  Lower case string
*
      CHARACTER * (*) AMESS
*
*  FUNCTION RETURN VALUE
*  ---------------------
*   Argument translated to upper case
*
*  EXTERNAL FUNCTION DEFINITIONS
*  -----------------------------
*
      CHARACTER*1 GKAN1
      INTEGER GKNA1
*
*  LOCALS
*  ------
*   J      Loop index
*   TMESS  Intermediate Upper case string
*
      INTEGER  J
      CHARACTER * 10 TMESS
*
*---------------------------------------------------------------------

      TMESS = '  '
      DO 1 J = 1, MIN(10,LEN(AMESS))
        IF (AMESS(J:J).GE.'a'.AND.AMESS(J:J).LE.'z') THEN
            TMESS(J:J)=GKAN1(GKNA1('A')+GKNA1(AMESS(J:J))-GKNA1('a'))
         ELSE
            TMESS(J:J) = AMESS(J:J)
         END IF

    1 CONTINUE

      TRUP = TMESS(1:10)

      END
      SUBROUTINE TX(NITEM)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Process Text bundles
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT   NITEM  Number of Bundles read
*
      INTEGER NITEM
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     ?      /WDTWRK/ ?
*
      INCLUDE '../../include/gkwdt.par'
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtwrk.cmn'
*
*  LOCALS
*  ------
*     NINT   Number of integers in an item
*     NREAL  Number of reals in an item
*
      INTEGER NINT, NREAL
      PARAMETER ( NINT = 4, NREAL = 2)
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     FWDT     DISK         Read TX Bundles
*     LIST     DISK         Write listing
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

      NITEM = 0

      CALL SECBGN(KPTXB)

*     Read Items

*                  KTXI    (= ITEM number)
      READ(FWDT,*) INTS(1)

*     Listing
      IF (INTS(1).GT.0) WRITE(LIST,1000)

*     while item>0 do
    1 CONTINUE
      IF (INTS(1).LT.0) GO TO 2

         NITEM = NITEM + 1

         CALL ITMNXT(KPTXB)

*                     KTXFN,   KTXPN,   QCHXP,    QCHSP,    KTXCI
         READ(FWDT,*) INTS(2), INTS(3), REALS(1), REALS(2), INTS(4)

*        Listing
         WRITE(LIST,1001) INTS(1), INTS(2), INTS(3), REALS(1),
     :                    REALS(2), INTS(4)

*        Add to buffer

         CALL ITMADD(KPTXB, NINT, INTS, NREAL, REALS)

*                     KTXI   (= ITEM number)
         READ(FWDT,*) INTS(1)

*     end while
      GO TO 1
    2 CONTINUE

*     Flush buffer

      CALL SECEND(KPTXB)

*  -------
*  FORMATS
*  -------

 1000 FORMAT(' BUNDLE',T10,'   KTXFN',T25,'   KTXPN',
     :       T40,'  QCHXP',T55,'  QCHSP',T70,'  KTXCI'/)
 1001 FORMAT(1X,I5,T10,I5,T25,I5,T40,F10.4,T55,F10.4,T70,I5)

      END

*
*
      SUBROUTINE VL(NITEM)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Process Valuator devices
*
*  MAINTENANCE LOG
*  ---------------
*     09/06/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT   NITEM  Number of devices read
*
      INTEGER NITEM
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     ?      /WDTWRK/ ?
*
      INCLUDE '../../include/gkwdt.par'
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtwrk.cmn'
*
*  LOCALS
*  ------
*     LFOUND = True if input device, = False if terminator
*     NINT   Number of integers in an item
*     NREAL  Number of reals in an item
*
      LOGICAL LFOUND
      INTEGER NINT, NREAL
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     FWDT     DISK         Read VL devices
*     LIST     DISK         Write listing
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------


      NITEM = 0

      CALL SECBGN(KVL)

*     repeat { until terminator }
    1 CONTINUE

         CALL INPDEV (KVL, KVLDR, LFOUND)

         IF (LFOUND) THEN

            NITEM = NITEM + 1

*           Valuator Specific Data

*                        QINVL,    QMNVL,    QMXVL
            READ(FWDT,*) REALS(1), REALS(2), REALS(3)
            WRITE(LIST,1000) REALS(1), REALS(2), REALS(3)

*           Add device class specific data to buffer

            NINT = 0
            NREAL = 3

            CALL ITMADD(KVL, NINT, INTS, NREAL, REALS)

         END IF

*     Until termintor
      IF (LFOUND) GO TO 1

*     Flush buffer

      CALL SECEND(KVL)

*  -------
*  FORMATS
*  -------

 1000 FORMAT(T10,'QINVL....',F10.4,T40,'QMNVL....',F10.4/
     :       T10,'QMXVL....',F10.4)

      END

*
*
      SUBROUTINE WDT(NITEM)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Process general WDT information
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*     09/06/83  CJW   Now reads KCHXPF, QMNCHX and QMXCHX
*     14/07/83  CJW   Add maximum number of Patterns
*     07/02/90  KEVP  Removed assignment to unused KWDTRC
*
*  ARGUMENTS
*  ---------
*     OUT   NITEM  Number of items read (always one)
*
      INTEGER NITEM
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     ?      /WDTWRK/ ?
      INCLUDE '../../include/gkwdt.par'
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtbuf.cmn'
      INCLUDE 'wdtwrk.cmn'
*
*  LOCALS
*  ------
*     NINT   Number of integers in an item
*     NREAL  Number of reals in an item
*
      INTEGER     NINT,          NREAL
      PARAMETER ( NINT = KSZOFF, NREAL = 12)
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     FWDT     DISK         WDT read
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

      NITEM = 0

      CALL SECBGN(KWDT)

*     Read Item - One only for WDT

      NITEM = NITEM + 1

         CALL ITMNXT(KWDT)

*                     QDSDX,   QDSDY,   KDSRX,  KDSRY
         READ(FWDT,*) REALS(1),REALS(2),INTS(1),INTS(2)

*        Listing
         WRITE(LIST,1000) REALS(1),REALS(2),INTS(1),INTS(2)

*                     KLNWD,  QNMLNW,  QMNLNW,  QMXLNW

         READ(FWDT,*) INTS(3),REALS(3),REALS(4),REALS(5)
*        Listing
         WRITE(LIST,1001) INTS(3),REALS(3),REALS(4),REALS(5)

*                     KMKSZ,  QNMMKS,  QMNMKS,  QMXMKS

         READ(FWDT,*) INTS(4),REALS(6),REALS(7),REALS(8)
*        Listing
         WRITE(LIST,1002) INTS(4),REALS(6),REALS(7),REALS(8)

*                     KCHH,   QMNCHH,  QMXCHH

         READ(FWDT,*) INTS(5),REALS(9),REALS(10)
*        Listing
         WRITE(LIST,1003) INTS(5),REALS(9),REALS(10)

*                     KCHXPF,   QMNCHX,  QMXCHX

         READ(FWDT,*) INTS(6),REALS(11),REALS(12)

*        Listing
         WRITE(LIST,1004) INTS(6),REALS(11),REALS(12)

*                     KMXPAB

         READ(FWDT,*) INTS(7)

*        Listing
         WRITE(LIST,1005) INTS(7)

*        Add to buffer

         CALL ITMADD(KWDT, NINT, INTS, NREAL, REALS)


*  -------
*  FORMATS
*  -------

 1000 FORMAT(1X,'QDSDX . . ',F10.4,T25,'QDSDY . . ',F10.4/
     :       1X,'KDSRX . . ',I5  ,T25,'KDSRY . . ',I5)
 1001 FORMAT(1X,'KLNWD . . ',I5  ,T25,'QNMLNW. . ',F10.4/
     :       1X,'QMNLNW. . ',F10.4,T25,'QMXLNW. . ',F10.4)
 1002 FORMAT(1X,'KMKSZ . . ',I5  ,T25,'QNMMKS. . ',F10.4/
     :       1X,'QMNMKS. . ',F10.4,T25,'QMXMKS. . ',F10.4)
 1003 FORMAT(1X,'KCHH. . . ',I5  ,T25,'QMNCHH. . ',F10.4/
     :       1X,'QMXCHH. . ',F10.4)
 1004 FORMAT(1X,'KCHXPF. . ',I5  ,T25,'QMNCHX. . ',F10.4/
     :       1X,'QMXCHX. . ',F10.4)
 1005 FORMAT(1X,'KMXPAB. . ',I5)

      END

*
*
      SUBROUTINE WDTEND(NINT, NMBR)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Update the directory part of the WDT
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP   NINT   Number of integers in an item
*     INP   NMBR   Number of items for each section of WDT
      INTEGER NINT, NMBR(NINT)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /WDTBUF/
*
      INCLUDE '../../include/gkwdt.par'
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtwrk.cmn'
*
*  LOCALS
*  ------
*     NREAL  Number of reals in an item
*
      INTEGER     NREAL
      PARAMETER ( NREAL = 0)
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     DWDT     DISK         Updated
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------


*     Update the WDT

      CALL ITMADD(KWDT, NINT, NMBR, NREAL, REALS)

*     Put the WDT back

      CALL SECEND(KWDT)

      END

*
*
      SUBROUTINE WKDEND

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    xxx
*  GKS Function name:  xxx
*  Author:             xxx
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  WDT file is initialised.
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  CJW   Original version stabilized
*
*  COMMON BLOCK USAGE
*  ------------------
*     ?      /WDTBUF/  ?
*     ?      /WDTWRK/ ?
*
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE '../../include/gkwdt.par'
      INCLUDE 'wdtwrk.cmn'
      INCLUDE 'wdtwkd.cmn'
*
*  LOCALS
*  ------
*     NINT   Number of integers in an item
*     NREAL  Number of reals in an item
*
      INTEGER NINT, NREAL
      PARAMETER ( NINT = KMXDIR, NREAL = 0)
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     DWDT     disk         Update
*     LIST     disk         produce listing
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

*     Update the Workstation directory

      CALL ITMADD(KWKDIR, NINT, KWKITM, NREAL, REALS)

*     Save Workstation directory

      CALL SECEND(KWKDIR)

      END

*
*
      SUBROUTINE XTRACT(BUFFER, ILEN, INT)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Extract text from quoted string, convert to ASCII
*
*  MAINTENANCE LOG
*  ---------------
*     17/07/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP   BUFFER Quoted String
*     INP   ILEN   Size of array INT
*     OUT   INT    Array to hold ASCII - Length stored in INT(1)
*
      INTEGER ILEN, INT(*)
      CHARACTER * (*) BUFFER
*
*  LOCALS
*  ------
*     IEND   Position of second quote
*     QUOTE  The quote character found
*
      INTEGER IEND
      CHARACTER QUOTE
*
*  COMMENTS
*  --------
*     Length of string held in INT(1)
*
*---------------------------------------------------------------------

      QUOTE = BUFFER(1:1)
      IEND = INDEX(BUFFER(2:), QUOTE) + 1
      IF (IEND .EQ. 0) IEND = LEN(BUFFER) + 1
      IF (IEND .GT. 2)
     :        CALL GKNTOA(IEND-2, BUFFER(2:IEND-1), INT(2))
      INT(1) = IEND - 2

      END
*
*
      SUBROUTINE BFFIND(ITYPE, IBUF)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    ???
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Find buffer of name "ITYPE"
*
*  MAINTENANCE LOG
*  ---------------
*     22/06/83  CJW   Original version stabilized
*     11/11/87  RMK   Cray compiler doesn't allow a character to
*                     be assigned a substring of itself. So added a
*                     temporary string in code which strips leading
*                     blanks (S222).
*
*  ARGUMENTS
*  ---------
*     INP ITYPE  Name of section
*     OUT IBUF   Buffer number
*
      INTEGER ITYPE, IBUF
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /WDTBUF/  Allocate buffer
*
      INCLUDE 'wdterr.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtbuf.cmn'
*
*  LOCALS
*  ------
*     I       Loop index
*     NAME    Used to convert ITYPE into a string
*     TNAME   Temporary string used when removing leading blanks
*
      CHARACTER * 5 NAME
      CHARACTER * 4 TNAME
      INTEGER I
*
*
*---------------------------------------------------------------------

*     Search for name

      DO 1 I = 1, KNBF
         IF (KNAMES(I) .EQ. ITYPE) THEN
            IBUF = I
            RETURN
         END IF
    1 CONTINUE

*     Error - no such buffer

      WRITE(NAME,'(I5)') ITYPE

*     Remove leading blanks

      DO 2 I = 1, 4
         IF (NAME(1:1) .NE. ' ') GO TO 3
         TNAME = NAME(2:5)
         NAME = TNAME
    2 CONTINUE

    3 CONTINUE

      CALL ERROR(KERROR, 'Internal Error - Cannot find Buffer '//NAME)

      END

*
*
      SUBROUTINE BFFIX(IBUF)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    ???
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Move item KITEM(IBUF)+1 into item 1
*
*  MAINTENANCE LOG
*  ---------------
*     20/07/83  CJW   Original version stabilized
*     07/02/90  KEVP  Added commenting.
*
*  ARGUMENTS
*  ---------
*     INP IBUF   Buffer number
*
      INTEGER IBUF
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /WDTBUF/  Allocate buffer
*
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtbuf.cmn'
*
*  LOCALS
*  ------
*     ITEM   Current number of items - 1
*     ISTART Position of start of last item
*     IEND   Position of end of last item
*     ISIZE  Length (Integers or reals) of last item
*     I      Loop index
*     J      Array index
*
      INTEGER ITEM, ISTART, IEND, ISIZE, I, J
*
*---------------------------------------------------------------------


      ITEM = KITEM(IBUF)
      KSIZE(IBUF) = KSCALR * KSZWD

*     Integers

      ISTART = KINT(ITEM,IBUF) + 1
      IEND = KINT(ITEM+1,IBUF)
      ISIZE = IEND + 1 - ISTART
      KSIZE(IBUF) = KSIZE(IBUF) + (ISIZE+2) * KSZWD
      J = 0
      DO 1 I = ISTART, IEND
      J = J + 1
      KDATA(J,IBUF) = KDATA(I,IBUF)
    1 CONTINUE
      KIOFF(IBUF) = ISIZE
      KINT(1,IBUF) = KINT(0,IBUF) + ISIZE

*     Reals

      ISTART = KREAL(ITEM,IBUF) + 1
      IEND = KREAL(ITEM+1,IBUF)
      ISIZE = IEND + 1 - ISTART
      KSIZE(IBUF) = KSIZE(IBUF) + ISIZE * KSZWD
      J = 0
      DO 2 I = ISTART, IEND
      J = J + 1
      QDATA(J,IBUF) = QDATA(I,IBUF)
    2 CONTINUE
      KROFF(IBUF) = ISIZE
      KREAL(1,IBUF) = KREAL(0,IBUF) + ISIZE

*     One item not overflowing into another record
      KITEM(IBUF) = 1
      KNEXT(IBUF) = KUNSET
      KMORE(IBUF) = KNO
*
*     Allocate record
      KCREC(IBUF) = KNXREC
      KNXREC = KNXREC + 1

      END

*
*
      SUBROUTINE BFNXT(IBUF)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    ???
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Page out the current buffer and start a fresh one
*
*  MAINTENANCE LOG
*  ---------------
*     22/06/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT IBUF   Buffer number
*
      INTEGER IBUF
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /WDTBUF/  Allocate buffer
*
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtbuf.cmn'
*
*  COMMENTS
*  --------
*
*     This is used to EXTEND an item beyond the end of a record. If
*     the buffer contains only one item the item is split ( KMORE flag
*     is set to KYES). Otherwise the prior items are output and the last
*     item shuffled to the front.
*
*---------------------------------------------------------------------

      KNEXT(IBUF) = KNXREC
      IF (KITEM(IBUF) .EQ. 1) THEN
         KMORE(IBUF) = KYES
         CALL BFPUT(KCREC(IBUF), IBUF)
         KNEXT(IBUF) = KUNSET
         KMORE(IBUF) = KNO
         KCREC(IBUF) = KNXREC
         KNXREC = KNXREC + 1
         KIOFF(IBUF) = 0
         KROFF(IBUF) = 0
         KSIZE(IBUF) = (KSCALR+2) * KSZWD
         KINT(1,IBUF) = KINT(0,IBUF)
         KREAL(1,IBUF) = KREAL(0,IBUF)
      ELSE
         KITEM(IBUF) = KITEM(IBUF) - 1
         CALL BFPUT(KCREC(IBUF), IBUF)
         CALL BFFIX(IBUF)
      END IF

      END

*
*
      SUBROUTINE BFPUT(IREC, IBUF)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    ???
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Output a buffer to the DWDT
*
*  MAINTENANCE LOG
*  ---------------
*     22/06/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IREC   Record number
*     OUT IBUF   Buffer number
*
      INTEGER IREC, IBUF
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /WDTBUF/  Allocate buffer
*
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtbuf.cmn'
*
*  LOCALS
*  ------
*     I       Loop index
*     J       Loop index
*     IERR    Error return code
*
      INTEGER I, J, IERR
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     DWDT     disk         output
*
*---------------------------------------------------------------------


      WRITE(DWDT, REC=IREC, IOSTAT=IERR)
     :   KITEM(IBUF), KNEXT(IBUF), KMORE(IBUF),
     :  (KINT(J,IBUF)-KINT(J-1,IBUF),KREAL(J,IBUF)-KREAL(J-1,IBUF),
     :  (KDATA(I,IBUF), I = KINT(J-1,IBUF)+1, KINT(J,IBUF)),
     :  (QDATA(I,IBUF), I = KREAL(J-1,IBUF)+1, KREAL(J,IBUF)),
     :            J = 1, KITEM(IBUF))

      END

*
*
      SUBROUTINE BFSET

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    ???
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Initialize Buffered output to Direct access WDT
*
*  MAINTENANCE LOG
*  ---------------
*     22/06/83  CJW   Original version stabilized
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /WDTBUF/  Preset
*
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtbuf.cmn'
*
*  LOCALS
*  ------
*     I      Loop index
*
      INTEGER I
*
*---------------------------------------------------------------------


      DO 1 I = 1, KNBF
         KINT(0,I)  = 0
         KREAL(0,I) = 0
         KNAMES(I) = KUNSET
    1 CONTINUE
      KNXREC = 1

      END

*
*
      SUBROUTINE SECBGN(ITYPE)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    ???
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Begin a section
*
*  MAINTENANCE LOG
*  ---------------
*     22/06/83  CJW   Original version stabilized
*     17/07/83  CJW   Built on top of ENTBGN
*
*  ARGUMENTS
*  ---------
*     INP ITYPE  Name of section
*
      INTEGER ITYPE
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify  /WDTWKD/  Store Workstation directory
*
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtwkd.cmn'
*
*  LOCALS
*  ------
*     IREC    Record number
*
      INTEGER IREC
*
*
*---------------------------------------------------------------------

*     Get a new buffer

      CALL ENTBGN(ITYPE, IREC)

*     Modify the Workstation Directory - if its a "normal" entry

      IF (ITYPE .GT. 0) KWKITM(ITYPE) = IREC

      END

*
*
      SUBROUTINE ENTBGN(ITYPE, IREC)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    ???
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Begin an entry
*
*  MAINTENANCE LOG
*  ---------------
*     18/07/83  CJW   Original version stabilized
*     07/02/90  KEVP  Tidied up record allocation (S322).
*
*  ARGUMENTS
*  ---------
*     INP ITYPE  Name of section
*     OUT IREC   Record Number
*
      INTEGER ITYPE, IREC
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read /WDTBUF/  Allocate buffer
*
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtbuf.cmn'
*
*  LOCALS
*  ------
*     IBUF    Buffer number
*
      INTEGER IBUF
*
*
*---------------------------------------------------------------------

*     Allocate Record

      IREC = KNXREC
      KNXREC = KNXREC + 1

*     Get a new buffer

      CALL BFFIND(KUNSET, IBUF)
      KNAMES(IBUF) = ITYPE
      KITEM(IBUF) = 0
      KNEXT(IBUF) = KUNSET
      KMORE(IBUF) = KNO
      KCREC(IBUF) = IREC
      KIOFF(IBUF) = 0
      KROFF(IBUF) = 0
      KSIZE(IBUF) = KSCALR * KSZWD


      END

*
*
      SUBROUTINE ITMADD(ITYPE, NINT, INTS, NREAL, REALS)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    WDT Generator
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Add data to an item
*
*  MAINTENANCE LOG
*  ---------------
*     22/06/83  CJW   Original version stabilized
*     07/02/90  KEVP  Inserted warning into comments
*
*  ARGUMENTS
*  ---------
*     INP ITYPE  Name of section
*     INP NINT   Amount of integer data to add to item
*     INP INTS   Integer data
*     INP NREAL  Amount of real data to add to item
*     INP REALS  Real data
*
      INTEGER ITYPE, NINT, INTS(*), NREAL
      REAL REALS(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /WDTBUF/  Allocate buffer
*
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtbuf.cmn'
*
*  LOCALS
*  ------
*     I       Loop index
*     IBUF    Buffer number
*
      INTEGER I, IBUF

*  WARNING
*  -------
*    This routine may allocate a new record.
*    Therefore, it should not be called if the current value of
*    KNXREC has been assigned to anything that matters.
*
*---------------------------------------------------------------------

      CALL BFFIND(ITYPE, IBUF)

*     Copy Integers

      DO 1 I = 1, NINT
         IF (KSIZE(IBUF) .GE. KMXQBU) CALL BFNXT(IBUF)
         KSIZE(IBUF) = KSIZE(IBUF) + KSZWD
         KIOFF(IBUF) = KIOFF(IBUF) + 1
         KINT(KITEM(IBUF),IBUF) = KINT(KITEM(IBUF),IBUF) + 1
         KDATA(KIOFF(IBUF),IBUF) = INTS(I)
    1 CONTINUE

*     Copy Reals

      DO 2 I = 1, NREAL
         IF (KSIZE(IBUF) .GE. KMXQBU) CALL BFNXT(IBUF)
         KSIZE(IBUF) = KSIZE(IBUF) + KSZWD
         KROFF(IBUF) = KROFF(IBUF) + 1
         KREAL(KITEM(IBUF),IBUF) = KREAL(KITEM(IBUF),IBUF) + 1
         QDATA(KROFF(IBUF),IBUF) = REALS(I)
    2 CONTINUE

      END

*
*
      SUBROUTINE ITMNXT(ITYPE)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    ???
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Begin an item
*
*  MAINTENANCE LOG
*  ---------------
*     22/06/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP ITYPE  Name of section
*
      INTEGER ITYPE
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /WDTBUF/  Allocate buffer
*
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtbuf.cmn'
*
*  LOCALS
*  ------
*     IBUF    Buffer number
*
      INTEGER IBUF
*
*
*---------------------------------------------------------------------

      CALL BFFIND(ITYPE, IBUF)

*     Counts (Number integers, number reals) will go into buffer
*     so must add up room they will take
      KSIZE(IBUF) = KSIZE(IBUF) + (2 * KSZWD)

      KITEM(IBUF) = KITEM(IBUF) + 1
      KINT (KITEM(IBUF), IBUF) = KINT(KITEM(IBUF)-1, IBUF)
      KREAL(KITEM(IBUF), IBUF) = KREAL(KITEM(IBUF)-1, IBUF)

      END

*
*
      SUBROUTINE ENTEND(ITYPE)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    ???
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     End an entry
*
*  MAINTENANCE LOG
*  ---------------
*     18/07/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP ITYPE  Name of section
*
      INTEGER ITYPE
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /WDTBUF/  Allocate buffer
*
      INCLUDE 'wdtenm.par'
      INCLUDE 'wdtbuf.par'
      INCLUDE 'wdtbuf.cmn'
*
*  LOCALS
*  ------
*     IBUF    Buffer number
*
      INTEGER IBUF
*
*
*---------------------------------------------------------------------

      CALL BFFIND(ITYPE, IBUF)
      CALL BFPUT(KCREC(IBUF),IBUF)
      KNAMES(IBUF) = KUNSET

      END

*
*
      SUBROUTINE SECEND(ITYPE)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    ???
*  Author:             CJW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     End a section
*
*  MAINTENANCE LOG
*  ---------------
*     22/06/83  CJW   Original version stabilized
*     17/06/83  CJW   Built on top of ENTEND
*
*  ARGUMENTS
*  ---------
*     INP ITYPE  Name of section
*
      INTEGER ITYPE
*
*
*---------------------------------------------------------------------

      CALL ENTEND(ITYPE)
      END

