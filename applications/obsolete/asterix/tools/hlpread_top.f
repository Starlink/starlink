*+  HLPREAD_TOP - ASTERIX help system 
      SUBROUTINE HLPREAD_TOP( HELPFILE, KEY, FNAME, SPOOL, SCREEN )
*
*    Description :
*
*     Invokes Starlink Help System
*
*    Method :
*    Authors :
*
*     BHVAD::RJV
*     David J. Allan (JET-X, University of Birmingham)
*     Richard Beard (ROSAT, University of Birmingham)
*
*    History :
*
*      7 Nov 91 : V1.5-0 Original (RJV)
*      6 Mar 92 : V1.5-1 NOSCREEN option added (RJV)
*      9 Jul 92 : V1.6-0 Uses C main program. Help and index filenames
*                        passed from main() (DJA)
*     13 Jul 92 : V1.6-1 Replaced '\' with BSLASH to allow UNIX compile (DJA)
*     21 Aug 92 : V1.6-2 Updated for changed routines in HLP_ V2.1 (DJA)
*     28 Jan 93 : V1.6-3 Added STR_ABBREV to save linking with ASTLIB. Makes
*                        live much easier with UNIX re-build (DJA)
*     29 Jul 93 : V1.6-4 More i/o to FIO, and use ERR_ properly. Portable
*                        spooler added. SPOOL, NOSCREEN and FILE control 
*                        removed to top level (DJA)
*     14 Nov 94 : V1.8-0 Use stack for files, support embedded HTML and
*                        file jumping. Spool file named using library root
*                        name (DJA)
*     30 Jan 98 : V2.2-0 Fix problem causing some xterms to keep on
*                        underlining (RB)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      INTEGER HLPREAD_IN,HLPREAD_OUT
      EXTERNAL HLPREAD_IN,HLPREAD_OUT,HLPREAD_NAMETR
      INTEGER HLP_HELP
      INTEGER CHR_LEN
      LOGICAL CHR_ABBRV
*
*    Global variables :
*
      INCLUDE 'HLPREAD_CMN'
*
*    Import :
*
      CHARACTER*(*)           	HELPFILE          	! Help root name
      CHARACTER*(*)           	KEY               	! User supplied help keys
      CHARACTER*(*)           	FNAME             	! User supplied file name
      LOGICAL                 	SPOOL             	! Spool the output?
      LOGICAL                 	SCREEN            	! Do console output?
*
*    Local constants :
*
      INTEGER                 	LOUT              	! Width of output
        PARAMETER             	( LOUT = 80 )
*
*    Local variables :
*
      CHARACTER*80            	SCRATCH_DIR       	! Scratch directory name
      CHARACTER*200             PATH

      INTEGER			ILINK			! Loop over links
      INTEGER                 	JSTAT			! HLP status code
      INTEGER                 	LH			! Length of HELPFILE

      LOGICAL                 	FOUND             	! Matched an index entry?
*-

*    Start error reporting
      CALL ERR_BEGIN( STATUS )

*    Initialise stacks
      HSP = 0
      NFILE = 0

*    Length of HELPFILE
      LH = CHR_LEN(HELPFILE)

      SC_NLINE = 0
      A_LEN = 0
      A_EXIT = .FALSE.
      A_HYPER = .FALSE.
      A_TOP = .FALSE.
      A_FILE = .FALSE.
      A_PREV = .FALSE.
      A_SCREEN = SCREEN

*    Get screen dimensions - note that a depth of zero implies a file device
      CALL AIO_SCRNSZ( SC_WID, SC_DEP, STATUS )
      IF ( SC_DEP .EQ. 0 ) SC_DEP = 24

*    Spool output?
      IF ( SPOOL ) THEN
        A_FILE = .TRUE.
        CALL PSX_GETENV( 'AST_SCRATCH', SCRATCH_DIR, STATUS )
        FNAME = SCRATCH_DIR(:CHR_LEN(SCRATCH_DIR))/
     :                      /HELPFILE(:LH)//'.lis'

*    User supplied a filename
      ELSE IF ( FNAME(1:1) .GT. ' ' ) THEN
        A_FILE = .TRUE.

*    Not a terminal
      ELSE IF ( SC_DEP .EQ. 0 ) THEN
        A_FILE = .TRUE.
 
      END IF

*    Open file?
      IF ( A_FILE ) THEN
        CALL FIO_OPEN( FNAME, 'WRITE', 'LIST', 0, A_FD, STATUS )
      END IF

*    Remove any leading spaces
      CALL CHR_LDBLK( KEY )

*    Push the user supplied file arguments onto the file stack
      PATH= '/'//HELPFILE(1:LH)//'/'//KEY
      CALL HLPREAD_SETPATH(PATH, STATUS )

*    While more help to output
      JSTAT = 1
      DO WHILE ( (JSTAT.EQ.1) .AND. .NOT. A_EXIT )

*      Clear input buffer
        IBUFP = 0

*      Write help from current library and current path
        JSTAT = HLP_HELP( HLPREAD_OUT, LOUT, HST_PATH(HSP),
     :                    HFI_HLN(NFILE), 
     :                    HFI_ROOT(NFILE)(:HFI_LROOT(NFILE)),
     :                    1, HLPREAD_IN, HLPREAD_NAMETR )

*      Hypertext has been invoked
        IF ( A_HYPER ) THEN

          JSTAT = 1

*        See if valid hypertext tag
          FOUND = .FALSE.
          ILINK = HST_FLINK(HSP)
          DO WHILE ( (ILINK .LE. (HST_FLINK(HSP) + HST_NLINK(HSP)-1)) 
     :               .AND. .NOT. FOUND )
            IF ( CHR_ABBRV( A_TEXT(:A_LEN), HLI_TXT(ILINK), 
     :                                               A_LEN) ) THEN
              FOUND = .TRUE.
            ELSE
              ILINK = ILINK + 1
            END IF
          END DO

*        Found link in table?
          IF ( FOUND ) THEN

*          Update the help stack
            CALL HLPREAD_SETPATH( HLI_PATH(ILINK), STATUS )

          ELSE
            WRITE(*,'(/1X,A,A/)') 'Sorry, no documentation on ',
     :                                           KEY(1:A_LEN)
            HST_PATH(HSP) = ' '

          ENDIF
          A_HYPER = .FALSE.

*      Go back to previous display
        ELSE IF ( A_PREV ) THEN

          JSTAT=1
          CALL HLPREAD_POPPATH( STATUS )
          A_PREV = .FALSE.

*  display top level
        ELSE IF ( A_TOP ) THEN

          JSTAT = 1
          HST_PATH(HSP) = ' '
          A_TOP = .FALSE.

        END IF

*    Help display loop
      END DO

*    Pop paths off stack
      DO WHILE ( HSP .GT. 0 )
        CALL HLPREAD_POPPATH( STATUS )
      END DO

*    Close secondary output and spool if required
      IF ( A_FILE ) THEN
        CALL FIO_CLOSE( A_FD, STATUS )
        IF ( SPOOL ) THEN
          IF ( STATUS .EQ. SAI__OK ) THEN
            CALL UTIL_SPOOL( FNAME, 'LIST', .TRUE., STATUS )
            IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
          END IF
        END IF
      ENDIF

      END

*+
      SUBROUTINE HLPREAD_INDEX( FD,KEY,PATH,FOUND,STATUS)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'FIO_ERR'
*    Import :
      INTEGER FD
      CHARACTER*(*) KEY
*    Export :
      CHARACTER*(*) PATH
      LOGICAL FOUND
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
      LOGICAL CHR_ABBRV
*    Local constants :
*    Local variables :
c      CHARACTER*10 SHORTKEY
      CHARACTER*80 REC
      INTEGER LP,LK
c      INTEGER IK,LSK
*-

*    No index at this level?
      IF ( FD .EQ. 0 ) THEN
        FOUND = .FALSE.

      ELSE IF ( STATUS .EQ. SAI__OK ) THEN

        FOUND=.FALSE.
        LK=CHR_LEN(KEY)

*  if keyword contains _ then reduce down to minimal abbreviation
c        IF (INDEX(KEY(2:),'_').GT.0) THEN
c          LSK=1
c          SHORTKEY=KEY(1:1)
c          DO IK=2,LK
c            IF (KEY(IK:IK).EQ.'_') THEN
c              SHORTKEY(LSK+1:)=KEY(IK:IK+1)
c              LSK=LSK+2
c            ENDIF
c          ENDDO
c          KEY=SHORTKEY
c        ENDIF

        DO WHILE ( (STATUS.EQ.SAI__OK) .AND. .NOT. FOUND )

          CALL FIO_READF( FD, REC, STATUS )
          IF ( STATUS .EQ. SAI__OK ) THEN
            LP=INDEX(REC,' ')
            IF (CHR_ABBRV(KEY(:LK),REC(:LP),LK)) THEN
              FOUND=.TRUE.
              PATH=REC(LP:)
              CALL CHR_LDBLK( PATH )
            END IF
          END IF

        ENDDO
        IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )

        IF ( .NOT. FOUND ) PATH = KEY
 
        CALL FIO_RWIND( FD, STATUS )

      ENDIF

      END


*+
      INTEGER FUNCTION HLPREAD_IN(STRING,PROMPT,L)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
*    Import :
      CHARACTER*(*) PROMPT
*    Export :
      CHARACTER*(*) STRING
      INTEGER L
*    Functions :
      INTEGER CHR_LEN
*    Global variables :
      INCLUDE 'HLPREAD_CMN'
*    Local constants :
      CHARACTER*1 BSLASH
*    Local variables :
      CHARACTER*80 NAME
      CHARACTER*10 CHEV
      CHARACTER*200 PATH
      INTEGER ISTAT
      INTEGER ILEV,NLEV
      INTEGER T,S,N,I,LP,STATUS,IP
*-

      BSLASH=CHAR(92)

      IF ( A_IGNORE ) THEN
        A_IGNORE = .FALSE.
        HLPREAD_IN = -1
        RETURN
      END IF

      STATUS = 0

      IF ( A_SCREEN ) THEN

*      Determine tree level and name from prompt string
        CALL HLPREAD_LEVEL(PROMPT,NLEV)
        T=INDEX(PROMPT,'Topic?')
        IF (T.GT.1) THEN
          NAME=PROMPT(:T-1)
        ELSE IF (T.EQ.1) THEN
          NAME=' '
        ELSE
          S=INDEX(PROMPT,'Subtopic?')
          IF (S.GT.1) THEN
            NAME=PROMPT(:S-1)
          ELSE
            NAME=' '
          END IF
        END IF

*      Remove dots from name
        N = CHR_LEN(NAME)
        DO I = 1, N
          IF ( NAME(I:I) .EQ. '.' ) THEN
            NAME(I:I) = ' '
          END IF
        END DO
        
*      Construct new prompt
        CHEV = ' '
        DO ILEV = 1, NLEV+1
          CHEV(ILEV:ILEV) = '-'
        END DO
        CHEV(NLEV+2:NLEV+2) = '>'

*      Get user response      
        WRITE(*,'(A,$)') CHEV(:NLEV+3)
 10     READ(*,'(A)',IOSTAT=ISTAT) STRING
        IF ( STRING(1:2).EQ.'@@') THEN
          CALL HLPREAD_DIAG()
          GOTO 10
        END IF

*    Not screen mode
      ELSE
        STRING = ' '

      END IF

*  reset line counter
      SC_NLINE=0
*  Ctrl-Z to exit
      IF (ISTAT.EQ.-1) THEN
        A_EXIT=.TRUE.

*  null entry at top level to exit
      ELSEIF (NLEV.EQ.0.AND.STRING.EQ.' ') THEN
        A_EXIT=.TRUE.

      ELSE
        L=CHR_LEN(STRING)
*  jump out of tree
        IF (STRING(1:1).EQ.BSLASH) THEN
          A_TEXT=STRING(2:)
          A_LEN=L-1
*  jump back to top
          IF (A_LEN.EQ.0) THEN
            A_TOP=.TRUE.
*  hypertext mode
          ELSE
            A_HYPER=.TRUE.
          ENDIF

*  repeat page
        ELSE IF (STRING(1:1) .EQ. '?') THEN

c          STRING = HST_PATH(HSP)
c          L = CHR_LEN( STRING )

*  go back to previous display
        ELSEIF (STRING.EQ.'-') THEN
          A_PREV = .TRUE.

*  normal hierarchical mode
        ELSE          
          LP = CHR_LEN( HST_PATH(HSP) )
          IF ( L .GT. 0 ) THEN
            LP=MAX(1,LP)
            PATH= HST_PATH(HSP)(1:LP)//' '//STRING(1:L)
            CALL HLPREAD_PUSHPATH(PATH, STATUS )
          ELSE
            IF ( HSP .GT. 1 ) THEN
              IF ( HST_IFILE(HSP) .NE. HST_IFILE(HSP-1) ) THEN
                A_PREV = .TRUE.
              ELSE
                CALL HLPREAD_POPPATH( STATUS )
              END IF
            ELSE

*            NLEV > 1 and HSP = 1 means we went straight into a library
*            at this depth. If so, adjust path directly as previous 
*            level is not on the stack.
              IF ( NLEV .GT. 1 ) THEN
                IP = LP
                DO WHILE ( (IP.GE.1) .AND. 
     :                     (HST_PATH(HSP)(IP:IP).NE.' ') )
                  IP = IP - 1
                END DO
                HST_PATH(HSP)(:LP) = HST_PATH(HSP)(:IP-1)
              ELSE
                A_EXIT = .TRUE.
              END IF

            END IF
          END IF

        ENDIF
      ENDIF

*    Return error to get back to top level for special modes
      IF ( A_EXIT .OR. A_TOP .OR. A_HYPER .OR. A_PREV ) THEN
        HLPREAD_IN = -1
      ELSE
        HLPREAD_IN = 1
      END IF

      END


*+  HLPREAD_FLUSH
      SUBROUTINE HLPREAD_FLUSH( STATUS )
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Function declarations :
*
      INTEGER CHR_LEN
*
*    Status :
*
      INTEGER	STATUS
*
*    Global variables :
*
      INCLUDE 'HLPREAD_CMN'
*
*    Local constants :
*
      CHARACTER*4		BOLD_ON			! Bold on/off tags
      CHARACTER*4		BOLD_OFF	
      CHARACTER*4		LINE_ON			! Underline on/off tags
      CHARACTER*5		LINE_OFF	

      CHARACTER*4		BLINK_ON		! Blink on/off tags
      CHARACTER*5		BLINK_OFF	

      CHARACTER*1		BSLASH			! The backslash 
*
*    Local variables :
*
      CHARACTER*132 		BUFFER			! Output buffer
      CHARACTER*40 		INPUT                   ! User response

      INTEGER			ILINK			! New link number
      INTEGER I,J,ITAG
      INTEGER 			ISTAT			! i/o status code
*-
      BOLD_ON=CHAR(27)//'[1m'
      BOLD_OFF=CHAR(27)//'[0m'
      LINE_ON=CHAR(27)//'[4m'
      LINE_OFF=CHAR(27)//'[0m'
      BLINK_ON=CHAR(27)//'[5m'
      BLINK_OFF=CHAR(27)//'[0m'
      BSLASH = CHAR(92)

*    Check buffer has data
      IF ( IBUFP .EQ. 0 ) RETURN

*    Check line count in screen mode
      IF ( A_SCREEN ) THEN

*      Filled top bit of screen?
        IF ( SC_NLINE .GT. (SC_DEP-2) ) THEN

*        Get user response
          WRITE(*,'(A,$)') 'more...'
 10       READ(*,'(A)',IOSTAT=ISTAT) INPUT
          IF ( ISTAT .EQ. -1 ) THEN
*  ctrl-Z to exit
            A_EXIT=.TRUE.

*        Diagnostic dump
          ELSE IF ( INPUT(1:2) .EQ. '@@' ) THEN
            CALL HLPREAD_DIAG()
            GOTO 10

*        Go to top display
          ELSE IF ( INPUT .EQ. BSLASH ) THEN
            A_TOP = .TRUE.

*        Hypertext jump
          ELSE IF ( INPUT(1:1) .EQ. BSLASH ) THEN
            A_TEXT = INPUT(2:)
            A_LEN = CHR_LEN(A_TEXT)
            A_HYPER = .TRUE.

*        Go back to previous display
          ELSE IF (INPUT(1:1).EQ.'-'.OR.INPUT(1:1).EQ.'?') THEN
            A_PREV = .TRUE.

          END IF

*        Always start a new page
          SC_NLINE = 0

*        Abort now if state flags set
          IF ( A_HYPER .OR. A_TOP .OR. A_EXIT .OR. A_PREV ) THEN
            A_IGNORE = .TRUE.
            RETURN
          END IF

*      End of screen filled test
        END IF

*    End of switch on screen mode
      END IF

*    Initialise scan over input buffer
      I=1
      J=1

*    Look for special codes in the help text, and replace with text from
*    the tags file if present and/or insert ANSI escape sequences.
      DO WHILE ( I .LE. IBUFP )

*      Special marker
        IF ( IBUF(I:I+1) .EQ. '@@' ) THEN

*        Skip marker
          I = I + 2

*        Bold on?
          IF ( IBUF(I:I) .EQ. 'B' ) THEN

            CALL HLPREAD_SINS( BUFFER, J, BOLD_ON )
            I = I + 1

*        Bold off?
          ELSE IF ( IBUF(I:I) .EQ. 'b' ) THEN

            CALL HLPREAD_SINS( BUFFER, J, BOLD_OFF )
            I = I + 1

*        Otherwise a tag index
          ELSE
            READ( IBUF(I:I+3), '(I4.4)' ) ITAG 
            I = I + 4
            CALL HLPREAD_GETTAG( ITAG, ILINK, STATUS )
            CALL HLPREAD_SINS( BUFFER, J, LINE_ON )
            CALL HLPREAD_SINS( BUFFER, J, HLI_TXT(ILINK)
     :                  (:CHR_LEN(HLI_TXT(ILINK))) )
            CALL HLPREAD_SINS( BUFFER, J, LINE_OFF )

          END IF

*      Skip to next character
        ELSE
          BUFFER(J:J) = IBUF(I:I)
          J = J + 1
          I = I + 1

        END IF

*    End loop over data
      END DO

*    Write data
      IF ( A_SCREEN ) THEN
        WRITE(*,'(A)') BUFFER(1:J-1)
        SC_NLINE = SC_NLINE + 1
      END IF
      IF ( A_FILE ) THEN
        CALL FIO_WRITE( A_FD, BUFFER(1:J-1), STATUS )
      END IF

*    Input buffer now empty
      IBUFP = 0

      END

      SUBROUTINE HLPREAD_SINS( BUF, PTR, BIT )
      CHARACTER*(*) BUF,BIT
      INTEGER		ptr
      BUF(PTR:PTR+LEN(BIT)-1) = BIT
      PTR = PTR + LEN(BIT)
      END

*+  HLPREAD_OUT
      INTEGER FUNCTION HLPREAD_OUT( STRING )
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*     Import :
*
      CHARACTER*(*) 		STRING
*
*    Function declarations :
*
      INTEGER			CHR_LEN
*
*    Global variables :
*
      INCLUDE 'HLPREAD_CMN'
*
*    Local constants :
*
      CHARACTER*1 TWIDDLE
        PARAMETER (TWIDDLE = '~')
*
*    Local variables :
*
      INTEGER			SL			! Length of STRING
      INTEGER			STATUS			! 
*-

*    Initialise
      STATUS = SAI__OK

*    Not ignoring text
      IF ( .NOT. A_IGNORE ) THEN

*      Continuing from previous line?
        IF ( IBUFP .GT. 0 ) THEN
          CALL CHR_LDBLK( STRING )
        END IF

*      Get length of input
        SL = MAX(1,CHR_LEN( STRING ))

*      Add to buffer
        IBUF(IBUFP+1:IBUFP+SL) = STRING(:SL)
        IBUFP = IBUFP + SL

*      Continuation on STRING?
        IF ( STRING(SL:SL) .EQ. TWIDDLE ) THEN
          IBUFP = IBUFP - 1
        ELSE
          CALL HLPREAD_FLUSH( STATUS )
        END IF

*    End of switch on ignore flag
      END IF

*    If in ignore mode, status is ok
      IF ( A_IGNORE ) THEN
        HLPREAD_OUT = 1
 
*    State flags force exit to top level     
      ELSE IF ( A_HYPER .OR. A_PREV .OR. A_TOP .OR. A_EXIT ) THEN
        HLPREAD_OUT = -1

*    Otherwise ok
      ELSE
        HLPREAD_OUT = 1

      END IF

      END


*+
      SUBROUTINE HLPREAD_LEVEL(STRING,LEV)

      IMPLICIT NONE

      CHARACTER*(*) STRING
      INTEGER LEV

      INTEGER CHR_LEN

      INTEGER L,I
*-

      L=CHR_LEN(STRING)
      LEV=0
      DO I=1,L
        IF (STRING(I:I).EQ.' ') THEN
          LEV=LEV+1
        ENDIF
      ENDDO

      END


*+  HLPREAD_PUSHPATH( PATH, STATUS )
      SUBROUTINE HLPREAD_PUSHPATH( PATH, STATUS )
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Global variables :
*
      INCLUDE 'HLPREAD_CMN'
*
*    Import :
*
      CHARACTER*(*)		PATH			! Help path
*
*    Status :
*
      INTEGER STATUS
*-

      HSP = HSP + 1
      HST_IFILE(HSP) = NFILE
      HST_PATH(HSP) = PATH
      IF ( HSP .EQ. 1 ) THEN
        HST_FLINK(HSP) = 1
      ELSE
        HST_FLINK(HSP) = HST_FLINK(HSP-1) + HST_NLINK(HSP-1)
      END IF
      HST_NLINK(HSP) = 0

      END


*+  HLPREAD_SETPATH
      SUBROUTINE HLPREAD_SETPATH( PATH, STATUS )
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Global variables :
*
      INCLUDE 'HLPREAD_CMN'
*
*    Import :
*
      CHARACTER*(*)		PATH			! Index path spec
*
*    Status :
*
      INTEGER STATUS
*
*    Function definitions :
*
      INTEGER			CHR_LEN
*
*    Local variables :
*
      CHARACTER*132		LNAME			! Library full name
      CHARACTER*132		LPATH			! Local version
      CHARACTER*132		NPATH			! Index entry
      CHARACTER*132		PARAMS			! Extra parameters
 
      INTEGER                   IP 			! Character index
      INTEGER			K			! Length of 1st word

      LOGICAL			FOUND			! Found index entry?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      NPATH = PATH

*    Loop point for library jump in index
  10  CONTINUE

*    Change of library?
      IF ( NPATH(1:1) .EQ. '/' ) THEN

*      Extract library name
        IP = INDEX(NPATH(2:),'/')
        IF ( IP .EQ. 0 ) IP = CHR_LEN(NPATH)
        LNAME = NPATH(2:IP)
        LPATH = NPATH(IP+2:)
        IP = IP - 1
      
*      Set up library and index names
        CALL HLPREAD_PUSHFILE( LNAME(:IP), STATUS )

*      Set the path
        CALL CHR_LDBLK( LPATH )

      ELSE
        LPATH = PATH

      END IF

*    Is this a relative path spec?
      IF ( (HSP.GT.1) .AND. (LPATH(1:3) .EQ. '.. ') ) THEN
        IP = CHR_LEN( LPATH ) 
        DO WHILE ( (IP.GE.1) .AND. (HST_PATH(HSP)(IP:IP).NE.' ') )
          IP = IP - 1
        END DO
        LPATH = HST_PATH(HSP)(1:IP-1)//LPATH(3:132)
      END IF

*    Get first word and rest of path
      K = INDEX(LPATH,' ')
      PARAMS = LPATH(K:)
      LPATH(K:) = ' '

*    Is there an index entry?
      CALL HLPREAD_INDEX( HFI_IFD(NFILE), LPATH(1:K), NPATH, 
     :                    FOUND, STATUS )

*    Set the path
      IF ( FOUND ) THEN

*      Add parameters back on 
        NPATH = NPATH(1:CHR_LEN(NPATH))//' '//PARAMS

*      Library jump?
        IF ( NPATH(1:1) .EQ. '/' ) THEN

*        Put top-level of root library on stack if this is the first call
          IF ( HSP .EQ. 0 ) THEN
            CALL HLPREAD_PUSHPATH( ' ', STATUS )
          END IF
          GOTO 10
        ELSE
          CALL HLPREAD_PUSHPATH( NPATH, STATUS )
        END IF
      ELSE
        CALL HLPREAD_PUSHPATH( LPATH(1:K)//PARAMS, STATUS )
      END IF
      
      END


*+  HLPREAD_PUSHFILE
      SUBROUTINE HLPREAD_PUSHFILE( LROOT, STATUS )
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Global variables :
*
      INCLUDE 'HLPREAD_CMN'
*
*    Import :
*
      CHARACTER*(*)		LROOT			! Help root name
*
*    Status :
*
      INTEGER 			STATUS
*
*    Function definitions :
*
      INTEGER			CHR_LEN
*
*    Local variables :
*
      CHARACTER*132             FILE
      INTEGER			LR			! Length of LROOT

      LOGICAL			FOUND
*-

*    Length of LROOT
      LR = CHR_LEN( LROOT )

*    Increment file pointer
      NFILE = NFILE + 1

*    Unit for library access
      CALL FIO_GUNIT( HFI_HLN(NFILE), STATUS )
      
*    Store the root
      HFI_ROOT(NFILE) = LROOT
      HFI_LROOT(NFILE) = LR
    
*    Look for the index
      FILE= LROOT(1:LR)//'.index'
      CALL AIO_PSRCH( 'AST_HELP_PATH',FILE, 
     :                FOUND, HFI_IDX(NFILE), STATUS )

      IF ( FOUND ) THEN
        CALL FIO_OPEN( HFI_IDX(NFILE), 'READ', 'LIST', 0, 
     :                 HFI_IFD(NFILE), STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          HFI_IFD(NFILE) = 0
        END IF

      ELSE
        HFI_IFD(NFILE) = 0
        HFI_IDX(NFILE) = ' '

      END IF

*    Look for the tags
      FILE= LROOT(1:LR)//'.tags'
      CALL AIO_PSRCH( 'AST_HELP_PATH',FILE,
     :                FOUND, HFI_TAG(NFILE), STATUS )

      IF ( FOUND ) THEN
        CALL FIO_OPEN( HFI_TAG(NFILE), 'READ', 'LIST', 0, 
     :                 HFI_TFD(NFILE), STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          HFI_TFD(NFILE) = 0
        END IF
        HFI_CTL(NFILE) = 0

      ELSE
        HFI_TFD(NFILE) = 0
        HFI_TAG(NFILE) = ' '

      END IF

      END


*+  HLPREAD_POPFILE
      SUBROUTINE HLPREAD_POPFILE( STATUS )
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Global variables :
*
      INCLUDE 'HLPREAD_CMN'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER			ISTAT			! Fortran i/o status
*-

      IF ( HFI_IFD(NFILE) .GT. 0 ) THEN
        CALL FIO_CLOSE( HFI_IFD(NFILE), STATUS )
        HFI_IFD(NFILE) = 0
      END IF

      CLOSE( UNIT=HFI_HLN(NFILE), IOSTAT=ISTAT )      
      CALL FIO_PUNIT( HFI_HLN(NFILE), STATUS )

      NFILE = NFILE - 1

      END


*+  HLPREAD_POPPATH
      SUBROUTINE HLPREAD_POPPATH( STATUS )
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Global variables :
*
      INCLUDE 'HLPREAD_CMN'
*
*    Status :
*
      INTEGER			STATUS
*-

*    Need to close the file?
      IF ( HSP .GT. 1 ) THEN
        IF ( HST_IFILE(HSP) .NE. HST_IFILE(HSP-1) ) THEN
          CALL HLPREAD_POPFILE( STATUS )
        END IF
      END IF

*    Decrement stack pointer
      HSP = HSP - 1

*    Exit if popped overything
      IF ( HSP .EQ. 0 ) A_EXIT = .TRUE.

      END


*+  HLPREAD_DIAG
      SUBROUTINE HLPREAD_DIAG( )
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Global variables :
*
      INCLUDE 'HLPREAD_CMN'
*
*    Local variables :
*
      INTEGER			I
*-

*    Write file list
      print *,'Num  Source root'
      DO I = 1, NFILE
        WRITE( *, '(1X,I3,2X,A40)' ) I, HFI_ROOT(I)(1:HFI_LROOT(I))
      END DO
      PRINT *,' '

*    Write stack list
      print *,'Num  Src  Topic path'
      DO I = 1, HSP
        WRITE( *, '(1X,I3,2X,I3,2X,A60)' ) I, HST_IFILE(I), HST_PATH(I)
      END DO

      END



*+  HLPREAD_NAMETR - Translate library names for HELP package
      SUBROUTINE HLPREAD_NAMETR( KMD, INSTR, OUTSTR, JSTAT )
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER			KMD			! HELP command
      CHARACTER*(*)		INSTR			! Input name
*
*    Export :
*
      CHARACTER*(*)		OUTSTR			! Output name
      INTEGER			JSTAT			! Status
*
*    Function definitions :
*
      INTEGER			CHR_LEN
*
*    Local variables :
*
      CHARACTER*132             PATH

      INTEGER			STATUS

      LOGICAL			FOUND			! Found file?
*-

*    Translate name command
      IF ( KMD .EQ. 0 ) THEN

*      Search for it in the path
        STATUS = SAI__OK
        PATH=INSTR(1:CHR_LEN(INSTR))//'.shl'
        CALL AIO_PSRCH( 'AST_HELP_PATH',PATH , 
     :                  FOUND, OUTSTR, STATUS )

*      Set status
        IF ( FOUND ) THEN
          JSTAT = 0
        ELSE
          JSTAT = -17
        END IF

      END IF

      END



*+  HLPREAD_GETTAG
      SUBROUTINE HLPREAD_GETTAG( ITAG, ILNK, STATUS )
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Global variables :
*
      INCLUDE 'HLPREAD_CMN'
*
*    Import :
*
      INTEGER			ITAG			! Tag # in tag file
*
*    Export :
*
      INTEGER			ILNK			! New link
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local variables :
*
      CHARACTER*132		LINE			! Line of tag file

      INTEGER			JLNK
*-

*    Tag file exists? 
      IF ( HFI_TFD(NFILE) .GT. 0 ) THEN

*      Scan tags already read in 
        IF ( HST_NLINK(HSP) .GT. 0 ) THEN
          JLNK = HST_FLINK(HSP)
          DO WHILE ( JLNK .LE. HST_FLINK(HSP) + HST_NLINK(HSP) - 1 )
            IF ( ITAG .EQ. HLI_ITAG(JLNK) ) THEN
              ILNK = JLNK
              GOTO 99
            ELSE
              JLNK = JLNK + 1
            END IF
          END DO
        END IF

*      If tag number is higher than the current line, then rewind
        IF ( (HFI_CTL(NFILE).GT.0) .AND. (ITAG.LE.HFI_CTL(NFILE)) ) THEN
          CALL FIO_RWIND( HFI_TFD(NFILE), STATUS )
          HFI_CTL(NFILE) = 0 
        END IF

*      Scan until the line is reached
        DO WHILE ( (HFI_CTL(NFILE).NE.ITAG) .AND. (STATUS.EQ.SAI__OK) )
          CALL FIO_READF( HFI_TFD(NFILE), LINE, STATUS )
          HFI_CTL(NFILE) = HFI_CTL(NFILE) + 1
        END DO

*      If error, report it
        IF ( STATUS .EQ. SAI__OK ) THEN

          ILNK = HST_FLINK(HSP) + HST_NLINK(HSP)

          HST_NLINK(HSP) = HST_NLINK(HSP)  + 1

          HLI_ITAG(ILNK) = ITAG
          HLI_PATH(ILNK) = LINE(:40)
          HLI_TXT(ILNK) = LINE(41:)

        ELSE
          CALL ERR_ANNUL( STATUS )
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Illegal tag number present '/
     :                  /'in help source', STATUS )

        END IF

*    Otherwise error
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Tag present in help source with no '/
     :                /'associated tag file', STATUS )

      END IF

*    Tidy up
 99   CONTINUE

      END
