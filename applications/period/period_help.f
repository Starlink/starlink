
      SUBROUTINE PERIOD_HELP(COMMAND)

C=============================================================================
C Routine to provide on-line help for PERIOD.
C
C Written by Vikram Singh Dhillon @LPO 17-March-1993.
C
C Added code to read the value of the environmental variable
C PERIOD_HELP that defines the path to where the help files are
C kept. Also, restructured the code somewhat.
C
C GJP June 1995
C
C=============================================================================

      IMPLICIT NONE

C-----------------------------------------------------------------------------
C  External References
C  GJP June 1995
C-----------------------------------------------------------------------------
      INTEGER
     :  CHR_LEN                ! Length of character strings ignoring
                               ! trailing blanks

C-----------------------------------------------------------------------------
C PERIOD_HELP declarations.
C-----------------------------------------------------------------------------

      INTEGER I, J
      LOGICAL PERIOD_PARSE
      CHARACTER*1 KEY
      CHARACTER*12 COMMAND
      CHARACTER*72 STRING

C-----------------------------------------------------------------------------
C The name of the environmental variable defining the path to the help files
C GJP June 1995
C-----------------------------------------------------------------------------

      CHARACTER*12 LIBNAM
      PARAMETER ( LIBNAM = 'PERIOD_HELP' ) ! Env variable name
      CHARACTER*122 PATH                   ! Library path
      CHARACTER*256 FNAME
      CHARACTER*8 MESS1
      CHARACTER*20 NAME1
      INTEGER NC1, NC2
      INTEGER STATUS

C-----------------------------------------------------------------------------
C Declare data arrays.
C-----------------------------------------------------------------------------


C-----------------------------------------------------------------------------
C Get the help file path - GJP June 1995
C-----------------------------------------------------------------------------

C  Initialise strings.
      FNAME=' '
      MESS1=' '
      PATH= ' '

C  Translate the environment variable/logical name.
      STATUS=0
      CALL PSX_GETENV( LIBNAM, PATH, STATUS )
      IF ( STATUS .NE. 0 ) THEN
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) ' '
         WRITE (*, *) '** ERROR: Problems encountered while'
         WRITE (*, *) '          looking at environmental variable'
         WRITE (*,*)  '          '//LIBNAM
         WRITE (*, *) ' '
         RETURN
       END IF


C-----------------------------------------------------------------------------
C Help information.
C-----------------------------------------------------------------------------

      IF ( PERIOD_PARSE(COMMAND,'HELP') ) THEN
         MESS1='HELP'
         NAME1='period_help.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP INPUT') ) THEN
         MESS1='INPUT'
         NAME1='period_input.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP OGIP') ) THEN
         MESS1='OGIP'
         NAME1='period_ogip.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP NOISE') ) THEN
         MESS1='NOISE'
         NAME1='period_noise.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP WINDOW') ) THEN
         MESS1='WINDOW'
         NAME1='period_window.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP FAKE') ) THEN
         MESS1='FAKE'
         NAME1='period_fake.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP DETREND') ) THEN
         MESS1='DETREND'
         NAME1='period_detrend.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP SINE') ) THEN
         MESS1='SINE'
         NAME1='period_sine.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP FOLD') ) THEN
         MESS1='FOLD'
         NAME1='period_fold.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP HELP') ) THEN
         MESS1='HELP'
         NAME1='period_help.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP QUIT') ) THEN
          MESS1='QUIT'
          NAME1='period_quit.hlp'
          GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP FIT') ) THEN
         MESS1='FIT'
         NAME1='period_fit.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP OPEN') ) THEN
         MESS1='OPEN'
         NAME1='period_open.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP CLOSE') ) THEN
         MESS1='CLOSE'
         NAME1='period_close.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP PERIOD') ) THEN
         MESS1='PERIOD'
         NAME1='period_period.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP PLT') ) THEN
         MESS1='PLT'
         NAME1='period_plt.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP STATUS') ) THEN
         MESS1='STATUS'
         NAME1='period_status.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP OUTPUT') ) THEN
         MESS1='OUTPUT'
         NAME1='period_output.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP SELECT') ) THEN
         MESS1='SELECT'
         NAME1='period_select.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP FREQ') ) THEN
         MESS1='PERIOD_.HLP'
         NAME1='period_freq.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP CHISQ') ) THEN
         MESS1='CHISQ'
         NAME1='period_chisq.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP FT') ) THEN
         MESS1='FT'
         NAME1='period_ft.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP SCARGLE') ) THEN
         MESS1='SCARGLE'
         NAME1='period_scargle.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP CLEAN') ) THEN
         MESS1='CLEAN'
         NAME1='period_clean.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP STRING') ) THEN
         MESS1='STRING'
         NAME1='period_string.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP PDM') ) THEN
         MESS1='PDM'
         NAME1='period_pdm.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP PEAKS') ) THEN
         MESS1='PEAKS'
         NAME1='period_peaks.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP SIG') ) THEN
         MESS1='SIG'
         NAME1='period_sig.hlp'
         GOTO 10
      END IF
      IF ( PERIOD_PARSE(COMMAND,'HELP EXIT') ) THEN
         MESS1='EXIT'
         NAME1='period_quit.hlp'
         GOTO 10
      END IF

C   Jump out point for help subject unknown.
C   This is only reached when none of the above statements are
C   satisfied.
      CALL PERIOD_WRITEBELL()
      WRITE (*, *) ' '
      WRITE (*, *) '** ERROR: Help command ',COMMAND
      WRITE (*, *) '          has not been recognised.'
      WRITE (*, *) ' '
      RETURN

C   Open the required help file.
10    CONTINUE

C   Determine how much of the path string is used.
      NC1=CHR_LEN(PATH)

C   Determine how much of the file name string is used.
      NC2=CHR_LEN(NAME1)

C   Construct output string and open the file.
      FNAME=PATH(1:NC1)//'/'//NAME1(1:NC2)
      OPEN (UNIT=10, FILE=FNAME, STATUS='OLD', ERR=4000)

C      Read the file contents.
         DO 50 I = 1, 1000000
            DO 20 J = 1, 21
               READ (10, '(A)', END=3000) STRING
               WRITE (*, *) STRING
 20         CONTINUE

C         Prompt at end of page.
            WRITE (*, *) ' '
            WRITE (*, '(X,A,$)')
     :      '** OK: Please hit return to continue or q to quit... '
            READ (*, '(A)', ERR=3000) KEY
            CALL PERIOD_CASE(KEY, .TRUE.)
            IF ( KEY.EQ.'Q' ) GO TO 5000
            WRITE (*, *) ' '
 50      CONTINUE
 100     CONTINUE

C   Fall through location.
 3000 CONTINUE
      CLOSE (UNIT=10)

      WRITE (*, *) ' '
      WRITE (*, *) 'End of the HELP file!'
      WRITE (*, *) ' '

      RETURN

C   Jump out point if the help file could not be opened.
 4000 CONTINUE
      CALL PERIOD_WRITEBELL()
      WRITE (*, *) '** ERROR: Could not open HELP file ',NAME1
      WRITE (*, *) ' '
      WRITE (*, *) 'Does environmental variable ',LIBNAM
      WRITE (*, *) 'have the right value? It is currently set to'
      WRITE (*, *) PATH
      WRITE (*, *) ' '
      RETURN

C   Help file read aborted.
 5000 CONTINUE
      CLOSE (UNIT=10)

      WRITE (*, *) ' '
      WRITE (*, *) 'HELP file read aborted!!!'
      WRITE (*, *) ' '

      RETURN


      END
