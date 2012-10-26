*  History:
*     16 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*        Disuse the TRAPC condition handler.
*     30 Nov 1993 (hme):
*        Change .SPX extension to lower case (but leave journal file at
*        SPECX.spx.
*     17 Dec 1993 (hme):
*        Re-order IODATA common block to avoid alignment problems.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*     07 Feb 1994 (hme):
*        Call PARSEAT before GEN_AT to translate environment variables.
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused LS, TCHAR, STRING_TEST
*        Correct error number 101 to 100 for WRITE
*-----------------------------------------------------------------------

      LOGICAL FUNCTION DO_SYSCOMM (PROCEDURE, COMMAND, EXECUTE, IERR)

      IMPLICIT  NONE

*     Formal parameters
      LOGICAL   PROCEDURE
      CHARACTER COMMAND*(*)
      LOGICAL   EXECUTE
      INTEGER   IERR

*     Local variables
      INTEGER   NCH
      INTEGER   I
      INTEGER   ILS
      INTEGER   ISP
      INTEGER   LOUT
      LOGICAL   TEST
      LOGICAL   READONLY
      CHARACTER SYMBOL*64
      CHARACTER INSTRING*64
      CHARACTER STRING*256
      INTEGER   ISTAT
      CHARACTER PROMPT*40
      INTEGER   RETLEN

*     IF / DO control

      INCLUDE 'SCL_IFDO.INC'

*     Command line buffer

      INCLUDE 'SCL_BUFFER.INC'

*     Error control

      CHARACTER ERR_CHAR*1
      CHARACTER ERRORS*4
      DATA      ERRORS /'IWEF'/

      INTEGER             MAX_OK_ERROR
      COMMON /SPX_ERRORS/ MAX_OK_ERROR

*     Job status

      INCLUDE  'JPI'

*     Miscellaneous

      INCLUDE  'IODATA'
      INCLUDE  'CNF_PAR'

*     For GEN_INQSYMB

      INTEGER   ADDR
      INTEGER   SYM_INDEX
      CHARACTER TYPE*4
      INTEGER   LENGTH

*     Function names

      LOGICAL   PUSH_IFSTACK
      LOGICAL   PULL_IFSTACK
*      LOGICAL   STRING_TEST
      INTEGER   STACK_POINTER
      INTEGER   GEN_ILEN
      LOGICAL*4 SCL_DO

* ---- VMS only ------
*     EXTERNAL TRAPC
* --------------------

CD     PRINT *, '-- do_syscomm --   ierr=', ierr

      DO_SYSCOMM = .TRUE.
      IF (IERR.NE.0) RETURN

*   Establish CTRL(C) condition handler

* ---- VMS only ------
*     IF (PROC_MODE.EQ.3) CALL LIB$ESTABLISH (TRAPC)
* --------------------

*----------------------
*  IF:
*----------------------

      IF (COMMAND.EQ.'IF') THEN

*       If we are only searching for the next ELSEIF/ELSE/ENDIF then
*       we need to ignore this, but to make a mental note that subsequent
*       ELSEIF/ELSEs are to be ignored, and that the next ENDIF should only
*       decrement the nest count.

        IF (.NOT.EXECUTE) THEN
          IF_SKIP = IF_SKIP + 1
CD        PRINT *,' IF nesting increased -- ', IF_SKIP
          RETURN
        END IF

*       We are not allowing this at command level -- too complicated

        ISP = STACK_POINTER()
        IF (ISP.EQ.0) THEN
          IERR = 77
          RETURN
        END IF

*       What if we are already in the middle of an IF statement?
*       Save the information about that on the IF-stack

        IF_LEVEL = ISP
        IF (.NOT.
     &      PUSH_IFSTACK (IF_LEVEL)) THEN
          IERR = 78
          RETURN
        END IF

*       Well, there is a strong possibility that we MAY want to execute
*       stuff in one of the IF blocks, so we certainly need to execute each
*       test until we find a positive one or until we exit the IF.

        WAIT_FOR_ENDIF     = .FALSE.

*       Execute the test now. If positive we start doing the contents of the
*       current IF block, otherwise wait for the next test.

        CALL GEN_GETSTR2 (3, ' ', ' ', ' ', STRING, ISTAT)
CD      Print *, 'Expression to test: ', STRING(:50)
        CALL GEN_EVAL_AE (STRING, 'L4', DO_TO_ELSEIF, ISTAT)

        EXECUTE = DO_TO_ELSEIF
        RETURN

*----------------------
*  ELSEIF:
*----------------------

      ELSE IF (COMMAND.EQ.'ELSEIF') THEN

*       Ignore if we are presently skipping over stuff

        IF (IF_SKIP.NE.0) THEN
          RETURN
        END IF

*       Error if no IF going at present

        IF (IF_LEVEL.EQ.0) THEN
          IERR = 76
          RETURN
        END IF

*       If we were doing the stuff in the previous IF block then this
*       statement terminates that until the next ENDIF

        IF (DO_TO_ELSEIF .OR. WAIT_FOR_ENDIF) THEN
          WAIT_FOR_ENDIF = .TRUE.
          DO_TO_ELSEIF   = .FALSE.

*       Otherwise we need to evaluate the test and see if we need to
*       execute the stuff in THIS IF block.

        ELSE
        CALL GEN_GETSTR2 (3, ' ', ' ', ' ', STRING, ISTAT)
CD      Print *, 'Expression to test: ', STRING(:50)
        CALL GEN_EVAL_AE (STRING, 'L4', DO_TO_ELSEIF, ISTAT)
        END IF

        EXECUTE = DO_TO_ELSEIF
        RETURN

*----------------------
*  ELSE:
*----------------------

      ELSE IF (COMMAND.EQ.'ELSE') THEN

*       Ignore if we are presently skipping over stuff

        IF (IF_SKIP.NE.0) RETURN

*       Error if no IF going at present

        IF (IF_LEVEL.EQ.0) THEN
          IERR = 76
          RETURN
        END IF

*       If we were doing the stuff in the previous IF block then this
*       statement terminates that until the next ENDIF

        IF (DO_TO_ELSEIF) THEN
          WAIT_FOR_ENDIF = .TRUE.
          DO_TO_ELSEIF   = .FALSE.

*       This is just like ELSEIF, except that if we have not done anything
*       yet then we automatically do this part. How do we tell if we have
*       done anything yet? Examine WAIT_FOR_ENDIF: This is only set AFTER
*       an IF block exits.

        ELSE IF (WAIT_FOR_ENDIF) THEN
          DO_TO_ELSEIF    = .FALSE.

*       Since we have neither just completed the previous IF block nor
*       executed any OTHER IF block in this construct previously we had
*       better do this "catch-all" clause

        ELSE
          DO_TO_ELSEIF    = .TRUE.
        END IF

        EXECUTE = DO_TO_ELSEIF
        RETURN

*----------------------
*  ENDIF:
*----------------------

      ELSE IF (COMMAND.EQ.'ENDIF') THEN

*       If we are currently skipping over stuff to look for THIS statement,
*       then just decrement the IF_SKIP parameter

        IF (IF_SKIP.GT.0) THEN
          IF_SKIP = IF_SKIP - 1
CD        PRINT *,' IF nesting decreased -- ', IF_SKIP
          RETURN
        END IF

*       Error if no IF going at present

        IF (IF_LEVEL.EQ.0) THEN
          IERR = 76
          RETURN
        END IF

*       Of course this doesn't mean that we have done ALL IFs that might
*       be outstanding. Find out state now this one is done...

        IF (.NOT. PULL_IFSTACK (ISP)) THEN
          IERR = 79
          RETURN
        ELSE
          DO_TO_ELSEIF   = .TRUE.
          WAIT_FOR_ENDIF = .FALSE.
          EXECUTE        = DO_TO_ELSEIF
          IF_SKIP        = 0
          IF_LEVEL       = ISP
        END IF

        IF (IF_LEVEL.EQ.0) EXECUTE = .TRUE.

        RETURN

      END IF

*   Ok, well the command wasn't part of the IF set, but it may be that
*   we are in the middle of an un-executed IF block. If so just return
*   (checking also that we are somewhere inside an IF block at all).

CD    PRINT *,'    if_level, do_to_elseif: ', if_level, do_to_elseif
      IF (IF_LEVEL.NE.0 .AND. .NOT.DO_TO_ELSEIF) RETURN

*   So now we can check if it was something more mundane,
*   and if so then just do whatever it wants us to.
*   If not found at all return with IERR=81

*----------------------
*  DO:
*----------------------

      IF (COMMAND.EQ.'DO') THEN
        IF (SCL_DO (IERR)) THEN
          ENDDO = .FALSE.
        ELSE
          CLOSE (5)
          CLOSE (6)
          CALL INIT_CLI
          ENDDO = .TRUE.
        END IF

*----------------------
*  BREAK:
*----------------------

      ELSE IF (COMMAND.EQ.'BREAK') THEN
          CALL SCL_BREAK()

*----------------------
*  ENDDO: End the DO loop
*----------------------

      ELSE IF (COMMAND.EQ.'ENDDO') THEN
        CALL SCL_ENDDO (ENDDO)

*----------------------
*   SET-JOURNAL: Set the journal file on or off
*----------------------

      ELSE IF (COMMAND.EQ.'SET-JOURNAL') THEN
        CALL GEN_YESNO ('Journal file on?', .FALSE., TEST, ISTAT)
        IF (TEST) THEN
          CALL GEN_JNLON ('SPECX.spx', IERR)
        ELSE
          CALL GEN_JNLOFF (IERR)
        END IF

*----------------------
*   PRINT: Print a variable
*----------------------

      ELSE IF (COMMAND.EQ.'PRINT') THEN
        CALL GEN_GETSTR2 (3, 'Items to print? ',' ',' ', STRING, ISTAT)
        CALL SET_LUN_OUT (ILOUT2)
        CALL GEN_SPRINT  (STRING, IERR)
        IF (IERR.NE.0) IERR = 109
        CALL SET_LUN_OUT (6)

*----------------------
*   WRITE: Encode contents of variable(s) as character string
*----------------------

      ELSE IF (COMMAND.EQ.'WRITE') THEN
        CALL GEN_GETSTR2 (1, 'Character variable? ',
     &                    ' ', ' ', SYMBOL, ISTAT)
        CALL GEN_GETSTR2 (3, 'Items to print? ',
     &                    ' ', ' ', STRING, ISTAT)

        CALL GEN_INQSYMB (SYMBOL, SYM_INDEX, TYPE, LENGTH,
     &                    ADDR, READONLY, ISTAT)

        IF (ISTAT.eq.0) THEN
          IF (TYPE(:1).EQ.'C') THEN
            READ (TYPE(2:), *) LOUT
            CALL GEN_WRITE  (STRING, %VAL(CNF_PVAL(ADDR)), LOUT, IERR)
            IF (IERR.NE.0) IERR = 109   ! Error in PRINT command
          ELSE
            IERR = 98                   ! Must be a string variable
          END IF
        ELSE IF (SYM_INDEX.EQ.0) THEN
          IERR = 100                    ! Symbol not found
        ELSE IF (READONLY) THEN
          IERR = 102                    ! Variable is readonly
        END IF

*----------------------
*   READ: Read SPECX variable from internal string
*----------------------

      ELSE IF (COMMAND.EQ.'READ') THEN
        CALL GEN_GETSTR2  (1, 'Variable name?',
     &                    ' ', ' ', SYMBOL, ISTAT)
        CALL GEN_INQSYMB (SYMBOL, SYM_INDEX, TYPE, LENGTH,
     &                    ADDR, READONLY, ISTAT)

        IF (SYM_INDEX.NE.0 .AND. .NOT.READONLY) THEN
          CALL GEN_GETSTR ('Input string?', INSTRING,
     &                    ' ', INSTRING, ISTAT)
          CALL GEN_HDNORM  (INSTRING, INSTRING, ILS, ISTAT)
          CALL GEN_DECODE  (TYPE, INSTRING, %VAL(CNF_PVAL(ADDR)), ISTAT)
        ELSE IF (SYM_INDEX.EQ.0) THEN
          IERR = 101
        ELSE IF (READONLY) THEN
          IERR = 102
        END IF

*----------------------
*   SHOW-SYMBOLS: List the defined command symbols
*----------------------

      ELSE IF (COMMAND.EQ.'SHOW-SYMBOLS') THEN
        CALL GEN_GETSTR2 (1, 'Symbol name?', ' ', ' ', SYMBOL, ISTAT)
        CALL SPECX_SHOWSYMB (SYMBOL)

*----------------------
*   SHOW-VARIABLES: List the defined variables
*----------------------

      ELSE IF (COMMAND.EQ.'SHOW-VARIABLES') THEN
        CALL GEN_GETSTR2 (1, 'Symbol name?', ' ', ' ', SYMBOL, ISTAT)
        CALL SPECX_SHOW_TABLE (SYMBOL)

*----------------------
*   DECLARE: Declare a SPECX variable
*----------------------

      ELSE IF (COMMAND.EQ.'DECLARE') THEN
        CALL GEN_GETSTR2 (1, 'Variable name?', ' ', ' ', SYMBOL, ISTAT)
        CALL GEN_GETSTR2 (1, 'Type? (Cnnn/L4/I4/R4/R8)',
     &                       ' ', ' ', TYPE, ISTAT)
        CALL SPECX_MAKE_VAR (SYMBOL, TYPE, IERR)
        IF (IERR.EQ.101) IERR = 0

*----------------------
*   ASK: Ask for a SPECX variable and set it.
*----------------------

      ELSE IF (COMMAND.EQ.'ASK') THEN
        CALL GEN_GETSTR2  (1, 'Prompt (.le.40 chars)?',
     &                    ' ', ' ', PROMPT, ISTAT)
        CALL GEN_GETSTR2  (1, 'Variable name?',
     &                    ' ', ' ', SYMBOL, ISTAT)
        CALL GEN_INQSYMB (SYMBOL, SYM_INDEX, TYPE, LENGTH,
     &                    ADDR, READONLY, ISTAT)

*       PRINT *, '--- ask ---'
*       PRINT *, '    prompt    = ', prompt
*       PRINT *, '    variable  = ', symbol
*       PRINT *, '    sym_index = ', sym_index
*       PRINT *, '    address   = ', addr
*       PRINT *, '    type      = ', type
*       PRINT *, '    length    = ', length

        IF (SYM_INDEX.NE.0 .AND. .NOT.READONLY) THEN

          CALL GEN_HDNORM (PROMPT, PROMPT, ILS, IERR)

          IF (TYPE.EQ.'I4') THEN
            CALL GEN_GETI4A2 (PROMPT(:ILS), %VAL(CNF_PVAL(ADDR)), 
     :                        LENGTH,
     &                        ' ', %VAL(CNF_PVAL(ADDR)), RETLEN, ISTAT)

          ELSE IF (TYPE.EQ.'R4') THEN
            CALL GEN_GETR4A2 (PROMPT(:ILS), %VAL(CNF_PVAL(ADDR)), 
     :                        LENGTH,
     &                        ' ', %VAL(CNF_PVAL(ADDR)), RETLEN, ISTAT)

          ELSE IF (TYPE.EQ.'R8') THEN
            CALL GEN_GETR8A2 (PROMPT(:ILS), %VAL(CNF_PVAL(ADDR)), 
     :                        LENGTH,
     &                        ' ', %VAL(CNF_PVAL(ADDR)), RETLEN, ISTAT)

          ELSE IF (TYPE.EQ.'L4') THEN
            CALL GEN_YESNO   (PROMPT(:ILS), %VAL(CNF_PVAL(ADDR)),
     &                        %VAL(CNF_PVAL(ADDR)), ISTAT)

          ELSE IF (TYPE(:1).EQ.'C') THEN
            READ (TYPE(2:), *) NCH
            CALL GEN_GETSTR (PROMPT(:ILS), ' ', ' ', STRING, ISTAT)
            CALL XCOPY (NCH, %REF(STRING), %VAL(CNF_PVAL(ADDR)))

          END IF

        ELSE IF (SYM_INDEX.EQ.0) THEN
          IERR = 101
        ELSE IF (READONLY) THEN
          IERR = 102
        END IF

*----------------------
*   RETURN: return prematurely from a command file
*----------------------

      ELSE IF (COMMAND.EQ.'RETURN') THEN
        IF (STACK_POINTER().NE.0) THEN
          CALL SCL_UNWIND (1)
        END IF
        EXECUTE = .TRUE.

*----------------------
*   SET-ERROR-RETURN: Say what level of error should be trapped
*----------------------

      ELSE IF (COMMAND.EQ.'SET-ERROR-RETURN') THEN
        CALL GEN_GETSTR2 (1, 'Level to trap errors at? (I/W/E/F)',
     &                    'W', ' ', ERR_CHAR, ISTAT)
        CALL UUCASE      (ERR_CHAR)

        I = 4
        DO WHILE (ERR_CHAR.NE.ERRORS(I:I) .AND. I.GT.0)
          I = I - 1
        END DO
        MAX_OK_ERROR = MAX (I-1, 0)

*----------------------
*   $:  Send a command string to VMS
*----------------------

      ELSE IF (COMMAND.EQ.'$') THEN
        CALL GEN_GETSTR2 (3, 'Shell command line?', ' ',
     &                       ' ', STRING, ISTAT)
        ILS = GEN_ILEN(STRING)
        CALL GEN_HDNORM (STRING, STRING, ILS, IERR)
        CALL COMND (STRING(:ILS))

*----------------------
*   @:  Invoke an indirect command file
*----------------------

      ELSE IF (COMMAND.EQ.'@') THEN
        CALL GEN_GETSTR2 (1, 'Command file to run? (no extension)',
     &                       ' ', ' ', STRING, ISTAT)
        IF (ISTAT.EQ.0) THEN
          CALL PARSEAT(STRING)
          CALL GEN_AT (STRING(:GEN_ILEN(STRING))//'.spx', IERR)
          IF (IERR.NE.0) IERR = 84  ! @ command error
        ELSE
          IERR = 18
        END IF

*----------------------
*   :=  Assign a string to a command symbol
*----------------------

      ELSE IF (COMMAND.EQ.':=') THEN
        CALL GEN_GETSTR2 (1, 'Symbol name?', ' ', ' ', SYMBOL, ISTAT)
        CALL GEN_GETSTR2 (3, 'Equivalence string?', ' ', ' ', STRING,
     &                    ISTAT)
        IF (ISTAT.EQ.0) THEN
          CALL GEN_HDNORM     (STRING, STRING, ILS, IERR)
          CALL SPECX_CRSYMBOL (SYMBOL(:GEN_ILEN(SYMBOL)),
     &                         STRING(:ILS), IERR)
        ELSE
          IERR = 18
        END IF

*----------------------
*   =   Assign a value to a variable
*----------------------

      ELSE IF (COMMAND.EQ.'=') THEN
        CALL GEN_GETSTR2 (1, 'Variable name?', ' ', ' ', SYMBOL, ISTAT)
        CALL GEN_GETSTR2 (3, 'Value or expression?', ' ', ' ', STRING,
     &                    ISTAT)
        IF (ISTAT.EQ.0) THEN

          ILS    = GEN_ILEN (STRING)
*         IF (STRING_TEST(STRING, ILS, IERR)) THEN
*           STRING = STRING(:ILS) // ' '
*         ELSE
*           CALL GEN_EVAL_STR (STRING, STRING, IERR)
*           ILS = GEN_ILEN (STRING)
*         END IF

          CALL SPECX_SET_VALUE (SYMBOL(:GEN_ILEN(SYMBOL)),
     &                          STRING(:ILS), IERR)
*       To retain previous behaviour now that SPECX_SET_VALUE correctly
*       returns IERR - prevents SPECX error message
!          IERR = 0
        ELSE
          IERR = 18
        END IF

*----------------------
*   LABEL: Enter the label position for later lookup
*----------------------

      ELSE IF (COMMAND.EQ.'LABEL') THEN

*----------------------
*   GO-TO: Spool forward/backward to an accessible label
*----------------------

      ELSE IF (COMMAND.EQ.'GO-TO') THEN

*----------------------
*   Other?
*----------------------

      ELSE
        IERR = 81      ! Command not found. Note that this is a "good" status
                       ! for this routine, as it allows DO_COMMAND to execute.

      END IF

* ---- VMS only ------
*     CALL LIB$REVERT ()
* --------------------

      RETURN
      END

*-----------------------------------------------------------------------
