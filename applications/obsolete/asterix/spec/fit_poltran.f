*+ FIT_POLTRAN - Translates composite fit model string to inverse Polish
      SUBROUTINE FIT_POLTRAN(MODSPEC,POLISH,STATUS)
*
*    Description :
*
*     The character string specifying a composite fit model (cmodel)
*     e.g.   PL+AB*(BR-LI)
*     is translated into a reverse Polish string in which all models are
*     replaced by the symbol 'M'. The model is much more readily evaluated
*     from this Polish representation.
*     Note - no attempt is made to check the validity of the model keys.
*
*    Method :
*
*     The cmodel string is scanned from the left. A model key causes
*     immediate entry of an 'M' into the output string. All other symbols
*     are stacked, but any higher priority symbols on the stack are first
*     popped to the output string (except in the case of '(' which is always
*     stacked immediately). ')' causes the stack to be popped back to the
*     previous '('.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Trevor Ponman  (BHVAD::TJP)
*
*    History :
*
*     15 Jan 85 : Original (POLTRAN)
*     19 Mar 85 : Bug fix (BHVAD::TJP)
*     29 Jun 87 : Renamed to FIT_POLTRAN - no significant changes (TJP)
*     20 Jun 89 : ASTERIX88 version, some tidying and renaming (TJP)
*      9 Aug 93 : Model keys can now be of any length. Removed all the
*                 line numbers! (DJA)
*      2 Jun 94 : Explicitly reset the polish string (DJA)
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
      CHARACTER*(*) 		MODSPEC		! Cmodel specification string
*
*    Export :
*
      CHARACTER*(*)		POLISH		! Polish translation of MODSPEC
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      INTEGER 			CHR_LEN
      INTEGER FIT_POLTRAN_PRIOR			! Returns symbol priority
      LOGICAL 			CHR_ISALM       ! Character is alphanumeric?
*
*    Local constants :
*
      INTEGER                MAXSYMST		! Maximum size of symbol stack
	PARAMETER            ( MAXSYMST=30 )
*
*    Local variables :
*
      CHARACTER*1 SYM				! Symbol from MODSPEC
      CHARACTER*1 STACK(MAXSYMST)		! Symbol stack

      INTEGER IPRI				! Priority of current symbol
      INTEGER LSTR				! Total length of MODSPEC
      INTEGER NCHR				! No of characters processed
      INTEGER NPOL				! No of chars entered in POLISH
      INTEGER NSTACK				! No of entries in symbol stack
      INTEGER SPRI(MAXSYMST)			! Priorities of stacked symbols
*-

*    Status check
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Reset the output string
      CALL CHR_FILL( ' ', POLISH )

*    Initialise counters
      NCHR = 0
      NPOL = 0
      NSTACK = 0
      LSTR = CHR_LEN(MODSPEC)

*    Start of loop
      DO WHILE ( (NCHR .LT. LSTR) .AND. (STATUS.EQ.SAI__OK) )

*      Next character
        NCHR = NCHR + 1
	SYM = MODSPEC(NCHR:NCHR)
	IF ( INDEX('()+-*',SYM) .NE. 0 ) THEN

*        Processing operator or bracket
	  IPRI = FIT_POLTRAN_PRIOR(SYM)
	  IF(NSTACK.EQ.0.OR.SYM.EQ.'(')THEN
	    CALL FIT_POLTRAN_STACK(SYM,IPRI,NSTACK,STACK,SPRI)

	  ELSE IF ( SYM .EQ. ')' ) THEN

*          R.h. bracket - pop stack until l.h. bracket
            DO WHILE ( (STATUS.EQ.SAI__OK) .AND.
     :                 (STACK(NSTACK).NE.'(') )
	      CALL FIT_POLTRAN_POP(NSTACK,NPOL,STACK,POLISH,STATUS)
            END DO
	    IF ( STATUS .EQ. SAI__OK ) NSTACK = NSTACK - 1

	  ELSE

*          Operator
 30	    IF(IPRI.GT.SPRI(NSTACK))THEN
	      CALL FIT_POLTRAN_STACK(SYM,IPRI,NSTACK,STACK,SPRI)
	    ELSE
	      CALL FIT_POLTRAN_POP(NSTACK,NPOL,STACK,POLISH,STATUS)
	      IF(STATUS.NE.SAI__OK) GOTO 99
	      IF(NSTACK.EQ.0)THEN
	        CALL FIT_POLTRAN_STACK(SYM,IPRI,NSTACK,STACK,SPRI)
              ELSE
                GOTO 30
	      END IF
	    END IF

	  END IF

	ELSE

*        Processing model key
          DO WHILE ( (NCHR.LE.LSTR) .AND.
     :                 CHR_ISALM(MODSPEC(NCHR:NCHR)) )
	    NCHR = NCHR + 1
          END DO
	  NCHR = NCHR - 1
	  NPOL = NPOL + 1
	  POLISH(NPOL:NPOL) = 'M'

	END IF

      END DO

*    Exit sequence
      DO WHILE ( (NSTACK .GT. 0) .AND. (STATUS.EQ.SAI__OK) )
	CALL FIT_POLTRAN_POP( NSTACK, NPOL, STACK, POLISH, STATUS )
      END DO

*    Exit
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', '...from FIT_POLTRAN', STATUS )
      END IF

      END

*+  FIT_POLTRAN_POP - Pops an operator to string POLISH
      SUBROUTINE FIT_POLTRAN_POP(NSTACK,NPOL,STACK,POLISH,STATUS)
*    Description :
*     Decrements stack pointer and loads symbol and priority onto stacks STACK
*     and SPRI.
*    History :
*     20 Jun 89: ASTERIX88 version
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
	INCLUDE 'SAE_PAR'
*    Import :
*    Import-Export :
	INTEGER NSTACK			! Pointer to end of stack (incremented)
	INTEGER NPOL			! Pointer to end of POLISH string
	CHARACTER*1 STACK(*)		! Operator stack
	CHARACTER*80 POLISH		! Reverse Polish model spec string
*    Export :
*    Status :
	INTEGER STATUS
*    Local constants :
*    Local variables :
*-

* Status check
	IF(STATUS.NE.SAI__OK) RETURN

* Check for empty stack
	IF(NSTACK.LT.1)THEN
	  CALL ERR_REP('POPERR','Attempt to pop empty symbol stack',STATUS)
	  STATUS=SAI__ERROR
	  GO TO 999
	ENDIF

* Pop stack (operator entered in polish string)
	NPOL=NPOL+1
	POLISH(NPOL:NPOL)=STACK(NSTACK)
	NSTACK=NSTACK-1

* Exit
 999	IF(STATUS.NE.SAI__OK) CALL ERR_REP('EXERR','from FIT_POLTRAN_POP',
     :  STATUS)
	END

*+  FIT_POLTRAN_PRIOR - Evaluates the stacking priority of an operator
      INTEGER FUNCTION FIT_POLTRAN_PRIOR(SYM)
*    Description :
*     Operator priority 1 to 4 is assigned.
*    History :
*     20 Jun 89: ASTERIX88 version
*    Type definitions :
      IMPLICIT NONE
*    Import :
	CHARACTER*1 SYM			! Input symbol (arith operator)
*    Local constants :
*    Local variables :
*-
	IF(SYM.EQ.'(')THEN
	  FIT_POLTRAN_PRIOR=1
	ELSE IF(SYM.EQ.'+'.OR.SYM.EQ.'-')THEN
	  FIT_POLTRAN_PRIOR=2
	ELSE IF(SYM.EQ.'*')THEN
	  FIT_POLTRAN_PRIOR=3
	ELSE
	  FIT_POLTRAN_PRIOR=4
	ENDIF
	END

*+  FIT_POLTRAN_STACK - Stacks symbol (operator or bracket)
      SUBROUTINE FIT_POLTRAN_STACK(SYM,IPRI,NSTACK,STACK,SPRI)
*    Description :
*     Increments stack pointer and loads symbol and priority onto stacks STACK
*     and SPRI.
*    History :
*     20 Jun 89: ASTERIX88 version
*    Type definitions :
      IMPLICIT NONE
*    Import :
	CHARACTER*1 SYM			! Input symbol (arith operator)
	INTEGER IPRI			! Symbol priority
*    Import-Export :
	INTEGER NSTACK			! Pointer to end of stack (incremented)
	CHARACTER*1 STACK(*)		! Operator stack
	INTEGER SPRI(*)			! Stack of operator priorities
*    Export :
*    Local constants :
*    Local variables :
*-

	NSTACK=NSTACK+1
	STACK(NSTACK)=SYM
	SPRI(NSTACK)=IPRI
	END
