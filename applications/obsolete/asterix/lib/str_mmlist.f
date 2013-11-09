*+  STR_MMLIST - Decodes string containing list of integer values or ranges.
      SUBROUTINE STR_MMLIST(STRING, NELS, NOUT, MINMAX)
* Description :
*     <description of what the subroutine does>
* History :
*      Author	Clive Page	1987 Feb 2.
*      May 10 1988  Asterix88 version   (LTVAD::RDS)
* Type Definitions :
      IMPLICIT NONE
* Import :
	CHARACTER*(*) STRING	!input: e.g. '1-3, 5-6, 7, 25'
	INTEGER NELS		!input: number of min-max pairs acceptable.
* Export :
	INTEGER NOUT		!output: number of pairs actually set.
	INTEGER MINMAX(2,NELS)	!output: returns array of non-negative integers.
				! i=1 min of each range, i=2 max of each range.
* Local constants :
*     <local constants defined by PARAMETER>
* Local variables :
	LOGICAL NUMBER, MINUS
        INTEGER IVAL
        INTEGER I
*-
*
	IVAL = 0
	NOUT = 0
	NUMBER = .FALSE.
	MINUS  = .FALSE.
*
	DO I = 1,LEN(STRING)
	    IF(STRING(I:I) .GE. '0' .AND. STRING(I:I) .LE. '9') THEN
		IVAL = 10 * IVAL + ICHAR(STRING(I:I)) - ICHAR('0')
		NUMBER = .TRUE.
	    ELSE
		IF(NUMBER) THEN
		    IF(MINUS .AND. NOUT .GT. 0) THEN
			MINMAX(2, NOUT) = IVAL
		    ELSE IF(NOUT .LT. NELS) THEN
			NOUT = NOUT + 1
			MINMAX(1, NOUT) = IVAL
			MINMAX(2, NOUT) = IVAL
		    ELSE
			WRITE(*,*)'STR_MMLIST warning: excess input ignored'
			GO TO 999
		    END IF
		    IVAL = 0
		    MINUS = .FALSE.
		END IF
		IF(STRING(I:I) .EQ. '-') MINUS = .TRUE.
		NUMBER = .FALSE.
	    END IF
	END DO
999	CONTINUE
*
	END
