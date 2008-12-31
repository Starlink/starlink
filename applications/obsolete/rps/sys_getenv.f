*+SYS_GETENV	Get text from environment by name
	SUBROUTINE SYS_GETENV(NAME,TEXT,LENT,ISTAT)
	INTEGER LENT,ISTAT
	CHARACTER*(*) NAME,TEXT
*NAME	input	name of environment symbol/variable
*TEXT	output	text value returned
*LENT	output	number of characters returned
*ISTAT	in/out	returns status , 0 is OK
* SUNOS version
*-Author Dick Willngale 1991-Sep-2
	INTEGER LEN_TRIM
C
	IF(ISTAT.NE.0) RETURN
C
	CALL GETENV(NAME,TEXT)
	LENT=LEN_TRIM(TEXT)
	IF(LENT.EQ.0) THEN
		ISTAT=1
	ENDIF
	END
