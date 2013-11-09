*+  STR_ITEM - Gets next item from a list with specified delimiter.
      SUBROUTINE STR_ITEM(STRING, DELIM, IPOS, ITEM)
* Description :
*     <description of what the subroutine does>
* History :
*      Author	Clive Page	1986 July 25.
*      11 MaY 1988     New header   (LTVAD::RDS)
* Type Definitions :
      IMPLICIT NONE
* Import :
	CHARACTER*(*) STRING	!input: string containing list of items.
	CHARACTER*(*) DELIM	!input: delimiter (normally 1 char e.g. comma).
* Import-Export :
	INTEGER IPOS		!in/out: on first entry points to first char of
				!	string, returned pointing to first
				!	char after delimiter. If no more items
				!	to find, returns value >LEN(STRING).
* Export :
	CHARACTER*(*) ITEM	!output: next item found in the list, if nothing
				!	returns blanks.
* Local constants :
*     <local constants defined by PARAMETER>
* Local variables :
        INTEGER I,LPOS
*-

	IF(IPOS .GT. LEN(STRING)) THEN
		ITEM = ' '
	ELSE
		I = INDEX(STRING(IPOS:), DELIM)
*Get LPOS pointing to last valid character of item before delimiter/end
		IF(I .EQ. 0) THEN
			LPOS = LEN(STRING)
		ELSE
			LPOS = IPOS + I - 2
		END IF
		ITEM = STRING(IPOS:LPOS)
		IPOS = LPOS + LEN(DELIM) + 1
	END IF
*
	END
