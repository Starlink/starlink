*+  FIND_NOT_BLANK
*	9 Apr 1992	M. Duesterhaus (GSFC)	ORIGINAL
**********************************************************************
      INTEGER FUNCTION FIND_NOT_BLANK( STRING1 )

*  -----------
*  DESCRIPTION
*  -----------

* finds first character in string1 that is not blank
*  ---------
*  VARIABLES
*  ---------

*INPUT:

      CHARACTER*(*) STRING1

*LOCAL:

      CHARACTER*132 STRING3	! Uppercase version of STRING1.
      INTEGER	    ST1,END1

      END1 = LEN(STRING1)
      STRING3 = STRING1
      FIND_NOT_BLANK = 0
      ST1=1
      DO WHILE (ST1.LE.END1)
	IF (STRING3(ST1:ST1) .NE. ' ')  THEN
          FIND_NOT_BLANK = ST1
          ST1 = END1 +1
        ELSE
          ST1 = ST1 +1
        END IF
      END DO
      END                                                                        ! Return field number.
