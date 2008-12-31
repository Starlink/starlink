*+  FIND_FIRST
*	13 Apr 1992	M. Duesterhaus (GSFC)	ORIGINAL
**********************************************************************
      INTEGER FUNCTION FIND_FIRST ( STRING1,STRING2,SUBINDEX )

*  -----------
*  DESCRIPTION
*  -----------

* finds first character in string1 that matches a char in string2
*  ---------
*  VARIABLES
*  ---------

*INPUT:

      CHARACTER*(*) STRING1
      CHARACTER*(*) STRING2
      INTEGER	    SUBINDEX    ! Position in STRING2 of char that matched

*LOCAL:

      CHARACTER*132 STRING3	! LOCAL version of STRING1.
      CHARACTER*132 STRING4	! LOCAL version of STRING2.
      INTEGER	    END1,END2,I,J
      INTEGER	    FOUND


      STRING3 = STRING1
      STRING4 = STRING2
      END1= LEN(STRING1)
      END2= LEN(STRING2)

      SUBINDEX = 0
      FOUND = 0
      DO I=1,END1
	DO J=1,END2
	  IF (STRING3(I:I) .EQ. STRING4(J:J)) THEN
	    FOUND=I
	    SUBINDEX = J
	    GOTO 45
	  ENDIF
	END DO
      END DO
  45  FIND_FIRST = FOUND

      END                                                                        ! Return field number.
