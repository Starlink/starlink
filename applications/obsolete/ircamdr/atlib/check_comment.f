	SUBROUTINE CHECK_COMMENT( DATALINE, IS_COMMENT)

	IMPLICIT NONE

	INTEGER LENLINE, J, ICHAR_DATALINE

	CHARACTER*( *) DATALINE

	LOGICAL IS_COMMENT

*      Get dataline length
	LENLINE = LEN( DATALINE)

*      Initialize comment variable
	IS_COMMENT = .FALSE.

*      Scan the line for non-numeric characters
	DO J = 1, LENLINE

*        Get the ASCII code for current character in dataline
	  ICHAR_DATALINE = ICHAR( DATALINE( J:J))

	  IF( ( ICHAR_DATALINE .LT. 48 .OR.       ! between 0 and 9
     :	        ICHAR_DATALINE .GT. 57) .AND.     !     ditto
     :	      ( ICHAR_DATALINE .NE. 43 .AND.      ! + sign
     :	        ICHAR_DATALINE .NE. 45 .AND.      ! - sign
     :	        ICHAR_DATALINE .NE. 46 .AND.      ! .
     :	        ICHAR_DATALINE .NE. 9  .AND.      ! tab
     :	        ICHAR_DATALINE .NE. 32 .AND.      ! space
     :	        ICHAR_DATALINE .NE. 69 .AND.      ! E for exponential
     :	        ICHAR_DATALINE .NE. 101)) THEN    ! e for exponential

	    IS_COMMENT = .TRUE.

	    GOTO 10

	  END IF

	END DO

  10	CONTINUE

	END
