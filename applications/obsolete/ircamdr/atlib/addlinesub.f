	SUBROUTINE ADDLINESUB( DIMSX, DIMSY, ARRIN, ODIMSX, ODIMSY, ARROUT,
     :	                       COLORROW, LINENUM, LINEVAL, STATUS)

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	INTEGER
     :	  DIMSX,
     :	  DIMSY,
     :	  ODIMSX,
     :	  ODIMSY,
     :	  STATUS,
     :	  J,
     :	  K,
     :	  LINENUM

	REAL
     :	  ARROUT( ODIMSX, ODIMSY),
     :	  ARRIN( DIMSX, DIMSY),
     :	  LINEVAL

	CHARACTER*(*)
     :	  COLORROW

*      put column or row identifer to upper case and test
	CALL CHR_UCASE( COLORROW)
	IF( COLORROW( 1:1) .EQ. 'R') THEN
	  COLORROW = 'R'
	ELSE
	  COLORROW = 'C'
	END IF

*      scan through all the input pixels in Y ...
	DO J = 1, DIMSY

*        tell user which row is being processed ...
	  IF( IFIX( J/20.0+0.5)*20 .EQ. J) THEN
	    CALL MSG_SETI( 'J', J)
	    CALL MSG_OUT( 'MESSAGE', 'Processing row ^J ...', STATUS)
	  END IF

*        scan through all pixels in X
	  DO K = 1, DIMSX
	    IF( COLORROW( 1:1) .EQ. 'R') THEN
	      IF( LINENUM .EQ. J) THEN
	        ARROUT( K, J) = LINEVAL
	      ELSE
	        ARROUT( K, J) = ARRIN( K, J)
	      END IF
	    ELSE
	      IF( LINENUM .EQ. K) THEN
	        ARROUT( K, J) = LINEVAL
	      ELSE
	        ARROUT( K, J) = ARRIN( K, J)
	      END IF
	    END IF
	  END DO
	END DO

	END
