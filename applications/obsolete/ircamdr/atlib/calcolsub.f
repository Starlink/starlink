	SUBROUTINE CALCOLSUB( DIMSX, DIMSY, ARRIN1, ARRIN2, ARROUT,
     :	                      LSB1, LSB2, PLATESCALE, STATUS)

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	INTEGER
     :	  DIMSX,
     :	  DIMSY,
     :	  STATUS,
     :	  J,
     :	  K

	REAL
     :	  ARROUT( DIMSX, DIMSY),
     :	  ARRIN1( DIMSX, DIMSY),
     :	  ARRIN2( DIMSX, DIMSY),
     :	  LSB1,
     :	  LSB2,
     :	  PLATESCALE

*      scan through all the input pixels in Y ...
	DO J = 1, DIMSY

*        scan through all the input pixels in X ...
	  DO K = 1, DIMSX

	    IF( ARRIN1( K, J) .LE. LSB1 .AND.
     :	        ARRIN2( K, J) .LE. LSB2) THEN

	      ARROUT( K, J) = ARRIN1( K, J) - ARRIN2( K, J)

	    ELSE

	      ARROUT( K, J) = 0.0

	    END IF

	  END DO

	END DO

	END
