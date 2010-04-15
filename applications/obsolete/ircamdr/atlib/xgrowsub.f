	SUBROUTINE XGROWSUB( DIMSX, DIMSY, ARRIN, ODIMSX, ODIMSY, ARROUT,
     :	                     STATUS)

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	INTEGER
     :	  DIMSX,
     :	  DIMSY,
     :	  ODIMSX,
     ;	  ODIMSY,
     :	  STATUS,
     :	  J,
     :	  K

	REAL
     :	  ARROUT( ODIMSX, ODIMSY),
     :	  ARRIN( DIMSX, DIMSY)

*      scan through all the input pixels ...

	DO J = 1, ODIMSY

	  DO K = 1, ODIMSX

*          form the array with the input pixel values

	    ARROUT( K, J) = ARRIN( DIMSX, J)

	  END DO

	END DO

	END
