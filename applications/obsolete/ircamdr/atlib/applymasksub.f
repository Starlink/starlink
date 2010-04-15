	SUBROUTINE APPLYMASKSUB( DIMSX, DIMSY, ARRIN, ARRIN2, ARROUT,
     :	                        MAGICNO, NUMPIX, STATUS)

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	INTEGER
     :	  DIMSX,
     :	  DIMSY,
     :	  STATUS,
     :	  J,
     :	  K,
     :	  NUMPIX

	REAL
     :	  ARRIN( DIMSX, DIMSY),
     :	  ARRIN2( DIMSX, DIMSY),
     :	  ARROUT( DIMSX, DIMSY),
     :	  MAGICNO

*      initialize the number of bad pixels found
	NUMPIX = 0

*      scan image
	DO J = 1, DIMSY
	  DO K = 1, DIMSX
	    IF( ARRIN2( K, J) .GT. 0.999 .AND.
     :	      ARRIN2( K, J) .LT. 1.001) THEN
	      NUMPIX = NUMPIX + 1
	      ARROUT( K, J) = MAGICNO
	    ELSE
	      ARROUT( K, J) = ARRIN( K, J)
	    END IF
	  END DO
	END DO

	END
