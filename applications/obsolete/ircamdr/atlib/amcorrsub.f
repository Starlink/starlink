	SUBROUTINE AMCORRSUB( DIMSX, DIMSY, ARRIN, ODIMSX, ODIMSY,
     :	                      ARROUT, EXTVAL, AIRMASS, MAGICNO,
     :	                      FACTOR, STATUS)

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	INTEGER
     :	  DIMSX,
     :	  DIMSY,
     :	  ODIMSX,
     :	  ODIMSY,
     :	  STATUS,
     :	  J,
     :	  K

	REAL
     :	  ARROUT( ODIMSX, ODIMSY),
     :	  ARRIN( DIMSX, DIMSY),
     :	  EXTVAL,
     :	  AIRMASS,
     :	  FACTOR,
     :	  MAGICNO

*      calculate the correction factor from airmass

	FACTOR = 10**(AIRMASS*EXTVAL/2.5)

*      scan through all the input pixels at start of image ...

	DO J = 1, ODIMSY
	  DO K = 1, ODIMSX
	    IF( ARRIN( K, J) .NE. MAGICNO) THEN
	      ARROUT( K, J) = ARRIN( K, J)*FACTOR
	    ELSE
	      ARROUT( K, J) = ARRIN( K, J)
	    END IF
	  END DO
	END DO

	END
