
	SUBROUTINE TOEQSUB( NX, NY, ARRINP, ARRINT, ARROUTT, TH, STATUS)

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	INTEGER NX, NY, STATUS
	INTEGER I, J

	REAL ARRINP( NX, NY), ARRINT( NX, NY), TH
	REAL ARROUTT( NX, NY)
	REAL POLMIN
	PARAMETER( POLMIN = 0.01)

*      Loops to scan through array rotating theta
	DO I = 1, NY
	  DO J = 1, NX

*          Test if p is greater than minimum p considered
	    IF( ARRINP( J, I) .GT. POLMIN) THEN

*            Add correction to input theta and set output theta
	      ARROUTT( J, I) = ARRINT( J, I) + TH

*            Test if output theta is beterrn 0 and 180 and correct
	      IF( ARROUTT( J, I) .LT. 0.0) THEN
	        ARROUTT( J, I) = ARROUTT( J, I) + 180.0
	      ELSE IF( ARROUTT( J, I) .GT. 180.0) THEN
	        ARROUTT( J, I) = ARROUTT( J, I) - 180.0
	      ELSE
	        ARROUTT( J, I) = ARROUTT( J, I)
	      END IF

	    ELSE

*            If polarization below minimum then set theta to 0
	      ARROUTT( J, I) = 0.0

	    END IF

	  END DO

	END DO

	END
