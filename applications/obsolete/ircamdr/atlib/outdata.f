	SUBROUTINE OUTDATA( ARRIN, ARROUT, NX, NY)

* Type definitions :

	IMPLICIT  NONE			! no default typing allowed

* Local variables :

	INTEGER NX, NY, J, K

	REAL ARRIN( NX, NY), ARROUT( NX, NY)

	DO J = 1, NY

	  DO K = 1, NX

	    ARROUT( K, J) = ARRIN( K, J)

	  END DO

	END DO

	END
