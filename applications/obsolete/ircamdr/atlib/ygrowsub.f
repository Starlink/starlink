	SUBROUTINE YGROWSUB( DIMS1, DIMS2, ARRIN, ODIMS1, ODIMS2,
     :                       ARROUT, STATUS)

*  HISTORY
*    11-Aug-1994  Changed DIM arguments so that routine will compile (SKL@JACH)
*

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	INTEGER
     :	  DIMS1, DIMS2,
     :	  ODIMS1, ODIMS2,
     :	  STATUS,
     :	  J,
     :	  K

	REAL
     :	  ARROUT( ODIMS1, ODIMS2),
     :	  ARRIN( DIMS1, DIMS2)

*      scan through all the input pixels ...

	DO J = 1, ODIMS2

	  DO K = 1, ODIMS1

*          form the array with the input pixel values

	    ARROUT( K, J) = ARRIN( K, DIMS2)

	  END DO

	END DO

	END
