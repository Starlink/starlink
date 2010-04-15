	SUBROUTINE YADDSUB( DIMS1, DIMS2, ARRIN, ODIMS1, ODIMS2,
     :                      ARROUT, YST, YSZ, AVER, STATUS)

* HISTORY
*   11-Aug-1994  Changed DIM arguments so that routine will compile (SKL@JACH)
*

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	INTEGER
     :	  DIMS1, DIMS2,
     :	  ODIMS1, ODIMS2,
     :	  STATUS,
     :	  YST,
     :    YSZ,
     :	  J,
     :	  K

	REAL
     :	  ARROUT( ODIMS1, ODIMS2 ),
     :	  ARRIN( DIMS1, DIMS2 )

	LOGICAL
     :	  AVER

*      set all output pixels to zero

	DO K = 1, ODIMS1

	  ARROUT( K, ODIMS2 ) = 0.0

	END DO

*      scan through all the rows to be added from input image ...

	DO K = 1, ODIMS1

	  DO J = YST, ( YST+YSZ-1)

*          form the array with the added input pixel values

	    ARROUT( K, ODIMS2) = ARROUT( K, ODIMS2) + ARRIN( K, J)

	  END DO

	  IF( AVER) THEN

	    ARROUT( K, ODIMS2) = ARROUT( K, ODIMS2)/( YST+YSZ-1)

	  END IF

	END DO

	END
