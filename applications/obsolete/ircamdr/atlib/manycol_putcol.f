
	SUBROUTINE MANYCOL_PUTCOL( NCOLS, CURNUM, CURCOL, INPVAL)

	IMPLICIT  NONE              ! no implicit typing allowed

	INCLUDE  'SAE_PAR'          ! SSE global definitions

	INTEGER  STATUS             ! global status parameter

	INTEGER
     :	  CURNUM,
     :	  J,
     :	  K,
     :    NCOLS           ! number of cycles

	REAL
     :	  CURCOL( 3, 256),
     :    INPVAL( 3, 256, 10)  ! colour values

*      check status on entry - return if not o.k.
	STATUS = SAI__OK

	IF ( STATUS .NE. SAI__OK ) THEN

	  RETURN

	END IF

	DO J = 1, 256

	  DO K = 1, 3

	    INPVAL( K, J, CURNUM) = CURCOL( K, J)

	  END DO

	END DO

	END
