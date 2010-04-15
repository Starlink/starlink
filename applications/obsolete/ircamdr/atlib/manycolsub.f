	SUBROUTINE MANYCOLSUB( DIMS, INPVAL, OUTVAL, NCOLS, STATUS )

	IMPLICIT  NONE              ! no implicit typing allowed

	INCLUDE  'SAE_PAR'          ! SSE global definitions

	INTEGER  STATUS             ! global status parameter

	INTEGER
     :    DIMS( 2 ),      ! dimensions of input DATA_ARRAY
     :	  I,
     :	  J,
     :	  K,
     :    L,
     :	  N,
     :    NBIN,
     :    NCOLS           ! number of cycles

	REAL
     :    INPVAL( 3, 256, 10),  ! colour values
     :	  OUTVAL( 3, 256)

*      check status on entry - return if not o.k.

	IF ( STATUS .NE. SAI__OK ) THEN

	  RETURN

	END IF

	NBIN = 256/NCOLS

	L = 0

	DO N = 1, NCOLS

	  DO I = 1, NBIN

	    L = L + 1

	    DO K = 1, 3

	      OUTVAL( K, L) = INPVAL( K, ( I*NCOLS-(NCOLS-1)), N)

	    END DO

	  END DO

	END DO

	IF( NBIN*NCOLS .LT. 256) THEN

	  DO J = NBIN*NCOLS, 256

	    DO K = 1, 3

	      OUTVAL( K, J) = INPVAL( K, NBIN*NCOLS-1, NCOLS)

	    END DO

	  END DO

	END IF

	END
