	SUBROUTINE COLCYCLESUB( DIMS, INPVAL, OUTVAL, NCYCLES, STATUS)

	IMPLICIT  NONE              ! no implicit typing allowed

	INCLUDE  'SAE_PAR'          ! SSE global definitions

	INTEGER  STATUS             ! global status parameter

	INTEGER
     :    DIMS( 2 ),      ! dimensions of input DATA_ARRAY
     :	  I,
     :	  J,
     :	  K,
     :    L,
     :    NBIN,
     :    NCYCLES         ! number of cycles

	REAL
     :    INPVAL( 3, 256),  ! colour values
     :	  junk( 3, 256),
     :    OUTVAL( 3, 256)   ! colour values

*      check status on entry - return if not o.k.

	IF ( STATUS .NE. SAI__OK ) THEN

	  RETURN

	END IF

	IF( NCYCLES .LT. 1) NCYCLES = 1

	NBIN = 256/NCYCLES

	IF( NCYCLES .LT. 1) NCYCLES = 1

	NBIN = 256/NCYCLES

!	type *,'Ncycles Nbins = ', ncycles, nbin

!	do j = 1, 10
!	  type *, inpval( 1, j), inpval( 2, j), inpval( 3, j)
!	end do

	DO I = 1, NBIN

	  DO K = 1, 3

	    JUNK( K, I) = INPVAL( K, ( I*NCYCLES-(NCYCLES-1)))

	  END DO

	END DO

!	do j = 1, 10
!	  type *, junk( 1, j), junk( 2, j), junk( 3, j)
!	end do

	L = 0

	DO J = 1, 256

	  L = L + 1

	  IF( L .GT. NBIN) L = 1

	  DO K = 1, 3

	    OUTVAL( K, J) = JUNK( K, L)

	  END DO

	END DO

	IF( NBIN*NCYCLES .LT. 256) THEN

	  DO J = NBIN*NCYCLES, 256

	    DO K = 1, 3

	      OUTVAL( K, J) = OUTVAL( K, NBIN*NCYCLES-1)

	    END DO

	  END DO

	END IF

	END
