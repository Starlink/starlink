*+  THETAFIXSUB -

	SUBROUTINE THETAFIXSUB( IDIMS1, IDIMS2, ARRAY_IN, ODIMS1, ODIMS2,
     :                          ARRAY_OUT, STATUS)

* Description :
*
* This routine ...
*
* Invocation : 	SUBROUTINE THETAFIXSUB( IDIMS, ARRAY_IN, ODIMS, ARRAY_OUT,
*                                  STATUS)
*
* Parameters :
*
* Method :
*
* Bugs :
*
* None known.
*
* Authors : Colin Aspin ROE ( REVA::CAA )
*
* History :
*
*  15-05-1987 :  First implementation (REVA::CAA)
*  11-AUG-1994   Changed DIM arguments so that routine wil compile (SKL@JACH)
*
* Type definitions :

	IMPLICIT  NONE		  ! no implicit typing allowed

* Global constants :

	INCLUDE  'SAE_PAR'	    ! SSE global definitions

* Status :

	INTEGER  STATUS		 ! global status parameter

* Local constants :

* Local variables :

	INTEGER
     :  IDIMS1, IDIMS2,  ! dimensions of input DATA_ARRAY
     :  ODIMS1, ODIMS2,  ! dimensions of output DATA_ARRAY
     :	I,	     ! counter for array element number
     :	K            ! counter for array element number

	REAL
     :	ARRAY_IN( IDIMS1, IDIMS2),   ! input data image
     :	ARRAY_OUT( ODIMS1, ODIMS2)   ! output data image

*-
*    check status on entry - return if not o.k.
*
	IF ( STATUS .NE. SAI__OK ) THEN
	   RETURN
	END IF
*
* loop to scan through all rows and columns of input data
*
	DO K = 1, ODIMS2

	  DO I = 1, ODIMS1

	    IF( ARRAY_IN( I, K) .LT. 0.0) THEN

	      ARRAY_OUT( I, K) = ARRAY_IN( I, K) + 180.0

	    ELSE IF( ARRAY_IN( I, K) .GT. 180.0) THEN

	      ARRAY_OUT( I, K) = ARRAY_IN( I, K) - 180.0

	    ELSE

	      ARRAY_OUT( I, K) = ARRAY_IN( I, K)

	    END IF

	  END DO

	END DO

	END
