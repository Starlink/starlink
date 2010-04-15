*+  COPYAB - puts the odd and even columns of data in an image into separate
* images

	SUBROUTINE COPYAB( IDIMS1, IDIMS2, ARRAY_IN, ODIMS1, ODIMS2,
     :                     ARRAY_A, ARRAY_B, STATUS)

* Description :
*
* This routine ...
*
* Invocation : 	SUBROUTINE COPYAB( IDIMS, ARRAY_IN, ODIMS, ARRAY_A, ARRAY_B,
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
*  23-04-1986 :  First implementation (REVA::CAA)
*  11-AUG-1994   Changed DIMS input so that routine will compile (SKL@JACH)
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
     :  IDIMS1,  ! dimensions of input DATA_ARRAY
     :  IDIMS2,  ! dimensions of input DATA_ARRAY
     :  ODIMS1,  ! dimensions of output DATA_ARRAY
     :  ODIMS2,  ! dimensions of output DATA_ARRAY
     :	I,	     ! counter for array element number
     :	K,           ! counter for array element number
     :	L            ! counter for array element number

	REAL
     :	ARRAY_IN( IDIMS1, IDIMS2),   ! input data image
     :	ARRAY_A( ODIMS1, ODIMS2),    ! output A channel data image
     :	ARRAY_B( ODIMS1, ODIMS2)     ! output B channel data image

*-
*    check status on entry - return if not o.k.
*
	IF ( STATUS .NE. SAI__OK ) THEN
	   RETURN
	END IF
*
* loop to scan through all rows of input data putting each into the output
* arrays
*
	DO K = 1, MIN( IDIMS2, ODIMS2)
*
* reset the output column counter to start column 1
*
	  I = 1
*
* loop to scan through each column of input array and set output arrays
*
	  DO L = 1, IDIMS1, 2

	    ARRAY_A( I, K) = ARRAY_IN( L, K)
*
* increment column counting variable
*
	    I = I + 1
*
* check that element of output array inside limits of array size
*
	    IF( I .GT. ODIMS1+1) RETURN
	  END DO
	END DO
*
* loop to scan through all rows of input data putting each into the output
* arrays
*
	DO K = 1, MIN( IDIMS2, ODIMS2)
*
* reset the output column counter to start column 1
*
	  I = 1
*
* loop to scan through each column of input array and set output arrays
*
	  DO L = 2, IDIMS1, 2

	    ARRAY_B( I, K) = ARRAY_IN( L, K)
*
* increment column counting variable
*
	    I = I + 1
*
* check that element of output array inside limits of array size
*
	    IF( I .GT. ODIMS1+1) RETURN
	  END DO
	END DO

	END
