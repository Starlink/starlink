

*+  COPYAB_COMB - puts the odd and even columns of data in two images into
*                 one image

	SUBROUTINE COPYAB_COMB( IDIMS1, IDIMS2, ARRAY_INA, ARRAY_INB,
     :	                        ODIMS1, ODIMS2, ARRAY_OUT, STATUS)

* Description :
*
* This routine ...
*
* Invocation :
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
     :	J,	     ! counter for array element number
     :	KA,           ! counter for array element number
     :	KB,           ! counter for array element number
     :	LA,           ! counter for array element number
     :	LB            ! counter for array element number

	REAL
     :	ARRAY_INA( IDIMS1, IDIMS2),   ! input data image
     :	ARRAY_INB( IDIMS1, IDIMS2),   ! input data image
     :	ARRAY_OUT( ODIMS1, ODIMS2)    ! output A channel data image

*-
*      check status on entry - return if not o.k.
*
	IF ( STATUS .NE. SAI__OK ) THEN
	   RETURN
	END IF
*
*      loop to scan through all rows of output data putting each of the input
*      arrays into it
*
	LA = 1
	LB = 1

	DO I = 1, ODIMS2

	  KA = 1
	  KB = 1

	  DO J = 1, ODIMS1

	    IF( IFIX( J/2.0 + 0.5)*2 .EQ. J) THEN

	      IF( KB .GE. 1 .AND. KB .LE. IDIMS1 .AND.
     :	          LB .GE. 1 .AND. LB .LE. IDIMS2) THEN

	        ARRAY_OUT( J, I) = ARRAY_INB( KB, LB)

	        KB = KB + 1

	      END IF

	    ELSE

	      IF( KA .GE. 1 .AND. KA .LE. IDIMS1 .AND.
     :	          LA .GE. 1 .AND. LA .LE. IDIMS2) THEN

	        ARRAY_OUT( J, I) = ARRAY_INA( KA, LA)

	        KA = KA + 1

	      END IF

	    END IF

	  END DO

	  LA = LA + 1
	  LB = LB + 1

	END DO

	END
