	SUBROUTINE CALCULATE_MAXMIN( DATA_ARRAY, XMAXIMUM, XMINIMUM)

* calculates the maximum and minimum values in an image
	IMPLICIT NONE

	INCLUDE 'PLT2DCOM'

	INTEGER J
	INTEGER K

	REAL DATA_ARRAY( NX, NY)
	REAL XMAXIMUM
	REAL XMINIMUM

* define limits of maximum, minimum values
	XMAXIMUM = -1.0E20
	XMINIMUM = 1.0E20

* loops to scan through data array
	DO J = 1, NY
	  DO K = 1, NX

* tests for maximum and minimum
	    IF( DATA_ARRAY( K, J) .NE. -1.0E-20) THEN
	      XMAXIMUM = MAX( XMAXIMUM, DATA_ARRAY( K, J))
	      XMINIMUM = MIN( XMINIMUM, DATA_ARRAY( K, J))
	    END IF
	  END DO
	END DO

* test if maximum and minimum are the same (within the tolerance) and if so
* then take plus and minus one tenth about theie value as the maximum and
* minimum to be used
	IF( ABS( XMAXIMUM - XMINIMUM) .LT. 0.0001) THEN
	  IF( XMAXIMUM .GE. 0.0) THEN
	    XMAXIMUM = XMAXIMUM*1.1
	  ELSE
	    XMAXIMUM = XMAXIMUM*0.9
	  END IF
	  IF( XMINIMUM .GE. 0.0) THEN
	    XMINIMUM = XMINIMUM*0.9
	  ELSE
	    XMINIMUM = XMINIMUM*1.1
	  END IF
	END IF

	END
