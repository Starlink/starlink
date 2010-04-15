
	SUBROUTINE WRAPCORSUB( DIMSX, DIMSY, ARRIN, ODIMSX, ODIMSY, ARROUT,
     :	                       DATAVAL, VALADD, STATUS)

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	INTEGER
     :	  DIMSX,
     :	  DIMSY,
     :	  ODIMSX,
     :	  ODIMSY,
     :	  STATUS,
     :	  J,
     :	  K,
     :	  NUMPIX

	REAL
     :	  ARROUT( ODIMSX, ODIMSY),
     :	  ARRIN( DIMSX, DIMSY),
     :	  DATAVAL,
     :	  VALADD

*      initialize the number of pixels below threshold variable
	NUMPIX = 0

*      scan through all the input pixels in Y ...
	DO J = 1, DIMSY

*        tell user which row is being processed ...
	  IF( IFIX( J/20.0+0.5)*20 .EQ. J) THEN

	    CALL MSG_SETI( 'J', J)
	    CALL MSG_OUT( 'MESSAGE', 'Processing row ^J ...', STATUS)

	  END IF

*        scan through all pixels in X
	  DO K = 1, DIMSX

*          test if pixel is above or below specified threshold
	    IF( ARRIN( K, J) .LT. DATAVAL) THEN

*            if below then add on value and increment number variable
	      ARROUT( K, J) = ARRIN( K, J) + VALADD
	      NUMPIX = NUMPIX + 1

	    ELSE

*            if above then set output value
	      ARROUT( K, J) = ARRIN( K, J)

	    END IF

	  END DO

	END DO

*      tell user how many pixels below specified threshold
	CALL MSG_SETR( 'VAL', DATAVAL)
	CALL MSG_SETI( 'NUM', NUMPIX)
	CALL MSG_OUT( 'MESS',
     :	  'Number of pixels below ^VAL in image = ^NUM', STATUS)

	END
