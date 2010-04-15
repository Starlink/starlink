	SUBROUTINE BLOCK_TICKS( XSTC, YSTC, XENC, YENC, BLOCK_MAGNIF,
     :	                        CORIENT, N1, N2, STATUS)

	IMPLICIT NONE

	INCLUDE 'PLT2DCOM'

* define local variables

	INTEGER BLOCK_MAGNIF
	INTEGER J
	INTEGER N1
	INTEGER N2
	INTEGER NUMTICKS /5/
	INTEGER STATUS

	REAL X1
	REAL X2
	REAL XSTC
	REAL XENC
	REAL Y1
	REAL Y2
	REAL YSTC
	REAL YENC

	CHARACTER*( *) CORIENT

* test which way around the block is 'H' or 'V'

	IF( CORIENT .EQ. 'H' .OR. CORIENT .EQ. 'HORIZONTAL') THEN

* loop for X axis tick marks

	  DO J = 1, NUMTICKS

* plot bottom X axis tick marks

	    X1 = XSTC + N1*0.25*BLOCK_MAGNIF*( J - 1)
	    Y1 = YSTC
	    Y2 = YSTC + N2*BLOCK_MAGNIF

	    CALL SGS_LINE( X1, Y1, X1, Y2)

	  END DO

	ELSE

* loop for Y axis tick marks

	  DO J = 1, NUMTICKS

* plot left Y axis tick marks

	    X1 = XSTC
	    Y1 = YSTC + N2*0.25*BLOCK_MAGNIF*( J - 1)
	    X2 = XSTC + N1*BLOCK_MAGNIF

	    CALL SGS_LINE( X1, Y1, X2, Y1)

	  END DO

	END IF

* empty plot buffer of remaining graphics

	CALL SGS_FLUSH

	END
