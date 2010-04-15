	SUBROUTINE CUT_MAXMIN( CUT_SCALING, NUMBER_POINTS, MAXP, LINE_DATA,
     :	                       CUT_VALUES, X_LO, X_HI, Y_LO, Y_HI,
     :	                       CUT_POINTER)

* Calculates the maximum and minimum values or plotting as cut

	IMPLICIT NONE

	INCLUDE 'PLT2DCOM'

	INTEGER
     :	  J,
     :	  NUMBER_POINTS,
     :	  MAXP,
     :	  STATUS

	REAL
     :	  CUT_MEAN,
     :	  CUT_RANGE,
     :	  CUT_VALUES( MAXP),
     :	  CVMAX,
     :	  CVMIN,
     :	  LINE_DATA( MAXP, 2),
     :	  LXEXT,
     :	  LXMAX,
     :	  LXMIN,
     :	  LYEXT,
     :	  LYMAX,
     :	  LYMIN,
     :	  TOLERANCE,
     :	  X_HI,
     :	  X_LO,
     :	  Y_HI,
     :	  Y_LO

	PARAMETER ( TOLERANCE = 1.0E-10)

	CHARACTER
     :	  CUT_POINTER*1,
     :	  CUT_SCALING*1

* check whether scaling indicated range plot and get range if it did

	IF( CUT_SCALING .EQ. 'R') THEN

	  CALL PAR_GET0R( 'CUT_RANGE', CUT_RANGE, STATUS)

	END IF

* define limits of X,Y and CV maximum, minimum values

	LXMAX = -1.0E20
	LXMIN =  1.0E20
	LYMAX = -1.0E20
	LYMIN =  1.0E20
	CVMAX = -1.0E20
	CVMIN =  1.0E20

* loops to scan through LINE_DATA X

	DO J = 1, MIN( NUMBER_POINTS, MAXP)

* tests for maximum and minimum

	    LXMAX = MAX( LXMAX, LINE_DATA( J, 1))
	    LXMIN = MIN( LXMIN, LINE_DATA( J, 1))

	END DO

* loops to scan through LINE_DATA Y

	DO J = 1, MIN( NUMBER_POINTS, MAXP)

* tests for maximum and minimum

	  LYMAX = MAX( LYMAX, LINE_DATA( J, 2))
	  LYMIN = MIN( LYMIN, LINE_DATA( J, 2))

	END DO

* test which of X and Y is of longest extent and use as cut X values

	LXEXT = ABS( LXMAX - LXMIN)
	LYEXT = ABS( LYMAX - LYMIN)

	IF( LXEXT .GE. LYEXT) THEN

	  X_LO = LXMIN
	  X_HI = LXMAX

	  CUT_POINTER = 'X'

	ELSE

	  X_LO = LYMIN
	  X_HI = LYMAX

	  CUT_POINTER = 'Y'

	END IF

* test if the max,min values are the same and act on this

	IF( ABS( X_HI-X_LO) .LT. TOLERANCE) THEN

	  X_HI = 1.0
	  X_LO = 0.0

	END IF

* if cut scaling is range user range for Y hi lo values

	IF( CUT_SCALING .EQ. 'R') THEN

* calculate the mean of the pixels

	  CUT_MEAN = 0.0

	  DO J = 1, MIN( NUMBER_POINTS, MAXP)

	    CUT_MEAN = CUT_MEAN + CUT_VALUES( J)

	  END DO

	  IF( NUMBER_POINTS .NE. 0) THEN

	    CUT_MEAN = CUT_MEAN/NUMBER_POINTS

	  ELSE

	    CUT_MEAN = 0.0

	  END IF

* calculate te ranged max,min for display

	  Y_HI = CUT_MEAN + CUT_RANGE

	  Y_LO = CUT_MEAN - CUT_RANGE

	ELSE

* loops to scan through CUT_VALUES

	  DO J = 1, MIN( NUMBER_POINTS, MAXP)

* tests for maximum and minimum

	    CVMAX = MAX( CVMAX, CUT_VALUES( J))
	    CVMIN = MIN( CVMIN, CUT_VALUES( J))

	  END DO

* puts maximum and minimum Cut values into real variables

	  Y_LO = CVMIN
	  Y_HI = CVMAX

	END IF

	IF( ABS( Y_HI - Y_LO) .LT. TOLERANCE) THEN

	  Y_HI = 1.0
	  Y_LO = 0.0

	END IF

	END
