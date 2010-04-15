	SUBROUTINE CUT_LINSETC( X1, X2, Y1, Y2, MAXP, LINDAT, NPTS, STATUS)

* Description : Calculates the coordinates of points at equal spacing of one
*               pixel, along a column of pixels

	IMPLICIT NONE

	INTEGER
     :	  X1,
     :	  X2,
     :	  Y1,
     :	  Y2,
     :	  MAXP,
     :	  NPTS,
     :	  STATUS,
     :	  I,
     :	  J,
     :	  STEP

	REAL
     :	  LINDAT( MAXP, 2)

* set the number of pixels and line data array to the column pixel numbers

	NPTS = ABS( Y2 - Y1) + 1

* if Y1 is greater than Y2 then set step to -1 otherwise set step to 1

	IF( Y1 .GT. Y2) THEN

	  STEP = -1

	ELSE

	  STEP = 1

	END IF

* loop to set the index of the pixel in the column

	J = 0

	DO I = Y1, Y2, STEP

	  J = J + 1

	  LINDAT( MIN( J, MAXP), 1) = X1

	  LINDAT( MIN( J, MAXP), 2) = I

	END DO

	END
