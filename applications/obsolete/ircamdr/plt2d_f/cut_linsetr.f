	SUBROUTINE CUT_LINSETR( X1, X2, Y1, Y2, MAXP, LINDAT, NPTS, STATUS)

* Description : Calculates the coordinates of points at equal spacing of one
*               pixel, along a line joining two given points.

	IMPLICIT NONE

	INTEGER
     :	  NPTS,
     :	  X1,
     :	  X2,
     :	  Y1,
     :	  Y2,
     :	  MAXP,
     :	  I,
     :	  J,
     :	  STATUS,
     :	  STEP

	REAL
     :	  LINDAT( MAXP, 2)

* set the number of pixels and the line data to the row pixel numbers

	NPTS = ABS( X2 - X1) + 1

* set the step for the data loop depending on the direction of cut

	IF( X1 .GT. X2) THEN

	  STEP = -1

	ELSE

	  STEP = 1

	END IF

* loop to set the data index values

	J = 0

	DO I = X1, X2, STEP

	  J = J + 1

	  LINDAT( MIN( J, MAXP), 1) = I

	  LINDAT( MIN( J, MAXP), 2) = Y1

	END DO

	END
