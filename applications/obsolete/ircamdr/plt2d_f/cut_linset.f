	SUBROUTINE CUT_LINSET( X1, X2, Y1, Y2, MAXP, LINDAT, NPTS, STATUS)

* Description : Calculates the coordinates of points at equal spacing of one
*               pixel, along a line joining two given points.

	IMPLICIT NONE

	INTEGER
     :	  NPTS,
     :	  X1,
     :	  X2,
     :	  Y1,
     :	  Y2,
     :	  I,
     :	  MAXP,
     :	  STATUS

	REAL
     :	  RX1,
     :	  RY1,
     :	  RX2,
     :	  RY2,
     :	  LINDAT( MAXP, 2),
     :	  SEP,
     :	  THETA,
     :	  CT,
     :	  ST,
     :	  RAD,
     :	  TOLERANCE

	PARAMETER ( RAD = 57.29578)
	PARAMETER ( TOLERANCE = 1.0E-19)

* float the integer indexes of the cut to real numbers

	RX1 = FLOAT( X1)
	RY1 = FLOAT( Y1)
	RX2 = FLOAT( X2)
	RY2 = FLOAT( Y2)

* calculate the x,y positions of points at radii of integral pixel spacings
* from the first point

	IF( ( ABS( RY2-RY1) .LT. TOLERANCE) .AND.
     :	    ( ABS( RX2-RX1) .LT. TOLERANCE)) THEN

	   NPTS = 1

	   LINDAT( 1, 1) = RX1
	   LINDAT( 1, 2) = RY1

	ELSE

	   THETA = ATAN2( RY2-RY1, RX2-RX1)

	   CT = COS( THETA)
	   ST = SIN( THETA)

	   SEP = SQRT( ( RX2-RX1)**2 + ( RY2-RY1)**2)

	   NPTS = NINT( SEP) + 1

	END IF

	LINDAT( 1, 1) = RX1
	LINDAT( 1, 2) = RY1

	LINDAT( NPTS+1, 1) = RX2
	LINDAT( NPTS+1, 2) = RY2

	DO I = 1, MIN( MAXP, ( NPTS-1))

	  LINDAT( I+1, 1) = I*CT + X1
	  LINDAT( I+1, 2) = I*ST + Y1

	END DO

	END
