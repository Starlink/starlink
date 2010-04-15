
	SUBROUTINE RADIM_DATA( NX, NY, INARR, X1, Y1, X2, Y2, DATA, N)

	IMPLICIT NONE

	INTEGER N, J, I
	INTEGER NX, NY, IX1, IY1, IX2, IY2

	REAL INARR( NX, NY), DATA( 10000)
	REAL X1, Y1, X2, Y2, F, G, VAL1, VAL2, VAL3, VAL4
	REAL THETA, CT, ST, SEP, LINDAT( 10000, 2)

	CT = 0.0
	ST = 0.0

	IF( ( ABS( Y2-Y1) .LT. 1.0E-19) .AND.
     :	    ( ABS( X2-X1) .LT. 1.0E-19)) THEN

	   N = 1

	   LINDAT( 1, 1) = X1
	   LINDAT( 1, 2) = Y1

	ELSE

	   THETA = ATAN2( Y2-Y1, X2-X1)

	   CT = COS( THETA)
	   ST = SIN( THETA)

	   SEP = SQRT( ( X2-X1)**2 + ( Y2-Y1)**2)

	   N = NINT( SEP) + 1

	END IF

	LINDAT( 1, 1) = X1
	LINDAT( 1, 2) = Y1

	LINDAT( N+1, 1) = X2
	LINDAT( N+1, 2) = Y2

	DO J = 1, MIN( 10000, ( N-1))

	  LINDAT( J+1, 1) = J*CT + X1
	  LINDAT( J+1, 2) = J*ST + Y1

	END DO

	DO I = 1, MIN( 10000, ( N-1))

	    IX1 = LINDAT( I, 1)
	    IY1 = LINDAT( I, 2)
	    IX2 = IX1 + 1
	    IY2 = IY1 + 1

	    IF( IX1 .LT. 1) IX1 = 1
	    IF( IX1 .GT. NX) IX1 = NX
	    IF( IY1 .LT. 1) IY1 = 1
	    IF( IY1 .GT. NY) IY1 = NY
	    IF( IX2 .LT. 1) IX2 = 1
	    IF( IX2 .GT. NX) IX2 = NX
	    IF( IY2 .LT. 1) IY2 = 1
	    IF( IY2 .GT. NY) IY2 = NY
C
C get the four surrounding points in the data array
C
	    VAL1 = INARR( IX1, IY1)
	    VAL2 = INARR( IX2, IY1)
	    VAL3 = INARR( IX1, IY2)
	    VAL4 = INARR( IX2, IY2)
C
C f & g are the fractional pixel displacements of the
C interpolation point
C
	   F = LINDAT( I, 1) - IX1
	   G = LINDAT( I, 2) - IY1
C
C on exit, 'line' will contain the array of interpolated values
C ready for plotting. bilinear interpolation is used.
C
	   DATA( I) = F*( VAL2-VAL1) + F*G*( VAL1+VAL4-VAL2-VAL3)
     :		      + G*( VAL3-VAL1) + VAL1

	END DO

	END
