	SUBROUTINE CUT_INTERPOLATE( NPTS, MAXP, LINDAT, LINE, NX, NY,
     :	                            DATAARRAY, STATUS)

	IMPLICIT NONE

	INTEGER
     :	  NPTS,
     :	  NX,
     :	  NY,
     :	  IX1,
     :	  IY1,
     :	  IX2,
     :	  IY2,
     :	  MAXP,
     :	  STATUS,
     :	  I

	REAL
     :	  LINDAT( MAXP, 2),
     :	  LINE( MAXP),
     :	  DATAARRAY( NX, NY),
     :	  VAL1,
     :	  VAL2,
     :	  VAL3,
     :	  VAL4,
     :	  F,
     :	  G

* interpolate the cut from the indices in lindat array

	DO I = 1, NPTS

	    IX1 = LINDAT( I, 1)
	    IY1 = LINDAT( I, 2)
	    IX2 = IX1 + 1
	    IY2 = IY1 + 1

	    IF( IX1 .LT. 1) IX1 = 1
	    IF( IY1 .LT. 1) IY1 = 1
	    IF( IX2 .GT. NX) IX2 = IX1
	    IF( IY2 .GT. NY) IY2 = IY1

* get the four surrounding points in the data array

	   VAL1 = DATAARRAY( IX1, IY1)
	   VAL2 = DATAARRAY( IX2, IY1)
	   VAL3 = DATAARRAY( IX1, IY2)
	   VAL4 = DATAARRAY( IX2, IY2)

* f & g are the fractional pixel displacements of the
* interpolation point

	   F = LINDAT( I, 1) - IX1
	   G = LINDAT( I, 2) - IY1

* on exit, 'line' will contain the array of interpolated values
* ready for plotting. bilinear interpolation is used.

	   LINE( I) = F*( VAL2 - VAL1) + F*G*( VAL1 + VAL4 - VAL2 - VAL3)
     :		    + G*( VAL3 - VAL1) + VAL1

	END DO

	END
