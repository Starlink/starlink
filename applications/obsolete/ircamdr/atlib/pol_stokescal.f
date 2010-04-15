	SUBROUTINE POL_STOKESCAL( INT_1, INT_2, STOKES)

*      Subroutine to calculate a Stokes parameter (Q or U) from the input
*      intensities at 2 of the 4 possible waveplate position in a polarization
*      measurement i.e. 0 degrees, 22.5 degrees, 45 degrees and 67.5 degrees

*      Define local variables

	REAL INT_1,	! The intensity at 0 degrees or 22.5 degrees
     :	     INT_2,	! The intensity at 45 degrees or 67.5 degrees
     :	     STOKES,	! The Stokes parameter calculated
     :	     TOLERANCE	! Tolerance on sum of intensity values being 0

*      Define the tolerance parameter

	PARAMETER ( TOLERANCE = 0.0001)

*      Test if the sum of the input intensities is smaller than the specified
*      tolerance value

	IF( ABS( INT_1 + INT_2) .GT. TOLERANCE) THEN

*        Calculate the Stokes parameter from the intensities input

	  STOKES = ( INT_1 - INT_2)/( INT_1 + INT_2)*100.0

	ELSE

*        Set the Stokes parameter to zero if the sum of the intensities is
*        smaller than the tolerance value

	  STOKES = 0.0

	END IF

*      Test the value of the Stokes parameter calculates and set to 100%
*      if the value is larger than 100 or -100% if smaller than -100

	IF( STOKES .LT. -100.0) THEN

	  STOKES = -100.0

	ELSE IF( STOKES .GT. 100.0) THEN

	  STOKES = 100.0

	END IF

	END
