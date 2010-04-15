	SUBROUTINE POL2_STOKESCAL( INT_1, INT_2, INT_3, INT_4, STOKES)

*      Subroutine to calculate a Stokes parameter (Q or U) from the input
*      intensities at 2 of the 4 possible waveplate position in a polarization
*      measurement i.e. 0 degrees, 22.5 degrees, 45 degrees and 67.5 degrees
*      dual-beam polarization mode values

*      Define local variables

	REAL INT_1,	! The o- intensity at 0 degrees or 22.5 degrees
     :	     INT_2,	! The e- intensity at 0 degrees or 22.5 degrees
     :       INT_3,     ! The o- intensity at 45 degrees or 67.5 degrees
     :	     INT_4,     ! The e- intensity at 45 degrees or 67.5 degrees
     :	     STOKES,	! The Stokes parameter calculated
     :	     RAT1,
     :	     RAT2,
     :	     RAT3,
     :	     TOLERANCE	! Tolerance on sum of intensity values being 0

*      Define the tolerance parameter

	PARAMETER ( TOLERANCE = 0.0001)

*      Test if the sum of the input intensities is smaller than the specified
*      tolerance value

	IF( ABS( INT_1 + INT_2 + INT_3 + INT_4) .GT. TOLERANCE) THEN

*        Calculate the Stokes parameter from the intensities input

	  IF( INT_2 .GT. 0.0) THEN
	    RAT1 = INT_1/INT_2
	  ELSE
	PRINT *, 'int_2 <= 0.0'
	    RAT1 = 1.0E20
	  END IF
	  IF( INT_4 .GT. 0.0) THEN
	    RAT2 = INT_3/INT_4
	  ELSE
	PRINT *, 'int_4 <= 0.0'
	    RAT2 = 1.0E20
	  END IF
	  IF( RAT2 .GT. 0.0) THEN
	    IF( RAT1/RAT2 .GT. 0.0) THEN
	      RAT3 = SQRT( RAT1/RAT2)
	    ELSE
	PRINT *, 'rat1/rat2 <= 0.0'
	      RAT3 = 0.0
	    END IF
	  ELSE
	PRINT *, 'rat2 <= 0.0'
	    RAT3 = 0.0
	  END IF
	  STOKES = ( ( RAT3 - 1)/( RAT3 + 1))*100

	ELSE

*        Set the Stokes parameter to zero if the sum of the intensities is
*        smaller than the tolerance value

	  STOKES = 0.0

	END IF

*      Test the value of the Stokes parameter calculates and set to 100%
*      if the value is larger than 100 or -100% if smaller than -100

	IF( STOKES .LT. -100.0) THEN

!	  STOKES = -100.0

	ELSE IF( STOKES .GT. 100.0) THEN

!	  STOKES = 100.0

	END IF

	END
