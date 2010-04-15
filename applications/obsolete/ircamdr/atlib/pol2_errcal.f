	SUBROUTINE POL2_ERRCAL( INT_1, INT_2, INT_3, INT_4,
     :	                        INT_5, INT_6, INT_7, INT_8,
     :	                        POL, ELDN, POL_ERROR, THETA_ERROR)

*      Subroutine to calculate the polarization error from the 8 intensities
*      at the 4 waveplate positions 0 degrees, 22.5 degrees, 45 degrees and
*      67.5 degrees

*      Define local variables

	REAL
     :       ELDN,		! electrons/data number
     :       INT_1,		! The o- intensity at 0 degrees
     :	     INT_2,		! The e- intensity at 0 degrees
     :	     INT_3,		! The o- intensity at 45 degrees
     :	     INT_4,		! The e- intensity at 45 degrees
     :       INT_5,		! The o- intensity at 22.5 degrees
     :	     INT_6,		! The e- intensity at 22.5 degrees
     :	     INT_7,		! The o- intensity at 67.5 degrees
     :	     INT_8,		! The e- intensity at 67.5 degrees
     :	     POL,		! The percentage polarization
     :	     POL_ERROR,		! The polarization error
     :       SUM,		! The sum of the intensities
     :       THETA_ERROR	! The polarization error


*      Calculate the sum of the intensities

	SUM = INT_1 + INT_2 + INT_3 + INT_4 +
     :	      INT_5 + INT_6 + INT_7 + INT_8

*      Check if sum of four intensities > ZERO

	IF( SUM .GT. 0.0) THEN

*        Calculate the square of the polarization error

	  POL_ERROR = 10000.0/( SUM*ELDN)

*        Calculate the real polarization shot-noise error

	  IF( POL_ERROR .GE. 0.0) THEN

	    POL_ERROR = SQRT (POL_ERROR)

	  ELSE

	    POL_ERROR = 100.0

	  END IF

	ELSE

*        Sets the polarization error if intensities are zero

	  POL_ERROR =100.0

	END IF

*      Calculate the position angle error

	IF( POL .GT. 0.0) THEN

	  THETA_ERROR = 28.6*( POL_ERROR/POL)

	ELSE

	  THETA_ERROR = 45.0

	END IF

	END
