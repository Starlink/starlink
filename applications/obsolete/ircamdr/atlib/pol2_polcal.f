	SUBROUTINE POL2_POLCAL( QVAL, UVAL, POL)

*      Subroutine to calculate the percentage polarization from the Q and U
*      Stokes parameters

*      Define local variables

	REAL POL,	! The resultant percentage polarization
     :	     QVAL,	! The input Q Stokes parameter value
     :	     UVAL	! The input U Stokes parameter value

*      Calculate the square of the percentage polarization from the Q,U
*      Stokes parameters

	POL = QVAL**2 + UVAL**2

*      Test if the square of the percentage polarization is greater or equal
*      than zero

	IF( POL .GE. 0.0 .AND. POL .LE. 10000.0) THEN

*        If the square of the percentage polarization is zero or positive then
*        calculate the square root of the value

	  POL = SQRT( POL)

	ELSE

*        Polarization squared is illegal so set polarization to 0

	  POL = 0.0

	END IF

	END
