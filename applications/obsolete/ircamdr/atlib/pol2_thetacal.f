	SUBROUTINE POL2_THETACAL( QVAL, UVAL, THETA)

*      Subroutine to calculate the position angle of polarization from
*      the Q and U Stokes parameters

*      Define local variables

	REAL PI,	! The value of Pi
     :	     QVAL,	! The input Q Stokes parameter value
     :	     UVAL,	! The input U Stokes parameter value
     :	     THETA	! The resutlant position angle of polarization

*      Define Pi

	PI = 4.0*ATAN( 1.0)

*      Test the Q value for a zero value

	IF( QVAL .NE. 0.0)THEN

*        Test the Q and U values for positive values

	  IF( QVAL .GE. 0.0 .AND. UVAL .GE. 0.0) THEN

*          Calculate the position angle without add any correction

	    THETA = 0.5*ATAN( UVAL/QVAL)*180.0/PI

	  END IF

*        Test Q and U values for positve Q, negative U values

	  IF( QVAL .GE. 0.0 .AND. UVAL .LT. 0.0) THEN

*          Calculate the position angle with a 180 degrees correction

	    THETA = 0.5*ATAN( UVAL/QVAL)*180./PI + 180.0

	  END IF

*        Test the Q and U values for negative Q and negative U values

	  IF( QVAL .LT. 0.0 .AND. UVAL .LT. 0.0) THEN

*          Calculate the position angle with a 90 degrees correction

	    THETA = 0.5*ATAN( UVAL/QVAL)*180.0/PI + 90.0

	  END IF

*        Test the Q and U values for negative Q and positve U values

	  IF( QVAL .LT. 0.0 .AND. UVAL .GE. 0.0) THEN

*          Calculate the position angle with a 90 degrees correction

	    THETA = 0.5*ATAN( UVAL/QVAL)*180.0/PI + 90.0

	  END IF

*        Test if resultant position angle is greater that 180 and correct
*        if necessary

	  IF( THETA .GE. 180.0) THETA = THETA - 180.0

	ELSE

*        Set position angle to 45 if the Q and U values are both zero

	  THETA = 45.0

	END IF

	END
