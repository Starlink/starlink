	SUBROUTINE CUT_INTERPOLATEC( NPTS, MAXP, LINDAT, LINE, NX, NY,
     :	                             DATAARRAY, STATUS)

* Description : Gets the data values from the image

	IMPLICIT NONE

	INTEGER
     :	  NPTS,
     :	  NX,
     :	  NY,
     :	  MAXP,
     :	  STATUS,
     :	  I,
     :	  J,
     :	  K

	REAL
     :	  LINDAT( MAXP, 2),
     :	  LINE( MAXP),
     :	  DATAARRAY( NX, NY)

* loop to specify line data from input image

	DO I = 1, NPTS

	  J = IFIX( LINDAT( I, 1) + 0.5)
	  K = IFIX( LINDAT( I, 2) + 0.5)

	  IF( J .GE. 1 .AND. J .LE. NX .AND.
     :	      K .GE. 1 .AND. K .LE. NY) THEN

	    LINE( I) = DATAARRAY( J, K)

	  ELSE

	    LINE( I) = 0.0

	  END IF

	END DO

	END
