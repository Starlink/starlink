	SUBROUTINE CUT_INTERPOLATER( NPTS, MAXP, LINDAT, LINE, NX, NY,
     :	                             DATAARRAY, STATUS)

* Description : Get the row data from the data image

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

* get the line data from the image

	DO I = 1, NPTS

	  J = IFIX( LINDAT( I, 1) + 0.5)
	  K = IFIX( LINDAT( I, 2) + 0.5)

	  IF( J .GT. 0 .AND. J .LE. NX .AND.
     :	      K .GT. 0 .AND. K .LE. NY) THEN

	    LINE( I) = DATAARRAY( J, K)

	  ELSE

	    LINE( I) = 0.0

	  END IF

	END DO

	END
