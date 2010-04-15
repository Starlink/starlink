	SUBROUTINE MED3D_NORM( DIMSX, DIMSY, ARROUT, XST, XSZ, YST, YSZ)

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	INTEGER DIMSX, DIMSY, J, K, N, STATUS,
     :	        XST, XSZ, YST, YSZ

	REAL ARROUT( DIMSX, DIMSY), VALMIN, VALMAX, SUM, MEAN, MEDIAN,
     :	     MODE, TOLERANCE, XEN, YEN

	REAL*8 DATA( 65536)

	STATUS = SAI__OK

	TOLERANCE = 1.0E-10

	N = 0

*      scan through all the output image pixels ...
	XST = MAX( 1, XST)
	YST = MAX( 1, YST)
	XEN = MIN( DIMSX, ( XST+XSZ-1))
	YEN = MIN( DIMSY, ( YST+YSZ-1))

	DO J = YST, YEN

	  DO K = XST, XEN

*          increment number of pixels counting variable
	    N = N + 1

*          form the array with the pixel values to be medianed
	    DATA( N) = ARROUT( K, J)

	  END DO

	END DO

*      calculate the median value of the normalization area
	CALL PDA_QSAD( N, DATA )

	CALL MED3D_CALMEDSUB( N, DATA, VALMAX, VALMIN, SUM, MEAN,
     :	                      MEDIAN, MODE)

*      tell user the normalization value
	CALL MSG_OUT( 'BLANK', ' ', STATUS)

	CALL MSG_SETR( 'NORM', MEDIAN)
	CALL MSG_OUT( 'MESSAGE',
     :	  'Normalization value (median) = ^NORM', STATUS)

*      scale the output image to 1.0 using median value calculated above
	DO J = 1, DIMSY

	  DO K = 1, DIMSX

	    IF( ABS( MEDIAN) .GT. TOLERANCE) THEN

	      ARROUT( K, J) = ARROUT( K, J)/MEDIAN

	    ELSE

	      ARROUT( K, J) = 1.0

	    END IF

	  END DO

	END DO

	END
