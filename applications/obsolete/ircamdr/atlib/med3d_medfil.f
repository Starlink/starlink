	SUBROUTINE MED3D_MEDFIL( COUNTER, WDIMSX, WDIMSY, WDIMSZ, WORK,
     :	                         WLDIMSX, LWORK, DIMSX, DIMSY, ARROUT,
     :	                         VIEW, XPIX, YPIX, WHICH, SCALING,
     :	                         XST, XSZ, YST, YSZ, NORMALIZATION)

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	INTEGER
     :	  COUNTER,
     :	  WDIMSX,
     :	  WDIMSY,
     :	  WDIMSZ,
     :	  WLDIMSX,
     :	  DIMSX,
     :	  DIMSY,
     :	  J,
     :	  K,
     :	  L,
     :	  I,
     :	  XPIX,
     :	  YPIX,
     :	  STATUS,
     :	  XST,
     :	  XSZ,
     :	  YST,
     :	  YSZ

	REAL
     :	  WORK( WDIMSX, WDIMSY, WDIMSZ),
     :	  ARROUT( DIMSX, DIMSY),
     :	  VALMIN,
     :	  VALMAX,
     :	  SUM,
     :	  MEAN,
     :	  MEDIAN,
     :	  MODE

	REAL*8
     :	  LWORK( WLDIMSX)

	CHARACTER*( *)
     :	  WHICH

	LOGICAL
     :	  VIEW,
     :	  SCALING,
     :	  NORMALIZATION

*      Set adam status variable
	STATUS = SAI__OK

*      check scaling and call subroutine to do it if requested
	IF( SCALING) THEN

	  CALL MED3D_SCALING( WDIMSX, WDIMSY, WDIMSZ, WORK, DIMSX, DIMSY,
     :	                      COUNTER, XST, XSZ, YST, YSZ)

	END IF

*      scan through all the input pixels ...
	DO J = 1, DIMSY

	  DO K = 1, DIMSX

*          form the array with the pixel values to be medianed
	    DO L = 1, COUNTER

	      LWORK( L) = WORK( K, J, L)

	    END DO

*          test if user wants to view the raw LWORK etc ...
	    IF( K .EQ. XPIX .AND. J .EQ. YPIX) THEN

	      IF( VIEW ) THEN

	        CALL MSG_OUT( 'BLANK', ' ', STATUS)

	        CALL MSG_OUT( 'MESSAGE', 'Raw view-pixel values ...',
     :                    STATUS)

	        DO I = 1, COUNTER

	          CALL MSG_SETI( 'NUM', I)
	          CALL MSG_SETI( 'X', XPIX)
	          CALL MSG_SETI( 'Y', YPIX)
	          CALL MSG_SETR( 'VAL', SNGL( LWORK( I)))
	          CALL MSG_OUT( 'MESSAGE',
     :	     'Stack Image number ^NUM, pixel ^X,^Y, raw value = ^VAL',
     :       STATUS)

	        END DO

	      END IF

	    END IF

*          Sort the pixel values in each stack image
	    CALL PDA_QSAD( COUNTER, LWORK )

*          call subroutine to find median for the input LWORK
	    CALL MED3D_CALMEDSUB( COUNTER, LWORK, VALMAX, VALMIN, SUM,
     :	                      MEAN, MEDIAN, MODE)

*          test if user wants to view the sorted LWORK etc ...
	    IF( K .EQ. XPIX .AND. J .EQ. YPIX) THEN

	      IF( VIEW ) THEN

	        CALL MSG_OUT( 'BLANK', ' ', STATUS)

	        CALL MSG_OUT( 'MESSAGE',
     :                    'Maximum, Minimum MEDIAN values ...',
     :	                  STATUS)

	        CALL MSG_SETR( 'VAL1', VALMAX)
	        CALL MSG_OUT( 'MESSAGE',
     :	         'Maximum value = ^VAL1',
     :	         STATUS)
	        CALL MSG_SETR( 'VAL2', VALMIN)
	        CALL MSG_OUT( 'MESSAGE',
     :	         'Minimum value = ^VAL2',
     :	         STATUS)

	        CALL MSG_OUT( 'BLANK', ' ', STATUS)

	        CALL MSG_OUT( 'MESSAGE', 'Statistics of MEDIANS ...',
     :                    STATUS)

	        CALL MSG_SETR( 'VAL3', SUM)
	        CALL MSG_OUT( 'MESSAGE',
     :	         'Sum of values = ^VAL3',
     :	         STATUS)
	        CALL MSG_SETR( 'VAL4', MEAN)
	        CALL MSG_OUT( 'MESSAGE',
     :	         'Mean value    = ^VAL4',
     :	         STATUS)
	        CALL MSG_SETR( 'VAL5', MEDIAN)
	        CALL MSG_OUT( 'MESSAGE',
     :	         'Median value  = ^VAL5',
     :	         STATUS)
	        CALL MSG_SETR( 'VAL6', MODE)
	        CALL MSG_OUT( 'MESSAGE',
     :	         'Mode value    = ^VAL6',
     :	         STATUS)

	      END IF

	    END IF

*          set the output pixel with the median value
	    IF( WHICH .EQ. 'MEAN') THEN

	      ARROUT( K, J) = MEAN

	    ELSE IF( WHICH .EQ. 'MODE') THEN

	      ARROUT( K, J) = MODE

	    ELSE

	      ARROUT( K, J) = MEDIAN

	    END IF

	  END DO

	END DO

*      check if want normalization and call subroutine to do it if requested
	IF( NORMALIZATION) THEN

	  CALL MED3D_NORM( DIMSX, DIMSY, ARROUT, XST, XSZ, YST, YSZ)

	END IF

	END
