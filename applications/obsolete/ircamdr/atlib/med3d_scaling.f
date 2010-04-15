	SUBROUTINE MED3D_SCALING( WDIMSX, WDIMSY, WDIMSZ, WORK,
     :	                          DIMSX, DIMSY, COUNTER, XST, XSZ,
     :	                          YST, YSZ)

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	INTEGER WDIMSX, WDIMSY, WDIMSZ, DIMSX, DIMSY, COUNTER, J, K,
     :	        I, N, STATUS, XST, XSZ, YST, YSZ, NPIX, XEN, YEN

	REAL WORK( WDIMSX, WDIMSY, WDIMSZ), VALMIN, VALMAX, SUM,
     :	     MEAN, MEDIAN, MODE, TOLERANCE

	REAL*8 ACTDATA( 65536), TEMP( 256), SCALVAL( 256)

	STATUS = SAI__OK

*      define tolerance on real comparisons
	TOLERANCE = 1.0E-10

*      calculate the x,y start,end pixels and number of pixels in the scaling
*      box
	XEN = XST + XSZ - 1
	YEN = YST + YSZ - 1

	IF( XST .LT. 1) XST = 1
	IF( XST .GT. DIMSX) XST = DIMSX
	IF( XEN .LT. XST) XEN = XST
	IF( XEN .GT. DIMSX) XEN = DIMSX

	IF( YST .LT. 1) YST = 1
	IF( YST .GT. DIMSY) YST = DIMSY
	IF( YEN .LT. YST) YEN = YST
	IF( YEN .GT. DIMSY) YEN = DIMSY

	NPIX = XSZ*YSZ

*      tell user number of pixels in scaling box
	CALL MSG_OUT( 'BLANK', ' ', STATUS)

	CALL MSG_OUT( 'MESSAGE', 'Scaling values ...', STATUS)

	CALL MSG_SETI( 'NUMP', NPIX)
	CALL MSG_OUT( 'MESSAGE',
     :	  'Number of pixels in scaling box = ^NUMP',
     :	  STATUS)

*      scan through all input images
	DO I = 1, COUNTER

*        initialize the ACTDATA array counter
	  N = 0

*        scan through all the input image areas specified ...
	  DO J = YST, YEN

	    DO K = XST, XEN

*            increment the ACTDATA array couting variable
	      N = N + 1

*            form the array with the pixel values to be medianed
	      ACTDATA( N) = WORK( K, J, I)

	    END DO

	  END DO

*        sort data
	  CALL PDA_QSAD( N, ACTDATA )

*        call subroutine to find median for the input ACTDATA
	  CALL MED3D_CALMEDSUB( N, ACTDATA, VALMAX, VALMIN, SUM, MEAN,
     :	                        MEDIAN, MODE)

*        store the median value for subsequent scaling
	  SCALVAL( I) = MEDIAN

*        tell user the median value for current image
	  CALL MSG_SETR( 'VAL1', MEDIAN)
	  CALL MSG_SETI( 'VAL2', I)
	  CALL MSG_OUT( 'MESSAGE',
     :	    'Image number ^VAL2, median value in scaling area = ^VAL1',
     :	    STATUS)

	END DO

*      save the ACTDATA for scaling in correct order
	DO J = 1, COUNTER

	  TEMP( J) = SCALVAL( J)

	END DO

*      calculate the median of the set of median values calculated above
	CALL PDA_QSAD( COUNTER, SCALVAL )

	CALL MED3D_CALMEDSUB( COUNTER, SCALVAL, VALMAX, VALMIN, SUM, MEAN,
     :	                      MEDIAN, MODE)

*      tell user the median of the median values
	CALL MSG_SETR( 'MED', MEDIAN)
	CALL MSG_OUT( 'MESSAGE',
     :	  'Median of median values from scaling area = ^MED',
     :	  STATUS)

*      check for a zero median value and correct
	IF( ABS( MEDIAN) .LT. TOLERANCE) THEN

	  CALL MSG_OUT( 'MESSAGE',
     :	    'Error, median value is < 1.0e-10 ... set to 1.0e-10 ...',
     :	    STATUS)

	  MEDIAN = 1.0E-10

	END IF

*      scan thorugh all images and scale with respect to the median of the
*      median values caluculated
	DO I = 1, COUNTER

*        calculate the SCALVAL for image scaling
	  SCALVAL( I) = TEMP( I)/MEDIAN

	  IF( ABS( SCALVAL( I)) .LT. TOLERANCE) THEN

	    CALL MSG_OUT( 'MESSAGE',
     :	      '   SCALVAL < 1.0E-10 ... set it to 1.0E-10 ...',
     :	      STATUS)

	    SCALVAL( I) = 1.0E-10

	  END IF

	  CALL MSG_SETD( 'SCA', SCALVAL( I))
	  CALL MSG_SETI( 'NUM', I)
	  CALL MSG_OUT( 'MESSAGE',
     :	    'Image number ^NUM, SCALING factor = ^SCA',
     :	    STATUS)

*        scan through all the input image areas specified ...
	  DO J = 1, DIMSY

	    DO K = 1, DIMSX

	        WORK( K, J, I) = WORK( K, J, I)/SCALVAL( I)

	    END DO

	  END DO

	END DO

	END
