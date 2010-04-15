	SUBROUTINE IMCOMBSUB( DIMSX, DIMSY, ARRIN1, ARRIN2, ARROUT,
     :	                      NUMAREA, XST, YST, XSZ, YSZ, STATUS)

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	INTEGER
     :	  DIMSX,
     :	  DIMSY,
     :	  STATUS,
     :	  J,
     :	  K,
     :	  L,
     :	  NPTS1,
     :	  NPTS2,
     :	  MAXDATA,
     :	  NUMAREA
      INTEGER
     :	  XST( 100),
     :	  YST( 100),
     :	  XSZ( 100),
     :	  YSZ( 100),
     :	  XEN( 100),
     :	  YEN( 100),
     :	  XSTART,
     :	  XEND,
     :	  YSTART,
     :	  YEND

	PARAMETER ( MAXDATA = 5000)

	REAL
     :	  ARRIN1( DIMSX, DIMSY),
     :	  ARRIN2( DIMSX, DIMSY),
     :	  ARROUT( DIMSX, DIMSY),
     :	  VALMAX1,
     :	  VALMAX2,
     :	  VALMIN1,
     :	  VALMIN2
      REAL
     :	  STD1,
     :	  STD2,
     :	  SUM1,
     :	  SUM2,
     :	  MEAN1,
     :	  MEAN2,
     :	  MEDIAN1,
     :	  MEDIAN2,
     :	  MODE1,
     :	  MODE2

	REAL*8
     :	  DATARR( MAXDATA),
     :	  SUMSQ1,
     :	  SUMSQ2,
     :	  DIFF

*      Set output image with input 1
	DO K = 1, DIMSY
	  DO L = 1, DIMSX
	    ARROUT( L, K) = ARRIN1( L, K)
	  END DO
	END DO

*      Scan through all areas
	DO J = 1, NUMAREA

*        Calculate start and end coordinates of area in image 1
	  XEN( J) = ( XST( J)+XSZ( J)-1)
	  YEN( J) = ( YST( J)+YSZ( J)-1)

*        Set specified area bad in output image
	  DO K = YST( J), YEN( J)
	    DO L = XST( J), XEN( J)
	      ARROUT( L, K) = -1.0E20
	    END DO
	  END DO

	  CALL MSG_OUT( 'MESS', ' ', STATUS)
	  CALL MSG_SETI( 'XST', XST( J))
	  CALL MSG_SETI( 'YST', YST( J))
	  CALL MSG_SETI( 'XEN', XEN( J))
	  CALL MSG_SETI( 'YEN', YEN( J))
	  CALL MSG_OUT( 'MESS',
     :	    'Area to be removed start:^XST,^YST - end:^XEN,^YEN',
     :	    STATUS)
	  XSTART = XST( J) - 2
	  YSTART = YST( J) - 2
	  XEND = XEN( J) + 2
	  YEND = YEN( J) + 2
	  IF( XSTART .LT. 1 .OR. XSTART .GT. DIMSX) XSTART = XST( J)
	  IF( YSTART .LT. 1 .OR. YSTART .GT. DIMSY) YSTART = YST( J)
	  IF( XEND .LT. 1 .OR. XEND .GT. DIMSX) XEND = XEN( J)
	  IF( YEND .LT. 1 .OR. YEND .GT. DIMSY) YEND = YEN( J)

*        Initialize number pixels variable and scan through area in image 1
	  NPTS1 = 0
	  DO K = YSTART, YEND
	    DO L = XSTART, XEND

*            Test if pixel under consideration is not in bad area
	      IF( ARROUT( L, K) .GT. -1.0E20) THEN

*              If surrounding bad area then put in data buffer for median
	        NPTS1 = NPTS1 + 1
	        DATARR( NPTS1) = DBLE( ARRIN1( L, K))

	      END IF
	    END DO
	  END DO

*        Test to see if number of pixels to be medianed is > 0
	  IF( NPTS1 .GT. 0) THEN

	    CALL MSG_SETI( 'NPTS1', NPTS1)
	    CALL MSG_OUT( 'MESS',
     :	      'Number of pixels in comparison area 1 = ^NPTS1',
     :	      STATUS)

*          Median data. First sort
	    CALL PDA_QSAD( NPTS1, DATARR )

*          Call subroutine to find median for the input DATARR
	    CALL MED3D_CALMEDSUB( NPTS1, DATARR, VALMAX1, VALMIN1, SUM1,
     :	                          MEAN1, MEDIAN1, MODE1)

*          Scan through pixels calculate standard deviation
	    SUMSQ1 = 0.0D0
	    DO K = 1, NPTS1
	      SUMSQ1 = SUMSQ1 + DATARR( K)**2
	    END DO

*          Test for overflows of single precision variable
	    IF( ABS( SUM1) .LT. 1.0E20) THEN

*            calculate std over pixels
	      STD1 = SUMSQ1/NPTS1 - ( SUM1/NPTS1)**2
	      IF( STD1 .GE. 0.0D0) THEN
	        STD1 = SQRT( STD1 )
	      ELSE
	       STD1 = -999.0
	      END IF
	    ELSE
	      STD1 = -999.0
	    END IF
	    CALL MSG_SETR( 'STD1',  STD1 )
	    CALL MSG_OUT( 'MESS',
     :	      'Standard deviation in comparison area 1 = ^STD1',
     :	      STATUS)

	  ELSE

	    CALL MSG_OUT( 'ERR',
     :	      'Error, number points in area 1 to be medianed = 0',
     :        STATUS)

	  END IF

*        Calculate start and end coordinates of area in image 1
	  XSTART = XST( J) - 2
	  YSTART = YST( J) - 2
	  XEND = XEN( J) + 2
	  YEND = YEN( J) + 2
	  IF( XSTART .LT. 1 .OR. XSTART .GT. DIMSX) XSTART = XST( J)
	  IF( YSTART .LT. 1 .OR. YSTART .GT. DIMSY) YSTART = YST( J)
	  IF( XEND .LT. 1 .OR. XEND .GT. DIMSX) XEND = XEN( J)
	  IF( YEND .LT. 1 .OR. YEND .GT. DIMSY) YEND = YEN( J)

*        Initialize number pixels variable and scan through area in image 1
	  NPTS2 = 0
	  DO K = YSTART, YEND
	    DO L = XSTART, XEND

*            Test if pixel under consideration is not in bad area
	      IF( ARROUT( L, K) .GT. -1.0E20) THEN

*              If surrounding bad area then put in data buffer for median
	        NPTS2 = NPTS2 + 1
	        DATARR( NPTS2) = DBLE( ARRIN2( L, K))

	      END IF
	    END DO
	  END DO

*        Test to see if number of pixels to be medianed is > 0
	  IF( NPTS2 .GT. 0) THEN

	    CALL MSG_SETI( 'NPTS2', NPTS2)
	    CALL MSG_OUT( 'MESS',
     :	      'Number of pixels in comparison area 2 = ^NPTS2',
     :	      STATUS)

*          Median data. First sort
	    CALL PDA_QSAD( NPTS2, DATARR )

*          Call subroutine to find median for the input DATARR
	    CALL MED3D_CALMEDSUB( NPTS2, DATARR, VALMAX2, VALMIN2, SUM2,
     :	                          MEAN2, MEDIAN2, MODE2)

*          Scan through pixels calculate standard deviation
	    SUMSQ2 = 0.0D0
	    DO K = 1, NPTS2
	      SUMSQ2 = SUMSQ2 + DATARR( K)**2
	    END DO

*          Test for overflows of single precision variable
	    IF( ABS( SUM2) .LT. 1.0E20) THEN

*            calculate std over pixels
	      STD2 = SUMSQ2/NPTS2 - ( SUM2/NPTS2)**2
	      IF( STD2 .GE. 0.0D0) THEN
	        STD2 = SQRT( STD2 )
	      ELSE
	       STD2 = -999.0
	      END IF
	    ELSE
	      STD2 = -999.0
	    END IF
	    CALL MSG_SETR( 'STD2', STD2 )
	    CALL MSG_OUT( 'MESS',
     :	      'Standard deviation in comparison area 2 = ^STD2',
     :	      STATUS)

	  ELSE

	    CALL MSG_OUT( 'ERR',
     :	      'Error, number points in area 2 to be medianed = 0',
     :        STATUS)

	  END IF

	  CALL MSG_SETI( 'NUM', J)
	  CALL MSG_SETR( 'MED1', MEDIAN1)
	  CALL MSG_SETR( 'MED2', MEDIAN2)
	  CALL MSG_OUT( 'MESS',
     :	   'Area ^NUM, medians from images 1/2 = ^MED1/^MED2',
     :	   STATUS)

	  DIFF = MEDIAN1 - MEDIAN2

*        Set output image with scaled area from image 2
	  DO K = YST( J), YEN( J)
	    DO L = XST( J), XEN( J)
	      ARROUT( L, K) = ARRIN2( L, K)+SNGL( DIFF)
	    END DO
	  END DO

	END DO

	END
