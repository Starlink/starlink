	SUBROUTINE STATSSUB2( ARRAY, DIMSX, DIMSY, XSTART, YSTART, XSIZE,
     :	                      YSIZE, MEDIAN, MODE)

* Description : Calculates the median and mode of pixels in sub-array of image

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	INTEGER
     :	  DIMSX,                       ! Image dimensions
     :	  DIMSY,                       ! Image dimensions
     :	  XSTART,                      ! X start of sub-array
     :	  YSTART,                      ! Y start of sub-array
     :	  XSIZE,                       ! X size sub-array
     :	  YSIZE,                       ! Y size of sub-array
     :	  J,                           ! counting variable
     :	  K,                           ! counting variable
     :	  L,                           ! counting variable
     :	  STATUS,                      ! adam status
     :	  MAXDATA                      ! Maximum data pixels for sorting

	PARAMETER ( MAXDATA = 100000)  ! Max data pixel

	REAL*8
     :	  ACTDATA( MAXDATA)            ! Data for sorting

	REAL
     :	  ARRAY( DIMSX, DIMSY),        ! Image array
     :	  MEDIAN,                      ! Median value
     :	  MODE,                        ! Mode value
     :	  SUM,                         ! Sum of values
     :    VALMIN,                      ! Minimum value
     :	  VALMAX,                      ! Maximum value
     :	  MEAN                         ! Mean value

*      initialize ADAM status variable

	STATUS = SAI__OK

*      initialize couting variable

	L = 0

*      scan through all the input pixels ...

	DO J = YSTART, ( YSTART+YSIZE-1)

	  DO K = XSTART, ( XSTART+XSIZE-1)

*          increment counting variable

	    L = L + 1

*          test if array subscript is less than max data allowed

	    IF( L .LE. MAXDATA) THEN

*            put the image pixel into the linear array

	      ACTDATA( L) = DBLE( ARRAY( K, J))

	    ELSE

	      CALL MSG_OUT( 'BLANK', ' ', STATUS)

	      CALL MSG_SETI( 'MAX', MAXDATA)
	      CALL MSG_OUT( 'MESSAGE',
     :	        'A maximum of ^MAX pixels can be used in MEDIAN sorting',
     :	        STATUS)
	      CALL MSG_OUT( 'MESSAGE',
     :	        'Data set TRUNCATED ...', STATUS)

	      CALL MSG_OUT( 'BLANK', ' ', STATUS)

*            decrement array subscript variable

	      L = L - 1

	      GOTO 100

	    END IF

	  END DO

	END DO

*      Here if data set too big for sorting array

  100	CONTINUE

*      Sort the pixel values in each stack image

	CALL PDA_QSAD( L, ACTDATA )

*      call subroutine to find median for the input ACTDATA

	CALL MED3D_CALMEDSUB( L, ACTDATA, VALMAX, VALMIN, SUM, MEAN,
     :	                      MEDIAN, MODE)

	END
