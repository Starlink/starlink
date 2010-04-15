	SUBROUTINE OEFIXSUB( IDIMS1, IDIMS2, ARRAY_IN, ARRAY_OUT,
     :                       STATUS)

* Description :
*
* Authors : Colin Aspin ROE ( REVA::CAA )
*
* History :
*  23-04-1986 :  First implementation (REVA::CAA)
*  14-JUL-1994   Changed LIB$ to FIO_ (SKL@JACH)
*  14-JUL-1994   Changed arguments so that routine would compile (SKL@JACH)
*  12-AUG-1994   Changed input DIM arguments for FIND_MEDIAN (SKL@JACH)
*   9-AUG-2004    Use FIO for open and close (TIMJ@JACH)
*
* Type definitions :

	IMPLICIT  NONE		  ! no implicit typing allowed

* Global constants :

	INCLUDE  'SAE_PAR'	    ! SSE global definitions
        INCLUDE  'DAT_PAR'          ! Necessary for non-VMS
        INCLUDE  'FIO_PAR'

* Status :

	INTEGER  STATUS		 ! global status parameter

* Local variables :

	INTEGER
     :    HDIMS( 2 ),  ! dimensions of output DATA_ARRAY for histogram
     :	  HIST_BINS,   ! number of bins in histogram
     :    IDIMS1,      ! dimensions of input DATA_ARRAY
     :    IDIMS2,      ! dimensions of input DATA_ARRAY
     :	  I,	       ! counter for array element number
     :	  J,	       ! counter for array element number
     :	  LUN,         ! lun for output data list
     :	  NDIMS,       ! number of dimemsions
     :	  PNTRH        ! pointer for histogram image

	REAL
     :	  ARRAY_IN( IDIMS1, IDIMS2 ),  ! input data image
     :	  ARRAY_OUT( IDIMS1, IDIMS2), ! output A channel data image
     :	  AVERAGE_MEDIAN_ODD,		    ! average median of odd channel
     :	  MEDIAN_EVEN,			    ! median of even channel
     :	  MEDIAN_ODD_1,			    ! median of odd channel
     :	  MEDIAN_ODD_2,			    ! median of odd channel
     :	  SCALER,			    ! scaler for median even
     :	  WORK_EVEN( 1000),		    ! work array for even channel
     :	  WORK_ODD_1( 1000),		    ! work array for odd channel
     :	  WORK_ODD_2( 1000)		    ! work array for odd channel

	CHARACTER*( DAT__SZLOC)
     :	  LOCH        			    ! locator for histogram image
*-
*      check status on entry - return if not o.k.
*
	IF ( STATUS .NE. SAI__OK ) THEN

	   RETURN

	END IF

*      create the output default histogram file

	HIST_BINS = 25
	HDIMS( 1) = IDIMS1
	HDIMS( 2) = HIST_BINS
	NDIMS = 2

	CALL CREOUT( 'HISTIM', 'OTITLE', NDIMS, HDIMS, LOCH, STATUS )
	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL DAT_ANNUL( LOCH, STATUS )
	  RETURN
	END IF
*
* map in a DATA_ARRAY components
*
	CALL CMP_MAPN( LOCH, 'DATA_ARRAY', '_REAL', 'WRITE',
     :                 NDIMS, PNTRH, HDIMS, STATUS )
	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL CMP_UNMAP( LOCH, 'DATA_ARRAY', STATUS )
	  CALL DAT_ANNUL( LOCH, STATUS )
	  RETURN
	END IF


*      open the output data list file
	CALL FIO_OPEN( 'IMAGEDIR/oefix.lis', 'WRITE','LIST',0, LUN,
     :        STATUS)

	CALL FIO_WRITE( LUN,
     :	'Column Number, Median_Odd1, Median_Odd2, Median_Even, Scaler')

*      scan through odd channel and find median value and median in adjacent
*      even channels

	DO J = 1, IDIMS1, 2

*        test the current loop value for message to user

	  IF( IFIX( (J-1)/10.0+0.5)*10 .EQ. (J-1)) THEN

	    CALL MSG_SETI( 'NUMTY', J-1)
	    CALL MSG_OUT( 'MESSAGE', 'Processing column ^NUMTY ...',
     :	                  STATUS)

	  END IF

*        scan down column

	  DO I = 1, IDIMS2

*          set the working array with column data

	    WORK_ODD_1( I) = ARRAY_IN( J, I)
	    IF( ( J+1) .LE. IDIMS1 ) THEN
	      WORK_EVEN( I) = ARRAY_IN( J+1, I)
	    ELSE
	      WORK_EVEN( I) = ARRAY_IN( J, I)
	    END IF
	    IF( ( J+2) .LE. IDIMS1 ) THEN
	      WORK_ODD_2( I) = ARRAY_IN( J+2, I)
	    ELSE
	      WORK_ODD_2( I) = WORK_ODD_1( I)
	    END IF

	  END DO

*        find median value in odd and even columns

	  CALL FIND_MEDIAN( HIST_BINS, J, IDIMS2, WORK_ODD_1,
     :	                    MEDIAN_ODD_1, HDIMS(1), HDIMS(2),
     :                      %VAL( PNTRH))

	  IF( ( J+1) .LE. IDIMS1 ) THEN
	    CALL FIND_MEDIAN( HIST_BINS, J+1, IDIMS2, WORK_EVEN,
     :	                      MEDIAN_EVEN, HDIMS(1), HDIMS(2),
     :                        %VAL( PNTRH))
	  END IF

	  IF( ( J+2) .LE. IDIMS1 ) THEN
	    CALL FIND_MEDIAN( HIST_BINS, J+2, IDIMS2, WORK_ODD_2,
     :	                      MEDIAN_ODD_2, HDIMS(1), HDIMS(2),
     :                        %VAL( PNTRH))
	  ELSE
	    CALL FIND_MEDIAN( HIST_BINS, J, IDIMS2, WORK_ODD_2,
     :	                      MEDIAN_ODD_2, HDIMS(1), HDIMS(2),
     :                        %VAL( PNTRH))
	  END IF

*        form the average of the odd medians and the scaler for the even one

	  AVERAGE_MEDIAN_ODD  = ( MEDIAN_ODD_1 + MEDIAN_ODD_2)/2.0

	  IF( ABS( MEDIAN_EVEN) .GT. 0.0001) THEN

	    SCALER = AVERAGE_MEDIAN_ODD/MEDIAN_EVEN

	  ELSE

	    SCALER = 1.0

	  END IF

*        loop to set the output image with scaled values

	  DO I = 1, IDIMS2

	    ARRAY_OUT( J, I) = ARRAY_IN( J, I)
	    IF( ( J+1) .LE. IDIMS1 ) THEN
	      ARRAY_OUT( J+1, I) = ARRAY_IN( J+1, I)*SCALER
	    END IF

	  END DO

*        write line in data list file

	  WRITE( LUN, *)
     :          J, MEDIAN_ODD_1, MEDIAN_ODD_2, MEDIAN_EVEN, SCALER

	END DO

*      close list file
	CALL FIO_CLOSE( LUN, STATUS )

*      release histogram image

	CALL CMP_UNMAP( LOCH, 'DATA_ARRAY', STATUS)
	CALL DAT_ANNUL( LOCH, STATUS)

	END
