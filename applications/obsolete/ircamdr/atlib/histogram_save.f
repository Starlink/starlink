	SUBROUTINE HISTOGRAM_SAVE( ODIMS1, ODIMS2, HDS_OUT,
     :	                           HISTOGRAM, HIST_MAX, HIST_MIN,
     :	                           HIST_BINS, TERMINAL_OUTPUT, STATUS)


* History
*  11-Oct-1994 Changed DIM arguments for UNIX compiler (SKL@JACH)
*

	IMPLICIT NONE

	INTEGER K
	INTEGER ODIMS2
	INTEGER ODIMS1
	INTEGER HISTOGRAM( ODIMS1 )
	INTEGER HIST_BINS
	INTEGER STATUS

	REAL HDS_OUT( ODIMS1, ODIMS2 )
	REAL HIST_MAX, HIST_MIN

	LOGICAL TERMINAL_OUTPUT
*
* transfer the histogram values to the out image
*
	DO K = 1, ODIMS1

	  HDS_OUT( K, 1) = REAL( HISTOGRAM( K))

	END DO
*
* calculate the histogram X axis Max, Min range values and store that in next
* element of the output DATA_ARRAY primitive
*
	DO K = 1, ODIMS1

	  HDS_OUT( K, 2) =
     :           HIST_MIN + ( K - 1)*( HIST_MAX - HIST_MIN)/HIST_BINS

	END DO
*
* look at the terminal output flag and if TRUE then print out the histogram
* values to the users terminal
*
	IF( TERMINAL_OUTPUT ) THEN

	  DO K = 1, ODIMS1

	    CALL MSG_SETI( 'COUNTER', K)
	    CALL MSG_SETR( 'BIN', HDS_OUT( K, 2))
	    CALL MSG_SETR( 'HIST', HDS_OUT( K, 1))

	    CALL MSG_OUT( 'OUTPUT',
     :'Bin Number = ^COUNTER, Bin DN Value = ^BIN, No. in Bin = ^HIST',
     :	                  STATUS)

	  END DO

	END IF

	END

