	SUBROUTINE FIND_MEDIAN( HIST_BINS, NUM, NUMBER, ARRIN, MEDIAN,
     :	                        HDIMS1, HDIMS2, ARRH)

* Description :
*
* Authors : Colin Aspin ROE ( REVA::CAA )
*
* History :
*  23-04-1986 :  First implementation (REVA::CAA)
*  12-AUG-1994   Changed DIM arguments so thar routine will compile,
*                changed input DIM arguments to GENHIS (SKL@JACH)
*
* Type definitions :

	IMPLICIT  NONE		  ! no implicit typing allowed

* Global constants :

	INCLUDE  'SAE_PAR'	    ! SSE global definitions

* Status :

	INTEGER  STATUS		 ! global status parameter

* Local variables :

	INTEGER
     :    NUM,         ! number of sample
     :    NUMBER,      ! number in sample
     :	  HDIMS1, HDIMS2,   ! dimensions of histogram image
     :	  HIST_BINS,   ! number of histogram bins
     :	  HISTOGRAM( 25), ! histogram values
     :	  IDIMS( 2),   ! input array dimensions
     :	  J	     ! counter for array element number

	REAL
     :	  ARRIN( NUMBER),			    ! input data
     :	  ARRH( HDIMS1, HDIMS2),		    ! output data histogram
     :	  MEAN,
     :	  MEDIAN,				    ! median
     :	  MODE,
     :	  SUGGESTED_MAX,
     :	  SUGGESTED_MIN,
     :	  SUM

*-
*
* calculate maximum and minimum in data
*
	CALL HIST_MAXMIN( NUMBER, 1, ARRIN, 1, 1, NUMBER, 1,
     :	                  SUGGESTED_MAX, SUGGESTED_MIN)
*
* call subroutine to generate the histogram of the requested section of the
* data between the requested maximum and minimum values
*
	IDIMS( 1) = NUMBER
	IDIMS( 2) = 1

	CALL GENHIS( IDIMS(1), IDIMS(2), ARRIN, 1, 1, NUMBER, 1,
     :	             SUGGESTED_MAX, SUGGESTED_MIN, HIST_BINS,
     :               HISTOGRAM, STATUS)
*
* put the histogram into the histogram image
*
	DO J = 1, NUMBER

	  IF( NUM .GE. 1 .AND. NUM .LE. HDIMS1 .AND.
     :	      J .GE. 1 .AND. J .LE. HDIMS2) THEN

	    ARRH( NUM, J) = HISTOGRAM( J)

	  END IF

	END DO
*
* call subroutine to take histogram and calculate median
*
	CALL HISTPROP( HISTOGRAM, HIST_BINS, SUGGESTED_MAX, SUGGESTED_MIN,
     :	               SUM, MEAN, MEDIAN, MODE, STATUS)

	END
