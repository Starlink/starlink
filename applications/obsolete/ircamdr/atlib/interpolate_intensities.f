	SUBROUTINE INTERPOLATE_INTENSITIES( COLOURS)

* Description : Subroutine to interpolate the blank (undefined) pens from the
*	     ones chosen by the user

	IMPLICIT NONE

	INTEGER K
	INTEGER PEN_END
	INTEGER PEN_START

	REAL COLOURS( 3, 256)

* define first pen specified as pen 0 and set next defined pen to 2

	PEN_START = 1
	PEN_END = 2

* loop to scan through pens until last pen is reached

	DO WHILE ( PEN_START .LE. 250)

* find next pen specified

	  DO WHILE ( COLOURS( 1, PEN_END) .LT. -1)
	    PEN_END = PEN_END + 1
	  END DO

!	  type *,'Start, end = ',pen_start, pen_end
!	  type *,'Start guns = ',colours( 1, pen_start),
!     *	                         colours( 2, pen_start),
!     *	                         colours( 3, pen_start)
!	  type *,'End guns   = ',colours( 1, pen_end),
!     *	                         colours( 2, pen_end),
!     *	                         colours( 3, pen_end)

* loop to interpolate the missing pens between the start and end pens

	  DO K = PEN_START+1, PEN_END-1
	    COLOURS( 1, K) = COLOURS( 1, PEN_START) +
     *	 ( K-PEN_START)*( COLOURS( 1, PEN_END)-COLOURS( 1, PEN_START))/
     *   (PEN_END-PEN_START+1)
	    COLOURS( 2, K) = COLOURS( 2, PEN_START) +
     *   ( K-PEN_START)*( COLOURS( 2, PEN_END)-COLOURS( 2, PEN_START))/
     *   (PEN_END-PEN_START+1)
	    COLOURS( 3, K) = COLOURS( 3, PEN_START) +
     *   ( K-PEN_START)*( COLOURS( 3, PEN_END)-COLOURS( 3, PEN_START))/
     *   (PEN_END-PEN_START+1)
	  END DO

* set new pen start and pen end

	  PEN_START = PEN_END
	  PEN_END = PEN_START + 1
	END DO

	END
