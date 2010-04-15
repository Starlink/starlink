

*+  GENHIS - calculates the histogram of an array of data

      SUBROUTINE GENHIS ( DIMS1, DIMS2, ARRAY, XSTART, YSTART, XFINISH,
     :                    YFINISH, VALMAX, VALMIN, NUMBIN, HIST,
     :                    STATUS )

*    Description :
*
*     This routine calculates the truncated intensity histogram
*     of a rectangular array of data.
*
*    Invocation :
*
*     CALL GENHIS( DIMS, ARRAY, XSTART, YSTART, XFINISH, YFINISH,
*                  VALMAX, VALMIN, NUMBIN, HIST, STATUS )
*
*    Parameters :
*
*     DIMS( 2 ) = INTEGER( READ )
*           The dimensions of the input array
*     ARRAY( DIMS(1), DIMS(2) ) = REAL( READ )
*           The input data aray
*     XSTART = INTEGER( READ )
*           x coordinate of start of sub-array to be included
*     YSTART = INTEGER( READ )
*           y coordinate of start of sub-array to be included
*     XFINISH = INTEGER( READ )
*           x coordinate of end of sub-array to be included
*     YFINISH = INTEGER( READ )
*           y coordinate of end of sub-array to be included
*     VALMAX = REAL( READ, WRITE )
*           Maximum intensity included in sub-array
*     VALMIN = REAL( READ, WRITE )
*           Minimum intensity included in sub-array
*     NUMBIN = INTEGER( READ )
*           Number of bins used in the histogram
*     HIST( NUMBIN ) = INTEGER( WRITE )
*           Array containing the intensity histogram
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Initialise histogram bins to zero
*     Calculate scaling factor from input max and min values
*     For all rows in selected sub-array
*        For all pixels in current row
*           Calculate correct histogram bin for current pixel
*           Increment histogram bin by one
*        Endfor
*     Endfor
*     Return
*
*    Bugs :
*
*     None are known at this time.
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*
*    History :
*
*     30-07-1985 : First SSE/ADAM implementation, with algorithm taken
*                : from Dennis Kelly's GENHIS from Starlink (UKTH::MARK)
*     14-04-1986 : Tidied up (REVA::MJM)
*     12-AUG-1994  Changed DIM arguments so that routine will compile(SKL@JACH)
*
*    Type Definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! global SSE definitions

*    Import :

      INTEGER
     :    DIMS1, DIMS2,           ! dimensions of input array
     :    XSTART,                 ! x start coord for sub-array
     :    YSTART,                 ! y   "     "    "   "    "
     :    XFINISH,                ! x finish  "    "   "    "
     :    YFINISH,                ! y   "     "    "   "    "
     :    NUMBIN                  ! number of bins used in histogram

      REAL
     :    ARRAY( DIMS1, DIMS2 ),  ! input data array
     :    VALMAX,        ! maximum value in included sub-array
     :    VALMIN         ! minimum   "    "     "     "    "

*    Export :

      INTEGER
     :    HIST( NUMBIN )          ! calculated intensity histogram

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      INTEGER
     :    DUMMY,                  ! dummy used in histogram calculation
     :    I, J, K                 ! counters

      REAL
     :    SCALE                   ! scale factor used for choosing correct bin

*-
*    check for error on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    first set the bins of the histogram to zero
      DO  K  =  1, NUMBIN
         HIST( K ) = 0
      END DO

*    next set the scaling factor, watching for too small value
      IF( ABS( VALMAX - VALMIN ) .GT. 1.0E-20 ) THEN
         SCALE  =  VALMAX - VALMIN
      ELSE
         SCALE  =  1.0
      END IF

*    now calculate the histogram - loop round the rows first
      DO  J  =  YSTART, YFINISH

*       now loop round all the pixels in the current row
         DO  I  =  XSTART, XFINISH

*          find bin number for particular point
            DUMMY  =  NUMBIN * ( ARRAY( I, J ) - VALMIN ) / SCALE

*          check to see that bin is within range
            DUMMY  =  MIN( NUMBIN, MAX( 1, DUMMY ) )

*          set required bin to be one bigger then before
            HIST( DUMMY )  =  HIST( DUMMY ) + 1

*       end of loop round pixels in current row
         END DO

*    end of loop round rows
      END DO


*    end and return
      END
