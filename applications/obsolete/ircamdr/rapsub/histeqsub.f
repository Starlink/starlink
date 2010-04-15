
*+  HISTEQSUB - equalises the histogram of a 2-d image

      SUBROUTINE HISTEQSUB( DIMS1, DIMS2, ARRAY, XSTART, YSTART,
     :                      XFINISH, YFINISH, NUMBIN, OLDHIST, MAP,
     :                      STATUS )

*    Description :
*
*     This routine takes as input a 2-d image and equalises (linearises)
*     its histogram. Output is an equalised version of the input image.
*     The histogram equalisation takes place over a specified region of
*     the array, and uses a specified number of bins in the algorithm.
*
*    Invocation :
*
*     CALL HISTEQSUB( DIMS, ARRAY, XSTART, YSTART, XFINISH, YFINISH,
*                     NUMBIN, OLDHIST, MAP, STATUS )
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get maximum and minimum values in defined subarray
*     Call GENHIS to generate histogram of old subarray
*     Initialise cumulative probability variable
*     For all bins in histogram
*        Calculate current bin probability and cumulative probability
*        Work out corresponding equalised histogram bin number
*        Set key element accordingly to record this number
*     Endfor
*     Call REMAP to remap the defined subarray according to the
*       key as worked out above
*     Return
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*
*    History :
*
*     31-07-1985 : First SSE/ADAM implementation, with algorithm
*                : taken from Gonzalez and Wintz, "Digital Image
*                : Processing" 1977 (UKTH::MARK)
*     14-04-1986 : Major reshuffle and tidy (REVA::MJM)
*     12-AUG-1994  Changed DIM arguments so that routine will compile and
*                  changed input DIM arguments for GENHIS, MAXMIN, REMAP
*                  (SKL@JACH)
*    Type Definitions :

      IMPLICIT  NONE            ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'        ! global SSE definitions

*    Import :

      INTEGER
     :    DIMS1, DIMS2,         ! dimensions of input/output images
     :    XSTART,               ! x start coord of sub-array to be equalised
     :    YSTART,               ! y   "     "    "  "    "    "  "     "
     :    XFINISH,              ! x finish  "    "  "    "    "  "     "
     :    YFINISH,              ! y   "     "    "  "    "    "  "     "
     :    NUMBIN                ! number of bins used in histograms

*    Import-Export :

      REAL
     :    ARRAY( DIMS1, DIMS2 )     ! image to be equalised

*    Export :

      INTEGER
     :    OLDHIST( NUMBIN ),    ! histogram of image before equalisation
     :    MAP( NUMBIN )         ! key to bin/bin histogram mapping

*    Status :

      INTEGER  STATUS           ! global status parameter

*    Local variables :

      INTEGER
     :    NUMPIX,               ! number of pixels in required subsection
     :    NEWBIN,               ! bin counter for new histogram
     :    MAXPOS( 2 ),          ! position of maximum found in sub-array
     :    MINPOS( 2 ),          !     "     " minimum   "    "  "    "
     :    J                     ! counter

      REAL
     :    MAXIMUM,              ! maximum value in required subsection of image
     :    MINIMUM,              ! minimum   "    "     "         "      "   "
     :    PROB,                 ! probability function for old bins
     :    CUMPROB               ! cumulative probability function

*-
*    check status on entry - return if not ok
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    get the maximum and minimum values in the required sub-array
      CALL MAXMIN( DIMS1, DIMS2, ARRAY, XSTART, YSTART, XFINISH,
     :             YFINISH, NUMPIX, MAXIMUM, MINIMUM, MAXPOS, MINPOS,
     :             STATUS )

*    generate a histogram of the required sub-array
      CALL GENHIS( DIMS1, DIMS2, ARRAY, XSTART, YSTART, XFINISH,
     :             YFINISH, MAXIMUM, MINIMUM, NUMBIN, OLDHIST,
     :             STATUS )

*    initialise the cumulative probability variable
      CUMPROB  =  0.0

*    loop round all the histogram bins in order to equalise the histogram
      DO  J  =  1, NUMBIN

*       calculate the probability for the jth bin, and the cumulative
*       probability so far
         PROB     =  REAL( OLDHIST( J ) ) / REAL( NUMPIX )
         CUMPROB  =  CUMPROB + PROB

*       now work out the corresponding bin in the equalised histogram
*       from the cumulative probability and number of bins
         NEWBIN  =  NINT( CUMPROB * NUMBIN )
         NEWBIN  =  MIN( NUMBIN, MAX( 1, NEWBIN ) )

*       keep track of which bin in the equalised histogram the pixels
*       in the jth bin of the old histogram belong to
         MAP( J )  =  NEWBIN

*    end of loop round bins to equalise old histogram
      END DO

*    finally, remap the required subsection of the array according to
*    the key contained in the MAPHIST array
      CALL REMAP( DIMS1, DIMS2, ARRAY, XSTART, YSTART, XFINISH,
     :            YFINISH, MAXIMUM, MINIMUM, NUMBIN, MAP, STATUS )


*    end and return

      END
