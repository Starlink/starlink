
*+ HISTO - generates an intensity histogram of an image

      SUBROUTINE HISTO ( STATUS )

*    Description :
*
*    An intensity histogram of a user specified sub-array of an image
*    and then certain statistical parameters are found from the histogram.
*    The routine will be later extended to allow interactive display and
*    manipulation of the histogram, and the subroutines used will be
*    utilised elsewhere in histogram equalisation programs, sky background
*    determination and so on.
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*           Image to be histogrammed
*     XSTART  =  INTEGER( READ )
*           x start coord of sub-array to be included
*     YSTART  =  INTEGER( READ )
*           y start coord of sub-array to be included
*     XSIZE  =  INTEGER( READ )
*           x size of sub-array to be included
*     YSIZE  =  INTEGER( READ )
*           y size of sub-array to be included
*     NUMPIX  =  INTEGER( WRITE )
*           Number of pixels included in sub-array
*     VALMAX  =  REAL( WRITE )
*           Maximum intensity value in sub-array
*     VALMIN  =  REAL( WRITE )
*           Minimum intensity value in sub-array
*     SUB  =  REAL( WRITE )
*           Total of all pixels in sub-array
*     MEAN  =  REAL( WRITE )
*           Mean of all pixels in sub-array
*     MEDIAN  =  REAL( WRITE )
*           Median of all pixels in sub-array
*     MODE  =  REAL( WRITE )
*           Mode of all pixels in sub-array
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get input image structure
*     If no error so far then
*        Map its DATA_ARRAY component
*        Output image dimensions to the user
*        Get position of sub-array to be examined
*        Get number of histogram bins to be used in calculation
*        If no error so far then
*           Call MAXMIN to find the maximum and minimum in sub-array
*           Call GENHIS to generate the histogram of the sub-array
*           Call HISTPROP to calculate the desired statistics
*           Output information to the user
*        Endif
*        Tidy up input data structure
*     Endif
*     End
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*
*    History :
*
*     31-07-1985 : First SSE/ADAM implementation, which just does
*                : basic statistics. (UKTH::MARK)
*     15-04-1986 : Tidied up error checking (REVA::MJM)
*     24-11-1986 : Fixed bug in max,min finding (HILO::MJM)
*     11-MAR-94    Changed DAT_, CMP_ to NDF_ (SKL@JACH)
*     12-MAR-1994  Changed input DIM arguments for GENHIS and MAXMIN (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE              ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'          ! global SSE definitions
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'

*    Status :

      INTEGER STATUS             ! global status parameter

*    Local Constants :

      INTEGER NDIMS              ! image dimensionality
      INTEGER MAXBIN             ! maximum number of histogram bins
      PARAMETER( NDIMS = 2 )     ! 2-d only
      PARAMETER( MAXBIN = 2048 ) ! should be enough

*    Local variables :

      INTEGER
     :    LOCI,                  ! locator for input data structure
     :    NELEMENTS,             ! number of elements mapped
     :    NDIM,                  ! dimensions from NDF_DIM
     :    IDIMS( NDIMS ),        ! dimensions of input image
     :    PNTRI,                 ! pointer to input DATA_ARRAY component
     :    XSTART,                ! x start coord of sub-array
     :    YSTART,                ! y   "     "   "   "    "
     :    XFINISH,               ! x end     "   "   "    "
     :    YFINISH,               ! y  "      "   "   "    "
     :    XSIZE,                 ! x size of box to be taken
     :    YSIZE,                 ! y   "   "  "   "  "   "
     :    NUMBIN,                ! number of bins in histogram
     :    HIST( MAXBIN ),        ! array containing histogram
     :    NUMPIX,                ! total number of pixels in sub-array
     :    MAXPOS( 2 ),           ! position of maximum value in sub-array
     :    MINPOS( 2 )            !     "     " minimum   "    "  "    "

      REAL
     :    MAXIMUM,               ! maximum intensity value in sub-array
     :    MINIMUM,               ! minimum     "       "   "   "    "
     :    SUM,                   ! total sum of pixels in sub-array
     :    MEAN,                  ! mean value of pixels in sub-array
     :    MEDIAN,                ! median value of pixels in sub-array
     :    MODE                   ! mode of pixels in sub-array


*-
*    check status on entry
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    get a locator to input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    if no error so far then continue
      IF ( STATUS .EQ. SAI__OK ) THEN

*       map the DATA_ARRAY component of the input data structure
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )

*       get dimensions
         CALL NDF_DIM( LOCI, NDIMS, IDIMS, NDIM, STATUS )

*       tell user dimensions of input array
         CALL MSG_SETI( 'XDIM', IDIMS(1) )
         CALL MSG_SETI( 'YDIM', IDIMS(2) )
         CALL MSG_OUT( 'INPUT_DIMS',
     :        'Image is ^XDIM by ^YDIM pixels', STATUS )

*       get x and y start coords of sub-array to be processed
         CALL AIF_GET0I( 'XSTART', 1, 1, IDIMS(1), XSTART, STATUS )
         CALL AIF_GET0I( 'YSTART', 1, 1, IDIMS(2), YSTART, STATUS )

*       get x and y sizes of sub-array to be processed
         CALL AIF_GET0I( 'XSIZE', IDIMS(1)-XSTART+1, 1,
     :                    IDIMS(1)-XSTART+1, XSIZE, STATUS )
         CALL AIF_GET0I( 'YSIZE', IDIMS(2)-YSTART+1, 1,
     :                    IDIMS(2)-YSTART+1, YSIZE, STATUS )

*       calculate the box finish coordinates
         XFINISH  =  XSTART + XSIZE - 1
         YFINISH  =  YSTART + YSIZE - 1

*       get the number of bins to be used in the histogram - suggest
*       maximum as a default
         CALL AIF_GET0I( 'NUMBIN', MAXBIN, 1, MAXBIN, NUMBIN, STATUS )

*       check for error before accessing pointers
         IF ( STATUS .EQ. SAI__OK ) THEN

*          get maximum and minimum values in sub-array
            CALL MAXMIN( IDIMS(1), IDIMS(2), %VAL( PNTRI ), XSTART,
     :                   YSTART, XFINISH, YFINISH, NUMPIX, MAXIMUM,
     :                   MINIMUM, MAXPOS, MINPOS, STATUS )

*          now call subroutine to generate histogram
            CALL GENHIS( IDIMS(1), IDIMS(2), %VAL( PNTRI ), XSTART,
     :                   YSTART, XFINISH, YFINISH, MAXIMUM, MINIMUM,
     :                   NUMBIN, HIST, STATUS )

*          now call subroutine that calculates statistical parameters
*          from the histogram
            CALL HISTPROP( HIST, NUMBIN, MAXIMUM, MINIMUM, SUM, MEAN,
     :                     MEDIAN, MODE, STATUS )

*          write out parameters to user
            CALL MSG_SETI( 'NUMPIX', NUMPIX )
            CALL MSG_OUT( 'HISTO_NUMPIX',
     :        'Number of pixels in sub-array     = ^NUMPIX', STATUS )
	    CALL PAR_PUT0I( 'HISTO_NUMPIX', NUMPIX, STATUS)

            CALL MSG_SETR( 'VALMAX', MAXIMUM )
            CALL MSG_OUT( 'HISTO_VALMAX',
     :        'Maximum value in sub-array is     = ^VALMAX', STATUS )
	    CALL PAR_PUT0R( 'HISTO_MAX', MAXIMUM, STATUS)

            CALL MSG_SETI( 'MAXXPOS', MAXPOS( 1 ) )
            CALL MSG_SETI( 'MAXYPOS', MAXPOS( 2 ) )
            CALL MSG_OUT( 'HISTO_MAXPOS',
     :    'Location of maximum               = ^MAXXPOS,^MAXYPOS',
     :     STATUS )
	    CALL PAR_PUT0I( 'HISTO_XMAX', MAXPOS( 1), STATUS)
	    CALL PAR_PUT0I( 'HISTO_YMAX', MAXPOS( 2), STATUS)

            CALL MSG_SETR( 'VALMIN', MINIMUM )
            CALL MSG_OUT( 'HISTO_VALMIN',
     :        'Minimum value in sub-array is     = ^VALMIN', STATUS )
	    CALL PAR_PUT0R( 'HISTO_MIN', MINIMUM, STATUS)

            CALL MSG_SETI( 'MINXPOS', MINPOS( 1 ) )
            CALL MSG_SETI( 'MINYPOS', MINPOS( 2 ) )
            CALL MSG_OUT( 'HISTO_MINPOS',
     :    'Location of minimum               = ^MINXPOS,^MINYPOS',
     :     STATUS )
	    CALL PAR_PUT0I( 'HISTO_XMIN', MINPOS( 1), STATUS)
	    CALL PAR_PUT0I( 'HISTO_YMIN', MINPOS( 2), STATUS)

            CALL MSG_SETR( 'SUM', SUM )
            CALL MSG_OUT( 'HISTO_SUM',
     :        'Sum of pixels in sub-array is     = ^SUM', STATUS )
	    CALL PAR_PUT0R( 'HISTO_SUM', SUM, STATUS)

            CALL MSG_SETR( 'MEAN', MEAN )
            CALL MSG_OUT( 'HISTO_MEAN',
     :        'Mean of pixels in sub-array is    = ^MEAN', STATUS )
	    CALL PAR_PUT0R( 'HISTO_MEAN', MEAN, STATUS)

            CALL MSG_SETR( 'MEDIAN', MEDIAN )
            CALL MSG_OUT( 'HISTO_MEDIAN',
     :        'Median of pixels in sub-array is  = ^MEDIAN', STATUS )
	    CALL PAR_PUT0R( 'HISTO_MEDIAN', MEDIAN, STATUS)

            CALL MSG_SETR( 'MODE', MODE )
            CALL MSG_OUT( 'HISTO_MODE',
     :        'Mode of pixels in sub-array is    = ^MODE', STATUS )
	    CALL PAR_PUT0R( 'HISTO_MODE', MODE, STATUS)

*       end of if-no-error-before-accessing-pointers check
         END IF

*       tidy up the input data structure
         CALL NDF_ANNUL( LOCI, STATUS )

*    end of if-no-error-after-getting-input-structure check
      END IF


*    end
      END
