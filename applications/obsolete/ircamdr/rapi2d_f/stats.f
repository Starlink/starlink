*+ STATS - works out image statistics

      SUBROUTINE STATS ( STATUS )

*    Description :
*
*     This routine takes a 2-d image and outputs information
*     about that image, including dimensions, total, mean,
*     maximum, minimum, and standard deviation. The region
*     analysed is a sub-image of specified size and position.
*
*    Invocation :
*
*     CALL STATS( STATUS )
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*           Image to be analysed
*     XSTART =  INTEGER( READ )
*           x start coordinate of sub-array to be analysed
*     YSTART =  INTEGER( READ )
*           y start coordinate of sub-array to be analysed
*     XSIZE  =  INTEGER( READ )
*           x size of sub-array to be analysed
*     YSIZE  =  INTEGER( READ )
*           y size of sub-array to be analysed
*     AGAIN  =  LOGICAL( READ )
*           Whether or not another sub-array is to be analysed
*
*    Method :
*
*     If error on entry then return immediately
*     Get input image from environment
*     If no error so far then
*        Map its DATA_ARRAY component
*        Output dimensions of input image to user
*        Do while user wants to analyse another sub-array
*           Get start coords of sub-array
*           Get size of sub-array making sure it stays on array
*           If no error so far then
*              Call STATSSUB to do actual work
*              Call STATSSUB2 to calculate the additional statistics
*              Output results to environment
*           Endif
*           Ask whether another sub-array to be analysed
*           Cancel parameters associated with sub-array
*        Enddo
*     Endif
*     End
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*
*    History :
*     09-12-1985 : First implementation (UKTH::MARK)
*     17-01-1986 : Improved error checking and tidied (REVA::MJM)
*     26-11-1986 : added par_put's for means,stds (UKIRT::CAA)
*     22-08-1988 : added additional statistics (JACH::CAA)
*     12_Apr-1994  changed DAT and CMP calls to NDF (SKL@JACH)
*    Endhistory

*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'NDF_PAR'
      INCLUDE  'NDF_ERR'

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local constants :

      INTEGER NDIMS               ! input image dimensionality
      PARAMETER ( NDIMS = 2 )     ! 2-d images only

*    Local variables :

      INTEGER
     :  LOCI,           ! input data structure
     :  DIMS( NDIMS ),  ! dimensions of input DATA_ARRAY
     :  NELEMENTS,       ! number of elements mapped by NDF_MAP
     :  NDIM,           ! number of dimensions from NDF_DIM
     :  PNTRI,          ! pointer to input DATA_ARRAY component
     :  XSTART,         ! x start coord of sub-array
     :  YSTART,         ! y   "     "    "  "    "
     :  XSIZE,          ! x size of sub-array
     :  YSIZE,          ! y   "   "  "    "
     :  NUMPIX,         ! number of pixels in sub-array
     :  XSIZEMAX,       ! maximum permissible sub-array in x
     :  YSIZEMAX        !    "         "       "    "   in y

      REAL
     :  TOTAL,          ! total of pixels in sub-array
     :  MEAN,           ! mean of pixels in sub-array
     :  STDDEV,         ! standard deviation of pixels in sub-array
     :  MAXIMUM,        ! maximum pixel value in sub-array
     :  MINIMUM,        ! minimum   "     "    "  "    "
     :	MEDIAN,         ! median    "     "    "  "    "
     :	MODE            ! mode      "     "    "  "    "

      LOGICAL                 ! true if :
     :  AGAIN                 ! user wants to do another sub-array

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    get locator to the input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    if no error so far then continue
      IF ( STATUS .EQ. SAI__OK ) THEN

*       map in its DATA_ARRAY component
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )

         CALL NDF_DIM( LOCI, NDIMS, DIMS, NDIM, STATUS )

*       output the dimensions of the image to the user
         CALL MSG_SETI( 'XDIM', DIMS( 1 ) )
         CALL MSG_SETI( 'YDIM', DIMS( 2 ) )
         CALL MSG_OUT( 'INPUT_DIMS',
     :             'Image is ^XDIM by ^YDIM pixels', STATUS )

*       initialise the AGAIN logical
         AGAIN  =  .TRUE.

*       loop whilst the user wants another sub-array
         DO WHILE( AGAIN .AND. ( STATUS .EQ. SAI__OK ) )

*          get the sub-array start coordinates and box size, setting
*          dynamic defaults such that the chosen sub-array does not
*          fall off the input array
            CALL AIF_GET0I( 'XSTART', 1, 1, DIMS( 1 ), XSTART, STATUS )
            CALL AIF_GET0I( 'YSTART', 1, 1, DIMS( 2 ), YSTART, STATUS )

            XSIZEMAX  =  10000
            YSIZEMAX  =  10000

            CALL AIF_GET0I( 'XSIZE', XSIZEMAX, 1, XSIZEMAX, XSIZE,
     :                       STATUS )
            CALL AIF_GET0I( 'YSIZE', YSIZEMAX, 1, YSIZEMAX, YSIZE,
     :                       STATUS )

*          check that sub-array limits are within image and set to max
*          allowed if they aren't
	    IF( ( XSTART + XSIZE -1) .GT. DIMS( 1)) THEN
	      XSIZE = DIMS( 1) - XSTART + 1
	    END IF

	    IF( ( YSTART + YSIZE -1) .GT. DIMS( 2)) THEN
	      YSIZE = DIMS( 2) - YSTART + 1
	    END IF

*          check for error before calling subroutine
            IF ( STATUS .EQ. SAI__OK ) THEN

*             now call the subroutine to do the work

               CALL STATSSUB( %VAL( PNTRI ), DIMS( 1), DIMS( 2),
     :	                      XSTART, YSTART, XSIZE, YSIZE, NUMPIX,
     :                        MAXIMUM, MINIMUM, TOTAL, MEAN,
     :                        STDDEV, STATUS )

               CALL STATSSUB2( %VAL( PNTRI), DIMS( 1), DIMS( 2),
     :	                       XSTART, YSTART, XSIZE, YSIZE, MEDIAN,
     :                         MODE)

*             on return, display the results

               CALL MSG_OUT( 'BLANK', ' ', STATUS )

               CALL MSG_SETI( 'NUMPIX', NUMPIX )
               CALL MSG_OUT( 'STATS_NUMPIX',
     :       'Number of pixels in calculation      =  ^NUMPIX',
     :        STATUS )
	       CALL PAR_PUT0I( 'NUMPIX', NUMPIX, STATUS)

               CALL MSG_SETR( 'MAXIMUM', MAXIMUM )
               CALL MSG_OUT( 'STATS_MAX',
     :       'Maximum pixel value found            =  ^MAXIMUM',
     :        STATUS )

               CALL MSG_SETR( 'MINIMUM', MINIMUM )
               CALL MSG_OUT( 'STATS_MIN',
     :       'Minimum pixel value found            =  ^MINIMUM',
     :        STATUS )

               CALL MSG_SETR( 'TOTAL', TOTAL )
               CALL MSG_OUT( 'STATS_TOTAL',
     :       'Total of all pixels added            =  ^TOTAL',
     :        STATUS )

               CALL MSG_SETR( 'MEAN', MEAN )
               CALL MSG_OUT( 'STATS_MEAN',
     :       'Mean of all pixels                   =  ^MEAN',
     :        STATUS )
	       CALL PAR_PUT0R( 'MEAN', MEAN, STATUS)

               CALL MSG_SETR( 'STDDEV', STDDEV )
               CALL MSG_OUT( 'STATS_STDDEV',
     :       'Standard deviation of all pixels     =  ^STDDEV',
     :        STATUS )
	       CALL PAR_PUT0R( 'STD', STDDEV, STATUS)

               CALL MSG_OUT( 'BLANK', ' ', STATUS )

               CALL MSG_SETR( 'MEDIAN', MEDIAN )
               CALL MSG_OUT( 'STATS_MEDIAN',
     :       'Median value of all pixels           =  ^MEDIAN',
     :        STATUS )
	       CALL PAR_PUT0R( 'MEDIAN', MEDIAN, STATUS)

               CALL MSG_SETR( 'MODE', MODE )
               CALL MSG_OUT( 'STATS_MODE',
     :       'Mode of all pixels                   =  ^MODE',
     :        STATUS )
	       CALL PAR_PUT0R( 'MODE', MODE, STATUS)

               CALL MSG_OUT( 'BLANK', ' ', STATUS )

*          end of if-no-error-then-call-subroutine check
            END IF

*          ask whether user wants another sub-array
            CALL PAR_GET0L( 'AGAIN', AGAIN, STATUS )

*          annul parameter values before looping
            CALL PAR_CANCL( 'XSTART', STATUS )
            CALL PAR_CANCL( 'YSTART', STATUS )
            CALL PAR_CANCL( 'XSIZE', STATUS )
            CALL PAR_CANCL( 'YSIZE', STATUS )
            CALL PAR_CANCL( 'AGAIN', STATUS )

            CALL MSG_OUT( 'BLANK', ' ', STATUS )

*       end of do-while-again loop
         END DO

*       tidy up the input data structure
         CALL NDF_ANNUL( LOCI, STATUS )

*    end of if-error-after-getting-input-image-locator check
      END IF

*    end

      END
