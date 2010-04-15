
*+  CENTROID - finds the centroids of star-like image features

      SUBROUTINE CENTROID ( STATUS )

*    Description :
*
*     This routine takes a 2-d image and returns the x,y coordinates
*     of star-like features in that image, as identified by the user
*     via the input of initial guess coordinates. The program loops
*     asking for new stars until told to quit.
*
*    Invocation :
*
*     CALL CENTROID( STATUS )
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*           Image to be analysed
*     XINIT  =  REAL( READ )
*           x guess coordinate of star to be centroided
*     YINIT  =  REAL( READ )
*           y start coordinate of star to be centroided
*     SEARCH  =  INTEGER( READ )
*           Size of search box to be used
*     POSITIVE  =  LOGICAL( READ )
*           True if image features are positive above background
*     MAXSHIFT  =  REAL( READ )
*           Maximum shift allowed between guess and output positions
*     MAXITER  =  INTEGER( READ )
*           Maximum number of iterations to be used in search
*     TOLER  =  REAL( READ )
*           Accuracy required in centroiding
*     ANOTHER  =  LOGICAL( READ )
*           Whether or not another star is to be centroided
*
*    Method :
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
*
*     14-01-1986 :  First implementation
*                :  (REVA::MJM)
*     10-MAR-94     Changed DAT_, CMP_ calls to NDF_ (SKL@JACH)
*     12-AUG-1994   Changed input DIM arguments for CENTROID_LOCATE (SKL@JACH)
*
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
      REAL INVALID                ! invalid pixel flag
      PARAMETER ( INVALID = -32767.0 )  ! set to this for now

*    Local variables :

      INTEGER
     :    LOCI,           ! input data structure
     :    DIMS( NDIMS ),  ! dimensions of input DATA_ARRAY
     :    NDIM,           ! Number dimensions from NDF_DIM
     :    NELEMENTS,      ! Number of elements mapped,
     :    PNTRI,          ! pointer to input DATA_ARRAY component
     :    SEARCH,         ! size of search box to be used
     :    MAXITER,        ! maximum number of iterations to be used
     :    ERROR           ! used to record error returns from LOCATE

      REAL
     :    XINIT,          ! input x coordinate guess position
     :    YINIT,          !   "   y      "       "       "
     :    XFINAL,         ! output calculated x position
     :    YFINAL,         !    "        "     y     "
     :    MAXSHIFT,       ! max shift allowed between guess and output
     :    TOLER           ! accuracy required in centroiding


      LOGICAL
     :    POSITIVE,       ! true if image features are positive in sign
     :    ANOTHER         ! true if user wants to locate another star

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    get locator to the input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    map in its DATA_ARRAY component
      CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :               PNTRI, NELEMENTS, STATUS )

*    get dimensions of array
      CALL NDF_DIM( LOCI, NDIMS, DIMS, NDIM, STATUS)

*    get the relevant parameters for centroiding
      CALL AIF_GET0I( 'SEARCH', 9, 1, 21, SEARCH, STATUS )
      CALL AIF_GET0I( 'MAXITER', 3, 1, 9, MAXITER, STATUS )
      CALL PAR_GET0L( 'POSITIVE', POSITIVE, STATUS )
      CALL AIF_GET0R( 'MAXSHIFT', 5.0, 0.0, 20.0, MAXSHIFT, STATUS )
      CALL AIF_GET0R( 'TOLER', 0.05, 0.0, 2.0, TOLER, STATUS )

*    if everything is o.k. up to now then proceed
      IF ( STATUS .EQ. SAI__OK ) THEN

*       initialise the ANOTHER logical
         ANOTHER =  .TRUE.

*       loop whilst the user wants to locate another star
         DO WHILE( ANOTHER )

*          get the initial guess coordinates, setting up the dynamic
*          defaults such that the point is actually on the array
            CALL AIF_GET0R( 'XINIT', 1.0, 0.0, REAL( DIMS( 1 ) ),
     :                       XINIT, STATUS )
            CALL AIF_GET0R( 'YINIT', 1.0, 0.0, REAL( DIMS( 2 ) ),
     :                       YINIT, STATUS )

*          call the subroutine that does the actual work next
            CALL CENTROID_LOCATE( %VAL( PNTRI ), DIMS(1), DIMS(2),
     :                            XINIT, YINIT, SEARCH, POSITIVE,
     :                            MAXSHIFT, MAXITER, TOLER, INVALID,
     :                            XFINAL, YFINAL, ERROR, STATUS )

*          on return, check the value of the error variable, and act
*          accordingly

            IF (STATUS .EQ. SAI__OK) THEN

               IF ( ERROR .EQ. 1 ) THEN

*                there was no data in the given search area
                  CALL MSG_OUT( 'NO_DATA',
     :  'No data in given search area - please try again', STATUS )

               ELSE IF ( ERROR .EQ. 2 ) THEN

*                the maximum shift allowable was exceeded
                  CALL MSG_OUT( 'SHIFT_EXCEED',
     :  'Maximum shift was exceeded - please try again', STATUS )

               ELSE

*                no error encountered by LOCATE - output results
                  CALL MSG_OUT( 'BLANK', ' ', STATUS )
                  CALL MSG_SETR( 'XINIT', XINIT )
                  CALL MSG_SETR( 'YINIT', YINIT )
                  CALL MSG_OUT( 'INPUT_POS',
     :   'Input guess position was    ^XINIT, ^YINIT', STATUS )
                  CALL MSG_SETR( 'XFINAL', XFINAL )
                  CALL MSG_SETR( 'YFINAL', YFINAL )
                  CALL MSG_OUT( 'OUTPUT_POS',
     :   'Output centroid position is ^XFINAL, ^YFINAL', STATUS )
                  CALL MSG_OUT( 'BLANK', ' ', STATUS )
	          CALL PAR_PUT0R( 'XFINAL', XFINAL, STATUS)
	          CALL PAR_PUT0R( 'YFINAL', YFINAL, STATUS)

               END IF

            ELSE

                  CALL ERR_REP('ERR','Bad status after LOCATE',
     :                          STATUS)

            END IF

*          now ask whether or not another star is to be located
            CALL PAR_GET0L( 'ANOTHER', ANOTHER, STATUS )

*          annul parameters before looping and reset the error variable
            CALL PAR_CANCL( 'XINIT', STATUS )
            CALL PAR_CANCL( 'YINIT', STATUS )
            CALL PAR_CANCL( 'ANOTHER', STATUS )
            ERROR  =  0

         END DO

      END IF

*    tidy up the input data structure
      CALL NDF_ANNUL( LOCI, STATUS )

*    end

      END
