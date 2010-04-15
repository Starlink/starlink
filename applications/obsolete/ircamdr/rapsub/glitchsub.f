
*+  GLITCHSUB - replaces pixel value with local median

      SUBROUTINE GLITCHSUB( DIMS1, DIMS2, ARRAY, XCOORD, YCOORD,
     :                      OLDVAL, NEWVAL, STATUS )

*    Description :
*
*     This routine replaces a specified pixel with the median of the
*     eight surrounding pixels, or less if pixel adjoins an edge.
*     The median is defined from a histogram of the surrounding
*     eight pixels.
*
*    Parameters :
*
*     DIMS( 2 ) = INTEGER( READ )
*           The dimensions of the input array
*     ARRAY( DIMS(1), DIMS(2) ) = REAL( READ, WRITE )
*           The input data aray
*     XCOORD = INTEGER( READ )
*           The x coordinate of the pixel to be replaced
*     YCOORD = INTEGER( READ )
*           The y coordinate of the pixel to be replaced
*     OLDVAL = REAL( WRITE )
*           Old value of deglitched pixel
*     NEWVAL = REAL( WRITE )
*           New value of deglitched pixel
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Initialise variables and valid pixel arrays
*     For all pixels immediately surrounding bad pixel
*        If pixel is not bad one and is on array then
*           Include it in valid pixel arrays, and update the
*             max and min
*        Endif
*     Endfor
*     If max=min then
*        Set NEWVAL = max and return immediately
*        Set pixel to be deglitched to be NEWVAL
*     Endif
*     Work out histogram scaling factor from max and min
*     Initialise histogram bins
*     For all pixels immediately surrounding bad one
*        If a valid pixel ten
*           Work out its histogram bin
*           Increment that histogram bin by one
*        Endif
*     Endfor
*     Call HISTPROP to work out properties of histogram
*     Set NEWVAL and array pixel to returned median value
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
*     07-06-1985 : Original implementation for the SSE (REVA::MJM)
*     28-06-1985 : Changed to consider ARRAY as Import/Export, and
*                : just to update it, not some new array. (REVA::MJM)
*     11-04-1986 : Major overhaul to implement median replacement
*                : by histogram technique (REVA::MJM)
*     04-09-1986 : Bug fix - when max=min over 3x3 box around 'bad' pixel
*                : set NEWVAL and bad pixel to max before returning
*     15-JUL-1994  Changed arguments to input DIMS separately
*                  so that routine will compile (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT  NONE           ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'       ! global SSE definitions

*    Status :

      INTEGER  STATUS          ! global status parameter

*    Import :

      INTEGER
     :    DIMS1,           ! dimensions of input array
     :    DIMS2,           ! dimensions of input array
     :    XCOORD,              ! x coord of pixel to be deglitched
     :    YCOORD               ! y coord of pixel to be deglitched

*    Import-Export :

      REAL
     :    ARRAY( DIMS1, DIMS2 )   ! input data array

*    Export :

      REAL
     :    OLDVAL,              ! old value of pixel
     :    NEWVAL               ! new value of pixel

*    Local constants :

      INTEGER
     :    NUMBIN,              ! number of bins in histogram
     :    SIZVAL               ! size of valid pixel array
      PARAMETER( NUMBIN = 2048 )
      PARAMETER( SIZVAL = 9 )  ! only looking at 3x3 box

*    Local variables :

      INTEGER
     :    HIST( NUMBIN ),      ! histogram array
     :    DUMMY,               ! variable used as index into histogram
     :    I, J, K, L, M, N     ! counter variables

      REAL
     :    VALARRAY( SIZVAL ),  ! array containing valid pixel values
     :    MAXIMUM,             ! maximum found amongst the valid pixels
     :    MINIMUM,             !    "      "      "     "    "     "
     :    SCALE,               ! scale used when forming histogram
     :    SUM,                 ! sum returned by HISTPROP
     :    MEAN,                ! mean    "     "     "
     :    MEDIAN,              ! median  "     "     "
     :    MODE                 ! mode    "     "     "

      LOGICAL                  ! true if :
     :    VALPIX( SIZVAL ),    ! surrounding pixel is valid
     :    CENTRAL,             ! current pixel is the central one
     :    ONARRAY              ! surrounding pixel is on the array

*-
*    check for error on entry - return if not ok
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

*    record old value
      OLDVAL  =  ARRAY( XCOORD, YCOORD )

*    set up valid pixel arrays
      DO  L  =  1, SIZVAL
         VALPIX( L )    =  .FALSE.
         VALARRAY( L )  =  0.0
      END DO

*    initialise pixel counter and max/min variables
      N  =  0
      MAXIMUM  =  -1.0E20
      MINIMUM  =   1.0E20

*    scan the surrounding pixels to find the max and min of
*    those that are actually on the array - loop the rows first
      DO  J  =  YCOORD-1, YCOORD+1

*       loop the pixels in the current row
         DO  I  =  XCOORD-1, XCOORD+1

*          increment pixel counter
            N  =  N + 1

*          set up the central pixel logical and the 'on-array'
*          pixel logical
            CENTRAL  =  ( I .EQ. XCOORD .AND. J .EQ. YCOORD )
            ONARRAY  =  ( I .GE. 1 .AND. I .LE. DIMS1 .AND.
     :                    J .GE. 1 .AND. J .LE. DIMS2 )

*          check this pixel is on the image and that it is not
*          the not the central pixel
            IF ( ONARRAY .AND. .NOT. CENTRAL ) THEN

*             this is a valid pixel for the histogram - update
*             the maximum and minimum, flag the pixel, and store
*             its value
               MAXIMUM  =  MAX( ARRAY( I, J ), MAXIMUM )
               MINIMUM  =  MIN( ARRAY( I, J ), MINIMUM )
               VALPIX( N )    =  .TRUE.
               VALARRAY( N )  =  ARRAY( I, J )

*          end of valid-pixel check
            END IF

*       end of loop round pixels in current row
         END DO

*    end of loop round rows
      END DO

*    check for max=min - return immediately with NEWVAL and central
*    pixel set to max
      IF ( MAXIMUM .EQ. MINIMUM ) THEN
         NEWVAL  =  MAXIMUM
         ARRAY( XCOORD, YCOORD ) = MAXIMUM
         RETURN
      END IF

*    set up the histogram scaling factor, checking for a number too small
      IF ( ABS( MAXIMUM - MINIMUM ) .GT. 1.0E-20 ) THEN
         SCALE  =  MAXIMUM - MINIMUM
      ELSE
         SCALE  =  1.0
      END IF

*    initialise the histogram bins to zero
      DO  K  =  1, NUMBIN
         HIST( K )  =  0
      END DO

*    now calculate the histogram from the valid pixel map
      DO  M  =  1, SIZVAL

*       check for a valid pixel
         IF ( VALPIX( M ) ) THEN

*          find bin number for current valid pixel
            DUMMY  =  NINT( NUMBIN * (VALARRAY(M) - MINIMUM) / SCALE )

*          check that the calculated bin stays in range
            DUMMY  =  MIN( NUMBIN, MAX( 1, DUMMY ) )

*          increment histogram bin by one
            HIST( DUMMY ) = HIST( DUMMY ) + 1

*       end of if-valid-pixel check
         END IF

*    end of loop round surrounding pixels
      END DO

*    now call HISTPROP to get us the median value of the surrounding
*    pixels
      CALL HISTPROP( HIST, NUMBIN, MAXIMUM, MINIMUM, SUM, MEAN,
     :               MEDIAN, MODE, STATUS )

*    set the new pixel value to be the median as found by HISTPROP
      NEWVAL  =  MEDIAN
      ARRAY( XCOORD, YCOORD )  =  NEWVAL


*    end and return
      END
