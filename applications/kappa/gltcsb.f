*+  GLTCSB - replaces pixel value with local median within a 2-d image

      SUBROUTINE GLTCSB( DIM1, DIM2, XCOORD, YCOORD, ARRAY, OLDVAL,
     :                   NEWVAL, STATUS )
*
*    Description :
*
*     This routine replaces a specified pixel with the median of the
*     eight surrounding pixels, or less if pixel adjoins an edge.
*     The median is defined from a histogram of the surrounding
*     eight pixels. There must be at least 3 non-magic-value pixels
*     in the surrounding pixels, otherwise the result is undefined.
*
*    Invocation :
*
*      CALL GLTCSB( DIM1, DIM2, XCOORD, YCOORD, ARRAY, OLDVAL, NEWVAL,
*                   STATUS )
*
*    Arguments :
*
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d array.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d array.
*     XCOORD = INTEGER( READ )
*         The x co-ordinate of the pixel to be replaced
*     YCOORD = INTEGER( READ )
*         The y co-ordinate of the pixel to be replaced
*     ARRAY( DIM1, DIM2 ) = REAL( READ, WRITE )
*         The input data array, deglitched on exit
*     OLDVAL = REAL( WRITE )
*         Old value of deglitched pixel
*     NEWVAL = REAL( WRITE )
*         New value of deglitched pixel
*     STATUS  =  INTEGER( READ )
*         Global status parameter
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Initialise variables and valid pixel arrays
*     For all pixels immediately surrounding bad pixel
*        If pixel is not the bad (central) one, is valid and is within
*          the array then
*           Include it in valid pixel arrays, and update the
*             max and min, and count of valid pixels
*        Endif
*     Endfor
*     If number of valid pixels is too small
*        Set new value to be bad 
*     Elseif max equals min then 
*        Set new value to be maximum
*     Else
*        Compute position of median in sample using equal weighting
*        Sort sample array until median pixel is found, note if there
*          are an even number of pixels, the average of the two spanning
*          the median are averaged
*        New value becomes the median value
*     Endif
*     Output value is the median value
*     End
*
*    Bugs :
*
*     None are known at this time.
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     07-06-1985 : Original implementation for the SSE (REVA::MJM)
*     28-06-1985 : Changed to consider ARRAY as Import/Export, and
*                : just to update it, not some new array. (REVA::MJM)
*     11-04-1986 : Major overhaul to implement median replacement
*                : by histogram technique (REVA::MJM)
*     1986 Aug 13: Renamed from GLITCHSUB, renamed HISTPROP to HSTPRP
*                  completed prologue, and nearly conformed to Starlink
*                  programming standards (RL.STAR::CUR).
*     1986 Sep 4 : Renamed parameters section to arguments, applied
*                  bad-pixel handling and bug fix (RL.STAR::CUR).
*     1988 Sep 2 : Copes with almost all pixels invalid (RL.STAR::CUR).
*     1989 Jul 27: Passed array dimensions as separate variables and
*                  improved the efficiency when almost all pixels are
*                  invalid (RL.STAR::CUR).
*     1989 Sep 5 : Used sorting algorithm, rather than the histogram
*                  technique which is inefficient (RL.STAR::CUR).
*
*    Type Definitions :

      IMPLICIT  NONE           ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'       ! global SSE definitions
      INCLUDE  'PRM_PAR'       ! Primdat parameter definitions

*    Status :

      INTEGER  STATUS

*    Import :

      INTEGER
     :    DIM1, DIM2,
     :    XCOORD,
     :    YCOORD

*    Import-Export :

      REAL
     :    ARRAY( DIM1, DIM2 )

*    Export :

      REAL
     :    OLDVAL,
     :    NEWVAL

*    Local constants :

      INTEGER
     :    MINPIX,              ! Minimum number of valid pixel to form
                               ! histogram and compute median
     :    SIZVAL               ! Size of valid pixel array
      PARAMETER( MINPIX = 3 )  ! 2 is too small
      PARAMETER( SIZVAL = 9 )  ! only looking at 3x3 box     

*    Local variables :

      INTEGER
     :  ACTMED,                ! Actual median position in sorted values
     :  CURR,                  ! Index to current smallest value during
                               ! sorting
     :  INDEX,                 ! Index to sample elements
     :  NUMVAL,                ! Counter of number of valid pixels
     :  SUMWT,                 ! Sum of weights used to locate median
                               ! during sort
     :  TEST,                  ! Index to sample element for testing
                               ! against CURR
     :  I, J                   ! Counter variables

      REAL
     :  CURVAL,                ! Current smallest value during sorting
     :  VALARR( SIZVAL ),      ! Array containing valid pixel values
     :  MAXMUM,                ! Maximum found amongst the valid pixels
     :  MINMUM                 ! Minimum   "      "     "    "     "

      LOGICAL                  ! True if:
     :  CENTRL,                ! Current pixel is the central one
     :  ONARR                  ! Current pixel is on the array

*-
*    check for error on entry - return if not ok

      IF ( STATUS .NE. SAI__OK ) RETURN

*    record old value

      OLDVAL  =  ARRAY( XCOORD, YCOORD )

*    set up valid pixel arrays

      DO  I  =  1, SIZVAL
         VALARR( I )  =  0.0
      END DO

*    initialise pixel counter and max/min variables

      NUMVAL = 0
      MAXMUM  =  VAL__MINR
      MINMUM  =  VAL__MAXR

*    scan the surrounding pixels to find the max and min of
*    those that are actually on the array - loop the lines first

      DO  J  =  YCOORD-1, YCOORD+1

*       loop the pixels in the current line

         DO  I  =  XCOORD-1, XCOORD+1

*          set up the central pixel logical and the 'on-array'
*          pixel logical

            CENTRL  =  ( I .EQ. XCOORD .AND. J .EQ. YCOORD )
            ONARR  =  ( I .GE. 1 .AND. I .LE. DIM1 .AND.
     :                  J .GE. 1 .AND. J .LE. DIM2 )

*          check this pixel is on the image and that it is not
*          the not the central pixel

            IF ( ONARR .AND. .NOT. CENTRL ) THEN

*             check that the pixel is not bad

               IF ( ARRAY( I, J ) .NE. VAL__BADR ) THEN

*                one more valid pixel

                  NUMVAL = NUMVAL + 1

*                this is a valid pixel for the histogram - update
*                the maximum and minimum, flag the pixel, and store
*                its value

                  MAXMUM  =  MAX( ARRAY( I, J ), MAXMUM )
                  MINMUM  =  MIN( ARRAY( I, J ), MINMUM )
                  VALARR( NUMVAL )  =  ARRAY( I, J )

*             end of valid-pixel checks

               END IF

            END IF

*       end of loop round pixels in current line

         END DO

*    end of loop round lines

      END DO

*    check that sufficient valid pixels are present in the histogram

      IF ( NUMVAL .LE. MINPIX ) THEN
         NEWVAL  =  VAL__BADR

*    check for max=min -- return immediately with NEWVAL set to maximum
*    and central pixel set to maximum

      ELSE IF ( ABS( MAXMUM - MINMUM ) .LT. 1.0 / VAL__BADR ) THEN
         NEWVAL  =  MAXMUM

      ELSE

*       Compute median position

         ACTMED = NUMVAL / 2 + 1

*       Sort VALARR array. Initialise index to VALARR element for
*       comparison

         TEST = 1
         SUMWT = 0

*       Can terminate sort once accumulated weight (position in
*       expanded list) includes median position

         DO WHILE ( SUMWT .LT. ACTMED )

*          Initialise index to current element and value

            CURR   = TEST
            CURVAL = VALARR( TEST )

*          Compare all VALARR elements after test element
*          against current value

            DO  INDEX = TEST+1, NUMVAL

               IF ( VALARR( INDEX ) .LT. CURVAL ) THEN

*                Have found a value in list which is less than the
*                current smallest value, this element becomes the new
*                current element

                  CURR   = INDEX
                  CURVAL = VALARR( INDEX )
               END IF
            END DO

            IF ( CURR .NE. TEST ) THEN

*             A smaller value than the test value was found so
*             swap the values

               VALARR( CURR ) = VALARR( TEST )
               VALARR( TEST )  = CURVAL
            END IF

            SUMWT = SUMWT + 1

*          Increment index to test element

            TEST = TEST + 1
         END DO

*       No true median value when there are an even number of points so
*       take the average of the points spanning the true median. Note
*       since the lowest even value of NUMVAL that can reach this point
*       is 4 (giving a TEST of 3), the array indices will not go to
*       zero.

         IF ( MOD( NUMVAL, 2 ) .EQ. 0 ) THEN

            NEWVAL  =  0.5 * ( VALARR( TEST - 1 ) + VALARR( TEST - 2 ) )
         ELSE

*          Set the new pixel value to be the median

            NEWVAL  =  VALARR( TEST - 1 )
         END IF
      END IF

*    replace pixel by new value

      ARRAY( XCOORD, YCOORD )  =  NEWVAL


*    end and return

      END

