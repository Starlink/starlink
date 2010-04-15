*+  MEDWTS - performs 2-D weighted median filtering on a 2-D array
      SUBROUTINE MEDWTS( DIFF, STEP, NUMSAM, MEDPOS, SAMSIZ, SAMINF,
     :  IDIMS1, IDIMS2, ARRIN, ODIMS1, ODIMS2, ARROUT, SAMPLE, SAMWT,
     :  STATUS )
*    Description :
*     The input array, ARRIN, is filtered using a weighted median filter to
*     give the output array, ARROUT. An input image point will only be
*     replaced by the sample median if the absolute value of the difference
*     between the input image point and the sample median is greater than DIFF.
*     The extent of the filter is defined by STEP and the weighting values and
*     offsets for the elements of the filter are given in SAMINF.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*      CALL MEDWTS( DIFF, STEP, NUMSAM, MEDPOS, SAMSIZ, SAMINF, IDIMS, ARRIN,
*     :  ODIMS, ARROUT, SAMPLE, SAMWT, STATUS )
*    Parameters :
*     DIFF = REAL( READ )
*           Determines whether replacement by the median will take place.
*           Replacement will only take place if the absolute value of the
*           input array point minus the median value of the weighted sample
*           is greater than DIFF.
*     STEP = INTEGER( READ )
*           Gives the separation in pixels between the filter elements.
*     NUMSAM = INTEGER( READ )
*           The number of elements in the sample with non-zero weights.
*     MEDPOS = INTEGER( READ )
*           The position of the median in the sorted sample.
*     SAMSIZ = INTEGER( READ )
*           Dimension of the SAMINF, SAMPLE and SAMWT arrays.
*     SAMINF( SAMSIZ, 3 ) = INTEGER( READ )
*           Contains the offsets and weights for the elements of the filter.
*     IDIMS( 2 ) = INTEGER( READ )
*           Dimensions of input data array.
*     ARRIN( IDIMS(1), IDIMS(2) ) = REAL( READ )
*           Data to be filtered.
*     ODIMS( 2 ) = INTEGER( READ )
*           Dimensions of the output data array.
*     ARROUT( ODIMS(1), ODIMS(2) ) = REAL( WRITE )
*           Will hold the filtered data.
*     SAMPLE( SAMSIZ ) = REAL( UPDATE )
*           Used to store the data to be sorted.
*     SAMWT( SAMSIZ ) = INTEGER( UPDATE )
*           Used to store the weights corresponding to the data being sorted.
*     STATUS = INTEGER( READ )
*           This is the global status, if this variable has an error value
*           on entry then an immediate return will occur.
*    Method :
*     If no error on entry then
*        For all rows of output image
*           Calculate position of corresponding input image row
*           For all points in output image row
*              Calculate position of corresponding input image point
*              For number of points in sample
*                 Calculate position of point for inclusion in sample
*                 Store value of point in sample array
*                 Store weight of point in SAMWT array
*              Endfor
*              Sort sample array until median is found
*              If difference between input image point and median is greater
*                than specified diffrence then
*                 Output image point becomes median
*              Else
*                 Output image point becomes input image point
*              Endif
*           Endfor
*        Endfor
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     24/10/1983 : Original version                     (ROE::ASOC5)
*     17/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     12-Aug-1994  Changed DIM arguments so that routine will compile(SKL@JACH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  IDIMS1, IDIMS2, ! dimensions of input image array
     :  ODIMS1, ODIMS2, !      "      " output image array
     :  STEP,       ! spacing between median filter elements
     :  NUMSAM,     ! number of values in SAMPLE for sorting
     :  SAMSIZ,     ! size of SAMPLE and SAMWT arrays
     :  MEDPOS,     ! position of median of values in sorted list
     :  SAMINF( SAMSIZ, 3 ) ! offsets and weights corresponding to sample
      REAL
     :  ARRIN( IDIMS1, IDIMS2 ),  ! input image frame
     :  DIFF ! value replaced by median if abs(value-median) > diff
*    Import-export :
      INTEGER
     :  SAMWT( SAMSIZ ) ! array to hold weights during sorting
      REAL
     :  SAMPLE( SAMSIZ ) ! array to hold values for sorting
*    Export :
      REAL
     :  ARROUT( ODIMS1, ODIMS2 ) ! output image frame
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  XPOS,   ! X position of image element being processed
     :  YPOS,   ! Y     "    "    "      "      "        "
     :  OFSETX, ! offset in X to input image points
     :  OFSETY, !    "    " Y  "   "     "     "
     :  INDEX,  ! index to sample elements
     :  CURR,   ! index to current smallest value during sorting
     :  CURWT,  ! weight corresponding to currents smallest value
     :  TEST,   ! index to sample element for testing against CURR
     :  SUMWT,  ! sum of weights used to locate median during sort
     :  X,      ! X position of output image elements
     :  Y       ! Y     "     "    "     "       "
      REAL
     :  CURVAL, ! current smallest value during sorting
     :  MEDIAN  ! value of median for testing agains input image points
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       apply the 2-D weighted median filter
*       for all rows of the output image
         DO Y = 1, ODIMS2

*          calculate position of corresponding input image row
            OFSETY = Y + STEP

*          for all points in output image row
            DO X = 1, ODIMS1

*             calculate position of corresponding input image point
               OFSETX = X + STEP

*             fill SAMPLE array with values for sorting
               DO INDEX = 1, NUMSAM

*                get offset position of sample element
                  XPOS = OFSETX + SAMINF( INDEX, 1 )
                  YPOS = OFSETY + SAMINF( INDEX, 2 )

*                put input image element into the sample array
                  SAMPLE( INDEX ) = ARRIN( XPOS, YPOS )

*                put corresponding weight into sample weighting array
                  SAMWT( INDEX ) = SAMINF( INDEX, 3 )
               ENDDO

*             sort SAMPLE array keeping track af weights
*             initialise index to SAMPLE element for comparison
               TEST = 1
               SUMWT = 0

*             can terminate sort once accumulated weight ( position in
*             expanded list ) includes median position
               DO WHILE( SUMWT .LT. MEDPOS )

*                initialise index to current elements, current value and weight
                  CURR   = TEST
                  CURVAL = SAMPLE( TEST )
                  CURWT  = SAMWT( TEST )

*                compare all SAMPLE elements after test element
*                against current value
                  DO INDEX = TEST+1, NUMSAM

                     IF( SAMPLE( INDEX ) .LT. CURVAL ) THEN

*                      have found a value in list which is less than current
*                      smallest value, this element becomes new current element
                        CURR   = INDEX
                        CURVAL = SAMPLE( INDEX )
                        CURWT  = SAMWT( INDEX )
                     ENDIF
                  ENDDO

                  IF( CURR .NE. TEST ) THEN

*                   a smaller value than the test value was found so swap the
*                   values and weights round
                     SAMPLE( CURR ) = SAMPLE( TEST )
                     SAMPLE( TEST )  = CURVAL
                     SAMWT( CURR )  = SAMWT( TEST )
                     SAMWT( TEST )   = CURWT
                  ENDIF

                  SUMWT = SUMWT + SAMWT( TEST )
*                increment index to test element
                  TEST = TEST + 1
               ENDDO

*             set up median value
               MEDIAN = SAMPLE( TEST - 1 )

*             test for replacement
               IF( ABS( ARRIN( OFSETX, OFSETY ) - MEDIAN ) .GT. DIFF )
     :           THEN

*                replace value by median
                  ARROUT( X, Y ) = MEDIAN
               ELSE

*                no change
                  ARROUT( X, Y ) = ARRIN( OFSETX, OFSETY )
               ENDIF
            ENDDO
         ENDDO
      ENDIF

      END
