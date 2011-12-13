      SUBROUTINE KPS1_MDWTD( DIFF, STEP, DAMP, NUMSAM, MEDPOS, MEDTHR,
     :                         SAMSIZ, SAMINF, IDIM1, IDIM2, ARRIN,
     :                         ODIM1, ODIM2, SAMPLE, SAMWT, ARROUT,
     :                         CHANGE, STATUS )

*+
*  Name:
*     KPS1_MDWTx

*  Purpose:
*     Performs 2-dimensional weighted median filtering of an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL KPS1_MDWTx( DIFF, STEP, DAMP, NUMSAM, MEDPOS, MEDTHR,
*                       SAMSIZ, SAMINF, IDIM1, IDIM2, ARRIN, ODIM1,
*                       ODIM2, SAMPLE, SAMWT, ARROUT, CHANGE, STATUS )

*  Description:
*     The input array, ARRIN, is filtered using a weighted median
*     filter to give the output array, ARROUT.  An input image point
*     will only be replaced by the sample median if:
*       a) the absolute value of the difference between the input image
*          point and the sample median is greater than DIFF;
*       b) at least three valid pixels were used to define it;
*       c) and at least MEDTHR*MEDPOS weighting was used to define it.
*
*     The extent of the filter is defined by STEP and the weighting
*     values and offsets for the elements of the filter are given in
*     SAMINF.  A damping algorithm may be switched on by calling the
*     routine with DAMP=.TRUE.  In this case values are not replaced by
*     the median value but by a value mid way between it and the median
*     value.  This may be used to prevent the output oscillating
*     endlessly between two solutions if the filter is being used
*     iteratively.

*  Arguments:
*     DIFF = DOUBLE PRECISION (Given)
*        Determines whether replacement by the median will take place.
*        Replacement will only take place if the absolute value of the
*        input array point minus the median value of the weighted
*        sample is greater than DIFF.
*     STEP = INTEGER (Given)
*        Gives the separation in pixels between the filter elements.
*     DAMP = LOGICAL (Given)
*        Flag which may be set .TRUE. to switch on a damping algorithm,
*        in which the data value is replaced not by the median value but
*        by a value midway between it and the median value.  This is
*        used to prevent the output oscillating endlessly between two
*        solutions if the filter is being used iteratively.
*     NUMSAM = INTEGER (Given)
*        The number of elements in the sample with non-zero weights.
*     MEDPOS = INTEGER (Given)
*        The position of the median in the sorted sample if no invalid
*        pixels are present.
*     MEDTHR = REAL (Given)
*        Minimum median position as a fraction of MEDPOS, after the
*        removal of any invalid pixels.
*     SAMSIZ = INTEGER (Given)
*        Dimension of the SAMINF, SAMPLE, and SAMWT arrays.
*     SAMINF( SAMSIZ, 3 ) = INTEGER (Given)
*        The x-y offsets and weights for the elements of the filter.
*     IDIM1 = INTEGER (Given)
*        The first dimension of the input 2-dimensional array.
*     IDIM2 = INTEGER (Given)
*        The second dimension of the input 2-dimensional array.
*     ARRIN( IDIM1, IDIM2 ) = ? (Given)
*        Data array to be filtered.
*     ODIM1 = INTEGER (Given)
*        The first dimension of the output 2-dimensional array.
*     ODIM2 = INTEGER (Given)
*        The second dimension of the output 2-dimensional array.
*     SAMPLE( SAMSIZ ) = DOUBLE PRECISION (Returned)
*        Used to store the data to be sorted.
*     SAMWT( SAMSIZ ) = INTEGER (Returned)
*        Used to store the weights corresponding to the data being
*        sorted.
*     ARROUT( ODIM1, ODIM2 ) =  ? (Returned)
*        The array of filtered data.
*     CHANGE = LOGICAL (Returned)
*        A flag which is returned .TRUE. when the data has been changed
*        by the filtering.  If CHANGE is returned .FALSE., then the
*        filtering has converged and the caller can stop iterating.
*     STATUS = INTEGER (Given)
*        Global status.

*  Notes:
*     -  There is a routine for each numeric data type: replace "x" in
*     the routine name by D, R, I, W, UW, B or UB as appropriate.  The
*     ARRIN and ARROUT arguments to the routine must have the data
*     type specified.

*  Algorithm:
*     If no error on entry then
*        initialise CHANGE to FALSE
*        For all lines of output image
*           Calculate position of corresponding input image line
*           For all points in output image line
*              Calculate position of corresponding input image point
*              Initialise running totals of weight and number in sample
*              For number of points in sample
*                 Calculate position of point for inclusion in sample
*                 If pixel is valid then
*                    Add to running total of actual number of values in
*                      sample to be sorted
*                    Store value of point in sample array
*                    Store weight of point in SAMWT array
*                    Increment running total of weight
*                 Endif
*              Endfor
*              Compute position of median in sorted sample using summed
*                weight
*              If insufficient valid pixels have been used to form the
*                sample or median is too small then
*                Set output pixel to be input pixel value
*              Else
*                 Sort sample array until median is found
*                 If difference between input image point and median is
*                  greater than specified diffrence then
*                    set CHANGE to TRUE
*                    If damping is required then
*                      Output image point becomes mid way between the
*                       old value and the median
*                    Else
*                      Output image point becomes median
*                    Endif
*                 Else
*                   Output image point becomes input image point
*                 Endif
*              Endif
*           Endfor
*        Endfor
*     Endif

*  Copyright:
*     Copyright (C) 1983-1984, 1986, 1989, 1993 Science & Engineering
*     Research Council. Copyright (C) 1995 Central Laboratory of the
*     Research Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DB: Dave Baines (ROE)
*     MJC: Malcolm J. Currie (STARLINK)
*     SMB: Steven Beard (ROE)
*     {enter_new_authors_here}

*  History:
*     24/10/1983 (DB):
*        Original version.
*     17/02/1984 (DB):
*        Documentation brought up to standard.
*     1986 September 11 (MJC):
*        Renamed parameters section to arguments, added invalid-pixel
*        handling, which necessitated an additional argument (MEDTHR -
*        5th), reordered ARROUT (old 10th to new 13th) in arguments and
*        tidied.
*     1989 August 7 (MJC):
*        Passed array dimensions as separate variables.
*     1993 July 20 (SMB):
*        DAMP and CHANGE arguments added and damping algorithm
*        incorporated.  Also bug fixed: ARRIN(X,Y) set to
*        ARROUT(OFSETX,OFSETY) if there are insufficient points to make
*        a median, rather than ARROUT(X,Y).
*     1995 July 27 (MJC):
*        Made generic (from MEDWTS).  Used a modern prologue and style.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      DOUBLE PRECISION DIFF
      INTEGER STEP
      LOGICAL DAMP
      INTEGER NUMSAM
      INTEGER MEDPOS
      REAL MEDTHR
      INTEGER SAMSIZ
      INTEGER SAMINF( SAMSIZ, 3 )
      INTEGER IDIM1
      INTEGER IDIM2
      DOUBLE PRECISION ARRIN( IDIM1, IDIM2 )
      INTEGER ODIM1
      INTEGER ODIM2

*  Arguments Returned:
      DOUBLE PRECISION SAMPLE( SAMSIZ )
      INTEGER SAMWT( SAMSIZ )
      DOUBLE PRECISION ARROUT( ODIM1, ODIM2 )
      LOGICAL CHANGE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ACTMED             ! Actual median position in sorted sample
      INTEGER ACTSAM             ! Actual number of values in SAMPLE for
                                 ! sorting, i.e. exclusion of invalid
                                 ! pixels
      INTEGER CURR               ! Index to current smallest value
                                 ! during sorting
      DOUBLE PRECISION CURVAL    ! Current smallest value during sorting
      INTEGER CURWT              ! Weight corresponding to currents
                                 ! smallest value
      DOUBLE PRECISION DELTA     ! Difference between a data value and the median
      DOUBLE PRECISION  MEDIAN   ! Value of median for testing against
                                 ! input image points
      INTEGER OFSETX             ! Offset in X to input image points
      INTEGER OFSETY             ! Offset in Y to input image points
      INTEGER SINDEX             ! Index to sample elements
      INTEGER SUMWT              ! Sum of weights used to locate median
                                 ! during sort
      INTEGER TEST               ! Index to sample element for testing
                                 ! against CURR
      INTEGER WEIGHT             ! Running total of weights in SAMPLE
                                 ! for valid pixels
      INTEGER X                  ! X position of output image elements
      INTEGER XPOS               ! X position of image element being
                                 ! processed
      INTEGER Y                  ! Y position of output image elements
      INTEGER YPOS               ! Y position of image element being
                                 ! processed

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the CHANGE flag.  It will be set .TRUE. later if any data
*  values are changed by the filter.
      CHANGE = .FALSE.

*  Apply the 2-dimensional weighted median filter for all lines of the
*  output image.
      DO Y = 1, ODIM2

*  Calculate position of corresponding input image line.
         OFSETY = Y + STEP

*  For all points in the output image line.
         DO X = 1, ODIM1

*  Calculate the position of the corresponding input image point.
            OFSETX = X + STEP

*  Initialise running totals for sum of values of valid pixels and the
*  sum of their corresponding weights.
            ACTSAM = 0
            WEIGHT = 0

*  Fill the SAMPLE array with values for sorting.
            DO SINDEX = 1, NUMSAM

*  Get offset position of sample element.
               XPOS = OFSETX + SAMINF( SINDEX, 1 )
               YPOS = OFSETY + SAMINF( SINDEX, 2 )

*  If the pixel is valid then increment number-of-values counter.
               IF ( ARRIN( XPOS, YPOS ) .NE. VAL__BADD ) THEN
                  ACTSAM = ACTSAM + 1

*  Put input image element into the sample array and then put
*  corresponding weight into sample weighting array.
                  SAMPLE( ACTSAM ) = NUM_DTOD( ARRIN( XPOS, YPOS ) )
                  SAMWT( ACTSAM ) = SAMINF( SINDEX, 3 )

*  Keep running total of the weights.
                  WEIGHT = WEIGHT + SAMWT( ACTSAM )
               END IF
            END DO

*  Compute the median position.
            ACTMED = WEIGHT / 2 + 1

*  Only allow median to be computed if sufficient pixels and weights
*  were used to form sample.
            IF ( ACTSAM .LT. 3 .OR.
     :           ACTMED .LT. INT( MEDTHR * MEDPOS ) ) THEN

*  Pass input value to the output pixel.
               ARROUT( X, Y ) = ARRIN( OFSETX, OFSETY )
            ELSE

*  Sort SAMPLE array keeping track of the weights.  Initialise the
*  index to SAMPLE element for comparison.
               TEST = 1
               SUMWT = 0

*  Can terminate sort once accumulated weight (position in expanded
*  list) includes median position.
               DO WHILE( SUMWT .LT. ACTMED )

*  Initialise the index to the current elements, current value, and
*  weight.
                  CURR   = TEST
                  CURVAL = SAMPLE( TEST )
                  CURWT  = SAMWT( TEST )

*  Compare all SAMPLE elements after test element against the
*  current value.
                  DO SINDEX = TEST + 1, ACTSAM

                     IF ( SAMPLE( SINDEX ) .LT. CURVAL ) THEN

*  We have found a value in list which is less than current smallest
*  value; this element becomes new current element.
                        CURR   = SINDEX
                        CURVAL = SAMPLE( SINDEX )
                        CURWT  = SAMWT( SINDEX )
                     END IF
                  END DO

                  IF ( CURR .NE. TEST ) THEN

*  A smaller value than the test value was found so swap the values and
*  weights.
                     SAMPLE( CURR ) = SAMPLE( TEST )
                     SAMPLE( TEST )  = CURVAL
                     SAMWT( CURR )  = SAMWT( TEST )
                     SAMWT( TEST )   = CURWT
                  END IF

                  SUMWT = SUMWT + SAMWT( TEST )

*  Increment index to test the element.
                  TEST = TEST + 1
               END DO

*  Set up the median value.
               MEDIAN = SAMPLE( TEST - 1 )

*  Test for replacement.
               DELTA = MEDIAN - NUM_DTOD( ARRIN( OFSETX, OFSETY ) )

               IF ( ABS( DELTA ) .GT. DIFF ) THEN

*  The value should be replaced.  Record the fact that at least one
*  data value has changed.
                  CHANGE = .TRUE.

*  Reduce the size of the change if damping is required.
                  IF ( DAMP ) DELTA = DELTA / 2.0

*  Replace the value.
                  ARROUT( X, Y ) = ARRIN( OFSETX, OFSETY ) +
     :                             NUM_DTOD( DELTA )
               ELSE

*  No change, so just copy from the input array.
                  ARROUT( X, Y ) = ARRIN( OFSETX, OFSETY )
               END IF

*  End of condition for the number in the sample and median position.
            END IF

*  End of loops for all pixels.
         END DO
      END DO

      END
