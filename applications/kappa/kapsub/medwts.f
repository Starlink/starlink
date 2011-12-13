      SUBROUTINE MEDWTS( DIFF, STEP, NUMSAM, MEDPOS, MEDTHR, SAMSIZ,
     :                   SAMINF, IDIM1, IDIM2, ARRIN, ODIM1, ODIM2,
     :                   SAMPLE, SAMWT, ARROUT, STATUS )
*+
*  Name:
*     MEDWTS

*  Purpose:
*     Performs 2-D weighted median filtering on a 2-D array

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     SUBROUTINE

*  Invocation:
*      CALL MEDWTS( DIFF, STEP, NUMSAM, MEDPOS, MEDTHR, SAMSIZ, SAMINF,
*    :             IDIM1, IDIM2, ARRIN, ODIM1, ODIM2, SAMPLE, SAMWT,
*    :             ARROUT, STATUS )

*  Description:
*     The input array, ARRIN, is filtered using a weighted median
*     filter to give the output array, ARROUT. An input image point will

*  Arguments:
*     DIFF = REAL( READ )
*         Determines whether replacement by the median will take
*           place. Replacement will only take place if the absolute
*           value of the input array point minus the median value of the
*           weighted sample is greater than DIFF.
*     STEP = INTEGER( READ )
*         Gives the separation in pixels between the filter elements.
*     NUMSAM = INTEGER( READ )
*         The number of elements in the sample with non-zero weights.
*     MEDPOS = INTEGER( READ )
*         The position of the median in the sorted sample if no
*           invalid pixels are present.
*     MEDTHR = REAL ( READ )
*         Minimum median position as a fraction of MEDPOS, after the
*           removal of invalid pixels.
*     SAMSIZ = INTEGER( READ )
*         Dimension of the SAMINF, SAMPLE and SAMWT arrays.
*     SAMINF( SAMSIZ, 3 ) = INTEGER( READ )
*         Contains the x,y offsets and weights for the elements of the
*           filter.
*     IDIM1 = INTEGER( READ )
*         The first dimension of the input 2-d array.
*     IDIM2 = INTEGER( READ )
*         The second dimension of the input 2-d array.
*     ARRIN( IDIM1, IDIM2 ) = REAL( READ )
*         Data to be filtered.
*     ODIM1 = INTEGER( READ )
*         The first dimension of the output 2-d array.
*     ODIM2 = INTEGER( READ )
*         The second dimension of the output 2-d array.
*     SAMPLE( SAMSIZ ) = REAL( UPDATE )
*         Used to store the data to be sorted.
*     SAMWT( SAMSIZ ) = INTEGER( UPDATE )
*         Used to store the weights corresponding to the data being
*           sorted.
*     ARROUT( ODIM1, ODIM2 ) = REAL( WRITE )
*         Will hold the filtered data.
*     STATUS = INTEGER( READ )
*         This is the global status, if this variable has an error
*           value on entry then an immediate return will occur.

*  Algorithm:
*     If no error on entry then
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
*                    Output image point becomes median
*                 Else
*                   Output image point becomes input image point
*                 Endif
*              Endif
*           Endfor
*        Endfor
*     Endif

*  Only Be Replaced By The Sample Median If:
*       a) the absolute value of the difference between the input image
*          point and the sample median is greater than DIFF;
*       b) at least three valid pixels were used to define it;
*       c) and at least MEDTHR*MEDPOS weighting was used to define it.
*     The extent of the filter is defined by STEP and the weighting
*     values and offsets for the elements of the filter are given in
*     SAMINF. An immediate return will occur if STATUS has an error
*     value on entry.

*  Copyright:
*     Copyright (C) 1983-1984, 1986, 1989, 1992 Science & Engineering
*     Research Council. All Rights Reserved.

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
*     Dave Baines (ROE::ASOC5)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*     {enter_new_authors_here}

*  History:
*     24-OCT-1983 (ROE::ASOC5):
*        : Original version
*     17-FEB-1984 (ROE::ASOC5):
*        : Documentation brought up to standard
*     1986 Sep 11: Renamed parameters section to arguments, added
*                  invalid-pixel handling, which necessitated an
*                  additional argument (MEDTHR - 5th), reordered
*                  ARROUT (old 10th to new 13th) in arguments
*                  and tidied (RL.STAR::CUR).
*     1989 Aug  7: Passed array dimensions as separate variables
*                  (RL.STAR::CUR).
*     1992 Jul 20: Fixed bug indexing when median is not computed.
*                  (RL.STAR::CUR).
*     {enter_further_changes_here}

*-

*  Type Definitions:

      IMPLICIT NONE

*  Global Constants:

      INCLUDE 'SAE_PAR'        ! SSE global constants
      INCLUDE 'PRM_PAR'        ! PRIMDAT public constants

*  Arguments Given:

      INTEGER
     :  IDIM1, IDIM2,
     :  ODIM1, ODIM2,
     :  STEP,
     :  NUMSAM,
     :  SAMSIZ,
     :  MEDPOS,
     :  SAMINF( SAMSIZ, 3 )

      REAL
     :  ARRIN( IDIM1, IDIM2 ),
     :  DIFF,
     :  MEDTHR

*  Arguments Given and Returned:

      INTEGER
     :  SAMWT( SAMSIZ )

      REAL
     :  SAMPLE( SAMSIZ )

*  Arguments Returned:

      REAL
     :  ARROUT( ODIM1, ODIM2 )

*  Status:

      INTEGER STATUS

*  Local Variables:

      INTEGER
     :  XPOS,                  ! X position of image element being
                               ! processed
     :  YPOS,                  ! Y position of image element being
                               ! processed
     :  OFSETX,                ! Offset in X to input image points
     :  OFSETY,                !    "    " Y  "   "     "     "
     :  INDEX,                 ! Index to sample elements
     :  ACTSAM,                ! Actual number of values in SAMPLE for
                               ! sorting, i.e. exclusion of invalid
                               ! pixels
     :  ACTMED,                ! Actual median position in sorted sample
     :  CURR,                  ! Index to current smallest value during
                               ! sorting
     :  CURWT,                 ! Weight corresponding to currents
                               ! smallest value
     :  TEST,                  ! Index to sample element for testing
                               ! against CURR
     :  SUMWT,                 ! Sum of weights used to locate median
                               ! during sort
     :  WEIGHT,                ! Running total of weights in SAMPLE
                               ! for valid pixels
     :  X,                     ! X position of output image elements
     :  Y                      ! Y     "     "    "     "       "

      REAL

     :  CURVAL,                ! Current smallest value during sorting
     :  MEDIAN                 ! Value of median for testing agains
                               ! input image points
*.

*    check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

*       apply the 2-D weighted median filter
*       for all lines of the output image

         DO  Y = 1, ODIM2

*          calculate position of corresponding input image line

            OFSETY = Y + STEP

*          for all points in output image line

            DO  X = 1, ODIM1

*             calculate position of corresponding input image point

               OFSETX = X + STEP

*             initialise running totals for sum of values of valid
*             pixels and the sum of their corresponding weights

               ACTSAM = 0
               WEIGHT = 0

*             fill SAMPLE array with values for sorting

               DO INDEX = 1, NUMSAM

*                get offset position of sample element

                  XPOS = OFSETX + SAMINF( INDEX, 1 )
                  YPOS = OFSETY + SAMINF( INDEX, 2 )

*                if pixel is valid then increment number-of-values
*                counter

                  IF ( ARRIN( XPOS, YPOS ) .NE. VAL__BADR ) THEN
                     ACTSAM = ACTSAM + 1

*                   put input image element into the sample array and
*                   then put corresponding weight into sample weighting
*                   array

                     SAMPLE( ACTSAM ) = ARRIN( XPOS, YPOS )
                     SAMWT( ACTSAM ) = SAMINF( INDEX, 3 )

*                   keep running total of weights

                     WEIGHT = WEIGHT + SAMWT( ACTSAM )
                  END IF
               END DO

*             compute median position

               ACTMED = WEIGHT/2 + 1

*             only allow median to be computed if sufficient pixels and
*             weights were used to form sample

               IF ( ACTSAM .LT. 3 .OR.
     :              ACTMED .LT. INT( MEDTHR*MEDPOS ) ) THEN

*                pass input value to output pixel

                  ARROUT( X, Y ) = ARRIN( OFSETX, OFSETY )
               ELSE

*                sort SAMPLE array keeping track of weights
*                initialise index to SAMPLE element for comparison

                  TEST = 1
                  SUMWT = 0

*                can terminate sort once accumulated weight (position in
*                expanded list) includes median position

                  DO WHILE( SUMWT .LT. ACTMED )

*                  initialise index to current elements, current value
*                  and weight

                     CURR   = TEST
                     CURVAL = SAMPLE( TEST )
                     CURWT  = SAMWT( TEST )

*                   compare all SAMPLE elements after test element
*                   against current value

                     DO  INDEX = TEST+1, ACTSAM

                        IF ( SAMPLE( INDEX ) .LT. CURVAL ) THEN

*                         have found a value in list which is less than
*                         current smallest value, this element becomes
*                         new current element

                           CURR   = INDEX
                           CURVAL = SAMPLE( INDEX )
                           CURWT  = SAMWT( INDEX )
                        END IF
                     END DO

                     IF ( CURR .NE. TEST ) THEN

*                      a smaller value than the test value was found so
*                      swap the  values and weights round

                        SAMPLE( CURR ) = SAMPLE( TEST )
                        SAMPLE( TEST )  = CURVAL
                        SAMWT( CURR )  = SAMWT( TEST )
                        SAMWT( TEST )   = CURWT
                     END IF

                     SUMWT = SUMWT + SAMWT( TEST )

*                   increment index to test element

                     TEST = TEST + 1
                  END DO

*                set up median value

                  MEDIAN = SAMPLE( TEST - 1 )

*                test for replacement

                  IF ( ABS( ARRIN( OFSETX, OFSETY ) - MEDIAN ) .GT.
     :                DIFF ) THEN

*                   replace value by median

                     ARROUT( X, Y ) = MEDIAN
                  ELSE

*                   no change

                     ARROUT( X, Y ) = ARRIN( OFSETX, OFSETY )
                  END IF

*             end of condition for number in sample and median position
               END IF

*          end of loops for all pixels
            END DO
         END DO
      END IF

      END
