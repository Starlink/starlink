      SUBROUTINE MEDWV( DIM, ARRAY, NELM, MEDTHR, SAMSIZ, SAMINF,
     :                  SAMPLE, SAMWT, MEDIAN, STATUS )
*+
*  Name:
*     MEDWV

*  Purpose:
*     Performs weighted median filtering on a vector

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     SUBROUTINE

*  Invocation:
*      CALL MEDWV( DIM, ARRAY, NELM, MEDTHR, SAMSIZ, SAMINF,
*    :            SAMPLE, SAMWT, MEDIAN, STATUS )

*  Description:
*     The weighted median value of the input array is obtained. The
*     median returned will be undefined if:
*       a) less than three valid pixels were used to define it;
*       b) or the weighting used to define it was less than the
*          specified threshold times the unweighted median position
*          (excluding any invalid pixels).
*
*     The weighted array is only partially sorted via Quicksort for
*     efficiency.

*  Arguments:
*     DIM = INTEGER( READ )
*         Dimension of input data array.
*     ARRAY( DIM ) = REAL( READ )
*         Data to be filtered.
*     NELM = INTEGER( READ )
*         Number of values in the array to be used to compute median
*     MEDTHR = REAL ( READ )
*         Minimum median position as a fraction of MEDPOS, after the
*           removal of invalid pixels.
*     SAMSIZ = INTEGER( READ )
*         Dimension of the SAMINF, SAMPLE and SAMWT arrays.
*     SAMINF( SAMSIZ ) = INTEGER( READ )
*         Weights for the elements of the vector.
*     SAMPLE( SAMSIZ ) = REAL( UPDATE )
*         Used to store the data to be sorted.
*     SAMWT( SAMSIZ ) = INTEGER( UPDATE )
*         Used to store the weights corresponding to the data being
*           sorted.
*     MEDIAN = REAL( WRITE )
*         The weighted median of the data array.
*     STATUS = INTEGER( READ )
*         This is the global status, if this variable has an error
*           value on entry then an immediate return will occur.

*  Algorithm:
*     If no error on entry then
*        For all points in input array
*           Initialise running totals of weight and number in sample
*           If pixel is valid then
*              Add to running total of actual number of values in
*                sample to be sorted
*              Store value of point in sample array
*              Store weight of point in SAMWT array
*              Increment running total of weight
*           Endif
*        Endfor
*        Compute position of median in sorted sample using summed weight
*        If insufficient valid pixels have been used to form the
*          sample or median is too small then
*           Set output pixel to be input pixel value
*        Else
*           Sort sample array keeping track of the weights until the
*             median is found (when the accumulated weights includes the
*             median position)
*        Endif
*     Endif

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     Malcolm Currie  STARLINK (RAL::CUR)
*     {enter_new_authors_here}

*  History:
*     1989 May 24: Original version (RAL::CUR).
*     {enter_further_changes_here}

*-

*  Type Definitions:

      IMPLICIT NONE

*  Global Constants:

      INCLUDE 'SAE_PAR'        ! Environment global definitions
      INCLUDE 'PRM_PAR'        ! PRIMDAT public constants

*  Arguments Given:

      INTEGER
     :  DIM,
     :  NELM,
     :  SAMSIZ,
     :  SAMINF( SAMSIZ )
      REAL
     :  ARRAY( DIM ),
     :  MEDTHR

*  Arguments Given and Returned:

      INTEGER
     :  SAMWT( SAMSIZ )

      REAL
     :  SAMPLE( SAMSIZ )

*  Arguments Returned:

      REAL
     :  MEDIAN

*  Status:

      INTEGER STATUS

*  Local Variables:

      INTEGER
     :  INDEX,                 ! Index to sample elements
     :  ACTSAM,                ! Actual number of values in SAMPLE for
                               ! sorting, i.e. after exclusion of
                               ! invalid pixels
     :  ACTMED,                ! Actual median position in sorted sample
     :  CURR,                  ! Index to current smallest value during
                               ! sorting
     :  CURWT,                 ! Weight corresponding to current
                               ! smallest value
     :  MEDPOS,                ! The position of the median in the
                               ! sorted sample if no invalid pixels are
                               ! present.
     :  TEST,                  ! Index to sample element for testing
                               ! against CURR
     :  SUMWT,                 ! Sum of weights used to locate median
                               ! during sort
     :  WEIGHT                 ! Running total of weights in SAMPLE
                               ! for valid pixels

      REAL

     :  CURVAL                 ! current smallest value during sorting
*.

*    Check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

*       Initialise running totals for sum of values of valid
*       pixels and the sum of their corresponding weights

         ACTSAM = 0
         WEIGHT = 0

*       For all points in input vector

         DO  INDEX = 1, NELM

*          If pixel is valid then increment number-of-values counter

            IF ( ARRAY( INDEX ) .NE. VAL__BADR ) THEN
               ACTSAM = ACTSAM + 1

*             Fill SAMPLE array with values for sorting.
*             Put input image element into the sample array and then
*             put corresponding weight into sample weighting array

               SAMPLE( ACTSAM ) = ARRAY( INDEX )
               SAMWT( ACTSAM ) = SAMINF( INDEX )

*             Keep running total of weights

               WEIGHT = WEIGHT + SAMWT( ACTSAM )
            END IF
         END DO

*       Compute median positions with and without weighting

         ACTMED = WEIGHT/2 + 1
         MEDPOS = ACTSAM/2 + 1

*       Only allow median to be computed if sufficient pixels and
*       weights were used to form sample

         IF ( ACTSAM .LT. 3 .OR.
     :        ACTMED .LT. INT( MEDTHR * MEDPOS ) ) THEN

*          Median is undefined

            MEDIAN = VAL__BADR
         ELSE

*         Sort SAMPLE array keeping track of weights.
*         Initialise index to SAMPLE element for comparison

           TEST = 1
           SUMWT = 0

*         Can terminate sort once accumulated weight (position in
*         expanded list) includes median position

           DO WHILE ( SUMWT .LT. ACTMED )

*             Initialise index to current elements, current value
*             and weight

               CURR   = TEST
               CURVAL = SAMPLE( TEST )
               CURWT  = SAMWT( TEST )

*             Compare all SAMPLE elements after test element
*             against current value

               DO  INDEX = TEST+1, ACTSAM

                  IF ( SAMPLE( INDEX ) .LT. CURVAL ) THEN

*                   Have found a value in list which is less than
*                   current smallest value, this element becomes
*                   new current element

                     CURR   = INDEX
                     CURVAL = SAMPLE( INDEX )
                     CURWT  = SAMWT( INDEX )
                  END IF
               END DO

               IF ( CURR .NE. TEST ) THEN

*                A smaller value than the test value was found so
*                swap the values and weights round

                  SAMPLE( CURR ) = SAMPLE( TEST )
                  SAMPLE( TEST )  = CURVAL
                  SAMWT( CURR )  = SAMWT( TEST )
                  SAMWT( TEST )   = CURWT
               END IF

               SUMWT = SUMWT + SAMWT( TEST )

*             Increment index to test element

               TEST = TEST + 1
            END DO

*          Set up median value

            MEDIAN = SAMPLE( TEST - 1 )

*       End of condition for number in sample and median position

         END IF

      END IF

      END
