      SUBROUTINE ICMMM ( ARRAY, DIM, SMEAN, SMED, SMODE, SIGMA, SKEW,
     :                   NSAM, NINVAL, STATUS )
*+
*  Name:
*     ICMMM

*  Purpose:
*     Iteratively finds the mean, median and mode using the.

*  Language:
*     Starlink

*  Invocation:
*     CALL ICMMM ( ARRAY, DIM, SMEAN, SMED, SMODE, SIGMA, SKEW, NSAM,

*  Description:
*     This finds the mean, median and mode (mmm) of a sample of data
*     taken from a background region of an astronomical plate. It should
*     not be used as a general mmm routine as there are special features
*     which are peculiar to the problem of contamination by stars. The
*     data should not include any invalid pixel values as these are not
*     trapped. The data need not be sorted as a NAG sorting routine
*     (M01CAE, single precision) is called internally. The routine also
*     returns the standard deviation and the skew of the sample.

*     The magic-value method is used for bad pixels.

*  Arguments:
*     ARRAY( DIM ) = REAL( READ )
*        Vector of data samples.
*     DIM = INTEGER( READ )
*        The number of data in the sample.
*     SMEAN = REAL( WRITE )
*        The mean of the sample.
*     SMED = REAL( WRITE )
*        The median of the sample.
*     SMODE = REAL( WRITE )
*        The mode of the sample.
*     SIGMA = REAL( WRITE )
*        The standard deviation of the sample.
*     SKEW = REAL( WRITE )
*        The skewness of the sample.
*     NSAM = INTEGER( WRITE )
*        The population of the sample after the iterative procedure.
*     NINVAL = INTEGER( WRITE )
*        The number of bad pixels in the sample.
*     STATUS  =  INTEGER( READ, WRITE )
*        Global status value.

*  Algorithm:
*     Check for error on entry - return if not o.k.

*  Copyright:
*     Copyright (C) 1990, 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     Malcolm J. Currie  STARLINK ( RAL::CUR )
*     {enter_new_authors_here}

*  History:
*     1990 Jan 24: First ADAM implementation, based upon the similar
*        routine in DAOPHOT, modified by Nick Eaton; now
*        has bad-pixel handling (RAL::CUR).
*     1990 Jun 3 : Improved handling of variance for the general case
*        (RAL::CUR).
*     1992 Jun 3 : Replaced NAG routine by KAPPA sorting routine.
*        Although the latter is slower it is available on
*        Unix (RAL::CUR).
*     {enter_further_changes_here}

*  Bugs:
*     None known.
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No assumed typing allowed


*  Global Constants:
      INCLUDE  'SAE_PAR'       ! SSE global definitions
      INCLUDE  'PRM_PAR'       ! Magic-value definitions


*  Arguments Given:
      INTEGER
     :    DIM

      REAL
     :    ARRAY( DIM )


*  Arguments Returned:
      REAL
     :  SIGMA,                 ! Sample standard deviation
     :  SKEW,                  ! Sample skewness
     :  SMEAN,                 ! Sample mean
     :  SMED,                  ! Sample median
     :  SMODE                  ! Sample mode

      INTEGER
     :   NSAM                  ! Number of data elements after removal
                               ! of bad pixels


*  Status:
      INTEGER  STATUS


*  Local Constants:
      INTEGER
     :  MAXITR,                ! Maximum nuber of iterations
     :  MIND                   ! Minimum number of data points in the
                               ! sample permitted
      PARAMETER( MAXITR = 20 )
      PARAMETER( MIND = 5 )


*  Local Variables:
      LOGICAL                  ! If true:
     :   BPLOOP,               ! Continue search for bad pixels
     :   MOVE,                 ! There are more elements to move in or
                               ! out of the partition
     :   REDO                  ! Continue to loop to re-evaluate the
                               ! statistics

      INTEGER
     :   I, K,                 ! Loop counters
     :   ISTEP,                ! Direction in which to step through the
                               ! array to go from the old partition to
                               ! the new one
     :   JSTEP,                ! The number of steps to move an edge
                               ! of the partition
     :   NINVAL,               ! Number of bad pixels in the sample
     :   NITER,                ! Number of iterations
     :   PMAX,                 ! Array index of the maximum value in
                               ! the sample after removal of bad pixels
     :   PMIN,                 ! Array index of the minimum value in
                               ! the sample after removal of bad pixels
     :   SMAX,                 ! Array index of the maximum value in
                               ! the analysis
     :   SMIN                  ! Array index of the minimum value in
                               ! the analysis

      REAL
     :   CUT1,                 ! Lower threshold for the partition
     :   CUT2,                 ! Upper threshold for the partition
     :   DELTA,                ! Value of element added or subtracted
     :   HW,                   ! Half-width of the partition
     :   R,                    ! Chauvenet rejection parameter
     :   SAM,                  ! Number of data elements after removal
                               ! of bad pixels before iteration
     :   SMID,                 ! First guess median
     :   STAT( 4 ),            ! Previous loop's statistics, when
                               ! oscillating about a solution
     :   SUM,                  ! Sum of data values
     :   SUM2,                 ! Sum of squared data values
     :   VARNCE                ! Variance


*.

*    Check status on entry - return if not o.k..

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Begin by sorting the data in ascending order.

      CALL KPG1_QSRTR( DIM, 1, DIM, ARRAY, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ICMMM__SORT',
     :     'Unable to find the mean, median, and mode using the '/
     :     /'Chauvenet criterion.', STATUS )
         GOTO 999
      END IF

      PMAX = DIM
      PMIN = 1

*    Look for and count bad pixels. Note we cannot assume the value
*    of bad pixels is large negative, but we do assume that its absolute
*    value large so the bad pixels will be at one end or other of the
*    sorted data.

      IF ( ARRAY( DIM ) .EQ. VAL__BADR ) THEN
         I = DIM - 1
         BPLOOP = .TRUE.
         DO WHILE ( BPLOOP .AND. I .GT. 1 )
            BPLOOP = ARRAY( I ) .EQ. VAL__BADR
            I = I - 1
         END DO
         NINVAL = DIM - I - 1

*       Adjust the size of the partition accordingly.
*       Henceforth sample minimum points to the highest value rejected
*       at the lower end of the vector, and sample maximum points to
*       the highest value accepted at the upper end of the vector. Thus
*       their difference is the number of elements within the acceptance
*       range.

         PMIN = 0
         PMAX = I + 1

      ELSE IF ( ARRAY( 1 ) .EQ. VAL__BADR ) THEN
         I = 2
         BPLOOP = .TRUE.
         DO WHILE ( BPLOOP .AND. I .LT. DIM )
            BPLOOP = ARRAY( I ) .EQ. VAL__BADR
            I = I + 1
         END DO
         NINVAL = I - 2

*       Adjust the size of the partition accordingly.

         PMIN = NINVAL
         PMAX = DIM
      END IF

      NSAM = PMAX - PMIN
      IF ( NSAM .LT. MIND ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ICMMM__NAGSORT',
     :     'Error sorting the data', STATUS )
         GOTO 999
      END IF

*     Calculate the median value for the whole ensemble of elements.
*     Note that the following equation is valid whether the number
*     of data in the sample is odd or even.

      SMID = 0.5 * ( ARRAY( ( PMIN + PMAX + 1 ) / 2 ) +
     :       ARRAY( ( PMIN + PMAX ) / 2 + 1 ) )

*    For the first pass consider only elements in a symmetric interval
*    about the median value.

      HW = MIN( SMID - ARRAY( PMIN + 1 ), ARRAY( PMAX ) - SMID )
      CUT1 = SMID - HW
      CUT2 = SMID + HW

*    Initialise some variables for the next loop.

      SUM = 0.0
      SUM2 = 0.0
      SMIN = PMIN
      SMAX = PMAX

*    Sum for the deviation within this range.

      DO I = SMIN + 1, SMAX
         IF ( ARRAY( I ) .LT. CUT1 ) THEN
            SMIN = I
         ELSE
            IF ( ARRAY( I ) .GT. CUT2 ) GOTO 10
            DELTA = ARRAY( I ) - SMID
            SUM = SUM + DELTA
            SUM2 = SUM2 + DELTA * DELTA
            SMAX = I
         END IF
      END DO

  10  CONTINUE

      SAM = REAL ( SMAX - SMIN )

*    Compute the median, mean and sigma of this first pass. Note the
*    mean is initially calculated about smid to reduce the truncation
*    errors in sigma. Also the variance is checked in case it is zero
*    or negative (due to rounding).

      SMED = 0.5 * ( ARRAY( ( SMIN + SMAX + 1 ) / 2 ) +
     :       ARRAY( ( SMIN + SMAX ) / 2 + 1 ) )

      SMEAN = SUM / SAM
      VARNCE = 1. / ( SAM - 1. ) * ( SUM2 - SAM * SMEAN * SMEAN )
      IF ( VARNCE .GT. 0.0 ) THEN
         SIGMA = SQRT( VARNCE )
      ELSE
         SIGMA = 0.0
      END IF
      SMEAN = SMEAN + SMID

*    Now calculate the mode. If the mean is near the median (and mode),
*    that means the contamination is slight, and the mean value is what
*    is really needed. The exact constraint is arbitrary, so this may
*    cause an underestimate or overestimate of the mode when the
*    scatter in the peak of the distribution is caused by random
*    fluctuations rather than by contamination, but this bias is
*    negligible compared to the problem of contamination.
*    Otherwise use the empirical formula to calculate the mode.

      IF ( SIGMA .LT. VAL__SMLR ) THEN
         SMODE = SMEAN
      ELSE IF ( ABS( SMEAN - SMED ) / SIGMA .LT. 0.1 ) THEN
         SMODE = SMEAN
      ELSE
         SMODE = 3.0 * SMED - 2.0 * SMEAN
      END IF

*    Store the initial values in the last-result array.  These are
*    needed when the fine tuning oscillates.

      STAT( 1 ) = SMEAN
      STAT( 2 ) = SMED
      STAT( 3 ) = SMODE
      STAT( 4 ) = SIGMA

*    The rest of the routine improves the results by rejection and
*    recomputation.

      REDO = .TRUE.
      NITER = 0

      DO WHILE ( REDO )
         NITER = NITER + 1
         IF ( SMAX - SMIN .LT. MIND ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ICMMM__TFEWD',
     :        'Too few data to compute statistics.', STATUS )
            GOTO 999
         END IF

*       Compute the Chauvenet rejection criterion.

         R = ALOG10( REAL( SMAX - SMIN ) )
         R = MAX( 2.0, R * ( 1.1695 - 0.1042 * R ) + 0.8895 )

*       Compute rejection limits symmetric about the current mode.

         HW = R * SIGMA + 0.5 * ABS( SMEAN - SMODE )
         CUT1 = SMODE - HW
         CUT2 = SMODE + HW

*       Recompute the mean and sigma by adding and/or subtracting
*       data values at both ends of the interval of acceptable values.
*       Elements are added or subtracted depending on whether the limit
*       is moving towards or away from the mode.

         REDO = .FALSE.

*       Is the lower threshold above or below the currently accepted
*       minimum value?

         ISTEP = INT( SIGN( 1.0001, CUT1 - ARRAY( SMIN + 1 ) ) )
         JSTEP = ( ISTEP + 1 ) / 2

*      If istep = +1, jstep = 1. If istep = -1, jstep = 0. If istep = +1
*      then there is at least one pixel to be deleted at the low end.

         MOVE = .TRUE.
         DO WHILE ( MOVE )
            IF ( ( ISTEP .LT. 0 ) .AND. ( SMIN .LE. PMIN ) ) THEN
               MOVE = .FALSE.

*          Quit when the partition is located between the current
*          minimum element and its lower adjacent element.

            ELSE IF ( ( ARRAY( SMIN ) .LE. CUT1 ) .AND.
     :                ( ARRAY( SMIN + 1 ) .GE. CUT1 ) ) THEN
               MOVE = .FALSE.
            ELSE

*             Subtract the next higher data value below the lower
*             threshold from the summations, or add the next lower data
*             value above the threshold, depending on the direction the
*             partition is being moved.

               DELTA = ARRAY( SMIN + JSTEP ) - SMID
               SUM = SUM - ISTEP * DELTA
               SUM2 = SUM2 - ISTEP * DELTA * DELTA
               SMIN = SMIN + ISTEP
               REDO = .TRUE.
            END IF
         END DO

*       Is the upper threshold above or below the current maximum?

         ISTEP = INT( SIGN( 1.0001, CUT2 - ARRAY( SMAX ) ) )
         JSTEP = ( ISTEP + 1 ) / 2

*       If istep = +1, jstep = 1. If istep = -1, jstep = 0. If
*       istep = -1 then there is at least one pixel to be subtracted
*       from the high end.

         MOVE = .TRUE.
         DO WHILE ( MOVE )
            IF ( ( ISTEP .GT. 0 ) .AND. ( SMAX .GE. PMAX ) ) THEN
               MOVE = .FALSE.

*          Quit when the partition is located between the current
*          maximum element and its higher adjacent element.

            ELSE IF ( ( ARRAY( SMAX ) .LE. CUT2 ) .AND.
     :                ( ARRAY( SMAX + 1 ) .GE. CUT2 ) ) THEN
               MOVE = .FALSE.
            ELSE

*             Subtract the next higher data value above the upper
*             threshold from the summations, or add the next upper data
*             value below the threshold, depending on the direction the
*             partition is being moved.

               DELTA = ARRAY( SMAX + JSTEP ) - SMID
               SUM = SUM + ISTEP * DELTA
               SUM2 = SUM2 + ISTEP * DELTA * DELTA
               SMAX = SMAX + ISTEP
               REDO = .TRUE.
            END IF
         END DO

         NSAM = SMAX - SMIN
         IF ( NSAM .LT. MIND ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ICMMM__TFEWD',
     :        'Too few data to compute statistics.', STATUS )
            GOTO 999
         END IF

*       Compute the mean and sigma from this pass.  Watch for the case
*       where all the values are equal.

         SMEAN = SUM / REAL( NSAM )
         VARNCE = 1. / REAL( NSAM - 1 ) *
     :            ( SUM2 - REAL( NSAM ) * SMEAN * SMEAN )
         IF ( VARNCE .GT. 0.0 ) THEN
            SIGMA = SQRT( VARNCE )
         ELSE
            SIGMA = 0.0
         END IF
         SMEAN = SMEAN + SMID

*       Obtain the median. To first approximation the median is the
*       value of the sky in the middle pixel of the sorted data (if the
*       total number is odd) or the mean of the central two pixels (if
*       the total number of pixels is even).
*
*         SMED = 0.5 * ( ARRAY( ( SMIN + SMAX + 1 ) / 2 ) +
*     :          ARRAY( ( SMIN + SMAX ) / 2 + 1 ) )
*
*       However, this is not good enough. The estimator for the mode can
*       change by three units if there is a tiny change in the list of
*       data values sufficient to alter the median value of the sample
*       by one unit. We need something more robust than this. As a
*       first attempt try finding the median by calculating the mean of
*       the central nine or ten values (less if there are fewer elements
*       left).

         SMED = 0.0
         K = 0
         DO I = MAX( SMIN + 1, ( SMIN + SMAX - 8 ) / 2 ),
     :          MIN( SMAX, ( SMIN + SMAX + 11 ) / 2 )
            SMED = SMED + ARRAY( I )
            K = K + 1
         END DO

*       Had earlier check, so number cannot be zero.

         SMED = SMED / REAL( K )

*       Now calculate the mode as before.

         IF ( SIGMA .LT. VAL__SMLR ) THEN
            SMODE = SMEAN
         ELSE IF ( ABS( SMEAN - SMED ) / SIGMA .LT. 0.1 ) THEN
            SMODE = SMEAN
         ELSE
            SMODE = 3.0 * SMED - 2.0 * SMEAN
         END IF

*       If the system is oscillating about two states ad infinitum, just
*       average the two sets of values.  Exit the loop.

         IF ( NITER .EQ. MAXITR ) THEN
            SMEAN = 0.5 * ( SMEAN + STAT( 1 ) )
            SMED = 0.5 * ( SMED + STAT( 2 ) )
            SMODE = 0.5 * ( SMODE + STAT( 3 ) )
            SIGMA = 0.5 * ( SIGMA + STAT( 4 ) )
            REDO = .FALSE.
         ELSE

*          Store the current values in the last-result array.  These are
*          needed when the fine tuning oscillates.

            STAT( 1 ) = SMEAN
            STAT( 2 ) = SMED
            STAT( 3 ) = SMODE
            STAT( 4 ) = SIGMA
         END IF

*    End of iteration loop

      END DO

*    Calculate the skewness.

      SKEW = ( SMEAN - SMODE ) / MAX( 1.0, SIGMA )

  999 CONTINUE

      END
