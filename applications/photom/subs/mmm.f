************************************************************************

      SUBROUTINE MMM ( SKY, NSKY, SMEAN, SMED, SMODE, SIGMA, SKEW )

*+
*  Name :
*     MMM
*
*  Purpose :
*     This finds the mean, median and mode ( mmm ) of a sample of data
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL MMM( SKY, NSKY, SMEAN, SMED, SMODE, SIGMA, SKEW )
*
*  Description :
*     This finds the mean, median and mode ( mmm ) of a sample of data
*     taken from a background region of an astronomical plate. It should
*     not be used as a general mmm routine as there are special features
*     which are peculiar to the problem of contamination by stars. The
*     data should not include any invalid pixel values as these are not
*     trapped. The data need not be sorted as a NAG sorting routine
*     ( M01CAE, single precision ) is called internally. The routine also
*     returns the standard deviation and the skew of the sample.
*     If an error occurs somewhere then the sigma is set to -1.
*
*  Arguments :
*     SKY( NSKY ) = REAL (Given)
*        Vector of data samples
*     NSKY = INTEGER (Given and Returned)
*        Number of data samples in sky vector
*     SMEAN = REAL (Returned)
*        Mean of sample
*     SMED = REAL (Returned)
*        Median of sample
*     SMODE = REAL (Returned)
*        Mode of sample
*     SIGMA = REAL (Returned)
*        Standard deviation of sample
*     SKEW = REAL (Returned)
*        Skew of sample
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     {enter_new_authors_here}
*
*  History :
*     10-JAN-1988 (NE):
*        Original version.
*        The routine has been taken from a similar routine in DAOPHOT.
*     10-MAR-1991 (NE):
*        Fixed bug for median estimator for less than 8 pixels.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Arguments Given and Returned :
      INTEGER NSKY

*  Arguments Given :
      REAL SKY( NSKY )

*  Arguments Returned :
      REAL SMEAN
      REAL SMED
      REAL SMODE
      REAL SIGMA
      REAL SKEW

*  Local Variables :
      LOGICAL REDO

      INTEGER I, I1, I2, ISTEP, JSTEP, K, SMAX, MAXITR, SMIN,
     :        MINSKY, NITER

      DOUBLE PRECISION CUT1, CUT2, DELTA, HW, R, SMID, SUM, SUM2
      REAL WORK

*  Local Data :
      DATA MAXITR / 20 /, MINSKY / 5 /
*.

*   Begin by sorting the data.
      CALL SHSORT( 'A', 1, NSKY, 1, WORK, SKY )

*   Calculate the median value for the whole ensemble of sky pixels.
*   Note that if nsky is odd then (nsky+1)/2 and (nsky/2)+1 will be the
*   same number, whereas if nsky is even, they will be different
*   numbers. This same trick will be used again later.

      SMID = 0.5D0 * ( DBLE( SKY( ( NSKY + 1 ) / 2 ) ) +
     :                 DBLE( SKY( NSKY / 2 + 1 ) ) )

*   For the first pass consider only pixels in a symmetric interval
*   about the median value.

      HW = MIN( SMID - DBLE( SKY( 1 ) ), DBLE( SKY( NSKY ) ) - SMID )
      CUT1 = SMID - HW
      CUT2 = SMID + HW

*   Initialise some variables for the next loop.

      SUM = 0.0D0
      SUM2 = 0.0D0
      SMIN = 0
      SMAX = NSKY

*   Sum for the deviation within this range.
*   Henceforth smin points to the highest value rejected at the lower
*   end of the vector, and smax points to the highest value accepted at
*   the upper end of the vactor, thus smax - smin is the number of
*   pixels within the acceptance range.

      DO I = 1, NSKY
         IF ( DBLE( SKY( I ) ) .LT. CUT1 ) THEN
            SMIN = I
         ELSE
            IF ( DBLE( SKY( I ) ) .GT. CUT2 ) GOTO 10
            DELTA = DBLE( SKY( I ) ) - SMID
            SUM = SUM + DELTA
            SUM2 = SUM2 + DELTA ** 2
            SMAX = I
         ENDIF
      ENDDO

  10  CONTINUE

*   Compute the mean and sigma of this first pass. Note the mean is
*   initially calculated about smid to reduce the truncation errors
*   in sigma.

      SMED = 0.5D0 * ( DBLE( SKY( ( SMIN + SMAX + 1 ) / 2 ) ) +
     :                 DBLE( SKY( ( SMIN + SMAX ) / 2 + 1 ) ) )
      SMEAN = REAL( SUM / DBLE( SMAX - SMIN ) )
      SIGMA = REAL( SQRT( SUM2 / DBLE( SMAX - SMIN ) -
     :              DBLE( SMEAN ) **2 ) )
      SMEAN = SMEAN + REAL( SMID )

*   Now calculate the mode. If the mean is less than the mode, that
*   means the contamination is slight, and the mean value is what is
*   really needed. Note that this introduces a slight bias toward
*   underestimating the sky when the scatter in the sky is caused by
*   random fluctuations rather than by contamination, but this bias is
*   negligible compared to the problem of contamination.
*   Use the empirical formula to calculate the mode.

      SMODE = SMEAN
      IF ( SMED .LT. DBLE( SMEAN ) ) THEN
         SMODE = REAL( 3.0D0 * SMED - 2.0D0 * DBLE( SMEAN ) )
      ENDIF

*   The rest of the routine improves the results by rejection and
*   recomputation.

      REDO = .TRUE.
      NITER = 0
      DO WHILE ( REDO )
         NITER = NITER + 1
         IF ( ( NITER .GT. MAXITR ) .OR.
     :        ( SMAX - SMIN .LT. MINSKY ) ) THEN
            SIGMA = -1.0
            GOTO 99
         ENDIF

*   Compute Chauvenet rejection criterion

         R = LOG10( DBLE( SMAX - SMIN ) )
         R = MAX( 2.0D0, R * ( 1.1695D0 - 0.1042D0 * R ) + 0.8895D0 )

*   Compute rejection limits symmetric about the current mode

         HW = R * DBLE( SIGMA ) + 0.5D0 * DBLE( ABS( SMEAN - SMODE ) )
         HW = MAX( 1.5D0, HW )
         CUT1 = DBLE( SMODE ) - HW
         CUT2 = DBLE( SMODE ) + HW

*   Recompute the mean and sigma by adding and/or subtracting sky values
*   at both ends of the interval of acceptable values.
*   At each end of the interval istep will show the direction to step
*   through the vector to go from the old partition to the new one.
*   Pixels are added or subtracted depending on whether the limit is
*   moving toward or away from the mode.

         REDO = .FALSE.

*   Is cut1 above or below the minimum currently accepted value?

         ISTEP = INT( SIGN( 1.0001D0, CUT1 - DBLE( SKY( SMIN + 1 ) ) ) )
         JSTEP = ( ISTEP + 1 ) / 2

*   If istep = +1, jstep = 1. If istep = -1, jstep = 0. If istep = +1
*   then there is at least one pixel to be deleted at the low end.

         IF ( ISTEP .GT. 0 ) GOTO 30
  20     CONTINUE
         IF ( ( ISTEP .LT. 0 ) .AND. ( SMIN .LE. 0 ) ) GOTO 40

*   Quit when sky( smin ) < cut1 <= sky( smin + 1 )

         IF ( ( DBLE( SKY( SMIN ) ) .LE. CUT1 ) .AND.
     :        ( DBLE( SKY( SMIN + 1 ) ) .GE. CUT1 ) ) GOTO 40

*   If istep is positive, subtract out the sky value at smin + 1,
*   if istep is negative, add in the sky value at smin.

  30     CONTINUE
         DELTA = DBLE( SKY( SMIN + JSTEP ) ) - SMID
         SUM = SUM - ISTEP * DELTA
         SUM2 = SUM2 - ISTEP * DELTA ** 2
         SMIN = SMIN + ISTEP
         REDO = .TRUE.
         GOTO 20

  40     CONTINUE

*   Is cut2 above or below the current maximum

         ISTEP = INT( SIGN( 1.0001D0, CUT2 - DBLE( SKY( SMAX ) ) ) )
         JSTEP = ( ISTEP + 1 ) / 2

*   If istep = +1, jstep = 1. If istep = -1, jstep = 0. If istep = -1
*   then there is at least one pixel to be subtracted from the high end.

         IF ( ISTEP .LT. 0 ) GOTO 60
  50     CONTINUE
         IF ( ( ISTEP .GT. 0 ) .AND. ( SMAX .GE. NSKY ) ) GOTO 70

*   Quit when sky( smax ) <= cut2 < sky( smax + 1 )

         IF ( ( DBLE( SKY( SMAX ) ) .LE. CUT2 ) .AND.
     :        ( DBLE( SKY( SMAX + 1 ) ) .GE. CUT2 ) ) GOTO 70

*   If istep is positive, add in the sky value at smax + 1,
*   if istep is negative, subtract off the sky value at smax.

  60     CONTINUE
         DELTA = DBLE( SKY( SMAX + JSTEP ) ) - SMID
         SUM = SUM + ISTEP * DELTA
         SUM2 = SUM2 + ISTEP * DELTA ** 2
         SMAX = SMAX + ISTEP
         REDO = .TRUE.
         GOTO 50

  70     CONTINUE

*   Compute mean and sigma from this pass

         SMEAN = REAL( SUM / DBLE( SMAX - SMIN ) )
         SIGMA = REAL( SQRT( SUM2 / DBLE( SMAX - SMIN ) -
     :                       DBLE( SMEAN ) ** 2 ) )
         SMEAN = SMEAN + REAL( SMID )

*   Obtain the median. To first approximation the meadian is the value
*   of the sky in the middle pixel of the sorted data ( if the total
*   number is odd ) or the mean of the central two pixels ( if the total
*   number of pixels is even ).
*
*         SMED = 0.5 * ( SKY( ( SMIN + SMAX + 1 ) / 2 ) +
*     :          SKY( ( SMIN + SMAX ) / 2 + 1 ) )
*
*   However this is not good enough. The estimator for the mode can
*   change by three units if there is a tiny change in the list of sky
*   pixels sufficient to alter the median value of the sky brightness by
*   one unit. Need something more robust than this. As a first attempt
*   try finding the median by calculating the mean of the central nine
*   or ten sky values.

         SMED = 0.0D0
         K = 0
         I1 = MAX( SMIN + 1, ( SMIN + SMAX - 8 ) / 2 )
         I2 = MIN( SMAX, ( SMIN + SMAX + 11 ) / 2 )
         DO I = I1, I2
            SMED = SMED + DBLE( SKY( I ) )
            K = K + 1
         ENDDO
         SMED = SMED / DBLE( K )

*   Now calculate the mode. If the mean is less than the mode, that
*   means the contamination is slight, and the mean value is what is
*   really needed. Note that this introduces a slight bias toward
*   underestimating the sky when the scatter in the sky is caused by
*   random fluctuations rather than by contamination, but this bias is
*   negligible compared to the problem of contamination.
*   Use the empirical formula to calculate the mode.

         SMODE = SMEAN
         IF ( SMED .LT. DBLE( SMEAN ) ) THEN
            SMODE = REAL( 3.0D0 * SMED - 2.0D0 * DBLE( SMEAN ) )
         ENDIF

*   End of iteration loop

      ENDDO

*   Calculate skew

      SKEW = ( SMEAN - SMODE ) / MAX( 1.0, SIGMA )
      NSKY = SMAX - SMIN

  99  CONTINUE

      END

* $Id$
