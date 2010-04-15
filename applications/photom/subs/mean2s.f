************************************************************************

      SUBROUTINE MEAN2S ( SKY, NSKY, SMEAN, SIGMA )

*+
*  Name :
*     MEAN2S
*
*  Purpose :
*     This calculates the mean after applying 2 sigma rejection
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL MEAN2S( SKY, NSKY, SMEAN, SIGMA )
*
*  Description :
*     This calculates the mean after applying 2 sigma rejection
*
*  Arguments :
*     SKY( NSKY ) = REAL (Given)
*        Vector of data samples
*     NSKY = INTEGER (Given)
*        Dimension of data array
*     SMEAN = REAL (Returned)
*        Mean of sample with 2 sigma rejection
*     SIGMA = REAL (Returned)
*        Standard deviation of sample
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
*     10-SEP-1989 (NE):
*        Original version.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Arguments Given :
      INTEGER NSKY
      REAL SKY( NSKY )

*  Arguments Returned :
      REAL SMEAN
      REAL SIGMA

*  Local Constants :
*   Maximum number of iterations of the rejection procedure.
      INTEGER MXITER
      PARAMETER ( MXITER = 3 )

*  Local Variables :
      INTEGER J, NITER, NS, OLDMAX, OLDMIN, SMAX, SMIN

      DOUBLE PRECISION D, HW, SHI, SLO, SMID, SUM, SUM2, TWOSIG
      REAL WORK
*.

*   Sort the data. This eases computation for the rejection loops.
      CALL SHSORT( 'A', 1, NSKY, 1, WORK, SKY )

*   Calculate the median value for the whole ensemble of sky pixels.
*   Note that if nsky is odd then (nsky+1)/2 and (nsky/2)+1 will be the
*   same number, whereas if nsky is even, they will be different
*   numbers.
      SMID = 0.5D0 * ( DBLE( SKY( ( NSKY + 1 ) / 2 ) ) +
     :                 DBLE( SKY( NSKY / 2 + 1 ) ) )

*   For the first pass consider only pixels in a symmetric interval
*   about the median value.
      HW = MIN( SMID - DBLE( SKY( 1 ) ), DBLE( SKY( NSKY ) ) - SMID )
      SLO = SMID - HW
      SHI = SMID + HW

*   Initialise some variables for the next loop.
      SUM = 0.0D0
      SUM2 = 0.0D0
      SMIN = 1
      SMAX = NSKY

*   Sum for the deviation within this range.
      DO J = 1, NSKY
         IF ( DBLE( SKY( J ) ) .LT. SLO ) THEN
            SMIN = J + 1
         ELSE
            IF ( DBLE( SKY( J ) ).GT. SHI ) GOTO 10
            D = DBLE( SKY( J ) ) - SMID
            SUM = SUM + D
            SUM2 = SUM2 + D ** 2
            SMAX = J
         ENDIF
      ENDDO

  10  CONTINUE

*   Compute the mean and sigma of this first pass. Note the mean is
*   initially calculated about smid to reduce the truncation errors
*   in sigma.
      NS = SMAX - SMIN + 1
      SMEAN = REAL( SUM / DBLE( NS ) )
      SIGMA = REAL( SQRT( SUM2 / DBLE( NS ) - DBLE( SMEAN ) ** 2 ) )
      SMEAN = SMEAN + REAL( SMID )

*   Loop back to here for the main rejection loop.
      NITER = 0
  20  CONTINUE

*   Calculate the mean and standard deviation
      NS = SMAX - SMIN + 1
      SMEAN = REAL( SUM / DBLE( NS ) )
      SIGMA = REAL( SQRT( SUM2 / DBLE( NS ) - DBLE( SMEAN ) ** 2 ) )
      SMEAN = SMEAN + REAL( SMID )

*   Check the number of iterations
      IF ( NITER .GE. MXITER ) THEN
         GOTO 99
      ENDIF
      NITER = NITER + 1

*   Calculate the 2 sigma rejection limits
      TWOSIG = 2.0D0 * DBLE( SIGMA )
      SLO = DBLE( SMEAN ) - TWOSIG
      SHI = DBLE( SMEAN ) + TWOSIG

*   Find out the indices of the points within the 2 sigma limits.
*   Move the indices according to the direction of the shift.
*   Check the lower index and update the sums
      OLDMIN = SMIN
      IF ( SLO .GE. DBLE( SKY( SMIN ) ) ) THEN
  30     CONTINUE
         IF ( SMIN .LT. NSKY ) THEN
            IF ( DBLE( SKY( SMIN ) ) .LT. SLO ) THEN
               D = DBLE( SKY( SMIN ) ) - SMID
               SUM = SUM - D
               SUM2 = SUM2 - D ** 2
               SMIN = SMIN + 1
               GOTO 30
            ENDIF
         ENDIF
      ELSE
  40     CONTINUE
         IF ( SMIN .GT. 1 ) THEN
            IF ( DBLE( SKY( SMIN - 1 ) ) .GT. SLO ) THEN
               SMIN = SMIN - 1
               D = DBLE( SKY( SMIN ) ) - SMID
               SUM = SUM + D
               SUM2 = SUM2 + D ** 2
               GOTO 40
            ENDIF
         ENDIF
      ENDIF

*   Check the upper index and update the sums.
      OLDMAX = SMAX
      IF ( SHI .LE. DBLE( SKY( SMAX ) ) ) THEN
  50     CONTINUE
         IF ( SMAX .GT. 1 ) THEN
            IF ( DBLE( SKY( SMAX ) ) .GT. SHI ) THEN
               D = DBLE( SKY( SMAX ) ) - SMID
               SUM = SUM - D
               SUM2 = SUM2 - D ** 2
               SMAX = SMAX - 1
               GOTO 50
            ENDIF
         ENDIF
      ELSE
  60     CONTINUE
         IF ( SMAX .LT. NSKY ) THEN
            IF ( DBLE( SKY( SMAX + 1 ) ) .LT. SHI ) THEN
               SMAX = SMAX + 1
               D = DBLE( SKY( SMAX ) ) - SMID
               SUM = SUM + D
               SUM2 = SUM2 + D ** 2
               GOTO 60
            ENDIF
         ENDIF
      ENDIF

*   If the limits are different to the old ones then repeat the
*   rejection loop.
      IF ( ( OLDMIN .NE. SMIN ) .OR. ( OLDMAX .NE. SMAX ) ) THEN
         GOTO 20
      ENDIF

  99  CONTINUE

      END

* $Id$
