      SUBROUTINE POL1_SNGVA( EL, DIN, T, PHI, EPS, DIMST, STOKES, SUM1,
     :                       SUM2, TVAR, WORK, DEZERO, ZERO, STATUS )

*+
*  Name:
*     POL1_SNGVA

*  Purpose:
*     Increment running sum images used to estimate input variances.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SNGVA( EL, DIN, T, PHI, EPS, DIMST, STOKES, SUM1, SUM2,
*                      TVAR, WORK, DEZERO, ZERO, STATUS )

*  Description:
*     For each pixel, this routine finds the residual between the
*     supplied input intensity value (DIN), and the corresponding
*     intensity value implied by the Stokes vectors (STOKES). The
*     corresponding pixel in SUM1 is incremented by 1.0 (if the
*     input intensity value and Stokes vectors is good), and the
*     corresponding pixel in SUM2 is incremented by the square of
*     the residual.
*
*     Also returns the mean squared residual (i.e. the mean variance) in
*     the supplied image alone (excluding unusally large residuals).

*  Arguments:
*     EL = INTEGER (Given)
*        The number of pixels in an image.
*     DIN( EL ) = REAL (Given)
*        The input intensity values read from the current input NDF.
*     T = REAL (Given)
*        The analyser transmission factor for the current NDF.
*     PHI = REAL (Given)
*        The analyser angle for the current NDF. In radians.
*     EPS = REAL (Given)
*        The analyser efficiency factor for the current NDF.
*     DIMST = INTEGER (Given)
*        No. of planes in STOKES.
*     STOKES( EL, DIMST ) = REAL (Given)
*        The current (smoothed) estimate of the Stokes parameters.
*        multiple of the standard deviation.
*     SUM1( EL ) = REAL (Given and Returned)
*        The number of good residuals stored in SUM2 at each pixel.
*     SUM2( EL ) = REAL (Given and Returned)
*        The sum of the squared residuals at each pixel.
*     TVAR = REAL (Given and Returned)
*        The mean squared residual in the supplied image. Returned as
*        VAL__BADR if the supplied image has no good data. Should be
*        supplied equal to VAL__BADR if no estimate is available.
*     WORK( EL ) = REAL (Given and Returned)
*        A work array.
*     DEZERO = LOGICAL (Given)
*        If .FALSE., then ZERO is always returned as 0.0. Otherwise,
*        its value is equal to the mean of the residuals for this image.
*     ZERO = REAL (Returned)
*        The zero point for this input NDF. The returned value should be
*        added on to the values stored in DIN before using the DIN value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-APR-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER EL
      REAL DIN( EL )
      REAL T
      REAL PHI
      REAL EPS
      INTEGER DIMST
      REAL STOKES( EL, DIMST )
      LOGICAL DEZERO

*  Arguments Given and Returned:
      REAL SUM1( EL )
      REAL SUM2( EL )
      REAL WORK( EL )

*  Arguments Returned:
      REAL TVAR
      REAL ZERO

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NBIN
      PARAMETER ( NBIN = 1000 )

*  Local Variables:
      INTEGER HI
      INTEGER HIST( NBIN )
      INTEGER I
      INTEGER J
      INTEGER LO
      INTEGER POP
      INTEGER TSUM1
      REAL DELTA
      REAL DHI
      REAL DLO
      REAL EXPECT
      REAL K1
      REAL K2
      REAL K3
      REAL MAXRES
      REAL MINRES
      REAL RES
      REAL SQRES
      REAL TLIM
      REAL TSUM2

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store some constants.
      K1 = 0.5*T
      K2 = EPS*COS( 2*PHI )
      K3 = EPS*SIN( 2*PHI )

*  Increment the max and min residual values.
      MAXRES = VAL__MINR
      MINRES = VAL__MAXR

*  Do each pixel.
      DO I = 1, EL

*  Check all values are good.
         IF( STOKES( I, 1 ) .NE. VAL__BADR .AND.
     :       STOKES( I, 2 ) .NE. VAL__BADR .AND.
     :       STOKES( I, 3 ) .NE. VAL__BADR .AND.
     :       DIN( I ) .NE. VAL__BADR ) THEN

*  Calculate the expected intensity value on the basis of the supplied
*  Stokes vector.
            EXPECT = K1*( STOKES( I, 1 ) + K2*STOKES( I, 2 )
     :                                   + K3*STOKES( I, 3 ) )

*  Store the residual.
            RES = EXPECT - DIN( I )
            WORK( I ) = RES

*  Increment the max and min residual values.
            MAXRES = MAX( MAXRES, RES )
            MINRES = MIN( MINRES, RES )

*  Store a bad value if no residual can be found.
         ELSE
            WORK( I ) = VAL__BADR
         END IF

      END DO

*  Set up the size of each histogram bin.
      DELTA = ( MAXRES - MINRES )/NBIN

*  If no zero point corrections are to be performed, use a zero point of
*  0.0.
      IF( .NOT. DEZERO ) THEN
         ZERO = 0.0

*  Otherwise, if delta is zero, the mean variance value is equal to the
*  maximum residual (and the minimum residual). In this case there is no
*  need to form the histogram.
      ELSE IF( DELTA .EQ. 0 ) THEN
         ZERO = MAXRES

*  Otherwise,
      ELSE

*  Initialise the histogram to hold zeros in every bin.
         DO J = 1, NBIN
            HIST( J ) = 0
         END DO

*  Bin the residuals stored in the image formed above.
         DO I = 1, EL
            IF( WORK( I ) .NE. VAL__BADR ) THEN
               J = INT( ( WORK( I ) - MINRES )/DELTA ) + 1
               J = MAX( 1, MIN( NBIN, J ) )
               HIST( J ) = HIST( J ) + 1
            END IF
         END DO

*  Count the total histogram population.
         POP = 0
         DO J = 1, NBIN
            POP = POP + HIST( J )
         END DO

*  For the cumulative populations at the 5% and 95% points in the
*  histogram.
         LO = NINT( 0.05*REAL( POP ) )
         HI = NINT( 0.95*REAL( POP ) )

*  Find the data value corresponding to these percentiles.
         J = 1
         POP = HIST( J )
         DO WHILE( POP .LT. LO .AND. J .LE. NBIN )
            J = J + 1
            POP = POP + HIST( J )
         END DO
         DLO = ( J - 1 )*DELTA + MINRES

         DO WHILE( POP .LT. HI .AND. J .LE. NBIN )
            J = J + 1
            POP = POP + HIST( J )
         END DO
         DHI = J*DELTA + MINRES

*  Find the mean of the residuals between these two limits.
         TSUM1 = 0
         TSUM2 = 0.0
         DO I = 1, EL
            RES = WORK( I )
            IF( RES .NE. VAL__BADR ) THEN
               IF( RES .GE. DLO .AND. RES .LE. DHI ) THEN
                  TSUM2 = TSUM2 + RES
                  TSUM1 = TSUM1 + 1
               END IF
            END IF
         END DO

         IF( TSUM1 .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'POL1_SNGVA_ERR1', 'All data rejected '//
     :                    'while estimating variances.', STATUS )
            GO TO 999
         ELSE
            ZERO = TSUM2/TSUM1
         END IF

      END IF

*  Increment the supplied running sum image. Do each pixel.
      TSUM1 = 0
      TSUM2 = 0.0

      DO I = 1, EL

*  Check the residual is good.
         IF( WORK( I ) .NE. VAL__BADR ) THEN

*  Form the squared residual, correcting for zero point drift.
            SQRES = ( WORK( I ) - ZERO )**2

*  Increment the running sum images.
            SUM1( I ) = SUM1( I ) + 1.0
            SUM2( I ) = SUM2( I ) + SQRES

*  Increment the running sum values for this single image.
            TSUM1 = TSUM1 + 1
            TSUM2 = TSUM2 + SQRES

         END IF

      END DO

*  Return the mean variance in this image.
      IF( TSUM1 .GT. 0 ) THEN
         TVAR = TSUM2/REAL( TSUM1 )
      ELSE
         TVAR = VAL__BADR
      END IF

 999  CONTINUE

      END
