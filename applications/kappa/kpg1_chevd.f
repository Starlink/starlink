      SUBROUTINE KPG1_CHEVD( XMIN, XMAX, NCOEF, CHCOEF, NPTS, X,
     :                         EVAL, STATUS )
*+
*  Name:
*     KPG1_CHEVx

*  Purpose:
*     Evaluates a one-dimensional Chebyshev polynomial.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_CHEVx( XMIN, XMAX, NCOEF, CHCOEF, NPTS, X, EVAL,
*                      STATUS )

*  Description:
*     This routine evaluates a one-dimensional Chebyshev polynomial for
*     one or more arguments.  It uses Clenshaw's recurrence
*     relationship.

*  Arguments:
*     XMIN = ? (Given)
*        The lower endpoint of the range of the fit.  The Chebyshev
*        series representation is in terms of a normalised variable,
*        evaluated as ( 2x - (XMAX + XMIN) ) / (XMAX - XMIN), where x
*        is the original variable.  XMIN must be less than XMAX.
*     XMAX = ? (Given)
*        The upper endpoint of the range of the fit.  See XMIN.
*     NCOEF = INTEGER (Given)
*        The number of coefficients.  This must be at least the
*        polynomial order plus one.
*     CC( NCOEF ) = ? (Given)
*        The Chebyshev coefficients.
*     NPTS = INTEGER (Given)
*        The number of arguments for which the Chebyshev polynomial is
*        to be evaluated.
*     X( NPTS ) = ? (Given)
*        The arguments for which the Chebyshev polynomial is to be
*        evaluated.
*     EVAL( NPTS ) = ? (Returned)
*        The evaluated polynomial for the supplied arguments.  Should an
*        argument lie beyond the range [XMIN,XMAX], the bad value is
*        returned in the corresponding element of EVAL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Notes:
*     There is a routine for the real and double precision data types:
*     replace "x" in the routine name by R or D respectively.  The
*     XMIN, XMAX, CC, X, and EVAL arguments supplied to the routine
*     must have the data type specified.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 January 29 (MJC):
*        Original version.
*     1997 July 31 (MJC):
*        Corrected a couple of error messages and tests.  Made more
*        efficient for a constant (zero-order) fit.
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
      DOUBLE PRECISION XMIN
      DOUBLE PRECISION XMAX
      INTEGER NCOEF
      DOUBLE PRECISION CHCOEF( NCOEF )
      INTEGER NPTS
      DOUBLE PRECISION X( NPTS )

*  Arguments Returned:
      DOUBLE PRECISION EVAL( NPTS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION D                  ! Summation to current order
      DOUBLE PRECISION DP1                ! Summation to current order plus one
      DOUBLE PRECISION DP2                ! Summation to current order plus two
      INTEGER I                  ! Loop counter
      INTEGER ORDER              ! Loop counter for polynomial order
      DOUBLE PRECISION XN                 ! Normalised argument
      DOUBLE PRECISION XN2                ! Twice the normalised argument
      DOUBLE PRECISION XR                 ! Argument range

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the data limits.
      IF ( XMAX .LE. XMIN ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETD( 'MX', XMAX )
         CALL MSG_SETD( 'MN', XMIN )
         CALL ERR_REP( 'KPG1_CHEVx_RANGE',
     :     'Scaling range for Chebyshev polynomial has minimum (^MN) '/
     :     /'not less than the maximum (^MX).  Programming error.',
     :     STATUS )
         GOTO 999
      END IF

*  Validate the number of coefficients.
      IF ( NCOEF .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NC', NCOEF )
         CALL ERR_REP( 'KPG1_CHEVx_NCOEF',
     :     'Chebyshev polynomial has fewer than 1 term.  '/
     :     /'Programming error.', STATUS )
         GOTO 999
      END IF

*  First deal with the trivial case for efficiency.
      IF ( NCOEF .EQ. 1 ) THEN

*  Assign the constant to each point.  Note the convention where the
*  first coefficient is halved.
         DO I = 1, NPTS
            EVAL( I ) = CHCOEF( 1 ) / 2.0D0
         END DO

*  Now tackle the general case.
      ELSE

*  Define a useful variable.
         XR = XMAX - XMIN

*  Loop for all the values.
         DO I = 1, NPTS

*  Check that the argument is in range.  When it isn't, assign the
*  bad value.
            IF ( ( X( I ) - XMAX ) *
     :           ( X( I ) - XMIN ) .GT. 0.0D0 ) THEN
               EVAL( I ) = VAL__BADD
            ELSE            

*  Normalise the variable to lie in the range -1 to +1.  This form of
*  the expression guarantees that the computed normalised value lies
*  no more than four times the machine precision from its true value.
               XN = ( ( X( I ) - XMIN ) - ( XMAX - X( I ) ) ) / XR

*  Initialise variable for recurrence relationship.
               XN2 = 2.0D0 * XN

*  Apply Clenshaw's recurrence relationship for efficiency.  For terms
*  greater than NCOEF the value is zero.
               DP2 = 0.0D0
               DP1 = 0.0D0
               IF ( NCOEF .GT. 1 ) THEN
                  DO ORDER = NCOEF, 2, -1
                     D = DP1
                     DP1 = XN2 * DP1 - DP2 + CHCOEF( ORDER )
                     DP2 = D
                  END DO
               END IF

*  The final iteration is different.  The constant term is half of the
*  coefficient in the Chebyshev series.
               EVAL( I ) = DP1 * XN - DP2 + CHCOEF( 1 ) / 2.0D0
            END IF
         END DO
      END IF

  999 CONTINUE

      END
