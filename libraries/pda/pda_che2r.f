      SUBROUTINE PDA_CHE2R( NPTS, XMIN, XMAX, X, YMIN, YMAX, Y, XDEG,
     :                      YDEG, NCOEF, CC, NW, WORK, EVAL, IFAIL )
*+
*  Name:
*     PDA_CHE2X

*  Purpose:
*     Evaluates a 2-dimensional Chebyshev polynomial.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PDA_CHE2X( NPTS, XMIN, XMAX, X, YMIN, YMAX, Y, XDEG,
*                     YDEG, NCOEF, CC, NW, WORK, EVAL, IFAIL )

*  Description:
*     This routine evaluates a two-dimensional Chebyshev polynomial for
*     one or more arguments.  It uses Clenshaw's recurrence
*     relationship twice.

*  Arguments:
*     XMIN = REAL (Given)
*        The lower endpoint of the range of the fit along the first
*        dimension.  The Chebyshev series representation is in terms of
*        a normalised variable, evaluated as (2x - (XMAX + XMIN) ) /
*        (XMAX - XMIN), where x is the original variable.  XMIN must be
*        less than XMAX.
*     XMAX = REAL (Given)
*        The upper endpoint of the range of the fit along the second
*        dimension.  See XMIN.
*     X( NPTS ) = REAL (Given)
*        The co-ordinates along the first dimension for which the
*        Chebyshev polynomial is to be evaluated.
*     YMIN = REAL (Given)
*        The lower endpoint of the range of the fit along the first
*        dimension.  The Chebyshev series representation is in terms of
*        a normalised variable, evaluated as (2y - (YMAX + YMIN) ) /
*        (YMAX - YMIN), where y is the original variable.  YMIN must be
*        less than YMAX.
*     YMAX = REAL (Given)
*        The upper endpoint of the range of the fit along the second
*        dimension.  See YMIN.
*     Y = REAL (Given)
*        The co-ordinate along the second dimension for which the
*        Chebyshev polynomial is to be evaluated.
*     XDEG = INTEGER (Given)
*        The degree of the polynomial along the first dimension.
*     YDEG = INTEGER (Given)
*        The degree of the polynomial along the second dimension.
*     MCOEF = INTEGER (Given)
*        The number of coefficients.  This must be at least the product
*        of (XDEG+1) * (YDEG+1).
*     CC( MCOEF ) = REAL (Given)
*        The Chebyshev coefficients.  These should be the order such
*        that CCij is in CC( i*(YDEG+1)+j+1 ) for i=0,XDEG; j=0,YDEG.
*        In other words the opposite order to Fortran standard.
*     NW = INTEGER (Given)
*        The number of elements in the work array.  It must be at least
*        XDEG + 1.
*     WORK( NW ) = REAL (Returned)
*        Workspace.
*     EVAL( NPTS ) = REAL (Returned)
*        The evaluated polynomial for the supplied arguments.  Should an
*        element of argument X lie beyond the range [XMIN,XMAX], IFAIL=7
*        is reutrned.
*     IFAIL = INTEGER (Returned)
*        The status.  A value of 0 indicates that the routine completed
*        successfully.  Positive values indicate the following errors:
*
*           IFAIL = 1    XMAX less than or equal to XMIN
*           IFAIL = 2    YMAX less than or equal to YMIN
*           IFAIL = 3    NCOEF less than 1.
*           IFAIL = 4    XDEG or YDEG less than 1.
*           IFAIL = 5    Number of coefficients is too great, namely
*                        (XDEG+1)*(YDEG+1) is greater than NCOEF.
*           IFAIL = 6    Y lies outside the range YMIN to YMAX.
*           IFAIL = 7    An element of X lies outside the range XMIN to
*                        XMAX.

*  Notes:
*     - A double precision version of this function is available, named
*     PDA_CHE2D.

*  [optional_subroutine_items]...

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 February 28 (MJC):
*        Original version based upon KPG1_CHE2x.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER NPTS
      REAL XMIN
      REAL XMAX
      REAL X( NPTS )
      REAL YMIN
      REAL YMAX
      REAL Y
      INTEGER XDEG
      INTEGER YDEG
      INTEGER NCOEF
      REAL CC( NCOEF )
      INTEGER NW

*  Arguments Returned:
      REAL WORK( NW )
      REAL EVAL( NPTS )

*  Status:
      INTEGER IFAIL              ! Returned status

*  Local Variables:
      INTEGER IOFFCC             ! Index offset of Chebyshev coefficient
      REAL D                  ! Summation to current order
      REAL DP1                ! Summation to current order plus one
      REAL DP2                ! Summation to current order plus two
      INTEGER ECOEF              ! Expected number of Chebyshev
                                 ! coefficients
      INTEGER I                  ! Loop counter for summation over
                                 ! second axis
      INTEGER K                  ! Loop counter for co-ordinates
      INTEGER ORDER              ! Loop counter for polynomial order
      REAL XN2                ! Twice the normalised first-axis
                                 ! co-ordinate
      REAL XNORM              ! Normalised first-axis co-ordinate
      REAL XR                 ! First-axis co-ordinate range
      REAL YN2                ! Twice the normalised second-axis
                                 ! co-ordinate
      REAL YNORM              ! Normalised second-axis co-ordinate
      REAL YR                 ! Second-axis co-ordinate range

*.

*  Initialsie the status.
      IFAIL = 0

*  Validate the data limits.
      IF ( XMAX .LE. XMIN ) THEN
         IFAIL = 1
         GOTO 999
      END IF

*  Validate the data limits.
      IF ( YMAX .LE. YMIN ) THEN
         IFAIL = 2
         GOTO 999
      END IF

*  Validate the number of coefficients.
      IF ( NCOEF .LT. 1 ) THEN
         IFAIL = 3
         GOTO 999
      END IF

*  Validate the orders.
      IF ( XDEG .LT. 1 .OR. YDEG .LT. 1 ) THEN
         IFAIL = 4
         GOTO 999
      END IF

*  Check that the number of coefficients is not excessive.
      ECOEF = ( XDEG + 1 ) * ( YDEG + 1 )
      IF ( ECOEF .GT. NCOEF ) THEN
         IFAIL = 5
         GOTO 999
      END IF

*  Test for an invalid Y co-ordinate.
      IF ( ( Y - YMAX ) * ( Y - YMIN ) .GT. 0.0E0 ) THEN
         IFAIL = 6
         GOTO 999
      END IF

*  Loop for each point to be evaluated.
      DO K = 1, NPTS

*  Check that the x co-ordinate is in range.  Exit with a failure if any
*  of the x co-ordinates lie out of the Chebyshev range.
         IF ( ( X( K ) - XMAX ) * ( X( K ) - XMIN ) .GT. 0.0E0 ) THEN
            IFAIL = 7
            GOTO 999
         END IF
      END DO

*  Evaluate results.
*  =================

*  Define two useful variables.
      XR = XMAX - XMIN
      YR = YMAX - YMIN

*  Normalise the variable to lie in the range -1 to +1.  This form of
*  the expression guarantees that the computed normalised value lies
*  no more than four times the machine precision from its true value.
      YNORM = ( ( Y - YMIN ) - ( YMAX - Y ) ) / YR

*  Initialise variable for recurrence relationship.
      YN2 = 2.0E0 * YNORM

*  Sum the coefficients times the second-axis Chebyshev polynomials.
*  =================================================================

*  Apply Clenshaw's recurrence relationship for efficiency.  For terms
*  greater than NCOEF the value is zero.  Note the
      DO I = 1, XDEG + 1
         IOFFCC = ( I - 1 ) * ( YDEG + 1 )
         DP2 = 0.0E0
         DP1 = 0.0E0
         IF ( YDEG .GT. 1 ) THEN
            DO ORDER = YDEG + 1, 2, -1
               D = DP1
               DP1 = YN2 * DP1 - DP2 + CC( IOFFCC + ORDER )
               DP2 = D
            END DO
         END IF

*  The final iteration is different.  The constant term is half of the
*  coefficient in the Chebyshev series.
         WORK( I ) = DP1 * YNORM - DP2 + CC( IOFFCC + 1 ) / 2.0E0
      END DO

*  Loop for each point to be evaluated.
      DO K = 1, NPTS

*  Normalise the variable to lie in the range -1 to +1.  This form of
*  the expression guarantees that the computed normalised value lies
*  no more than four times the machine precision from its true value.
         XNORM = ( ( X( K ) - XMIN ) - ( XMAX - X( K ) ) ) / XR

*  Sum the C(I)s times the first-axis Chebyshev polynomials.
*  =========================================================

*  Initialise variable for recurrence relationship.
         XN2 = 2.0E0 * XNORM

*  Apply Clenshaw's recurrence relationship for efficiency.  For terms
*  greater than NCOEF the value is zero.
         DP2 = 0.0E0
         DP1 = 0.0E0
         IF ( XDEG .GT. 1 ) THEN
            DO ORDER = XDEG + 1, 2, -1
               D = DP1
               DP1 = XN2 * DP1 - DP2 + WORK( ORDER )
               DP2 = D
            END DO
         END IF

*  The final iteration is different.  The constant term is half of the
*  coefficient in the Chebyshev series.
         EVAL( K ) = DP1 * XNORM - DP2 + WORK( 1 ) / 2.0E0
      END DO

  999 CONTINUE

      END
