      SUBROUTINE IRM_FIT2( LBND, UBND, Y, X, M, C, RMS, STATUS )
*+
*  Name:
*     IRM_FIT2

*  Purpose:
*     Fit a least squares straight line to supplied data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_FIT2( LBND, UBND, Y, X, M, C, RMS, STATUS )

*  Description:
*     A straight line is fitted to the data supplied in X and Y, using
*     the least squares criterion. The returned values of M and C are
*     the gradient and intercept of the fit, so that y = M.x + C. The
*     RMS residual of the Y data from the fit is returned in RMS.
*
*     An error is reported if there are less than two good data values
*     in Y, or if the X values cover a range of zero.

*  Arguments:
*     LBND = INTEGER (Given)
*        The lower bound of the X and Y arrays.
*     UBND = INTEGER (Given)
*        The upper bound of the X and Y arrays.
*     Y( LBND : UBND ) = REAL (Given)
*        The Y data values. Any bad values are ignored.
*     X( LBND : UBND ) = REAL (Given)
*        The X positions corresponding to each Y data value.
*     M = REAL (Returned)
*        The gradient.
*     C = REAL (Returned)
*        The intercept.
*     RMS = REAL (Returned)
*        The RMS residual between the Y values and the fit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-OCT-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants

*  Arguments Given:
      INTEGER LBND
      INTEGER UBND
      REAL Y( LBND : UBND )
      REAL X( LBND : UBND )

*  Arguments Returned:
      REAL M
      REAL C
      REAL RMS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION DENOM     ! Denominator
      DOUBLE PRECISION SX        ! Sum of X values.
      DOUBLE PRECISION SXX       ! Sum of X squared values.
      DOUBLE PRECISION SXY       ! Sum of X*Y values.
      DOUBLE PRECISION SY        ! Sum of Y values.
      DOUBLE PRECISION SYY       ! Sum of Y squared values.

      INTEGER I                  ! Loop count.
      INTEGER N                  ! No. of points in sums.

      REAL XVAL                  ! X value
      REAL YVAL                  ! Y value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the running sums to zero.
      SY = 0.0D0
      SX = 0.0D0
      SXY = 0.0D0
      SXX = 0.0D0
      SYY = 0.0D0
      N = 0

*  Loop round finding the sums of all the necessary terms.
      DO I = LBND, UBND
         XVAL = X( I )
         YVAL = Y( I )

         IF( XVAL .NE. VAL__BADR .AND. YVAL .NE. VAL__BADR ) THEN
            SY = SY + DBLE( YVAL )
            SX = SX + DBLE( XVAL )
            SXY = SXY + DBLE( XVAL )*DBLE( YVAL )
            SXX = SXX + DBLE( XVAL )**2
            SYY = SYY + DBLE( YVAL )**2
            N = N + 1
         END IF

      END DO

*  Report an error if there are less than 2 good data values.
      IF( N .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRM_FIT2_ERR1',
     :                 'IRM_FIT2: No good data values suupplied',
     :                 STATUS )
         GO TO 999

      ELSE IF( N .EQ. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRM_FIT2_ERR2',
     :                 'IRM_FIT2: Only 1 good data value found',
     :                 STATUS )
         GO TO 999

      END IF

*  Form the denominator used to calculate the returned values.
      DENOM =  N*SXX - SX*SX

*  Report an error if the denominator is zero.
      IF( DENOM .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRM_FIT2_ERR3',
     :                 'IRM_FIT2: All supplied X values are equal',
     :                 STATUS )

      END IF

*  Form the gradient.
      M = REAL( ( N*SXY - SX*SY )/DENOM )

*  Form the intercept.
      C = REAL( ( SXX*SY - SX*SXY ) /DENOM )

*  Form the RMS residual.
      RMS = REAL( SQRT( MAX( 0.0D0, ( SYY +
     :      ( 2.0*SX*SY*SXY - SY*SY*SXX - SXY*SXY*N )/DENOM)/N ) ) )

*  If an error has occurred, add a context message.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRM_FIT2_ERR4',
     :          'IRM_FIT2: Unable to fit a least squares straight line',
     :                 STATUS )
      END IF

      END
