      SUBROUTINE ARY1_VBND( NDIM, LBND, UBND, STATUS )
*+
*  Name:
*     ARY1_VBND

*  Purpose:
*     Check array bounds for validity.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_VBND( NDIM, LBND, UBND, STATUS )

*  Description:
*     The routine checks that the number of array dimensions and the
*     lower and upper array bounds supplied are valid and reports an
*     error if they are not. Otherwise, the routine returns without
*     action.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of array dimensions.
*     LBND( NDIM ) = INTEGER (Given)
*        Array of lower dimension bounds.
*     UBND( NDIM ) = INTEGER (Given)
*        Array of upper dimension bounds.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Check the number of dimensions is valid and does not exceed the
*     system-imposed upper limit ARY__MXDIM.
*     -  Check that the lower bound of each dimension does not exceed
*     the corresponding upper bound.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-JUN-1989  (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Arguments Given:
      INTEGER NDIM
      INTEGER LBND( * )
      INTEGER UBND( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER I                  ! Loop counter for dimensions

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the number of array dimensions is valid.
      IF ( ( NDIM .LE. 0 ) .OR. ( NDIM .GT. ARY__MXDIM ) ) THEN
         STATUS = ARY__NDMIN
         CALL MSG_SETI( 'NDIM', NDIM )
         CALL MSG_SETI( 'MXDIM', ARY__MXDIM )
         CALL ERR_REP( 'ARY1_VBND_NDIM',
     :   'Number of array dimensions (^NDIM) is invalid; this ' //
     :   'number should lie between 1 and ^MXDIM inclusive ' //
     :   '(possible programming error).', STATUS )

*  Check the lower and upper bounds of each dimension for validity.
      ELSE
         DO 1 I = 1, NDIM
            IF ( LBND( I ) .GT. UBND( I ) ) THEN
               STATUS = ARY__DIMIN
               CALL MSG_SETI( 'LBND', LBND( I ) )
               CALL MSG_SETI( 'DIM', I )
               CALL MSG_SETI( 'UBND', UBND( I ) )
               CALL ERR_REP( 'ARY1_VBND_DIM',
     :         'Lower bound (^LBND) of array dimension ^DIM ' //
     :         'exceeds the corresponding upper bound (^UBND) ' //
     :         '(possible programming error).', STATUS )
               GO TO 2
            END IF
1        CONTINUE
2        CONTINUE
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_VBND', STATUS )

      END
