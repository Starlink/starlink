      SUBROUTINE ARY1_NEL( NDIM, LBND, UBND, EL, STATUS )
*+
*  Name:
*     ARY1_NEL

*  Purpose:
*     Calculate the number of elements in an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_NEL( NDIM, LBND, UBND, EL, STATUS )

*  Description:
*     The routine calculates the number of elements in a
*     multi-dimensional array from the lower and upper bounds
*     information. The bounds information is not checked for validity.

*  Arguments:
*     NDIM = INTEGER (Given)
*        Number of array dimensions.
*     LBND( NDIM ) = INTEGER (Given)
*        Lower array bounds.
*     UBND( NDIM ) = INTEGER (Given)
*        Upper array bounds.
*     EL = INTEGER (Returned)
*        Number of elements in the array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Multiply together the array extents in each dimension.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-JUN-1989  (RFWS):
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

*  Arguments Given:
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )

*  Arguments Returned:
      INTEGER EL

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER I                  ! Loop counter for dimensions

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      EL = 1

*  Multiply together the array extents in each dimension.
      DO 1 I = 1, NDIM
         EL = EL * ( UBND( I ) - LBND( I ) + 1 )
1     CONTINUE
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_NEL', STATUS )

      END
