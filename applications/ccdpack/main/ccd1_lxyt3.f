      SUBROUTINE CCD1_LXYT3( X, Y, NIN, TR, STATUS )
*+
*  Name:
*     CCD1_LXYT3

*  Purpose:
*     Transforms the X and Y positions using a linear transformation.
*     This version returns the results in the same array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_LXYT2( XIN, YIN, NIN, TR, STATUS )

*  Description:
*     The routine transforms the X and Y positions using a linear
*     transformation whose coefficients are specified by the TR array.
*     The transformed positions are returned in the same arrays as the
*     input vaues.

*  Arguments:
*     X( NIN ) = DOUBLE PRECISION (Given and Returned)
*        Input X positions.
*     Y( NIN ) = DOUBLE PRECISION (Given and Returned)
*        Input Y positions.
*     NIN = INTEGER (Given)
*        The number of input values.
*     TR( 6 ) = DOUBLE PRECISION (Given)
*        The transformation coefficients. The transformation is:
*
*        XDASH = TR( 1 ) + TR( 2 ) * X + TR( 3 ) * Y
*        YDASH = TR( 4 ) + TR( 5 ) * X + TR( 6 ) * Y
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The linear transform used by this routines is:
*        XDASH = TR( 1 ) + TR( 2 ) * X + TR( 3 ) * Y
*        YDASH = TR( 4 ) + TR( 5 ) * X + TR( 6 ) * Y

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-JUL-1992 (PDRAPER):
*        Original version.
*     27-JUL-1992 (PDRAPER):
*        Changed to use separated X and Y positions.
*     5-OCT-1992 (PDRAPER):
*        Changed to return the results in the input arrays.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NIN
      DOUBLE PRECISION TR( 6 )

*  Arguments Given and Returned:
      DOUBLE PRECISION X( NIN )
      DOUBLE PRECISION Y( NIN )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION XX
      DOUBLE PRECISION YY
      INTEGER I

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Transform the positions and copy the extra values into ARROUT.
      DO 1 I = 1, NIN
         XX = X( I )
         YY = Y( I )
         X( I ) = TR( 1 ) + TR( 2 ) * XX + TR( 3 ) * YY
         Y( I ) = TR( 4 ) + TR( 5 ) * XX + TR( 6 ) * YY
 1    CONTINUE

      END
* $Id$
