      SUBROUTINE ARY_VALID( IARY, VALID, STATUS )
*+
*  Name:
*     ARY_VALID

*  Purpose:
*     Determine whether an array identifier is valid.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_VALID( IARY, VALID, STATUS )

*  Description:
*     Determine whether an array identifier is valid (i.e. associated
*     with an array).

*  Arguments:
*     IARY = INTEGER (Given)
*        Identifier to be tested.
*     VALID = LOGICAL (Returned)
*        Whether the identifier is valid.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Try to convert the identifier into the associated ACB index.
*     -  Note whether the attempt succeeded or not.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-JUL-1989 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Arguments Given:
      INTEGER IARY

*  Arguments Returned:
      LOGICAL VALID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to associated ACB entry

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Try to convert the identifier into an ACB index.
      CALL ARY1_ID2AC( IARY, IACB )

*  Note whether the attempt succeeded; it did not if a value of zero
*  was returned.
      VALID = IACB .NE. 0

      END
