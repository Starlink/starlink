      SUBROUTINE NDF_VALID( INDF, VALID, STATUS )
*+
*  Name:
*     NDF_VALID

*  Purpose:
*     Determine whether an NDF identifier is valid.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_VALID( INDF, VALID, STATUS )

*  Description:
*     The routine determines whether an NDF identifier is valid (i.e.
*     associated with an NDF).

*  Arguments:
*     INDF = INTEGER (Given)
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
*     29-SEP-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
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
      INTEGER INDF

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
      CALL NDF1_ID2AC( INDF, IACB )

*  Note whether the attempt succeeded; it did not if a value of zero
*  was returned.
      VALID = IACB .NE. 0

      END
