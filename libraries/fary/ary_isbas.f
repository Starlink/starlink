      SUBROUTINE ARY_ISBAS( IARY, BASE, STATUS )
*+
*  Name:
*     ARY_ISBAS

*  Purpose:
*     Enquire if an array is a base array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_ISBAS( IARY, BASE, STATUS )

*  Description:
*     The routine returns a logical value indicating whether the array
*     whose identifier is supplied is a base array (as opposed to an
*     array section).

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     BASE = LOGICAL (Returned)
*        Whether the array is a base array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Import the array identifier.
*     -  Determine whether the array is a base array from its Access
*     Control Block entry.
*     -  If an error occurred, then report context information.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-JUL-1989 (RFWS):
*        Original version.
*     13-SEP-1989 (RFWS):
*        Changed the name of the routine.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_CUT( ARY__MXACB ) = LOGICAL (Read)
*           Whether the array is a cut.

*  Arguments Given:
      INTEGER IARY

*  Arguments Returned:
      LOGICAL BASE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to array entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  The array is a base array if it is not a cut.
         BASE = .NOT. ACB_CUT( IACB )
      END IF
       
*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_ISBAS_ERR',
     :   'ARY_ISBAS: Error enquiring whether an array is a base array.',
     :   STATUS )
         CALL ARY1_TRACE( 'ARY_ISBAS', STATUS )
      END IF

      END
