      SUBROUTINE ARY_STATE( IARY, STATE, STATUS )
*+
*  Name:
*     ARY_STATE

*  Purpose:
*     Determine the state of an array (defined or undefined).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_STATE( IARY, STATE, STATUS )

*  Description:
*     The routine returns a logical value indicating whether an array's
*     pixel values are currently defined.

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     STATE = LOGICAL (Returned)
*        Whether the array's pixel values are defined.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Import the array identifier.
*     -  Ensure that state information is available in the DCB.
*     -  Obtain the array's state.
*     -  If an error occurred, then report context information.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-SEP-1989 (RFWS):
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
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_STA( ARY__MXDCB ) = LOGICAL (Read)
*           Data object state.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IARY

*  Arguments Returned:
      LOGICAL STATE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to array entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Inport the array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Ensure that state information is available in the DCB.
         IDCB = ACB_IDCB( IACB )
         CALL ARY1_DSTA( IDCB, STATUS )

*  Obtain the array's state.
         IF ( STATUS .EQ. SAI__OK ) THEN
            STATE = DCB_STA( IDCB )
         END IF
      END IF
       
*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_STATE_ERR',
     :   'ARY_STATE: Error obtaining array state information.',
     :   STATUS )
         CALL ARY1_TRACE( 'ARY_STATE', STATUS )
      END IF

      END
