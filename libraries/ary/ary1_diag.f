      SUBROUTINE ARY1_DIAG( IARY, BLOCKS, STATUS )
*+
*  Name:
*     ARY1_DIAG

*  Purpose:
*     Display internal diagnosic information about an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DIAG( IARY, BLOCKS, STATUS )

*  Description:
*     The routine displays diagnostic information about the internal
*     common block entries associated with an erray through the ADAM
*     parameter system.

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     BLOCKS = CHARACTER * ( * ) (Given)
*        Character string which specifies which types of internal
*        information are to be displayed. If this string contains a
*        'D', then Data Control Block information is displayed. If it
*        contains an 'A', then Access Control Block information is
*        displayed. If it contains an 'M', then Mapping Control Block
*        information is displayed. Any combination of these three
*        characters may be given (case-insensitive).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Import the array identifier.
*     -  Call the appropriate routine to display each type of internal
*     information, according to the value of the BLOCKS argument.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-SEP-1989 (RFWS):
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
      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_IMCB( ARY__MXACB ) = INTEGER (Read)
*           Index to mapping entry in the MCB.

*  Arguments Given:
      INTEGER IARY
      CHARACTER * ( * ) BLOCKS

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

*  If required, display details of the Data Control Block.
         IF ( ( INDEX( BLOCKS, 'D' ) .NE. 0 ) .OR.
     :        ( INDEX( BLOCKS, 'd' ) .NE. 0 ) ) THEN
            CALL ARY1_DDIAG( ACB_IDCB( IACB ) )
         END IF

*  If required, display details of the Access Control Block.
         IF ( ( INDEX( BLOCKS, 'A' ) .NE. 0 ) .OR.
     :        ( INDEX( BLOCKS, 'a' ) .NE. 0 ) ) THEN
            CALL ARY1_ADIAG( IACB )
         END IF

*  If required, display details of the Mapping Control Block.
         IF ( ( INDEX( BLOCKS, 'M' ) .NE. 0 ) .OR.
     :        ( INDEX( BLOCKS, 'm' ) .NE. 0 ) ) THEN
            CALL ARY1_MDIAG( ACB_IMCB( IACB ) )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DIAG', STATUS )

      END
