      SUBROUTINE ARY_RESET( IARY, STATUS )
*+
*  Name:
*     ARY_RESET

*  Purpose:
*     Reset an array to an undefined state.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_RESET( IARY, STATUS )

*  Description:
*     The routine resets an array so that its values become undefined.
*     Its use is advisable before making format changes to an array if
*     retention of the existing values is not required (e.g. before
*     changing its data type with the ARY_STYPE routine); this will
*     avoid the cost of converting the existing values.

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine may only be used to reset the state of a base
*     array. If an array section is supplied, then it will return
*     without action. No error will result.
*     -  An array cannot be reset while it is mapped for access. This
*     routine will fail if this is the case.

*  Algorithm:
*     -  Import the array identifier.
*     -  Check that WRITE access to the array is permitted.
*     -  If an array section has been provided, then check that it is
*     not mapped for access. Report an error if it is.
*     -  If the array is a base array, then check that no part of it is
*     mapped for access. Report an error if it is.
*     -  Reset the array's state to "undefined".
*     -  Set its bad pixel flag to .TRUE..
*     -  If an error occurred, then report context information.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-AUG-1989 (RFWS):
*        Original version.
*     13-SEP-1989 (RFWS):
*        Corrected error in the ARY1_DRST routine name.
*     13-SEP-1989 (RFWS):
*        Added check that WRITE access to the array is permitted.
*     18-SEP-1989 (RFWS):
*        Modified to set the bad pixel flag to .TRUE. after a reset
*        operation.
*     7-MAR-1990 (RFWS):
*        Added checks that the array is not mapped for access.
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
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.
*        DCB_NREAD( NDF__MXDCB ) = INTEGER (Read)
*           Number of current read accesses to the data object.
*        DCB_NWRIT( NDF__MXDCB ) = INTEGER (Read)
*           Number of current write accesses to the data object.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_CUT( ARY__MXACB ) = LOGICAL (Read)
*           Whether the array is a "cut".
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to the data object entry in the DCB.

*  Arguments Given:
      INTEGER IARY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to array entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the array identifier and check that WRITE access to the array
*  is permitted.
      CALL ARY1_IMPID( IARY, IACB, STATUS )
      CALL ARY1_CHACC( IACB, 'WRITE', STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If an array section has been specified, then there is nothing to do.
*  However, a check is still performed to ensure that the array is not
*  mapped through the identifier supplied. Report an error if it is.
         IF ( ACB_CUT( IACB ) ) THEN
            IF ( ACB_IMCB( IACB ) .NE. 0 ) THEN
               STATUS = ARY__ISMAP
               IDCB = ACB_IDCB( IACB )
               CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
               CALL ERR_REP( 'ARY_RESET_MAP',
     :         'The array ^ARRAY is mapped for access through the ' //
     :         'specified identifier (possible programming error).',
     :         STATUS )
            END IF

*  If the array is a base array, then check that there is no mapped
*  access to any part of it.
         ELSE
            IDCB = ACB_IDCB( IACB )
            IF ( ( DCB_NREAD( IDCB ) .NE. 0 ) .OR.
     :           ( DCB_NWRIT( IDCB ) .NE. 0 ) ) THEN

*  Report an error if there is.
               STATUS = ARY__ISMAP
               IDCB = ACB_IDCB( IACB )
               CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
               CALL ERR_REP( 'ARY_RESET_BMAP',
     :         'The base array ^ARRAY is mapped for access, ' //
     :         'perhaps through another identifier (possible ' //
     :         'programming error).', STATUS )

*  Reset the array's state to "undefined".
            ELSE
               CALL ARY1_DRST( IDCB, STATUS )

*  Set its bad pixel flag to .TRUE..
               CALL ARY1_SBD( .TRUE., IACB, STATUS )
            END IF
         END IF
      END IF
       
*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_RESET_ERR',
     :   'ARY_RESET: Error resetting an array to an undefined state.',
     :   STATUS )
         CALL ARY1_TRACE( 'ARY_RESET', STATUS )
      END IF

      END
