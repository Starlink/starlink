      SUBROUTINE ARY_MAP( IARY, TYPE, MMOD, PNTR, EL, STATUS )
*+
*  Name:
*     ARY_MAP

*  Purpose:
*     Obtain mapped access to an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_MAP( IARY, TYPE, MMOD, PNTR, EL, STATUS )

*  Description:
*     The routine obtains mapped access an array, returning a pointer
*     to the mapped values and a count of the number of elements
*     mapped.

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     TYPE = CHARACTER * ( * ) (Given)
*        The numerical data type required for access (e.g. '_REAL').
*     MMOD = CHARACTER * ( * ) (Given)
*        The mapping mode for access to the array: 'READ', 'UPDATE' or
*        'WRITE', with an optional initialisation mode '/BAD' or
*        '/ZERO' appended.
*     PNTR = INTEGER (Returned)
*        Pointer to the mapped values.
*     EL = INTEGER (Returned)
*        Number of elements mapped.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Import the array identifier.
*     -  Map the array data.
*     -  Calculate the number of mapped data elements.
*     -  If an error occurred, then report context information.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-JUL-1989 (RFWS):
*        Original version.
*     7-SEP-1989 (RFWS):
*        Simplified the description of the routine's purpose and the
*        context error message.
*     11-SEP-1989 (RFWS):
*        Changed the order of the arguments.
*     3-OCT-1989 (RFWS):
*        Changed the argument order again.
*     24-JAN-1990 (RFWS):
*        Renamed from ARY_MAPV to ARY_MAP.
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
*        ACB_LBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Lower bounds of array.
*        ACB_NDIM( ARY__MXACB ) = INTEGER (Read)
*           Number of array dimensions.
*        ACB_UBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Upper bounds of array.

*  Arguments Given:
      INTEGER IARY
      CHARACTER * ( * ) TYPE
      CHARACTER * ( * ) MMOD

*  Arguments Returned:
      INTEGER PNTR
      INTEGER EL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DUMMY              ! Dummy variable
      INTEGER IACB               ! Index to array entry in ACB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )

*  Map the array data.
      CALL ARY1_MAPS( IACB, TYPE, .FALSE., MMOD, PNTR, DUMMY, STATUS )

*  Calculate the number of mapped data elements.
      CALL ARY1_NEL( ACB_NDIM( IACB ), ACB_LBND( 1, IACB ),
     :               ACB_UBND( 1, IACB ), EL, STATUS )

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_MAP_ERR',
     :   'ARY_MAP: Error obtaining mapped access to an array.', STATUS )
         CALL ARY1_TRACE( 'ARY_MAP', STATUS )
      END IF

      END
