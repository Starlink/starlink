      SUBROUTINE ARY1_UMP( IACB, STATUS )
*+
*  Name:
*     ARY1_UMP

*  Purpose:
*     Unmap an array with an entry in the ACB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_UMP( IACB, STATUS )

*  Description:
*     The routine performs an unmapping operation on an ACB entry
*     through which an array has previously been mapped for access.  An
*     error will be reported if the array has not been mapped through
*     the specified ACB entry.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the array entry in the ACB on which the unmapping
*        operation is to be performed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Algorithm:
*     -  Save the error context on entry.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Handle each form of array in turn, calling the appropriate
*     routine to unmap it.
*     -  If the form entry in the DCB is not recognised, then report an
*     error.
*     -  Restore the error context.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-JUL-1989 (RFWS):
*        Original version.
*     18-SEP-1989 (RFWS):
*        Minor improvement to error message.
*     13-FEB-1990 (RFWS):
*        Installed support for primitive arrays.
*     23-FEB-1990 (RFWS):
*        Removed check on whether the array is actually mapped; this is
*        now done at a lower level.
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
*        DCB_FRM( ARY__MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Read)
*           Form of the array.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to the data object entry in the DCB.

*  Arguments Given:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER TSTAT              ! Temporary status variable
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK
       
*  Obtain an index to the data object entry in the DCB.
      STATUS = SAI__OK
      IDCB = ACB_IDCB( IACB )

*  Handle each form of array in turn, calling the appropriate routine to
*  unmap it.

*  Primitive and simple arrays.
*  ===========================
      IF ( ( DCB_FRM( IDCB ) .EQ. 'PRIMITIVE' ) .OR.
     :     ( DCB_FRM( IDCB ) .EQ. 'SIMPLE' ) ) THEN
         CALL ARY1_UMPS( IACB, STATUS )

*  If the form entry in the DCB was not recognised, then report an
*  error.
      ELSE
         STATUS = ARY__FATIN
         CALL MSG_SETC( 'BADFORM', DCB_FRM( IDCB ) )
         CALL ERR_REP( 'ARY1_UMP_FORM',
     :   'Unsupported array form ''^BADFORM'' found in Data ' //
     :   'Control Block (internal programming error).', STATUS )
      END IF
       
*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT

*  Call error tracing routine if appropriate.
         ELSE
            CALL ARY1_TRACE( 'ARY1_UMP', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release error stack.
      CALL ERR_RLSE

      END
