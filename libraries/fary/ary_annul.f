      SUBROUTINE ARY_ANNUL( IARY, STATUS )
*+
*  Name:
*     ARY_ANNUL

*  Purpose:
*     Annul an array identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_ANNUL( IARY, STATUS )

*  Description:
*     The routine annuls the array identifier supplied so that it is no
*     longer recognised as a valid identifier by the ARY_ routines.
*     Any resources associated with it are released and made available
*     for re-use. If the array is mapped for access, then it is
*     automatically unmapped by this routine.

*  Arguments:
*     IARY = INTEGER (Given and Returned)
*        The array identifier to be annulled. A value of ARY__NOID is
*        returned (as defined in the include file ARY_PAR).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances. In particular, it
*     will fail if the identifier supplied is not initially valid, but
*     this will only be reported if STATUS is set to SAI__OK on entry.
*     -  An error will result if an attempt is made to annul the last
*     remaining identifier associated with an array which is in an
*     undefined state (unless it is a temporary array, in which case it
*     will be deleted at this point).

*  Algorithm:
*     -  Save the error context on entry.
*     -  Import the array identifier.
*     -  Annul the associated ACB entry.
*     -  Reset the array identifier value.
*     -  Restore the error context.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-AUG-1989 (RFWS):
*        Original version.
*     9-OCT-1989 (RFWS):
*        Added STATUS check after call to ARY1_IMPID.
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

*  Arguments Given and Returned:
      INTEGER  IARY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER TSTAT              ! Temporary status variable
      INTEGER IACB               ! Index to array entry in the ACB

*.
       
*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK
       
*  Import the array identifier.
      STATUS = SAI__OK
      CALL ARY1_IMPID( IARY, IACB, STATUS )

*  Annul the associated ACB entry and reset the array identifier value.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL ARY1_ANL( IACB, STATUS )
      END IF
      IARY = ARY__NOID
       
*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT

*  Report context information and call error tracing routine if
*  appropriate.
         ELSE
            CALL ERR_REP( 'ARY_ANNUL_ERR',
     :      'ARY_ANNUL: Error annulling array identifier.', STATUS )
            CALL ARY1_TRACE( 'ARY_ANNUL', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release error stack.
      CALL ERR_RLSE

      END
