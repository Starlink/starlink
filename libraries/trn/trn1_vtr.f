      SUBROUTINE TRN1_VTR( LOCTR, STATUS )
*+
*  Name:
*     TRN1_VTR

*  Purpose:
*     Validate a transformation structure.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_VTR( LOCTR, STATUS )

*  Description:
*     The routine checks that the HDS locator supplied is associated
*     with a scalar structure of type TRN_TRANSFORM.  It also checks
*     that the transformation has a software version number which does
*     not exceed that of the present software.  If the transformation
*     structure fails any of these tests, an error is reported and a
*     STATUS value is returned.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     5-FEB-1988 (RFWS):
*        Original version.
*     16-FEB-1988 (RFWS):
*        Improved error handling.
*     13-FEB-1992 (RFWS):
*        Eliminated unused variable.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes

*  Arguments Given:
      CHARACTER * ( * ) LOCTR    ! Locator to transformation structure

*  Status:
      INTEGER STATUS            ! Error status

*  Local Variables:
      CHARACTER * ( DAT__SZTYP ) TYPE ! Object HDS type string
      INTEGER SIZE               ! Object size
      REAL VERSN                 ! Software version number

*.

*  Check status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Obtain the size and type of the object supplied.
      CALL DAT_SIZE( LOCTR, SIZE, STATUS )
      CALL DAT_TYPE( LOCTR, TYPE, STATUS )

*  If there is no error, check the structure is scalar and of type
*  TRN_TRANSFORM.  Report an error if it is not.
      IF( STATUS .EQ. SAI__OK ) THEN
        IF( SIZE .NE. 1 ) THEN
          STATUS = TRN__DIMIN    ! dimensions invalid
          CALL TRN1_ERRL( 'TRN1_VTR', LOCTR, STATUS )

        ELSE IF( TYPE .NE. 'TRN_TRANSFORM' ) THEN
          STATUS = TRN__TYPIN    ! type invalid
          CALL TRN1_ERRL( 'TRN1_VTR', LOCTR, STATUS )

*  If there is no error, obtain the software version number from the
*  transformation.  This will be checked against the current software
*  version.
        ELSE
          CALL TRN1_RDVER( LOCTR, VERSN, STATUS )
        ENDIF
      ENDIF

*  Exit routine.
      END
