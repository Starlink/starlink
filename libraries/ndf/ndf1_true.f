      SUBROUTINE NDF1_TRUE( EL, LARRAY, STATUS ) 
*+
*  Name:
*     NDF1_TRUE

*  Purpose:
*     Set all elements of a vectorised logical array to .TRUE..

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_TRUE( EL, LARRAY, STATUS )

*  Description:
*     The routine sets all elements of the vectorised logical array
*     supplied to the value .TRUE..

*  Arguments:
*     EL = INTEGER (Given)
*        Number of array elements to be set.
*     LARRAY( EL ) = LOGICAL (Returned)
*        Logical array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Loop through the array, setting all elements to .TRUE..

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-FEB-1990 (RFWS):
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
      INTEGER EL

*  Arguments Returned:
      LOGICAL LARRAY( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for array elements

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop through the array, setting all elements to .TRUE..
      DO 1 I = 1, EL
         LARRAY( I ) = .TRUE.
1     CONTINUE
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_TRUE', STATUS )

      END
