      SUBROUTINE ARY1_CPYNC( LOC1, NAME, LOC2, STATUS )
*+
*  Name:
*     ARY1_CPYNC

*  Purpose:
*     Copy a named HDS component from one structure to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_CPYNC( LOC1, NAME, LOC2, STATUS )

*  Description:
*     The routine copies a named HDS component (if it exists) from one
*     structure to another, preserving its name in the process. If the
*     component to be copied does not exist, then the routine completes
*     without action, but no error results. An error will be reported,
*     however, if a component exists to be copied but a component of
*     the same name already exists in the output structure.

*  Arguments:
*     LOC1 = CHARACTER * ( * ) (Given)
*        Locator to input HDS structure.
*     NAME = CHARACTER * ( * ) (Given)
*        HDS name of the component to be copied.
*     LOC2 = CHARACTER * ( * ) (Given)
*        Locator to the HDS structure which is to receive the copied
*        component.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise.
*     -  See if the component to be copied exists.
*     -  If it does, then locate it and copy it.
*     -  Annul the locator when done.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-AUG-1989 (RFWS):
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
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Arguments Given:
      CHARACTER * ( * ) LOC1
      CHARACTER * ( * ) NAME
      CHARACTER * ( * ) LOC2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL THERE              ! Whether the component exists
      CHARACTER * ( DAT__SZLOC ) TLOC ! Temporary locator

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      THERE = .TRUE.

*  See if the component to be copied exists.
      CALL DAT_THERE( LOC1, NAME, THERE, STATUS )
      IF ( ( STATUS .EQ. SAI__OK ) .AND. THERE ) THEN

*  If so, then locate it and copy it.
         TLOC = ARY__NOLOC
         CALL DAT_FIND( LOC1, NAME, TLOC, STATUS )
         CALL DAT_COPY( TLOC, LOC2, NAME, STATUS )

*  Annul the locator when done.
         CALL DAT_ANNUL( TLOC, STATUS )
         TLOC = ARY__NOLOC
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_CPYNC', STATUS )

      END
