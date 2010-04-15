      SUBROUTINE FIND30( ILEVEL, PMMENU, MMENU, STATUS )
*+
*  Name:
*     FIND30

*  Purpose:
*     Displays the Main Menu, and allows user to select option

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND30( ILEVEL, PMMENU, MMENU, STATUS )

*  Description:
*     Displays the Main Menu, and allows user to select option

*  Arguments:
*     ILEVEL = INTEGER (Given)
*        Value of ILEVEL parameter, gives program interaction level
*     PMMENU = CHARACTER * ( * ) (Given)
*        Parameter MAINCHOICE for users main menu choice
*     MMENU = CHARACTER * ( * ) (Given and Returned)
*        Value of MAINCHOICE parameter, gives users choice from main
*        menu
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     MSG:
*        MSG_OUT
*     PAR:
*        PAR_CANCL, PAR_CHOIC

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1991 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 General constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors
      INCLUDE 'MSG_PAR'          ! Message reporting constants
      INCLUDE 'MSG_ERR'          ! Message reporting errors
      INCLUDE 'ERR_PAR'          ! Error reporting constants
      INCLUDE 'ERR_ERR'          ! Error reporting errors
      INCLUDE 'PAR_ERR'          ! Parameter errors

*  Arguments Given:
      INTEGER ILEVEL
      CHARACTER * ( * ) PMMENU

*  Arguments Given and Returned:
      CHARACTER * ( * ) MMENU

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If ILEVEL is greater than 2 display the main menu
      IF ( ILEVEL .GT. 2 ) THEN
         CALL MSG_OUT( ' ', 'MAIN MENU', STATUS )
         CALL MSG_OUT( ' ', '  I = Input source positions', STATUS )
         CALL MSG_OUT( ' ', '  S = Find survey data', STATUS )
         CALL MSG_OUT( ' ', '  Q = Exit FINDCRDD', STATUS )
      END IF

*  Ask user for choice from main menu
      CALL PAR_CHOIC( PMMENU, 'I', 'I,S,Q', .TRUE., MMENU, STATUS )

*  Cancel the parameter so that a new value is obtained next time
*  through this section
      CALL PAR_CANCL( PMMENU, STATUS )

      END
