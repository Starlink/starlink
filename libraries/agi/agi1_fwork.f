************************************************************************

      SUBROUTINE AGI_1FWORK ( WKNAME, WKSLOC, FOUND, STATUS )

*+
*  Name:
*     AGI_1FWORK

*  Purpose:
*     Find a locator to a workstation.

*  Language:
*     VAX Fortran

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL AGI_1FWORK( WKNAME, WKSLOC, FOUND, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Find the workstation specified by WKNAME.

*  Algorithm:
*     Initialise the returned variable.
*     Check status on entry.
*     If given workstation is in database then
*        Get the locator to the workstation.
*     Endif

*  Authors:
*     Nick Eaton  ( DUVAD::NE )
*     {enter_new_authors_here}

*  History:
*     July 1988
*     July 1989  Read database locator from common block
*     September 1990  Initialise FOUND
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'


*  Arguments Given:
*     Name of workstation to find
      CHARACTER * ( * ) WKNAME


*  Arguments Returned:
*     Locator to workstation. Undefined if .NOT. FOUND
      CHARACTER * ( DAT__SZLOC ) WKSLOC

*     Flag to indicate if workstation has been found
      LOGICAL FOUND


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'agi_locs'

*.


*   Initialise the returned variable
      FOUND = .FALSE.

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Check if workstation is there
         CALL DAT_THERE( DABLOC, WKNAME, FOUND, STATUS )

*   Get locator to workstation
         WKSLOC = ' '
         IF ( FOUND ) THEN
            CALL DAT_FIND( DABLOC, WKNAME, WKSLOC, STATUS )
         ENDIF

      ENDIF

*      print*, '+++++ AGI_1FWORK +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

