************************************************************************

      SUBROUTINE AGI_1FPST ( WKSLOC, PSTLOC, FOUND, STATUS )

*+
*  Name:
*     AGI_1FPST

*  Purpose:
*     Find the picture structure.

*  Language:
*     VAX Fortran

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL AGI_1FPST( WKSLOC, PSTLOC, FOUND, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Find the picture structure in the database.

*  Algorithm:
*     Initialise the returned variable.
*     Check status on entry.
*     If picture structure is in workstation then
*        Get the locator to the picture structure.
*     Endif

*  Authors:
*     Nick Eaton  ( DUVAD::NE )
*     {enter_new_authors_here}

*  History:
*     July 1988
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
      INCLUDE 'agi_nam'


*  Arguments Given:
*     Locator to workstation
      CHARACTER * ( DAT__SZLOC ) WKSLOC


*  Arguments Returned:
*     Locator to picture structure. Undefined if .NOT. FOUND
      CHARACTER * ( DAT__SZLOC ) PSTLOC

*     Flag to indicate if picture structure has been found
      LOGICAL FOUND


*  Status:
      INTEGER STATUS

*.


*   Initialise the returned variable
      FOUND = .FALSE.

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Check to see if picture structure is present
         CALL DAT_THERE( WKSLOC, AGI__PCNAM, FOUND, STATUS )

*   If so get the locator to the structure
         PSTLOC = ' '
         IF ( FOUND ) THEN
            CALL DAT_FIND( WKSLOC, AGI__PCNAM, PSTLOC, STATUS )
         ENDIF

      ENDIF

*      print*, '+++++ AGI_1FPST +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

