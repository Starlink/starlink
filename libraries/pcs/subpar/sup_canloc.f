      SUBROUTINE SUBPAR_CANLOC ( NAMECODE, STATUS )
*+
*  Name:
*     SUBPAR_CANLOC

*  Purpose:
*     Marks the HDS locators of a parameter as cancelled.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_CANLOC ( NAMECODE, STATUS )

*  Description:
*     The locator stoarage for the indicated parameter is marked as
*     invalid.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        pointer to the parameter
*     STATUS=INTEGER

*  Algorithm:
*     NAMECODE indexes into the arrays for holding the locator values.

*  Authors:
*     BDK: B D Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     20-SEP-1984 (BDK):
*        Original
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_ERR'


*  Arguments Given:
      INTEGER NAMECODE          ! pointer to the parameter


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Check that NAMECODE is in range, and set the validity flag to false.
*
      IF ( ( NAMECODE .LE. PARPTR ) .AND. ( NAMECODE .GT. 0 ) ) THEN

         PARVALID(NAMECODE) = .FALSE.

      ELSE

         STATUS = SUBPAR__NOPAR

      ENDIF

      END
