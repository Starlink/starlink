      SUBROUTINE DAT_DELET ( PARAM, STATUS )
*+
*  Name:
*     DAT_DELET

*  Purpose:
*     Delete an object associated with a parameter

*  Language:
*     Fortran 77

*  Invocation:
*     CALL DAT_DELET ( PARAM, STATUS )

*  Description:
*     Get an object name and delete the object.

*  Arguments:
*     PARAM=CHARACTER*(*) (given)
*        Name of program parameter
*     STATUS=INTEGER (given and returned)
*        Global status

*  Algorithm:
*     The character string associated with the given parameter is
*     obtained, and interpreted as a filename (an HDS container
*     file), followed by the full name of the structure component
*     required. The component is deleted if possible. The data structure
*     down to the level immediately above the required new component
*     must exist already.
*     Cancel the parameter.

*  Authors:
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (Starlink)
*     {enter_new_authors_here}

*  History:
*     20-MAR-1985 (BDK)
*        Original 
*     04-MAY-1987 (BDK)
*        Change to call SUBPAR_DELET 
*     16-JUN-1998 (AJC)
*        Re-format prologue
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) PARAM          ! parameter name

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER NAMECODE                     ! pointer to program parameter

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL SUBPAR_FINDPAR ( PARAM, NAMECODE, STATUS )
      CALL SUBPAR_DELET ( NAMECODE, STATUS )

      END
