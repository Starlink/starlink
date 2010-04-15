      SUBROUTINE
     : CHP_OPEN( STATUS )
*+
*  Name:
*     CHP_OPEN

*  Purpose:
*     OPEN the CHP system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_OPEN( STATUS )
*
*  Description:
*     Opens the CHP system and performs house keeping tasks.
*     CHP_OPEN should be the first CHP call in your application
*     and CHP_CLOSE the last.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Anticipated Errors:
*     None

*  Authors:
*     ARW: Alan R Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-NOV-1991 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
*      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHP_PAR'   ! CHP constants

*  Global Variables:
      INCLUDE 'CHP_CMN'   ! CHP comon area

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER COUNT
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
      call chp_init(status)
*
      do count = 1, CHP__MXASS
        opcatnames(count) = '9999'
      enddo
*
      call chi_open(status)
*
      END
