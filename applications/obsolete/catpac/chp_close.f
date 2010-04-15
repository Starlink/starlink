      SUBROUTINE
     : CHP_CLOSE( STATUS )
*+
*  Name:
*     CHP_CLOSE

*  Purpose:
*     CLOSE the CHP system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_CLOSE( STATUS )
*
*  Description:
*     Closes the CHP system and performs house keeping task to release
*     resources. CHP_OPEN should be the first CHP call in your
*     application and CHP_CLOSE the last.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Anticipated Errors:
*     None

*  Authors:
*     ARW: Alan R Wood (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1993 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHP_PAR'   ! Standard CHP constants

*  Global Variables:
      INCLUDE 'CHP_CMN'   ! CHP comon area

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CC ! Catalogue count
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Close and download all the loaded cataloge.
*
      do cc = 1, CHP__MXASS
        if (.not.(opcatnames(cc) .eq. '9999') .and.
     :      .not.(opcatsys(cc))) then
        call chp_dloadcat(cc,status)
        endif
      enddo
      call chi_close(status)
*
*
      END
