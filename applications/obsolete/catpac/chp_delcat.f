      SUBROUTINE
     : CHP_DELCAT( INPUT, STATUS )
*+
*  Name:
*     CHP_DELCAT

*  Purpose:
*     DELete a CATalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_DELCAT( INPUT, STATUS )
*
*  Description:
*     Delete a catalogue from the system.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue to be deleted.
*     STATUS = INTEGER (Given and Returned)
*        Global status.
*
*  Notes:
*     If the catalogue can not be deleted an insufficient privilege to delete
*     error will be reported.
*
*  Anticipated Errors:
*     CHP__CATNOTFND
*     CHP__INSPRIVDEL

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

*  Global Variables:

*  Arguments Given:
      CHARACTER * ( * ) INPUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Delete the additional information file.
*
      call chp_deladdf(input, status)
*
*    Delete the catalogue
*
      call chi_delcat(input, status)
*
      END
