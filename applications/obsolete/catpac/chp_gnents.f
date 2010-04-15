      SUBROUTINE
     : CHP_GNENTS( INPUT, NUMENTS, STATUS )
*+
*  Name:
*     CHP_GNENTS

*  Purpose:
*     Get the Number of ENTries in a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_GNENTS( INPUT, NUMENTS, STATUS )
*
*  Description:
*     Get the number of entries in a catalogue.

*  Arguments:
*     INPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the catalogue.
*     NUMENTS = INTEGER (Returned)
*        Number of entries in the catalogue.
*     STATUS = INTEGER
*        Global status.

*  Anticipated Errors:
*     CHP__CATNOTFND

*  Authors:
*     ARW: Alan R Wood (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     1-JUL-1993 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHI_PAR'   ! Standard CHI constants
      INCLUDE 'CHI_ERR'   ! Standard CHI errors

*  Arguments Given:
      CHARACTER * ( CHI__SZNAME ) INPUT

*  Arguments Returned:
      INTEGER NUMENTS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
         call chi_gnents(input, numents, status)
*
      END
