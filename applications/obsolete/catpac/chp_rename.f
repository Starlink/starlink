      SUBROUTINE
     : CHP_RENAME( INPUT, NEWNAME, STATUS )
*+
*  Name:
*     CHP_RENAME

*  Purpose:
*     RENAME a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_RENAME( INPUT, NEWNAME, STATUS )

*  Description:
*     Rename a catalogue.

*  Arguments:
*     INPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the catalogue.
*     NEWNAME = CHARACTER * ( CHP__SZNAME ) (Given)
*        New name of the catalogue.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Notes:
*     The input catalogue is RESET to its first entry on exit from this routine.
*
*     If the catalogue can not be renamed an insufficient privilege to update
*     error will be reported.
*

*  Anticipated Errors:
*     CHP__CATNOTFND
*     CHP__INSPRIVUP

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
      INCLUDE 'SAE_PAR'    ! Standard SAE constants
      INCLUDE 'CHP_PAR'    ! Standard CHP constants
      INCLUDE 'CHP_ERR'    ! Standard CHP errors
      INCLUDE 'FIO_ERR'    ! Standard FIO errors

*  Global Variables:
      INCLUDE 'CHP_CMN'    ! Standard CHP variables

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      CHARACTER * ( * ) NEWNAME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CD

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
        call chp_getcd(input, 'READ', cd, status)
        CPname(cd) = newname
*
        call chi_rename(input, newname, status)
*
        call chp_deladdf(input, status)
*
      END
