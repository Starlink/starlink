      SUBROUTINE GET_DTNS( DTNS, STATUS )
*+
*  Name:
*     GET_DTNS

*  Purpose:
*     Gets and processes a DTNS string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GET_DTNS( DTNS, STATUS )

*  Description:
*     The routine gets string from the parameter system using the ADAM
*     parameter DTNS. This string is then processed to see if the
*     characters 'D' 'T' 'N' or 'S' are present. If these characters are
*     present then they are entered into the output string in the order
*     D-T-N-S, characters which are not present are replaced with an X.
*     DTNS is the code for the components of a logfile record which the
*     user requires. The characters are returned in upper case.

*  Arguments:
*     DTNS = CHARACTER * ( 4 ) (Returned)
*        String containing the characters 'DTNS' in this order, missing
*        characters are replaced with 'X'
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     UNKNOWN:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     - (UNKNOWN):
*        Original version - part of ADAM listlog utility.
*     8-JAN-1992 (PDRAPER):
*        Changed to accept and process the DTNS elements in any order.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Returned:
      CHARACTER * ( 4 ) DTNS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 20 ) INDTNS  ! Input string
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the DTNS value from the user.
      CALL PAR_GET0C('DTNS',INDTNS,STATUS)

*  Convert it to upper case and strip leading blanks.
      CALL CHR_UCASE(INDTNS)

*  Look for the appropriate elements.
      DTNS = 'XXXX'
      IF ( INDEX( INDTNS, 'D' ) .NE. 0 ) DTNS( 1:1 ) = 'D'
      IF ( INDEX( INDTNS, 'T' ) .NE. 0 ) DTNS( 2:2 ) = 'T'
      IF ( INDEX( INDTNS, 'N' ) .NE. 0 ) DTNS( 3:3 ) = 'N'
      IF ( INDEX( INDTNS, 'S' ) .NE. 0 ) DTNS( 4:4 ) = 'S'
      END
* $Id$
