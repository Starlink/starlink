      SUBROUTINE SUBPAR_ADMUS( ADMUSR, AULEN, STATUS )
*+
*  Name:
*     SUBPAR_ADMUS

*  Purpose:
*     To obtain a string defining the ADAM_USER directory

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_ADMUS( ADMUSR, STATUS )

*  Description:
*     This is the VMS version.
*     The routine just supplies the string 'ADAM_USER:'

*  Arguments:
*     ADMUSR = CHARACTER*(*) (Returned)
*        String containing the definition of the ADAM_USER directory
*     AULEN = INTEGER (Returned)
*        The used length of the string
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JAN-1992 (AJC):
*        Original version.
*     {enter_changes_here}


*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Returned:
      CHARACTER*(*) ADMUSR
      INTEGER AULEN

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the string and length
      ADMUSR = 'ADAM_USER:'
      AULEN = 10

      END
* $Id$
