      PROGRAM ERR_TEST
*+
*  Name:
*     ERR_TEST

*  Purpose:
*     Test the installation of the ERR/MSG libraries.

*  Language:
*     Starlink Fortran 77

*  Description:
*     Test calls to the ERR/MSG libraries.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-SEP-1993 (PCTR):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Initialise status.
      STATUS = SAI__OK

*  Call MSG_OUT.
      CALL MSG_BELL( STATUS )
      CALL MSG_OUT( ' ', 'MSG is installed and working.', STATUS )

*  Call ERR_REP and ERR_FLUSH.
      STATUS = SAI__ERROR
      CALL ERR_MARK
      CALL ERR_REP( ' ', 'ERR is installed and working.', STATUS )
      CALL ERR_FLBEL( STATUS )
      CALL ERR_RLSE

      END
