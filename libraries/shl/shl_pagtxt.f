      SUBROUTINE SHL_PAGTXT( TEXT, STATUS )
*+
*  Name:
*     SHL_PAGTXT

*  Purpose:
*     Prints page formatted text to standard output

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SHL_PAGTXT( TEXT, STATUS)

*  Arguments:
*     TEXT = CHARACTER*(*) (Given)
*        Text to be displayed
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2004 Oct 4 (TIMJ):
*        Original version. Public wrapper to PTHLPO

*  Notes:
*     The user should reset the internal state before displaying
*     the first line of text by calling SHL_PAGRST()

*  Bugs:
*     {note_new_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions

*.

*  Arguments Given:
      CHARACTER * ( * ) TEXT

*  Status:
      INTEGER STATUS             ! Inherited global status

*  External Variables:
      INTEGER SHL_PTHLPO
      EXTERNAL SHL_PTHLPO

*  Local Variables:
      INTEGER ISTAT              ! Status from PTHLPO

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      ISTAT = SHL_PTHLPO( TEXT )

      IF (ISTAT .NE. 1) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SHL_PAGTXT_ERR','An error occurred writing '/
     :                 /'paged text to the output device', STATUS )
      END IF

      END
