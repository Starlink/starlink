      SUBROUTINE SUBPAR_FNAME( INNAM, OUTNAM, NAMLEN, STATUS )
*+
*  Name:
*     SUBPAR_FNAME

*  Purpose:
*     To return a fully expanded filename

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_FNAME( INNAM, OUTNAM, NAMLEN, STATUS )

*  Description:
*     This is the VMS version of the subroutine.
*     It does not need to do anything except transfer INNAM to OUTNAM and
*     find its used length.

*
*  Deficiencies:
*
*  Arguments:
*     INNAM = CHARACTER*(*) (Given)
*        The filename to be expanded
*     OUTNAM = CHARACTER*(*) (Returned)
*        The filename expanded
*     NAMLEN = INTEGER (Returned)
*        The used length of OUTNAM
*     STATUS = INTEGER (Given)
*        The global status.

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-AUG-1991 (AJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER*(*) INNAM

*  Arguments Returned:
      CHARACTER*(*) OUTNAM
      INTEGER NAMLEN
      
*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of string
      EXTERNAL CHR_LEN

*  Local Variables:
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      OUTNAM = INNAM
      NAMLEN = CHR_LEN( OUTNAM )

      END
