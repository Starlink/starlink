      SUBROUTINE KPG1_NDFNM( INDF, NAME, NMLEN, STATUS )
*+
*  Name:
*     KPG1_NDFNM

*  Purpose:
*     Return the name of an NDF without a directory path (Unix only).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_NDFNM( INDF, NAME, NMLEN, STATUS )

*  Description:
*     Gets the full path to the supplied NDF, then removes any directory 
*     path from the start, and return the resulting string.
*
*     Note, Unix file names are assumed.

*  Arguments:
*     INDF = INTEGER (Given)
*        The NDF.
*     NAME = CHARACTER * ( * ) (Returned)
*        The NDF name without directory path.
*     NMLEN = INTEGER (Returned)
*        The used length of NAME.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-AUG-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      CHARACTER NAME*(*)
      INTEGER NMLEN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER PATH*255         ! Full path for NDF
      INTEGER PLEN               ! Used length of PATH
      INTEGER DIREND             ! Index of final "/" character
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the full path to the NDF.
      CALL NDF_MSG( 'NDF', INDF )
      CALL MSG_LOAD( ' ', '^NDF', PATH, PLEN, STATUS )

*  Find the last "/" marking the end of the directory path.
      CALL NDG1_LASTO( PATH( : PLEN ), '/', DIREND, STATUS )
      DIREND = DIREND + 1

*  Copy the NDF name to the returned string.
      NAME = PATH( DIREND:PLEN )

*  Return the length of the string.
      NMLEN =  PLEN - DIREND + 1

      END
