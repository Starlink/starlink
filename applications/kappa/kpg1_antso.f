      SUBROUTINE KPG1_ANTSO( STATUS )
*+
*  Name:
*     KPG1_SOLIN

*  Purpose:
*     Sets the aspect source flag of the GKS linetype to bundled.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ANTSO( STATUS )

*  Description:
*     The routine sets the aspect source flag of the linetype of the 
*     current workstation to bundled.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     A GKS workstation should be open and active.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 June 12 (MJC):
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

*  Local Variables:
      INTEGER GSTAT              ! Graphics status
      INTEGER LASF( 13 )         ! GKS list of aspect source flags

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Inquire the current aspect source flag setting.

      CALL GQASF( GSTAT, LASF )

*    Set the linetype aspect source flags to bundled.

      LASF( 1 ) = 0
      CALL GSASF( LASF )

*    Determine whether or not there was an error in GKS.

      CALL GKS_GSTAT( STATUS )

      END
