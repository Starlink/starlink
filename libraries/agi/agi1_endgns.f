      SUBROUTINE AGI1_ENDGNS( STATUS )
*+
*  Name:
*     AGI1_ENDGNS

*  Purpose:
*     Stop GNS system after AGI usage

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL AGI1_ENDGNS( STATUS )

*  Description:
*     This routine exists entirely to allow AGP to subclass
*     this routine without having a GNS dependency itself.
*     This code is abstracted from AGI_END so that AGP
*     does not have to provide a more extensive rewrite of
*     AGI_END simply to remove the GNS dependency.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     14-JUL-2004 (TIMJ):
*        Original version. Two lines taken from AGI_END

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

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Make sure GNS is shut down for all packages
      CALL GNS_STOP( 'GKS', STATUS )
      CALL GNS_STOP( 'IDI', STATUS )

      END
