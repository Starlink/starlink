      SUBROUTINE SUBPAR_DEXIT( STATUS )
*+
*  Name:
*     SUBPAR_DEXIT

*  Purpose:
*     UNIX dummy version of VMS declare exit handler for SUBPAR

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_DEXIT( STATUS )

*  Description:
*     Does nothing

*  Arguments:
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

*  Global Constatnts:
      INCLUDE 'SAE_PAR'          ! SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      END
