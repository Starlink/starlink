      SUBROUTINE KPG1_ENVDF( VARNAM, DEF, STATUS )
*+
*  Name:
*     KPG1_ENVDF

*  Purpose:
*     See if an environment variable is defined.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ENVDF( VARNAM, DEF, STATUS )

*  Description:
*     This routine returns a logical flag indicating if the specified
*     environment variable is defined.

*  Arguments:
*     VARNAM = CHARACTER * ( * ) (Given)
*        The environment variable to check.
*     DEF = LOGICAL (Returned)
*        Is the environment variable defined?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine attempts to execute even if STATUS is set to an
*     error on entry.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-SEP-1999 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PSX_ERR'          ! PSX error constants 

*  Arguments Given:
      CHARACTER VARNAM*(*)

*  Arguments Returned:
      LOGICAL DEF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER VAL*40           ! Value of the environment variable
*.

*  Begin a new error reporting context.
      CALL ERR_BEGIN( STATUS )

*  Attempt to get the value of the anvironment variable KAPPAENVDFOSE.
      CALL PSX_GETENV( VARNAM, VAL, STATUS )

*  If the environment variable was not defined, annul the error, and 
*  indicate that it is not defined.
      IF( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )
         DEF = .FALSE.

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         DEF = .TRUE.

      END IF

*  End the error reporting context.
      CALL ERR_END( STATUS )

      END
