      SUBROUTINE KPG1_VERB( VERB, STATUS )
*+
*  Name:
*     KPG1_VERB

*  Purpose:
*     Should KAPPA report verbose messages?

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_VERB( VERB, STATUS )

*  Description:
*     This routine returns a logical flag indicating if KAPPA
*     applications should report verbose information. This is the case if
*     the environment variable KAPPA_VERBOSE is defined (the value assigned
*     to the environment variable is immaterial).

*  Arguments:
*     VERB = LOGICAL (Returned)
*        Should KAPPA run in verbose mode? Returned .FALSE, if an error 
*        has already occurred, or if this routine should fail for any
*        reason.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine attempts to execute even if STATUS is set to an
*     error on entry.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-SEP-1998 (DSB):
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

*  Arguments Returned:
      LOGICAL VERB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ENV*40           ! Value of KAPPA_VERBOSE env. variable
*.

*  Begin a new error reporting context.
      CALL ERR_BEGIN( STATUS )

*  Attempt to get the value of the anvironment variable KAPPA_VERBOSE.
      CALL PSX_GETENV( 'KAPPA_VERBOSE', ENV, STATUS )

*  If the environment variable was not defined, annul the error, and 
*  indicate that verbose mode should not be used.
      IF( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )
         VERB = .FALSE.

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         VERB = .TRUE.

      END IF

*  End the error reporting context.
      CALL ERR_END( STATUS )

      END
