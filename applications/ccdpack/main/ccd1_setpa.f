      SUBROUTINE CCD1_SETPA( PARAM, VALUE, STATUS )
*+
*  Name:
*     CCD1_SETPA

*  Purpose:
*     Sets the value of a program parameter.

*  Language:
*     Starlink Fortran-77.

*  Invocation:
*     CALL CCD1_SETPA( PARAM, VALUE, STATUS )

*  Description:
*     This is a non-standard routine for setting the value of a command
*     parameter to a value as if given on the command-line. Its use is
*     strictly for super-applications which need to call existing
*     commands directly (so that for instance parameters can be set to
*     null or derived values) and is generally to be avoided.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*       The parameter whose value is to be set.
*     VALUE = CHARACTER * ( * ) (Given)
*       The value.
*     STATUS = INTEGER ({status_access_mode})
*        The global status.

*  Notes:
*     - Uses undocumented SUBPAR calls.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     4-JUN-1997 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard constants
      
*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) VALUE
      
*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      INTEGER NAMCOD            ! Code for PARAM 
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      
*  Get the NAMECODE and then set the parameter.
      CALL SUBPAR_FINDPAR( PARAM , NAMCOD, STATUS )
      CALL SUBPAR_CMDPAR( NAMCOD, VALUE, STATUS)
      END
