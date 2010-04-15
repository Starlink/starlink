      SUBROUTINE PISA_MON4( STATUS )
*+
*  Name:
*     PISA_MON4

*  Purpose:
*     Top-level monolith routine for PISA I*4 code.

*  Language:
*     FORTRAN

*  Invocation:
*     CALL PISA_MON4( STATUS )

*  Description:
*     This routine calls the appropriate routine to perform the
*     specified action. An error will be reported and STATUS will
*     be set if the action name is not recognised.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-AUG-1992 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_PAR'

*  Status:
      INTEGER STATUS

*  Local variables:
      CHARACTER * ( PAR__SZNAM ) NAME ! Name of task
*.

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the task name.
      CALL TASK_GET_NAME( NAME, STATUS )

*   Call the appropriate task
      IF ( NAME .EQ. 'PISAFIND4' ) THEN
         CALL PISAFIND4( STATUS )
      ELSE IF ( NAME .EQ. 'PISAFIT4' ) THEN
         CALL PISAFIT4( STATUS )

*  If the name name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'PISA_ERR',
     :                 'PISA(I4): The name name ''^NAME'' is ' //
     :                 'not recognised by the PISA(I4) '//
     :                 'monolith.', STATUS )
      END IF

      END

* $Id$
