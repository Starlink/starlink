      SUBROUTINE PHOTOM_MON( STATUS )
*+
*  Name:
*     PHOTOM_MON

*  Purpose:
*     Top-level monolith routine.

*  Language:
*     FORTRAN

*  Invocation:
*     CALL PHOTOM_MON( STATUS )

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
*     5-NOV-1996 (PDRAPER):
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

*  Call the appropriate task
      IF ( NAME .EQ. 'PHOTOM' ) THEN
         CALL PHOTOM( STATUS )
      ELSE IF ( NAME .EQ. 'PHOTOPT' ) THEN
         CALL PHOTOPT( STATUS )
      ELSE IF ( NAME .EQ. 'PHOTGREY' ) THEN
         CALL PHOTGREY( STATUS )
      ELSE IF ( NAME .EQ. 'AUTOPHOTOM' ) THEN
         CALL AUTOPHOTOM( STATUS )

*  If the name name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'PHOTOM_ERR',
     :                 'PHOTOM: The name name ''^NAME'' is ' //
     :                 'not recognised by the PHOTOM '//
     :                 'monolith.', STATUS )
      END IF
      END

* $Id$
