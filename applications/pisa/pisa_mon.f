      SUBROUTINE PISA_MON( STATUS )
*+
*  Name:
*     PISA_MON

*  Purpose:
*     Top-level monolith routine.

*  Language:
*     FORTRAN

*  Invocation:
*     CALL PISA_MON( STATUS )

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
*     11-AUG-1992 (PDRAPER):
*        Original version.
*     12-MAY-1994 (PDRAPER):
*        Added PISA2ARD routine.
*     13-MAY-1994 (PDRAPER):
*        New style monolith.
*     20-NOV-1995 (PDRAPER):
*        Added PISA2CAT.
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
      IF ( NAME .EQ. 'PISAFIND' ) THEN
         CALL PISAFIND( STATUS )
      ELSE IF ( NAME .EQ. 'PISAPLOT' ) THEN
         CALL PISAPLOT( STATUS )
      ELSE IF ( NAME .EQ. 'PISAFIT' ) THEN
         CALL PISAFIT( STATUS )
      ELSE IF ( NAME .EQ. 'PISAGEN' ) THEN
         CALL PISAGEN( STATUS )
      ELSE IF ( NAME .EQ. 'PISAPEAK' ) THEN
         CALL PISAPEAK( STATUS )
      ELSE IF ( NAME .EQ. 'PISAKNN' ) THEN
         CALL PISAKNN( STATUS )
      ELSE IF ( NAME .EQ. 'PISAMATCH' ) THEN
         CALL PISAMATCH( STATUS )
      ELSE IF ( NAME .EQ. 'PISACUT' ) THEN
         CALL PISACUT( STATUS )
      ELSE IF ( NAME .EQ. 'ADDNOISE' ) THEN
         CALL ADDNOISE( STATUS )
      ELSE IF ( NAME .EQ. 'PISAGREY' ) THEN
         CALL PISAGREY( STATUS )
      ELSE IF ( NAME .EQ. 'PISA2ARD' ) THEN
         CALL PISA2ARD( STATUS )
      ELSE IF ( NAME .EQ. 'PISA2CAT' ) THEN
         CALL PISA2CAT( STATUS )

*  If the name name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'PISA_ERR',
     :                 'PISA: The name name ''^NAME'' is ' //
     :                 'not recognised by the PISA '//
     :                 'monolith.', STATUS )
      END IF

      END

* $Id$
