      SUBROUTINE DATACUBE_MON( STATUS )
*+
*  Name:
*     DATACUBE_MON

*  Purpose:
*     Top-level monolith routine.

*  Language:
*     FORTRAN

*  Invocation:
*     CALL DATACUBE_MON( STATUS )

*  Description:
*     This routine calls the appropriate routine to perform the
*     specified action. An error will be reported and STATUS will
*     be set if the action name is not recognised.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils


*  Authors:
*     AALLAN: Alasdair Allan (STARLINK, Keele University)
*     {enter_new_authors_here}

*  History:
*     31-MAY-2000 (AALLAN):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
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
      IF ( NAME .EQ. 'GETBOUND' ) THEN
         CALL GETBOUND( STATUS )  
      ELSE IF ( NAME .EQ. 'PUTAXIS' ) THEN
         CALL PUTAXIS( STATUS )    
      ELSE IF ( NAME .EQ. 'COPYAXIS' ) THEN
         CALL COPYAXIS( STATUS )     
*  If the name name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'DATACUBE_ERR',
     :                 'DATACUBE: The name name ''^NAME'' is ' //
     :                 'not recognised by the DATACUBE '//
     :                 'monolith.', STATUS )
      END IF
      END
         
         

