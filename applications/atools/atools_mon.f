      SUBROUTINE ATOOLS_MON( STATUS )
*+
* Name:
*     ATOOLS_MON

*  Purpose:
*     Top-level ADAM monolith routine for the ATOOLS package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATOOLS_MON( STATUS )

*  Description:
*     This routine obtains the name of the current action and calls the
*     appropriate routine to perform the specified operation. An error
*     will be reported and STATUS will be set if the action name is not
*     recognised.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David S. Berry (Starlink)
*     {enter_new_authors_here}

*  History:
*     16-JAN-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      CHARACTER * ( 15 ) NAME   ! Action name

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the action name.
      CALL TASK_GET_NAME( NAME, STATUS )

*  Test the action name against each valid value in turn, calling the
*  appropriate routine...

      IF ( NAME .EQ. 'ASTADDFRAME' ) THEN
         CALL ASTADDFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTCLEAR' ) THEN
         CALL ASTCLEAR( STATUS )

      ELSE IF ( NAME .EQ. 'ASTGET' ) THEN
         CALL ASTGET( STATUS )

      ELSE IF ( NAME .EQ. 'ASTSET' ) THEN
         CALL ASTSET( STATUS )

      ELSE IF ( NAME .EQ. 'ASTTEST' ) THEN
         CALL ASTTEST( STATUS )

      ELSE IF ( NAME .EQ. 'ATLHELP' ) THEN
         CALL ATLHELP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTCMPFRAME' ) THEN
         CALL ASTCMPFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTCMPMAP' ) THEN
         CALL ASTCMPMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTFRAME' ) THEN
         CALL ASTFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTGETFRAME' ) THEN
         CALL ASTGETFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTGETMAPPING' ) THEN
         CALL ASTGETMAPPING( STATUS )

      ELSE IF ( NAME .EQ. 'ASTSKYFRAME' ) THEN
         CALL ASTSKYFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTFRAMESET' ) THEN
         CALL ASTFRAMESET( STATUS )

      ELSE IF ( NAME .EQ. 'ASTPERMMAP' ) THEN
         CALL ASTPERMMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTUNITMAP' ) THEN
         CALL ASTUNITMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTREMOVEFRAME' ) THEN
         CALL ASTREMOVEFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTREMAPFRAME' ) THEN
         CALL ASTREMAPFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTMATRIXMAP' ) THEN
         CALL ASTMATRIXMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTCONVERT' ) THEN
         CALL ASTCONVERT( STATUS )

*  If the action name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'ATOOLS_MON_ERR',
     :        'ATOOLS_MON: The action name ''^NAME'' is ' //
     :        'not recognised by the ATOOLS_MON monolith.',
     :        STATUS )
      END IF

      END
