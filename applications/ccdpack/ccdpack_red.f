      SUBROUTINE CCDPACK_RED( STATUS )
*+
*  Name:
*     CCDPACK_RED

*  Purpose:
*     Top-level monolith for reduction tasks.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCDPACK( STATUS )

*  Description:
*     This routine gets the action name directly from the Unix kernel.
*     It then calls the appropriate routine to perform the
*     specified action. An error will be reported and STATUS will
*     be set if the action name is not recognized.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-OCT-1995 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! Parameter system constants
      INCLUDE 'CCD1_PAR'         ! VERS value.

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      CHARACTER * ( PAR__SZNAM ) ACTION ! Action name

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the action name.
      CALL TASK_GET_NAME( ACTION, STATUS )

*  Set the NDF application name.
      CALL NDF_HAPPN( ACTION // VERS, STATUS )

*  Test the action name against each valid value in turn, calling the
*  appropriate routine...  
      IF ( ACTION .EQ. 'CALCOR' ) THEN
         CALL CALCOR( STATUS )

      ELSE IF ( ACTION .EQ. 'DEBIAS' ) THEN
         CALL DEBIAS( STATUS )

      ELSE IF ( ACTION .EQ. 'FLATCOR' ) THEN
         CALL FLATCOR( STATUS )

      ELSE IF ( ACTION .EQ. 'MAKEBIAS' ) THEN
         CALL MAKEBIAS( STATUS )

      ELSE IF ( ACTION .EQ. 'MAKECAL' ) THEN
         CALL MAKECAL( STATUS )

      ELSE IF ( ACTION .EQ. 'MAKEFLAT' ) THEN
         CALL MAKEFLAT( STATUS )

*  If the action name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'ACTION', ACTION )
         CALL ERR_REP( 'CCDPACK_ERR',
     :                 'CCDPACK: The action name ''^ACTION'' is ' //
     :                 'not recognised by the CCDPACK '//
     :                 'monolith.', STATUS )
      END IF

      END
* $Id$
