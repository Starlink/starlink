      SUBROUTINE CCDPACK_REG( STATUS )
*+
*  Name:
*     CCDPACK_REG

*  Purpose:
*     Top-level monolith for registration tasks.

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
*     31-OCT-1995 (PDRAPER):
*        Original version 
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
      IF ( ACTION.EQ. 'CCDEDIT' ) THEN
         CALL CCDEDIT( STATUS )

      ELSE IF ( ACTION .EQ. 'FINDCENT' ) THEN
         CALL FINDCENT( STATUS )

      ELSE IF ( ACTION .EQ. 'FINDOBJ' ) THEN
         CALL FINDOBJ( STATUS )

      ELSE IF ( ACTION .EQ. 'FINDOFF' ) THEN
         CALL FINDOFF( STATUS )

      ELSE IF ( ACTION .EQ. 'IDICURS' ) THEN
         CALL IDICURS( STATUS )

      ELSE IF ( ACTION.EQ. 'MAKEMOS' ) THEN
         CALL MAKEMOS( STATUS )

      ELSE IF ( ACTION.EQ. 'PAIRNDF' ) THEN
         CALL PAIRNDF( STATUS )

      ELSE IF ( ACTION.EQ. 'PLOTLIST' ) THEN
         CALL PLOTLIST( STATUS )

      ELSE IF ( ACTION.EQ. 'REGISTER' ) THEN
         CALL REGISTER( STATUS )

      ELSE IF ( ACTION.EQ. 'TRANLIST' ) THEN
         CALL TRANLIST( STATUS )

      ELSE IF ( ACTION .EQ. 'TRANNDF' ) THEN 
         CALL TRANNDF( STATUS )

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
