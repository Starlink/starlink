      SUBROUTINE CCDPACK_RES( STATUS )
*+
*  Name:
*     CCDPACK_RES

*  Purpose:
*     Top-level monolith for miscellaneous routines.

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
      IF ( ACTION .EQ. 'CCDCLEAR' ) THEN
         CALL CCDCLEAR( STATUS )

      ELSE IF ( ACTION .EQ. 'CCDEXP' ) THEN 
         CALL CCDEXP( STATUS )

      ELSE IF ( ACTION .EQ. 'CCDGENERATE' ) THEN 
         CALL CCDGENERATE( STATUS )

      ELSE IF ( ACTION .EQ. 'CCDIMP' ) THEN 
         CALL CCDIMP( STATUS )

      ELSE IF ( ACTION.EQ. 'CCDNDFAC' ) THEN
         CALL CCDNDFAC( STATUS )

      ELSE IF ( ACTION .EQ. 'CCDNOTE' ) THEN
         CALL CCDNOTE( STATUS )

      ELSE IF ( ACTION .EQ. 'CCDSETUP' ) THEN
         CALL CCDSETUP( STATUS )

      ELSE IF ( ACTION.EQ. 'CCDSHOW' ) THEN
         CALL CCDSHOW( STATUS )

      ELSE IF ( ACTION .EQ. 'IMPORT' ) THEN
         CALL IMPORT( STATUS )

      ELSE IF ( ACTION.EQ. 'PICINFO' ) THEN
         CALL PICINFO( STATUS )

      ELSE IF ( ACTION .EQ. 'PRESENT' ) THEN 
         CALL PRESENT( STATUS )

      ELSE IF ( ACTION .EQ. 'SCHEDULE' ) THEN
         CALL SCHEDULE( STATUS )

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
