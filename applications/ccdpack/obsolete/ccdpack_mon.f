      SUBROUTINE CCDPACK_MON( STATUS )
*+
*  Name:
*     CCDPACK_MON

*  Purpose:
*     Top-level monolith routine - C-shell CCDPACK executable.

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
*     29-JUN-1992 (PDRAPER):
*        Original version - based on VMS ICL monolith routine of the
*        same name.
*     8-MAR-1993 (PDRAPER):
*        Added new tasks (registration).
*     1-DEC-1993 (PDRAPER): 
*        Added automated processing tasks.
*     5-JAN-1994 (PDRAPER):
*        Converted into a new style monolith (ICL and C-shell).
*     5-JAN-1994 (PDRAPER):
*        Added IMPORT action.
*     9-SEP-1995 (PDRAPER):
*        Added NDF applications naming for history components.
*     29-SEP-1995 (PDRAPER):
*        Added PICINFO application.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! Parameter system constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters

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

      ELSE IF ( ACTION .EQ. 'CCDCLEAR' ) THEN
         CALL CCDCLEAR( STATUS )

      ELSE IF ( ACTION.EQ. 'CCDEDIT' ) THEN
         CALL CCDEDIT( STATUS )

      ELSE IF ( ACTION .EQ. 'CCDGENERATE' ) THEN 
         CALL CCDGENERATE( STATUS )

      ELSE IF ( ACTION.EQ. 'CCDNDFAC' ) THEN
         CALL CCDNDFAC( STATUS )

      ELSE IF ( ACTION .EQ. 'CCDNOTE' ) THEN
         CALL CCDNOTE( STATUS )

      ELSE IF ( ACTION .EQ. 'CCDSETUP' ) THEN
         CALL CCDSETUP( STATUS )

      ELSE IF ( ACTION.EQ. 'CCDSHOW' ) THEN
         CALL CCDSHOW( STATUS )

      ELSE IF ( ACTION .EQ. 'DEBIAS' ) THEN
         CALL DEBIAS( STATUS )

      ELSE IF ( ACTION .EQ. 'FINDCENT' ) THEN
         CALL FINDCENT( STATUS )

      ELSE IF ( ACTION .EQ. 'FINDOBJ' ) THEN
         CALL FINDOBJ( STATUS )

      ELSE IF ( ACTION .EQ. 'FINDOFF' ) THEN
         CALL FINDOFF( STATUS )

      ELSE IF ( ACTION .EQ. 'FLATCOR' ) THEN
         CALL FLATCOR( STATUS )

      ELSE IF ( ACTION .EQ. 'IDICURS' ) THEN
         CALL IDICURS( STATUS )

      ELSE IF ( ACTION .EQ. 'IMPORT' ) THEN
         CALL IMPORT( STATUS )

      ELSE IF ( ACTION .EQ. 'MAKEBIAS' ) THEN
         CALL MAKEBIAS( STATUS )

      ELSE IF ( ACTION .EQ. 'MAKECAL' ) THEN
         CALL MAKECAL( STATUS )

      ELSE IF ( ACTION .EQ. 'MAKEFLAT' ) THEN
         CALL MAKEFLAT( STATUS )

      ELSE IF ( ACTION.EQ. 'MAKEMOS' ) THEN
         CALL MAKEMOS( STATUS )

      ELSE IF ( ACTION.EQ. 'PAIRNDF' ) THEN
         CALL PAIRNDF( STATUS )

      ELSE IF ( ACTION.EQ. 'PICINFO' ) THEN
         CALL PICINFO( STATUS )

      ELSE IF ( ACTION.EQ. 'PLOTLIST' ) THEN
         CALL PLOTLIST( STATUS )

      ELSE IF ( ACTION .EQ. 'PRESENT' ) THEN 
         CALL PRESENT( STATUS )

      ELSE IF ( ACTION.EQ. 'REGISTER' ) THEN
         CALL REGISTER( STATUS )

      ELSE IF ( ACTION .EQ. 'SCHEDULE' ) THEN
         CALL SCHEDULE( STATUS )

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
