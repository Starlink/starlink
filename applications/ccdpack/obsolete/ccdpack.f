      SUBROUTINE CCDPACK( ACTION, STATUS )
*+
*  Name:
*     CCDPACK

*  Purpose:
*     Top-level ADAM monolith routine for the CCDPACK package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCDPACK( ACTION, STATUS )

*  Description:
*     This routine interprets the action name passed to it and calls
*     the appropriate routine to perform the specified action. An error
*     will be reported and STATUS will be set if the action name is not
*     recognised.

*  Arguments:
*     ACTION = CHARACTER * ( * ) (Given and Returned)
*        The action name to be interpreted. The value given will be
*        forced to upper case by this routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JUL-1991 (PDRAPER):
*        Original version.
*     2-JUL-1993 (PDRAPER):
*        Added new tasks (registration).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given and Returned:
      CHARACTER * ( * ) ACTION

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert the action name to upper case.
      CALL CHR_UCASE( ACTION )

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

      ELSE IF ( ACTION.EQ. 'PLOTLIST' ) THEN
         CALL PLOTLIST( STATUS )

      ELSE IF ( ACTION.EQ. 'REGISTER' ) THEN
         CALL REGISTER( STATUS )

      ELSE IF ( ACTION.EQ. 'TRANLIST' ) THEN
         CALL TRANLIST( STATUS )

      ELSE IF ( ACTION .EQ. 'TRANNDF' ) THEN 
         CALL TRANNDF( STATUS )

*  Include slightly non-standard listlog for efficiency.
      ELSE IF ( ACTION .EQ. 'LISTLOG' ) THEN
         CALL LISTLOG( STATUS )

*  If the action name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'ACTION', ACTION )
         CALL ERR_REP( 'CCDPACK_ERR',
     :                 'CCDPACK: The action name ''^ACTION'' is ' //
     :                 'not recognised by the CCDPACK monolith.',
     :                 STATUS )
      END IF

      END
* $Id$
