      SUBROUTINE KPG1_ASGFW( STATUS )
*+
*  Name:
*     KPG1_ASGFW

*  Purpose:
*     Writes a line of an AST Object description to a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASGFW( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine writes a line of an AST Object description to a GRP
*     group, optionally prepending it with the a given string. It is 
*     intended to be used as a sink function with AST_CHANNEL.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Global Constants:
      INCLUDE 'KPG_AST'          ! KPG AST common blocks.
*        ASTGRP = INTEGER (Read)
*           GRP identifier for the group.
*        ASTGSP = CHARACTER * 1 (Read)
*           The character to be prepended to the text. Ignored if blank.

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER LINE*(GRP__SZNAM)! Text to be written to the group
      INTEGER L                  ! Length of text
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the line of text to be written to the group.
      LINE = ' '
      CALL AST_GETLINE( LINE, L, STATUS ) 

*  If required, prepend it with the supplied character.
      IF( ASTGSP .NE. ' ' ) CALL CHR_PREFX( ASTGSP, LINE, L ) 

*  Append it to the group.
      CALL GRP_PUT( ASTGRP, 1, LINE( : L ), 0, STATUS ) 

      END
