      SUBROUTINE ATL1_SETOP( PARAM, IAST, STATUS )
*+
*  Name:
*     ATL1_SETOP

*  Purpose:
*     Write an AST Object to a text file specified using an environment
*     parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL1_SETOP( PARAM, IAST, STATUS )

*  Description:

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name.
*     IAST = INTEGER (Given)
*        The AST Object.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JAN-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'GRP_PAR'          ! GRP constants 
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER IAST

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER OPT*128
      INTEGER I
      INTEGER IGRP
      INTEGER SIZE
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a group of attribute values.
      IGRP = GRP__NOID
      CALL KPG1_GTGRP( PARAM, IGRP, SIZE, STATUS )

*  If not supplied, annul the error.
      IF( STATUS .EQ. PAR__NULL ) THEN 
         CALL ERR_ANNUL( STATUS )
 
*  Otherwise, apply each one to the returned Frame.
      ELSE
         DO I = 1, SIZE
            CALL GRP_GET( IGRP, I, 1, OPT, STATUS )
            IF( OPT .NE. ' ' ) THEN
                CALL AST_SET( IAST, OPT( : CHR_LEN( OPT ) ), STATUS )
            END IF
         END DO

*  Delete the group.
         CALL GRP_DELET( IGRP, STATUS )

      END IF

      END
