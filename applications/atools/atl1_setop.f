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

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name.
*     IAST = INTEGER (Given)
*        The AST Object.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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
