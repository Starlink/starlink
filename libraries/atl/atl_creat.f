      SUBROUTINE ATL_CREAT( PARAM, IAST, STATUS )
*+
*  Name:
*     ATL_CREAT

*  Purpose:
*     Write an AST Object to a text file or NDF specified using an environment
*     parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_CREAT( PARAM, IAST, STATUS )

*  Description:
*     Write an AST Object to a text file or NDF specified using an environment
*     parameter.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name.
*     IAST = INTEGER (Given)
*        The AST Object, or AST__NULL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001, 2003 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JAN-2001 (DSB):
*        Original version.
*     15-SEP-2003 (DSB):
*        Changed call to FIO_ANNUL to FIO_CLOSE. Previously the output
*        text file was not completely flushed when being used as a 
*        monolith (e.g. from ICL), resulting in incomplete output files.
*     11-MAY-2006 (DSB):
*        Increase maximum line length to 300 characters.
*     30-MAY-2006 (DSB):
*        Moved into ATL library and changed prefix from "ATL1_" to "ATL_".
*     11-DEC-2008 (DSB):
*        Modified to call ATL_SHOW to do the work.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER IAST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER FNAME*255
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the name of the output file.
      CALL PAR_GET0C( PARAM, FNAME, STATUS )

*  Dump the object to the file.
      CALL ATL_SHOW( IAST, FNAME, STATUS )

*  Tell the user.
      CALL MSG_SETC( 'F', FNAME )
      CALL ATL_NOTIF( '   AST data written to text file ''^F''.', 
     :                 STATUS )

      END

