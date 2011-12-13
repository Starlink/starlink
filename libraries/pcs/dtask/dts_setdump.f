      SUBROUTINE DTASK_SETDUMP ( STATUS )
*+
*  Name:
*     DTASK_SETDUMP

*  Purpose:
*     Dummy for Unix: enable generation of stack dump on command

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_SETDUMP ( STATUS )

*  Description:
*     Enable or re-enable facility for generating a stack dump of a
*     task.

*  Arguments:
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*     STATUS=INTEGER

*  Algorithm:
*     The first call of this routine will be from mainline code. In this
*     case, create a mailbox with a name based on the process name, and
*     start a QIO to it declaring a completion AST handler. The AST
*     handler generates a stack dump when some other task writes to the
*     mailbox. The AST handler also calls this routine to re-enable the
*     QIO.
*     Subsequent calls to this routine will be from the AST handler. In
*     this case, restart the QIO.

*  Copyright:
*     Copyright (C) 1986 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     13-AUG-1986 (REVAD::BDK):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
*  Status:
      INTEGER STATUS

*  Local Variables:
*.


      IF ( STATUS .NE. SAI__OK ) RETURN


      END
