      SUBROUTINE DAT_DELET ( PARAM, STATUS )
*+
*  Name:
*     DAT_DELET

*  Purpose:
*     Delete an object associated with a parameter

*  Language:
*     Fortran 77

*  Invocation:
*     CALL DAT_DELET ( PARAM, STATUS )

*  Description:
*     Get an object name and delete the object.

*  Arguments:
*     PARAM=CHARACTER*(*) (given)
*        Name of program parameter
*     STATUS=INTEGER (given and returned)
*        Global status

*  Algorithm:
*     The character string associated with the given parameter is
*     obtained, and interpreted as a filename (an HDS container
*     file), followed by the full name of the structure component
*     required. The component is deleted if possible. The data structure
*     down to the level immediately above the required new component
*     must exist already.
*     Cancel the parameter.

*  Copyright:
*     Copyright (C) 1985, 1987 Science & Engineering Research Council.
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (Starlink)
*     {enter_new_authors_here}

*  History:
*     20-MAR-1985 (BDK)
*        Original
*     04-MAY-1987 (BDK)
*        Change to call SUBPAR_DELET
*     16-JUN-1998 (AJC)
*        Re-format prologue
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) PARAM          ! parameter name

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER NAMECODE                     ! pointer to program parameter

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL SUBPAR_FINDPAR ( PARAM, NAMECODE, STATUS )
      CALL SUBPAR_DELET ( NAMECODE, STATUS )

      END
