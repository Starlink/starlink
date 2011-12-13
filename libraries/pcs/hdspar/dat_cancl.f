      SUBROUTINE DAT_CANCL ( PARAM, STATUS )
*+
*  Name:
*     DAT_CANCL

*  Purpose:
*     Cancel association between a parameter and a data object.

*  Language:
*     Fortran 77

*  Invocation:
*     CALL DAT_CANCL ( PARAM, STATUS )

*  Description:
*     An existing association between the named parameter and a data
*     system object is cancelled, and the container file closed.
*     This routine will attempt to operate regardless of the given
*     STATUS value.
*
*     The parameter enters the CANCELLED state.

*  Arguments:
*     PARAM=CHARACTER*(*) (given)
*        Name of program parameter
*     STATUS=INTEGER (given and returned)
*        Global status

*  Algorithm:
*     The internal identifying number for the named parameter is
*     obtained, and used to call SUBPAR_CANCL which does the work.

*  Copyright:
*     Copyright (C) 1984-1985 Science & Engineering Research Council.
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
*     24-SEP-1984 (BDK)
*        Original
*     21-MAR-1985 (BDK)
*        Make execute even if status bad
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
      CHARACTER*(*) PARAM

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER NAMECODE               ! pointer to internal parameter
                                     ! storage

      INTEGER ISTAT                  ! temporary status

*.

      ISTAT = STATUS
      STATUS = SAI__OK

      CALL SUBPAR_FINDPAR ( PARAM, NAMECODE, STATUS )

      CALL SUBPAR_CANCL ( NAMECODE, STATUS )

      IF ( ISTAT .NE. SAI__OK ) STATUS = ISTAT

      END
