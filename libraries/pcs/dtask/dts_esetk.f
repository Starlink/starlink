      SUBROUTINE DTASK_ESETK ( NAME, STATUS )
*+
*  Name:
*     DTASK_ESETK

*  Purpose:
*     Set message associated with status into token

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_ESETK ( NAME, STATUS )

*  Description:
*     Set message associated with status into EMS token.
*     This is done via this routine to assist in portability.

*  Arguments:
*     NAME=CHARACTER*(*) (given)
*           name of the token to be set
*     STATUS=INTEGER

*  Algorithm:
*     Call ERR_FACER

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-OCT-1992 (AJC)::
*
*       Original version
*     30-SEP-1994 (AJC)::
*
*       Use ERR_FACER not ERR_SYSER (Now common for Unix and VMS)
*     01-MAR-1995 (AJC)::
*
*       Remove comment VMS version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
*  Arguments Given:
      CHARACTER*(*) NAME
*  Status:
      INTEGER STATUS
*.

*   If status is OK, set 'OK'
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( NAME, 'OK' )

*   otherwise call ERR_FACER
      ELSE
         CALL ERR_FACER( NAME, STATUS )

      ENDIF

      END
