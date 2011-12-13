      SUBROUTINE PARSECON_TABENT ( STATE, TOKTYPE, ACTCODE,
     :  NEWSTATE, STATUS )
*+
*  Name:
*     PARSECON_TABENT

*  Purpose:
*     Look-up parsing state-table.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_TABENT ( STATE, TOKTYPE, ACTCODE,
*    :   NEWSTATE, STATUS )

*  Description:
*     Look-up parsing state-table

*  Arguments:
*     STATE=INTEGER (given)
*        current parsing state
*     TOKTYPE=INTEGER (given)
*        type of current token
*     ACTCODE=INTEGER (returned)
*        action code
*     NEWSTATE=INTEGER (returned)
*        new parsing state
*     STATUS=INTEGER

*  Copyright:
*     Copyright (C) Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'


*  Arguments Given:
      INTEGER STATE           ! current parsing state

      INTEGER TOKTYPE         ! type of current token


*  Arguments Returned:
      INTEGER ACTCODE         ! action code (0 = none)

      INTEGER NEWSTATE        ! new parsing state


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'PARSECON_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Look-up the two values corresponding to the given combination of
*   parse-state and token-type. An invalid combination will result in
*          ACTCODE = ERROR
*          NEWSTATE = FACEGOT
*
      ACTCODE = ACTTAB(STATE,TOKTYPE)
      NEWSTATE = STATETAB(STATE,TOKTYPE)

      END
