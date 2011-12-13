      SUBROUTINE PARSECON_SETFACE ( NAME, STATUS )
*+
*  Name:
*     PARSECON_SETFACE

*  Purpose:
*     Set interface name.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_SETFACE ( NAME, STATUS )

*  Description:
*     Sets interface name into common block.

*  Arguments:
*     NAME=CHARACTER*(*) (given)
*        interface name
*     STATUS=INTEGER

*  Algorithm:
*     Put the given string into the common-block variable.
*     Declare the name to be an action.

*  Copyright:
*     Copyright (C) 1984, 1985, 1991, 1993 Science & Engineering Research Council.
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
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     02.10.1984:  Original (REVAD::BDK)
*     23.08.1985:  handle monoliths (REVAD::BDK)
*     12.11.1991:  Initialise HIPOS (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'


*  Arguments Given:
      CHARACTER*(*) NAME                 ! string to be inserted


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'
      INCLUDE 'PARSECON4_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   set the interface file name
      IF ( .NOT. MONOLITH ) THEN
         FACENAME = NAME
      ENDIF

*   initialise the highest parameter position number for this task
      HIPOS = 0

*   declare the name to be an action
      CALL PARSECON_NEWACT ( NAME, STATUS )
      CALL PARSECON_SETOB ( .TRUE., STATUS )

      END
