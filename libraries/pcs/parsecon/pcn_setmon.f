      SUBROUTINE PARSECON_SETMON ( NAME, STATUS )
*+
*  Name:
*     PARSECON_SETMON

*  Purpose:
*     set-up for parsing monolith IFL.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_SETMON ( NAME, STATUS )

*  Description:
*     Handle MONOLITH statement

*  Arguments:
*     NAME=CHARACTER*(*) (given)
*        name of the monolith
*     STATUS=INTEGER

*  Algorithm:
*     Set the flag which indicates that the interface file is the
*     declaration for a monolith.

*  Copyright:
*     Copyright (C) 1985, 1993 Science & Engineering Research Council.
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
*     23.08.1985:  Original (REVAD::BDK)
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
      CHARACTER*(*) NAME               ! name of the monolith


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      MONOLITH = .TRUE.
      FACENAME = NAME

      END
