      SUBROUTINE PARSECON_MESTEXT ( TEXT, STATUS )
*+
*  Name:
*     PARSECON_MESTEXT

*  Purpose:
*     Stores text for a message parameter.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_MESTEXT ( TEXT, STATUS )

*  Description:
*     The parameter currently being defined is a message parameter.
*     Set-up the parameter as a message, and store the message text.

*  Arguments:
*     TEXT=CHARACTER*(*) (given)
*        text of the message
*     STATUS=INTEGER

*  Algorithm:
*     Mark the most-recently defined parameter for READ access, set
*     VPATH to INTERNAL, set the type to character, and put the text
*     string into the static default.

*  Copyright:
*     Copyright (C) 1984, 1993 Science & Engineering Research Council.
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
*     02.10.1984
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
      INCLUDE 'SUBPAR_PAR'


*  Arguments Given:
      CHARACTER*(*) TEXT              ! text of message


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Set type
*
      PARTYPE(PARPTR) = SUBPAR__CHAR
*
*   Set text as static default
*
      CALL PARSECON_SETDEF ( TEXT, STATUS )
*
*   Mark parameter for READ access and INTERNAL VPATH
*
      PARWRITE(PARPTR) = .FALSE.
      PARVPATH(1,PARPTR) = SUBPAR__INTERNAL

      END
