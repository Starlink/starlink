      SUBROUTINE SUBPAR_GETKEY ( NAMECODE, KEYWORD, STATUS )
*+
*  Name:
*     SUBPAR_GETKEY

*  Purpose:
*     get a parameter's keyword.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_GETKEY ( NAMECODE, KEYWORD, STATUS )

*  Description:
*     Return the keyword associated with the indicated program
*     parameter.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        code number of parameter
*     KEYWORD=CHARACTER*(*) (returned)
*        keyword for parameter
*     STATUS=INTEGER

*  Algorithm:
*     Copy the keyword from the SUBPAR common blocks.

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
*     BDK: B D Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     13-NOV-1984 (BDK):
*        Original version
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
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
      INTEGER NAMECODE                ! pointer to parameter


*  Arguments Returned:
      CHARACTER*(*) KEYWORD           ! parameter keyword


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Copy the value from storage
*
      KEYWORD = PARKEY(NAMECODE)

      END
