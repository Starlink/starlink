      SUBROUTINE SUBPAR_CANLOC ( NAMECODE, STATUS )
*+
*  Name:
*     SUBPAR_CANLOC

*  Purpose:
*     Marks the HDS locators of a parameter as cancelled.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_CANLOC ( NAMECODE, STATUS )

*  Description:
*     The locator stoarage for the indicated parameter is marked as
*     invalid.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        pointer to the parameter
*     STATUS=INTEGER

*  Algorithm:
*     NAMECODE indexes into the arrays for holding the locator values.

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
*     20-SEP-1984 (BDK):
*        Original
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
      INCLUDE 'SUBPAR_ERR'


*  Arguments Given:
      INTEGER NAMECODE          ! pointer to the parameter


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Check that NAMECODE is in range, and set the validity flag to false.
*
      IF ( ( NAMECODE .LE. PARPTR ) .AND. ( NAMECODE .GT. 0 ) ) THEN

         PARVALID(NAMECODE) = .FALSE.

      ELSE

         STATUS = SUBPAR__NOPAR

      ENDIF

      END
