************************************************************************

      SUBROUTINE AGI_1FPST ( WKSLOC, PSTLOC, FOUND, STATUS )

*+
*  Name:
*     AGI_1FPST

*  Purpose:
*     Find the picture structure.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGI_1FPST( WKSLOC, PSTLOC, FOUND, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Find the picture structure in the database.

*  Algorithm:
*     Initialise the returned variable.
*     Check status on entry.
*     If picture structure is in workstation then
*        Get the locator to the picture structure.
*     Endif

*  Copyright:
*     Copyright (C) 1988, 1990 Science & Engineering Research Council.
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
*     Nick Eaton  ( DUVAD::NE )
*     {enter_new_authors_here}

*  History:
*     July 1988
*     September 1990  Initialise FOUND
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'agi_nam'


*  Arguments Given:
*     Locator to workstation
      CHARACTER * ( DAT__SZLOC ) WKSLOC


*  Arguments Returned:
*     Locator to picture structure. Undefined if .NOT. FOUND
      CHARACTER * ( DAT__SZLOC ) PSTLOC

*     Flag to indicate if picture structure has been found
      LOGICAL FOUND


*  Status:
      INTEGER STATUS

*.


*   Initialise the returned variable
      FOUND = .FALSE.

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Check to see if picture structure is present
         CALL DAT_THERE( WKSLOC, AGI__PCNAM, FOUND, STATUS )

*   If so get the locator to the structure
         PSTLOC = ' '
         IF ( FOUND ) THEN
            CALL DAT_FIND( WKSLOC, AGI__PCNAM, PSTLOC, STATUS )
         ENDIF

      ENDIF

*      print*, '+++++ AGI_1FPST +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

