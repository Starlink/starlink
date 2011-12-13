************************************************************************

      SUBROUTINE AGI_1FPAR ( PICLOC, PTYPE, PARLOC, FOUND, STATUS )

*+
*  Name:
*     AGI_1FPAR

*  Purpose:
*     Find a parameter in a picture.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGI_1FPAR ( PICLOC, PTYPE, PARLOC, FOUND, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Find a parameter in a picture structure.

*  Algorithm:
*     Initialise the returned variable
*     Check status on entry.
*     If parameter is in picture structure then
*        Get the locator to the parameter.
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


*  Arguments Given:
*     Locator to picture structure
      CHARACTER * ( DAT__SZLOC ) PICLOC

*     Name of parameter to find
      CHARACTER * ( * ) PTYPE


*  Arguments Returned:
*     Locator to parameter. Undefined if .NOT. FOUND
      CHARACTER * ( DAT__SZLOC ) PARLOC

*     Flag to indicate if parameter has been found
      LOGICAL FOUND


*  Status:
      INTEGER STATUS

*.


*   Initialise the returned variable
      FOUND = .FALSE.

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Check if parameter is there
         CALL DAT_THERE( PICLOC, PTYPE, FOUND, STATUS )

*   Get locator to parameter
         PARLOC = ' '
         IF ( FOUND ) THEN
            CALL DAT_FIND( PICLOC, PTYPE, PARLOC, STATUS )
         ENDIF

      ENDIF

*      print*, '+++++ AGI_1FPAR +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

