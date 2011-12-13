************************************************************************

      SUBROUTINE AGI_1FPIC ( PSTLOC, PICNUM, PICLOC, FOUND, STATUS )

*+
*  Name:
*     AGI_1FPIC

*  Purpose:
*     Find a picture in a picture structure.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGI_1FPIC( PSTLOC, PICNUM, PICLOC, FOUND, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Find a picture in a picture stucture.

*  Algorithm:
*     Check status on entry.
*     Inquire the current number of pictures in the array of pictures.
*     If number less than given picture number
*     or picture number less than one then
*        Indicate picture not found.
*     Else
*        Get locator to picture cell.
*     Endif

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     David Berry
*     {enter_new_authors_here}

*  History:
*     July 1988
*     Feb 2006: Initialise the returned value of FOUND before checking status
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
      CHARACTER * ( DAT__SZLOC ) PSTLOC

*     Requested picture number in array of pictures
      INTEGER PICNUM


*  Arguments Returned:
*     Locator to picture. Undefined in .NOT. FOUND
      CHARACTER * ( DAT__SZLOC ) PICLOC

*     Flag to indicate if picture has been found
      LOGICAL FOUND


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER SIZE

*.


      FOUND = .FALSE.
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Get the number of current cells
         CALL DAT_SIZE( PSTLOC, SIZE, STATUS )

*   If there are fewer pictures than picnum then one cannot be found
*   or if picnum is invalid
         PICLOC = ' '
         IF ( ( SIZE .LT. PICNUM ) .OR. ( PICNUM .LT. 1 ) ) THEN
            FOUND = .FALSE.
         ELSE
            CALL DAT_CELL( PSTLOC, 1, PICNUM, PICLOC, STATUS )
            FOUND = .TRUE.
         ENDIF

      ENDIF

*      print*, '+++++ AGI_1FPIC +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

