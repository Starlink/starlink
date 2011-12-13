************************************************************************

      SUBROUTINE AGI_1WPARI ( PICLOC, PTYPE, PVAL, STATUS )

*+
*  Name:
*     AGI_1WPARI

*  Purpose:
*     Write an integer parameter to the database.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGI_1WPARI( PICLOC, PTYPE, PVAL, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Write an integer parameter to the database

*  Algorithm:
*     Check status on entry.
*     If the given parameter is not present then
*        Create the parameter.
*     Endif
*     Write the contents of the array into the parameter field.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     Jun 1990
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
*     Locator to picture
      CHARACTER * ( DAT__SZLOC ) PICLOC

*     Name of parameter
      CHARACTER * ( * ) PTYPE

*     Value to write to the parameter
      INTEGER PVAL


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'agi_locs'


*  Local Variables:
      LOGICAL FOUND

      CHARACTER * ( DAT__SZLOC ) PARLOC

*.


      IF ( STATUS .EQ. SAI__OK ) THEN

*   Check if the parameter is present
         PARLOC = ' '
         CALL AGI_1FPAR( PICLOC, PTYPE, PARLOC, FOUND, STATUS )

*   If the structure is not there then create it
         IF ( .NOT. FOUND ) THEN
            CALL DAT_NEW0I( PICLOC, PTYPE, STATUS )
            CALL DAT_FIND( PICLOC, PTYPE, PARLOC, STATUS )
         ENDIF

*   Put value into element
         CALL DAT_PUT0I( PARLOC, PVAL, STATUS )
         CALL DAT_ANNUL( PARLOC, STATUS )
         PARLOC = ' '

*   Indicate that the database has been updated
         FLUSH = .TRUE.

      ENDIF

*      print*, '+++++ AGI_1WPARI +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

