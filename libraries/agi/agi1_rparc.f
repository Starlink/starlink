************************************************************************

      SUBROUTINE AGI_1RPARC ( PICLOC, PTYPE, FOUND, PVAL, STATUS )

*+
*  Name:
*     AGI_1RPARC

*  Purpose:
*     Read the contents of a character parameter.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGI_1RPARC( PICLOC, PTYPE, FOUND, PVAL, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Read the contents of a character parameter

*  Algorithm:
*     Check status on entry.
*     If the parameter is present then
*        Read the contents of the parameter into a temporary string.
*        Equate the output string with the temporary string.

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
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
      INCLUDE 'AGI_PAR'


*  Arguments Given:
*     Locator to picture
      CHARACTER * ( DAT__SZLOC ) PICLOC

*     Name of parameter to be read
      CHARACTER * ( * ) PTYPE


*  Arguments Returned:
*     Flag to indicate if parameter has been found
      LOGICAL FOUND

*     Content of paramter. Undefined if .NOT. FOUND
      CHARACTER * ( * ) PVAL


*  Status:
      INTEGER STATUS


*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) PARLOC
      CHARACTER * ( AGI__CMAX ) CTEMP

*.


      IF ( STATUS .EQ. SAI__OK ) THEN

*   Check if the parameter is present
         PARLOC = ' '
         CALL AGI_1FPAR( PICLOC, PTYPE, PARLOC, FOUND, STATUS )

*   Read contents of element
         IF ( FOUND ) THEN
            CALL DAT_GETC( PARLOC, 0, 0, CTEMP, STATUS )
            PVAL = CTEMP
            CALL DAT_ANNUL( PARLOC, STATUS )
            PARLOC = ' '
         ENDIF

      ENDIF

*      print*, '+++++ AGI_1RPARC +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

