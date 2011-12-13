************************************************************************

      SUBROUTINE AGI_1WPARS ( PICLOC, PNAME, COMENT, DEVICE, NDC, WORLD,
     :                        MEMID, STATUS )

*+
*  Name:
*     AGI_1WPARS

*  Purpose:
*     Write all the parameters into a picture structure.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGI_1WPARS ( PICLOC, PNAME, COMENT, DEVICE, NDC, WORLD,

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Write all the parameters into a picture structure

*  Algorithm:
*     Check status on entry.
*     Write each of the parameters in turn.

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
*     Oct 1988
*     Jun 1990  Added MEMID parameter
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

*     Name of picture
      CHARACTER * ( * ) PNAME

*     Description of picture contents
      CHARACTER * ( * ) COMENT

*     Array of device coordinates
      REAL DEVICE( 4 )

*     Array of normalised device coordinates
      REAL NDC( 4 )

*     Array of world coordinates
      REAL WORLD( 4 )

*     Memory identifier
      INTEGER MEMID


*  Status:
      INTEGER STATUS

*.


      IF ( STATUS .EQ. SAI__OK ) THEN

*   Write the parameters into the picture structure
         CALL AGI_1WPARC( PICLOC, 'PNAME', PNAME, STATUS )
         CALL AGI_1WPARC( PICLOC, 'COMENT', COMENT, STATUS )
         CALL AGI_1WARPR( PICLOC, 'DEVICE', DEVICE, STATUS )
         CALL AGI_1WARPR( PICLOC, 'NDC', NDC, STATUS )
         CALL AGI_1WARPR( PICLOC, 'WORLD', WORLD, STATUS )
         CALL AGI_1WPARI( PICLOC, 'MEMID', MEMID, STATUS )

      ENDIF

*      print*, '+++++ AGI_1WPARS +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

