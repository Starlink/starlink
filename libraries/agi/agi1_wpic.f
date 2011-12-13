************************************************************************

      SUBROUTINE AGI_1WPIC ( WKNAME, PNAME, COMENT, DEVICE, NDC, WORLD,
     :                       MEMID, PICNUM, STATUS )

*+
*  Name:
*     AGI_1WPIC

*  Purpose:
*     Fill a picture.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGI_1WPIC( WKNAME, PNAME, COMENT, DEVICE, NDC, WORLD,

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Write the picture descriptors into the database and make the given
*     picture the current picture.

*  Algorithm:
*     Check status on entry.
*     Ensure a picture structure exists.
*     Put the parameters into the picture structure.

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
*     June 1990  Added MEMID parameter
*     September 1990  Changed PICNUM to an export argument
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
*     Name of workstation
      CHARACTER * ( * ) WKNAME

*     Picture name
      CHARACTER * ( * ) PNAME

*     Description of picture
      CHARACTER * ( * ) COMENT

*     Device coordinates of picture
      REAL DEVICE( 4 )

*     Normalised device coordinates of picture
      REAL NDC( 4 )

*     World coordinates of picture
      REAL WORLD( 4 )

*     Memory identifier
      INTEGER MEMID


*  Arguments Returned:
*     Number of picture in array of pictures
      INTEGER PICNUM


*  Status:
      INTEGER STATUS


*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) PICLOC, WKSLOC

*.


      IF ( STATUS .EQ. SAI__OK ) THEN

*   Check picture structure is present
         CALL AGI_1ODB( STATUS )
         WKSLOC = ' '
         CALL AGI_1OWORK( WKNAME, WKSLOC, STATUS )
         PICLOC = ' '
         CALL AGI_1OPIC( WKSLOC, PICNUM, PICLOC, STATUS )

*   Fill elements with passed parameters
         CALL AGI_1WPARS( PICLOC, PNAME, COMENT, DEVICE, NDC, WORLD,
     :                    MEMID, STATUS )

         CALL DAT_ANNUL( PICLOC, STATUS )
         PICLOC = ' '
         CALL DAT_ANNUL( WKSLOC, STATUS )
         WKSLOC = ' '

      ENDIF

*      print*, '+++++ AGI_1WPIC +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

