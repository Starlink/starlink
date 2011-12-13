************************************************************************

      SUBROUTINE AGI_1PNEW ( WKNAME, PNAME, COMENT, DEVICE, NDC,
     :                       WORLD, MEMID, PICNUM, STATUS )

*+
*  Name:
*     AGI_1PNEW

*  Purpose:
*     Save current picture parameters.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGI_1PNEW( WKNAME, PNAME, COMENT, DEVICE, NDC, WORLD,

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Save current picture parameters in the database and in the cache

*  Algorithm:
*     Check status on entry.
*     Get a locator to the top level database structure.
*     Get a locator to the workstation.
*     Inquire how many pictures are on the workstation.
*     Make the picture number one greater than the number of pictures.
*     Create a new picture containing the passed parameters.
*     Indicate this is the active picture.
*     Write this picture into the cache.

*  Copyright:
*     Copyright (C) 1988, 1989, 1990, 1993 Science & Engineering Research Council.
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
*     Aug 1988
*     Jul 1989  Read database locator from common block
*     Nov 1989  PNAME - Remove leading blanks and change to upper case
*     Jun 1990  Added MEMID parameter
*     Sep 1990  Removed inquiries to the number of pictures
*     Jan 1993  Initialise header block if necessary
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
*     Name of workstation
      CHARACTER * ( * ) WKNAME

*     Name of picture
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


*  Global Variables:
      INCLUDE 'agi_cache'


*  Local Variables:
      INTEGER POINT

      CHARACTER * ( AGI__SZNAM ) LNAME

*.


*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Use a local variable to manipulate the name string.
*   Remove leading blanks and change to upper case
         LNAME = PNAME
         CALL CHR_LDBLK( LNAME )
         CALL CHR_UCASE( LNAME )

*   Write the information into the database
         CALL AGI_1WPIC( WKNAME, LNAME, COMENT, DEVICE, NDC, WORLD,
     :                   MEMID, PICNUM, STATUS )

*   Indicate this is the active picture
         CALL AGI_1WPACT( WKNAME, PICNUM, STATUS )

*   Write the information into the cache
         CALL AGI_1WCACH( WKNAME, PICNUM, LNAME, COMENT, DEVICE, NDC,
     :                    WORLD, MEMID, POINT, STATUS )

*   Remember the number of pictures on this device
         IF ( CNUMPW .NE. WKNAME ) CHEAD = -1
         CNUMPW = WKNAME
         CNUMPS = PICNUM

      ENDIF

*      print*, '+++++ AGI_1PNEW +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

