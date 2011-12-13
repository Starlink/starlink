************************************************************************

      SUBROUTINE AGI_1OPIC ( WKSLOC, PICNUM, PICLOC, STATUS )

*+
*  Name:
*     AGI_1OPIC

*  Purpose:
*     Open a picture structure.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGI_1OPIC( WKSLOC, PICNUM, PICLOC, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Open a picture in the picture structure

*  Algorithm:
*     Check status on entry
*     Inquire the number of pictures in the picture structure.
*     If the picture structure does not exist then
*        Create new picture structure and get a locator to it.
*     Else
*        Increase the number of pictures by one.
*     Endif
*     Get a locator to the new picture.
*     Annul the picture structure locator.

*  Copyright:
*     Copyright (C) 1988, 1989, 1990 Science & Engineering Research Council.
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
*     Jul 1988
*     Dec 1989  Added label structure
*     Sep 1990  Changed PICNUM to an export argument
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
      INCLUDE 'AGI_PAR'


*  Arguments Given:
*     Locator to workstation
      CHARACTER * ( DAT__SZLOC ) WKSLOC


*  Arguments Returned:
*     Number of picture in workstation structure.
      INTEGER PICNUM

*     Locator to newly created picture
      CHARACTER * (DAT__SZLOC ) PICLOC


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'agi_locs'


*  Local Variables:
      LOGICAL FOUND

      INTEGER SIZE

      CHARACTER * ( DAT__SZLOC ) LABLOC, LSTLOC, PSTLOC

*.


*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Find out how many pictures are in the workstation entry
         PSTLOC = ' '
         CALL AGI_1IPIC( WKSLOC, PSTLOC, SIZE, FOUND, STATUS )

*   If there is no picture structure then create one with one picture
*   as long as picnum is valid.
         IF ( .NOT. FOUND ) THEN
            PICNUM = 1
            CALL DAT_NEW( WKSLOC, AGI__PCNAM, AGI__PCNAM, 1, 1,
     :                    STATUS )
            CALL DAT_FIND( WKSLOC, AGI__PCNAM, PSTLOC, STATUS )

*   Also create the 'pactive' structure
            CALL DAT_NEW( WKSLOC, AGI__ACNAM, '_INTEGER', 0, 0,
     :                    STATUS )

*   Also create the 'labels' structure
            CALL DAT_NEW1C( WKSLOC, AGI__LANAM, AGI__SZLAB, 1,
     :                      STATUS )
            LSTLOC = ' '
            CALL DAT_FIND( WKSLOC, AGI__LANAM, LSTLOC, STATUS )

         ELSE
*   Extend the number of pictures by one
            PICNUM = SIZE + 1
            CALL DAT_ALTER( PSTLOC, 1, PICNUM, STATUS )

*   Also extend the size of the label structure
            LSTLOC = ' '
            CALL DAT_FIND( WKSLOC, AGI__LANAM, LSTLOC, STATUS )
            CALL DAT_ALTER( LSTLOC, 1, PICNUM, STATUS )
         ENDIF

*   Get a locator to the picture
         PICLOC = ' '
         CALL DAT_CELL( PSTLOC, 1, PICNUM, PICLOC, STATUS )
         CALL DAT_ANNUL( PSTLOC, STATUS )
         PSTLOC = ' '

*   Fill the new label element with blanks
         LABLOC = ' '
         CALL DAT_CELL( LSTLOC, 1, PICNUM, LABLOC, STATUS )
         CALL DAT_PUT0C( LABLOC, '               ', STATUS )
         CALL DAT_ANNUL( LSTLOC, STATUS )
         LSTLOC = ' '
         CALL DAT_ANNUL( LABLOC, STATUS )
         LABLOC = ' '

*   Indicate that the database has been updated
         FLUSH = .TRUE.

      ENDIF

*      print*, '+++++ AGI_1OPIC +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

