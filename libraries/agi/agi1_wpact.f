************************************************************************

      SUBROUTINE AGI_1WPACT ( WKNAME, VALUE, STATUS )

*+
*  Name:
*     AGI_1WPACT

*  Purpose:
*     Indicate which is the currently active picture.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGI_1WPACT( WKNAME, VALUE, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Indicate which is the currently active picture.

*  Algorithm:
*     Check status on entry.
*     Get a locator to the top level database structure.
*     Get a loctor to the workstation.
*     Get a locator to the picture active element.
*     Store the current active picture in here.
*     Tidy up

*  Copyright:
*     Copyright (C) 1988, 1989 Science & Engineering Research Council.
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
*     Amended July 1989  Read database locator from common block
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
*     Name of the workstation
      CHARACTER * ( * ) WKNAME

*     Number of currently active picture
      INTEGER VALUE


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'agi_locs'


*  Local Variables:
      LOGICAL FOUND

      CHARACTER * ( DAT__SZLOC ) TMPLOC, WKSLOC

*.


*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Find the workstation
         CALL AGI_1FDB( FOUND, STATUS )
         IF ( FOUND ) THEN
            WKSLOC = ' '
            CALL AGI_1FWORK( WKNAME, WKSLOC, FOUND, STATUS )
            IF ( FOUND ) THEN

*   Find the 'pactive' structure
               TMPLOC = ' '
               CALL DAT_THERE( WKSLOC, AGI__ACNAM, FOUND, STATUS )
               IF ( FOUND ) THEN
                  CALL DAT_FIND( WKSLOC, AGI__ACNAM, TMPLOC, STATUS )

*   Put the new active number into the database
                  CALL DAT_PUTI( TMPLOC, 0, 0, VALUE, STATUS )
                  CALL DAT_ANNUL( TMPLOC, STATUS )
                  TMPLOC = ' '

*   Indicate that the database has been updated
                  FLUSH = .TRUE.
               ENDIF
               CALL DAT_ANNUL( WKSLOC, STATUS )
               WKSLOC = ' '

            ENDIF
         ENDIF
      ENDIF

*      print*, '+++++ AGI_1WPACT +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

