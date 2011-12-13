************************************************************************

      SUBROUTINE AGI_1OWORK ( WKNAME, WKSLOC, STATUS )

*+
*  Name:
*     AGI_1OWORK

*  Purpose:
*     Open workstation structure.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGI_1OWORK ( WKNAME, WKSLOC, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Open a workstation structure

*  Algorithm:
*     Check status on entry.
*     Find out if the workstation is already open.
*     If it is not then
*        Create a new workstation structure and get a locator to it.
*     Endif

*  Copyright:
*     Copyright (C) 1988, 1989, 1993 Science & Engineering Research Council.
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
*     Amended January 1993  Create header structure
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
*     Name of workstation
      CHARACTER * ( * ) WKNAME


*  Arguments Returned:
*     Locator to workstation
      CHARACTER * ( DAT__SZLOC ) WKSLOC


*  Status:
      INTEGER STATUS

*    Global variables
      INCLUDE 'agi_locs'


*  Local Variables:
      LOGICAL FOUND

      CHARACTER * ( DAT__SZLOC ) HENLOC

*.


      IF ( STATUS .EQ. SAI__OK ) THEN

*   Check if workstation is already open
         WKSLOC = ' '
         CALL AGI_1FWORK( WKNAME, WKSLOC, FOUND, STATUS )

*   If there is no workstation then create it and get locator
         IF ( .NOT. FOUND ) THEN
            CALL DAT_NEW( DABLOC, WKNAME, AGI__WKTYP, 0, 0, STATUS )
            CALL DAT_FIND( DABLOC, WKNAME, WKSLOC, STATUS )

*   Create the workstation header block and intialise it to zero
            HENLOC = ' '
            CALL DAT_NEW0I( WKSLOC, AGI__HENAM, STATUS )
            CALL DAT_FIND( WKSLOC, AGI__HENAM, HENLOC, STATUS )
            CALL DAT_PUT0I( HENLOC, 0, STATUS )
            CALL DAT_ANNUL( HENLOC, STATUS )
            HENLOC = ' '

*   Indicate that the database has been updated
            FLUSH = .TRUE.

         ENDIF

      ENDIF

*      print*, '+++++ AGI_1OWORK +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

