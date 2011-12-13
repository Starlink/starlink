************************************************************************

      SUBROUTINE AGD_1IIDIP ( MEMID, XSCRL, YSCRL, ZOOMF, STATUS )

*+
*  Name:
*     AGD_1IIDIP

*  Purpose:
*     Recover IDI parameters from the current workstation.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGD_1IIDIP( MEMID, XSCRL, YSCRL, ZOOMF, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Get the IDI memory zoom and scroll factors for the current
*     workstation from the common block or from the database.

*  Algorithm:
*     Initialise the returned values.
*     Check status on entry.
*     Verify the memory identifier.
*     Get the details of the current picture.
*     If the current display identifier mathces the one in the
*     common blocks then
*        Get the returned values from the common block.
*     Else
*        Open the database and get a locator to the workstation
*        structure.
*        If there is an IDI parameter structure and
*        it has the correct dimensions then
*           Read the values from the structure.
*        Endif
*     Endif

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
*     June 1990
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
      INCLUDE 'AGI_ERR'


*  Arguments Given:
*     IDI memory identifier
      INTEGER MEMID


*  Arguments Returned:
*     X-scoll
      INTEGER XSCRL

*     Y-scroll
      INTEGER YSCRL

*     Zoom factor
      INTEGER ZOOMF


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'agi_idips'
      INCLUDE 'agi_locs'
      INCLUDE 'agi_pfree'


*  Local Variables:
      LOGICAL FOUND, YESNO

      INTEGER DISPID, IDIMS( 2 ), IDIPAR( 3, MXMEMS ), J, NDIM

      CHARACTER * ( DAT__SZLOC ) ISTLOC, WKSLOC
      CHARACTER * ( DAT__SZNAM ) WKNAME

*.


*   Initialise the returned values
      XSCRL = 0
      YSCRL = 0
      ZOOMF = 0

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Verify the memory identifier
      IF ( ( MEMID .LT. 0 ) .OR. ( MEMID .GE. MXMEMS ) )THEN
         STATUS = AGI__MEMIN
         CALL MSG_SETI( 'IVAL', MEMID )
         CALL ERR_REP( 'AGD_1IIDIP_MMIN', 'IDI memory ^IVAL invalid',
     :                 STATUS )
         GOTO 99
      ENDIF

*   Get details of the current picture
      IF ( CURPID .GT. 0 ) THEN
         WKNAME = CAGIWK( CURPID )
         DISPID = CIDIID( CURPID )
      ELSE
         STATUS = AGI__NOCUP
         CALL ERR_REP( 'AGD_1IIDIP_NOCP', 'No current picture', STATUS )
         GOTO 99
      ENDIF

*   If the current device and the device of the current picture match
*   then get the values from the common blocks and return
      IF ( DISPID .EQ. CDIPID ) THEN
         XSCRL = CXSCRL( MEMID )
         YSCRL = CYSCRL( MEMID )
         ZOOMF = CZOOMF( MEMID )
         GOTO 99
      ENDIF

*   Have to get the IDI parameters from the database
*   Get a locator to the workstation structure
      CALL AGI_1FDB( FOUND, STATUS )
      IF ( FOUND ) THEN
         WKSLOC = ' '
         CALL AGI_1FWORK( WKNAME, WKSLOC, FOUND, STATUS )
      ENDIF

*   If the workstation structure was found then continue
      IF ( FOUND ) THEN

*   See if the IDIPAR structure is present
         CALL DAT_THERE( WKSLOC, AGI__IDNAM, YESNO, STATUS )
         IF ( YESNO ) THEN
            ISTLOC = ' '
            CALL DAT_FIND( WKSLOC, AGI__IDNAM, ISTLOC, STATUS )

*   Check that it has the correct dimensions
            CALL DAT_SHAPE( ISTLOC, 2, IDIMS, NDIM, STATUS )
            IF ( ( NDIM .LT. 1 ) .OR. ( NDIM .GT. 2 ) .OR.
     :           ( IDIMS( 1 ) .NE. 3 ) ) THEN
               STATUS = AGI__DINER
               CALL ERR_REP( 'AGI_1IIDIP_DINE',
     :                       'Database integrity error', STATUS )
               GOTO 98
            ENDIF

*   Read the parameters
            IF ( IDIMS( 2 ) .GT. MXMEMS ) THEN
               IDIMS( 2 ) = MXMEMS
            ENDIF
            CALL DAT_GETI( ISTLOC, NDIM, IDIMS, IDIPAR, STATUS )
            J = MEMID + 1
            ZOOMF = IDIPAR( 1, J )
            XSCRL = IDIPAR( 2, J )
            YSCRL = IDIPAR( 3, J )

*   Annul the locators
  98        CONTINUE
            CALL DAT_ANNUL( ISTLOC, STATUS )
            ISTLOC = ' '
         ENDIF
         CALL DAT_ANNUL( WKSLOC, STATUS )
         WKSLOC = ' '

*   Otherwise report an error
      ELSE
         STATUS = AGI__WKSNF
         CALL ERR_REP( 'AGD_1IIDIP_WKNF', 'Workstation not found',
     :                 STATUS )
         GOTO 99
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGD_1IIDIP +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

