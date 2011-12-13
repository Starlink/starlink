************************************************************************

      SUBROUTINE AGI_IBASE ( PICID, STATUS )

*+
*  Name:
*     AGI_IBASE
*
*  Purpose:
*     Inquire base picture for current device
*
*  Invocation:
*     CALL AGI_IBASE( PICID, STATUS )
*
*  Description:
*     A picture identifier for the base picture on the current device
*     is returned. The picture is not selected as the current picture.
*
*  Arguments:
*     PICID = INTEGER (Returned)
*        Identifier for base picture
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Get the workstation name from the current picture.
*     Search forwards for the base picture starting at the first picture.
*     Get a new picture identifier.
*     Store the picture details in the common blocks.
*
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
*     NE: Nick Eaton (Durham University)
*
*  History:
*     Oct 1988 (NE):
*        Original version
*     Dec 1989 (NE);
*         Added CIDIID
*     Jun 1990 (NE):
*         Added MEMID parameter
*     Sep 1990 (NE):
*         Removed PICID search
*     Nov 1990 (NE):
*         Do not copy parameter name
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'
      INCLUDE 'AGI_ERR'

*  Global variables :
      INCLUDE 'agi_locs'
      INCLUDE 'agi_nest'
      INCLUDE 'agi_pfree'

*  Arguments Returned
      INTEGER PICID

*  Status :
      INTEGER STATUS

*  Local variables :
      INTEGER ISTAT, MEMID, PICNUM

      REAL DEVICE( 4 ), NDC( 4 ), WORLD( 4 )

      CHARACTER * ( AGI__CMAX ) COMENT
      CHARACTER * ( AGI__SZNAM ) PNAME
      CHARACTER * ( DAT__SZNAM ) WKNAME
*.

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Get the name of the workstation for the current picture
         IF ( CURPID .GT. 0 ) THEN
            WKNAME = CAGIWK( CURPID )
         ELSE
            STATUS = AGI__NOCUP
            CALL ERR_REP( 'AGI_IBASE_NOCUP',
     :                    'No current picture', STATUS )
            GOTO 99
         ENDIF

*   Search forwards for the base zone starting at the first picture
         CALL AGI_1SEARP( WKNAME, 'BASE', 'F', 'F', PICNUM, PNAME,
     :                    COMENT, DEVICE, NDC, WORLD, MEMID, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Get a new identifier
*   Use a local status for the free list access
         ISTAT = SAI__OK

*   Get a picture identifier from the free list
         CALL AGI_1FNEXT( FRELEN, FRELIS, NEXFRE, PICID, ISTAT )

*   If the exit status is not OK then the free list is full
         IF ( ISTAT .NE. SAI__OK ) THEN
            STATUS = AGI__NOPID
            CALL ERR_REP( 'AGI_IBASE_NOPID',
     :                    'No more picture identifiers', STATUS )
            GOTO 99
         ELSE
            CNUMID = CNUMID + 1
         ENDIF

*   Store the picture details in the common block
*   Copy some of the details from the current picture
         CAGIWK( PICID ) = WKNAME
         CPICNM( PICID ) = PICNUM
         CGRAWK( PICID ) = CGRAWK( CURPID )
         PICACS( PICID ) = PICACS( CURPID )
         CIDIID( PICID ) = CIDIID( CURPID )
         CLEVEL( PICID ) = CNEST
      ENDIF

*   Flush HDS if database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGI_IBASE +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

