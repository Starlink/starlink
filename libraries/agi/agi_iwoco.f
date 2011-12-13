************************************************************************

      SUBROUTINE AGI_IWOCO ( WX1, WX2, WY1, WY2, STATUS )

*+
*  Name:
*     AGI_IWOCO
*
*  Purpose:
*     Inquire world coordinates of current picture
*
*  Invocation:
*     CALL AGI_IWOCO( WX1, WX2, WY1, WY2, STATUS )
*
*  Description:
*     Return the world coordinate limits of the current picture.
*
*  Arguments:
*     WX1 = REAL (Returned)
*        World coordinate of left edge of picture
*     WX2 = REAL (Returned)
*        World coordinate of right edge of picture
*     WY1 = REAL (Returned)
*        World coordinate of bottom edge of picture
*     WY2 = REAL (Returned)
*        World coordinate of top edge of picture
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Get details of the current picture.
*     Read the contents of the current picture.
*
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
*     NE: Nick Eaton (Durham University)
*
*  History:
*     Oct 1988 (NE):
*        Original version
*     Jun 1990 (NE):
*        Added MEMID parameter
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
      INCLUDE 'agi_pfree'

*  Arguments Returned :
      REAL WX1
      REAL WX2
      REAL WY1
      REAL WY2

*  Status :
      INTEGER STATUS

*  Local variables :
      LOGICAL FOUND

      INTEGER MEMID, PICNUM

      REAL DEVICE( 4 ), NDC( 4 ), WORLD( 4 )

      CHARACTER * ( AGI__CMAX ) COMENT
      CHARACTER * ( AGI__SZNAM ) PNAME
      CHARACTER * ( DAT__SZNAM ) WKNAME
*.

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Get details of the current picture
         IF ( CURPID .GT. 0 ) THEN
            WKNAME = CAGIWK( CURPID )
            PICNUM = CPICNM( CURPID )
         ELSE
            STATUS = AGI__NOCUP
            CALL ERR_REP( 'AGI_IWOCO_NOCUP', 'No current picture',
     :                    STATUS )
            GOTO 99
         ENDIF

*   Read the contents of the current picture
         CALL AGI_1RPIC( WKNAME, PICNUM, PNAME, COMENT, DEVICE, NDC,
     :                   WORLD, MEMID, FOUND, STATUS )

*   If picture is not there then indicate an error
         IF ( .NOT. FOUND ) THEN
            STATUS = AGI__PICNF
            CALL ERR_REP( 'AGI_IWOCO_PICNF', 'Picture not found',
     :                    STATUS )
            GOTO 99

*   Otherwise transfer the world coordinates to the ouput arguments
         ELSE
            WX1 = WORLD( 1 )
            WX2 = WORLD( 2 )
            WY1 = WORLD( 3 )
            WY2 = WORLD( 4 )
         ENDIF
      ENDIF

*   Flush HDS if database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGI_IWOCO +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

