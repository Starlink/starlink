************************************************************************

      SUBROUTINE AGD_DEACT ( STATUS )

*+
*  Name:
*     AGD_DEACT
*
*  Purpose:
*     Close down IDI
*
*  Invocation:
*     CALL AGD_DEACT( STATUS )
*
*  Description:
*     Close down IDI whatever the value of status. This should be called
*     after all AGD and IDI routines.
*
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Make sure the database contains the latest zoom and scroll factors.
*     Close all the devices that have been opened by AGD.
*     Clear the IDI active flag.
*     Clear the IDI-related common blocks.
*
*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
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
*     June 1990 (NE):
*        Original version
*     April 1991 (NE):
*        Return error if interface is not active
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'
      INCLUDE 'AGI_ERR'

*  Global variables :
      INCLUDE 'agi_idips'
      INCLUDE 'agi_locs'
      INCLUDE 'agi_pfree'

*  Status :
      INTEGER STATUS

*  Local variables :
      INTEGER DISPID, ISTAT, J, K
*.

*   Signal an error if the interface is not active
      IF ( .NOT. CIDION ) THEN
         STATUS = AGI__GRPNA
         CALL ERR_REP( 'AGD_DEACT_GRPNA', 'Graphics package not active',
     :                 STATUS )
         GOTO 99
      ENDIF

*   Update the zoom and scrolls of the current device
      ISTAT = 0
      CALL AGD_UPDAT( ISTAT )
      CALL AGD_1SIDIP( ISTAT )

*   Close those devices that have entries in the common block
      DO 20 J = 1, FRELEN
         IF ( CIDIID( J ) .GT. 0 ) THEN
            DISPID = CIDIID( J )
            CALL IIDCLO( DISPID, ISTAT )

*   Remove all references to this device from the common block
            DO 10 K = J, FRELEN
               IF ( CIDIID( K ) .EQ. DISPID ) THEN
                  CIDIID( K ) = 0
               ENDIF
  10        CONTINUE
         ENDIF
  20  CONTINUE

*   Reset the graphics package flag
      CIDION = .FALSE.

*   Clear out the IDI parameter common blocks
      ISTAT = 0
      CALL AGD_1IINIT( ISTAT )

*   Flush HDS if the database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGD_DEACT +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

