      SUBROUTINE AGP_CURAG( AGINAM, STATUS )
*+
*  Name:
*     AGP_CURAG
*
*  Purpose:
*     Return the AGI workstation name for the currently opened PGPLOT
*     device.
*
*  Invocation:
*     CALL AGP_CURAG( AGINAM, STATUS )
*
*  Description:
*     This routine returns the AGI workstation name for the currently
*     opened PGPLOT device, or a blank string if PGPLOT is not currently
*     open.
*
*  Arguments:
*     AGINAM = CHARACTER*(*) (Returned)
*        AGI name for the PGPLOT device
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*
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
*     DSB: David Berry (Starlink)
*
*  History:
*     2-NOV-2001 (DSB):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Returned:
      CHARACTER AGINAM*(*)

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER DEVNAM*60          ! PGPLOT device
      CHARACTER STRING*6           ! PGPLOT state
      INTEGER LENSTR               ! Used length of a string
*.

*  Initialize
      AGINAM = ' '

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Check to see if PGPLOT is open.
      CALL PGQINF( 'STATE', STRING, LENSTR )
      IF ( STRING( :LENSTR ) .NE. 'CLOSED' ) THEN

*   If so, find out the current workstation name
         CALL PGQINF( 'TYPE', DEVNAM, LENSTR )

*   Convert this into an AGI name
         CALL AGI_1GWNAM( DEVNAM( :LENSTR ), 'GKS', AGINAM, STATUS )

      END IF

      END
