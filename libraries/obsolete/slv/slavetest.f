      SUBROUTINE SLAVETEST( STATUS )
*+
*  Name:
*     pain

*  Purpose:
*     SLV test ADAM task

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     8-MAY-1997 (RFWS):
*        Original version.
*     {enter_changes_here}

*-

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INTEGER KAPPAPID
      INTEGER NDFPID
      INTEGER SLV_LOADW
      INTEGER STATUS
      INTEGER TIMEOUT
      INTEGER VIEWID
      LOGICAL DETACH

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DETACH = .FALSE.
      TIMEOUT = 15

*  Load tasks.
      NDFPID = SLV_LOADW( 'NDFPACK', 'ndfpack_mon', DETACH, TIMEOUT,
     :                    STATUS )
      KAPPAPID = SLV_LOADW( 'KAPPA', 'kappa_mon', DETACH, TIMEOUT,
     :                      STATUS )
      VIEWID = SLV_LOADW( 'KAPVIEW', 'kapview_mon', DETACH, TIMEOUT,
     :                    STATUS )

*  Run the applications.
      CALL SLV_OBEYW( 'NDFPACK', 'ndftrace', 'prompt', 'NDF<NDF',
     :                STATUS )
      CALL SLV_OBEYW( 'KAPPA', 'stats', 'prompt', 'NDF<NDF',
     :                STATUS )
      CALL SLV_OBEYW( 'KAPVIEW', 'display', 'prompt',
     :                'IN<NDF,MODE<MODE,PERCENTILES<PERCENTILES',
     :                STATUS )

*  You could kill the tasks here (like this), but they die anyway when
*  the main application task terminates.
c      CALL SLV_KILL( NDFPID, STATUS )
c      CALL SLV_WAITK( NDFPID, 10, STATUS )

*  Report a contextual error message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SLAVETEST_ERR',
     :                 'SLAVETEST: Error occurred in test program.',
     :                 STATUS )
      END IF
      END
