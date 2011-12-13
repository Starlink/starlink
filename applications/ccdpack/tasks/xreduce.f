      SUBROUTINE XREDUCE( PID, STATUS )
*+
*  Name:
*     XREDUCE

*  Purpose:
*     Starts up the XREDUCE command.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL XREDUCE( PID, STATUS )

*  Arguments:
*     PID = CHARACTER * ( * )
*        The PID string for any process created by the SLV routines
*        (not used in this routine).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine runs up the XREDUCE application. It is necessary
*     as we need to run all commands as task under IRAF/CL. This
*     ensures that we share the same environment (variables) as CL.

*  Usage:
*     RUNXREDUCE

*  ADAM Parameters:
*     NONE

*  Notes:
*     This routine is undocumented. It should work as described in the
*     XREDUCE documentation, except that the process is run in the
*     background, so that it doesn't block other CL interactions.

*  Copyright:
*     Copyright (C) 1997, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     5-JUN-1997 (PDRAPER):
*        Original version.
*     29-AUG-2004 (TIMJ):
*        Use ONE_EXEC rather than CCD1_EXEC
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Status:
      CHARACTER * ( * ) PID     ! Task identifier string
      INTEGER STATUS            ! Global status

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN          ! Used length of string

*  Local Variables:
      CHARACTER * ( 132 ) CCDDIR ! CCDPACK_DIR

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      CALL PSX_GETENV( 'CCDPACK_DIR', CCDDIR, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_OUT( ' ', 'Starting the XREDUCE interface', STATUS )
         CCDDIR = CCDDIR( :CHR_LEN( CCDDIR ) ) // '/xreduce &'
         CALL ONE_EXEC( CCDDIR, STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'XREDUCE_ERR',
     :               'XREDUCE: failed to start XREDUCE.', STATUS )
      END IF

      END
