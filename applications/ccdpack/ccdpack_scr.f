      SUBROUTINE CCDPACK_SCR( STATUS )
*+
*  Name:
*     CCDPACK_SCR

*  Purpose:
*     Top-level monolith for super applications (script) routines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCDPACK( STATUS )

*  Description:
*     This routine gets the action name directly from the Unix kernel.
*     It then calls the appropriate routine to perform the
*     specified action. An error will be reported and STATUS will
*     be set if the action name is not recognized.
*
*     If this is called as a detached subroutine (from say ICL or CL)
*     then the extension that should be used to attached to any existing
*     subroutines is determined and passed to each action. These should
*     append this string to any task names they use (this should make
*     them reuse any monoliths already loaded).

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JUN-1997 (PDRAPER):
*        Original version.
*     06-JUL-2020 (GSB):
*        Update KPG1_LGCMD call adding new CPUTIM argument.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'PAR_PAR'         ! Parameter system constants
      INCLUDE 'CCD1_PAR'        ! VERS value.
      INCLUDE 'PSX_ERR'         ! PSX error codes
      INCLUDE 'PRM_PAR'         ! Primitive data constants

*  Status:
      INTEGER STATUS            ! Global status

*  Local variables:
      CHARACTER * ( 132 ) ENV   ! Environment variable value
      CHARACTER * ( VAL__SZI ) PID ! PID for any process
      CHARACTER * ( PAR__SZNAM ) ACTION ! Action name
      INTEGER NVAL              ! Number of characters
      INTEGER CPUTIM( 4 )       ! Context info for KPG1_CPUTM

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the action name.
      CALL TASK_GET_NAME( ACTION, STATUS )

*  Determine the environment that we are running in. This is decided by
*  the following rules. If the variable ICL_TASK_NAME is set it
*  specifies the name of this monolith as known to the ADAM message
*  system. If this takes the form ccdpack_scrXXXX (where XXXX is an
*  integer, the parent process id), then we are running from ICL. If
*  this name takes the form ccdpack_scr_XXXX then we are running from the
*  CL adaptor process. In either case the tasks that we create or attach
*  too are also of this form (actually we don't care we just append the
*  bit after the name ccdpack_scr to any new tasks). TCL detached tasks use
*  a user defined naming scheme that we will need to define.
      CALL ERR_MARK
      PID = ' '
      CALL PSX_GETENV( 'ICL_TASK_NAME', ENV, STATUS )
      IF ( STATUS .EQ. PSX__NOENV ) THEN

*  We are not a subprocess.
         CALL ERR_ANNUL( STATUS )
      ELSE

*  Running as a subprocess of some command language.
         NVAL = INDEX( ENV, 'ccdpack_scr' )
         IF ( NVAL .NE. 0 ) THEN
            PID = ENV( NVAL + 11: )
         END IF
      END IF
      CALL ERR_RLSE

*  Tweak the numerics on a RedHat 7 Linux system.
      CALL CCD1_LINFLT

*  Record the current CPU time in CPUTIM.
      CALL KPG1_CPUTM( CPUTIM, VAL__BADD )

*  Test the action name against each valid value in turn, calling the
*  appropriate routine...
      IF ( ACTION .EQ. 'CCDALIGN' ) THEN
         CALL CCDALIGN( PID, STATUS )

      ELSE IF ( ACTION .EQ. 'REDUCE' ) THEN
         CALL REDUCE( PID, STATUS )

      ELSE IF ( ACTION .EQ. 'XREDUCE' ) THEN
         CALL XREDUCE( PID, STATUS )

*  If the action name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'ACTION', ACTION )
         CALL ERR_REP( 'CCDPACK_ERR',
     :                 'CCDPACK: The action name ''^ACTION'' is ' //
     :                 'not recognised by the CCDPACK '//
     :                 'monolith.', STATUS )
      END IF

*  Log the task and its parameters to a log file specified by enviromnent
*  variable CCDPACK_LOG.
      CALL KPG1_LGCMD( ACTION, 'CCDPACK', CPUTIM, STATUS )

      END
