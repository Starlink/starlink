      PROGRAM MAINTASK
*+
*  Name:
*     MAINTASK

*  Purpose:
*     This is the main routine for a UNIX ADAM task for running from ICL

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     The task is invoked in the normal way for running tasks from ICL

*  Description:
*     Start the ERR system (switches on message deferral).
*     Enable MSG tuning by users.
*     Tune the MAXWPL parameter of HDS
*     Determine if the task has been started from ICL (ie if the environment
*     variable ICL_TASK_NAME has been defined) and call DTASK_DTASK
*     or DTASK_DCLTASK as appropriate. ICL_TASK_NAME should be set by
*     ICL to the name by which it wants the task to register with the
*     message system.
*     Close down the ERR system (It is expected that all messages have
*     already been output.
*     Then, if STATUS and environment variable ADAM_EXIT are set, exit
*     with system status set to 1; otherwise exit normally.

*  Copyright:
*     Copyright (C) 1991, 1993-1994 Science & Engineering Research
*     Council.
*     Copyright (C) 1996, 1998 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     Copyright (C) 2016 East Asian Observatory.
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
*     AJC: A J Chipperfield (STARLINK)
*     MJC: Malcolm J. Currie (JAC, Hawaii)
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     8-JUL-1991 (AJC):
*        Original version.
*     17-MAY-1993 (AJC):
*        Modify for ICL or shell running
*     23-AUG-1993 (AJC):
*        Change name of environment variable to ICL_TASK_NAME
*     16-FEB-1994 (AJC):
*        Tune HDS_SHELL
*     02-DEC-1994 (BKM):
*       Add DTS_SETSIG
*     24-SEP-1996 (AJC):
*       Change HDS shell value from 2 (tcsh) to 1 (csh) as HDS doesn't
*       fall through to lower numbers if shell unavailable
*     14-SEP-1998 (AJC):
*       Indicate success or failure on exit (subject to ADAM_EXIT set).
*     2011 October 18 (MJC):
*       Added MSG_TUNE to permit user control of MSG tuning parameters.
*     4-MAY-2016 (DSB):
*       Remove the code that caused the default HDS shell to be changed
*       to csh. The default HDS shell is now sh (modern sh derived
*       shells support "~" expansion).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSX_ERR'

*  External References:
      EXTERNAL DEVINIT           ! Dummy task initialisation
      EXTERNAL DTASK_APPLIC      ! User code calling routine

*  Local Constants:
      INTEGER PAGE_NUM           ! HDS working page limit
      PARAMETER ( PAGE_NUM = 100 )

*  Local Variables:
      INTEGER STATUS             ! Task status
      CHARACTER*80 ICLID         ! If we were started by ICL this local
                                 ! environment variable will be inherited
*.

*  Initialise Memory Routines
      CALL STARMEM_INIT()

*  Initialise error system
      CALL ERR_START

*  Initialise task status
      STATUS = SAI__OK

*  Permit users to set MSG tuning via environment variables.
      CALL MSG_TUNE( 'ENVIRONMENT', 0, STATUS )

*  Increase HDS's maximum working page list size (MAXWPL) from the default
      CALL HDS_TUNE ( 'MAXWPL', PAGE_NUM, STATUS )

*  Attempt to get the ICL environment variable
*  Be prepared for 'normal' situation of failure
      CALL ERR_MARK
      CALL PSX_GETENV( 'ICL_TASK_NAME', ICLID, STATUS )

*  If it's set we are running from ICL
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL ERR_RLSE
*  Setup Unix signal handling
         CALL DTASK_SETSIG( .TRUE. )
         CALL DTASK_DTASK ( DEVINIT, DTASK_APPLIC, STATUS )

*  Otherwise we are running from the shell
      ELSE IF ( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL ERR_RLSE
*  Setup Unix signal handling
         CALL DTASK_SETSIG( .FALSE. )
         CALL DTASK_DCLTASK ( DEVINIT, DTASK_APPLIC, STATUS )

      ENDIF

*   Close error reporting
      CALL ERR_STOP ( STATUS )

*   Exit
*   If STATUS and environment variable ADAM_EXIT are set,
*   make an error exit. We have no further use for STATUS or error
*   reporting.
      IF ( STATUS .NE. SAI__OK ) THEN
*    We have a bad status
         STATUS = SAI__OK
         CALL ERR_MARK
         CALL PSX_GETENV( 'ADAM_EXIT', ICLID, STATUS )
*      If ADAM_EXIT is set, exit with status 1
         IF ( STATUS .EQ. SAI__OK ) CALL EXIT( 1 )
      ENDIF

*   Otherwise exit normally

      END

      SUBROUTINE DEVINIT ( STATUS )
*+
*  Name:
*     DEVINIT

*  Purpose:
*     Dummy initialisation routine for ADAM tasks.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Description:
*     This is a dummy initialisation routine for ADAM tasks. It will be
*     used if the programmer does not supply one.

*  Arguments:
*     STATUS=INTEGER

*  Algorithm:
*     Return.

*  Copyright:
*     Copyright (C) 1989, 1991 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     W.F.Lupton (AAOEPP::WFL)
*     {enter_new_authors_here}

*  History:
*     30-APR-1989 (AAOEPP::WFL):
*        Original
*     09-APR-1991 (REVAD::BDK):
*        Call SUBPAR_SETCHECK
*     25-APR-1991 (REVAD::BDK):
*        Check status on entry
*     14-MAY-1991 (ROE::BMC):
*        Remove NEEDS list checking control
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
*  Status:
      INTEGER STATUS
*.
      IF ( STATUS .NE. SAI__OK ) RETURN

      END
