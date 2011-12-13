      SUBROUTINE SUBPAR_SYNC ( STATUS )
*+
*  Name:
*     SUBPAR_SYNC

*  Purpose:
*     exchange synchronisation message with a task.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_SYNC ( STATUS )

*  Description:
*     NOTE: for use only within A- and C-tasks.
*     Locates the calling task, if there is one, then sends a
*     synchronisation message to it, and waits for the reply.

*  Arguments:
*     STATUS=INTEGER

*  Algorithm:
*     Get the path and messid to the task which issued the run command.
*     Send a sync message to the controlling task using ADAM_ACKNOW so
*     that the existing message id is used. Accept the reply.

*  Copyright:
*     Copyright (C) 1985, 1988, 1991, 1993 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     BDK: B D Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     11-NOV-1985 (BDK):
*        Original
*     09-FEB-1988 (BDK):
*        Make RUNFACE an integer
*     25-NOV-1991 (BDK):
*        use ADAM_ACKNOW
*     26-FEB-1993 (AJC):
*        Change MESERRS to MESSYS_ERR
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*      8-MAR-1993 (AJC):
*        Include MESSYS_PAR not MESDEFNS and DDMSG
*        INFINITE with MESSYS__INFINITE
*        and MSG_VAL_LEN with MESSYS__VAL_LEN
*      9-AUG-1993 (AJC):
*        INCLUDE SUBPAR_PARERR not PAR_ERR
*     13-JUN-2001 (AJC)
*        Call AMS (FAMS) directly, not via ADAM
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_ERR'
      INCLUDE 'SUBPAR_PARERR'


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  Local Constants:
      INTEGER TIMEOUT                    ! timeout on receive message
      PARAMETER ( TIMEOUT = MESSYS__INFINITE )
      INTEGER NOCONTEXT
      PARAMETER ( NOCONTEXT = 0 )
      CHARACTER NONAME*(*)
      PARAMETER ( NONAME = ' ' )

*  Local Variables:
      INTEGER USRPATH     !  pointer to the path to the user interface
      INTEGER MESSID      !  message number of the RUN message
      INTEGER MSGSTATUS   !  message status fom GETREPLY
      INTEGER MESLEN      !  message length from GETREPLY
      INTEGER CONTEXT     !  context from GETREPLY (not used)
      CHARACTER*32 NAME   !  name from GETREPLY (not used)

*    Local variables using parameter declared in 'DDMSG' :
      CHARACTER VALUE*(MESSYS__VAL_LEN)  !  value from GETREPLY (not used)

*.


      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   If the task is being run directly at the terminal, there is no need
*   to send a message.
*
      IF ( RUNFACE .NE. SUBPAR__TASK ) RETURN

*
*   Obtain the path to the user interface which initiated this action,
*   along with the message identifier.
*
      USRPATH = RUNPATH
      MESSID = RUNID

      IF ( USRPATH .GT. 0 ) THEN
*
*      construct the message and send it ...
*
          CALL FAMS_REPLY( USRPATH, MESSID, MESSYS__MESSAGE,
     :      MESSYS__SYNC, NOCONTEXT, NONAME, 1, ' ', STATUS )
*
*      wait for reply to this specific message
*
         CALL FAMS_GETREPLY( TIMEOUT, USRPATH, MESSID,
     :     MSGSTATUS, CONTEXT, NAME, MESLEN, VALUE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( MSGSTATUS .NE. MESSYS__SYNCREP ) THEN
               STATUS = MSGSTATUS
            END IF
         END IF

      ELSE

         STATUS = PAR__NOUSR

      ENDIF


      IF ( STATUS .NE. SAI__OK ) THEN
         CALL EMS_FACER( 'STAT', STATUS )
         CALL EMS_REP( 'SUP_SYNC1',
     :      'SUBPAR: ^STAT', STATUS )
         CALL EMS_REP( 'SUP_SYNC2',
     :      'SUBPAR: Error attempting to synchronise', STATUS )
      END IF

      END
