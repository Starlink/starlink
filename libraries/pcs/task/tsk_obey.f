      SUBROUTINE TASK_OBEY ( TASK_NAME, NAME, INVAL, OUTVAL, PATH,
     :  MESSID, STATUS )
*+
*  Name:
*     TASK_OBEY

*  Purpose:
*     Send an OBEY to a task

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_OBEY ( TASK_NAME, NAME, INVAL, OUTVAL, PATH,
*     :  MESSID, STATUS )

*  Description:
*     Send an OBEY command and command-line parameter string to a task.

*  Arguments:
*     TASK_NAME=CHARACTER*(*) (given)
*           the name of the task
*     NAME=CHARACTER*(*) (given)
*           the name of the action
*     INVAL=CHARACTER*(*) (given)
*           the command-line parameter values
*     OUTVAL=CHARACTER*(*) (returned)
*           the string returned from the task
*     PATH=INTEGER (returned)
*           the path to the task
*     MESSID=INTEGER (returned)
*           the message identifier for the OBEY
*     STATUS=INTEGER

*  Algorithm:
*     Get a path to the named task and send it a message.

*  Copyright:
*     Copyright (C) 1987, 1991 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     05-NOV-1987 (REVAD::BDK):
*        Original
*     06-MAY-1991 (REVAD::BDK):
*        Remove DDMSG
*     08-AUG-1991 (REVAD::BDK):
*        Do message forwarding
*     11-JUN-2001 (AJC):
*        Replace ADAM calls with AMS
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADAM_DEFNS'
      INCLUDE 'MESSYS_PAR'
*  Arguments Given:
      CHARACTER*(*) TASK_NAME   ! the name of the task

      CHARACTER*(*) NAME        ! the name of the parameter

      CHARACTER*(*) INVAL       ! the command-line parameter values

*  Arguments Returned:
      CHARACTER*(*) OUTVAL      ! the string returned from the task

      INTEGER PATH              ! the path to the task
      INTEGER MESLEN            ! length of INVAL
      INTEGER MESSID            ! the message identifier for the OBEY

*  Status:
      INTEGER STATUS

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      OUTVAL = ' '
      CALL FAMS_PATH ( TASK_NAME, PATH, STATUS )
      MESLEN = MIN( LEN(INVAL), MESSYS__VAL_LEN )
      CALL FAMS_SEND( PATH, MESSYS__MESSAGE, SAI__OK, OBEY, NAME,
     :  MESLEN, INVAL, MESSID, STATUS )
      CALL TASK_DONE ( MESSYS__INFINITE, PATH, MESSID, OUTVAL, STATUS )


      END
