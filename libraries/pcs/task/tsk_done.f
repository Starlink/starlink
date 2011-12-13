      SUBROUTINE TASK_DONE ( TIMEOUT, PATH, MESSID, OUTVAL, STATUS )
*+
*  Name:
*     TASK_DONE

*  Purpose:
*     Wait for final acknowledgement from task

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_DONE ( TIMEOUT, PATH, MESSID, OUTVAL, STATUS )

*  Description:
*     Wait for a final acknowledgement from a task executing a
*     GET/SET/OBEY/CANCEL.
*     The routine will return when the required message arrives, or if
*     it times-out, or if there is an EXTINT event. Requests from the
*     task for parameter prompts or output of messages associated with
*     the action are automatically forwarded to the user interface.
*     Parameter values sent by the user interface are forwarded to the
*     task.

*  Arguments:
*     TIMEOUT=INTEGER (given)
*           timeout in millisecs. -1 gives infinite timeout.
*     PATH=INTEGER (given)
*           path to the task
*     MESSID=INTEGER (given)
*           messid for the action.
*     OUTVAL=CHARACTER*(*) (returned)
*           The value string from the task
*     STATUS=INTEGER

*  Algorithm:
*     Get replies from the task, handling parameter requests until the
*     final completion message is received.

*  Copyright:
*     Copyright (C) 1987, 1989, 1991-1993 Science & Engineering
*     Research Council. Copyright (C) 2001 Central Laboratory of the
*     Research Councils. All Rights Reserved.

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
*     16-NOV-1987 (REVAD::BDK):
*        Use SUBPAR_WRITE to forward messages
*     18-APR-1989 (REVAD::BDK):
*        Return VALUE on success
*     06-MAY-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     08-AUG-1991 (REVAD::BDK):
*        Handle GSOC, not just OBEY
*     25-NOV-1991 (REVAD::BDK):
*        Use ADAM_ACKNOW for SYNC replies
*     04-OCT-1992 (RLVAD::AJC):
*        Add PAR_PAR for porting
*     24-AUG-1993: Use SUBPAR_SYS not PAR_PAR
*                  SUBPAR__NAMELEN not PAR__SZNAM (RLVAD::AJC)
*     11-JUN-2001 (AJC):
*        Call AMS (FAMS) directly
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_ERR'
      INCLUDE 'DTASK_ERR'
      INCLUDE 'MESSYS_LEN'

*  Arguments Given:
      INTEGER TIMEOUT           ! timeout in millisecs.
                                ! -1 gives infinite timeout.

      INTEGER PATH              ! path to the task

      INTEGER MESSID            ! messid for the action.

*  Arguments Returned:
      CHARACTER*(*) OUTVAL      ! The value string from the task

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER MSGSTATUS              ! message status returned from task
      INTEGER MESLEN                 ! length of INTVAL
      CHARACTER*(MESSYS__VAL_LEN) INTVAL ! message returned from task
      INTEGER CONTEXT                ! context returned from task
      LOGICAL FINISHED               ! loop controller
      CHARACTER*(SUBPAR__NAMELEN) REPLACT ! action name returned by GETREPLY
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*    Loop picking up parameter requests until a completion status
*    is returned from the task.
*
      FINISHED = .FALSE.

      DO WHILE ( .NOT. FINISHED )

         CALL FAMS_GETREPLY( TIMEOUT, PATH, MESSID, MSGSTATUS, CONTEXT,
     :     REPLACT, MESLEN, INTVAL, STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( MESLEN .LT. MESSYS__VAL_LEN )
     :        INTVAL(MAX(1,MESLEN+1):) = ' '

            IF ( MSGSTATUS .EQ. MESSYS__PARAMREQ ) THEN
               CALL TASK_ASKPARAM (
     :           PATH, INTVAL, MESSID, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  FINISHED = .TRUE.
               ENDIF
            ELSE IF ( MSGSTATUS .EQ. MESSYS__INFORM ) THEN
               CALL SUBPAR_WRITE ( INTVAL, STATUS )
               INTVAL = ' '
            ELSE IF ( MSGSTATUS .EQ. MESSYS__SYNC ) THEN
               CALL SUBPAR_SYNC ( STATUS )
               CALL FAMS_REPLY( PATH, MESSID, MESSYS__MESSAGE,
     :           MESSYS__SYNCREP, CONTEXT, REPLACT, 1, ' ', STATUS )
            ELSE
               FINISHED = .TRUE.
               OUTVAL = INTVAL
               STATUS = MSGSTATUS
            END IF

         ELSE
            FINISHED = .TRUE.
            OUTVAL = ' '

         ENDIF

      ENDDO

      END
