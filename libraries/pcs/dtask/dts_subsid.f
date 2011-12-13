      SUBROUTINE DTASK_SUBSID ( DTASK_APPLIC, PATH, MESSID, CONTEXT,
     :  AKEY, MSGSTATUS, VALUE, STATUS )
*+
*  Name:
*     DTASK_SUBSID

*  Purpose:
*     Check for a message from a subsidiary task

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_SUBSID ( DTASK_APPLIC, PATH, MESSID, CONTEXT,
*     :  AKEY, MSGSTATUS, VALUE, STATUS )

*  Description:
*     Check whether the message has come from a subsidiary task. If it
*     has, check what sort of message it is and either forward it or
*     reactivate this application.

*  Arguments:
*     DTASK_APPLIC=EXTERNAL (given)
*           application calling routine
*     PATH=INTEGER (given)
*           path for received message
*     MESSID=INTEGER (given)
*           identifier for transaction
*     CONTEXT=INTEGER (given)
*           GET, SET, OBEY or CANCEL
*     AKEY=CHARACTER*(*) (given)
*           action keyword
*     MSGSTATUS=INTEGER (given)
*           status in received message
*     VALUE=CHARACTER*(*) (given and returned)
*           received VALUE string
*     STATUS=INTEGER

*  Copyright:
*     Copyright (C) 1991-1993 Science & Engineering Research Council.
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
*     11-JUN-1991 (REVAD:BDK):
*        Original
*     18-OCT-1991 (RLVAD::AJC):
*        Always call ERR_REP with bad status
*     25-NOV-1991: use ADAM_ACKNOW for forwarding replies to SYNCs
*                 (REVAD::BDK)
*     13-OCT-1992: add INCLUDE 'PAR_PAR'
*                  Get ^STATUS via DTASK_ESETK (RLVAD::AJC)
*     23-AUG-1993 (RLVAD::AJC):
*        Replace PAR_PAR with SUBPAR_SYS
*     11-JUN-2001: Call AMS (FAMS) _REPLY and _PLOOKUP directly
*                  ADAM_PRCNAM now DTASK_PRCNAM (AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'DTASK_SYS'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_ERR'

*  Arguments Given:
      EXTERNAL DTASK_APPLIC   ! application calling routine
      INTEGER PATH            ! path for received message
      INTEGER MESSID          ! identifier for transaction
      INTEGER CONTEXT         ! GET, SET, OBEY or CANCEL
      CHARACTER*(*) AKEY      ! action keyword
      INTEGER MSGSTATUS       ! status in received message

*  Arguments Given and Returned:
      CHARACTER*(*) VALUE     ! received VALUE string

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'DTASK_CMN'

*  Local Variables:
      INTEGER ACTPTR                    ! action pointer
      CHARACTER*(MESSYS__TNAME) MYNAME  ! name of this task
      INTEGER NLENGTH                   ! actual length of MYNAME
      CHARACTER*(MESSYS__TNAME) BADNAME ! name of task trying to
                                        ! communicate
      INTEGER MESLEN                    ! length of VALUE
      INTEGER ISTAT                     ! local status
      LOGICAL TRANSPARENT               ! TRUE => message to be forwarded
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Check if message is from a subsidiary task
*
      CALL TASK_SEARCH_MESSINFO ( PATH, MESSID, ACTPTR, STATUS )

      IF ( ACTPTR .GT. 0 ) THEN
*
*      It is on the list. Enable communications to parent task
*
         CALL SUBPAR_PUTPATH ( ACTPATH(ACTPTR), ACTMESSID(ACTPTR),
     :     STATUS )
*
*      Relay parameter requests, informational messages and
*      synchronisation transparently.
*
         TRANSPARENT = .TRUE.
         IF ( MSGSTATUS .EQ. MESSYS__PARAMREQ ) THEN
            CALL TASK_ASKPARAM ( PATH, VALUE, MESSID, STATUS )
         ELSE IF ( MSGSTATUS .EQ. MESSYS__INFORM ) THEN
            CALL SUBPAR_WRITE ( VALUE, STATUS )
         ELSE IF ( MSGSTATUS .EQ. MESSYS__SYNC ) THEN
            CALL SUBPAR_SYNC ( STATUS )
            MESLEN = MIN( LEN(VALUE), MESSYS__VAL_LEN )
            CALL FAMS_REPLY( PATH, MESSID, MESSYS__MESSAGE,
     :        MESSYS__SYNCREP, CONTEXT, AKEY, MESLEN, VALUE, STATUS )

         ELSE
*
*         Not transparent. Save message details so application can
*         pick them up if called.
*
            TRANSPARENT = .FALSE.
            CALL TASK_PUT_MESSINFO ( PATH, CONTEXT, AKEY, VALUE,
     :        MESSID, MSGSTATUS )
         ENDIF
*
*      If failed to handle transparent operation or if subsidiary
*      action is complete, clear its entry.
*      (NB, failed transparent handling is indistinguishable from
*      action completion).
*
         IF ( TRANSPARENT .AND. ( STATUS .NE. SAI__OK ) ) THEN
*
*         Error in trying to forward messages.
*
            STATUS = SAI__OK
            CALL TASK_REMOVE_MESSINFO ( PATH, MESSID, STATUS )
            CALL DTASK_OBEY ( DTASK_APPLIC, ACTPTR, VALUE, STATUS )

         ELSE IF ( .NOT. TRANSPARENT ) THEN

            IF ( MSGSTATUS .NE. MESSYS__TRIGGER ) THEN
*
*            Action is complete in subsidiary task
*
               CALL TASK_REMOVE_MESSINFO ( PATH, MESSID, STATUS )
            ENDIF
            CALL DTASK_OBEY ( DTASK_APPLIC, ACTPTR, VALUE, STATUS )

         ENDIF

      ELSE
*
*      Not found in task list. Interpret it as bad status on received
*      message
*
         ISTAT = SAI__OK
         CALL DTASK_PRCNAM ( MYNAME, NLENGTH, ISTAT )
         CALL DTASK_ESETK( 'STAT', MSGSTATUS )
         CALL ERR_REP ( ' ', MYNAME(1:NLENGTH) //
     :           ' received unexpected message, ^STAT', MSGSTATUS )
         ISTAT = SAI__OK
         CALL FAMS_PLOOKUP ( PATH, BADNAME, ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
                  BADNAME = 'unknown'
         ENDIF
         CALL MSG_SETC ( 'BADNAME', BADNAME )
         CALL MSG_SETC ( 'AKEY', AKEY )
         IF ( VALUE .EQ. ' ' ) THEN
            CALL ERR_REP ( ' ', 'From task ^BADNAME, action '//
     :        '^AKEY', MSGSTATUS )
         ELSE
            CALL MSG_SETC ( 'VALUE', VALUE )
            CALL ERR_REP ( ' ', 'From task ^BADNAME, action '//
     :        '^AKEY, value ^VALUE', MSGSTATUS )
         ENDIF
*
*            Clear-out ERR and MSG.
*
         CALL ERR_CLEAR ( STATUS )

      ENDIF

      END
