      SUBROUTINE DTASK_GSOC ( DTASK_APPLIC, PATH, MESSID, CONTEXT, NAME,
     :  VALUE, STATUS )
*+
*  Name:
*     DTASK_GSOC

*  Purpose:
*     Interpret a GET/SET/OBEY/CANCEL/CONTROL message

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_GSOC ( DTASK_APPLIC, PATH, MESSID, CONTEXT, NAME,
*     :  VALUE, STATUS )

*  Description:
*     Interpret a message requesting GET, SET, OBEY, CANCEL or CONTROL. If
*     necessary, activate the application.

*  Arguments:
*     DTASK_APPLIC=EXTERNAL (given)
*           application calling routine
*     PATH=INTEGER (given)
*           path for message received
*     MESSID=INTEGER (given)
*           transaction number for message received
*     CONTEXT=INTEGER (given)
*           context of message received
*     NAME=CHARACTER*(*) (given)
*           name field in received message
*     VALUE=CHARACTER*(*) (given)
*           command-line parameter string
*     STATUS=INTEGER

*  Algorithm:
*     A GET, SET or CONTROL is passed to the routine which handles it. For
*     OBEY and CANCEL check whether this is a valid request before calling
*     the corresponding routine.

*  Copyright:
*     Copyright (C) 1984, 1991-1993 Science & Engineering Research
*     Council. Copyright (C) 2001 Central Laboratory of the Research
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
*     John Cooke (REVS::JAC) 22May84
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     22-MAY-1984 (REVA::ADAM]):
*        First insertion
*     10-JUN-1991: DTASK_GSOC developed from various earlier routines
*                  (REVAD::BDK)
*     27-JUN-1991 (REVAD::BDK):
*        Call DTASK_SRCHKEY - name was wrong
*     25-NOV-1991 (REVAD::BDK):
*        Use ADAM_ACKNOW
*     28-FEB-1992 (AAO::TJF):
*        Add ACODE argument to DTASK_ADDLST
*     07-APR-1992 (RLVAD::BKM):
*        Add CONTROL context and CONTROL DEFAULT action
*     15-JUL-1992: Correct handling of unknown context messages
*     15-JUL-1992: Try to recover from GSOC errors
*     13-OCT-1992 (RLVAD::AJC):
*        Add INCLUDE 'PAR_PAR'
*     23-APR-1993 (RLVAD::AJC):
*        Remove unused variable declarations
*     23-AUG-1993 (RLVAD::AJC):
*        Replace PAR_PAR with SUBPAR_SYS
*                  Replace PAR__SZNAM with SUBPAR__NAMELEN  (RLVAD::AJC)
*     11-JUN-2001 (AJC):
*        Call AMS_REPLY (FAMS) directly
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'ADAM_DEFNS'
      INCLUDE 'DTASK_SYS'
      INCLUDE 'DTASK_ERR'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_ERR'
*  Arguments Given:
      EXTERNAL DTASK_APPLIC        ! application calling routine
      INTEGER PATH                 ! path for message received
      INTEGER MESSID               ! transaction number for message
                                   ! received
      INTEGER CONTEXT              ! context of message received
      CHARACTER*(*) NAME           ! name field in received message
      CHARACTER*(*) VALUE          ! command-line parameter string
*  Status:
      INTEGER STATUS
*  Global Variables:
      INCLUDE 'DTASK_CMN'
*  Local Variables:
      INTEGER ACTPTR               ! action pointer
      CHARACTER*(SUBPAR__NAMELEN) ANAME ! action name
      INTEGER ACTLEN               ! length of action name
      CHARACTER*(SUBPAR__NAMELEN) AKEY  ! action keyword
      INTEGER MESSTATUS            ! message status sent out
      INTEGER MESLEN               ! length of VALUE
      INTEGER SEQ                  ! sequence number for action
      INTEGER ACODE                ! number for the action in the
                                   ! parameter system
*.

      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Enable communications for error reporting
*
      CALL SUBPAR_PUTPATH ( PATH, MESSID, STATUS )
*
*      Check for SET, GET or CONTROL
*
      IF ( CONTEXT .EQ. SET ) THEN
         CALL DTASK_SET ( PATH, NAME, VALUE, MESSID, STATUS )
      ELSE IF ( CONTEXT .EQ. GET ) THEN
         CALL DTASK_GET ( PATH, NAME, MESSID, STATUS )
      ELSE IF ( CONTEXT .EQ. CONTROL) THEN
         CALL DTASK_CONTROL( PATH, NAME, VALUE, MESSID, STATUS )
*
*      An OBEY or CANCEL. Check if the action is active.
*
      ELSE IF ( CONTEXT .EQ. OBEY .OR. CONTEXT .EQ. CANCEL ) THEN
         CALL DTASK_SRCHKEY ( NAME, ACTPTR, STATUS )
         IF ( ( CONTEXT .EQ. OBEY ) .AND.
     :     ( STATUS .EQ. DTASK__ACTACTIVE ) ) THEN
            MESSTATUS = DTASK__REJECTED
            STATUS = SAI__OK
            CALL DTASK_COMSHUT ( PATH, MESSID, MESSTATUS, CONTEXT, NAME,
     :        VALUE, STATUS )

         ELSE IF ( ( CONTEXT .EQ. CANCEL ) .AND.
     :     ( STATUS .EQ. DTASK__NOTFOUND ) ) THEN
            MESSTATUS = DTASK__NOTACTIVE
            STATUS = SAI__OK
            CALL DTASK_COMSHUT ( PATH, MESSID, MESSTATUS, CONTEXT, NAME,
     :        VALUE, STATUS )

         ELSE
*
*         Valid OBEY or CANCEL.
*         Look-up the action keyword in the parameter system.
*
            STATUS = SAI__OK
            AKEY = NAME
            CALL SUBPAR_FINDACT ( AKEY, ACODE, STATUS )
            CALL SUBPAR_ACTNAME ( ACODE, ANAME, ACTLEN, STATUS )
*
*         Give the command-line parameter string to the parameter system
*
            CALL SUBPAR_CMDLINE ( ACODE, CONTEXT, VALUE, STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( CONTEXT .EQ. OBEY ) THEN
*
*               Start-up a new action.
*               Mark the action as active and clear the list of
*               subsidiary tasks for it.
*
                  SEQ = 0
                  CALL DTASK_ADDLST ( ANAME, AKEY, PATH, MESSID, SEQ,
     :              ACODE, ACTPTR, STATUS )
                  CALL TASK_CLEAR_MESSINFO ( ACTPTR, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
*
*                   Send the initial acknowledgement
*
                     MESSTATUS = DTASK__ACTSTART
                     MESLEN = MIN( LEN(VALUE), MESSYS__VAL_LEN )
                     CALL FAMS_REPLY( PATH, MESSID, MESSYS__MESSAGE,
     :                 MESSTATUS, CONTEXT, AKEY, MESLEN, VALUE, STATUS )
                     IF ( STATUS .NE. SAI__OK ) THEN
                        MESSTATUS = STATUS
                        CALL ERR_REP ( ' ',
     :                    'failed to send initial acknowledgement',
     :                    STATUS )
                        STATUS = SAI__OK
                        CALL DTASK_ACTSHUT ( PATH, MESSID, MESSTATUS,
     :                    CONTEXT, ACTPTR, ANAME, AKEY, VALUE, STATUS )
                     ELSE
*
*                     Can start the OBEY
*
                        CALL DTASK_OBEY ( DTASK_APPLIC, ACTPTR, VALUE,
     :                    STATUS )
                     ENDIF
                  ELSE
                     CALL ERR_REP ( ' ', 'failed to start action',
     :                 STATUS )
                     MESSTATUS = STATUS
                     STATUS = SAI__OK
                     CALL DTASK_COMSHUT ( PATH, MESSID, MESSTATUS,
     :                 CONTEXT, NAME, VALUE, STATUS )
                  ENDIF
               ELSE
*
*                Cancel was requested
*
                  CALL DTASK_CANCEL ( DTASK_APPLIC, PATH, MESSID,
     :              ACTPTR, VALUE, STATUS )

               ENDIF

            ELSE
*
*             Error returned from parameter system
*
               MESSTATUS = STATUS
               STATUS = SAI__OK
               CALL DTASK_COMSHUT ( PATH, MESSID, MESSTATUS,
     :            CONTEXT, NAME, VALUE, STATUS )

            ENDIF

         ENDIF
      ELSE
*
*      Illegal CONTEXT message
*
         MESSTATUS = DTASK__ILLCONTEXT
         STATUS = SAI__OK
         CALL DTASK_COMSHUT ( PATH, MESSID, MESSTATUS,
     :      CONTEXT, NAME, VALUE, STATUS )
      ENDIF

      END
