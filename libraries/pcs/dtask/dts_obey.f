      SUBROUTINE DTASK_OBEY ( DTASK_APPLIC, ACTPTR, VALUE, STATUS )
*+
*  Name:
*     DTASK_OBEY

*  Purpose:
*     Handle action for "obey" request

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_OBEY ( DTASK_APPLIC, ACTPTR, VALUE, STATUS )

*  Description:
*     Carry out an OBEY, whether first-time in or as a result of a
*     reschedule. After the application has returned, set-up any
*     reschedule it may have requested.

*  Arguments:
*     DTASK_APPLIC=EXTERNAL (given)
*           address of action routine
*     ACTPTR=INTEGER (given)
*           index for looking-up the action details
*     VALUE=CHARACTER*(*) (given and returned)
*           command line parameter string
*     STATUS=INTEGER

*  Algorithm:
*     Call DTASK_APPLIC, check the returned status and set up any
*     requested rescheduling. If the action has completed send the final
*     acknowledgment.

*  Copyright:
*     Copyright (C) 1984-1987, 1989-1993 Science & Engineering Research
*     Council. Copyright (C) 3276 Particle Physics & Astronomy Research
*     Council. All Rights Reserved.

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
*     John Cooke (REVA::JAC) 17May84
*     {enter_new_authors_here}

*  History:
*     22-MAY-1984 (REVA::ADAM]):
*        First insertion
*     22-MAY-1984 (REVA::ADAM]):
*        Remove entry from action list once complete
*     22-MAY-1984 (REVA::ADAM]):
*        Test debug
*     22-MAY-1984 (REVA::ADAM]):
*        Ditto
*     22-MAY-1984 (REVA::ADAM]):
*        Repair call to addlst
*     22-MAY-1984 (REVA::ADAM]):
*        Remove debug
*     22-MAY-1984 (REVA::ADAM]):
*        Increment actseq on "wait" or "stage"
*     24-MAY-1984 (REVA::ADAM]):
*        Added msg_context - was missing
*     24-MAY-1984 (REVA::ADAM]):
*        Handle rejected actions by not calling act
*     20-JUN-1984 (REVA::ADAM):
*        Changed error symbol names
*     20-JUN-1984 (REVA::ADAM):
*        Removed references to obstat
*     25-JUN-1984 (REVA::ADAM):
*        Added AST interrupt
*     25-JUN-1984 (REVA::ADAM):
*        Added seq increment for astint
*     17-AUG-1984 (REVA::ADAM):
*        Add "inform" return status
*     26-OCT-1984 (REVAD::BDK):
*        Add CHECKACT
*     14-NOV-1984 (REVA::ADAM):
*        Made 'SEQ' an import/export parameter
*     14-NOV-1984 (REVA::ADAM):
*        Allow seq to return as 0 (for first return!)
*     16-NOV-1984 (REVA::ADAM):
*        New version with parameter system
*     16-NOV-1984 (REVA::ADAM):
*        Handle ACTCONSTR status
*     16-NOV-1984 (REVA::ADAM):
*        Handle constraint status correctly!
*     16-NOV-1984 (REVA::ADAM):
*        Try again!
*     24-NOV-1984 (REVAD::BDK):
*        Use DTASK_ACKNOW
*     24-NOV-1984 (REVA::ADAM):
*        Handle SUBPAR_FINDACT status correctly (BDK)
*     23-JUN-1985: report name of parameter violating constraints
*                      (REVAD::BDK)
*     09-OCT-1985 (REVAD::BDK):
*        Trap OK and NORMAL status returns from ACT
*     25-MAR-1986: trap ACTCANCEL return from ACT, clarify handling of
*                  error conditions after CHECKACT (REVAD::BDK)
*     09-JAN-1987 (AAOEPP::JAB):
*        New command line parser added
*     26-MAY-1987 (REVAD::BDK):
*        Use action keyword
*     30-APR-1989: call DTASK_APPLIC rather than ACT and surround with
*                  TASK_PUT_CURRINFO and TASK_GET_CURRINFO calls (AAOEPP::WFL)
*     30-APR-1989: call TASK_CLEAR_MESSINFO at start and end of action to
*                  clear records of active subsidiary actions (AAOEPP::WFL)
*     01-MAY-1989: handle ACT__MESSAGE status; support timeout on ACT__ASTINT
*                  ACT__MESSAGE (AAOEPP::WFL)
*     14-NOV-1989: remove include 'mesdefns'
*                  re-compile with MAXACTTOT = 32767 not 2**30
*                  (revised DTCOMMON) (RLVAD::AJC)
*     01-MAR-1990: call DTASK_APPLIC all arguments required by ACT (and more);
*                  improve status checking, correct comments (AAOEPP::WFL)
*     02-MAR-1990 (AAOEPP::WFL):
*        Restore include 'mesdefns' (defines INFINITE)
*     09-APR-1991: used passed-in PATH and MESSID for rescheduled
*                  actions (REVAD::BDK)
*     25-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     30-APR-1991: revise INCLUDE files, reduce sizes of arguments to
*                  SUBPAR_CHECKACT (REVAD::BDK)
*     01-MAY-1991: remove ACTTOT, fit the TJF mod DTASK_ACT_SCHED
*                  (REVAD::BDK)
*     03-MAY-1991 (REVAD::BDK):
*        Cancel timers on reschedules
*     09-MAY-1991: change order of arguments to this routine. Pass AKEY
*                  to ADDLST, improve error trapping (REVAD::BDK)
*     10-MAY-1991: close ERR and MSG whenever the transaction closes
*                  don't bother trapping -ve values of SEQ (REVAD::BDK)
*     13-MAY-1991 (REVAD::BDK):
*        Use ACTSHUT and COMSHUT
*     14-MAY-1991 (ROE::BMC):
*        Remove action parameter constraint checking
*     28-MAY-1991 (REVAD::BDK):
*        Remove lib$cvt_dx_dx
*     28-MAY-1991 (ROE::BMC):
*        Remove reference to NOWAIT and PARNAME
*     07-JUN-1991 (REVAD::BDK):
*        Change arguments to DTASK_APPLIC
*     10-JUN-1991: sizeable rewrite, change subroutine arguments
*                  (REVAD::BDK)
*     05-JUL-1991 (REVAD::BDK):
*        Copy action keyword from COMMON
*     22-AUG-1991 (REVAD::BDK):
*        Add REQUEST argument to DTASK_APPLIC
*     13-OCT-1992: add INCLUDE 'PAR_PAR'
*                  use DTASK__SYSNORM instead of SS$_NORMAL (RLVAD::AJC)
*     27-JUL-1993 (RLVAD::AJC):
*        Remove unused VALID and ACTLEN
*     23-AUG-1993 (RLVAD::AJC):
*        Replace PAR_PAR with SUBPAR_SYS
*                  Replace PAR__SZNAM with SUBPAR__NAMELEN  (RLVAD::AJC)
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
      INCLUDE 'ACT_ERR'

*  Arguments Given:
      EXTERNAL DTASK_APPLIC      ! address of action routine
      INTEGER ACTPTR             ! index for looking-up the action
                                 ! details
      CHARACTER*(*) VALUE        ! command line parameter string

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'DTASK_CMN'

*  Local Variables:
      INTEGER SCHEDTIME              ! time in milliseconds for
                                     ! rescheduled action
      INTEGER SEQ                    ! sequence number for stage of
                                     ! action
      INTEGER ACODE                  ! pointer to the action in the
                                     ! parameter system tables
      INTEGER MESSTATUS              ! status to return in
                                     ! acknowledgment
      CHARACTER*(SUBPAR__NAMELEN) ANAME   ! action name
      CHARACTER*(SUBPAR__NAMELEN) AKEY    ! action keyword
      LOGICAL HANDLED                ! did DTASK_ACT_SCHED detect a
                                     ! reschedule action request from
                                     ! the application
      INTEGER REQUEST                ! copy of request code returned
                                     ! from the application
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Call the application handler.
*
      ACODE = ACTCODE(ACTPTR)
      ANAME = ACTNAME(ACTPTR)
      AKEY = ACTKEY(ACTPTR)
      SEQ = ACTSEQ(ACTPTR)

      CALL DTASK_APPLIC ( OBEY, ACODE, ANAME, ACTPTR, SEQ, VALUE,
     :  SCHEDTIME, REQUEST, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN
*
*      Check for a reschedule request.
*
         CALL DTASK_ACT_SCHED ( REQUEST, ACTPTR, SEQ, SCHEDTIME,
     :     HANDLED, STATUS )

         IF ( HANDLED ) THEN
*
*         A reschedule was requested
*
            IF ( STATUS .NE. SAI__OK ) THEN
*
*            Attempted reschedule failed.
*
               MESSTATUS = STATUS
               STATUS = SAI__OK
               CALL DTASK_ACTSHUT ( ACTPATH(ACTPTR), ACTMESSID(ACTPTR),
     :           MESSTATUS, OBEY, ACTPTR, ANAME, AKEY, VALUE, STATUS )
            ENDIF

         ELSE
*
*         A reschedule was not requested.
*         Set message status depending on the application status return;
*         assume that entire action is complete if status is
*         not an expected status return.
*
             IF ( REQUEST .EQ. ACT__END ) THEN
                MESSTATUS = DTASK__ACTCOMPLETE
             ELSE IF ( REQUEST .EQ. ACT__UNIMP ) THEN
                MESSTATUS = DTASK__ACTUNIMP
             ELSE IF ( REQUEST .EQ. ACT__INFORM ) THEN
*
*             The value string contains message text for the user.
*
                MESSTATUS = DTASK__ACTINFORM
             ELSE
*
*             Return the unexpected status.
*
               IF ( REQUEST .EQ. SAI__OK ) THEN
                  MESSTATUS = DTASK__IVACTSTAT
                  CALL ERR_REP ( ' ',
     :              'DTASK_OBEY: application returned illegal SAI__OK',
     :              MESSTATUS )
               ELSE IF ( REQUEST .EQ. DTASK__SYSNORM ) THEN
                  MESSTATUS = DTASK__IVACTSTAT
                  CALL ERR_REP ( ' ',
     :              'DTASK_OBEY: application returned illegal '/
     :              /'SS$_NORMAL', MESSTATUS )
               ELSE IF ( REQUEST .EQ. ACT__CANCEL ) THEN
                  MESSTATUS = DTASK__IVACTSTAT
                  CALL ERR_REP ( ' ',
     :              'DTASK_OBEY: application returned illegal '/
     :              /'ACT__CANCEL',
     :              MESSTATUS )
               ELSE
                  MESSTATUS = REQUEST
               ENDIF
            ENDIF
*
*         Shut down the action.
*
            STATUS = SAI__OK
            CALL DTASK_ACTSHUT ( ACTPATH(ACTPTR), ACTMESSID(ACTPTR),
     :        MESSTATUS, OBEY, ACTPTR, ANAME, AKEY, VALUE, STATUS )

         ENDIF

      ELSE
*
*      Error status returned from application
*
         MESSTATUS = STATUS
         STATUS = SAI__OK
         CALL DTASK_ACTSHUT ( ACTPATH(ACTPTR), ACTMESSID(ACTPTR),
     :     MESSTATUS, OBEY, ACTPTR, ANAME, AKEY, VALUE, STATUS )

      ENDIF

      END
