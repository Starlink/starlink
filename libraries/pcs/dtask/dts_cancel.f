      SUBROUTINE DTASK_CANCEL ( DTASK_APPLIC, PATH, MESSID, ACTPTR,
     :  VALUE, STATUS )
*+
*  Name:
*     DTASK_CANCEL

*  Purpose:
*     Cancel an action if active

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_CANCEL ( DTASK_APPLIC, PATH, MESSID, ACTPTR,
*     :  VALUE, STATUS )

*  Description:
*     Tell the application that a request has been received to cancel an
*     action which is currently waiting to be rescheduled.

*  Arguments:
*     DTASK_APPLIC=EXTERNAL (given)
*           address of action routine
*     PATH=INTEGER (given)
*           message path needed for reply
*     MESSID=INTEGER (given)
*           transaction number
*     ACTPTR=INTEGER (given)
*           action pointer
*     VALUE=CHARACTER*(*) (given and returned)
*           command line parameter string
*     STATUS=INTEGER

*  Algorithm:
*     Check the named action is in progress.
*     Give the command-line parameter string to the parameter system.
*     Call DTASK_APPLIC to inform the application code what has
*     happened.
*     Check the status returned from the application to see whether the
*     action is to be terminated or whether it is to continue
*     rescheduling.
*     In any case, send an acknowledgement to the task which requested
*     the CANCEL.
*     Send an acknowledgement to the task which issued the obey if the
*     action has ended.

*  Copyright:
*     Copyright (C) 1984-1987, 1989-1993 Science & Engineering Research
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
*     {original_author_entry}

*  History:
*     22-MAY-1984 (REVA::ADAM]):
*        First insertion ( stub )
*     25-MAY-1984 (REVA::ADAM]):
*        Now implemented
*     25-MAY-1984 (REVA::ADAM]):
*        %val on actptr in cantim
*     25-MAY-1984 (REVA::ADAM]):
*        Remove %val on actptr !
*     25-MAY-1984 (REVA::ADAM]):
*        Replace %val again for tests
*     20-JUN-1984 (REVA::ADAM):
*        Changed error symbol names
*     25-JUN-1984 (REVA::ADAM):
*        Added AST interrupts
*     02-OCT-1984 (REVAD::BDK):
*        Handle command-line parameters
*     16-NOV-1984 (REVA::ADAM):
*        New version for parameter system
*     16-NOV-1984 (REVA::ADAM):
*        Handle CANCONSTR status
*     24-NOV-1984 (REVA::ADAM):
*        Use DTASK_ACKNOW (BDK)
*     16-APR-1985 (REVAD::BDK):
*        Send two acknowledgements if needed
*     21-JUN-1985: return name of parameter violating constraints
*                      (REVAD::BDK)
*     21-MAR-1986: remove special handling of reschedules and ASTs
*                  and pass SEQ to ACT (REVAD::BDK)
*     25-MAR-1986: don't send 2nd acknowledgement if action not active
*                      (REVAD::BDK)
*     22-JAN-1987 (AAOEPP::JAB):
*        New command line parser added
*     26-MAY-1987 (REVAD::BDK):
*        Use action keyword
*     30-APR-1989: call DTASK_APPLIC rather than ACT and surround with
*                  TASK_PUT_CURRINFO and TASK_GET_CURRINFO calls (AAOEPP::WFL)
*     30-APR-1989 (AAOEPP::WFL):
*        Call TASK_CLEAR_MESSINFO on action completion
*     01-MAY-1989 (AAOEPP::WFL):
*        Check for ACT__MESSAGE status returned
*     01-MAR-1990: call DTASK_APPLIC all arguments required by ACT (and more);
*                  improve status checking, correct comments (AAOEPP::WFL)
*     09-APR-1991: always send two acknowledgements if the action was
*                  actually cancelled, two ensure that both transactions
*                  are closed-down (REVAD::BDK)
*     25-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     30-APR-1991: revise INCLUDE files, reduce sizes of arguments to
*                  SUBPAR_CHECKACT (REVAD::BDK)
*     02-MAY-1991 (REVAD::BDK):
*        Fit the TJF mod DTASK_ACT_SCHED
*     03-MAY-1991 (REVAD::BDK):
*        Cancel timer if necessary
*     09-MAY-1991: VALUE is given and returned, change order of
*                  arguments (REVAD::BDK)
*     14-MAY-1991 (ROE::BMC):
*        Remove action parameter constraint checking
*     27-MAY-1991 (REVAD::BDK):
*        User ERR_REP and DTASK_COMSHUT
*     07-JUN-1991: change arguments to DTASK_APPLIC, add extra error
*                  reporting and change comments (REVAD::BDK)
*     11-JUN-1991: change call arguments, assume the parameter system
*                  has been set-up already (REVAD::BDK)
*     23-JUL-1991 (REVAD::BDK):
*        Get action keyword from COMMON
*     22-AUG-1991 (REVAD::BDK):
*        Add REQUEST argument to DTASK_APPLIC
*     25-NOV-1991 (REVAD::BDK):
*        Use ADAM_ACKNOW
*     13-OCT-1992: add INCLUDE 'PAR_PAR'
*                  Get ^STATUS via DTASK_ESETK (RLVAD::AJC)
*     27-JUL-1993 (RLVAD::AJC):
*        Remove unused ACTLEN
*     23-AUG-1993 (RLVAD::AJC):
*        Replace PAR_PAR with SUBPAR_SYS
*                  Replace PAR__SZNAM with SUBPAR__NAMELEN  (RLVAD::AJC)
*     11-JUN-2001 (AJC)::
*        Call AMS_REPLY (FAMS) directly
*     {enter_further_changes_here}

*  Bugs:
*     <description of any "bugs" which have not been fixed>
*     Authors
*     John Cooke (REVS::JAC) 22May84
*     {note_new_bugs_here}

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
      INCLUDE 'ACT_ERR'

*  Arguments Given:
      EXTERNAL DTASK_APPLIC  ! address of action routine
      INTEGER PATH           ! message path needed for reply
      INTEGER MESSID         ! transaction number
      INTEGER ACTPTR         ! action pointer

*  Arguments Given and Returned:
      CHARACTER*(*) VALUE    ! command line parameter string

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'DTASK_CMN'

*  Local Variables:
      INTEGER SEQ                     ! sequence number for stage of
                                      ! action
      INTEGER SCHEDTIME               ! requested reschedule time in
                                      ! millisec
      INTEGER ACODE                   ! parameter system code number for
                                      ! the action
      INTEGER MESSTATUS               ! status returned in acknowledgment
      INTEGER MESLEN                  ! length of VALUE
      INTEGER PATHOB                  ! path to task issuing original OBEY
      INTEGER MESSIDOB                ! messid of original OBEY
      CHARACTER*(SUBPAR__NAMELEN) ANAME    ! action name
      CHARACTER*(SUBPAR__NAMELEN) AKEY     ! action keyword
      LOGICAL HANDLED                 ! did DTASK_ACT_SCHED handle the
                                      ! reschedule
      INTEGER REQUEST                 ! status signalling reschedule
                                      ! type requested
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Extract the details of the OBEY transaction
*
      PATHOB = ACTPATH(ACTPTR)
      MESSIDOB = ACTMESSID(ACTPTR)
      SEQ = ACTSEQ(ACTPTR)
      ACODE = ACTCODE(ACTPTR)
      ANAME = ACTNAME(ACTPTR)
      AKEY = ACTKEY(ACTPTR)
*
*   Call the application.
*
      CALL DTASK_APPLIC ( CANCEL, ACODE, ANAME, ACTPTR, SEQ,
     :  VALUE, SCHEDTIME, REQUEST, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN
*
*      Check for a reschedule request
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
*            Remove any outstanding timer
*            Clear list of active actions in subsidiary tasks.
*            Remove entry from the action list.
*
               MESSTATUS = STATUS
               CALL ERR_REP ( ' ',
     :           'failed to reschedule on receipt of cancel ' //
     :           AKEY, STATUS )
               CALL DTASK_ESETK ( 'STAT', STATUS )
               CALL ERR_REP ( ' ', '^STAT', STATUS )
               STATUS = SAI__OK
               CALL DTASK_CANTIM ( ACTPTR, STATUS )
               CALL TASK_CLEAR_MESSINFO ( ACTPTR, STATUS )
               CALL DTASK_REMLST ( ANAME, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL DTASK_ESETK ( 'STAT', STATUS )
                  CALL ERR_REP ( ' ', 'DTASK_CANCEL: ^STAT',
     :              STATUS )
               ENDIF
*
*            Close down the original OBEY transaction.
*
               STATUS = SAI__OK
               MESLEN = MIN( LEN(VALUE), MESSYS__VAL_LEN )
               CALL FAMS_REPLY( PATHOB, MESSIDOB, MESSYS__MESSAGE,
     :           MESSTATUS, CANCEL, AKEY, MESLEN, VALUE, STATUS )

            ELSE
*
*         The action has been rescheduled to complete the cancellation.
*         Tell the process which issued the cancel that everything is ok.
*
               MESSTATUS = DTASK__ACTCANCEL

            ENDIF

         ELSE IF ( REQUEST .EQ. ACT__CANCEL ) THEN
*
*         No more rescheduling required.
*         Remove any outstanding timer
*         Clear list of active actions in subsidiary tasks.
*         Remove entry from the action list.
*
            CALL DTASK_CANTIM ( ACTPTR, STATUS )
            CALL TASK_CLEAR_MESSINFO ( ACTPTR, STATUS )
            CALL DTASK_REMLST ( ANAME, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               MESSTATUS = DTASK__ACTCANCEL
            ELSE
               MESSTATUS = STATUS
            ENDIF
*
*         Close down the original OBEY transaction.
*
            STATUS = SAI__OK
               MESLEN = MIN( LEN(VALUE), MESSYS__VAL_LEN )
               CALL FAMS_REPLY( PATHOB, MESSIDOB, MESSYS__MESSAGE,
     :           MESSTATUS, CANCEL, AKEY, MESLEN, VALUE, STATUS )

         ELSE IF ( REQUEST .EQ. ACT__END ) THEN
*
*         Invalid status.
*
            CALL ERR_REP ( ' ',
     :        'the application returned ACT__END request', REQUEST )
            CALL ERR_REP ( ' ',
     :        'this is invalid in response to a CANCEL command',
     :        REQUEST )

            MESSTATUS = DTASK__ACTNOTCANCEL

         ELSE
*
*         Invalid request returned from application
*
            IF ( REQUEST .EQ. SAI__OK ) THEN
               MESSTATUS = DTASK__ACTNOTCANCEL
            ELSE
               MESSTATUS = REQUEST
            ENDIF
            CALL ERR_REP ( ' ',
     :        'application returned invalid request', MESSTATUS )

         ENDIF

      ELSE
*
*      Error returned from application
*
         CALL ERR_REP ( ' ', 'CANCEL failed with bad status', STATUS )
         MESSTATUS = STATUS
      ENDIF
*
*   Close down the CANCEL transaction.
*
      STATUS = SAI__OK
      CALL DTASK_COMSHUT ( PATH, MESSID, MESSTATUS, CANCEL, AKEY,
     :  VALUE, STATUS )

      END
