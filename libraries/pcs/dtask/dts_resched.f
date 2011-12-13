      SUBROUTINE DTASK_RESCHED ( ACTPTR, ACTCNT, SCHEDTIME, STATUS )
*+
*  Name:
*     DTASK_RESCHED

*  Purpose:
*     Unix version : sets a timer for rescheduling an action

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_RESCHED ( ACTPTR, ACTCNT, SCHEDTIME, STATUS )

*  Description:
*     Set a timer. When the timer completes the action specified by
*     ACTPTR and ACTCNT will be rescheduled if it is still waiting.

*  Arguments:
*     ACTPTR=INTEGER (given)
*           pointer to required action in action list
*     ACTCNT=INTEGER (given)
*           timer counter for the action
*     SCHEDTIME=INTEGER (given)
*           time before re-scheduling ( in milliseconds )
*     STATUS=INTEGER

*  Algorithm:
*     Use the Unix interval timer process accessed through FATIMER_SETTIMR
*     and DTASK_ASTHDLR to deliver a MESSYS format message to a task on its
*     reschedule queue after a delay.
*     The values of ACTPTR and ACTCNT are passed to and returned from the
*     interval timer in the reschedule message by packing them into 4 bytes.
*     Note that the VALUE of the packed result has to be passed - if the
*     ADDRESS was used it would get overwritten by the next action to request
*     a timed reschedule.
*     The timer parameter (c.f. AST value on VMS) doubles as the timer
*     identifier, so it is stored to allow subsequent cancelling of the timer.

*  Copyright:
*     Copyright (C) 1984, 1991-1994 Science & Engineering Research
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
*     John Cooke (REVS::JAC) 22May84
*     {enter_new_authors_here}

*  History:
*     22-MAY-1984 (REVA::ADAM]):
*        First insertion
*     22-MAY-1984 (REVA::ADAM]):
*        Remove %val on AST parameter
*     25-MAY-1984 (REVA::ADAM]):
*        Add %val on actptr in setimr
*     25-MAY-1984 (REVA::ADAM]):
*        Remove %val again !
*     25-MAY-1984 (REVA::ADAM]):
*        Replace %val again for tests
*     20-JUN-1984 (REVA::ADAM):
*        New error system; 10ms to 1ms ticks
*     25-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files and add comments
*     03-MAY-1991 (REVAD::BDK):
*        Store timer id
*     13-MAY-1991 (REVAD::BDK):
*        Don't use LIB$SIGNAL
*     28-MAY-1991 (REVAD::BDK):
*        Use ERR_REP
*     28-MAY-1991 (ROE::BMC):
*        Correct ER_REP to ERR_REP
*     05-JUN-1991: Pack the AST parameter using arithmetic rather than
*                  an equivalence statement (ROE::BMC)
*     07-JUN-1991 (REVAD::BDK):
*        Change comments
*     13-OCT-1992: add INCLUDE 'PAR_PAR'
*                  get ^STATUS via DTASK_ESETK (RLVAD::AJC)
*     23-AUG-1993 (RLVAD::AJC):
*        Replace PAR_PAR with SUBPAR_SYS
*     08-SEP-1993 (RLVAD::BKM):
*        Unix version
*     28-JUN-1994 (RAL::AJC):
*        New Unix version
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
*  Arguments Given:
      INTEGER ACTPTR          ! pointer to required action in action
                              ! list
      INTEGER ACTCNT          ! timer counter for the action
      INTEGER SCHEDTIME       ! time before re-scheduling in
                              ! milliseconds
*  Status:
      INTEGER STATUS

*    External references :
      EXTERNAL DTASK_CHDLR     ! Handler called directly from ATIMER service

*  Global Variables:
      INCLUDE 'DTASK_CMN'

*  Local Variables:
      INTEGER TIMERVAL         ! timer parameter
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Insert two 16-bit values in the timer parameter.
*
      TIMERVAL = ACTPTR * 65536 + ACTCNT
*
*   Set timer running
*
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL FATIMER_SETTIMR( SCHEDTIME, TIMERVAL, DTASK_CHDLR,
     :                         STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP ( ' ', 'DTASK_RESCHED: failed to start timer',
     :        STATUS )
         ELSE
            ACTTIM(ACTPTR) = TIMERVAL
         ENDIF
      ENDIF

      END
