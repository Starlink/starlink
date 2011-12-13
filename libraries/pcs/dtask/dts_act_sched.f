      SUBROUTINE DTASK_ACT_SCHED ( REQUEST, ACTPTR, SEQ, SCHEDTIME,
     :  HANDLED, STATUS)
*+
*  Name:
*     DTASK_ACT_SCHED

*  Purpose:
*     Set-up the rescheduling of an action

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_ACT_SCHED ( REQUEST, ACTPTR, SEQ, SCHEDTIME, HANDLED,
*     :  STATUS )

*  Description:
*       Check for the various reschedule codes that can be returned by
*       an application.

*  Arguments:
*     REQUEST=INTEGER (given)
*           Status returned from the application possibly specifying a
*           reschedule.
*     ACTPTR=INTEGER (given)
*           Pointer to the action in the action list.
*     SEQ=INTEGER (given)
*           The current sequence number for the action.
*     SCHEDTIME=INTEGER (given)
*           Time in milliseconds for reschedule action
*     HANDLED=LOGICAL (returned)
*           Set true if a reschedule was requested
*     STATUS=INTEGER

*  Algorithm:
*       REQUEST indicates the type of reschedule required, one
*       of  ACT__STAGE      Reschedule with no time delay.
*           ACT__WAIT       Reschedule with time delay of SCHEDTIME.
*           ACT__ASTINT     Reschedule on AST (with timeout of SCHEDTIME).
*           ACT__MESSAGE    Reschedule on completion of an action in a
*                           subsidiary task (with timeout of SCHEDTIME).
*
*       If REQUEST is one of these, cancel any "old" timer associated
*       with the action, increment the action counter and set HANDLED
*       true. In the case of ASTINT and MESSAGE, a new timer is started
*       if SCHEDTIME does not have the value INFINITE.
*
*       If REQUEST is not one of the above codes, the routine sets HANDLED
*       to false and returns.

*  Implementation Deficiencies:
*       The timeout on ACT__ASTINT may cause backward compatability problems
*       if the ACT routine sets a reschedule time and returns ACT__ASTINT
*       without expecting a timeout.

*  Copyright:
*     Copyright (C) 1991-1993 Science & Engineering Research Council.
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
*     Tony Farrell (AAO::TJF) 17Feb91
*     {enter_new_authors_here}

*  History:
*     17-FEB-1991 (AAO::TJF):
*        First insertion
*     01-MAY-1991: revise INCLUDE files and add REQUEST argument
*                  (REVAD::BDK)
*     03-MAY-1991: use DTASK_CANTIM to cancel any outstanding timer
*                  (REVAD::BDK)
*     13-MAY-1991 (REVAD:BDK):
*        Change comments
*     13-OCT-1992 (RLVAD::AJC):
*        Add INCLUDE 'PAR_PAR'
*     23-AUG-1993 (RLVAD::AJC):
*        Replace PAR_PAR with SUBPAR_SYS
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
      INCLUDE 'DTASK_ERR'
      INCLUDE 'ACT_ERR'
      INCLUDE 'MESSYS_PAR'

*  Arguments Given:
      INTEGER REQUEST           ! Reschedule request status
      INTEGER ACTPTR            ! action pointer
      INTEGER SEQ               ! sequence number
      INTEGER SCHEDTIME         ! requested reschedule time

*  Arguments Returned:
      LOGICAL HANDLED           ! .TRUE. => a reschedule has been set-up

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'DTASK_CMN'

*  Local Constants:
      INTEGER NOWAIT
      PARAMETER ( NOWAIT = 10) ! no wait defaults to 10ms Wait

*  Local Variables:
      INTEGER COUNT            ! Number of action subsidiary actions
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      HANDLED = .TRUE.

      IF ( REQUEST .EQ. ACT__STAGE ) THEN
*
*      Reschedule with no time delay.
*
         CALL DTASK_CANTIM ( ACTPTR, STATUS )
         CALL DTASK_RESCHED ( ACTPTR, ACTCOUNT(ACTPTR), NOWAIT,
     :     STATUS )
         ACTSEQ(ACTPTR) = SEQ + 1

      ELSE IF ( REQUEST .EQ. ACT__WAIT ) THEN
*
*      Reschedule with specified time delay.
*
         CALL DTASK_CANTIM ( ACTPTR, STATUS )
         CALL DTASK_RESCHED ( ACTPTR, ACTCOUNT(ACTPTR), SCHEDTIME,
     :     STATUS )
         ACTSEQ(ACTPTR) = SEQ + 1

      ELSE IF ( REQUEST .EQ. ACT__ASTINT ) THEN
*
*      Reschedule on AST. If specified, add reschedule on time-out.
*
         CALL DTASK_CANTIM ( ACTPTR, STATUS )
         IF ( SCHEDTIME .NE. MESSYS__INFINITE ) THEN
            CALL DTASK_RESCHED ( ACTPTR, ACTCOUNT(ACTPTR), SCHEDTIME,
     :        STATUS )
         ENDIF
         ACTSEQ(ACTPTR) = SEQ + 1

      ELSE IF ( REQUEST .EQ. ACT__MESSAGE ) THEN
*
*      Reschedule on  the completion of an action in a lower task.
*      Check that there are active actions.
*
         CALL DTASK_CANTIM ( ACTPTR, STATUS )
         CALL TASK_COUNT_MESSINFO ( ACTPTR, COUNT, STATUS )
         IF ( COUNT .EQ. 0 ) THEN
            IF ( STATUS .EQ. SAI__OK ) THEN
               STATUS = DTASK__NOSUBSIDACT
            ENDIF
*
*      If specified, reschedule on time-out.
*
         ELSE IF ( SCHEDTIME .NE. MESSYS__INFINITE ) THEN
            CALL DTASK_RESCHED ( ACTPTR, ACTCOUNT(ACTPTR), SCHEDTIME,
     :        STATUS )
         ENDIF
         ACTSEQ(ACTPTR) = SEQ + 1

      ELSE
         HANDLED = .FALSE.
      ENDIF

      END
