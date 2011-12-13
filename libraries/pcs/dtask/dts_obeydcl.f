      SUBROUTINE DTASK_OBEYDCL ( DTASK_APPLIC, NAME, VALUE, STATUS )
*+
*  Name:
*     DTASK_OBEYDCL

*  Purpose:
*     Obey action in DCL task

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_OBEYDCL ( DTASK_APPLIC, NAME, VALUE, STATUS )

*  Description:
*     Carry out an OBEY. This includes handling any command-line
*     parameters which came with the OBEY.

*  Arguments:
*     DTASK_APPLIC=EXTERNAL (given)
*           address of action routine
*     NAME=CHARACTER*(*) (given)
*           keyword of action required
*     VALUE=CHARACTER*(*) (given)
*           command line parameter string
*     STATUS=INTEGER

*  Algorithm:
*     Check the given action in the list of declared actions for the
*     task.
*     If everything is ok pass the command line parameter string to the
*     parameter system
*     If all this works, call DTASK_APPLIC (which is outside the
*     shareable image).

*  Copyright:
*     Copyright (C) 1985, 1987-1993 Science & Engineering Research
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
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     14-NOV-1985 (REVAD::BDK):
*        Original
*     09-JAN-1987 (AAOEPP::JAB):
*        New command line parser added
*     26-MAY-1987 (REVAD::BDK):
*        Use action keyword
*     09-FEB-1988 (REVAD::BDK):
*        Return bad status from CMDLINE
*     30-APR-1989: call DTASK_APPLIC rather than ACT and surround with
*                  TASK_PUT_CURRINFO and TASK_GET_CURRINFO calls (AAOEPP::WFL)
*     01-MAR-1990: call DTASK_APPLIC all arguments required by ACT (and more);
*                  improve status checking, correct comments (AAOEPP::WFL)
*     25-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     30-APR-1991: revise INCLUDE files, reduce sizes of arguments to
*                  SUBPAR_CHECKACT (REVAD::BDK)
*     14-MAY-1991 (ROE::BMC):
*        Remove action parameter constraint checking
*     28-MAY-1991 (REVAD::BDK):
*        Remove lib$cvt_dx_dx
*     04-MAY-1991 (ROE::BMC):
*        Remove redundant variables
*     04-MAY-1991 (ROE::BMC):
*        Update/correct comments
*     04-MAY-1991 (ROE::BMC):
*        Don't modify bad status returns from sub-routines
*     04-MAY-1991 (ROE::BMC):
*        Use ERR_REP to output errors rather than VALUE
*     07-JUN-1991 (REVAD::BDK):
*        Remove PATH and MESSID
*     22-AUG-1991 (REVAD::BDK):
*        Add REQUEST argument to DTASK_APPLIC
*     13-OCT-1992: add INCLUDE 'PAR_PAR'
*                  use DTASK__SYSNORM to avoid SS$NORMAL (RLVAD::AJC)
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
      EXTERNAL DTASK_APPLIC   ! address of action routine
      CHARACTER*(*) NAME      ! keyword of action required
      CHARACTER*(*) VALUE     ! command line parameter string

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'DTASK_CMN'

*  Local Variables:
      INTEGER ACTPTR                 ! pointer to the action in the
                                     ! action list
      INTEGER SCHEDTIME              ! time in milliseconds for
                                     ! rescheduled action
      INTEGER SEQ                    ! sequence number for stage of
                                     ! action
      INTEGER ACODE                  ! code number for the action in the
                                     ! parameter system
      CHARACTER*(SUBPAR__NAMELEN) ANAME   ! action name
      INTEGER ACTLEN                 ! length of ANAME
      INTEGER REQUEST                ! request code from the application
*.

      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   look-up the action in the parameter system
*
      CALL SUBPAR_FINDACT ( NAME, ACODE, STATUS )
      CALL SUBPAR_ACTNAME ( ACODE, ANAME, ACTLEN, STATUS )
*
*   parse the command-line
*
      CALL SUBPAR_CMDLINE ( ACODE, OBEY, VALUE, STATUS )

*   still OK so call the EXTERNAL routine that is an interface to the
*   application (arbitrarily use zero for action pointer, path and
*   message id).....
      IF ( STATUS .EQ. SAI__OK ) THEN
         ACTPTR = 0
         SEQ = 0
         CALL DTASK_APPLIC ( OBEY, ACODE, ANAME, ACTPTR, SEQ, VALUE,
     :     SCHEDTIME, REQUEST, STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN
*
*         Translate known application request returns.
*
            IF ( REQUEST .EQ. ACT__END ) THEN
               STATUS = DTASK__ACTCOMPLETE
            ELSE IF ( REQUEST .EQ. ACT__UNIMP ) THEN
               STATUS = DTASK__ACTUNIMP
            ELSE IF ( REQUEST .EQ. ACT__INFORM ) THEN
*
*            The value string contains message text for the user.
*
               STATUS = DTASK__ACTINFORM
            ELSE
*
*            Report the unexpected request.
*
               IF ( REQUEST .EQ. SAI__OK ) THEN
                  STATUS = DTASK__IVACTSTAT
                  CALL ERR_REP ( ' ', 'DTASK_OBEYDCL: application '//
     :              'returned illegal SAI__OK', STATUS )
               ELSE IF ( REQUEST .EQ. DTASK__SYSNORM ) THEN
                  STATUS = DTASK__IVACTSTAT
                  CALL ERR_REP ( ' ', 'DTASK_OBEYDCL: application '//
     :              'returned illegal SS$_NORMAL', STATUS )
               ELSE IF ( REQUEST .EQ. ACT__CANCEL ) THEN
                  STATUS = DTASK__IVACTSTAT
                  CALL ERR_REP ( ' ', 'DTASK_OBEYDCL: application '//
     :              'returned illegal ACT__CANCEL', STATUS )
               ENDIF
            ENDIF

         ENDIF

      ENDIF

      END
