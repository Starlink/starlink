      SUBROUTINE DTASK_DTASK ( DEVINIT, DTASK_APPLIC, STATUS )
*+
*  Name:
*     DTASK_DTASK

*  Purpose:
*     Top subroutine for tasks run as separate processes

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Description:
*     Initialise for this mode of operation, then wait for incoming
*     messages. When one is received, identify the CONTEXT (GET, SET,
*     OBEY, CANCEL or CONTROL) and pass the message information to the
*     corresponding routine.
*     If at all possible, recover from any error conditions and revert
*     to waiting for more messages.

*  Algorithm:
*     Find the name of the process running the program, and use the name
*     while initialising into the ADAM message and parameter systems.
*     Then loop receiving instructions and acting on them.
*     Receipt of a GET/SET/OBEY/CANCEL/CONTROL command starts a new
*     "transaction", identified by MESSID. The transaction and its
*     associated communications are closed-down when this task returns
*     the final message for the GET/SET/OBEY/CANCEL/CONTROL.

*  Copyright:
*     Copyright (C) 1984-1986, 1989-1993 Science & Engineering Research
*     Council. Copyright (C) 1995, 2001 Central Laboratory of the
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
*     John Cooke (REVS::JAC) 01May84
*     BDK: B.D.Kelly (ROE)
*     WFL: W.F.Lupton (AAO)
*     AJC: A.J.Chipperfield (Starlink, RAL)
*     BKM: B.K.McIlwrath (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     08-MAY-1984: added initial/final acknowledgment for OBEY
*     17-MAY-1984: dtask rescheduling started
*     19-JUN-1984: general rewrite started
*     26-OCT-1984 (REVAD::BDK):
*        Add full parameter system
*     04-DEC-1984 (REVAD::BDK):
*        Report error on exit
*     11-DEC-1984 (REVAD::BDK):
*        Report G/S/O/C errors without exiting
*     11-DEC-1984 (REVAD::BDK):
*        Use DTASK_ACKNOW
*     20-JUN-1985: report bad status on message received without exiting
*                       (REVAD::BDK)
*     25-MAR-1986 (REVAD::BDK):
*        Change severity before signalling errors
*     22-AUG-1986 (REVAD::BDK):
*        Report failure in ACKNOW without exiting
*     27-AUG-1986: make into subroutine instead of main program
*                 (REVAD::BDK)
*     27-AUG-1986 (REVA::ADAM):
*        Moved illcontext from adam__ to dtask__
*     27-AUG-1986 (REVA::ADAM):
*        Forgot to include dterrs!
*     30-APR-1989 (AAOEPP::WFL):
*        Enable and disable reporting
*     30-APR-1989 (AAOEPP::WFL):
*        Receive and pass down EXTERNAL DTASK_APPLIC
*     01-MAY-1989 (AAOEPP::WFL):
*        Use ERR_OUT rather than LIB$SIGNAL and TYPE
*     01-MAY-1989 (AAOEPP::WFL):
*        Don't change severity of failure status
*     02-MAY-1989 (AAOEPP::WFL):
*        Receive and call EXTERNAL DEVINIT
*     01-MAR-1990: initialise VALUE, remove DONE, remove MSG_START and
*                  ERR_START calls and localise area where MSG and ERR
*                  routines can work, correct comments (AAOEPP::WFL)
*     25-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     30-APR-1991 (REVAD::BDK):
*        Use MESSYS__TNAME
*     09-MAY-1991 (REVAD::BDK):
*        Tidy error reporting
*     09-MAY-1991 (REVAD::BDK):
*        Change order of arguments to OBEY and CANCEL
*     27-MAY-1991 (REVAD::BDK):
*        Use ERR_REP, don't call ERR_ and MSG_STOP
*     04-JUN-1991 (ROE::BMC):
*        Remove redundant variables
*     04-JUN-1991 (ROE::BMC):
*        Update/correct comments
*     04-JUN-1991 (ROE::BMC):
*        Use MYNAME length obtained instead of MSG_SETC
*     04-JUN-1991: Call ERR_CLEAR before DTASK_ACKNOW as the latter will
*                  terminate communications on this MESSID (ROE::BMC)
*     04-JUN-1991 (ROE::BMC):
*        Use DTASK_COMSHUT
*     07-JUN-1991: move error reporting at end of task inside the DO
*                  loop. Change comments, change AKEY to NAME
*                  (REVAD::BDK)
*     10-JUN-1991: rewrite message receiving and handling section
*                  (REVAD::BDK)
*     07-APR-1992 (RLVAD::BKM):
*        Add CONTROL context to prologue and comments
*     15-JUL-1992 (RLVAD::AJC):
*        Correct and split end messages
*     14-OCT-1992: Mods for portability
*                  Get ^STATUS via DTASK_ESETK
*                  INCLUDE PAR_PAR (RLVAD::AJC)
*     16-NOV-1992 (RLVAD::AJC):
*        Remove unused declarations ISTAT and BADNAME
*     08-MAR-1993 (RLVAD::AJC):
*        Use MESSYS__VAL_LEN - remove include DDMSG
*     23-AUG-1993 (RLVAD::AJC):
*        Replace PAR_PAR with SUBPAR_SYS
*                  Replace PAR__SZNAM with SUBPAR__NAMELEN  (RLVAD::AJC)
*     22-SEP-1993 (RLVAD::BKM):
*        Prevent attempts to use MESSYS after MSP errors
*     25-SEP-1995 (RLVAD:AJC):
*        Increase size of NAME to allow ACTNAME:PARNAME
*     31-JUL-1995: Add TEMPORARY call to DTASK_DECBUG to correct a compiler bug
*                  on Alpha/OSF Fortran. (BKM)
*     11-JUN-2001: Call AMS_RECEIVE (FAMS) directly
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
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_ERR'
      INCLUDE 'ADAM_DEFNS'
      INCLUDE 'DTASK_ERR'

*  Arguments Given:
      EXTERNAL DEVINIT        ! application initialisation routine
      EXTERNAL DTASK_APPLIC   ! application calling routine

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER MSGSTATUS                 ! status of received message
      INTEGER CONTEXT                   ! GET, SET, OBEY, CANCEL or CONTROL
      INTEGER MSGLEN                    ! Length of message value
      CHARACTER*( 2*SUBPAR__NAMELEN + 1 ) NAME
                                        ! action keyword for OBEY, CANCEL and
                                        ! CONTROL, parameter keyword for
                                        ! GET and SET
      CHARACTER*(MESSYS__VAL_LEN) VALUE     ! command line parameter string
      INTEGER PATH                      ! path to commanding process
      INTEGER MESSID                    ! identifier for transaction
      CHARACTER*(MESSYS__TNAME) MYNAME  ! name of this task
      INTEGER NLENGTH                   ! actual length of MYNAME
*.

      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Initialise VALUE in case DEVINIT fails.
*
      VALUE = ' '
*
*   Obtain process name.
*
      CALL DTASK_PRCNAM ( MYNAME, NLENGTH, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP ( ' ', 'DTASK_DTASK: failed to get process name,'
     :    , STATUS )
         CALL DTASK_ESETK ( 'STAT', STATUS )
         CALL ERR_REP ( ' ', '^STAT', STATUS )
      ELSE
*
*      Initialise intertask communication (MESSYS), parameter system
*      (SUBPAR) and tasking support library (TASK)
*
         CALL DTASK_INIT ( MYNAME, NLENGTH, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP ( ' ', MYNAME(1:NLENGTH) //
     :        ' task initialisation failed,', STATUS )
            CALL DTASK_ESETK ( 'STAT', STATUS )
            CALL ERR_REP ( ' ', '^STAT', STATUS )
         ELSE
*
*         Call the device-dependent initialisation routine.
*
            CALL DEVINIT ( STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP ( ' ', MYNAME(1:NLENGTH) //
     :           ' application initialisation failed,', STATUS )
               CALL DTASK_ESETK ( 'STAT', STATUS )
               CALL ERR_REP ( ' ', '^STAT', STATUS )
            ENDIF
         ENDIF
      ENDIF
*
*   Loop to receive and reply to messages.
*   The task is "never" expected to come out of this DO loop.
*   It is assumed the task is finally removed by being killed.
*
      DO WHILE ( STATUS .EQ. SAI__OK )
*
*      Switch error reporting to the terminal and error file
*
         CALL SUBPAR_PUTPATH ( 0, 0, STATUS )
*
*      Wait for a message
*
         MSGSTATUS = SAI__OK
         CALL FAMS_RECEIVE( MESSYS__INFINITE, MSGSTATUS, CONTEXT,
     :     NAME, MSGLEN, VALUE, PATH, MESSID, STATUS )
         IF( MSGLEN .LT. MESSYS__VAL_LEN )
     :     VALUE( MSGLEN+1: ) = ' '

         IF ( STATUS .NE. SAI__OK ) THEN
            CONTINUE

         ELSE IF ( MSGSTATUS .EQ. SAI__OK ) THEN
*
*         Incoming GET/SET/OBEY/CANCEL/CONTROL
*
            CALL DTASK_GSOC ( DTASK_APPLIC, PATH, MESSID, CONTEXT, NAME,
     :        VALUE, STATUS )
* TEMPORARY call (and routine) to fix a compiler bug on Alpha/OSF (31/07/95)
            CALL DTASK_DECBUG
         ELSE IF ( MSGSTATUS .EQ. MESSYS__EXTINT )  THEN
*
*         Should not happen in a task.
*
            CALL ERR_REP ( ' ', 'Invalid event EXTINT has occurred',
     :        MSGSTATUS )

         ELSE IF ( MSGSTATUS .EQ. MESSYS__RESCHED ) THEN
*
*         A timer has expired
*
            CALL DTASK_TIMEOUT ( DTASK_APPLIC, VALUE, STATUS )

         ELSE IF ( MSGSTATUS .EQ. MESSYS__ASTINT ) THEN
*
*         A message has arrived from an AST routine
*
            CALL DTASK_ASTINT ( DTASK_APPLIC, NAME, VALUE, STATUS )

         ELSE IF ( MSGSTATUS .EQ. MESSYS__KICK ) THEN
*
*         A message has come from this application
*
            CALL DTASK_KICK ( DTASK_APPLIC, NAME, VALUE, STATUS )


*         On Unix MSP errors are negative and may be reported in MSGSTATUS
         ELSE IF ( MSGSTATUS .GT. 0) THEN
*
*         Check if message is from a subsidiary task
*
            CALL DTASK_SUBSID ( DTASK_APPLIC, PATH, MESSID, CONTEXT,
     :        NAME, MSGSTATUS, VALUE, STATUS )

         ELSE
            STATUS = MSGSTATUS

         ENDIF

         IF ( STATUS .NE. SAI__OK ) THEN
*
*          Unrecoverable error, the task will exit.
*
            IF ( VALUE .EQ. ' ' ) THEN
               CALL ERR_REP ( ' ', MYNAME(1:NLENGTH) //
     :           ' unexpected failure,', STATUS )
               CALL DTASK_ESETK ( 'STAT', STATUS )
               CALL ERR_REP ( ' ', '^STAT', STATUS )
            ELSE
               CALL MSG_SETC ( 'VALUE', VALUE )
               CALL ERR_REP ( ' ', MYNAME(1:NLENGTH) //
     :           ' unexpected failure,', STATUS )
               CALL DTASK_ESETK ( 'STAT', STATUS )
               CALL ERR_REP ( ' ', '^STAT', STATUS )
               CALL MSG_SETC ( 'VALUE', VALUE )
               CALL ERR_REP ( ' ', 'VALUE: ^VALUE', STATUS )
            ENDIF
         ENDIF

      ENDDO

      END
