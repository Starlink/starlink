      SUBROUTINE DTASK_DCLTASK ( DEVINIT, DTASK_APPLIC, STATUS )
*+
*  Name:
*     DTASK_DCLTASK

*  Purpose:
*     ADAM top subroutine for running tasks from DCL

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Description:
*     ADAM tasks use this as their main subroutine when run directly at
*     a terminal using the DCL RUN command. They can also be declared as
*     DCL foreign commands, in which case the command-line parameters
*     are picked-up.

*  Arguments:
*     DEVINIT=EXTERNAL (given)
*           application initialisation routine
*     DTASK_APPLIC=EXTERNAL (given)
*           application calling routine
*     STATUS=INTEGER

*  Algorithm:
*     Initialize the parameter system, and inform it that the task is
*     running at the terminal. Get the command-line and pass it to the
*     command-line parser. Finally, call the application code.
*     The application routines are passed as EXTERNAL to allow the DTASK
*     library to be in a shareable image.

*  Copyright:
*     Copyright (C) 1985-1987, 1989, 1991-1993 Science & Engineering
*     Research Council. All Rights Reserved.

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
*     11-NOV-1985 (REVAD::BDK):
*        Original
*     25-MAR-1986 (REVAD::BDK):
*        Change severity before signalling error
*     27-AUG-1986: make this a subroutine rather than the main program
*                  (REVAD::BDK)
*     17-JUN-1987 (REVAD::BDK):
*        Pass the action name 'RUN' in a variable
*     30-APR-1989 (AAOEPP::WFL):
*        Receive and pass on EXTERNAL DTASK_APPLIC
*     01-MAY-1989 (AAOEPP::WFL):
*        Only output significant part of value string
*     01-MAY-1989: use ERR_OUT more and LIB$SIGNAL less; don't change severity
*                  of status (AAOEPP::WFL)
*     02-MAY-1989 (AAOEPP::WFL):
*        Receive and call EXTERNAL DEVINIT
*     25-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     30-APR-1991: revise INCLUDE files and improve error checking
*                  (REVAD::BDK)
*     23-MAY-1991 (REVAD::BDK):
*        Ensure STATUS is bad before calling ERR
*     27-MAY-1991 (REVAD::BDK):
*        User ERR_REP
*     04-MAY-1991 (ROE::BMC):
*        Modify comments
*     04-MAY-1991 (ROE::BMC):
*        Modify calls to ERR_REP to be more meaningful
*     04-MAY-1991 (ROE::BMC):
*        Set status OK if action has completed
*     07-JUN-1991 (REVAD::BDK):
*        Change comments
*     14-OCT-1992: Get command line via DTASK_GTCMD
*                  Get ^STATUS via DTASK_ESETK
*                  INCLUDE PAR_PAR (RLVAD::AJC)
*     02-NOV-1992 (RLVAD::AJC):
*        Get task name returned from SUBPAR_ACTDCL
*     08-MAR-1993 (RLVAD::AJC):
*        Use MESSYS__VAL_LEN not MSG_VAL_LEN
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
      INCLUDE 'DTASK_ERR'
      INCLUDE 'MESSYS_PAR'

*  Arguments Given:
      EXTERNAL DEVINIT         ! application initialisation routine
      EXTERNAL DTASK_APPLIC    ! application calling routine

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER VALUE*(MESSYS__VAL_LEN)   ! command-line parameter string
      CHARACTER*(SUBPAR__NAMELEN) NAME     ! action name. This is declared as
                                      ! a variable to allow the symbolic
                                      ! debugger to change the action
                                      ! name
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Initialise the parameter system and HDS.
*
      CALL SUBPAR_ACTDCL ( NAME, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
*
*      Call the device-dependent initialisation routine in case this is
*      really a Dtask being debugged
*
         CALL DEVINIT ( STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
*
*         Get the command line
*
            CALL DTASK_GTCMD( VALUE, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
*
*           NAME has been set by the call to SUBPAR_ACTDCL
               CALL DTASK_OBEYDCL ( DTASK_APPLIC, NAME, VALUE, STATUS )
*
*            Check for bad completion status.
*
               IF ( STATUS .NE. DTASK__ACTCOMPLETE ) THEN
                  CALL DTASK_ESETK( 'STAT', STATUS )
                  CALL ERR_REP ( ' ', 'Application exit status ^STAT',
     :              STATUS )
                  IF ( VALUE .NE. ' ' ) THEN
                     CALL MSG_SETC ( 'VALUE', VALUE )
                     CALL ERR_REP ( ' ', '^VALUE', STATUS )
                  ENDIF
               ELSE
                  STATUS = SAI__OK
               ENDIF
            ENDIF

         ELSE
            CALL DTASK_ESETK( 'STAT', STATUS )
            CALL ERR_REP ( ' ', 'DTASK_DCLTASK: DEVINIT returned bad '//
     :        'status ^STAT', STATUS )
         ENDIF
      ELSE
         CALL DTASK_ESETK( 'STAT', STATUS )
         CALL ERR_REP ( ' ',
     :     'DTASK_DCLTASK: failed to start parameter system ^STAT',
     :     STATUS )
      ENDIF

      END
