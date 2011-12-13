      SUBROUTINE DTASK_ASTHDLR ( ASTPARM )
*+
*  Name:
*     DTASK_ASTHDLR

*  Purpose:
*     AST handler for timed reschedules

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     Invoked by timer completion
*     interrupts.

*  Description:
*     Signals that a timer has completed indicating that an action is
*     due for rescheduling.

*  Arguments:
*     ASTPARM=INTEGER (given)
*           packed action number plus action counter passed by value

*  Algorithm:
*     Interpret the AST parameter. This has been passed by VALUE, which
*     is necessary to ensure it is unique to this timer event. The
*     parameter contains two two-byte integers.

*  Copyright:
*     Copyright (C) 1984, 1991, 1994 Science & Engineering Research
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
*     {enter_new_authors_here}

*  History:
*     22-MAY-1984 (REVA::ADAM]):
*        First insertion
*     25-MAY-1984 (REVA::ADAM]):
*        Astparm access mode to %loc
*     20-JUN-1984 (REVA::ADAM):
*        Two 16-bit parameters; new error system
*     20-JUN-1984 (REVA::ADAM):
*        Added save of ast parm itself for reference
*     21-JUN-1984 (REVA::ADAM):
*        Added interrupt flag
*     25-APR-1991 (REVAD::BDK):
*        Rearrange INCLUDE files and add comments
*     30-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files, remove REQASTPAR
*     13-MAY-1991 (REVAD::BDK):
*        Dont disable ASTs, use MESSYS_RESMSG
*     07-JUN-1991 (REVAD::BDK):
*        Change comments
*     28-JUN-1994 (RAL::AJC):
*        Version for Unix Version 2 timer system
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
*  Arguments Given:
      INTEGER ASTPARM           ! the VMS AST parameter passed by value
*  Local Variables:
      INTEGER ASTVAL            ! value of ASTPARM
      CHARACTER*4 VALUE         ! message sent to application
      INTEGER LENGTH            ! length of message sent
      INTEGER STATUS            ! local status
*    Data structures for ADAM:
      EQUIVALENCE ( ASTVAL, VALUE )
*.

*
*   Copy the timer identifier from the AST parameter.
*
      ASTVAL = ASTPARM
*
*   Inform the message system.
*
      STATUS = SAI__OK
      LENGTH = 4
      CALL FAMS_RESMSG( LENGTH, VALUE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'DTASK_ASTHDLR1',
     :   'AST handler failed to send timed reschedule message', STATUS )
      ENDIF

      END
