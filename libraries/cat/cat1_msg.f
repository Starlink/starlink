      SUBROUTINE CAT1_MSG (PARAM, MESSGE, STATUS)
*+
*  Name:
*     CAT1_MSG
*  Purpose:
*     Report a message.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_MSG (PARAM, MESSGE; STATUS)
*  Description:
*     Report a message.
*
*     This routine will only execute if the running status is ok (that
*     is, it behaves like a normal CAT or ADAM routine).
*  Arguments:
*     PARAM  =  CHARACTER*(*) (Given)
*        Name of the external parameter with which the message is
*        associated.
*     MESSGE  =  CHARACTER*(*) (Given)
*        Message to be reported.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Report the message if not in 'quiet' mode.
*
*     Note: this routine contains code for reporting the message via
*     both the ADAM error system and a simple Fortran PRINT statement.
*     Whichever is not required should be commented out.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     15/6/94  (ACD): Original version.
*     23/11/94 (ACD): Prefixed message with '!(Info.)'.
*     14/2/96  (ACD): Fixed an illegal bug in the ADAM version; a
*        variable length character argument was concatenated.
*     4/7/96   (ACD): Inserting missing argument in call to MSG_OUT
*        (this bug was presumably introduced by the fix of 14/2/96
*        because the routine worked previously).
*     10/12/96 (ACD): Passed argument PARAM rather than a blank string
*        to MSG_OUT.
*     1/7/99   (ACD): Added a space after '!(Info.)'.
*     21/12/99 (ACD): Modified to only report if messages is not in
*        'quiet' mode.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Global Variables:
      INCLUDE 'CAT1_CTRL_CMN'     ! Flags to control CAT.
*  Arguments Given:
      CHARACTER
     :  PARAM*(*),
     :  MESSGE*(*)
*  Status:
      INTEGER STATUS              ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  MSGLEN,    ! length of MESSGE (excl. trail. blanks).
     :  BUFPOS     !   "    "  BUFFER ( "  .   "  .   "   ).
      CHARACTER
     :  BUFFER*80  ! Output buffer.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check whether in quiet mode; messages are only reported if
*       CAT is not in quiet mode.

         IF (.NOT. QUIET__CAT1) THEN

            BUFFER = ' '
            BUFPOS = 0

            CALL CHR_PUTC ('!(Info.) ', BUFFER, BUFPOS)

            IF (MESSGE .NE. ' ') THEN
               MSGLEN = CHR_LEN(MESSGE)
               CALL CHR_PUTC (MESSGE(1 : MSGLEN), BUFFER, BUFPOS)
            END IF

*
*          Code for reporting the message via both the ADAM message
*          system and a Fortran PRINT statement is included below.
*          Comment out whichever is not required.

            CALL MSG_OUT (PARAM, BUFFER(1 : BUFPOS), STATUS)

C           PRINT2000, BUFFER(1 : BUFPOS)
C2000       FORMAT(1X, A / )

         END IF

      END IF

      END
