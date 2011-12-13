      SUBROUTINE ARD1_ARGS2( ELEM, L, IGRP, NARG, I, MORE, STATUS )
*+
*  Name:
*     ARD1_ARGS2

*  Purpose:
*     Copy an argument list from an element of an ARD description into
*     a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_ARGS2( ELEM, L, IGRP, NARG, I, MORE, STATUS )

*  Description:
*     If a new argument list is being constructed, look for the opening
*     parenthesis. Then go on to copy the remaining text from the ARD
*     expression into the returned group, up to the closing parenthesis
*     or the end of the element (which ever comes first). If
*     the closing parenthesis has not been reached, return in order for
*     a new element to be obtained so that the construction of the
*     argument list can be continued by a further call to this routine.

*  Arguments:
*     ELEM = CHARACTER * ( * ) (Given)
*        An element of an ARD description.
*     L = INTEGER (Given)
*        The index of the last non-blank character in ELEM.
*     IGRP = INTEGER (Given)
*        The GRP identifier for the group in which to store any remaining
*        text in ELEM.
*     NARG = INTEGER (Given and Returned)
*        The number of elements stored in IGRP so far. It should be
*        supplied equal to -1 if the opening parenthesis which marks
*        the start of the argument list has not yet been found.
*     I = INTEGER (Given and Returned)
*        The index within ELEM of the next character to be checked.
*     MORE = LOGICAL (Given and Returned)
*        .TRUE. if an argument list is currently being assembled.
*        Returned .FALSE. if the argument list is completed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-JUL-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants

*  Arguments Given:
      CHARACTER ELEM*(*)
      INTEGER L
      INTEGER IGRP

*  Arguments Given and Returned:
      INTEGER NARG
      INTEGER I
      LOGICAL MORE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CC*1             ! Next character
      INTEGER DEPTH              ! Depth of parenthesis nesting
      INTEGER START              ! Start of the text to be stored
      INTEGER J                  ! ENd of the text to be stored
      SAVE DEPTH
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If this is a new argument list, all we do this time through is
*  initialise things and find the opening parenthesis which marks the
*  start of the argument list. The routine then exists and is called
*  again to continue reading the argument list.
      IF( NARG .EQ. -1 ) THEN

*  Find the next non-blank character in ELEM. This should be the opening
*  parenthesis which marks the start of the argument list.
         CC = ELEM( I : I )
         DO WHILE( CC .EQ. ' ' .AND. I .LT. L )
            I = I + 1
            CC = ELEM( I : I )
         END DO

*  Increment the index of the next character to be checked so that it
*  refers to the first character after the opening parenthesis.
         I = I + 1

*  If the next non-blank character is an opening parenthesis, indicate
*  that the argument list has been started by setting the number of
*  arguments read so far to zero.
         IF( CC .EQ. '(' ) THEN
            NARG = 0

*  Report an error for any other non-blank character. CC will be blank
*  if no non-blank characters could be found in ELEM. In this case NEW
*  will be returned unchanged (i.e. .TRUE.) and this routine will be
*  re-entered with a new element.
         ELSE IF( CC .NE. ' ' ) THEN
            STATUS = ARD__ARGS
            CALL ERR_REP( 'ARD1_ARGS2_ERR1', 'No argument list found.',
     :                    STATUS )
         END IF

*  Indicate we are at parenthesis nesting depth zero.
         DEPTH = 0

*  If an argument list has been started...
      ELSE

*  ...but not completed, attempt to read characters from the
*  current element until a closing parenthesis at depth zero is
*  found.
         START = I
         DO WHILE( I .LE. L .AND. MORE .AND. STATUS .EQ. SAI__OK )

            IF( ELEM( I: I ) .EQ. '(' ) THEN
               DEPTH = DEPTH + 1
            ELSE IF( ELEM( I: I ) .EQ. ')' ) THEN
               IF( DEPTH .EQ. 0 ) THEN
                  MORE = .FALSE.
               ELSE
                  DEPTH = DEPTH - 1
               END IF
            END IF

            I = I + 1
         END DO

*  Determine the index of the last usable character.
         IF( MORE ) THEN
            J = I - 1
         ELSE
            J = I - 2
         END IF

*  Store the string as a new element in the returned group.
         IF( START .LE. J ) THEN
            IF( ELEM( START : J ) .NE. ' ' ) THEN
               NARG = NARG + 1
               CALL GRP_PUT( IGRP, 1, ELEM( START : J ), 0, STATUS )
            END IF
         END IF

      END IF

      END
