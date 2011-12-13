      SUBROUTINE ARD1_ARGS( ELEM, L, ARGREQ, MXARG, ARGS, NARG, I, MORE,
     :                      STATUS )
*+
*  Name:
*     ARD1_ARGS

*  Purpose:
*     Copy an argument list from an element of an ARD description into
*     an array

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_ARGS( ELEM, L, ARGREQ, MXARG, ARGS, NARG, I, MORE,
*                     STATUS )

*  Description:
*     If a new argument list is being constructed, look for the opening
*     parenthesis. Then go on to read in all the arguments up to the
*     closing parenthesis or the end of the element (which ever comes
*     first). If the closing parenthesis has been reached report an
*     error if an incorrect number of arguments has been obtained. If
*     the closing parenthesis has not been reached, return in order for
*     a new element to be obtained so that the construction of the
*     argument list can be continued by a further call to this routine.

*  Arguments:
*     ELEM = CHARACTER * ( * ) (Given)
*        An element of an ARD description.
*     L = INTEGER (Given)
*        The index of the last non-blank character in ELEM.
*     ARGREQ = INTEGER (Given)
*        The total number of arguments required.
*     MXARG = INTEGER (Given)
*        The size of the ARGS array.
*     ARGS( MXARG ) = DOUBLE PRECISION (Given and Returned)
*        An array holding the argument values obtained so far, in the
*        order in which they occur within the ARD description.
*     NARG = INTEGER (Given and Returned)
*        The number of arguments stored in ARGS so far. It should be
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
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     16-FEB-1994 (DSB):
*        Original version.
*     18-JUL-2001 (DSB):
*        Modified for ARD version 2.0.
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
      INTEGER ARGREQ
      INTEGER MXARG

*  Arguments Given and Returned:
      DOUBLE PRECISION ARGS( MXARG )
      INTEGER NARG
      INTEGER I
      LOGICAL MORE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CC*1             ! Next character
      LOGICAL OK                 ! Was an argument value obtained?
      DOUBLE PRECISION VALUE     ! An argument value

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
*  arguments read so far to zero. Report an error if the current
*  statement should not have an argument list.
         IF( CC .EQ. '(' ) THEN
            NARG = 0

            IF( ARGREQ .EQ. 0 ) THEN
               STATUS = ARD__ARGS
               CALL ERR_REP( 'ARD1_ARGS_ERR1', 'Unnecessary argument '//
     :                       'list found.', STATUS )
            END IF

*  Report an error for any other non-blank character. CC will be blank
*  if no non-blank characters could be found in ELEM. In this case NEW
*  will be returned unchanged (i.e. .TRUE.) and this routine will be
*  re-entered with a new element.
         ELSE IF( CC .NE. ' ' ) THEN
            STATUS = ARD__ARGS
            CALL ERR_REP( 'ARD1_ARGS_ERR2', 'No argument list found.',
     :                    STATUS )
         END IF

*  If an argument list has been started...
      ELSE

*  ...but not completed, attempt to read argument value from the
*  current element until the end of the element, or the end of the
*  argument list is encountered.
         DO WHILE( I .LE. L .AND. MORE .AND. STATUS .EQ. SAI__OK )

*  Read the next argument.
            CALL ARD1_GTARG( 0, 0, ELEM, L, I, OK, MORE, VALUE, STATUS )

*  If an argument was obtained, store it in the returned array. Report
*  an error if there is no room for any more arguments.
            IF( OK ) THEN
               NARG = NARG + 1
               IF( NARG .LE. MXARG ) THEN
                  ARGS( NARG ) = VALUE

               ELSE IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = ARD__ARGS
                  CALL MSG_SETI( 'MX', MXARG )
                  CALL ERR_REP( 'ARD1_ARGS_ERR3', 'Argument list '//
     :                         'contains too many values.', STATUS )
               END IF

*  If the end of the argument list has been reached, report an error if
*  the number of arguments obtained is incorrect.
            ELSE IF( .NOT. MORE ) THEN

               IF( NARG .NE. ARGREQ .AND. STATUS .EQ. SAI__OK ) THEN
                  STATUS = ARD__ARGS
                  CALL MSG_SETI( 'NARG', NARG )
                  CALL MSG_SETI( 'RARG', ARGREQ )
                  CALL MSG_SETC( 'DESC', ELEM )
                  CALL ERR_REP( 'ARD1_ARGS_ERR4', 'Argument list '//
     :                       'contains wrong number of values.',
     :                       STATUS )
               END IF

            END IF

         END DO

      END IF

      END
