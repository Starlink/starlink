      SUBROUTINE ARD1_GTARG( FRM, AXIS, ELEM, L, I, OK, MORE, VALUE,
     :                       STATUS )
*+
*  Name:
*     ARD1_GTARG

*  Purpose:
*     Read a numerical argument value from an argument list

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_GTARG( FRM, AXIS, ELEM, L, I, OK, MORE, VALUE, STATUS )

*  Description:
*     If the next non-blank character in the element is a closing
*     parenthesis, then MORE and OK are both returned FALSE to indicate
*     that the end of the argument list has been found. Otherwise, the
*     string between character I and the next delimiter, closing
*     parenthesis, or end of string (which ever comes first) is
*     converted into a numerical value. I is returned pointing to the
*     next character to be checked.

*  Arguments:
*     FRM = INTEGER (Given)
*        An AST Frame. The AST_UNFORMAT method of this Frame is used to
*        obtained the numerical value from the formatted text string.
*     AXIS = INTEGER (Given)
*        The index of the Axis within FRM to which the next value relates.
*        A value of 0 can be supplied to indicate that FRM should be
*        ignored, in which case the value is assumed to be formatted
*        as a simple floating point value.
*     ELEM = CHARACTER * ( * ) (Given)
*        An element of an ARD description.
*     L = INTEGER (Given)
*        The index of the last non-blank character in ELEM.
*     I = INTEGER (Given and Returned)
*        The index of the next character to be checked in ELEM.
*     OK = LOGICAL (Returned)
*        .TRUE. if an argument value was succesfully obtained. .FALSE.
*        otherwise.
*     MORE = LOGICAL (Returned)
*        Returned .FALSE. if a closing parenthesis was found. .FALSE.
*        otherwise.
*     VALUE = DOUBLE PRECISION (Returned)
*        The argument value (undefined if OK is returned .FALSE.).
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
*     17-FEB-1994 (DSB):
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
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants

*  Arguments Given:
      INTEGER FRM
      INTEGER AXIS
      CHARACTER ELEM*(*)
      INTEGER L

*  Arguments Given and Returned:
      INTEGER I

*  Arguments Returned:
      LOGICAL OK
      LOGICAL MORE
      DOUBLE PRECISION VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :  ATTR*20,                ! AST attribute name
     :  DOM*80,                 ! Domain
     :  LAB*80                  ! Axis label

      INTEGER
     :  DELIM,                   ! Offset from I to the next delimiter
     :  END,                     ! Offset from I to end of argument
     :  IAT,                     ! Used length fo a string
     :  J,                       ! Index of last character in argument
     :  NC,                      ! Number of characters read
     :  PAREN                    ! Offset from I to next ")" character
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the returned flags.
      OK = .FALSE.
      MORE = .TRUE.

*  Find the next non-blank character in ELEM.
 10   CONTINUE
      IF( ELEM( I : I ) .EQ. ' ' ) THEN
         IF( I .LT. L ) THEN
            I = I + 1
            GO TO 10
         END IF

*  If a non-blank character is found...
      ELSE

*  If the first non-blank character is a closing parenthesis, the
*  argument list is complete. Set the corresponding flag and increment
*  the pointer to the next character.
         IF( ELEM( I : I ) .EQ. ')' ) THEN
            MORE = .FALSE.
            I = I + 1

*  If the first non-blank character is an argument delimiter, increment
*  the pointer to the next character. The next pass through this routine
*  will pick up any arguments following the delimiter.
         ELSE IF( ELEM( I : I ) .EQ. ',' ) THEN
            I = I + 1

*  If the first non-blank character is neither a delimiter nor a closing
*  parenthesis, assume it is the first character of an argument value.
         ELSE

*  The last character in the argument value preceeds the next argument
*  delimiter or closing parenthesis (which ever comes first).
            DELIM = INDEX( ELEM( I : ), ',' )
            PAREN = INDEX( ELEM( I : ), ')' )

            IF( DELIM .EQ. 0 ) THEN
               END = PAREN

            ELSE IF( PAREN .EQ. 0) THEN
               END = DELIM

            ELSE
               END = MIN( PAREN, DELIM )

            END IF

*  If one or the other was found, calculate the index of the last
*  character before the sooner of the two.
            IF( END .GT. 0 ) THEN
               J = END + I - 2

*  If neither a closing parenthesis nor an argument delimiter was found,
*  assume the argument value extends to (and includes) the last
*  character in the string.
            ELSE
               J = L

            END IF

*  If the string is not null...
            IF( J .GE. I ) THEN

*  and if it is not blank...
               IF( ELEM( I : J ) .NE. ' ' ) THEN

*  Attempt to convert it to a numerical value. If no AST Axis is
*  specified...
                  IF( AXIS .LT. 1 ) THEN
                     CALL CHR_CTOD( ELEM( I : J ), VALUE, STATUS )

*  Otherwise, use the Unformat method of the supplied Frame.
                  ELSE
                     NC = AST_UNFORMAT( FRM, AXIS, ELEM( I : J ),
     :                                   VALUE, STATUS )
                     IF( NC .NE. J - I + 1 .AND.
     :                   STATUS .EQ. SAI__OK ) THEN
                        ATTR = 'Label('
                        IAT = 6
                        CALL CHR_PUTI( AXIS, ATTR, IAT )
                        CALL CHR_APPND( ')', ATTR, IAT )
                        LAB = AST_GETC( FRM, ATTR( : IAT ), STATUS )
                        DOM = AST_GETC( FRM, 'DOMAIN', STATUS )
                        IF( STATUS .EQ. SAI__OK ) STATUS = ARD__BADAR
                     END IF
                  END IF

*  Set the OK flag if a vlaue was obtained succesfully.
                  OK = ( STATUS .EQ. SAI__OK )

*  If the value was bad, display it.
                  IF( STATUS .NE. SAI__OK ) THEN

                     IF( AXIS .GE. 1 ) THEN
                        CALL MSG_SETC( 'L', '''' )
                        CALL MSG_SETC( 'L', DOM )
                        CALL MSG_SETC( 'L', ' '//LAB )
                        CALL MSG_SETC( 'L', '''' )
                     ELSE
                        CALL MSG_SETC( 'L', '''Axis' )
                        CALL MSG_SETC( 'L', ' ' )
                        CALL MSG_SETI( 'L', AXIS )
                        CALL MSG_SETC( 'L', '''' )
                     END IF

                     CALL MSG_SETC( 'DESC', ELEM( I : J ) )
                     CALL ERR_REP( 'ARD1_GTARG_ERR1', 'The string '//
     :                             '''^DESC'' cannot be interpreted '//
     :                             'as a ^L value.', STATUS )

                  END IF

*  Return the index of the next following the end of the argument value.
                  I = J + 1

*  Report an error if blank argument value was found.
               ELSE
                  STATUS = ARD__BADAR
                  CALL ERR_REP( 'ARD1_GTARG_ERR2', 'Blank argument '//
     :                          'found.', STATUS )
               END IF

*  Report an error if a null argument value was found
            ELSE
               STATUS = ARD__BADAR
               CALL ERR_REP( 'ARD1_GTARG_ERR3', 'Null argument found.',
     :                       STATUS )

            END IF

         END IF

      END IF

      END
