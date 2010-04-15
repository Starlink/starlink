      SUBROUTINE SST_PARGL( STR, MXARG, ARGS, NARGS, STATUS )
*+
*  Name:
*     SST_PARGL

*  Purpose:
*     Parse a Fortran routine argument list.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_PARGL( STR, MXARG, ARGS, NARGS, STATUS )

*  Description:
*     The routine parses a list of Fortran 77 routine arguments,
*     splitting them into separate arguments and adding them to a list
*     held in a character array.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        The comma separated list of argument names to be parsed (case
*        insensitive).
*     MXARG = INTEGER (Given)
*        The maximum number of arguments in the output list (i.e. the
*        declared size of the ARGS array).
*     ARGS( MXARG ) = CHARACTER * ( * ) (Given and Returned)
*        The list of arguments. New values (in upper case) will be
*        added to this array starting at element NARGS + 1. If any
*        returned values are truncated because the argument length is
*        not sufficient, then an ellipsis '...' will be appended.
*     NARGS = INTEGER (Given and Returned)
*        The number of elements in the ARGS array. This will be updated
*        to reflect additions made by this routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise pointers to the position of the "current" argument
*     list element.
*     -  Loop to identify each element in the list.
*     -  Find the next element and check it is not blank.
*     -  Check that the output list will not overflow. Report an error
*     if it will.
*     -  Add the new name to the output list.
*     -  If truncation occurred, then append an ellipsis '...'.
*     -  Convert the returned value to upper case.
*     -  Increment the pointer to the start of the next input list
*     element and return to process it.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify it under
*     the terms of the GNU General Public License as published by the Free Software
*     Foundation; either version 2 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,but WITHOUT ANY
*     WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
*     PARTICULAR PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License along with
*     this program; if not, write to the Free Software Foundation, Inc., 59 Temple
*     Place,Suite 330, Boston, MA  02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-OCT-1989 (RFWS):
*        Original version.
*     8-AUG-1990 (RFWS):
*        Changed to check for output list overflow and character string
*        truncation.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) STR
      INTEGER MXARG

*  Arguments Given and Returned:
      CHARACTER * ( * ) ARGS( MXARG )
      INTEGER NARGS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER F                  ! First character position of name
      INTEGER I1                 ! Position of start of list element
      INTEGER I2                 ! Position of end of list element
      INTEGER L                  ! Last character position of name
      INTEGER LSTAT              ! Local status variable
      INTEGER N                  ! Position of ellipsis

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise pointers to the character positions of the start and end
*  of the "current" argument name.
      I1 = 1
      I2 = 0

*  Loop to identify each element in the argument list.
1     CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( ( I1 .LE. LEN( STR ) ) .AND. ( STATUS .EQ. SAI__OK ) ) THEN

*  Find the end of the next argument name (the character before the
*  next comma or end of string).
         I2 = INDEX( STR( I1 : ), ',' )
         IF ( I2 .EQ. 0 ) THEN
            I2 = LEN( STR )
         ELSE
            I2 = I2 + I1 - 2
         END IF

*  If the next name was found, then find the first and last characters
*  in it (excluding surrounding spaces).
         IF ( I1 .LE. I2 ) THEN
            CALL CHR_FANDL( STR( I1 : I2 ), F, L )

*  Check that the name is not all blank.
            IF ( F .LE. L ) THEN
               F = F + I1 - 1
               L = L + I1 - 1

*  Check that the output list will not overflow. Report an error if it
*  will.
               IF ( NARGS + 1 .GT. MXARG ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'MXARG', MXARG )
                  CALL ERR_REP( 'SST_PARGL_XS',
     :            'Exceeded internal storage (^MXARG elements) ' //
     :            'for routine argument names.', STATUS )

*  Extract the argument name and insert it in the output list.
               ELSE
                  NARGS = NARGS + 1
                  CALL CHR_COPY( STR( F : L ), .FALSE., ARGS( NARGS ),
     :                           LSTAT )

*  If the name was truncated, then append an ellipsis '...'.
                  IF ( LSTAT .NE. 0 ) THEN
                     N = MAX( 1, LEN( ARGS( 1 ) ) - 2 )
                     ARGS( NARGS )( N : ) = '...'
                  END IF

*  Convert the extracted name to upper case.
                  CALL CHR_UCASE( ARGS( NARGS ) )
               END IF
            END IF
         END IF

*  Increment the pointer to the start of the next element in the input
*  string and return to process it.
         I1 = I2 + 2
         GO TO 1
      END IF

      END
* @(#)sst_pargl.f   1.1   94/12/05 11:31:31   96/07/05 10:27:30
