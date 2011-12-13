      SUBROUTINE GRP1_KWILD( SLOT, GEXP, STATUS )
*+
*  Name:
*     GRP1_KWILD

*  Purpose:
*     Enclose NAME_TOKEN characters within kernel delimiters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_KWILD( SLOT, GEXP, STATUS )

*  Description:
*     Any NAME_TOKEN characters within the supplied group expression are
*     enclosed within a pair of OPEN_KERNEL and CLOSE_KERNEL control
*     characters.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The slot number of the group whose control characters are to be
*        used.
*     GEXP = CHARACTER * ( * ) (Given and Returned)
*        The group expression,
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     26-JAN-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_CONST'        ! GRP_ private constants
      INCLUDE 'GRP_PAR'          ! GRP_ public constants
      INCLUDE 'GRP_ERR'          ! GRP_ error constants

*  Arguments Given:
      INTEGER SLOT

*  Arguments Given and Returned:
      CHARACTER GEXP*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Function giving used length of a string

*  Local Variables:
      LOGICAL AGAIN              ! Has end of string been reached?
      INTEGER I                  ! Index of current input character
      INTEGER INLEN              ! Used length of the input string.
      CHARACTER KERNEL*3         ! Enclosed NAME_TOKEN character
      LOGICAL KOPOK              ! Is the opening kernel character ok?
      LOGICAL KCLOK              ! Is the closing kernel character ok?
      LOGICAL MNMOK              ! Is the NAME_TOKEN character ok?
      INTEGER NMTOK              ! Offset to next NAME_TOKEN character
      INTEGER OLEN               ! Used length of the output string
      CHARACTER OUT*(GRP__SZGEX) ! The output group expression
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN


*  Get the required syntax characters for the group. Place them into a
*  character string so that the string holds the NAME_TOKEN character
*  enclosed between the opening and closing kernel characters.
      CALL GRP1_CONC( SLOT, GRP__POPKC, KERNEL( 1 : 1 ), KOPOK,
     :                STATUS )
      CALL GRP1_CONC( SLOT, GRP__PMNMC, KERNEL( 2 : 2 ), MNMOK,
     :                STATUS )
      CALL GRP1_CONC( SLOT, GRP__PCLKC, KERNEL( 3 : 3 ), KCLOK,
     :                STATUS )

*  Return without further action if no NAME_TOKEN control character
*  is currently defined.
      IF( MNMOK ) THEN

*  Report an error and abort if either of the kernel control characters
*  is not defined.
         IF( .NOT.( KCLOK .AND. KOPOK ) .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = GRP__INVEL
            CALL ERR_REP( 'GRP1_KWILD_ERR1',
     :                    'GRP1_KWILD: Modification elements cannot '//
     :                    'be used because the application has not '//
     :                    'defined any opening or closing kernel '//
     :                    'control characters.', STATUS )
            GO TO 999
         END IF

*  Save the used length of the input string.
         INLEN = CHR_LEN( GEXP )

*  Initialise the current character pointer.
         I = 1

*  Initialise the output string and the position of the last non-blank
*  character in the output string.
         OUT = ' '
         OLEN = 0

*  Loop round until no more NAME_TOKENs are found, or the end of the
*  string is encountered.
         AGAIN = .TRUE.
         DO WHILE( AGAIN )

*  Look for a NAME_TOKEN character.
            NMTOK = INDEX( GEXP( I : INLEN ), KERNEL( 2 : 2 ) )

*  If no NAME_TOKEN was found, leave the loop.
            IF( NMTOK .EQ. 0 ) THEN
               AGAIN = .FALSE.

*  Otherwise, append the string which comes before the NAME_TOKEN
*  character (if any) to the output string.
            ELSE
               IF( NMTOK .GT. 1 ) THEN
                  CALL CHR_APPND( GEXP( I : I + NMTOK - 2 ), OUT, OLEN )
               END IF

*  Now append the enclosed NAME_TOKEN to the output string.
               CALL CHR_APPND( KERNEL, OUT, OLEN )

*  Increment the index of the current character to point to the
*  character after the NAME_TOKEN in the input string.
               I = I + NMTOK

*  If the end of the string has been reached, leave the loop.
               IF( I .GT. INLEN ) AGAIN = .FALSE.

            END IF

         END DO

*  Now append the remainder of the input string (if any) to the output
*  string.
         IF( I .LE. INLEN ) CALL CHR_APPND( GEXP( I : INLEN ), OUT,
     :                                      OLEN )

*  Return the new group expression.
         GEXP = OUT

      END IF

 999  CONTINUE

      END
