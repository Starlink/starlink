      SUBROUTINE CHR_RJUST( STRING )
*+
*  Name:
*     CHR_RJUST

*  Purpose:
*     Right-justify a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_RJUST( STRING )

*  Description:
*     The given string is right-justified by filling out the spaces
*     between words with additional blank space. The right margin is
*     taken as the declared length of the given string. Unprintable
*     characters are interpreted as blanks.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        The string to be right-justified and returned.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (Starlink)
*     PCTR: P.C.T. Rees (Starlink)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-APR-1991 (RFWS):
*        Original version.
*     11-APR-1991 (PCTR):
*        Commented code and completed tests.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given and Returned:
      CHARACTER * ( * ) STRING

*  Local Variables:
      LOGICAL GAP                ! In an inter-word gap?
      LOGICAL SPACE              ! Character is space (or non-printing)?
      LOGICAL START              ! At start of (non-blank) string yet?

      INTEGER I                  ! Loop counter for character positions
      INTEGER ICOMP              ! No. characters in compressed string
      INTEGER INEW               ! New position to move character to
      INTEGER NGAP               ! No. inter-word gaps
      INTEGER NGAP0              ! No. gaps needing an extra 1 space
      INTEGER NSPACE             ! No. spaces required in line
      INTEGER PAD                ! Min. spaces needed between each word

      CHARACTER * 1 CVALUE       ! Single character

*.

*  Begin by cleaning the given string of any unprintable characters.
      CALL CHR_CLEAN( STRING )

*  Initialise flags.
      START = .FALSE.
      GAP = .FALSE.

*  Initialise gap accumulator.
      NGAP = 0

*  Initialise new position pointer.
      INEW = 0

*  Loop to eliminate all multiple gaps in the cleaned string: i.e
*  compress all white space in the string.
      DO 10 I = 1, LEN( STRING )

*     Get the next character from the string.
         CVALUE = STRING( I : I )

*     Assign the SPACE flag.
         SPACE = ( CVALUE .EQ. ' ' )

*     If the current character is a space and the previous character was
*     not a space, incement the number of gaps (NGAP) and update STRING;
*     else discard the space character.
         IF ( SPACE ) THEN

*        The current character is a space: check if the previous
*        character was a space within the body of the string and act
*        accordingly.
            IF ( START .AND. ( .NOT. GAP ) ) THEN
               GAP = .TRUE.
               NGAP = NGAP + 1
               INEW = INEW + 1
               STRING( INEW : INEW ) = ' '
            END IF
         ELSE

*        The current character is not a space: reset the GAP and START
*        flags, increment INEW and update STRING.
            GAP = .FALSE.
            START = .TRUE.
            INEW = INEW + 1
            STRING( INEW : INEW ) = CVALUE
         END IF
 10   CONTINUE

*  Assign the length of the compressed string, ensuring that the last
*  character in the compressed string is not a space (if it is, discard
*  it).
      ICOMP = INEW

      IF ( GAP ) THEN
         ICOMP = ICOMP - 1
         NGAP = NGAP - 1
      END IF

*  Check that the compressed string has non-white characters.
      IF ( NGAP .GT. 0 ) THEN

*     A string exists, determine the padding required to right-justify
*     the string.
         NSPACE = LEN( STRING ) - ICOMP + NGAP
         PAD = NSPACE / NGAP
         NGAP0 = NSPACE - ( PAD*NGAP )
         NGAP = 0
         INEW = LEN( STRING ) + 1

*     Loop to insert the padding into the string.
         DO 20 I = ICOMP, 1, -1
            CVALUE = STRING( I : I )

*        Pad the gaps to a minimum of PAD spaces.
            IF ( CVALUE .EQ. ' ' ) THEN
               NGAP = NGAP + 1
               INEW = INEW - PAD
               STRING( INEW : INEW+PAD-1 ) = ' '

*           Pad the last NGAP0 gaps with an extra space to make up any
*           remainder.
               IF ( NGAP .LE. NGAP0 ) THEN
                  INEW = INEW - 1
                  STRING( INEW : INEW ) = ' '
               END IF
            ELSE
               INEW = INEW - 1
               STRING( INEW : INEW ) = CVALUE
            END IF
 20      CONTINUE
      END IF

      END
