      SUBROUTINE NDF1_RJUST( STRING )
*+
*  Name:
*     NDF1_RJUST

*  Purpose:
*     Right justify a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_RJUST( STRING )

*  Description:
*     The routine right justifies a string by filling out the spaces
*     between words with additional blank space. The right margin is
*     taken as the length of the character variable supplied. Leading
*     blanks are preserved.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        The string to be right justified and returned.

*  Copyright:
*     Copyright (C) 1994 Particle Physics & Astronomy Research Council

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
*     {enter_new_authors_here}

*  History:
*     11-APR-1991 (RFWS):
*        Original version.
*     11-APR-1991 (PCTR):
*        Commented code and completed tests.
*     11-MAY-1993 (RFWS):
*        Adapted for use in the NDF_ library.
*     17-NOV-1994 (RFWS):
*        Modified to prevent removal of leading blanks. Also removed
*        conversion of non-printing characters into blanks.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given and Returned:
      CHARACTER * ( * ) STRING

*  Local Variables:
      CHARACTER * ( 1 ) C        ! Single character
      INTEGER F                  ! Position of first non-blank character
      INTEGER I                  ! Loop counter for character positions
      INTEGER IOUT               ! New position to move character to
      INTEGER NBLANK             ! No. blanks required in line
      INTEGER NCOMP              ! No. characters in compressed string
      INTEGER NEXTRA             ! No. gaps needing an extra 1 blank
      INTEGER NGAP               ! No. inter-word gaps
      INTEGER PAD                ! Min. blanks needed between each word
      LOGICAL GAP                ! In an inter-word gap?
      LOGICAL START              ! At start of (non-blank) string yet?

*.

*  Initialise.
      GAP = .FALSE.
      START = .FALSE.
      IOUT = 0
      NGAP = 0

*  Loop to eliminate all multiple embedded blanks from the string.
      DO 1 I = 1, LEN( STRING )

*  Get the next character from the string.
         C = STRING( I : I )

*  If the character is not blank or we have not yet reached the start of
*  non-blank text, then increment the output character count and move
*  the character to its new position.
         IF ( ( C .NE. ' ' ) .OR. ( .NOT. START ) ) THEN
            IOUT = IOUT + 1
            STRING( IOUT : IOUT ) = C

*  Note we are not in an inter-word gap.
            GAP = .FALSE.

*  Note where the start of non-blank text begins.
            IF ( ( C .NE. ' ' ) .AND. ( .NOT. START ) ) THEN
               F = I
               START = .TRUE.
            END IF

*  If the character is blank (after a previous non-blank character has
*  been encountered) and we are not already in an inter-word gap, then
*  increment the output character count and insert a single blank at
*  that position.
         ELSE IF ( .NOT. GAP ) THEN
            IOUT = IOUT + 1
            STRING( IOUT : IOUT ) = ' '

*  Note we are now in an inter-word gap and count it.
            GAP = .TRUE.
            NGAP = NGAP + 1
         END IF
 1    CONTINUE

*  Find the length of the compressed string, eliminating any final
*  blank/gap.
      NCOMP = IOUT
      IF ( GAP ) THEN
         NCOMP = NCOMP - 1
         NGAP = NGAP - 1
      END IF

*  If the compressed string has inter-word gaps, then determine the
*  minimum number of spaces to insert into each gap to right justify it.
      IF ( NGAP .GT. 0 ) THEN
         NBLANK = LEN( STRING ) - NCOMP + NGAP
         PAD = NBLANK / NGAP

*  Determine how many gaps must also receive an additional blank to make
*  up the total number required.
         NEXTRA = NBLANK - ( PAD * NGAP )

*  Loop through the compressed output string in reverse to insert the
*  padding. Omit any leading blanks from this process.
         NGAP = 0
         IOUT = LEN( STRING ) + 1
         DO 2  I = NCOMP, F, -1

*  Extract each character. If it is blank, then count an inter-word gap
*  and copy PAD blanks into the output string in its place.
            C = STRING( I : I )
            IF ( C .EQ. ' ' ) THEN
               NGAP = NGAP + 1
               IOUT = IOUT - PAD
               STRING( IOUT : IOUT + PAD - 1 ) = ' '

*  The last NEXTRA gaps (the first ones encountered going in reverse)
*  are padded with an extra blank to make up the total required.
               IF ( NGAP .LE. NEXTRA ) THEN
                  IOUT = IOUT - 1
                  STRING( IOUT : IOUT ) = ' '
               END IF

*  Retain all non-blank characters unchanged.
            ELSE
               IOUT = IOUT - 1
               STRING( IOUT : IOUT ) = C
            END IF
 2       CONTINUE
      END IF

      END
