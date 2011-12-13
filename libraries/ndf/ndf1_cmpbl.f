      SUBROUTINE NDF1_CMPBL( LEAD, STRING, NC )
*+
*  Name:
*     NDF1_CMPBL

*  Purpose:
*     Compress multiple blanks in a character string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CMPBL( LEAD, STRING, NC )

*  Description:
*     The routine replaces occurrences of multiple blanks in a string
*     with single blanks, shifting the non-blank characters to the left
*     as a result and adding new blanks at the right hand end.

*  Arguments:
*     LEAD = LOGICAL (Given)
*        A .FALSE. value indicates that compression of multiple blanks
*        should be restricted to embedded blanks only (leading blanks
*        being left unchanged). A .TRUE. value indicates that the
*        process should be applied to leading blanks as well.
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        The string to be processed.
*     NC = INTEGER (Returned)
*        Significant length of the processed string (the position of
*        the last non-blank character).

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     18-JUN-1993 (RFWS):
*        Original version.
*     16-NOV-1994 (RFWS):
*        Don't remove blanks at the start of the string.
*     17-NOV-1994 (RFWS):
*        Add the LEAD argument to control processing of leading blanks.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      LOGICAL LEAD

*  Arguments Given and Returned:
      CHARACTER * ( * ) STRING

*  Arguments Returned:
      INTEGER NC

*  Local Variables:
      CHARACTER * ( 1 ) C        ! Single input character
      INTEGER I                  ! Loop counter for input characters
      INTEGER IOUT               ! Output character counter
      LOGICAL GAP                ! In an inter-word gap?
      LOGICAL START              ! Start of non-blank text encountered?

*.

*  Initialise.
      NC = 0
      IOUT = 0
      GAP = .FALSE.

*  Leading blanks are only handled differently if LEAD is .TRUE..
      START = LEAD

*  Loop to process all the characters in the string.
      DO 1 I = 1, LEN( STRING )
         C = STRING( I : I )

*  If the next character is not blank or we have not yet reached the
*  start of non-blank text, then increment the output character count
*  and move the character to its new position.
         IF ( ( C .NE. ' ' ) .OR. ( .NOT. START ) ) THEN
            IOUT = IOUT + 1
            STRING( IOUT : IOUT ) = C

*  Note we are not in an inter-word gap and update the output string
*  length.
            GAP = .FALSE.
            NC = IOUT

*  Note when the start of non-blank text is reached.
            IF ( C .NE. ' ' ) START = .TRUE.

*  If the character is blank (after a previous non-blank character has
*  been encountered) and we are not already in an inter-word gap, then
*  increment the output character count and insert a single blank at
*  that position.
         ELSE IF ( .NOT. GAP ) THEN
            IOUT = IOUT + 1
            STRING( IOUT : IOUT ) = ' '

*  Note we are now in an inter-word gap.
            GAP = .TRUE.
         END IF
 1    CONTINUE

*  If any part of STRING has not been written to, then clear it.
      IF ( IOUT .LT. LEN( STRING ) ) STRING( IOUT + 1 : ) = ' '

      END
