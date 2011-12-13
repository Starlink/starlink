      LOGICAL FUNCTION CHR_SCOMP( STR1, STR2 )
*+
*  Name:
*     CHR_SCOMP

*  Purpose:
*     Compare two character strings using the ASCII character set.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR_SCOMP( STR1, STR2 )

*  Description:
*     The first string is compared with the second using the ASCII
*     character set, giving precedence to the left hand side of the
*     string. If the first string is less than or equal to the second,
*     the value .TRUE. is returned; otherwise the value .FALSE. is
*     returned.

*  Arguments:
*     STR1 = CHARACTER * ( * ) (Given)
*        The first character string.
*     STR2 = CHARACTER * ( * ) (Given)
*        The second character string.

*  Returned Value:
*     CHR_SCOMP = LOGICAL
*        Whether the first character string is less than or equal to
*        the second, using the ASCII character set.

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
*     PCTR: P.C.T. Rees (Starlink)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-APR-1991 (PCTR):
*        Original version.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STR1
      CHARACTER * ( * ) STR2

*  Local Variables:
      INTEGER LENGTH             ! Length of strings to be compared
      INTEGER SLEN1              ! Declared length of STR1
      INTEGER SLEN2              ! Declared length of STR2
      INTEGER ICHR               ! Character loop index

*.

*  Get the declared lengths of the two given character strings.
      SLEN1 = LEN( STR1 )
      SLEN2 = LEN( STR2 )

*  Initialise the returned value of CHR_SCOMP.
      CHR_SCOMP = .FALSE.

*  Determine the length of string to be compared: i.e. the length of the
*  shorter string.
      LENGTH = MIN( SLEN1, SLEN2 )

*  Check that there is something to compare.
      IF ( LENGTH .GT. 0 ) THEN

*     Loop to compare each character in the two character strings.
         DO 10 ICHR = 1, LENGTH
            IF ( LGT( STR1( ICHR : ICHR ),
     :                 STR2( ICHR : ICHR ) ) ) THEN

*           The first string is greater than the second.
               GO TO 20
            ELSE IF ( LLT( STR1( ICHR : ICHR ),
     :                     STR2( ICHR : ICHR ) ) ) THEN

*           The second string is greater than the first.
               CHR_SCOMP = .TRUE.
               GO TO 20
            END IF
 10      CONTINUE

*     The two strings are the same, up to the length of the shortest, so
*     check if STR1 is the shorter string.
         IF ( SLEN1 .LE. SLEN2 ) CHR_SCOMP = .TRUE.
 20      CONTINUE
      END IF

      END
