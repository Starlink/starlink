      LOGICAL FUNCTION KPG1_SHORT( TEMPLT, TEST, MARK, CASE, STATUS )
*+
*  Name:
*     KPG1_SHORT

*  Purpose:
*     Checks whether a string matches a supplied abbreviation template.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = KPG1_SHORT( TEMPLT, TEST, MARK, CASE, STATUS )

*  Description:
*     This routine returns .TRUE. if the supplied test string matches
*     the supplied abbreviation template. All characters in the template
*     must be matched by the corresponding characters in the test string,
*     with the exception that it is permissable for the test string to end
*     anywhere following the first occurrence of the mark string (the
*     mark string itself should not be matched in the test string). The
*     template sub-string in front of the first mark string thus gives
*     the minimum abbreviation which the test string can use.
*
*     For instance, if MARK='*' and TEMPLT='VECT*ORS', then the test string
*     matches if the first 4 characters are 'VECT' and any remaining
*     characters match 'ORS'.

*  Arguments:
*     TEMPLT = CHARACTER * ( * ) (Given and Returned)
*        The abbreviation template. Trailing blanks are ignored.
*     TEST = CHARACTER * ( * ) (Given and Returned)
*        The test string. Trailing blanks are ignored.
*     MARK = CHARACTER * ( * ) (Given and Returned)
*        The string which marks the end of the minimum abbreviation in
*        TEMPLT. Trailing blanks are ignored.
*     CASE = LOGICAL (Given)
*        Should the match be case sensitive?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Function Value:
*     KPG1_SHORT = LOGICAL
*        Returned .TRUE. if and only if the supplied test string matches the
*        template.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-MAR-1998 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER TEMPLT*(*)
      CHARACTER TEST*(*)
      CHARACTER MARK*(*)
      LOGICAL CASE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string
      LOGICAL CHR_SIMLR          ! Are strings equal apart from case?
      CHARACTER CHR_UPPER*1      ! Upper-case equivalent of a character

*  Local Variables:
      INTEGER I                  ! Template character index
      INTEGER J                  ! End-of-mark index.
      INTEGER K                  ! Test character index
      INTEGER MLEN               ! Significant length of MARK
      INTEGER TMPLEN             ! Significant length of TEMPLT
      INTEGER TSTLEN             ! Significant length of TEST
      LOGICAL ENDOK              ! Is it ok for text to end?
      LOGICAL MARKER             ! Does remaining template start with MARK?
*.

*  Initialize.
      KPG1_SHORT = .FALSE.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Assume we have a match.
      KPG1_SHORT = .TRUE.

*  Save the significant lengths of the supplied strings.
      MLEN = CHR_LEN( MARK )
      TMPLEN = CHR_LEN( TEMPLT )
      TSTLEN = CHR_LEN( TEST )

*  Set a flag to indicate that the test string may not end yet.
      ENDOK = .FALSE.

*  Initialise the index of the next test character to be checked.
      K = 1

*  We check each character in the template (excluding trailing blanks). If a
*  mismatch is found leave the loop.
      DO I = 1, TMPLEN

*  See if the remaining characters in the template start with the mark
*  string.
         J = MIN( TMPLEN, I + MLEN - 1 )
         IF( CASE ) THEN
            MARKER = ( TEMPLT( I : J ) .EQ. MARK( : MLEN ) )
         ELSE
            MARKER = CHR_SIMLR( TEMPLT( I : J ), MARK( : MLEN ) )
         END IF

*  If so, indicate that it is ok for the supplied text to end now.
         IF( MARKER ) THEN
            ENDOK = .TRUE.

*  Otherwise, if we have reached the end of the supplied text, leave the loop.
*  We have a match if it is OK for the text to end here.
         ELSE IF( K .GT. TSTLEN ) THEN
            KPG1_SHORT = ENDOK
            GO TO 10

*  Otherwise, if the characters are different, we do not have a match.
         ELSE
            IF( CASE ) THEN
               IF( TEMPLT( I : I ) .NE. TEST( K : K ) ) THEN
                  KPG1_SHORT = .FALSE.
                  GO TO 10
               END IF
            ELSE
               IF( CHR_UPPER( TEMPLT( I : I ) ) .NE.
     :             CHR_UPPER( TEST( K : K ) ) ) THEN
                  KPG1_SHORT = .FALSE.
                  GO TO 10
               END IF
            END IF

*  Move on to the next text character if the current characters match.
            K = K + 1

         END IF

      END DO

*  Arrive here only if a match has been found. If there are any remaing
*  characters in the test string, then we do not have a match.
      IF( K .LE. TSTLEN ) KPG1_SHORT = .FALSE.

 10   CONTINUE

      END

