      LOGICAL FUNCTION NDF1_SIMLR( STR1, STR2, N )
*+
*  Name:
*     NDF1_SIMLR

*  Purpose:
*     Case insensitive string comparison, permitting abbreviation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = NDF1_SIMLR( STR1, STR2, N )

*  Description:
*     The function returns a logical result indicating whether two
*     strings are the same apart from case. In assessing this, the
*     first string is allowed to be an abbreviation of the second
*     string, so long as it contains a specified minimum number of
*     characters.

*  Arguments:
*     STR1 = CHARACTER * ( * ) (Given)
*        The first string, which may be an abbreviation.
*     STR2 = CHARACTER * ( * ) (Given)
*        The second string.
*     N = INTEGER (Given)
*        The minimum number of characters to which the first string may
*        be abbreviated (although a smaller number will be accepted if
*        there are actually fewer than N characters in STR2).

*  Returned Value:
*     NDF1_SIMLR = LOGICAL
*        Whether the two strings match after allowing for case and
*        abbreviation of the first string to no less than N characters.

*  Algorithm:
*     -  Find how many characters there are in STR1, ignoring trailing
*     blanks, but using at least 1 character.
*     -  Determine how many characters from STR2 to compare it with.
*     -  Compare the selected regions of each string, ignoring case.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1989 (RFWS):
*        Original version.
*     26-SEP-1989 (RFWS):
*        Minor spelling corrections.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STR1
      CHARACTER * ( * ) STR2
      INTEGER N

*  External References:
      INTEGER CHR_LEN            ! Significant string length
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables:
      INTEGER L1                 ! No. characters to use from STR1
      INTEGER L2                 ! No. characters to use from STR2

*.

*  Find the number of characters in STR1, ignoring trailing blanks, but
*  using at least 1 character.
      L1 = MAX( 1, CHR_LEN( STR1 ) )

*  Find the number of characters from STR2 to compare with STR1. This
*  must include at least N characters (or more if present in STR1), but
*  cannot exceed the length of STR2.
      L2 = MIN( MAX( L1, N ), LEN( STR2 ) )

*  Compare the selected parts of the two strings, ignoring case.
      NDF1_SIMLR = CHR_SIMLR( STR1( : L1 ), STR2( : L2 ) )

      END
