      LOGICAL FUNCTION CHR_ABBRV( STR1, STR2, NCHAR )
*+
*  Name:
*     CHR_ABBRV

*  Purpose:
*     Return whether two strings are equal apart from case,
*     permitting abbreviations.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR_ABBRV( STR1, STR2, NCHAR )

*  Description:
*     Returns a logical result indicating whether two strings
*     are the same, apart from case. In assessing this, the first
*     string is allowed to be an abbreviation of the second string,
*     as long as it contains a specified minimum number of characters.

*  Arguments:
*     STR1 = CHARACTER * ( * ) (Given)
*        The first string, which may be an abbreviation.
*     STR2 = CHARACTER * ( * ) (Given)
*        The second string.
*     NCHAR = INTEGER (Given)
*        The minimum number of characters to which the first string may
*        be abbreviated (a smaller number will be accepted if there are
*        actually fewer than NCHAR characters in STR2).

*  Returned Value:
*     CHR_ABBRV = LOGICAL
*        Whether the two strings match after allowing for case and
*        abbreviation of the first string to no less than NCHAR
*        characters.

*  Algorithm:
*     -  Find how many characters there are in STR1, ignoring trailing
*     blanks, but using at least 1 character.
*     -  Determine how many characters from STR2 to compare it with.
*     -  Compare the selected regions of each string, ignoring case.

*  Copyright:
*     Copyright (C) 1989, 1994 Science & Engineering Research Council.
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
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1989 (RFWS):
*        Original version.
*     26-SEP-1989 (RFWS):
*        Minor spelling corrections.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STR1
      CHARACTER * ( * ) STR2

      INTEGER NCHAR

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

      INTEGER CHR_LEN            ! String length (ignoring trailing blanks)

*  Local Variables:
      INTEGER L1                 ! Number of characters to use from STR1
      INTEGER L2                 ! Number of characters to use from STR2

*.

*  Find the number of characters in STR1, ignoring trailing blanks, but
*  using at least 1 character.
      L1 = MAX( 1, CHR_LEN( STR1 ) )

*  Find the number of characters from STR2 to compare with STR1. This
*  must include at least NCHAR characters (or more if present in STR1), but
*  cannot exceed the length of STR2.
      L2 = MIN( MAX( L1, NCHAR ), LEN( STR2 ) )

*  Compare the selected parts of the two strings, ignoring case.
      CHR_ABBRV = CHR_SIMLR( STR1( : L1 ), STR2( : L2 ) )

      END
