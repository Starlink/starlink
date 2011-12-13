      SUBROUTINE CHR_PREFX( STR1, STR2, LEN2 )
*+
*  Name:
*     CHR_PREFX

*  Purpose:
*     Prefix a string with a substring.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_PREFX( STR1, STR2, LEN2 )

*  Description:
*     The substring STR1 is prefixed to the string STR2, moving the
*     string STR2 along to make room. The given string in STR2 may be
*     truncated by adding the prefix. The final length of the string
*     STR2, ignoring trailing blanks, is returned in LEN2.

*  Arguments:
*     STR1 = CHARACTER * ( * ) (Given)
*        The prefix string.
*     STR2 = CHARACTER * ( * ) (Given and Returned)
*        The string to be prefixed.
*     LEN2 = INTEGER (Returned)
*        The resultant length of the string STR2, ignoring trailing
*        blanks.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-JUN-1991 (PCTR):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STR1

*  Arguments Given and Returned:
      CHARACTER * ( * ) STR2

*  Arguments Returned:
      INTEGER LEN2

*  External References:
      INTEGER CHR_LEN            ! String length, ignoring trailing blanks

*  Local Variables:
      INTEGER ICH                ! Loop index
      INTEGER LEN1               ! Length of STR1
      INTEGER LMAX               ! Declared length of STR1
      INTEGER OFFSET             ! Character offset for prefix

*.

*  Find the declared lengths of the two given strings.
      LEN1 = LEN( STR1 )
      LMAX = LEN( STR2 )

*  Check if there is any room left for part of STR2 after the prefix.
      IF ( LEN1 .LT. LMAX ) THEN

*     Move STR2 along to make room for the prefix.
         DO 10 ICH = LMAX-LEN1, 1, -1
            OFFSET = LEN1 + ICH
            STR2( OFFSET : OFFSET ) = STR2( ICH : ICH )
 10      CONTINUE

*     Add the prefix.
         STR2 ( 1 : LEN1 ) = STR1
      ELSE

*     There is no room for any part of STR2 to remain after the prefix,
*     so just perform a string assignment.
         STR2 = STR1
      END IF

*  Find the length of the returned string, STR2, ignoring trailing
*  blanks.
      LEN2 = CHR_LEN( STR2 )

      END
