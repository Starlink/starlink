      CHARACTER * 1 FUNCTION CHR_LOWER( CVALUE )
*+
*  Name:
*     CHR_LOWER

*  Purpose:
*     Return the lowercase equivalent of a character.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR_LOWER( CVALUE )

*  Description:
*     If the given character is uppercase, the lowercase equivalent
*     is returned, otherwise the character will be returned unchanged.

*  Arguments:
*     CVALUE = CHARACTER * 1 (Given)
*        The character to be converted.

*  Returned Value:
*     CHR_LOWER = CHARACTER * 1 (Returned)
*        Lowercase equivalent of the given character, if the given
*        character is an uppercase letter; otherwise the character
*        is returned unchanged.

*  Algorithm:
*     This algorithm will only work for character sets which have a
*     constant offset between upper and lowercase and is therefore
*     dependent upon the Fortran 77 implementation.
*
*     Get the integer code of the given character.
*     If the given character is not an uppercase letter then
*        Set the returned value to the given character.
*     Else
*        The returned value is set to its lowercase equivalent,
*        assuming a constant offset between upper and lowercase.
*     Endif

*  Copyright:
*     Copyright (C) 1984, 1988, 1990, 1991, 1994 Science & Engineering Research Council.
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
*     ASOC5: Dave Baines (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     DLT: D.L. Terrett (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-JUN-1984 (ASOC5):
*        Original version.
*     3-OCT-1988 (AJC):
*        Improved documentation.
*     25-JAN-1990 (DLT):
*        Use ICHAR instead of integer parameters because of
*        DECstation compiler bugs.
*     6-FEB-1991 (PCTR):
*        Use the LLT and LGT intrinsic functions to determine case.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * 1 CVALUE

*.

*  Check if the given character is not an upper case letter.
      IF ( LGT( CVALUE, 'Z' ) ) THEN
         CHR_LOWER = CVALUE
      ELSE IF ( LLT( CVALUE, 'A' ) ) THEN
         CHR_LOWER = CVALUE
      ELSE

*     The returned value is set to the upper case equivalent of the
*     given character: add the offset between upper and lower case
*     characters.
         CHR_LOWER = CHAR( ICHAR( CVALUE )-ICHAR( 'A' )+ICHAR( 'a' ) )
      END IF

      END
