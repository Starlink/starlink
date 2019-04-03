#include <string.h>
#include "ndf1_types.h"
#include "ndf_ast.h"
#include "sae_par.h"

int ndf1Simlr( const char *str1, size_t start, size_t end, const char *str2,
               int n ){
/*
*+
*  Name:
*     ndf1Simlr

*  Purpose:
*     Case insensitive string comparison, permitting abbreviation.

*  Synopsis:
*     int ndf1Simlr( const char *str1, size_t start, size_t end,
*                    const char *str2, int n )

*  Description:
*     The function returns a logical result indicating whether two strings
*     are the same apart from case. In assessing this, the first string is
*     allowed to be an abbreviation of the second string, so long as it
*     contains a specified minimum number of characters.

*  Parameters:
*     str1
*        Pointer to a null terminated string holding the first string,
*        which may be an abbreviation.
*     start
*        The zero-based index of the first character to consider in "str1".
*        The whole string is used if "start" > "end".
*     end
*        The zero-based index of the last character to consider in "str1".
*        The whole string is used if "start" > "end".
*     str2
*        Pointer to a null terminated string holding the second string.
*     n
*        The minimum number of characters to which the first string may be
*        abbreviated (although a smaller number will be accepted if there
*        are actually fewer than "n" characters in "str2").

*  Returned Value:
*     Whether the two strings match after allowing for case and
*     abbreviation of the first string to no less than N characters.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   int result;           /* Returned value */
   size_t l1;            /* No. characters to use from STR1 */
   size_t l2;            /* No. characters to use from STR2 */
   int status;           /* Local status value */
   int *old_status;      /* Original status pointer */

/* Tell AST to watch a local status variable so that any previous error
   does not cause AST calls to return without action. */
   status = SAI__OK;
   old_status = astWatch( &status );

/* Initialise */
   result = 0;

/* Find the number of characters in "str1", ignoring trailing blanks, but
   using at least 1 character. */
   l1 = astChrLen( str1 );
   if( l1 < 1 ) l1 = 1;

/* Find the start and end values to use. */
   if( start > end ) {
      start = 0;
      end = l1 - 1;
   } else if( end >= l1 ) {
      end = l1 - 1;
   }

/* Adjust the number of characters to use from str1 and check there is
   some text left. */
   l1 = end - start + 1;
   if( l1 > 0 ) {

/* Find the number of characters from "str2" to compare with "str1". This
   must include at least "n" characters (or more if present in "str1"), but
   cannot exceed the length of "str2". */
      l2 = NDF_MIN( NDF_MAX( l1, n ), strlen( str2 ) );

/* Compare the selected parts of the two strings, ignoring case. */
      if( l1 <= l2 ) result =  astChrMatchN( str1 + start, str2, l1 );
   }

/* Re-instate the original AST status pointer. */
   astWatch( old_status );

/* Return the result */
   return result;
}

