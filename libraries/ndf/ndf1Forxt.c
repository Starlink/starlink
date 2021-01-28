#include <stdlib.h>
#include "sae_par.h"
#include "ndf_ast.h"
#include "ndf1.h"
#include <string.h>

void ndf1Forxt( const char *name, size_t start, size_t end, size_t *x1,
                size_t *x2, int *status ){
/*
*+
*  Name:
*     ndf1Forxt

*  Purpose:
*     Locate a foreign format extension specified within a complete
*     structure specification.

*  Synopsis:
*     void ndf1Forxt( const char *name, size_t start, size_t end, size_t *x1,
*                     size_t *x2, int *status )

*  Description:
*     Some foreign formats can hold the equivalent of several NDFs within
*     a single data file. An example is the FITS format. FITS files may
*     include image extensions, each of which could give rise to a separate
*     NDF. When a foreign data structure is specified, any syntax required
*     to identify a particular sub-structure within the foreign format file
*     should be specified after the file type (or file name if no file type
*     is given), and before any NDF slice specification. The syntax should
*     be enclosed within matching square brackets.
*
*     This function looks for such syntax within the specified name, and
*     returns the index of the opening and closing square brackets. No
*     checks can be performed on the validity of the syntax since it is
*     format-specific.

*  Parameters:
*     name
*        Pointer to a null terminated string holding the name of the
*        foreign NDF to be opened. NOTE, this should not include an NDF
*        slice specification.
*     start
*        Zero-based index of the first character to use in "name".
*     end
*        Zero-based index of the last character to use in "name".
*     *x1
*        Returned holding the index of the opening square bracket within
*        "name", or one more than the used length of the string if the name
*        does not include a foreign extension specifier.
*     *x2
*        Returned holding the index of the closing square bracket within
*        "name", or 0 if the name does not include a foreign extension
*        specifier.
*     *status
*        The global status.

*  Notes:
*     - If "start" is greater than "end", the whole of "name" is used.
*     - If this function is called with "status" set, then value of 1 and
*     zero will be returned for "x1" and "x2". The same values will also be
*     returned if the function should fail for any reason.

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
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.
*     28-JAN-2021 (DSB):
*        Fix bugs caused by errors in moving from 1-based indices to
*        zero-based indices.

*-
*/

/* Local Variables: */
   size_t i;             /* Character index */
   size_t ln;            /* Used length of NAME */

/* Set initial values for the "x1" and "x2" arguments. */
   if( start > end ) {
      *x1 = astChrLen( name );
      *x2 = 0;
      start = 0;
   } else {
      *x1 = end + 1;
      *x2 = start;
   }

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Save the used length of the string. */
   ln = *x1;

/* Any NDF slice specification should have been removed from "name" prior
   to calling this function. Therefore, if there is a foreign extension
   specifier, the final non-blank character should be a "]". */
   if( name[ ln - 1 ] == ']' ) {

/* Find the preceeding opening bracket "[". */
      for( i = ln - 2; i >= start; i-- ){
         if( name[ i ] == '[' ) {
            *x1 = i + 1;
            *x2 = ln;
            break;
         }
      }
   }
}

