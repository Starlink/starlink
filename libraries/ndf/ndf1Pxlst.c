#include <string.h>
#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf_ast.h"
#include "ndf_err.h"
#include "ndf1.h"

void ndf1Pxlst( int includ, const char *str,  size_t start, size_t end,
                AstKeyMap *keymap, int *status ){
/*
*+
*  Name:
*     ndf1Pxlst

*  Purpose:
*     Parse an extension name list.

*  Synopsis:
*     void ndf1Pxlst( int includ, const char *str, size_t start, size_t end,
*                     AstKeyMap *keymap, int *status )

*  Description:
*     This function parses a list of NDF extension names, extracting each
*     name from a comma separated list supplied and adding the name to an
*     AST KeyMap. Each entry in the KeyMap has a key that is the extension
*     name and a value which is zero if "includ" is zero and one if
*     "includ" is non-zero. The comma separated list may specify names for
*     EXCLUSION (i.e. extensions not to be copied) or INCLUSION (i.e.
*     extensions to be copied, over-riding a previous inclusion).
*
*     If a name equal to "*" is encountered, all entries currently in the
*     KeyMap are set to 1 (if "includ" is non-zero) or 0 (if "includ" is
*     zero).

*  Parameters:
*     includ
*        Whether the extensions specified in the list supplied are to be
*        included (as opposed to excluded) from an NDF copying operation.
*     str
*        Pointer to a null terminated string holding the comma separated
*        list of extension names.
*     start
*        The zero-based index of the first character to consider in "str1".
*        The whole string is used if "start" > "end".
*     end
*        The zero-based index of the last character to consider in "str1".
*        The whole string is used if "start" > "end".
*     keymap
*        A pointer to the AST KeyMap.
*     *status
*        The global status.

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
*     RFWS: R."f". Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   char *name;           /* Extension name */
   const char *p;        /* Pointer to matching character */
   int ikey;             /* KeyMap entry index */
   int keyval;           /* Value to assign to KeyMap entries */
   int nkey;             /* Number of entries in the keymap */
   size_t f;             /* First character position of name */
   size_t i1;            /* Position of start of list element */
   size_t i2;            /* Position of end of list element */
   size_t l;             /* Last character position of name */
   size_t nc;            /* Length of string */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Get the length of the string. */
   nc = strlen( str );

/* Find the start and end values to use. */
   if( start > end ) {
      start = 0;
      end = nc - 1;
   } else if( end >= nc ) {
      end = nc - 1;
   }

/* Check there is some text left. */
   if( start <= end ) {

/* Get the value to assign to entries in the keymap. */
      if( includ ) {
         keyval = 1;
      } else {
         keyval = 0;
      }

/* Initialise a pointer to the character position of the start of the
   "current" extension name. */
      i1 = start;

/* Loop to identify each element in the extension name list. */
      while( i1 <= end && *status == SAI__OK ){

/* Find the end of the next extension name (the character before the
   next comma or end of string). */
         p = strchr( str + i1, ',' );
         if( p ) {
            i2 = p - str - 1;
         } else {
            i2 = end;
         }

/* If the next name was found, then find the first and last characters
   in the name (excluding surrounding spaces). */
         if( i1 <= i2 ) {
            astFandl( str, i1, i2, &f, &l );

/* Check that the name is not all blank. */
            if( f <= l ) {

/* If the name is just an asterisk, assign 1 or 0 to all entries
   currently in the KeyMap. */
               if( f == l && str[f] == '*' ) {
                  nkey = astMapSize( keymap );
                  for( ikey = 0; ikey < nkey; ikey++ ){
                     astMapPut0I( keymap, astMapKey( keymap, ikey ), keyval, " " );
                  }

/* Otherwise, check the name for validity. */
               } else {
                  ndf1Chxnm( str, f, l, status );
                  if( *status == SAI__OK ) {

/* Extract the name and convert to upper case. */
                     name = ndf1Substr( str, f, l, status );
                     astChrCase( NULL, name, 1, 0 );

/* Add an entry to the KeyMap. This will over-write any existing entry
   for this extension name. */
                     astMapPut0I( keymap, name, keyval, " " );
                     name = astFree( name );
                  }
               }
            }
         }

/* Increment the pointer to the start of the next element in the input
   string and return to process it. */
         i1 = i2 + 2;
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Pxlst", status );

}

