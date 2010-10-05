#include "ast.h"
#include "sae_par.h"
#include <string.h>

void atlMapPut1C( AstKeyMap *this, const char *key, const char *value,
                  int len, int size, const char *comment, int *status ){
/*
*+
*  Name:
*     atlMapPut1C

*  Purpose:
*     Create a vector character string entry in an AST KeyMap from a list
*     of fixed length strings.

*  Language:
*     C.

*  Invocation:
*     void atlMapPut1C( AstKeyMap *this, const char *key, const char *value,
*                       int len, int size, const char *comment, int *status );

*  Description:
*     This function splits up a supplied character array into a set of
*     equal length sub-strings, null terminates them, and stores them
*     as a character vector in a KeyMap. See also atlMapGet1S.

*  Arguments:
*     this
*        A pointer to an existing KeyMap.
*     key
*        The key  for the new entry.
*     value
*        A character array containing the concatenated fixed length
*        strings. The length of this array should be at least "size*len".
*     len
*        The length of each fixed length string.
*     size
*        The number of fixed length strings in "value".
*     status
*        Pointer to the global status variable.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     10-MAR-2008 (DSB):
*        Original version.
*     5-OCT-2010 (DSB):
*        Renamed from atlMapPut1S to atlMapPut1C to avoid clash with
*        astMapPut1S.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*+
*/

/* Local Variables: */
   char **ptr;            /* Array holding pointers to individual strings */
   const char *p;         /* Pointer to start of next fixed length string */
   char *q;               /* Pointer to start of next null terminated string */
   int *old_status;       /* Pointer to original status variable */
   int i;                 /* Index of next string */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Make AST use the Fortran status variable. */
   old_status = astWatch( status );

/* Allocate memory for an array of string pointers. */
   ptr = astMalloc( sizeof( char * )*size );
   if( ptr ) {

/* Initialise it in case of errors. */
      for( i = 0; i < size; i++ ) ptr[ i ] = NULL;

/* Initialise a pointer to the start of the next fixed length string. */
      p = value;

/* Loop round each fixed length string. */
      for( i = 0; i < size; i++, p += len ) {

/* Allocate memory and store a copy of th efixed length string, adding an
   extra element for a terminating null character. */
         q = astMalloc( sizeof( char )*( len + 1 ) );
         if( q ) {
            (void) memcpy( q, p, len );

/* Terminate the copy of the fixed length string, and store a pointer to
   it in the array of string pointers. */
            q[ len ] = 0;
            ptr[ i ] = q;
         }
      }
   }

/* Put the strings into the KeyMap. */
    astMapPut1C( this, key, size, (const char **)ptr, comment );

/* Free the memory allocated above. */
   if( ptr ) {
      for( i = 0; i < size; i++ ) {
         ptr[ i ] = astFree( ptr[ i ] );
      }
      ptr = astFree( ptr );
   }

/* Make AST use its original status variable. */
   astWatch( old_status );
}

