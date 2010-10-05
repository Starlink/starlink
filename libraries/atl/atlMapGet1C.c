#include "ast.h"
#include "sae_par.h"

int atlMapGet1C( AstKeyMap *this, const char *key, int bufsize, int len,
                 int *nval, char *buf, int *status ) {
/*
*+
*  Name:
*     atlMapGet1C

*  Purpose:
*     Retrieve a vector of strings from an AST KeyMap entry as a list
*     of fixed length strings.

*  Language:
*     C.

*  Invocation:
*     int atlMapGet1C( AstKeyMap *this, const char *key, int bufsize,
*                      int len, int *nval, char *buf, int *status )

*  Description:
*     This function retrieves the null-terminated strings from a vector
*     element in a KeyMap, and concatenates them into a list of fixed
*     length strings, each padded with spaces.

*  Arguments:
*     this
*        A pointer to the KeyMap.
*     key
*        The key for the entry.
*     bufsize
*        The length of the "buf" array.
*     len
*        The required size ofr each fixed length string.
*     nval
*        Address of an int in which to return the number of fixed length
*        strings returned in "buf". This will be less than the number of
*        elements in the KeyMap entry if the supplied buffer is not large
*        enough to hold all the strings in the entry.
*     buf
*        A pointer to a buffer in which to return the concatenated, fixed
*        length strings.
*     status
*        Pointer to the global status variable.

*  Returned Value:
*     Non-zero if an entry with the given key was found in the KeyMap,
*     and zero otherwise.

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
*        Renamed from atlMapGet1S to atlMapGet1C to avoid clash with
*        astMapGet1S.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*+
*/

/* Local Variables: */
   char *iw;              /* Pointer to next character to be written */
   char *ptr;             /* Array holding all null terminated strings */
   const char *ir;        /* Pointer to next character to be read */
   int *old_status;       /* Pointer to original status variable */
   int i;                 /* Index of next string */
   int j;                 /* Index of next character */
   int mnlen;             /* Min of read and write string lengths */
   int mxlen;             /* Max length of any null-terminated string */
   int mxval;             /* Max number of strings that can be returned */
   int pad;               /* Replace next character with a space? */
   int result;            /* Was entry found in KeyMap? */

/* Initialise */
   result = 0;
   *nval = 0;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Make AST use the Fortran status variable. */
   old_status = astWatch( status );

/* Get the length of the entry and the maximum length of each individual
   string in the entry. */
   mxval = astMapLength( this, key );
   if( mxval > 0 ) {
      mxlen = astMapLenC( this, key );

/* Allocate memory for an array of concatenated null-terminated strings.
   Each string occupies "len+1" characters in this array. This  is the
   form in which astMapGet1C returned the data. */
      ptr = astMalloc( sizeof( char )*mxval*( mxlen + 1 ) );

/* Get the data. */
      if( astMapGet1C( this, key, mxlen + 1, mxval, &mxval, ptr ) ) {
         result = 1;

/* Determine how many complete strings will fit in the supplied buffer. */
         *nval = bufsize/len;
         if( *nval > mxval ) *nval = mxval;

/* Copy each one into the buffer. */
         mnlen = ( mxlen < len ) ? mxlen : len ;

         for( i = 0; i < *nval; i++ ) {
            iw = buf + i*len;
            ir = ptr + i*( mxlen + 1 );

            pad = 0;
            for( j = 0; j < mnlen; j++, ir++, iw++ ) {
               if( pad ) {
                  *iw = ' ';
               } else if( *ir ) {
                  *iw = *ir;
               } else {
                  *iw = ' ';
                  pad = 1;
               }
            }

            for( ; j < len; j++ ) *(iw++) = ' ';

         }

/* Pad the returned array with zeros. */
         for( i = len*( *nval ); i < bufsize; i++ ) *(iw++) = 0;
      }

/* Free resources. */
      ptr = astFree( ptr );
   }

/* Make AST use its original status variable. */
   astWatch( old_status );

/* Return the result. */
   return result;
}

