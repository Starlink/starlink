/*
 *+
 *  Name:
 *    one_wordexp_noglob

 *  Purpose:
 *    Expands shell variables without globbing using wordexp()

 *  Language:
 *    Starlink ANSI C

 *  Invocation:
 *    (void) one_wordexp_noglob( const char * words, char * expan,
 *                               size_t expanlen, int * status );

 *  Description:
 *    The input string is escaped using double quotes and then passed
 *    to the wordexp() function for expansion. Only a single expansion
 *    string is returned by this routine and it is an error for wordexp()
 *    to return multiple matches.

 *  Arguments:
 *    words = const char * (Given)
 *       The string to be shell expanded.
 *    expan = char * (Given & Returned)
 *       The buffer to receive the expanded string.
 *    expanlen = size_t (Given)
 *       The size of the expan buffer.
 *    status = int * (Given & Returned)
 *       The global status.

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  History:
*     2013-04-04 (TIMJ):
*        First version

*-
*/

#include "sae_par.h"
#include "one.h"
#include "star/util.h"
#include "star/mem.h"
#include "ems.h"

#include <string.h>
#include <wordexp.h>

void one_wordexp_noglob( const char * words, char * expan, size_t expanlen, int * status ) {

  char * buff = NULL;     /* Buffer to hold escaped input */
  size_t bufflen = 0;     /* Size of buff */
  wordexp_t pwordexp;     /* Results from wordexp */
  int retval = 0;         /* Wordexp() return value */

  if (*status != SAI__OK) return;

  /* Get a buffer to copy the WORDS with enough space for two
     quotes and the terminator */
  bufflen = strlen( words ) + 2 + 1;

  buff = starMallocAtomic( bufflen );

  /* Copy the words into the buffer and surround with double quotes */
  star_strlcpy( buff, "\"", bufflen );
  star_strlcat( buff, words, bufflen );
  star_strlcat( buff, "\"", bufflen );

  retval = wordexp( buff, &pwordexp, 0 );

  if (retval == 0) {
    if (pwordexp.we_wordc > 0 ) {
      one_strlcpy( expan, (pwordexp.we_wordv)[0], expanlen, status );

      if (pwordexp.we_wordc > 1 ) {
        /* Too many words */
        *status = SAI__ERROR;
        emsRepf( "", "%d results from noglob expansion of string '%s'",
                 status, (int)pwordexp.we_wordc, buff );
      }
    } else {
      *status = SAI__ERROR;
      emsRep( "", "Unexpectedly got no expansion of string", status );
    }
  } else {
    *status = SAI__ERROR;
    emsRepf("", "Internal error (%d) from wordexp()", status, retval );
  }

  wordfree( &pwordexp );
  starFree( buff );

}
