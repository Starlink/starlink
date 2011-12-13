/*
*+
*  Name:
*     ONE_WORDEXP_NOGLOB

*  Purpose:
*     WORDEXP wrapper that expands shell variables without globbing

*  Language:
*     Starlink ANSI C, intended to be called from Fortran

*  Invocation:
*     CALL ONE_WORDEXP_NOGLOB( WORDS, EXPAN, STATUS )

*  Description:
*     A wrapper around PSX_WORDEXP that does shell expansion without
*     doing globbing. Glob wildcard characters are escaped, as are
*     illegal, to wordexp(), shell metacharacters such as "(" and ")".
*
*     Only a single expansion string is returned by this routine and it
*     is an error for wordexp() to return multiple matches.

*  Arguments:
*     WORDS = CHARACTER*(*) (Given)
*        The string to be shell expanded.
*     EXPAN = CHARACTER*(*) (Given)
*        Expanded string.
*     STATUS = INTEGER (Given & Returned)
*        The global status.

*  See Also:
*     ONE_SHELL_ECHO is similar except that it guarantees to fork
*     whereas wordexp() might not fork.

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
*     2011-03-08 (TIMJ):
*        First version

*-
*/

#include "f77.h"
#include "sae_par.h"
#include "ems.h"
#include "star/mem.h"
#include "star/util.h"

/* We will be calling the PSX_WORDEXP Fortran wrapper so define
   the prototype. */
F77_SUBROUTINE(psx_wordexp)( CHARACTER(WORDS), INTEGER(CONTEXT),
                             CHARACTER(EXPAN), INTEGER(STATUS)
                             TRAIL(WORDS) TRAIL(EXPAN) );

F77_SUBROUTINE(one_wordexp_noglob)( CHARACTER(WORDS),
                                    CHARACTER(EXPAN), INTEGER(STATUS)
                                    TRAIL(WORDS) TRAIL(EXPAN) );

F77_SUBROUTINE(one_wordexp_noglob)( CHARACTER(WORDS),
                                    CHARACTER(EXPAN), INTEGER(STATUS)
                                    TRAIL(WORDS) TRAIL(EXPAN) ) {

  GENPTR_CHARACTER(EXPAN)
  GENPTR_INTEGER(STATUS)
  int BUFF_length = 0;
  F77_INTEGER_TYPE CONTEXT = 0;
  char * buff = NULL;     /* Buffer for escaped input */
  char * words = NULL;    /* C copy of WORDS */
  size_t bufflen = 0;

  /* We can pass many arguments directly to psx_wordexp */
  if (*STATUS != SAI__OK) return;

  /* Import the WORDS string into C */
  words = cnfCreim( WORDS, WORDS_length );

  /* Get a buffer to copy the WORDS with enough space for two
     quotes and the terminator */
  bufflen = strlen( words ) + 2 + 1;
  buff = starMallocAtomic( bufflen );

  /* Copy the words into the buffer and surround with double quotes */
  star_strlcpy( buff, "\"", bufflen );
  star_strlcat( buff, words, bufflen );
  star_strlcat( buff, "\"", bufflen );

  /* Now size the string for the Fortran call */
  BUFF_length = strlen( buff );

  /* Call the PSX routine to do all the work. Do not create
     a new fortran buffer for the C buffer that was copied from
     the original Fortran buffer... */
  F77_LOCK( F77_CALL(psx_wordexp)( buff, &CONTEXT, EXPAN, STATUS
                         TRAIL_ARG(BUFF) TRAIL_ARG(EXPAN) ); )

  /* Should only have one result */
  if (CONTEXT != 0) {
    while (CONTEXT != 0) {
      F77_LOCK( F77_CALL(psx_wordexp)( WORDS, &CONTEXT, EXPAN, STATUS
                             TRAIL_ARG(WORDS) TRAIL_ARG(EXPAN) ); )
    }
    F77_EXPORT_CHARACTER("", EXPAN, EXPAN_length);
    if (*STATUS == SAI__OK) {
      *STATUS = SAI__ERROR;
      emsSetnc( "WORDS", WORDS, WORDS_length);
      emsRep( "", "Multiple results from expansion of string '^WORDS'",
              STATUS );
    }
  }

  /* Tidy up */
  starFree( buff );
  cnfFree( words );

}
