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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  History:
*     2011-03-08 (TIMJ):
*        First version

*-
*/

#include "f77.h"
#include "sae_par.h"
#include "ems.h"
#include "star/mem.h"

/* Macro to test whether a character needs escaping
   and to call some code if a metacharacter is found. */
#define PROCESS_META( INPUT, POSN, ACTION ) \
  switch (WORDS[inpos]) {                   \
  case '|':                                 \
  case '&':                                 \
  case ';':                                 \
  case '<':                                 \
  case '>':                                 \
  case '(':                                 \
  case ')':                                 \
  case '{':                                 \
  case '}':                                 \
  case '*':   /* glob characters */         \
  case '[':                                 \
  case ']':                                                     \
  case '?':                                                     \
    /* Make sure that the previous character is not a \ */      \
    if (POSN > 0 && INPUT[POSN-1] != '\\') {                    \
      ACTION;                                                   \
    }                                                           \
    break;                                                      \
  }


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
  size_t inpos = 0;
  size_t outpos = 0;
  size_t nmeta = 0;

  /* We can pass many arguments directly to psx_wordexp */
  if (*STATUS != SAI__OK) return;

  /* Count how many metacharacters we will be escaping */
  for (inpos = 0; inpos < WORDS_length; inpos++) {
    PROCESS_META( WORDS, inpos, nmeta++ );
  }

  /* Get a buffer to copy the WORDS with enough space for escape
     characters and the terminator */
  buff = starMallocAtomic( WORDS_length + nmeta + 1 );

  /* Copy from WORDS to the buffer (assumes a Fortran string is
     a lot like a C string. */
  for (inpos = 0; inpos < WORDS_length; inpos++, outpos++) {
    PROCESS_META( WORDS, inpos, buff[outpos]='\\';outpos++ );
    buff[outpos] = WORDS[inpos];
  }
  buff[outpos] = '\0'; /* terminate */

  /* Now size the string */
  BUFF_length = cnfLenc( buff );
  buff[BUFF_length] = '\0';

  /* Call the PSX routine to do all the work. Do not create
     a new fortran buffer for the C buffer that was copied from
     the original Fortran buffer... */
  F77_CALL(psx_wordexp)( buff, &CONTEXT, EXPAN, STATUS
                         TRAIL_ARG(BUFF) TRAIL_ARG(EXPAN) );

  /* Should only have one result */
  if (CONTEXT != 0) {
    while (CONTEXT != 0) {
      F77_CALL(psx_wordexp)( WORDS, &CONTEXT, EXPAN, STATUS
                             TRAIL_ARG(WORDS) TRAIL_ARG(EXPAN) );
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

}
