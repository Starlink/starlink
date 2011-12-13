/*
*+
*  Name:
*     PSX_WORDEXP

*  Purpose:
*     Do a shell expansion of the supplied string.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL PSX_WORDEXP( WORDS, CONTEXT, EXPAN, STATUS )

*  Description:
*     Performs a shell-style word expansion on the supplied string
*     WORDS, returning context information in CONTEXT. The first result
*     will be returned in EXPAN and subsequent calls with the same
*     CONTEXT will return additional matching items. When the last item
*     is returned in EXPAN the CONTEXT will be freed and set back to 0. If
*     the routine is called again then a new expansion will be initiated.

*  Notes:
*     Continue to call this routine until CONTEXT is set to 0, otherwise
*     there will be a possible memory leak.

*  Arguments:
*     WORDS = CHARACTER*(*) (Given)
*        The string to be shell expanded.
*     CONTEXT = INTEGER (Given & Returned)
*        Should be initialised to 0 for initial shell expansion. Will
*        be set to zero when no more results are available.
*     EXPAN = CHARACTER*(*) (Given)
*        Expanded string. A new string will be returned each call until
*        all are returned an CONTEXT is set to 0.
*     STATUS = INTEGER (Given & Returned)
*        The global status.

*  References:
*     - POSIX Standard POSIX.1-2001

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     2010-03-17 (TIMJ):
*        Original version.
*     2010-04-23 (TIMJ):
*        Sanity check for 0 returned matches.

*-
*/

#include "sae_par.h"
#include "f77.h"
#include "psx_err.h"
#include "psx1.h"

#include <wordexp.h>

typedef struct {
  size_t nextpos;   /* Next position to read */
  wordexp_t pwordexp;
} Context;

F77_SUBROUTINE(psx_wordexp)( CHARACTER(WORDS), INTEGER(CONTEXT),
                             CHARACTER(EXPAN), INTEGER(STATUS)
                             TRAIL(WORDS) TRAIL(EXPAN) ) {
  GENPTR_CHARACTER(WORDS)
  GENPTR_CHARACTER(EXPAN)
  GENPTR_INTEGER(STATUS)
  GENPTR_INTEGER(CONTEXT)

  Context *ContextPtr = NULL;  /* Some where to put the result */

  if (*STATUS != SAI__OK) return;

  if (*CONTEXT == 0) {
    char * words = NULL;
    int retval = 0;

    /* first time through - get some nice CNF memory for the result */
    ContextPtr = cnfMalloc( sizeof(*ContextPtr) );
    memset( ContextPtr, 0, sizeof(*ContextPtr) );
    if (!ContextPtr) {
      *STATUS = PSX__NOMEM;
      psx1_rep_c( "PSX_WORDEXP_ERR1",
                  "Unable to allocate context memory", STATUS);
    }

    /* memory for the C version of WORDS */
    if (*STATUS == SAI__OK) {
      words = cnfMalloc( (1 + WORDS_length) * sizeof(*words) );
      if (!words) {
        *STATUS = PSX__NOMEM;
        psx1_rep_c( "PSX_WORDEXP_ERR2",
                    "Unable to allocate C words memory", STATUS );
      }

      if (*STATUS == SAI__OK) {
        F77_IMPORT_CHARACTER( WORDS, WORDS_length, words );

        /* call the routine to do the expansion */
        retval = wordexp( words, &(ContextPtr->pwordexp), 0 );

        if (retval == 0) {
          if ( ContextPtr->pwordexp.we_wordc > 0 ) {
            ContextPtr->nextpos = 1;
            F77_EXPORT_CHARACTER( (ContextPtr->pwordexp.we_wordv)[0],
                                  EXPAN, EXPAN_length );
          } else {
            ContextPtr->nextpos = 0;
            F77_EXPORT_CHARACTER( "", EXPAN, EXPAN_length );
            *STATUS = PSX__BDWXP;
            psx1_rep_c( "PSX_WORDEXP_ERR3",
                        "No matches for wildcard expansion",
                        STATUS );
          }
        } else {
          *STATUS = PSX__BDWXP;
          switch(retval) {
          case WRDE_BADCHAR:
            psx1_rep_c( "PSX_WORDEXP_ERR3",
                        "The WORDS argument contains illegal unquoted characters",
                        STATUS );
            break;
          case WRDE_BADVAL:
            psx1_rep_c( "PSX_WORDEXP_ERR3",
                        "An attempt was made to expand an undefined shell variable",
                        STATUS );
            break;
          case WRDE_CMDSUB:
            psx1_rep_c( "PSX_WORDEXP_ERR3",
                        "An attempt was made to use command substitution",
                        STATUS );
            break;
          case WRDE_NOSPACE:
            *STATUS = PSX__NOMEM;
            psx1_rep_c( "PSX_WORDEXP_ERR3",
                        "Not enough memory to store wordexp result",
                        STATUS );
            break;
          case WRDE_SYNTAX:
            psx1_rep_c( "PSX_WORDEXP_ERR3",
                        "Shell syntax error in WORDS",
                        STATUS );
            break;
          default:
            psx1_rep_c( "PSX_WORDEXP_ERR3",
                        "Unrecognized error from wordexp", STATUS );
          }

        }
      }
    }

    /* free local resources */
    if (words) cnfFree( words );

    /* Store the pointer in the context for the user */
    if (*STATUS == SAI__OK) {
      F77_EXPORT_POINTER( ContextPtr, *CONTEXT );
    } else {
      /* cleanup */
      if (ContextPtr) {
        wordfree( &(ContextPtr->pwordexp) );
        cnfFree( ContextPtr );
        ContextPtr = NULL;
      }
    }
  } else {
    /* repeat customer */
    F77_IMPORT_POINTER( *CONTEXT, ContextPtr );

    /* get the next value */
    F77_EXPORT_CHARACTER( (ContextPtr->pwordexp.we_wordv)[ContextPtr->nextpos],
                          EXPAN, EXPAN_length );
    ContextPtr->nextpos++;


  }

  /* Cleanup if we've returned all the results */
  if (ContextPtr) {
    if (ContextPtr->nextpos == ContextPtr->pwordexp.we_wordc) {
      /* have got all the words so need to free */
      wordfree( &(ContextPtr->pwordexp) );
      cnfFree( ContextPtr );
      *CONTEXT = 0;
    }
  }

}
