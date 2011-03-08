/*
*+
*  Name:
*     ONE_WORDEXP_FILE

*  Purpose:
*     WORDEXP wrapper that verifies a file exists

*  Language:
*     Starlink ANSI C, intended to be called from Fortran

*  Invocation:
*     CALL ONE_WORDEXP_FILE( WORDS, CONTEXT, EXPAN, STATUS )

*  Description:
*     A wrapper around PSX_WORDEXP that only returns files that
*     match the supplied WORDS string following expansion. EXPAN
*     will be an empty string if there are no matches and CONECT
*     will be set to 0.

*  Notes:
*     - Continue to call this routine until CONTEXT is set to 0, otherwise
*     there will be a possible memory leak.
*     - If there are multiple returns from PSX_WORDEXP that are not files
*     this routine will loop internally until a valid file is located.

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
*     2011-03-07 (TIMJ):
*        First version
*     2011-03-08 (TIMJ):
*        Continue looking for files if CONTEXT indicates that
*        there are more files to test and we have not yet found
*        a valid one.

*-
*/

#include "f77.h"
#include "sae_par.h"
#include "ems.h"
#include "star/mem.h"

#include <sys/stat.h>

/* We will be calling the PSX_WORDEXP Fortran wrapper so define
   the prototype. */
F77_SUBROUTINE(psx_wordexp)( CHARACTER(WORDS), INTEGER(CONTEXT),
                             CHARACTER(EXPAN), INTEGER(STATUS)
                             TRAIL(WORDS) TRAIL(EXPAN) );
F77_SUBROUTINE(one_wordexp_file)( CHARACTER(WORDS), INTEGER(CONTEXT),
                                  CHARACTER(EXPAN), INTEGER(STATUS)
                                  TRAIL(WORDS) TRAIL(EXPAN) );


F77_SUBROUTINE(one_wordexp_file)( CHARACTER(WORDS), INTEGER(CONTEXT),
                                  CHARACTER(EXPAN), INTEGER(STATUS)
                                  TRAIL(WORDS) TRAIL(EXPAN) ) {

  GENPTR_CHARACTER(EXPAN)
  GENPTR_INTEGER(STATUS)
  GENPTR_INTEGER(CONTEXT)
  char *expan = NULL;
  struct stat buf;

  /* We can pass many arguments directly to psx_wordexp */
  if (*STATUS != SAI__OK) return;

  /* Get a buffer for the C version of the expansion */
  expan = starMallocAtomic( EXPAN_length + 1 );

  while ( 1 ) {

    /* Call the PSX routine to do all the work */
    F77_CALL(psx_wordexp)( WORDS, CONTEXT, EXPAN, STATUS
                           TRAIL_ARG(WORDS) TRAIL_ARG(EXPAN) );

    /* Need to copy the Fortran EXPAN string to C */
    F77_IMPORT_CHARACTER( EXPAN, EXPAN_length, expan );

    /* Stat the file to see if it is there */
    if (stat( expan, &buf ) == 0 ) {
      /* Success */
      break;
    }

    /* if we still have CONTEXT we need to loop round and get the
       next value to see if that is a file, otherwise we blank
       the return string and exit. */
    if ( ! *CONTEXT) {

      /* The expansion is not associated with a file so export
         a blank string */
      F77_EXPORT_CHARACTER( "", EXPAN, EXPAN_length );
      break;
    }
  }

  /* Tidy up */
  starFree( expan );

}
