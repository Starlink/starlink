/*
*+
*  Name:
*     smf_expand_filegroup

*  Purpose:
*     Expand file wildcards in a Grp group

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     newgrp = smf_expand_filegroup( const Grp * igrp, int * status );

*  Arguments:
*     igrp = const Grp * (Given)
*        Input group to be expanded.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*      smf_expand_filegroup = Grp *
*         New group containing expansion of input group.

*  Description:
*     Given a group of file specifications that may include
*     wildcards, return a new group with the wildcards expanded.
*     No attempt is made to check that the resulting group
*     only contains files that actually exist.

*  Authors:
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

*  Notes:
*     - The expanded filenames are not validated to determine
*       if a file actually exists. It is assumed that the caller
*       will realise this.

*  See Also:
*     - one_wordexp_file
*     - Consider moving this routine to CVG.

*  History:
*     2014-03-21 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2014 Cornell University
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "star/grp.h"
#include "sae_par.h"

#include "smf.h"
#include "star/grp.h"
#include "mers.h"

#include <wordexp.h>

Grp *
smf_expand_filegroup( const Grp * igrp, int * status ) {

  Grp * ogrp = NULL;
  dim_t grpsize = 0;
  char grpentry[GRP__SZNAM+1];
  dim_t i;

  if (*status != SAI__OK) return ogrp;

  /* Somewhere to store the expanded information */
  ogrp = grpNew( "Files", status );

  /* Input group size */
  grpsize = grpGrpsz( igrp, status );

  /* For each member of the group we will use wordexp() to
     generate new expansions. */
  for (i = 1; i<=grpsize; i++) {
    size_t j = 0;
    char * pname = grpentry;
    int wstat = 0;
    wordexp_t pwordexp;

    grpGet( igrp, i, 1, &pname, GRP__SZNAM, status );
    if (*status != SAI__OK) break;

    /* wordexp() fills a struct that tells us the number
       of expanded entries and pointers to each string */
    wstat = wordexp( pname, &pwordexp, 0 );
    if (wstat != 0) {
      const char * errstr;

      switch(wstat) {
      case WRDE_BADCHAR:
        errstr = "illegal unquoted characters";
        break;
      case WRDE_BADVAL:
        errstr = "undefined shell variable";
        break;
      case WRDE_CMDSUB:
        errstr = "command substitution is not allowed";
        break;
      case WRDE_NOSPACE:
        errstr = "out of memory for expansion";
        break;
      case WRDE_SYNTAX:
        errstr = "shell syntax error";
        break;
      default:
        errstr = "error of unkown meaning";
      }

      *status = SAI__ERROR;
      errRepf("", "Error expanding wildcards in '%s': %s", status,
              pname, errstr);
      break;
    }

    for (j=0; j < pwordexp.we_wordc; j++) {
      msgOutiff(MSG__DEBUG, "", "Expanding %s [%zu] -> %s", status,
                pname, j, (pwordexp.we_wordv)[j]);
      grpPut1( ogrp, (pwordexp.we_wordv)[j], 0, status );
    }

    wordfree(&pwordexp);
  }

  msgOutiff(MSG__DEBUG, "", "Got %zu entries in expanded group", status,
            grpGrpsz(ogrp, status));

  if (*status != SAI__OK) {
    grpDelet( &ogrp, status );
  }

  return ogrp;

}
