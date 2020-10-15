/*
*+
*  Name:
*     smf_summarize_obs

*  Purpose:
*     Summarize observations being processed

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_summarize_obs( const Grp * igrp, int * status );

*  Arguments:
*     igrp = const Grp * (Given)
*        Group containing files to be summarized.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Reports a summary of all the observations being processed. The files
*     are obtained from a Grp.

*  See Also:
*     smf_obsmap_fill for populating the requird AstKeyMap structures.
*     smf_obsmap_report for reporting the results.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2009-04-24 (TIMJ):
*        Initial version. Some code relocated from smf_find_darks.
*     2009-05-25 (TIMJ):
*        Use new smf_obsmap_report API.

*  Copyright:
*     Copyright (C) 2008, 2009 Science and Technology Facilities Council.
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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* System includes */

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "star/grp.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"

void smf_summarize_obs( const Grp * igrp, int * status ) {

  smfData *data = NULL;      /* Input data file */
  int i;
  AstKeyMap * obsmap = NULL; /* Info from all observations */
  AstKeyMap * objmap = NULL; /* All the object names used */
  int size;                  /* Size of input group */

  if (*status != SAI__OK) return;

  obsmap = astKeyMap( " " );
  objmap = astKeyMap( " " );
  size = (int) grpGrpsz( igrp, status );

  for (i = 1; i <= size; i++) {
    /* open the file but just to get the header */
    smf_open_file( NULL, igrp, i, "READ", SMF__NOCREATE_DATA, &data, status );
    if (*status != SAI__OK) break;
    smf_obsmap_fill( data, obsmap, objmap, status );
    smf_close_file( NULL, &data, status );
  }
  smf_obsmap_report( MSG__NORM, obsmap, objmap, status );
  obsmap = astAnnul( obsmap );
  objmap = astAnnul( objmap );

  return;
}
