/*
*+
*  Name:
*     smf_open_group

*  Purpose:
*     Open group of files and store in smfArray

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_open_group( ThrWorkForce *wf, const Grp * igrp, const dim_t *refdims,
*                     smfArray **files, int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     igrp = const Grp* (Given)
*        Pointer to an input group of files
*     refdims[2] = const dim_t * (Given)
*        Reference comparison dimensions. If these do not match the
*        dimensions of the files an error occurs. If NULL, the files
*        are checked to be self-consistent.
*     files = smfArray** (Returned)
*        Set of files that have been read.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine opens each data file and stores the resulting
*     smfData into a smfArray.

*  Notes:
*     - Use smf_close_related to free the smfArray
*     - All members of the group must be identical for dimensions 1 and 2
*       and they must be the same as refdims[] if supplied.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-11-24 (TIMJ):
*       Original.
*     2014-01-10 (DSB):
*       Added argument wf.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008,2014 Science and Technology Facilities Council.
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
#include "mers.h"
#include "star/grp.h"
#include "star/thr.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"

void smf_open_group( ThrWorkForce *wf, const Grp * igrp, const dim_t refdims[],
                     smfArray **files, int *status ) {

  size_t i = 0;            /* Loop counter */
  size_t nfiles = 0;       /* Number of files in group */
  dim_t dims[2] = { 0, 0 };/* reference dimension for comparison */

  if (files) *files = NULL;

  if (*status != SAI__OK) return;

  *files = smf_create_smfArray( status );

  /* Copy the reference dimensions since we do not require that
     they be supplied externally */
  if (refdims) {
    dims[0] = refdims[0];
    dims[1] = refdims[1];
  }

  /* calculate size of group */
  nfiles = grpGrpsz( igrp, status );

  for (i = 1; i <= nfiles; i++) {
    smfData *ifile = NULL;
    smf_open_file( wf, igrp, (int) i, "READ", 0, &ifile, status );
    if (*status != SAI__OK) break;

    if (i == 1 && !refdims) {
      dims[0] = (ifile->dims)[0];
      dims[1] = (ifile->dims)[1];
    } else {
      if ( (dims[0] != (ifile->dims)[0]) ||
           (dims[1] != (ifile->dims)[1]) ) {
        msgSetk( "N", i);
        msgSetk( "RX", dims[0]);
        msgSetk( "RY", dims[1]);
        msgSetk( "BX", (ifile->dims)[0]);
        msgSetk( "BY", (ifile->dims)[1]);
        *status = SAI__ERROR;
        errRep( " ", "Dimensions of file ^N (^BX x ^BY) differ from that"
                " of the reference (^RX x ^RY)",
                status );
        break;
      }
    }

    /* all okay, so store the file */
    smf_addto_smfArray( *files, ifile, status );

  }

  if (*status != SAI__OK) {
    smf_close_related( wf, files, status );
  }

}
