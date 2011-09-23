/*
*+
*  Name:
*     RAWREWRTSC2WCS

*  Purpose:
*     Fix broken WCS in raw SCUBA-2 file by re-writing it

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_rawrecreatesc2wcs( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Some SCUBA-2 files end up with corrupt WCS extensions. It is possible
*     to recreate the WCS by looking at the JCMTSTATE and FITS header. The
*     new WCS is written to the file, replacing the original.

*  Notes:
*     - Supports SCUBA-2 raw data files
*     - Currently it will be necessary to remove the corrupt .WCS data
*       using KAPPA ERASE before running this command.

*  ADAM Parameters:
*     NDF = NDF (Read)
*          Input files to be fixed.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2011-09-22 (TIMJ):
*        Original version

*  Copyright:
*     Copyright (C) 2011 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include "star/grp.h"
#include "ast.h"
#include "sae_par.h"
#include "mers.h"

#include "smurf_par.h"
#include "libsmf/smf_err.h"
#include "libsmf/smf.h"
#include "smurflib.h"

void smurf_rawrewrtsc2wcs( int * status ) {

  size_t i;
  Grp *igrp = NULL;          /* Input group */
  size_t size;               /* Number of files in input group */


  if (*status != SAI__OK) return;

  ndfBegin();

  /* Read the input file group */
  kpg1Rgndf( "NDF", 0, 1, "", &igrp, &size, status );

  for (i=1; i<=size && ( *status == SAI__OK ); i++) {
    int indf = NDF__NOID;
    smfData *data = NULL;
    AstFrameSet * fixedwcs = NULL;
    int isok = 1;

    /* First open in READ mode as a sanity check */
    smf_open_file( igrp, i, "READ", 0, &data, status );
    if (*status != SAI__OK) break;
    if (data->hdr->instrument != INST__SCUBA2) {
      isok = 0;
      msgOut( "", "This command only works on ACSIS data files", status );
    }

    /* Get a fixed WCS frameset */
    if (isok) {
      smf_create_tswcs( data->hdr, &fixedwcs, status );
    }

    /* close up and skip if this is not a good file */
    smf_close_file( &data, status );
    if (!isok) continue;

    /* Now we try to update the file using NDF */
    ndgNdfas( igrp, i, "UPDATE", &indf, status );
    ndfPtwcs( fixedwcs, indf, status );
    ndfAnnul( &indf, status );
  }

  /* Cleanup */
  grpDelet( &igrp, status);
  ndfEnd( status );
}
