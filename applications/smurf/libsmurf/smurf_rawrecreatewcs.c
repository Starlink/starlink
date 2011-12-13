/*
*+
*  Name:
*     RAWRECREATEWCS

*  Purpose:
*     Fix broken raw ACSIS files by enablign the spectral WCS to be reconstructed

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_rawrecreatewcs( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Some ACSIS faults result in a file being written with the frequency
*     axis unable to be written because of an error in the FITS WCS
*     stored in the FITS header. This command lets the spectral WCS be
*     recreated once the FITS header has been fixed following a manual
*     intervention. This command will fail on files that already have
*     had the WCS information stripped from the FITS header.


*  Notes:
*     - Supports ACSIS raw data files

*  ADAM Parameters:
*     NDF = NDF (Read)
*          Input files to be fixed.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2011-03-30 (TIMJ):
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include "star/grp.h"
#include "ast.h"
#include "libacsis/specwrite/specwrite.h"
#include "sae_par.h"
#include "mers.h"

#include "smurf_par.h"
#include "libsmf/smf_err.h"
#include "libsmf/smf.h"
#include "smurflib.h"

void smurf_rawrecreatewcs( int * status ) {

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
    int isok = 1;

    /* First open in READ mode as a sanity check */
    smf_open_file( igrp, i, "READ", 0, &data, status );
    if (*status != SAI__OK) break;
    if (data->hdr->instrument != INST__ACSIS) {
      isok = 0;
      msgOut( "", "This command only works on ACSIS data files", status );
    }

    /* Look for a WCS header */
    if (*status == SAI__OK) {
      double dtemp = 0.0;
      smf_fits_getD( data->hdr, "CRVAL1", &dtemp, status );
      if (*status == SMF__NOKWRD) {
        errAnnul( status );
        isok = 0;
        msgOut( "", "This file does not seem to contain a FITS header with WCS information",
                status );
      }
    }

    /* sanity check the WCS if everything else looks ok - do this
       to prevent us opening the raw data file for update when we know
       we will fail. This will set status to bad. */
    if (isok && *status == SAI__OK) {
      AstFrameSet *wcs = NULL;
      astClear(data->hdr->fitshdr,"Card");
      astRead( data->hdr->fitshdr );
      if (*status != SAI__OK) {
        isok = 0;
        errRep("", "WCS in FITS header still invalid. Please fix before trying again",
               status );
      }
      if (wcs) wcs = astAnnul( wcs );
    }

    /* close up and skip if this is not a good file */
    smf_close_file( &data, status );
    if (!isok) continue;

    /* Now we try to update the file */
    ndgNdfas( igrp, i, "UPDATE", &indf, status );
    acsRewriteWCS( indf, status );
    ndfAnnul( &indf, status );
  }

  /* Cleanup */
  grpDelet( &igrp, status);
  ndfEnd( status );
}
